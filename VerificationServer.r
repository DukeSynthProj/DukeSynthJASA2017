# Duke University Synthetic Data Project
# Verification Server Functions

# Author:  Tom Balmat
# Version:  5/14/2017

# Copyright 2017 Duke University, Durham, North Carolina

# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
# associated documentation files (the "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
# following conditions:

# The above copyright notice and this permission notice shall be included in all copies or substantial
# portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
# LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

options(stringsAsFactors=F)

#####################################################################################################
#### load X'X fixed effects solution and differential privacy functions
#### note that functions are created in the current environment
#### an alternative to sourcing functions is to store them in a library (package) and load it
#### loading with sys.source(file, env=environment()) improves performance of function execution
#### when compared to performance when loaded with source()
#####################################################################################################

sys.source("FixedEffectsMatrixSolution.r", env=environment())
sys.source("BetaBinomialLaplacePosteriorDP.r", env=environment())
sys.source("CommonFunctions.r", env=environment())


#####################################################################################################
#### define SQL query function
#####################################################################################################

queryObservations <- function(source, cols, payPlan=NULL, fullTime=F, startYear=0, endYear=9999,
                              whereClause=NULL) {

  # query auhentic or synthetic OPM observations

  # to be verified:  valid source and cols requested

  library(RODBC)

  # parameters (case insensitive):
  # source:       "synthetic" or "authentic"
  # cols:         vector of SQL table columns to retrieve (note the use of OPM nomenclature)
  # payPlan:      char, limit observations to specified pay plan, "GSGrade<=15" for GS grade<=15, "" for all
  # fullTime:     T/F, limit observations to full time (work schedule "B" or "F")
  # startYear:    numeric YYYY, include observation with FY >= this value, 0000 for no start year filter
  # endYear:      numeric YYYY, include observation with FY <= this value, 9999 for no end year filter
  # whereClause:  matrix or data frame of SQL columns in column 1, syntactically correct SQL where clause
  #               (without the "where") in column 2
  #               example:  col-1   col-2
  #                         Age     >21
  #                         Sex     ='F'
  #               note that each row forms an individual clause and all rows are concatenated into
  #               a single SQL where using "and" (observations that satisfy all clauses are returned)
  #               pass a 0-row data frame if no where clause required
  #               example construction syntax:
  #                 t(matrix(c("Age", ">50", "Sex", "='F'"), nrow=2)))

  
  # result:  a data frame containing the columns requested from the source requested with filters
  #          applied, unless an error occurs, in which case a single cell data frame containing
  #          the error message generated by the DB interface

  # note that variable naming conventions and data validation rules are taken from
  # the Office of Personnel Management's Guide to Data Standards (available, as of Oct 2016, at
  # https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/
  #                                                    reporting-guidance/part-a-human-resources.pdf) 

  # example call:  mySet <- queryObservations(source="authentic",
  #                                           cols=c("PseudoID", "Sex", "Race", "Age", "BasicPay"),
  #                                           payPlan="",
  #                                           fullTime=T,
  #                                           startYear=0,
  #                                           endYear=9999,
  #                                           whereClause=t(matrix(c("Age", ">21",
  #                                                                  "Sex", "='F'"), nrow=2)))

  # assemble comma delimited where clause strings
  # note the replacement of single apostrophes with doubles to be proper SQL strings
  if(!is.null(whereClause)) {
    if(nrow(whereClause)>0) {
      whereColSQL <- gsub(" ", "", paste(whereClause[,1], collapse=","))
      whereClauseSQL <- gsub("'", "''", paste(whereClause[,2], collapse=","))
    } else {
      whereColSQL <- ""
      whereClauseSQL <- ""
    }
  } else {
    whereColSQL <- ""
    whereClauseSQL <- ""
  }      

  # configure database connection
  db <- odbcDriverConnect(connection="driver={SQL Server}; server=; database=;
                          trusted_connection=true", readOnlyOptimize=T)

  # query observations
  # note that observations with missing or invalid sex, race, age, education, FY, bureau, occupation
  # are omitted (when in requested column list)
  # THIS SHOULD BE IMPROVED BY OMITTING OBSERVATIONS WITH MISSING VALUES IN ANY REQUESTED COLUMN
  # oservations with BasicPay<=0 are omitted
  # limit to pay plan specified in @PayPlan
  # use @Payplan='GSGrade<=15' for GS only
  # use @WorkSchedule='FullTime' for 'B', 'F' work schedule observations
  obs <- sqlQuery(db, paste("DIBBSData @Style='VerificationData', ",
                                      "@Source='", source, "', ",
                                      "@SelectCol='", paste(cols, collapse=','), "', ",
                                      "@WorkSchedule='", ifelse(fullTime, "FullTime", ""), "', ",
                                      "@PayPlan='", ifelse(!is.null(payPlan), payPlan, ""), "', ",
                                      "@StartFY=", ifelse(!is.null(startYear), startYear, 0), ", ",
                                      "@EndFY=", ifelse(!is.null(endYear), endYear, 9999), ", ",
                                      "@WhereCol='", whereColSQL, "', ",
                                      "@WhereClause='", whereClauseSQL, "'", sep=""), stringsAsFactors=F)

  odbcClose(db)

  # the SQL proc scrubs observations based known data validity rules
  # omit any observations with null values in any column
  return(obs[complete.cases(obs),])

}


###############################################################################################################
#### define fixed effects model fitting function
###############################################################################################################

fitFEModel <- function(data, Y, contX, fixedX, refLevel, interactionX=NULL, TransformXY=NULL,
                       estBetaVar="", robustVarID="", nCoreXTX=6, nCoreBetaVar=0) {

  # parameters:
  # data:         data frame containing data to fit model to
  # Y:            character string containing name of vector in data containing dependent values
  # contX:        vector of character strings containing names of vectors in data containing
  #               continuous independent values
  # fixedX:       vector of character strings containing names of vectors in data containing
  #               fixed effect independent columns (sex, race, bureau, occupation, etc.)
  # refLevel:     vector of character strings containing fixed effect reference levels (in
  #               order of fixedX positions
  # interactionX: two column array of interaction vector pairs in data, each row a separate
  #               interaction 
  # TransformXY:  dependent/independent variable transformation instructions
  #               four column array, ech row applies one transform to a specified vector in data
  #               row format is F, par1, par2, colID
  #               where f is an R or user defined function to execute the transform
  #                     par1 is a character string indicating wich data column to transform
  #                     par2 is a constant, if needed for the transform function (the 2 in x**2)
  #                     colID is the character string to be applied to the transformed values  
  #               example:  rbind(data.frame("f"="^", "par1"="Age", "par2"="2", "colID"="AgeSq"),
  #                               data.frame("f"="log", "par1"="BasicPay", "par2"="", "colID"="lnBasicPay"))
  #               squares Age, naming the new vector "AgeSq" (which can now be referenced in the Y,
  #               contX, or fixedX parameters) and converts BasicPay to log(BasicPay), giving it
  #               the name "lnBasicPay"
  # estBetaVar:   type of standard errors to estimate (stdOLS, robust, cluster)
  # robustVarID:  cluster standard error group ID (must exist in data)
  # nCoreXTX:     number of parallel cores to use in X'X composition
  # nCoreBetaVar: number of parallel cores to use in robust/cluster SE estimation
                       
  # parameter verification to be made:
  # variables in Y, contX, fixedX, interactionX, and transformXY exist in data
  # each fixed effect has a ref level specified and the level is represented in the data
  # estBetaVar in ("", "none", "stdOLS", "robust", "clusterID")
  # robustVarID in ("", a column appearing in data)

  # performance notes:
  # appearance of feVar fixed effects in order of increasing dimension (number of levels) improves
  # efficiency of parallel apply functions involving cross products and interactions involving fixed
  # effects since this always selects the highest dimension var to be distributed to cores, maximizing
  # their utilization (reordering is done within the feXTX function)

  # apply transforms by evaluating supplied functions and parameters and appending results to observation df
  if(!is.null(TransformXY))
    for(i in 1:nrow(TransformXY)) {
      # identify which column being transformed
      j <- which(colnames(data)==TransformXY[i,"par1"])
      if(TransformXY[i,"par2"]=="") {
        # one parameter (a column in data) supplied
        data <- data.frame(data, eval(parse(text=paste("\"", TransformXY[i,"f"], "\"(data[,", j, "])", sep=""))))
      } else {
        # two parameters supplied
        data <- data.frame(data, eval(parse(text=paste("\"", TransformXY[i,"f"], "\"(data[,", j, "],",
                                                       TransformXY[i,"par2"], ")", sep=""))))
      }
      colnames(data)[ncol(data)] <- TransformXY[i,"colID"]
    }

  gc()

  # execute XTX FE model solution and return its results
  feXTX(data=data, Y=Y, contX=contX, fixedX=fixedX, refLevel=refLevel, interactionX=interactionX,
        estBetaVar=estBetaVar, robustVarID=robustVarID, nCoreXTX=nCoreXTX, nCoreVar=nCoreVar)

}


###############################################################################################################
#### define threshold verification measure function
###############################################################################################################

thresholdMeasure <- function(authData, nPartitions, model, modelCfg, threshPar, threshVal, threshDir,
                             epsPriv, alpha) {

  # compute threshold verification measure
  # partition authentic data into random "equal size" disjoint subsets and fit specified model to each
  # compute proportion of partitions with specified model parameter(s) beyond specified threshold value
  # apply posterior beta-binomial-Laplace to proportions 

  # parameters:
  # authData:     data frame containing authentic OPM data
  # nPartitions:  number of random disjoint authentic data partitions to make
  # model:        character string, type of model to fit (currently, feOLS is the only supported model)
  # modelCfg:     list with elements Y, contX, fixedX, refLevel, and TransformXY
  #               these are passed without modification to fitFEModel (see function for explanation of
  #               parameters)
  #               example:
  #               list("Y"="lnBasicPay",
  #                    "contX"=c("Age", "AgeSq", "EducationYears"),
  #                    "fixedX"=c("Race", "BureauID", "Occupation"),
  #                    "refLevel"=c("E", "VATA", "303"),
  #                    "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2", "colID"="AgeSq"),
  #                                        data.frame("f"="log", "par1"="BasicPay", "par2"="", "colID"="lnBasicPay")))
  # threshPar:    independent variable vectors in authData to report verification measures on (multiple
  #               vectors can be specified, c("Race-A", "Race-B") for races A and B)
  # threshVal:    threshold value (proportion partitions with threshPar estimates above/below this value
  #               are reported)
  # treshDir:     direction of comparison (<, <=, or >)
  # epsPriv:      Laplace posterior noise epsilon parameter
  # alpha:        beta distribution alpha parameter (two element vector)
  
  # parameter verification to be made:
  # variables in modelCfg exist in authData
  # threshPar exists in authData data and modelCfg
  # pseudo IDs exist in authData
  # 0 < reasonable lower bound < nPartitions < reasonable upper bound 
  # validate threshold value (numeric) and direction (<= or >)
  # reasonable boundaries on privacy epsilon (note relationship to beta-binomial-Laplace noise centering)
  # alpha = beta distribution extremity (0, 1) buffer (reasonable boundaries)

  status <- ""

  # create a time accounting matrix
  fitTime <- matrix(data=0, nrow=2, ncol=5)
  rownames(fitTime) <- c("tcomputeAuth", "tmemexportAuth")
  colnames(fitTime) <- c("user.self", "sys.self", "elapsed", "user.child", "sys.child")

  # generate authentic ID partitions, fit models to each data partition, compute proportion partitions
  # beyond threshold, apply posterior noise to proportion, prepare histogram of posterior proportions

  # generate list of randomly ordered, unique pseudoids
  pid <- sample(unique(authData[,"PseudoID"]))

  # generate disjoint pseudo ID partitions
  # compute the number of IDs per partition (the final partition will have ID count of other
  # partitions + balance (max of set size + nPartitions - 1)
  nid <- as.integer(length(pid)/nPartitions)
  idset <- lapply(1:nPartitions, function(i) pid[((i-1)*nid+1):ifelse(i<nPartitions, i*nid, length(pid))])

  gc()

  # create matrix to store partition parameter estimates
  betaPartition <- matrix(nrow=0, ncol=length(threshPar))
  colnames(betaPartition) <- threshPar

  # fit models to data sets corresponding to disjoint ID partitions
  i <- 1
  while(i<=nPartitions & status=="") {

    # fit model to observations for current subset of IDs
    if(model=="feOLS") {
      # identify observations for pseudo IDs in current set
      k <- which(authData[,"PseudoID"] %in% idset[[i]])
      authModel <- fitFEModel(data=authData[k,], Y=modelCfg$Y, contX=modelCfg$contX, fixedX=modelCfg$fixedX,
                              refLevel=modelCfg$refLevel, TransformXY=modelCfg$TransformXY, nCoreXTX=6)
      # save results
      status <- authModel$status
      fitTime["tcomputeAuth",] <- fitTime["tcomputeAuth",] + authModel$time["tcompute",]
      fitTime["tmemexportAuth",] <- fitTime["tmemexportAuth",] + authModel$time["tmem",]
      if(status=="")
        betaPartition <- rbind(betaPartition, authModel$beta[threshPar])
    } else {
      status <- "ERROR (thresholdMeasure):  Unknown model specified"
    }
    i <- i+1
  }

  gc()

  if(status=="") {
    # compute number of authentic partitions with parameter estimate satisfying threshold test
    # the result is a vector with one element (count of 1 partitions) per threshold parameter
    if(threshDir=="<=") {
      nPartitionThresh <- apply(betaPartition, 2, function(p) length(which(p<=threshVal)))
    } else if(threshDir==">") {
      nPartitionThresh <- apply(betaPartition, 2, function(p) length(which(p>threshVal)))
    } else {
      status <- "ERROR (thresholdMeasure):  Unknown threshold comparison direction specified"
    }
  }

  if(status=="") {

    # apply posterior noise to number of partitions at threshold
    # generate vector of beta, binomial, and Laplace p values
    # use a large iteration count to enable narrow histogram intervals
    # the result is a matrix of noise induced proportion 1-partition values, one column
    # per threshold parameter
    pDP <- apply(as.matrix(nPartitionThresh), 1,
             function(nPartition) DP.threshold(M=nPartitions, S=nPartition, epsilon=epsPriv,
                                               alpha=c(1, 1), nIter=250000))

    # compute mode of posterior p values using 250 cut bins and mean center of top 5 freq bins
    pMode <- apply(pDP, 2, frequencyMode, 250, 5)

    # save components of histogram of noise-induced partitions at threshold p values
    pHist <- apply(pDP, 2, function(pDP) hist(pDP, breaks=100, plot=F))

  }

  gc()

  # package results
  if(status=="") {
    list("status"=status, "betaPartition"=betaPartition, "nPartitionThresh"=nPartitionThresh,
         "pPosteriorMode"=pMode, "pHist"=pHist, "fitTime"=fitTime)
  } else {
    list("status"=status)
  }

}


###############################################################################################################
#### define longitudinal three-point verification measure function
###############################################################################################################

longitudinalThreePtMeasure <- function(authData, nPartitions, synthData, model, modelCfg,
                                       verPar, intervalMidpoint, deltaBand, epsPriv, alpha) {

  # compare longitudinal pattern of parameter estimates between synthetic and authentic data
  # on three intervals:  first to last year in data, first year to specified mid-point year,
  # and specified mid-point to last year
  # report, for each interval, the proportion authentic data partitions with longitudinal slope
  # having sign (positive/negative) equal to the slope measured on the corresponding interval in
  # the synthetic data
  # apply posterior beta-binomial-Laplace to proportions 

  # parameters:
  # authData:         data frame containing authentic OPM data
  # nPartitions:      number of random disjoint authentic data partitions to make
  # synthData:        data frame containing synthetic OPM data
  # model:            character string, type of model to fit (currently, feOLS is the only supported model)
  # modelCfg:         list with elements Y, contX, fixedX, refLevel, and TransformXY
  #                   these are passed without modification to fitFEModel (see function for explanation of
  #                   parameters)
  #                   example:
  #                   list("Y"="lnBasicPay",
  #                        "contX"=c("Age", "AgeSq", "EducationYears"),
  #                        "fixedX"=c("Race", "BureauID", "Occupation"),
  #                        "refLevel"=c("E", "VATA", "303"),
  #                        "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2", "colID"="AgeSq"),
  #                                            data.frame("f"="log", "par1"="BasicPay", "par2"="", "colID"="lnBasicPay")))
  # verPar:           independent variable vectors in authData to report verification measures on (multiple
  #                   vectors can be specified, c("Race-A", "Race-B") for races A and B)
  # intervalMidpoint: mid-point year
  # deltaBand:        tolerance value for tolerance band verification method (as opposed to sign method)
  # epsPriv:      Laplace posterior noise epsilon parameter
  # alpha:        beta distribution alpha parameter (two element vector)
  
  # parameter verification to be made:
  # variables in modelCfg exist in authData/synthData
  # verPar exists in synthetic data, authentic data, and modelCfg
  # pseudo IDs exist in auth and synth data sets
  # 0 < reasonable lower bound < nPartitions < reasonable upper bound 
  # validate interval midpoints (year between end years in data)
  # reasonable boundaries on privacy epsilon (note relationship to beta-binomial-Laplace noise centering)
  # alpha = beta distribution extremity (0, 1) buffer (reasonable boundaries)

  status <- ""

  # create a time accounting matrix
  fitTime <- matrix(data=0, nrow=4, ncol=5)
  rownames(fitTime) <- c("tcomputeSynth", "tmemexportSynth", "tcomputeAuth", "tmemexportAuth")
  colnames(fitTime) <- c("user.self", "sys.self", "elapsed", "user.child", "sys.child")

  if(model=="feOLS") {

    # fit model to synthetic data for each year
    betaSynth <- matrix(nrow=0, ncol=1+length(verPar))
    colnames(betaSynth ) <- c("year", verPar)
    yr <- sort(unique(synthData[,"FY"]))
    i <- 1
    while(i<=length(yr) & status=="") {
      k <- which(synthData[,"FY"]==yr[i])
      synthModel <- fitFEModel(data=synthData[k,], Y=modelCfg$Y, contX=modelCfg$contX, fixedX=modelCfg$fixedX,
                               refLevel=modelCfg$refLevel, TransformXY=modelCfg$TransformXY, nCoreXTX=6)
      status <- synthModel$status
      fitTime["tcomputeSynth",] <- fitTime["tcomputeSynth",] + synthModel$time["tcompute",]
      fitTime["tmemexportSynth",] <- fitTime["tmemexportSynth",] + synthModel$time["tmem",]
      if(status=="") {
        betaSynth <- rbind(betaSynth , c(yr[i], synthModel$beta[verPar]))
      }
      i <- i+1
    }

    gc()

    if(status=="") {

      # generate list of randomly ordered, unique pseudoids
      pid <- sample(unique(authData[,"PseudoID"]))
      # generate disjoint pseudo ID partitions
      # compute the number of IDs per partition (the final partition will have ID count of other
      # partitions + balance (max of set size + nPartitions - 1)
      nid <- as.integer(length(pid)/nPartitions)
      idset <- lapply(1:nPartitions, function(i) pid[((i-1)*nid+1):ifelse(i<nPartitions, i*nid, length(pid))])
      gc()
      # create matrix to store partition parameter estimates
      betaAuth <- matrix(nrow=0, ncol=2+length(verPar))
      colnames(betaAuth) <- c("partition", "year", verPar)
      # fit annual models to data sets corresponding to disjoint ID partitions
      yr <- sort(unique(synthData[,"FY"]))
      i <- 1
      while(i<=nPartitions & status=="") {
        j <- 1
        while(j<=length(yr) & status=="") {
          # fit model to observations for current year and subset of IDs
          k <- which(authData[,"PseudoID"] %in% idset[[i]] & authData[,"FY"]==yr[j])
          authModel <- fitFEModel(data=authData[k,], Y=modelCfg$Y, contX=modelCfg$contX, fixedX=modelCfg$fixedX,
                                  refLevel=modelCfg$refLevel, TransformXY=modelCfg$TransformXY, nCoreXTX=2)
          # save results
          status <- authModel$status
          fitTime["tcomputeAuth",] <- fitTime["tcomputeAuth",] + authModel$time["tcompute",]
          fitTime["tmemexportAuth",] <- fitTime["tmemexportAuth",] + authModel$time["tmem",]
          if(status=="")
            betaAuth <- rbind(betaAuth, c(i, yr[j], authModel$beta[verPar]))
          j <- j+1
        }
        i <- i+1
      }
    }

  } else {
    status <- "ERROR (longitudinalThreePointMeasure):  Unknown model specified"
  }

  gc()

  if(status=="") {

    # compute three trajectories: slope of beta estimates over entire period, slope from
    # first year to specified mid-point, and slope from mid-point to last year
    # note that the data may not include observations for the specified mid-point year
    # however, the slope over years in the three segments remain valid and comparable
    # between synhetic and authentic data and between authentic partitions
    # a problem arises when a single year is present, since this prevents slope computation
    # data sets or partitions with invalid or infinite slopes are omitted from proportion
    # aggregation and verification measure reporting

    # create a matrix for synthetic slope results (rows for intervals, cols for ver parameters)
    intervalSlopeSynth <- matrix(nrow=3, ncol=length(verPar))
    rownames(intervalSlopeSynth) <- c("y1-yn", "y1-ymid", "ymid-yn")
    colnames(intervalSlopeSynth) <- verPar
    # create a list for authentic slope results (one element for each verification parameter,
    # each element a matrix of slopes, rows for intervals, cols for partitions)
    intervalSlopeAuth <- list()
    # get year boundaries in data for current verification parameter
    yboundSynth <- c(min(betaSynth[,"year"]), max(betaSynth[,"year"]))
    yboundAuth <- c(min(betaAuth[,"year"]), max(betaAuth[,"year"]))
    # cycle through all specified parameters to be verified
    iver <- 1
    while(iver<=length(verPar) & status=="") {
      # compute interval slopes in synthetic data
      # verify that specified mid-point year between boundary years
      if(intervalMidpoint[iver]> yboundSynth[1] & intervalMidpoint[iver]<yboundSynth[2]) {
        # construct intervals (first to last year, first to mid-year, mid-year to last)
        # each column is an interval
        yintv <- matrix(c(yboundSynth[1], yboundSynth[2],
                          yboundSynth[1], intervalMidpoint[iver],
                          intervalMidpoint[iver], yboundSynth[2]), nrow=2)
        # compute least squares slopes on the three intervals
        intervalSlopeSynth[,iver] <- apply(yintv, 2,
                                       function(intv) {
                                        # subset over interval
                                        k <- which(betaSynth[,"year"] %in% intv[1]:intv[2])
                                        if(length(k)>1) {
                                          lm(betaSynth[k,verPar[iver]]~betaSynth[k,"year"])$coeff[2]
                                        } else {
                                          status <- paste("ERROR (longitudinalThreePointMeasure):  ",
                                                          "Insufficient synthetic data on interval ",
                                                          intv[1], "-", intv[2], ", model parameter ",
                                                          verPar[iver], sep="")
                                          return(0)
                                        }
                                      })
      } else {
        status <- paste("ERROR (longitudinalThreePointMeasure):  Mid-point year outside of ",
                        "years in synthetic data, model parameter ", verPar[iver], sep="")
      }
      if(status=="") {
        # compute interval slopes in authentic partitions
        # verify that specified mid-point year between boundary years
        if(intervalMidpoint[iver]> yboundAuth[1] & intervalMidpoint[iver]<yboundAuth[2]) {
          # construct intervals (first to last year, first to mid-year, mid-year to last)
          # each column is an interval
          yintv <- matrix(c(yboundAuth[1], yboundAuth[2],
                            yboundAuth[1], intervalMidpoint[iver],
                            intervalMidpoint[iver], yboundAuth[2]), nrow=2)
          # compute array of slopes for each partition and interval
          # save in new list element for current verification parameter
          # note that list elements are 3 X nPartitions matrices, one row for each nterval,
          # one col for each partition
          intervalSlopeAuth[[verPar[iver]]] <-
            mapply(1:nPartitions,
                     FUN=function(ipart) 
                         apply(yintv, 2,
                               function(intv) {
                                 # subset over interval
                                 k <- which(betaAuth[,"partition"]==ipart &
                                            betaAuth[,"year"] %in% intv[1]:intv[2])
                                 if(length(k)>1) {
                                   lm(betaAuth[k,verPar[iver]]~betaAuth[k,"year"])$coeff[2]
                                 } else {
                                   status <- paste("ERROR (longitudinalThreePointMeasure):  ",
                                   "Insufficient authentic data on interval ",
                                   intv[1], "-", intv[2], ", model parameter ",
                                   verPar[iver], sep="")
                                   return(0)
                                 }
                               }))
        } else {
          status <- paste("ERROR (longitudinalThreePointMeasure):  Mid-point year outside of ",
                          "years in synthetic data, model parameter ", verPar[iver], sep="")
        }
      }

      iver <- iver+1
    }
                                    
    if(status=="") {

      # compute verification measure as proportion, by verification parameter and interval,
      # authentic partitions with slope within delta band of slope of corresponding parameter
      # and interval from synthetic data
      # compare each verification parameter matrix to corresponding column in synthetic matrix
      slopeComparison <-
        lapply(1:length(verPar),
               FUN=function(iver)
                     apply(intervalSlopeAuth[[iver]], 2,
                           # compare auth and synth slopes by interval
                           function(ipartSlope)
                             ifelse(abs(ipartSlope-intervalSlopeSynth[,iver])<=deltaBand, 1, 0)))
      names(slopeComparison) <- verPar
      # each list element (one per verfication parameter) is a 3XnPartitions matrix of
      # binary values, 1 indicating within delta band, 0 indicating outside
      # matrix rows correspond to year intervals, cols to partitions
      # aggregate, by verification parameter and interval, count of 1 partitions
      # result is a matrix of partition counts, rows for intervals, cols for verif pars
      np1 <- mapply(1:length(verPar),
                    FUN=function(iver)
                          # sum each row (interval)
                          apply(slopeComparison[[iver]], 1, sum))
      colnames(np1) <- verPar

      # apply posterior noise to proportion partitions with sign agreement
      # generate vectors of beta-binomial-Laplace p values, one for each parameter and interval
      # use a large iteration count to enable narrow histogram intervals (increased resolution)
      # compute mode of simulated p values
      # return histogram structure for each vector of p's
      pDPmode <- list()
      pDPmode[["intervals"]] <- data.frame("par"=verPar,
                                           "intv1Begin"=rep(yboundAuth[1], length(verPar)),
                                           "intv1End"=rep(yboundAuth[2], length(verPar)),
                                           "intv2Begin"=rep(yboundAuth[1], length(verPar)),
                                           "intv2End"=intervalMidpoint,
                                           "intv3Begin"=intervalMidpoint,
                                           "intv3End"=rep(yboundAuth[2], length(verPar)))
      # note the transpose
      pDPmode[["pPosteriorMode"]] <- matrix(data=0, nrow=ncol(np1), ncol=nrow(np1))
      rownames(pDPmode[["pPosteriorMode"]]) <- verPar
      colnames(pDPmode[["pPosteriorMode"]]) <- c("intv1", "intv2", "intv3")
      for(i in 1:nrow(np1)) {
        for(j in 1:ncol(np1)) {
          # generate simulated (posterior) p values for partitions=1 frequency
          # use a large iteration count so that small intervals (used in mode) each
          # have many items
          pDP <- DP.threshold(M=nPartitions, S=np1[i,j], epsilon=epsPriv, alpha=c(1, 1), nIter=100000)
          # compute mode of posterior p using 250 cut bins and mean center of top 5 bins 
          # note storing in transpose
          pDPmode[["pPosteriorMode"]][j, i] <- frequencyMode(pDP, 150, 5)
          # save historam structure
          pDPmode[["hist"]][[verPar[j]]][[paste("intv", i, sep="")]]<- hist(pDP, breaks=100, plot=F)
        }
      }

    }

    gc()

    # package results
    if(status=="") {
      list("status"=status, "pPosteriorMode"=pDPmode, "betaSynth"=betaSynth, "betaAuth"=betaAuth,
           "intervalSlopeSynth"=intervalSlopeSynth, "intervalSlopeAuth"=intervalSlopeAuth, "fitTime"=fitTime)
    } else {
      list("status"=status)
    }

  }

}


