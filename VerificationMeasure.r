# Duke University Synthetic Data Project
# Verification Measure Script

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

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)


#####################################################################################################
#### load verification measure functions
#### note that functions are created in the current environment
#### an alternative to sourcing functions is to store them in a library (package) and load it
#### loading with sys.source(file, env=environment()) improves performance of function execution
#### when compared to performance when loaded with source()
#### compile does not improve performance much (because we use apply functions!)
#####################################################################################################

sys.source("VerificationServer.r", env=environment())


#####################################################################################################
#### compute race effect threshold verifcation measures, by sex and race
#####################################################################################################

# query authentic observations
# note the inclusion of pseudo ID for partitioning
# also, both sexes - subsets will be generated for individual models by sex
t <- proc.time()
opmAuth <- queryObservations(source="authentic",
                             cols=c("FY, PseudoID", "Sex", "Race", "Age", "BasicPay", "EducationYears",
                                    "BureauID, OccupationalCategory", "Occupation"),
                             payPlan=NULL,
                             fullTime=T,
                             startYear=0,
                             endYear=9999,
                             whereClause=NULL)
proc.time()-t
gc()

# alternative to querying SQL:  load test observations
# opmAuth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",", strip.white=T)

# construct model cfg
# not that sex is not specified since each subset will have one value only (is this homo(mono)sexual?)
# note the placement of fixed effects in order of dimension for optimal parallel cross-products in X'X
mCfg <- list("Y"="lnBasicPay",
             "contX"=c("Age", "AgeSq", "EducationYears"),
             "fixedX"=c("Race", "FY", "BureauID", "Occupation"),
             "refLevel"=c("E", "1988", "VATA", "303"),
             "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2", "colID"="AgeSq"),
                                 data.frame("f"="log", "par1"="BasicPay", "par2"="", "colID"="lnBasicPay")))

# compute threshold verification measures for non-reference races
# note that the parameter(s) of interest (threshPar) are specified in terms (name) reported by the fixed
# effects solution algorithm:
#   for continuous vars, the authData column name
#   for fixed effects, the authData column name followed by a dash and level as it appears in the data
#     examples:  Sex-M for Sex, male; Race-D for Race, Hispanic; Occupation-303, etc.
#   for interactions, colName1-colName2 for continuous-continuous, colName-feName(level) for continuous-
#   fixed effect interactions (note that regardless of appearance in interaction specification, continuous
#   var names always appear first), and fe1(level1)-fe2(level2) for fixed effect-fixed effect interactions
#     examples:  Age-EducationYears, Age-Occupation(303), FY(2000)-BureauID(123456789)
t <- proc.time() 
pThresh <- lapply(c("F", "M"),
             function(sex) {
               k <- which(opmAuth[,"Sex"]==sex)
               thresholdMeasure(authData=opmAuth[k,], nPartitions=50, model="feOLS", modelCfg=mCfg,
                                threshPar=c("Race-A", "Race-B", "Race-C", "Race-D"),
                                threshVal=-0.01, threshDir="<=", epsPriv=1, alpha=c(1, 1))
             })
names(pThresh) <- c("Female", "Male")
proc.time()-t

# review
pThresh$Female$pPosteriorMode
pThresh$Male$pPosteriorMode


#####################################################################################################
#### compute race effect three-point longitudinal verification measures, by sex and race
#####################################################################################################

# query authentic observations
# note the inclusion of pseudo ID for partitioning
# also, both sexes - subsets will be generated for individual models by sex and year
t <- proc.time()
opmAuth <- queryObservations(source="authentic",
                             cols=c("PseudoID", "Sex", "Race", "Age", "EducationYears",
                                    "FY", "BureauID, "Occupation", "BasicPay"),
                             payPlan=NULL,
                             fullTime=T,
                             startYear=0,
                             endYear=9999,
                             whereClause=NULL)
proc.time()-t
gc()

t <- proc.time()
opmSynth <- queryObservations(source="synthetic",
                              cols=c("PseudoID", "Sex", "Race", "Age", "EducationYears",
                                     "FY", "BureauID", "Occupation", "BasicPay"),
                              payPlan=NULL,
                              fullTime=T,
                              startYear=1988,
                              endYear=2011,
                              #whereClause=t(matrix(c("Age", ">50", "Sex", "='F'"), nrow=2)))
                              whereClause=NULL)
proc.time()-t
gc()

# alternative to querying SQL:  load test observations
# opmAuth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",", strip.white=T)
# opmSynth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",", strip.white=T)

# construct model cfg
# not that sex is not specified since each subset will have one value only (is this homo(mono)sexual?)
# note the placement of fixed effects in order of dimension for optimal parallel cross-products in X'X
mCfg <- list("Y"="lnBasicPay",
             "contX"=c("Age", "AgeSq", "EducationYears"),
             "fixedX"=c("Race", "BureauID", "Occupation"),
             "refLevel"=c("E", "VATA", "303"),
             "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2", "colID"="AgeSq"),
                                 data.frame("f"="log", "par1"="BasicPay", "par2"="", "colID"="lnBasicPay")))

# compute longitudinal verification measures for non-reference races
# note that the parameter(s) of interest (verPar) are specified in terms (name) reported by the fixed
# effects solution algorithm:
#   for continuous vars, the authData column name
#   for fixed effects, the authData column name followed by a dash and level as it appears in the data
#     examples:  Sex-M for Sex, male; Race-D for Race, Hispanic; Occupation-303, etc.
#   for interactions, colName1-colName2 for continuous-continuous, colName-feName(level) for continuous-
#   fixed effect interactions (note that regardless of appearance in interaction specification, continuous
#   var names always appear first), and fe1(level1)-fe2(level2) for fixed effect-fixed effect interactions
#     examples:  Age-EducationYears, Age-Occupation(303), FY(2000)-BureauID(123456789)

# females
t <- proc.time()
ks <- which(opmSynth[,"Sex"]=="F")
ka <- which(opmAuth[,"Sex"]=="F")
pLongF <- longitudinalThreePtMeasure(synthData=opmSynth[ks,], authData=opmAuth[ka,], nPartitions=50,
                      model="feOLS", modelCfg=mCfg, verPar=c("Race-A", "Race-B", "Race-C", "Race-D"),
                      intervalMidpoint=c(1998, 2000, 2000, 2000), deltaBand=0.002, epsPriv=1,
                      alpha=c(1, 1))
proc.time()-t

# males
t <- proc.time()
ks <- which(opmSynth[,"Sex"]=="M")
ka <- which(opmAuth[,"Sex"]=="M")
pLongM <- longitudinalThreePtMeasure(synthData=opmSynth[ks,], authData=opmAuth[ka,], nPartitions=50,
                      model="feOLS", modelCfg=mCfg, verPar=c("Race-A", "Race-B", "Race-C", "Race-D"),
                      intervalMidpoint=c(2003, 2002, 1998, 1997), deltaBand=0.002, epsPriv=1,
                      alpha=c(1, 1))
proc.time()-t

gc()

# review
pLongF$pPosteriorMode$pPosteriorMode
pLongM$pPosteriorMode$pPosteriorMode


