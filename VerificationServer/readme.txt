Instructions and notes for executing Verification Server algorithms used to develop empirical results included
in the paper "A Framework for Sharing Confidential Research Data, Applied to Investigating Differential Pay by Race
in the U. S. Government" prepared in 2017 by the Synthetic Data Project at Duke University

1.  Background

Verification consists of fitting a user specified model to synthetic and authentic observation data (SD and AD, let
SF and AF indicate synthetic and authentic fit, respectively) and presenting statistical results of the SF along
with a measure of "agreement" in the AF.  Generally, AF consists of individual models being fit to disjoint,
complete partitions of AD.  Two verification measures appear in the paper:  one for "threshold" measure and one
for "longitudinal" measure.  They are explained in sections 3.1 and 3.2 of the paper, respectively.

2.  Data used in verification measure computation

Verification of models fit to synthetic data requires two data sets:  the synthesized observations being studied
and authentic observations that the synthesized data represent.  Analyst specified models are fit to both data
sets and statistical results of fit to synthetic data are presented along with a measure of agreement in the authentic
data.  The synthetic data and verification system are designed to minimize risk of private information release using
principles of differential privacy (explained in greater detail in the paper) and at no time are authentic data, or
results of models fit to authentic data, released to users of the system.  The current paper utilizes synthetic
records intended as a substitute for authentic U.S. Office of Personnel Management (OPM) Central Personnel Data File
(CPDF) records, consisting of one record for each synthesized federal employee and fiscal year for the period 1988
through 2011.  Authentic CPDF records, made available by OPM in response to a Freedom of Information Act (FOIA)
request, are used for verification.    

At this time, due to restrictions imposed by OPM, neither synthetic nor authentic data are available to the public.
In the future, pending OPM approval, synthetic data and verification system results may be accessible to authorized
users.  A small set of CPDF-formatted observations, that we will call a test file, is included with the on-line
supplemental materials at:

https://github.com/DukeSynthProj/Programs/tree/master/VerificationServer.

The test file has the same set of variables as does the authentic CPDF file supplied to Due University by OPM, but
contains only 10,000 observations, representing 5000 distinct simulated employees.  Variables are named in accordance
with OPM nomenclature, which is fully explained in the OPM Guide to Data Standards, available at:

https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf

Test file observations were generated using marginal frequencies, as measured in 1,000,000 randomly sampled synthetic
observations, of variables used in the paper (sex, race, education, FY, bureau ID, and occupation).  Simulated basic
pay is drawn from a N(10000, 500) distribution.  Variables such as pay plan, grade, and duty station appear in the
test file, but contain empty values; although present in OPM formatted source files, they are not used in the analysis
and are not required by scripts, procedures, or algorithms.  The purpose of the test file is to allow readers to test
the code that we are releasing without revealing actual relationships in either authentic or synthetic data.

Note that, since authentic OPM observations are not available in the supplemental material, computation of meaningful
verification measures is not possible with the supplied software and test file.

3.  Code

The verification measure system, as implemented, retrieves authentic and synthetic observations from a secure SQL
database.  Once authentic data sets are retrieved, A hierarchical sequence of R functions processes each data set,
computes verification measures, and returns results.  All scripts and code necessary to execute the verification
measure system are included in the supplementary material repository: 

https://github.com/DukeSynthProj/Programs/tree/master/VerificationServer.

Functions are contained in separate files and are described below.

3.1  File:  DIBBSDataRetrieval.sql – This file contains MS T-SQL instructions to:
     a.  Create empty authentic and synthetic CPDF observation tables (CPDFNonDODStatusJdF2012, CPDFNonDODStatusJdF2014,
         and CPDFNonDODDIBBS2017) structured to accept data as delivered by OPM.  Note that the supplied test data
         (OPMSyntheticTestData.csv) can be imported into these enabling SQL data retrieval as executed in the verification
         functions.
     b.  Create the view (CPDFNonDODStatusDIBBS2016) referenced by the DIBBSData stored procedure (below) to retrieve
         versioned synthetic observations.
     c.  Create the DIBBSData stored procedure that is called by the verification R scripts.  There are three primary
         methods of execution (example call syntax is given):
         1.  Retrieve authentic observations:
             exec DIBBSData @style = 'LargeFixedEffectsModelNonUniqueIDJdF', @WorkSchedule = 'FullTime'
         2.  Retrieve synthetic observations:
             exec DIBBSData @style = 'LargeFixedEffectsModelNonUniqueIDDIBBS', @WorkSchedule = 'FullTime'
         3.  Retrieve observations as requested by verification server functions:
             exec DIBBSData @style='VerificationData'
         Additional information on options, parameters, and modes of execution are contained in the script files.

3.2  File:  VerificationMeasure.r – R script that contains example sessions for executing the threshold and three-point
            longitudinal verification measures presented in the paper.  Query and computation functions from
            VerificationServer.r (below) are called from this procedure.  Additional instructions on use are contained in
            the source file.

3.3  File:  VerificationServer.r – R script containing primary verification system functions, including:
     a.  queryObservations() – queries SQL synthetic and authentic observations based on user supplied filtering
         specification
     b.  fitFEModel() – fits a fixed effects model to user specified synthetic or authentic data set, a data frame
         formatted as the result of queryObservations(); calls feXTX() (below); returns model fit results
     c.  thresholdMeasure() – executes the threshold verification measure presented in the paper;  expects two
         queryObservations() style data frames (one for synthetic data one for authentic data); partitions authentic
         observations; calls fitFEModel() and DP.threshold() Laplace posterior noise algorithm; returns list of
         verification measure results
     d.  longitudinalThreePtMeasure() - executes the three-point longitudinal verification measure presented in the paper;
         expects two queryObservations() style data frames (one for synthetic data one for authentic data); partitions
         authentic observations; calls fitFEModel() and DP.threshold() Laplace posterior noise algorithm; returns list of
         verification measure results

3.4  File:  LaplaceDistribution.r – contains pdf, cmf, and random quantile functions based on the Laplace distribution,
     called by DP.threshold()

3.5  File:  BetaBinomialLaplacePosteriorDP.r – Laplace noise algorithm; primary function is DP.threshold(),
     called by verification measure functions, calls Laplace distribution functions

3.6  File:  FixedEffectsMatrixSolution.r – contains function feXTX() that fits OLS fixed effects models to large
     observation, high dimensional data; called by fitFEModel() in verification measure process

3.7  File:  CommonFunctions.r – contains functions for common tasks, such as computing the mode of an empirical
     distribution

Complete instructions on use of each function are provided in the source files.

4.  Software environment

    The verification server algorithms used to produce results for the paper were developed on a Microsoft Windows 7
    server, with 24 cores and 64 Gb of memory, using Microsoft SQL Server 2014 and R x64 version 3.2.1.  Required R
    libraries are RODBC and parallel.

5.  Example verification measure sessions

5.1 Threshold measure

Following are sample instructions to read OPM CPDF formatted observations and compute corresponding threshold measures.
As used in the paper, two sets of measures are computed:  one for female and one for male subsets of the data.  Note
that for demonstration, SQL database retrieval is replaced by a simple read of observations from the test file,
OPMSyntheticTestData.csv into a data frame that is then supplied to the threshold measure function.  Note, also, that
operating system locations used in source() calls in R scripts must be modified to reflect the actual location where
scripts have been saved.  The following instructions are to be executed from within R.

    # construct authentic and synthetic data frames using test file observations
    opmAuth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",", strip.white=T)
    opmSynth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",",
                       strip.white=T)

    # configure model to be verified (note that this is model explained in section 4.2 of the paper)
    mCfg <- list("Y"="lnBasicPay",
                 "contX"=c("Age", "AgeSq", "EducationYears"),
                 "fixedX"=c("Race", "FY", "BureauID", "Occupation"),
                 "refLevel"=c("E", "1988", "VATA", "303"),
                 "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2",
                                                "colID"="AgeSq"),
                                     data.frame("f"="log", "par1"="BasicPay",
                                               "par2"="", "colID"="lnBasicPay")))

    # execute threshold verification measure
    pThresh <- lapply(c("F", "M"),
                 function(sex) {
                   k <- which(opmAuth[,"Sex"]==sex)
                   thresholdMeasure(authData=opmAuth[k,], nPartitions=50,
                                    model="feOLS", modelCfg=mCfg,
                                    threshPar=c("Race-A", "Race-B", "Race-C",
                                                "Race-D"),
                                    threshVal=-0.01, threshDir="<=", epsPriv=1,
                                    alpha=c(1, 1))
                 })
    names(pThresh) <- c("Female", "Male") 

    # review threshold verification results
    pThresh$Female$pPosteriorMode
    pThresh$Male$pPosteriorMode

5.2  Longitudinal verification measure

Individual longitudinal measures are also computed for female and male subsets.

    # construct authentic and synthetic data frames using test file observations
    opmAuth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",", strip.white=T)
    opmSynth <- read.table("OPMSyntheticTestData.csv", header=T, sep=",",
                       strip.white=T)
    # configure model
    mCfg <- list("Y"="lnBasicPay",
                 "contX"=c("Age", "AgeSq", "EducationYears"),
                 "fixedX"=c("Race", "BureauID", "Occupation"),
                 "refLevel"=c("E", "114009000", "303"),
                 "TransformXY"=rbind(data.frame("f"="^", "par1"="Age", "par2"="2",
                                                "colID"="AgeSq"),
                                     data.frame("f"="log", "par1"="BasicPay",
                                                "par2"="", "colID"="lnBasicPay")))

    # females
    ks <- which(opmSynth[,"Sex"]=="F")
    ka <- which(opmAuth[,"Sex"]=="F")
    pLongF <- longitudinalThreePtMeasure(synthData=opmSynth[ks,], authData=opmAuth[ka,], nPartitions=50,
                          model="feOLS", modelCfg=mCfg, verPar=c("Race-A", "Race-B",
                                                                 "Race-C", "Race-D"),
                          intervalMidpoint=c(1998, 2000, 2000, 2000), deltaBand=0.002,
                          epsPriv=1, alpha=c(1, 1))

    # males
    ks <- which(opmSynth[,"Sex"]=="M")
    ka <- which(opmAuth[,"Sex"]=="M")
    pLongM <- longitudinalThreePtMeasure(synthData=opmSynth[ks,], authData=opmAuth[ka,], nPartitions=50,
                          model="feOLS", modelCfg=mCfg, verPar=c("Race-A", "Race-B",
                                                                 "Race-C", "Race-D"),
                          intervalMidpoint=c(2003, 2002, 1998, 1997), deltaBand=0.002,
                          epsPriv=1, alpha=c(1, 1))

    # review results
    pLongF$pPosteriorMode$pPosteriorMode
    pLongM$pPosteriorMode$pPosteriorMode

6.  Questions and comments

Questions and comments regarding code, scripts, or implementation of verification measure algorithms may be
directed to thomas.balmat@duke.edu

