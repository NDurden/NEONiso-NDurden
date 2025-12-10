##############################################################################################
#' @title Workflow for calibrating NEON atmospheric H2O isotope data

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com} \cr
#' 

#' @description
#' Workflow. Calibrating NEON atmospheric H2O isotope data.

#' @param Currently none

#' @return Currently none

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' Metzger, S., Durden, D., Sturtevant, C., Luo, H., Pingintha-Durden, N., Sachs, T., Serafimovich, A., Hartmann, J., Li, J., Xu, K., and Desai, A. R.: eddy4R: A community-extensible processing, analysis and modeling framework for eddy-covariance data based on R, Git, Docker and HDF5, Geosci. Model Dev. Discuss., 2017, 1-26, doi:10.5194/gmd-2016-318.
#' Fiorella, R. P., Good, S. P., Allen, S. T., Guo, J. S., Still, C. J., Noone, D. C., Anderegg, W. R. L.,  Florian, C. R., Luo, H., Pingintha-Durden, N., and Bowen, G.J.: Calibration Strategies for Detecting Macroscale Patterns in NEON Atmospheric Carbon Isotope Observations, J. Geophys. Res. Biogeosci., 2021, 126(3), e2020JG005862, https://doi.org/10.1029/2020JG005862.

#' @keywords water, isotope, storage, eddy-covariance, calibration

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2025-12-03)
#     original creation
##############################################################################################
# clean workspace
# rm(list=ls())

# clean console
# cat("\014")
library(eddy4R.base)

# Start logging
logLevel <- Sys.getenv('LOG_LEVEL')
rlog <- Logger.Singleton$new(logging_level = logLevel) #class defined in eddy4R.base
rlog$info("Start ECSE H2O isotope correction from flow.isoH2o.cor.R")


# check if require packages are installed 
packReq <- c("NEONiso", "parsedate", "Hmisc", "neonUtilities", "dplyr", "tidyr", "lubridate")

lapply(packReq, function(x) {
  tryCatch({rlog$debug(x)}, error=function(cond){print(x)})
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})
###################################################################################################
#install packages from Cran 
#install.packages("neonUtilities", dependencies=TRUE, repos='http://cran.rstudio.com/')
#neonUtilities >= 2.3.0
#install.packages("tidyr", dependencies=TRUE, repos='http://cran.rstudio.com/')
#tidyr >= 1.3.0
#install NEONiso from GitHub. Development version
#devtools::install_github("lanl/NEONiso")
#NEONiso >= 0.6.3
#install.packages("rlang", dependencies=TRUE, repos='http://cran.rstudio.com/')
#‘rlang’ >= 1.1.0 is required
##################################################################################################
# remove workflow package list
rm(packReq)
# load and attach packages

# names of packages
namePack <- c("DataCombine", "eddy4R.base", "eddy4R.turb", "NEONiso", "neonUtilities", "methods", "rhdf5", "deming", "dplyr", "tidyr") 

# load and attach
tmp <- sapply(namePack, library, character.only = TRUE); rm(tmp)

# INITIALIZE ENVIRONMENT #######################################################

# define global environment
eddy4R.base::def.env.glob()

# USER SELECTIONS
# deploy workflow file in default mode (dflt, uses web-based gold file), in environmental-variables-from-host mode (host, for batch-processing from command line),
# or in user selection mode (slct, for interactive data analysis in Rstudio) [character]
# see bottom of this section for command line instruction templates

# create list to hold parametric information in a central place
Para <- list()

##Example for setting up the environment variables########################################################
# base::Sys.setenv("METH" = "host")
# base::Sys.setenv("DATEOUT" = "2018-11-28")
# base::Sys.setenv("DIRINP" = paste0("/home/", base::Sys.getenv("USER"), "/eddy/data/iso/ONAQ/inp"))
# base::Sys.setenv("DIRMNT" = paste0("/home/", Sys.getenv("USER"), "/eddy"))
# base::Sys.setenv("DIROUT" = paste0("/home/", Sys.getenv("USER"), "/eddy/data/iso/ONAQ/out"))
# base::Sys.setenv("DIRTMP" = paste0("/home/", Sys.getenv("USER"), "/eddy/tmp"))
# base::Sys.setenv("FILEINP" = NA)
# base::Sys.setenv("FILEOUTBASE" = "NEON.D06.KONZ.IP4.00200.001.ecse")
# base::Sys.setenv("OUTMETH" = "hdf5")
# base::Sys.setenv("PRDINCRCALC" = 1)
# base::Sys.setenv("PRDWNDWCALC" = 1)
# base::Sys.setenv("READ" = "hdf5")
# base::Sys.setenv("VERSDP" = NA)
# base::Sys.setenv("VERSEDDY" = NA)

#e.g. to remove Sys.unsetenv("METH")
###########################################################################################################

# user selection in case corresponding environmenal variable is not assigned
if(!"METH" %in% base::names(base::Sys.getenv())) {
  Para$Flow$Meth <- c("dflt",  "host", "slct")[3]
  # if corresponding environmental variable is assigned, use that value
} else {
  Para$Flow$Meth <- base::Sys.getenv("METH")
}

# in case user selection mode is chosen (Para$Flow$Meth == "slct"), workflow parameters can be modified
if(Para$Flow$Meth == "slct") {
  
  # user-customizeable access to input and output data directories
  Para$Flow$DirUsr <- c(
    nd = "/eddy/data/isoH2o/CLBJ",
    dd = "/eddy/data/tmp"
  )["nd"]
  # for a detailed description of all default workflow parameters see ?eddy4R.base::def.para.flow.ecte, section Overview of workflow parameters
  Para$Flow$DateOut <- base::as.character(base::as.Date(base::as.character(20240205:20240205), format = "%Y%m%d"))[1:1]
  Para$Flow$DirInp <- base::paste0("/home/", base::Sys.getenv("USER"), Para$Flow$DirUsr, "/inp/20240205")
  Para$Flow$DirMnt <- base::paste0("/home/", Sys.getenv("USER"), "/eddy")
  Para$Flow$DirOut <- base::paste0("/home/", Sys.getenv("USER"), Para$Flow$DirUsr, "/out")
  Para$Flow$DirTmp <- NA #base::paste0("/home/", Sys.getenv("USER"), "/eddy/tmp")
  Para$Flow$DirWrk <- NA
  Para$Flow$FileInp <- base::dir(Para$Flow$DirInp, pattern = "*.h5")
  Para$Flow$FileOutBase <- c("NEON.D11.CLBJ.IP4.00200.001.ecse")
  Para$Flow$NameDataExt <- NA
  Para$Flow$OutMeth <- c("hdf5", "diag")[1]
  Para$Flow$OutSub <- NA
  Para$Flow$PrdIncrCalc <- 1
  #Para$Flow$PrdIncrPf <- 1
  Para$Flow$PrdWndwCalc <- 1
  #Para$Flow$PrdWndwPf <- 1
  Para$Flow$Read <- c("hdf5", "ff")[1]
  Para$Flow$VersDp <- "001"
  Para$Flow$VersEddy <- NA
  
}


# collect, and check for presence and absence of workflow parameters, create and set directories, and download reference data (if applicable)
Para$Flow <- eddy4R.base::def.para.flow.ecte(ParaFlow = Para$Flow,
                                             UrlInpRefe = "https://storage.googleapis.com/neon-ec-goldfiles/EC-iso-cor/inpRefeIsoCor_20230831.zip",
                                             UrlOutRefe = "https://storage.googleapis.com/neon-ec-goldfiles/EC-iso-cor/outRefeIsoCor_20230831.zip")
#update some parameter if Para$Flow$Meth == "dflt"
if (Para$Flow$Meth == "dflt") {
  Para$Flow$FileOutBase <- c("NEON.D06.KONZ.IP4.00200.001.ecse")
}


# INITIAL CHECKS ###############################################################
rlog$info("Initial Checks")
# check if number of input files (Para$Flow$FileInp) cover calculation window (Para$Flow$PrdWndwCalc)
# i.e. if Para$Flow$PrdWndwCalc is equal to 7, Para$Flow$FileInp should be equal to 7 files
if(Para$Flow$PrdWndwCalc > base::length(Para$Flow$FileInp)) {
  msg <- "number of daily input files < Para$Flow$PrdWndwCalc."
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# check that Para$Flow$DateOut is a strictly regular, daily sequence
if(!base::all(base::diff(base::as.Date(Para$Flow$DateOut)) == 1)) {
  msg <- "please ensure that Para$Flow$DateOut consists of subsequent days."
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# determine center days
dateCntr <- Para$Flow$DateOut[base::seq(from = 1, to = base::length(Para$Flow$DateOut), by = Para$Flow$PrdIncrCalc)]

# for each center day, determine the date sequence covering calculate window (i.e. 7)
for(idx in dateCntr) {
  
  Para$Flow$DateCalc[[idx]] <- base::as.character(base::seq.Date(
    from = base::as.Date(idx) - ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    to = base::as.Date(idx) + ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
    by = "day"))
  
}; rm(idx)

# check if there is an input file (expanded file) for each day over all calculation windows
# determine if there is an input file corresponding to each day
dateCalcExpd <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
  base::any(base::grepl(pattern = base::paste0(".*", x, ".expanded.h5?"), Para$Flow$FileInp))
)

# issue an error in case one or more files are missing
if(!base::all(dateCalcExpd)) {
  msg <- base::paste("expanded file for", base::paste0(base::names(dateCalcExpd)[!dateCalcExpd], collapse = ", "), "is missing.")
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}

# check if there is an input file (basic file) for the center day (this file will be used for writing output)
# determine if there is an input file corresponding to center day

dateCalcBasc <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
  base::any(base::grepl(pattern = base::paste0(".*", x, ".basic.h5?"), Para$Flow$FileInp))
)

# issue an error in case basic file center day are missing
if(!dateCalcBasc[[dateCntr]]) {
  msg <- base::paste("basic file for ", dateCntr, " is missing.")
  tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
  stop(msg)
}
# Processing ###################################################################
rlog$info("Begin to read-in data")
# determine full input file path for current center day in data processing window
# this day is used as reference for reading parametric information
# Expanded file path
Para$FileName$EcseExpd <- base::file.path(Para$Flow$DirInp, grep(pattern = paste0("ecse.*", Para$Flow$DateOut ,".*expanded.h5"), Para$Flow$FileInp, value = TRUE))
#DirFilePara <- base::file.path(Para$Flow$DirInp, grep(pattern = base::paste0(".*", dateCntr, ".*.h5?"), Para$Flow$FileInp, value = TRUE))

#Basic file path
Para$FileName$EcseBasc <- base::file.path(Para$Flow$DirInp, grep(pattern = paste0("ecse.*",Para$Flow$DateOut ,".*basic.h5"), Para$Flow$FileInp, value = TRUE))

#################################################################################################
#Check if NEONiso output already present in files
#################################################################################################
listObjExpd <- rhdf5::h5ls(Para$FileName$EcseExpd)
listObjBasc <- rhdf5::h5ls(Para$FileName$EcseBasc)
ObjExstExpd <- any(grepl(pattern = "dp01/data/isoH2o/calData", x = listObjExpd$group))
ObjExstBasc <- any(grepl(pattern = "dp01/data/isoH2o/calData", x = listObjBasc$group))
if(ObjExstExpd){
  #Provide error message
  rlog$error("NEONiso calData already output to Expanded file")
  #Interupt workflow
  rlang::interrupt()}
if(ObjExstBasc){
  #Provide error message
  rlog$error("NEONiso calData already output to Basic file")
  #Interupt workflow
  rlang::interrupt()}
#################################################################################################

#Grab the NEON specific 4-letter code for the site location (Loc) from the input file
Para$Flow$Loc <- eddy4R.base::def.para.site(FileInp = Para$FileName$EcseExpd)$Loc

#get level of tower
Para$Flow$LvlTowr <- eddy4R.base::def.para.site(FileInp = Para$FileName$EcseExpd)$LvlTowr

#In this case, expanded files will be used in calculation
#list all file names in inpDir
nameFile <- list.files(path = Para$Flow$DirInp,
                       pattern = "expanded.h5",
                       recursive = TRUE,
                       full.names = TRUE)

#check if all isoH2o input data is NaN
rlog$info("Initial Checks if all data are missing and during measuring period and/or validation period")
inpTmp <- neonUtilities::stackEddy(filepath = nameFile, avg = 9, level = "dp01",var = "isoH2o",useFasttime = TRUE, runLocal = TRUE)[[1]]

#define variables
#LvlTowr and validation periods
varLvlTowr <- paste0(Para$Flow$LvlTowr,"_09m")
varLvlVali <- paste0(c("h2oLow", "h2oMed", "h2oHigh"), "_03m")

#create tempdir if Para$Flow$DirTmp was not provided
if(is.na(Para$Flow$DirTmp)) Para$Flow$DirTmp <- tempdir()
#check if there are validation periods
valiTmp <- neonUtilities::stackEddy(filepath = nameFile, avg = 3, level = "dp01",var = "isoH2o",useFasttime = TRUE, runLocal = TRUE)[[1]]
valiTrue <- valiTmp$verticalPosition %in% c("h2oHigh", "h2oLow", "h2oMed")

#creating empty outData01 and outData02 when there are no data in inpTmp or no data in validation periods
if (nrow(inpTmp) == 0 || !any(valiTrue)){
  #download reference data
  rlog$info("Fail to perform correction due to missing due to no available data during measuring period and/or validation period")
  rlog$info("Downloding the golden file...")
  # reference input files
  #NEED TO UPDATE to the site that have isoH2o DATA
  eddy4R.base::def.dld.zip(Inp = list(Url = "https://storage.googleapis.com/neon-ec-goldfiles/EC-iso-cor/inpRefeIsoCor_20230831.zip", 
                                      Dir =  Para$Flow$DirTmp))
  nameFileTmp <- list.files(path = Para$Flow$DirTmp,
                            pattern = "2018-11-28.expanded.h5",
                            recursive = TRUE,
                            full.names = TRUE)
  rlog$info(nameFileTmp)
  
  #correcting data using Bowling_2003 method
  rlog$info("Golden File: Correcting isoH2o data...")
  ####################################################
  #replace these once updating package with the update function#
  source('~/eddy/docker/NEONiso-NDurden/R/reference_data_selection.R')
  source('~/eddy/docker/NEONiso-NDurden/R/reference_data_regression.R')
  source('~/eddy/docker/NEONiso-NDurden/R/calibrate_water.R')
  outDataTmp01 <- calibrate_water(inname = nameFile, 
                                  site = "CLBJ", 
                                  calibration_half_width = 1,
                                  filter_data = TRUE,
                                  force_cal_to_beginning = FALSE,
                                  force_cal_to_end = FALSE,
                                  r2_thres = 0.95,
                                  slope_tolerance = 9999,
                                  correct_ref_data = TRUE,
                                  write_to_file = FALSE)
  
  outData01 <- list()

  #creating empty dataframe for each measurement level from the reference file
  for(j in varLvlTowr) {
    for (k in names(outDataTmp01$wiso_subset_cal$`000_010_09m`)){
      outData01$wiso_subset_cal[[j]][[k]] <- data.frame(matrix(ncol=ncol(outDataTmp01$wiso_subset_cal$`000_010_09m`[[k]]),
                                                               nrow=0, 
                                                               dimnames=list(NULL, names(outDataTmp01$wiso_subset_cal$`000_010_09m`[[k]]))))
    }}
  #creating empty dataframe for each validation period from the reference file
  for(j in varLvlVali) {
    for (k in names(outDataTmp01$wiso_subset_cal$h2oHigh_03m)){
      outData01$wiso_subset_cal[[j]][[k]] <- data.frame(matrix(ncol=ncol(outDataTmp01$wiso_subset_cal[[j]][[k]]),
                                                               nrow=0, 
                                                               dimnames=list(NULL, names(outDataTmp01$wiso_subset_cal[[j]][[k]]))))
    }}
  #add calibration data.frame
  outData01$cal_df <- outDataTmp01$cal_df
  outData01$cal_df[1,] <- rep(NA, ncol(outData01$cal_df))
  
  #remove unused table
  rm(outDataTmp01, nameFileTmp)
  
} else {
  rlog$info("Starting data correction...")
  #correction processing
  #correcting data using Bowling_2003 method
  rlog$info("Correcting isoH2o...")
  outData01 <- calibrate_water(inname = nameFile, 
                               site = Para$Flow$Loc, 
                               calibration_half_width = 1,
                               filter_data = TRUE,
                               force_cal_to_beginning = FALSE,
                               force_cal_to_end = FALSE,
                               r2_thres = 0.95,
                               slope_tolerance = 9999,
                               correct_ref_data = TRUE,
                               write_to_file = FALSE)

}#end else
rlog$info("Finished data correction...")
#combine data from both method into one list
outData <- list()

#adding columns, combine results, clean up, organizing, and changing column names
for(j in names(outData01$wiso_subset_cal)) {
  for (k in names (outData01$wiso_subset_cal[[j]])){
    #remove verticalPosition and varname column
    outData01$wiso_subset_cal[[j]][[k]] <-  outData01$wiso_subset_cal[[j]][[k]][ , !(names( outData01$wiso_subset_cal[[j]][[k]]) %in% c("verticalPosition", "varname")) ]
    #outData01$wiso_subset_cal[[j]][[k]] <- outData01$wiso_subset_cal[[j]][[k]] %>% select(-verticalPosition, -varname)
    #adding 1st column with NA when number of row = 0
    #outData01$wiso_subset_cal
    if(nrow(outData01$wiso_subset_cal[[j]][[k]]) == 0){
      outData01$wiso_subset_cal[[j]][[k]][1,] <- rep(NA, ncol(outData01$wiso_subset_cal[[j]][[k]]))
    } else{
      outData01$wiso_subset_cal[[j]][[k]] <- outData01$wiso_subset_cal[[j]][[k]]
    }
    
    if (k %in% c("dlta18OH2o", "dlta2HH2o")){
      
      ######BOOK KEEPING######################################
      #change column names to NEON terms
      #original column names
      #dlta18OH2o = c("timeBgn","timeEnd","mean","min","max","vari","numSamp","mean_cal","max_cal", "min_cal")
      #dlta2HH20 = c("timeBgn","timeEnd","mean","min","max","vari","numSamp","mean_cal",max_cal", "min_cal")
      #colnames(outData01$wiso_subset_cal[[j]][[k]]) <- c("timeBgn","timeEnd","mean","min","max","vari","numSamp","meanCor","minCor","maxCor")
      #######################################################
      
      #create temporary table to combine results from both methods in NEON format
      tmpData <- data.frame(
        #provided best estimated; replace raw data with the correction
        mean = outData01$wiso_subset_cal[[j]][[k]]$mean_cal,
        min = outData01$wiso_subset_cal[[j]][[k]]$min_cal,
        max = outData01$wiso_subset_cal[[j]][[k]]$max_cal,
        vari = NaN, #vari not calculate after applying calibration (should talk to Rich)
        numSamp = outData01$wiso_subset_cal[[j]][[k]]$numSamp,
        #correction data
        meanCor = outData01$wiso_subset_cal[[j]][[k]]$mean_cal,
        minCor = outData01$wiso_subset_cal[[j]][[k]]$min_cal,
        maxCor = outData01$wiso_subset_cal[[j]][[k]]$max_cal,
        #raw/non corrected data
        meanRaw = outData01$wiso_subset_cal[[j]][[k]]$mean,
        minRaw = outData01$wiso_subset_cal[[j]][[k]]$min,
        maxRaw = outData01$wiso_subset_cal[[j]][[k]]$max,
        variRaw = outData01$wiso_subset_cal[[j]][[k]]$vari,
        timeBgn = outData01$wiso_subset_cal[[j]][[k]]$timeBgn,
        timeEnd = outData01$wiso_subset_cal[[j]][[k]]$timeEnd
      )
      #replace NA value with NaN
      tmpData[is.na(tmpData)] <- NaN
      
      outData$wiso_subset_cal[[j]][[k]] <- tmpData
    }
    else{
      #rearrange timeBgn & timeEnd to last column (NEON format)
      outData$wiso_subset_cal[[j]][[k]] <- cbind(outData01$wiso_subset_cal[[j]][[k]][,-which(names(outData01$wiso_subset_cal[[j]][[k]]) %in% c("timeBgn", "timeEnd"))],
                                                 timeBgn = outData01$wiso_subset_cal[[j]][[k]]$timeBgn, 
                                                 timeEnd = outData01$wiso_subset_cal[[j]][[k]]$timeEnd)
    }#end else
  }
  names(outData$wiso_subset_cal[[j]]) <- names(outData01$wiso_subset_cal[[j]])
}

#create a new cal_df table for each calibration method
outData$cal_df <- cbind(outData01$cal_df[,-which(names(outData01$cal_df) %in% c("timeBgn", "timeEnd"))],
                             timeBgn = outData01$cal_df$timeBgn, 
                             timeEnd = outData01$cal_df$timeEnd)

#change column names to NEON terms
colnames(outData$cal_df) <- c("slp18O", "ofst18O", "rsq18O", "cvLoo18O", "cv5Mae18O", "cv5Rmse18O",
                                   "slp2H", "ofst2H", "rsq2H", "cvLoo2H", "cv5Mae2H", "cv5Rmse2H",
                                   "timeBgn", "timeEnd")

#grab center day data
rlog$info(paste0("Grabing ", dateCntr, " data"))
dataDateCntr <- outData
#get start and end time
timeBgn <- base::as.POSIXct(paste(dateCntr, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
timeEnd   <- base::as.POSIXct(paste(dateCntr, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#convert NEON time to POSIXct

#adding 1st column with NA when number of row = 0
if(nrow(dataDateCntr$cal_df) == 0){
  dataDateCntr$cal_df[1,] <- rep(NA, ncol(dataDateCntr$cal_df))
} else{
  dataDateCntr$cal_df <- dataDateCntr$cal_df
}

#replacing timeBgn and timeEnd when all values in dataframe are NA (timeBgn and timeEnd = "NA-NA-NATNA:NA:NA")
if(all(is.na((dataDateCntr$cal_df[,-which(names(dataDateCntr$cal_df) %in% c("timeBgn", "timeEnd"))])))){
  dataDateCntr$cal_df$timeBgn <- paste0(dateCntr, "T00:00:00.000Z")
  dataDateCntr$cal_df$timeEnd <- paste0(dateCntr, "T23:59:59.000Z")
} else{
  dataDateCntr$cal_df$timeBgn <- as.POSIXct(paste(dataDateCntr$cal_df$timeBgn, " ", "00:00:00.00", sep=""), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
  dataDateCntr$cal_df$timeBgn <- base::as.POSIXct(paste(dataDateCntr$cal_df$timeBgn, " ", "00:00:00.001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  dataDateCntr$cal_df$timeEnd <- as.POSIXct(dataDateCntr$cal_df$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
  #subset center day data for cal_df table
  dataDateCntr$cal_df <- dataDateCntr$cal_df[which(dataDateCntr$cal_df$timeEnd >= timeBgn & dataDateCntr$cal_df$timeBgn <= timeEnd),]
  #convert time back to NEON hdf5 time
  dataDateCntr$cal_df$timeBgn <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$cal_df$timeBgn)
  dataDateCntr$cal_df$timeEnd <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$cal_df$timeEnd)
}#end if



#subset center day data for ciso_subset_cal
for(j in names(dataDateCntr$ciso_subset_cal)) {
  for (k in names (dataDateCntr$ciso_subset_cal[[j]])){
    # #adding 1st column with NA when number of row = 0
    # if(nrow(dataDateCntr$ciso_subset_cal[[j]][[k]]) == 0){
    #   dataDateCntr$ciso_subset_cal[[j]][[k]][1,] <- rep(NA, ncol( dataDateCntr$ciso_subset_cal[[j]][[k]]))
    # } else{
    #   dataDateCntr$ciso_subset_cal[[j]][[k]] <- dataDateCntr$ciso_subset_cal[[j]][[k]]
    # }
    #replace empty dataframe with NA
    if(all(is.na((dataDateCntr$ciso_subset_cal[[j]][[k]][,-which(names(dataDateCntr$ciso_subset_cal[[j]][[k]]) %in% c("timeBgn", "timeEnd"))])))){
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <- paste0(dateCntr, "T00:00:00.000Z")
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd <- paste0(dateCntr, "T23:59:59.000Z")
    }else{
      #convert NEON time to POSIXct
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <- as.POSIXct(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd <- as.POSIXct(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", origin = "1970-01-01 00:00:00")
      #subset data
      dataDateCntr$ciso_subset_cal[[j]][[k]] <-dataDateCntr$ciso_subset_cal[[j]][[k]][which(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd >= timeBgn & dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <= timeEnd),]
      #convert time back to NEON hdf5 time
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn <- NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeBgn)
      dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd <-  NEONiso:::convert_POSIXct_to_NEONhdf5_time(dataDateCntr$ciso_subset_cal[[j]][[k]]$timeEnd) 
    }#end else
  }#end loop k
}#end loop j


#Writing data to HDF5 ########################################################
#create list to hold data
rlog$info("Writing data to HDF5...")
data <- list()

#extract data from expanded hdf5 file for the center day
data$Expd <- eddy4R.base::def.hdf5.extr(FileInp = Para$FileName$EcseExpd)
#extract data from basic hdf5 file for the center day
data$Basc <- eddy4R.base::def.hdf5.extr(FileInp = Para$FileName$EcseBasc)

#replace isoH2o calibrate data
for(j in names(dataDateCntr$wiso_subset_cal)) {
  for (k in c("dlta18OH2o", "dlta2HH2o")){
    var <- paste0("/",Para$Flow$Loc, "/dp01/data/isoH2o","/",j,"/",k)
    data$Expd$listData[[var]] <- dataDateCntr$wiso_subset_cal[[j]][[k]]
    data$Basc$listData[[var]] <- dataDateCntr$wiso_subset_cal[[j]][[k]]
    #report only mean, min, max, vari, and numSamp (results from Bowling method in this case) for basic file
    data$Basc$listData[[var]] <- dataDateCntr$wiso_subset_cal[[j]][[k]][,which(names(dataDateCntr$wiso_subset_cal[[j]][[k]]) %in% c("mean", "min", "max", "vari","numSamp","timeBgn", "timeEnd"))]
    
    #unit re-assignment (only expanded files)
    #mean,min,max,vari,numSamp,
    #meanCor,minCor,maxCor,
    #meanRaw,minRaw,maxRaw,variRaw,timeBgn,timeEnd
    data$Expd$listAttr[[var]]$unit <-c("permill","permill","permill","permill2","NA",
                                       "permill","permill","permill", 
                                       "permill","permill","permill","permill2","NA","NA")
    
  }#end k loop
}#end j loop

#write out to hdf5
tmp <- eddy4R.base::def.hdf5.extr(FileInp = NULL,
                                  rpt = data$Expd,
                                  FileOut = base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, ".", dateCntr, ".", "expanded.h5"))
tmp <- eddy4R.base::def.hdf5.extr(FileInp = NULL,
                                  rpt = data$Basc,
                                  FileOut = base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, ".", dateCntr, ".", "basic.h5"))

rm(tmp)

#writing calibration parameters
rlog$info("Writing calibration parameters...")

for (packIdx in c("expanded.h5", "basic.h5")){
  outname <- base::paste0(Para$Flow$DirOut,"/",Para$Flow$FileOutBase, ".", dateCntr, ".", packIdx)  
  rhdf5::h5createGroup(outname,base::paste0("/",Para$Flow$Loc, "/dp01/data/isoH2o/calData"))
  
  fid <- rhdf5::H5Fopen(outname)
  calLoc <- rhdf5::H5Gopen(fid, paste0("/", Para$Flow$Loc, "/dp01/data/isoH2o/calData"))
  
  rhdf5::h5writeDataset(obj = dataDateCntr$cal_df_Bowl,
                        h5loc = calLoc,
                        name = "calData",
                        DataFrameAsCompound = TRUE)
  #writing units
  #"slp18O", "ofst18O", "rsq18O", "cvLoo18O", "cv5Mae18O", "cv5Rmse18O",
  #slp2H, "ofst2H", "rsq2H", "cvLoo2H", "cv5Mae2H", "cv5Rmse2H"
  #timeBgn", "timeEnd"
  #using NA for now; need to update
  unitCal <- c("NA", "permill", "NA", "permill", "permill", "permill",
                "NA", "permill", "NA", "permill", "permill", "permill",
                "NA", "NA")
  
  #open calDataBowl dataframe
  calLocBowl <- rhdf5::H5Dopen(calLoc, "calData")
  rhdf5::h5writeAttribute(unitCal, h5obj = calLocBowl, name = "unit")
  
  
  rhdf5::H5Gclose(calLoc)
  
  # close the group and the file
  rhdf5::H5Fclose(fid)
  rhdf5::h5closeAll()
}#end loop packIdx

rlog$info(paste0("dataset ", dateCntr, ": HDF5 generation complete!"))

#output csv files for diagnosis purpose
if (Para$Flow$Meth == "dflt") {
  #VersDp output version
  Para$Flow$VersDp <- paste0(Para$Flow$VersDp, "_", format(Sys.time(), "%Y%m%d_%H%M%S_%Z"))
  # create directory
  dir.create(paste0(Para$Flow$DirOut, "/", Para$Flow$Loc, "/", Para$Flow$VersDp), showWarnings = FALSE, recursive = TRUE)
  
  for(j in names(dataDateCntr$wiso_subset_cal)) {
    for (k in c("dlta18OH2o", "dlta2HH2o")){
      var <- paste0("/",Para$Flow$Loc, "/dp01/data/isoH2o","/",j,"/",k)
      
      #write to file
      utils::write.csv(data$Expd$listData[[var]], file = paste0(Para$Flow$DirOut, "/", Para$Flow$Loc, "/", Para$Flow$VersDp, "/", "dp01", "-", "isoH2o", "-",j, "-",k,".csv"), col.names = FALSE,
                       na = "NaN", row.names = FALSE, quote=FALSE)
    }}
  #write calibration parameter
  utils::write.csv(dataDateCntr$cal_df, file = paste0(Para$Flow$DirOut, "/", Para$Flow$Loc, "/", Para$Flow$VersDp, "/", "cal_df.csv"), col.names = FALSE,
                   na = "NaN", row.names = FALSE, quote=FALSE)
  
}#end creating CSV file





#######################################################################################################
##TEST RUN line by line for calibrate_carbon() 
#need to source extract_carbon_calibration_data() so the results will be match with 0.6.1 version
#source('~/eddy/docker/NEONiso-NDurden/R/reference_data_extraction.R')

inname <- nameFile
outname <- NULL
site <- Para$Flow$Loc
method <- c("Bowling_2003", "linreg")[1]
calibration_half_width <- 0.5
force_cal_to_beginning <- TRUE
force_cal_to_end <- TRUE
gap_fill_parameters <- FALSE
filter_ambient <- TRUE
r2_thres <- 0.95
correct_refData <- TRUE
write_to_file <- FALSE
remove_known_bad_months <- TRUE
plot_regression_data <- FALSE
plot_directory <- NULL
avg <- 6




