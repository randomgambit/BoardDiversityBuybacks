#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen
#  Dual licensed under the MIT or GPL Version 2 licenses.

##########################################################################################
# Creates the raw data for the buybacks diversity paper
##########################################################################################

##########################################################################################
# THE FIRST PART OF THE CODE IS STANDARD FOR ALL BUYBACK PROJECTS - TILL THE DATA FILTER FILE SOURCING
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

load("../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")
BUYBACK_DATA_PROJECT = BUYBACK_DATA; ISSUERS_DATA_PROJECT = ISSUERS_DATA
rm("BUYBACK_DATA","ISSUERS_DATA")

#############################################################################################################################
# Add the U-Index, depending on continuous_valuation_index (default is continuous_valuation_index=0) in Paper_global_parameters.R
# (Should standardize this, move it in the shared "buyback" dataset, and not run it again)
#############################################################################################################################

if (continuous_valuation_index){
  BUYBACK_DATA_PROJECT$Valuation_Index = 
    ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score), NA, (1-BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score), NA, (1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score), NA, BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score)
  
  ISSUERS_DATA_PROJECT$Valuation_Index = 
    ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$recent_performance_score), NA, (1-ISSUERS_DATA_PROJECT$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap_score), NA, (1-ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME_score), NA, ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME_score)
} else {
  # Make the U-Index scores
  BUYBACK_DATA_PROJECT$Performance_used <- ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score), NA, ceiling(5*(1-BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score)))
  BUYBACK_DATA_PROJECT$Performance_used[BUYBACK_DATA_PROJECT$Performance_used==0] <- 1
  BUYBACK_DATA_PROJECT$Size_used <- ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score), NA, ceiling(5*(1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score)))
  BUYBACK_DATA_PROJECT$Size_used[BUYBACK_DATA_PROJECT$Size_used==0] <- 1
  # USE OUR BE.ME, NOT THE FF
  BUYBACK_DATA_PROJECT$BEME_used <- ifelse(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score), NA, ceiling(5*BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score))
  BUYBACK_DATA_PROJECT$BEME_used[BUYBACK_DATA_PROJECT$BEME_used==0] <- 1
  BUYBACK_DATA_PROJECT$Valuation_Index = BUYBACK_DATA_PROJECT$Performance_used + BUYBACK_DATA_PROJECT$Size_used + BUYBACK_DATA_PROJECT$BEME_used 
  
  ISSUERS_DATA_PROJECT$Performance_used <- ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$recent_performance_score), NA, ceiling(5*(1-ISSUERS_DATA_PROJECT$DATASET$CRSP$recent_performance_score)))
  ISSUERS_DATA_PROJECT$Performance_used[ISSUERS_DATA_PROJECT$Performance_used==0] <- 1
  ISSUERS_DATA_PROJECT$Size_used <- ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap_score), NA, ceiling(5*(1-ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap_score)))
  ISSUERS_DATA_PROJECT$Size_used[ISSUERS_DATA_PROJECT$Size_used==0] <- 1
  # USE OUR BE.ME, NOT THE FF
  ISSUERS_DATA_PROJECT$BEME_used <- ifelse(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME_score), NA, ceiling(5*ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME_score))
  ISSUERS_DATA_PROJECT$BEME_used[ISSUERS_DATA_PROJECT$BEME_used==0] <- 1
  ISSUERS_DATA_PROJECT$Valuation_Index = ISSUERS_DATA_PROJECT$Performance_used + ISSUERS_DATA_PROJECT$Size_used + ISSUERS_DATA_PROJECT$BEME_used 
  
}

#############################################################################################################################
# All the data filters are done in here, except the boardex based ones
#############################################################################################################################

source("filter_bbdiversity_data.R")

# This is how we add new variables (e.g. see buybacks shared dataset creation)
get_feature_from_feature_datamatrix <- function(firm_characteristic_monthly, THE_DATASET,useonly_data,event_permno,check_only){
  rownames(firm_characteristic_monthly) <- str_sub(rownames(firm_characteristic_monthly), start=1,end=7)
  sapply(1:length(THE_DATASET$DATASET$SDC$permno), function(i) ifelse(check_only[i] & sum(!is.na(firm_characteristic_monthly[useonly_data[[i]], event_permno[i]])), tail(firm_characteristic_monthly[useonly_data[[i]], event_permno[i]][!is.na(firm_characteristic_monthly[useonly_data[[i]], event_permno[i]])],1), NA))
}

#############################################################################################################################
# Add the BOARDEX data
#############################################################################################################################

load("../FinanceData/created_monthly_data/GLOBAL_MONTHLY_DATABASE.Rdata")
load("../FinanceData/created_boardex_data/GLOBAL_BOARDEX_DATABASE.Rdata")
#load("../FinanceData/rawdata_execucomp/GLOBAL_CEO_DATABASE.Rdata")
cleanup = cleanup_boardex_global_database
# use last available non-NA 
#Fill with last available
for (iter in 1:length(GLOBAL_BOARDEX_DATABASE)){
  tmp <- apply(GLOBAL_BOARDEX_DATABASE[[iter]],2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
  rownames(tmp) <- rownames(GLOBAL_BOARDEX_DATABASE[[iter]])
  colnames(tmp) <- colnames(GLOBAL_BOARDEX_DATABASE[[iter]])
  tmp = tmp*(!is.na(GLOBAL_MONTHLY_DATABASE$returns_monthly))
  GLOBAL_BOARDEX_DATABASE[[iter]] <- tmp
}

# First get the scores for the networks
for (field in names(GLOBAL_BOARDEX_DATABASE))
  if (!is.null(dim(GLOBAL_BOARDEX_DATABASE[[field]])))
    GLOBAL_BOARDEX_DATABASE[[paste(field,"score",sep="_")]] <- get_cross_section_score(GLOBAL_BOARDEX_DATABASE[[field]],zero_special=T)


tmp_dates = as.Date(rownames(GLOBAL_BOARDEX_DATABASE$gender))

## Buybacks:
useonly_data = lapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, function(thedate) 
  which(tmp_dates< thedate  & thedate - tmp_dates < 370)) # Only most recent 370 days, UNTIL THE MONTH BEFORE THE EVENT
event_permno = as.character(BUYBACK_DATA_PROJECT$DATASET$SDC$permno)
event_month = str_sub(AddMonths(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,-1),start=1,end=7) # NOTE: WE TAKE THE PREVIOUS MONTH DATA (not important, as we only use the useonly_data)
check_only =  event_month %in% str_sub(rownames(GLOBAL_BOARDEX_DATABASE$gender),start=1,end=7) & event_permno %in% colnames(GLOBAL_BOARDEX_DATABASE$gender)
# Raw data first 
BUYBACK_DATA_PROJECT$DATASET$boardex <- list()
useonly = 1:length(names(GLOBAL_BOARDEX_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_BOARDEX_DATABASE[[iter]]
  BUYBACK_DATA_PROJECT$DATASET$boardex[[iter_i]] =  get_feature_from_feature_datamatrix(tmp,BUYBACK_DATA_PROJECT,useonly_data,event_permno,check_only)
  names(BUYBACK_DATA_PROJECT$DATASET$boardex)[iter_i]<- names(GLOBAL_BOARDEX_DATABASE)[iter]
}

## ISSUERS
useonly_data = lapply(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date, function(thedate) 
  which(tmp_dates< thedate  & thedate - tmp_dates < 370)) # Only most recent 370 days, UNTIL THE MONTH BEFORE THE EVENT
event_permno = as.character(ISSUERS_DATA_PROJECT$DATASET$SDC$permno)
event_month = str_sub(AddMonths(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date,-1),start=1,end=7) # NOTE: WE TAKE THE PREVIOUS MONTH DATA (not important, as we only use the useonly_data)
check_only =  event_month %in% str_sub(rownames(GLOBAL_BOARDEX_DATABASE$gender),start=1,end=7) & event_permno %in% colnames(GLOBAL_BOARDEX_DATABASE$gender)
# Raw data first 
ISSUERS_DATA_PROJECT$DATASET$boardex <- list()
useonly = 1:length(names(GLOBAL_BOARDEX_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_BOARDEX_DATABASE[[iter]]
  ISSUERS_DATA_PROJECT$DATASET$boardex[[iter_i]] =  get_feature_from_feature_datamatrix(tmp,ISSUERS_DATA_PROJECT,useonly_data,event_permno,check_only)
  names(ISSUERS_DATA_PROJECT$DATASET$boardex)[iter_i]<- names(GLOBAL_BOARDEX_DATABASE)[iter]
}

rm("tmp","useonly_data","tmp_dates")

#############################################################################################################################
# Add the INSTITUTIONAL data
# (this can some time move in the "shared" buyback dataset:
# HOWEVER THIS PROJECT USES A SHORTED HISTORY OF INSTITUTIONAL DATA THAN, SAY, THE SHORT SELL PROJECT
#############################################################################################################################

load("../FinanceData/created_TR_institutional_holdings/GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE_SIMPLE.Rdata")
tmp_dates = as.Date(rownames(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE$Institutional.Ownership.Ratio.1))

for (field in names(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE))
  if (!is.null(dim(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE[[field]])))
    GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE[[paste(field,"score",sep="_")]] <- get_cross_section_score(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE[[field]],zero_special=T)

#Buybacks:
BUYBACK_DATA_PROJECT$DATASET$institutional <- list()
useonly_data = lapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, function(thedate) 
  which(tmp_dates< thedate  & thedate - tmp_dates < 370))
event_permno = as.character(BUYBACK_DATA_PROJECT$DATASET$SDC$permno)
event_month = str_sub(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,start=1,end=7)
check_only =  event_month %in% str_sub(rownames(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE$Institutional.Ownership.Ratio.1),start=1,end=7) & event_permno %in% colnames(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE$Institutional.Ownership.Ratio.1)
useonly = 1:length(names(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE[[iter]]
  BUYBACK_DATA_PROJECT$DATASET$institutional[[iter_i]] =  get_feature_from_feature_datamatrix(tmp,BUYBACK_DATA_PROJECT,useonly_data,event_permno,check_only)
  names(BUYBACK_DATA_PROJECT$DATASET$institutional)[iter_i]<- names(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE)[iter]
}

#Issuers:
ISSUERS_DATA_PROJECT$DATASET$institutional <- list()
useonly_data = lapply(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date, function(thedate) 
  which(tmp_dates< thedate  & thedate - tmp_dates < 370))
event_permno = as.character(ISSUERS_DATA_PROJECT$DATASET$SDC$permno)
event_month = str_sub(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date,start=1,end=7)
check_only =  event_month %in% str_sub(rownames(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE$Institutional.Ownership.Ratio.1),start=1,end=7) & event_permno %in% colnames(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE$Institutional.Ownership.Ratio.1)
useonly = 1:length(names(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE))
for (iter_i in 1:length(useonly)){
  iter = useonly[iter_i]
  cat(iter,", ")
  tmp = GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE[[iter]]
  ISSUERS_DATA_PROJECT$DATASET$institutional[[iter_i]] =  get_feature_from_feature_datamatrix(tmp,ISSUERS_DATA_PROJECT,useonly_data,event_permno,check_only)
  names(ISSUERS_DATA_PROJECT$DATASET$institutional)[iter_i]<- names(GLOBAL_INSTITUTIONAL_OWNERSHIP_DATABASE)[iter]
}


##########################################################################################
# Finally save only what is needed
#boardex_dates = intersect(str_sub(colnames(GLOBAL_BOARDEX_DATABASE$gender),start=1,end=7), str_sub(rownames(BUYBACK_DATA_PROJECT$Risk_Factors_Monthly), start=1,end=7))
save(BUYBACK_DATA_PROJECT, ISSUERS_DATA_PROJECT,file = "../FinanceData/created_projects_datasets/bb_diversity.Rdata")


