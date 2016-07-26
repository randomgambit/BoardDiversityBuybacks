#  Copyright 2016, INSEAD
#  by T. Evgeniou, Theo Vermaelen
#  Dual licensed under the MIT or GPL Version 2 licenses.

##########################################################################################
# Creates the data used in the .Rnw file
##########################################################################################

##########################################################################################
# THE FIRST PART OF THE CODE IS STANDARD FOR ALL BUYBACK PROJECTS - TILL THE DATA FILTER FILE SOURCING
##########################################################################################

if (!exists("generate_all_appendices")){
  rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
  project_var_name = "gender_score" 
  project.main.IV.variable.name = "Gender (Score)"
}

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")
library("sampleSelection")
winsorize <- function(r) {r0 = r[!is.na(r)]; minval = quantile(r[!is.na(r)],0.01); maxval= quantile(r[!is.na(r)],0.99); r[!is.na(r)] <- ifelse(r[!is.na(r)] < minval | r[!is.na(r)] > maxval, NA, r[!is.na(r)]); r}

initial_vars = ls(all = TRUE) # takes time to save and load, so we save only what is needed at the end. 
# Project specific parameters
quantile_project = 0.5 # NOTE:if this is not 0.5 we need to change the code for project_relations below
quantile_simple = 0.2 # Quantile for defining high/low U-index etc simple categories
projectwindow = 365
project_var_name = project_var_name

#####
load("../FinanceData/created_projects_datasets/bb_diversity.Rdata") # PROJECT SPECIFIC DATA FILE TO LOAD
Risk_Factors_Monthly = BUYBACK_DATA_PROJECT$Risk_Factors_Monthly
Market_Monthly = BUYBACK_DATA_PROJECT$Market_Monthly

if (do.value.weight == 1){   
  value.weights_bb = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap
  value.weights_iss = ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap
} else {
  value.weights_bb = rep(1,length(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
  value.weights_iss = rep(1,length(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
}

BUYBACK_DATA_PROJECT$DATASET$DatesMonth <- create_dates_month(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, rownames(BUYBACK_DATA_PROJECT$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(BUYBACK_DATA_PROJECT$DATASET$DatesMonth) <- BUYBACK_DATA_PROJECT$DATASET$SDC$permno

ISSUERS_DATA_PROJECT$DATASET$DatesMonth <- create_dates_month(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date, rownames(ISSUERS_DATA_PROJECT$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(ISSUERS_DATA_PROJECT$DATASET$DatesMonth) <- ISSUERS_DATA_PROJECT$DATASET$SDC$permno

###############################################################################################
# SOME PROJECT SPECIFIC DATA CLEANUP
###############################################################################################

to_remove = which(is.na(BUYBACK_DATA_PROJECT$DATASET$boardex$gender))
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  BUYBACK_DATA_PROJECT$BEME_used <- BUYBACK_DATA_PROJECT$BEME_used[-to_remove]
  BUYBACK_DATA_PROJECT$Performance_used <- BUYBACK_DATA_PROJECT$Performance_used[-to_remove]
  BUYBACK_DATA_PROJECT$Size_used <- BUYBACK_DATA_PROJECT$Size_used[-to_remove]
  BUYBACK_DATA_PROJECT$Valuation_Index <- BUYBACK_DATA_PROJECT$Valuation_Index[-to_remove]
  
  BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly <- BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA_PROJECT$DATASET$SDC <- BUYBACK_DATA_PROJECT$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$CRSP))  BUYBACK_DATA_PROJECT$DATASET$CRSP[[field]] <- BUYBACK_DATA_PROJECT$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$boardex))  BUYBACK_DATA_PROJECT$DATASET$boardex[[field]] <- BUYBACK_DATA_PROJECT$DATASET$boardex[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA_PROJECT$DATASET$institutional))  BUYBACK_DATA_PROJECT$DATASET$institutional[[field]] <- BUYBACK_DATA_PROJECT$DATASET$institutional[[field]][-to_remove]
  for(field1 in ls(BUYBACK_DATA_PROJECT$DATASET$ibes))  
    for (field in ls(BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]])) 
      BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]][[field]]<- BUYBACK_DATA_PROJECT$DATASET$ibes[[field1]][[field]][-to_remove]
  rm("field","field1")
}
BUYBACK_DATA_PROJECT$cleanupMissingSomeValues = length(to_remove)

to_remove = which(is.na(ISSUERS_DATA_PROJECT$DATASET$boardex$gender))
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  ISSUERS_DATA_PROJECT$BEME_used <- ISSUERS_DATA_PROJECT$BEME_used[-to_remove]
  ISSUERS_DATA_PROJECT$Performance_used <- ISSUERS_DATA_PROJECT$Performance_used[-to_remove]
  ISSUERS_DATA_PROJECT$Size_used <- ISSUERS_DATA_PROJECT$Size_used[-to_remove]
  ISSUERS_DATA_PROJECT$Valuation_Index <- ISSUERS_DATA_PROJECT$Valuation_Index[-to_remove]
  
  ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly <- ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,-to_remove]
  ISSUERS_DATA_PROJECT$DATASET$SDC <- ISSUERS_DATA_PROJECT$DATASET$SDC[-to_remove,]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$CRSP))  ISSUERS_DATA_PROJECT$DATASET$CRSP[[field]] <- ISSUERS_DATA_PROJECT$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$boardex))  ISSUERS_DATA_PROJECT$DATASET$boardex[[field]] <- ISSUERS_DATA_PROJECT$DATASET$boardex[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA_PROJECT$DATASET$institutional))  ISSUERS_DATA_PROJECT$DATASET$institutional[[field]] <- ISSUERS_DATA_PROJECT$DATASET$institutional[[field]][-to_remove]
  for(field1 in ls(ISSUERS_DATA_PROJECT$DATASET$ibes))  
    for (field in ls(ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]])) 
      ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]][[field]]<- ISSUERS_DATA_PROJECT$DATASET$ibes[[field1]][[field]][-to_remove]
  rm("field","field1")
}
ISSUERS_DATA_PROJECT$cleanupMissingSomeValues = length(to_remove)

####################################################################################
# These are one by one the tables in the .Rnw. 
####################################################################################

# THIS IS WHAT IS USED FROM NOW ON AS DEFAULT
project_data_subset = BUYBACK_DATA_PROJECT$DATASET$boardex
project_var_used = project_data_subset[[project_var_name]]
project_data_subset_iss = ISSUERS_DATA_PROJECT$DATASET$boardex
project_var_used_iss = project_data_subset_iss[[project_var_name]]

############################################################################
# PREPARE ALL VARIABLES
############################################################################

# Some event categoeries (all T/F vectors)
company_subset_undervalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index > quantile(BUYBACK_DATA_PROJECT$Valuation_Index, 1-quantile_simple)
company_subset_overvalued_bb = BUYBACK_DATA_PROJECT$Valuation_Index < quantile(BUYBACK_DATA_PROJECT$Valuation_Index,quantile_simple)
High_perf_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score  > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score,1-quantile_simple)
Low_perf_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score  < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score,quantile_simple)
High_Size_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score  > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score,1-quantile_simple)
Low_Size_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score  < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score,quantile_simple)
High_Idiosyncr_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, quantile_simple)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score, 1-quantile_simple)
High_IVOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, 1-quantile_simple)
Low_IVOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score, quantile_simple)
High_VOL_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, 1-quantile_simple)
Low_VOL_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score, quantile_simple)
High_marketbeta_eventsBB = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score > quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, 1-quantile_simple)
Medium_marketbeta_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score >= quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, quantile_simple) & BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score <= quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, 1-quantile_simple)
Low_marketbeta_eventsBB  = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score < quantile(BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score, quantile_simple)
tmp = BUYBACK_DATA_PROJECT$DATASET$CRSP$leverage_lt_over_lt_plus_e
High_LEV_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_LEV_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
tmp = BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec_score
High_EPS_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_EPS_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
company_subset_undervalued_iss = ISSUERS_DATA_PROJECT$Valuation_Index > quantile(ISSUERS_DATA_PROJECT$Valuation_Index, 1-quantile_simple)
company_subset_overvalued_iss = ISSUERS_DATA_PROJECT$Valuation_Index < quantile(ISSUERS_DATA_PROJECT$Valuation_Index,quantile_simple)
BUYBACK_DATA_PROJECT$EU_index = sapply(1:length(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})
high_EU_bb = BUYBACK_DATA_PROJECT$EU_index > 3
low_EU_bb = BUYBACK_DATA_PROJECT$EU_index < 3
buybacks.events.past2years = 1*(BUYBACK_DATA_PROJECT$DATASET$CRSP$buybacks_events_past2years !=0)
rm("tmp")
#Recommendation score: 1. Strong Buy, 2. Buy, 3. Hold, 4. Underperform, 5. Sell
downgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) > scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
not_downgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) <= scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
upgraded_events = !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec) < scrub(BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)


#### Continuous variables now
Firm_size = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap
Firm_size_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap_score
Prior_R = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance
Prior_R_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$recent_performance_score
BEME = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME
BEME_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME_score
U_index = BUYBACK_DATA_PROJECT$Valuation_Index
EU_index = BUYBACK_DATA_PROJECT$EU_index
Vol_raw = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol
Vol_raw_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$pre_vol_Score
Idiosyncratic = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL
Idiosyncratic_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$IVOL_score
One_m_Rsqr = 1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq
One_m_Rsqr_score = 1-BUYBACK_DATA_PROJECT$DATASET$CRSP$Rsq_score
market_beta = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta
market_beta_score = BUYBACK_DATA_PROJECT$DATASET$CRSP$market_beta_score
Analyst_disagreement = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_disagreement_score)
Analyst_coverage = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_coverage)
Analyst_coverage_score = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$analyst_coverage_score)
Event.Size = BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size
buybacks.events.past2years = 1*(BUYBACK_DATA_PROJECT$DATASET$CRSP$buybacks_events_past2years !=0)
Total.Payout = (BUYBACK_DATA_PROJECT$DATASET$CRSP$Total_Payout)
lagged.dividend.payout.ratio = BUYBACK_DATA_PROJECT$DATASET$CRSP$divident_payout_ratio
lagged.dividend.payout.ratio[scrub(lagged.dividend.payout.ratio) < 0 | scrub(lagged.dividend.payout.ratio) > 100] <- NA
lagged.dividend.payout.ratio = (lagged.dividend.payout.ratio) 
Leverage = (BUYBACK_DATA_PROJECT$DATASET$CRSP$leverage_d_over_d_plus_e)
operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$operating_income)
std.operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$std_operating_income)
non.operating.income = (BUYBACK_DATA_PROJECT$DATASET$CRSP$non_operating_income)
liquid.assets = (BUYBACK_DATA_PROJECT$DATASET$CRSP$liquid_assets)
price.earnings.ratio = (BUYBACK_DATA_PROJECT$DATASET$CRSP$price_earnings_ratio)
capital.expenditures = (BUYBACK_DATA_PROJECT$DATASET$CRSP$capital_expenditures)
profitability = (BUYBACK_DATA_PROJECT$DATASET$CRSP$profitability)
# Institutional - still not standard across projects
Institutional = (BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1)
Institutional[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional_score = (BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1_score)
Institutional_score[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional.number = (BUYBACK_DATA_PROJECT$DATASET$institutional$num.institutional.investors)
Institutional.number[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
Institutional.number_score = (BUYBACK_DATA_PROJECT$DATASET$institutional$num.institutional.investors_score)
Institutional.number_score[scrub(BUYBACK_DATA_PROJECT$DATASET$institutional$Institutional.Ownership.Ratio.1) >= 100] <- NA
tmp = Institutional_score
High_inst_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_inst_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
rm("tmp")


# THESE ARE THE VARIABLES WE USE IN THE DATA SUMMARY STATS
all_characteristics_continuous_summary = cbind(
  buybacks.events.past2years,
  Firm_size,
  100*Prior_R,
  BEME,
  U_index,
  EU_index,
  100*Vol_raw,
  One_m_Rsqr,
  Event.Size,
  Analyst_coverage,
  Total.Payout,
  lagged.dividend.payout.ratio,
  Leverage,
  profitability,
  operating.income,
  std.operating.income,
  non.operating.income,
  liquid.assets,
  price.earnings.ratio,
  capital.expenditures,
  Institutional,
  Institutional.number
)
colnames(all_characteristics_continuous_summary) <- c(
  "Announced Repurchace in Previous 2 Years (0/1)",
  "Market Cap. (Mil.)", 
  "Prior Returns",
  "BE/ME", 
  "U-index",
  "EU-index",
  "Volatility (Percent)", 
  "One minus Rsq",
  "Percent Shares",
  "Analyst Coverage",
  "Total Payout in Event Year before Event",
  "Lag Dividend Payout Ratio",
  "Leverage","Profitability (ROA)",
  "Operating Income (Percent assets)", 
  "std Operating Income", 
  "Non-Operating Income (Percent assets)",
  "Liquid Assets (Percent assets)",
  "Price/Earnings Ratio",
  "Capital Expenditures (Percent assets)",
  "Institutional Holdings",
  "Number of Institutions"
)

# THESE ARE THE VARIABLES WE USE IN ALL REGRESSIONS
all_characteristics_continuous = cbind(
  buybacks.events.past2years,
  Firm_size_score,
  Prior_R_score,BEME_score,
  U_index,
  EU_index,
  Vol_raw_score,
  One_m_Rsqr_score,
  Event.Size,
  Analyst_coverage_score,
  Total.Payout,
  lagged.dividend.payout.ratio,
  Leverage,
  profitability,
  operating.income,
  std.operating.income,
  non.operating.income,
  liquid.assets,
  price.earnings.ratio,
  capital.expenditures,
  Institutional_score,
  Institutional.number_score
)
colnames(all_characteristics_continuous) <- c(
  "Announced Repurchace in Previous 2 Years (0/1)",
  "Market Cap. (Score)", 
  "Prior Returns (Score)",
  "BE/ME (Score)", 
  "U-index",
  "EU-index",
  "Volatility (Score)", 
  "One minus Rsq (Score)",
  "Percent Shares",
  "Analyst Coverage (Score)",
  "Total Payout in Event Year before Event",
  "Lag Dividend Payout Ratio",
  "Leverage","Profitability (ROA)",
  "Operating Income (Percent assets)", 
  "std Operating Income", 
  "Non-Operating Income (Percent assets)",
  "Liquid Assets (Percent assets)",
  "Price/Earnings Ratio",
  "Capital Expenditures (Percent assets)",
  "Institutional Holdings (Score)",
  "Number of Institutions (Score)"
)
##################################################################################################################
## ADD ANY PROJECT SPECIFIC VARIABLES TO all_characteristics_continuous
##################################################################################################################

# ADD THE PROJECT SPECIFIC VARIABLES TO all_characteristics_continuous

tmp = matrix(100*BUYBACK_DATA_PROJECT$DATASET$boardex$gender, ncol=1)
colnames(tmp) <- "Female Percent in Board"
all_characteristics_continuous_summary = cbind(all_characteristics_continuous_summary,tmp)

tmp = matrix(BUYBACK_DATA_PROJECT$DATASET$boardex[[project_var_name]], ncol=1)
colnames(tmp) <- project.main.IV.variable.name
all_characteristics_continuous = cbind(all_characteristics_continuous,tmp)

###############################################################################################
# Histograms
###############################################################################################

project_dates = str_sub(rownames(Risk_Factors_Monthly)[which(as.Date(rownames(Risk_Factors_Monthly)) >= min(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date) & as.Date(rownames(Risk_Factors_Monthly)) <= max(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date))])
project_dates = paste(project_dates,"01",sep="-")
yearlag = 0
bb_years = format(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, "%Y")
hist_data = project_data_subset[[project_var_name]]
###
yearly_features_count <- sapply(unique(format(as.Date(project_dates), "%Y")), function(i){
  tmp2 = hist_data[bb_years==as.character(as.numeric(i)+yearlag)]
  length(tmp2[!is.na(tmp2)])
})
names(yearly_features_count)<- unique(format(as.Date(project_dates), "%Y"))
###
yearly_features_mean <- sapply(unique(format(as.Date(project_dates), "%Y")), function(i){
  tmp2 = hist_data[bb_years==as.character(as.numeric(i)+yearlag)]
  non_na_mean(tmp2)
})
names(yearly_features_mean)<- unique(format(as.Date(project_dates), "%Y"))

datasummaryBB = rbind(
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) & BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) & BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size) | BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) & BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap) | BUYBACK_DATA_PROJECT$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME[BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME) & BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME[BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME) & BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME!=0]),sum(is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME >= 1e20) | BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME ==0 | BUYBACK_DATA_PROJECT$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryBB) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryBB)[ncol(datasummaryBB)-1] <- "std"
colnames(datasummaryBB)[ncol(datasummaryBB)] <- "Missing"

datasummaryISS = rbind(
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) & ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) & ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size) | ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) & ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) & ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap) | ISSUERS_DATA_PROJECT$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME[ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME) & ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME[ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME) & ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME!=0]),sum(is.na(ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME >= 1e20) | ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME ==0 | ISSUERS_DATA_PROJECT$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryISS) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryISS)[ncol(datasummaryISS)-1] <- "std"
colnames(datasummaryISS)[ncol(datasummaryISS)] <- "Missing"

########################################################################################################
# SINGLE SORT IRATS/CALENDAR
########################################################################################################
# WE USE THIS IN THE SINGLE SORTS
thefeature_single_sort = BUYBACK_DATA_PROJECT$DATASET$boardex$gender # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 

useonly_no_project_var = which(scrub(thefeature_single_sort) == 0 & !is.na(thefeature_single_sort))
no_project_var_all = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var], Risk_Factors_Monthly)
no_project_var = no_project_var_all$results
no_project_var_cal_all = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_no_project_var])
no_project_var_cal = no_project_var_cal_all$results

useonly_some_project_var = which(scrub(thefeature_single_sort) !=0 & !is.na(thefeature_single_sort))
some_project_var_all = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var], Risk_Factors_Monthly)
some_project_var = some_project_var_all$results
some_project_var_cal_all = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_some_project_var])
some_project_var_cal = some_project_var_cal_all$results

lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = some_project_var_all$results[,1]/some_project_var_all$results[,2]
no_project_var_minus_some_project_var = (no_project_var_all$results[,1] - some_project_var_all$results[,1])
tvalHL = no_project_var_minus_some_project_var/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, some_project_var_all$dfs -1))
no_project_var_minus_some_project_var = cbind(no_project_var_minus_some_project_var, tvalHL,pvalHL)
no_project_var_minus_some_project_var[nrow(no_project_var_minus_some_project_var),] <- 0

lowstd = no_project_var_cal_all$results[,1]/no_project_var_cal_all$results[,2]
highstd = some_project_var_cal_all$results[,1]/some_project_var_cal_all$results[,2]
no_project_var_minus_some_project_var_cal = (no_project_var_cal_all$results[,1] - some_project_var_cal_all$results[,1])
tvalHL = no_project_var_minus_some_project_var_cal/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(no_project_var_cal_all$dfs-1, some_project_var_cal_all$dfs -1))
no_project_var_minus_some_project_var_cal = cbind(no_project_var_minus_some_project_var_cal, tvalHL,pvalHL)
no_project_var_minus_some_project_var_cal[nrow(no_project_var_minus_some_project_var_cal),] <- 0

rm("no_project_var_all","no_project_var_cal_all","some_project_var_all","some_project_var_cal_all")

#######################################
#### SOME PROJECT SPECIFIC SINGLE SORTS

useonly_CEO_women = which((scrub(project_data_subset$CEO_female) != 0) & !is.na(project_data_subset$CEO_female))
CEO_women = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women], Risk_Factors_Monthly)$results
CEO_women_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_women])$results
###
useonly_CFO_women = which((scrub(project_data_subset$CFO_female) != 0) & !is.na(project_data_subset$CFO_female))
CFO_women = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women], Risk_Factors_Monthly)$results
CFO_women_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CFO_women])$results
###
useonly_senior_women = which(scrub(project_data_subset$other_senior_female) != 0 & !is.na(project_data_subset$other_senior_female))
senior_women  = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women], Risk_Factors_Monthly)$results
senior_women_cal  = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_senior_women])$results
###
useonly_CEO_men = which(scrub(project_data_subset$CEO_female) == 0 & !is.na(project_data_subset$CEO_female))
CEO_men = car_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men], Risk_Factors_Monthly)$results
CEO_men_cal = calendar_table(BUYBACK_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men], BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_men])$results

########################################################################################################
# DOUBLE SORT IRATS/CALENDAR
########################################################################################################
# WE USE THIS IN THE DOUBLE SORTS
thefeature_double_sort = project_var_used
tablename = "Div."
doublesort_method = "Complex"

# By U-Index and generic project split
BUYBACK_DATA_PROJECT$DATASET$SDC$project <- thefeature_double_sort
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_project,projectwindow,value.weights_bb,thefeature=thefeature_double_sort,method=doublesort_method,returnall = 1)
High_project_Uindex_eventsBB = tmp$High_feature_events; Low_project_Uindex_eventsBB = tmp$Low_feature_events
High_project_Uindex_BB  = tmp$High_feature; Low_project_Uindex_BB = tmp$Low_feature
High_project_Uindex_BB_Hedged  = tmp$High_feature_Hedged; Low_project_Uindex_BB_Hedged = tmp$Low_feature_Hedged
High_project_Uindex_BB48m = tmp$High_feature48m; Low_project_Uindex_BB48m  = tmp$Low_feature48m
High_project_Uindex_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Uindex_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"U-index",tablename)
project_Uindex_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_Uindex_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables")
# Get the basic low versus high U-index split
irats_under_tmp = tmp$feature_IRATStable
lowstd = irats_under_tmp$all_low$results[,1]/irats_under_tmp$all_low$results[,2]
highstd = irats_under_tmp$all_high$results[,1]/irats_under_tmp$all_high$results[,2]
HLdiff = -(irats_under_tmp$all_high$results[,1] - irats_under_tmp$all_low$results[,1])
tvalHL = HLdiff/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(irats_under_tmp$all_low$dfs-1, irats_under_tmp$all_high$dfs -1))
under_high_low = cbind(HLdiff, tvalHL,pvalHL)
colnames(under_high_low) <- c("Low-High Util.","t-stat","p-value") 
under_high_low[nrow(under_high_low),] <- 0
project_IRATStableBB = cbind(irats_under_tmp$all_low$results,irats_under_tmp$all_high$results,under_high_low)
#
irats_under_tmp = tmp$feature_IRATStable_cal
lowstd = irats_under_tmp$all_low$results[,1]/irats_under_tmp$all_low$results[,2]
highstd = irats_under_tmp$all_high$results[,1]/irats_under_tmp$all_high$results[,2]
HLdiff = -(irats_under_tmp$all_high$results[,1] - irats_under_tmp$all_low$results[,1])
tvalHL = HLdiff/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(irats_under_tmp$all_low$dfs-1, irats_under_tmp$all_high$dfs -1))
under_high_low = cbind(HLdiff, tvalHL,pvalHL)
colnames(under_high_low) <- c("Low-High Util.","t-stat","p-value") 
under_high_low[nrow(under_high_low),] <- 0
project_tableBB_cal = cbind(irats_under_tmp$all_low$results,irats_under_tmp$all_high$results,under_high_low)
rm("irats_under_tmp")
# Also make this into a T/F vector
High_project_eventsBB = rep(F, length(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date))
High_project_eventsBB[High_project_Uindex_eventsBB] <- TRUE
Low_project_eventsBB = rep(F, length(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date))
Low_project_eventsBB[Low_project_Uindex_eventsBB] <- TRUE
rm("tmp")
####
# EU index
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", high_EU_bb, low_EU_bb, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_EUindex_eventsBB = tmp$High_feature_events; Low_project_EUindex_eventsBB = tmp$Low_feature_events
High_project_EUindex_BB  = tmp$High_feature; Low_project_EUindex_BB = tmp$Low_feature
High_project_EUindex_BB_Hedged  = tmp$High_feature_Hedged; Low_project_EUindex_BB_Hedged = tmp$Low_feature_Hedged
High_project_EUindex_BB48m = tmp$High_feature48m; Low_project_EUindex_BB48m  = tmp$Low_feature48m
High_project_EUindex_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_EUindex_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"EU-index",tablename)
project_EUindex_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_EUindex_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables","tmp")
# By Vol
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", High_VOL_eventsBB, Low_VOL_eventsBB, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_Vol_eventsBB = tmp$High_feature_events; Low_project_Vol_eventsBB = tmp$Low_feature_events
High_project_Vol_BB  = tmp$High_feature; Low_project_Vol_BB = tmp$Low_feature
High_project_Vol_BB_Hedged  = tmp$High_feature_Hedged; Low_project_Vol_BB_Hedged = tmp$Low_feature_Hedged
High_project_Vol_BB48m = tmp$High_feature48m; Low_project_Vol_BB48m  = tmp$Low_feature48m
High_project_Vol_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Vol_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"Vol.",tablename)
project_Vol_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_Vol_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables","tmp")
# Idiosyncratic
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", High_Idiosyncr_eventsBB, Low_Idiosyncr_eventsBB, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_Idio_eventsBB = tmp$High_feature_events; Low_project_Idio_eventsBB = tmp$Low_feature_events
High_project_Idio_BB  = tmp$High_feature; Low_project_Idio_BB = tmp$Low_feature
High_project_Idio_BB_Hedged  = tmp$High_feature_Hedged; Low_project_Idio_BB_Hedged = tmp$Low_feature_Hedged
High_project_Idio_BB48m = tmp$High_feature48m; Low_project_Idio_BB48m  = tmp$Low_feature48m
High_project_Idio_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Idio_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"Idio.",tablename)
project_Idio_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_Idio_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables","tmp")
# Size
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", High_Size_eventsBB, Low_Size_eventsBB, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_Size_eventsBB = tmp$High_feature_events; Low_project_Size_eventsBB = tmp$Low_feature_events
High_project_Size_BB  = tmp$High_feature; Low_project_Size_BB = tmp$Low_feature
High_project_Size_BB_Hedged  = tmp$High_feature_Hedged; Low_project_Size_BB_Hedged = tmp$Low_feature_Hedged
High_project_Size_BB48m = tmp$High_feature48m; Low_project_Size_BB48m  = tmp$Low_feature48m
High_project_Size_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Size_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"Size",tablename)
project_Size_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_Size_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables","tmp")
# Performance
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", High_perf_eventsBB, Low_perf_eventsBB, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_perf_eventsBB = tmp$High_feature_events; Low_project_perf_eventsBB = tmp$Low_feature_events
High_project_perf_BB  = tmp$High_feature; Low_project_perf_BB = tmp$Low_feature
High_project_perf_BB_Hedged  = tmp$High_feature_Hedged; Low_project_perf_BB_Hedged = tmp$Low_feature_Hedged
High_project_perf_BB48m = tmp$High_feature48m; Low_project_perf_BB48m  = tmp$Low_feature48m
High_project_perf_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_perf_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"EU-index",tablename)
project_perf_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_perf_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables")
rm("tmp")
# Institutional
tmp = get_feature_results(BUYBACK_DATA_PROJECT$DATASET,"project", High_inst_eventsBB, Low_inst_eventsBB, quantile_project,projectwindow,value.weights_bb, thefeature = thefeature_double_sort,method= doublesort_method,returnall = 1)
High_project_Instit_eventsBB = tmp$High_feature_events; Low_project_Instit_eventsBB = tmp$Low_feature_events
High_project_Instit_BB  = tmp$High_feature; Low_project_Instit_BB = tmp$Low_feature
High_project_Instit_BB_Hedged  = tmp$High_feature_Hedged; Low_project_Instit_BB_Hedged = tmp$Low_feature_Hedged
High_project_Instit_BB48m = tmp$High_feature48m; Low_project_Instit_BB48m  = tmp$Low_feature48m
High_project_Instit_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Instit_BB_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"Instit.",tablename)
project_Instit_IRATStable_underBB = tmp_tables$IRATStable_underBB; project_Instit_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables","tmp")

##################################
### Some project specific analysis 

####### ISSUERS
#############################################
# Single sorts
thefeature_single_sort = ISSUERS_DATA_PROJECT$DATASET$boardex$gender # We don't use the score here as we want to find the 0 cases (score is not accurate for the 0s) 
###
useonly_no_project_var_iss = which(scrub(thefeature_single_sort) == 0 & !is.na(thefeature_single_sort))
no_project_var_all = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var_iss], Risk_Factors_Monthly)
no_project_var_iss = no_project_var_all$results
no_project_var_iss_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_no_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_no_project_var_iss], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly_no_project_var_iss])$results
###
useonly_some_project_var_iss = which(scrub(thefeature_single_sort) !=0 & !is.na(thefeature_single_sort))
some_project_var_all = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var_iss], Risk_Factors_Monthly)
some_project_var_iss = some_project_var_all$results
some_project_var_iss_cal = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_some_project_var_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_some_project_var_iss], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly_some_project_var_iss])$results
###
lowstd = no_project_var_all$results[,1]/no_project_var_all$results[,2]
highstd = some_project_var_all$results[,1]/some_project_var_all$results[,2]
no_project_var_minus_some_project_var_iss = (no_project_var_all$results[,1] - some_project_var_all$results[,1])
tvalHL = no_project_var_minus_some_project_var_iss/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(no_project_var_all$dfs-1, some_project_var_all$dfs -1))
no_project_var_minus_some_project_var_iss = cbind(no_project_var_minus_some_project_var_iss, tvalHL,pvalHL)
no_project_var_minus_some_project_var_iss[nrow(no_project_var_minus_some_project_var_iss),] <- 0
#############################################
useonly_CEO_women_iss = which((scrub(project_data_subset_iss$CEO_female) != 0) & !is.na(project_data_subset_iss$CEO_female))
CEO_women_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women_iss], Risk_Factors_Monthly)$results
CEO_women_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_women])$results
###
useonly_CFO_women_iss = which((scrub(project_data_subset_iss$CFO_female) != 0) & !is.na(project_data_subset_iss$CFO_female))
CFO_women_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women_iss], Risk_Factors_Monthly)$results
CFO_women_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CFO_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CFO_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CFO_women])$results
###
useonly_senior_women_iss = which(scrub(project_data_subset_iss$other_senior_female) != 0 & !is.na(project_data_subset_iss$other_senior_female))
senior_women_iss  = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women_iss], Risk_Factors_Monthly)$results
senior_women_cal_iss  = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_senior_women_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_senior_women_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_senior_women])$results
###
useonly_CEO_men_iss = which(scrub(project_data_subset_iss$CEO_female) == 0 & !is.na(project_data_subset_iss$CEO_female))
CEO_men_iss = car_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men_iss], Risk_Factors_Monthly)$results
CEO_men_cal_iss = calendar_table(ISSUERS_DATA_PROJECT$DATASET$returns_by_event_monthly[,useonly_CEO_men_iss], ISSUERS_DATA_PROJECT$DATASET$SDC$Event.Date[useonly_CEO_men_iss], Risk_Factors_Monthly,value.weights=value.weights_bb[useonly_CEO_men])$results
#############################################
# Double sorts
thefeature_double_sort_iss = project_var_used_iss
# By U-Index
ISSUERS_DATA_PROJECT$DATASET$SDC$project <- thefeature_double_sort_iss
# Note: some "warnings" appear due to the lack of data for the U-index split; as we don't use that we are ok
tmp = get_feature_results(ISSUERS_DATA_PROJECT$DATASET,"project", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_project,projectwindow,value.weights_iss,thefeature=thefeature_double_sort_iss,method=doublesort_method,returnall = 1)
High_project_Uindex_eventsISS = tmp$High_feature_events; Low_project_Uindex_eventsISS = tmp$Low_feature_events
High_project_Uindex_ISS  = tmp$High_feature; Low_project_Uindex_ISS = tmp$Low_feature
High_project_Uindex_ISS_Hedged  = tmp$High_feature_Hedged; Low_project_Uindex_ISS_Hedged = tmp$Low_feature_Hedged
High_project_Uindex_ISS48m = tmp$High_feature48m; Low_project_Uindex_ISS48m  = tmp$Low_feature48m
High_project_Uindex_ISS_Hedged48m  = tmp$High_feature_Hedged48m; Low_project_Uindex_ISS_Hedged48m = tmp$Low_feature_Hedged48m
tmp_tables = create_low_high_table(tmp,"U-index",tablename)
project_Uindex_IRATStable_underISS = tmp_tables$IRATStable_underBB; project_Uindex_IRATStable_underBB_cal = tmp_tables$IRATStable_underBB_cal
rm("tmp_tables")
# Get the basic low versus high U-index split
irats_under_tmp = tmp$feature_IRATStable
lowstd = irats_under_tmp$all_low$results[,1]/irats_under_tmp$all_low$results[,2]
highstd = irats_under_tmp$all_high$results[,1]/irats_under_tmp$all_high$results[,2]
HLdiff = -(irats_under_tmp$all_high$results[,1] - irats_under_tmp$all_low$results[,1])
tvalHL = HLdiff/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(irats_under_tmp$all_low$dfs-1, irats_under_tmp$all_high$dfs -1))
under_high_low = cbind(HLdiff, tvalHL,pvalHL)
colnames(under_high_low) <- c("Low-High Div.","t-stat","p-value") 
under_high_low[nrow(under_high_low),] <- 0
project_IRATStableISS = cbind(irats_under_tmp$all_low$results,irats_under_tmp$all_high$results,under_high_low)
#
irats_under_tmp = tmp$feature_IRATStable_cal
lowstd = irats_under_tmp$all_low$results[,1]/irats_under_tmp$all_low$results[,2]
highstd = irats_under_tmp$all_high$results[,1]/irats_under_tmp$all_high$results[,2]
HLdiff = -(irats_under_tmp$all_high$results[,1] - irats_under_tmp$all_low$results[,1])
tvalHL = HLdiff/sqrt(lowstd*lowstd + highstd*highstd)
pvalHL = 1-pt(tvalHL, df = pmin(irats_under_tmp$all_low$dfs-1, irats_under_tmp$all_high$dfs -1))
under_high_low = cbind(HLdiff, tvalHL,pvalHL)
colnames(under_high_low) <- c("Low-High Div.","t-stat","p-value") 
under_high_low[nrow(under_high_low),] <- 0
project_tableISS_cal = cbind(irats_under_tmp$all_low$results,irats_under_tmp$all_high$results,under_high_low)
colnames(project_IRATStableISS) <- c("Low Diversity: CAR", "t-stat","p-value","High Diversity: CAR", "t-stat","p-value","Low-High Diversity", "t-stat","p-value")
colnames(project_tableISS_cal) <- c("Low Diversity: CAL", "t-stat","p-value","High Diversity: CAL", "t-stat","p-value","Low-High Diversity", "t-stat","p-value")
rm("irats_under_tmp","tmp")

################################################################################################################################
# Some descriptive statistics and relations with other firm characteristics
################################################################################################################################

all_fund_sources = unique(unlist(sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) unlist(str_split(i,"\\+")))))
cash_funds = c("CR")
credit_funds = c("BL","BOR","CF","DS")
other_funds = setdiff(all_fund_sources,c(cash_funds,credit_funds))
all_purposes = unique(unlist(sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) unlist(str_split(i,"\\+")))))
good_purpose = c("ESV","UVL","ISV")
other_purpose = setdiff(all_purposes,c(good_purpose))

EUindex_bb = BUYBACK_DATA_PROJECT$EU_index
high_leverage = 0*EUindex_bb
high_leverage[High_LEV_eventsBB] <-1
low_leverage = 0*EUindex_bb
low_leverage[Low_LEV_eventsBB] <-1
Downgraded = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec < BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
Upgraded = (BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus1$mean_rec > BUYBACK_DATA_PROJECT$DATASET$ibes$month_minus2$mean_rec)
low_epsunc = 0*EUindex_bb
low_epsunc[Low_EPS_eventsBB] <-1
Credit = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), credit_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(cash_funds,other_funds)))==0)
Cash = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), cash_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(credit_funds,other_funds)))==0)
Good_purpose = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), good_purpose))!=0 & length(intersect(unlist(str_split(i,"\\+")), other_purpose))==0)
Stock_Option_Plan = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "STP"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV","UVL")))==0)
Undervalued = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "UVL"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("STP")))==0 )
Enhance_Shareholder_Value = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV")))!=0)

project_relations = cbind(Downgraded,Upgraded,Cash,Undervalued,Enhance_Shareholder_Value,Stock_Option_Plan)
## PROJECT SPECIFIC ADDITIONS HERE:
CEO.female = BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female !=0
CFO.female = BUYBACK_DATA_PROJECT$DATASET$boardex$CFO_female !=0
tmp = cbind(CEO.female,CFO.female)
colnames(tmp) <- c("CEO is Female","CFO is Female")
project_relations = cbind(project_relations,tmp)

project_relations= t(apply(project_relations,2,function(r){
  x = table(High_project_eventsBB[!is.na(r)], r[!is.na(r)])
  x = matrix(c(100*x[,2]/(x[,1]+x[,2]),prop.test(x)$p.value,prop.test(x)$statistic),ncol=1)
  x
}))
rownames(project_relations) <- gsub("_"," ", rownames(project_relations))

project_relations_continuous = t(apply(all_characteristics_continuous_summary,2,function(r){
  useonlyL = which(High_project_eventsBB == FALSE)
  useonlyH = which(High_project_eventsBB == TRUE)
  c(mean(r[useonlyL][!is.na(r[useonlyL])]), mean(r[useonlyH][!is.na(r[useonlyH])]), t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$p.value,t.test(r[useonlyL][!is.na(r[useonlyL])],r[useonlyH][!is.na(r[useonlyH])])$statistic)
}))
rownames(project_relations_continuous) <-  colnames(all_characteristics_continuous_summary)
project_relations = rbind(project_relations, project_relations_continuous)


########################################################################################################
# Cross sectional analyses
########################################################################################################
# Get the predicted post-announce abnormal returns (as developed by "../BuybacksApril2016/bb_issuers_new.R")
load("../FinanceData/created_projects_datasets/BUYBACKSnew_BSC1998_event_study_factor_coeffs.Rdata")
events_used = paste(BUYBACK_DATA_PROJECT$DATASET$SDC$permno, BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, sep=" ")
Estimated_returns = 100*Estimated_returns[events_used,]
BUYBACK_PreEvent_Factor_coeffs = BUYBACK_PreEvent_Factor_coeffs[events_used]
rm("events_used","BUYBACK_PreEvent_Factor_coeffs") # we don't need for now BUYBACK_PreEvent_Factor_coeffs

company_features_all = as.data.frame(all_characteristics_continuous)

####################################
# Some project specific parameters
tmp = BUYBACK_DATA_PROJECT$DATASET$boardex[[project_var_name]]
names(tmp) <- paste(str_sub(AddMonths(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date,-1),start = 1, end=7), BUYBACK_DATA_PROJECT$DATASET$SDC$permno, sep=" ")
tmp = tmp[rownames(company_features_all)]
project.main.IV.variable = tmp
company_features_all$project.main.IV.variable = project.main.IV.variable
cross_regressions_variables_individual = NULL 
cross_regressions_variables_complete = c("CEO.GenderInteraction")

# Add dummies
year_dummies_cross = sapply(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, function(i) str_sub(i, start=1,end=4))
industry_dummies_cross = as.character(BUYBACK_DATA_PROJECT$DATASET$CRSP$SICH)
industry_dummies_cross_2digit = ifelse(!is.na(BUYBACK_DATA_PROJECT$DATASET$CRSP$SICH), str_sub(as.character(BUYBACK_DATA_PROJECT$DATASET$CRSP$SICH), start=1,end=2), NA)
company_features_all$year_dummies_cross = year_dummies_cross
company_features_all$industry_dummies_cross = industry_dummies_cross
company_features_all$industry_dummies_cross_2digit = industry_dummies_cross_2digit

## PROJECT SPECIFIC VARIABLES
company_features_all$CEO.GenderInteraction = (BUYBACK_DATA_PROJECT$DATASET$boardex$CEO_female !=0)*project.main.IV.variable

cross_regressions_variables_individual = c(
  "Market Cap. (Score)",
  "BE/ME (Score)",
  "Prior Returns (Score)",
  "U-index",
  "EU-index", 
  "Volatility (Score)",
  "One minus Rsq (Score)",
  "Analyst Coverage (Score)", 
  cross_regressions_variables_individual,
  "project.main.IV.variable"
)

cross_regressions_variables_complete = c(
  "Market Cap. (Score)",
  "BE/ME (Score)",
  "Prior Returns (Score)",
  "Volatility (Score)",
  "One minus Rsq (Score)",
  "Analyst Coverage (Score)", 
  cross_regressions_variables_complete,
  "project.main.IV.variable"
)

nomissing_allowed = c("alphaT", "project.main.IV.variable",cross_regressions_variables_complete)

useonly = 1:length(BUYBACK_DATA_PROJECT$DATASET$SDC$CUSIP)

BSC1998_individual_regression = Reduce(rbind,lapply(cross_regressions_variables_individual, function(i){
  #cat(i, ", ")
  remove_vars_report = function(varname) (!str_detect(varname,"dummies") & !str_detect(varname,".Female"))
  BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,,drop=F],company_features_all[useonly,c(i,"year_dummies_cross","industry_dummies_cross_2digit"),drop=F], timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed, remove_vars_report =remove_vars_report)
  BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
  BSC1998_completemodel[2:nrow(BSC1998_completemodel),]
}))
rownames(BSC1998_individual_regression) <- cross_regressions_variables_individual
rownames(BSC1998_individual_regression)[which(rownames(BSC1998_individual_regression) == "project.main.IV.variable")] <- project.main.IV.variable.name

company_features = company_features_all[,c(cross_regressions_variables_complete,"year_dummies_cross","industry_dummies_cross_2digit")]
BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
rownames(BSC1998_completemodel) <- c("Intercept",cross_regressions_variables_complete)
rownames(BSC1998_completemodel)[which(rownames(BSC1998_completemodel) == "project.main.IV.variable")] <- project.main.IV.variable.name

# PROJECT SPECIFIC ROWNAMES
rownames(BSC1998_completemodel)[which(rownames(BSC1998_completemodel) == "CEO.GenderInteraction")] <- "CEO Female x Board Diversity"

################################################################################################################################
# Logistic regression for buyback event prediction
################################################################################################################################

load("../FinanceData/created_projects_datasets/BUYBACK_otherdata.Rdata")
#logistic_data_all = logistic.data.all.yearly 
#yearly.regression.variable = yearly.regression.variable.yearly
logistic_data_all = logistic.data.all.monthly 
yearly.regression.variable = yearly.regression.variable.monthly
Daily_Returns = Buyback_Daily_Returns[,paste(BUYBACK_DATA_PROJECT$DATASET$SDC$permno, BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date, sep="_")]
rm("logistic.data.all.yearly","logistic.data.all.monthly","buybacks.events","Buyback_Daily_Returns")

##########################################
# ADD SOME PROJECT SPECIFIC DATA

load("../FinanceData/created_boardex_data/GLOBAL_BOARDEX_DATABASE.Rdata")
# A helper function to take a "template" matrix and create a YEARLY vector
tmp = apply(GLOBAL_BOARDEX_DATABASE$gender,2,function(r) fill_NA_previous(r,lastfill=T)) # Fill all the way to the last month
gender_all <- yearly.regression.variable(tmp) 
tmp = get_cross_section_score(tmp)
# SET to_shift=F IF WE WANT TO SEE THIS MONTH GENDER (with some hindsight some times though, hence we leave it as is, TRUE, by default)
gender_score_all <- yearly.regression.variable(tmp) 
CEO_female_all <- yearly.regression.variable(apply(GLOBAL_BOARDEX_DATABASE$CEO_female,2,function(r) fill_NA_previous(r,lastfill=T))) # Fill all the way to the last month

logistic_data_all$gender.score = gender_score_all # ALWAYS CHECK THIS IS ALIGNED WITH THE KEY VARIABLE USED
logistic_data_all$CEO.GenderInteraction =  (scrub(CEO_female_all) !=0)*logistic_data_all$gender.score

rm("GLOBAL_BOARDEX_DATABASE", "CEO_female_all","gender_all", "gender_score_all","tmp")

# THE SPECIFIC TO THIS PROJECT CHOICES - KEEP KEY VARIABLE JUST BEFORE THE DUMMIES SO IT APPEARS IN THE LAST ROW OF THE TABLES
logistic_IV_used_thisproject = 
  c("CEO.GenderInteraction","gender.score",
    "year.dummies","industry.dummies.2digit"
  )

####################################################################################
#### Do the analyses now

# START FROM WHERE WE START HAVING BUYBACK DATA + 2 years (to get the previous buybacks)
tmp = as.Date(paste(str_sub(rownames(logistic_data_all), start=7, end=13), "01",sep="-"))
useonly = tmp >= (min(BUYBACK_DATA_PROJECT$DATASET$SDC$Event.Date) + 2*370)
logistic_data_all = logistic_data_all[useonly,]

winsorise_vars <- c(
  "size.all.score", 
  "MEBE.score", 
  "prior.returns.all.score",
  "Total.Payout", 
  "Total.Payout.last.year",
  "Leverage", 
  "profitability", 
  "operating.income", 
  "non.operating.income", 
  "std.operating.income", 
  "dividend.payout.ratio", 
  "liquid.assets", 
  "price.earnings.ratio", 
  "capital.expenditures", 
  "institutional.ratio.all", 
  "institutional.number.all"
)

if (length(winsorise_vars) > 0)
  logistic_data_all[,winsorise_vars] <- apply(logistic_data_all[,winsorise_vars], 2, winsorize)

logistic_IV_used= c(
  "buybacks.events.past2years", 
  "size.all.score", 
  "MEBE.score", 
  "prior.returns.all.score",
  "Total.Payout", 
  "Total.Payout.last.year",
  "Leverage", 
  "profitability", 
  "operating.income", 
  "non.operating.income", 
  "std.operating.income", 
  "dividend.payout.ratio", 
  "liquid.assets", 
  "price.earnings.ratio", 
  "capital.expenditures", 
  "institutional.ratio.all", 
  "institutional.number.all",
  logistic_IV_used_thisproject
)

###
logistic_data = logistic_data_all[,c(logistic_IV_used,"buybacks.events.this.period")]
logistic_data = logistic_data[apply(logistic_data,1,function(r) sum(is.na(r))) == 0,]
names(logistic_data) <- str_replace_all(names(logistic_data), "\\.","")

the_logistic_formula = paste("buybackseventsthisperiod ~ ", paste(str_replace_all(logistic_IV_used, "\\.",""), collapse = " + " ), sep=" ")
the_logistic_formula = as.formula(the_logistic_formula)

logistic_regression_buyback = glm(formula = the_logistic_formula, family = "binomial", data = logistic_data)
logistic_regression_buyback = summary(logistic_regression_buyback)$coefficients
logistic_regression_buyback = logistic_regression_buyback[,c(1,3,4)]
colnames(logistic_regression_buyback) <- c("Coeff.","t-stat","p-value")

probit_regression_buyback_glm = glm(formula = the_logistic_formula, family = binomial(link = "probit"), data = logistic_data)
probit_regression_buyback = summary(probit_regression_buyback_glm)$coefficients
probit_regression_buyback = probit_regression_buyback[,c(1,3,4)]
colnames(probit_regression_buyback) <- c("Coeff.","t-stat","p-value")

logistic_regression_buyback_project = logistic_regression_buyback[!str_detect(rownames(logistic_regression_buyback),"dummi"),]
probit_regression_buyback_project = probit_regression_buyback[!str_detect(rownames(probit_regression_buyback),"dummi"),]

rm("logistic_IV_used_thisproject","logistic_regression_buyback","probit_regression_buyback","the_logistic_formula","logistic_IV_used")

########################################################################################
### HECKMAN
########################################################################################

HeckmanIMR1 = invMillsRatio(probit_regression_buyback_glm)$IMR1
names(HeckmanIMR1) <- rownames(logistic_data)
rownames_second_stage_rename = paste(str_sub(rownames(company_features_all), start=9, end=13), str_sub(rownames(company_features_all), start=1,end=7), sep=" ")
tmp = match(rownames_second_stage_rename,names(HeckmanIMR1))
HeckmanIMR1_matched = ifelse(is.na(tmp), NA, HeckmanIMR1[tmp])

company_features_heckman = company_features_all[,c(cross_regressions_variables_complete)]
company_features_heckman$HeckmanIMR1 = HeckmanIMR1_matched
company_features_heckman = company_features_heckman[!is.na(HeckmanIMR1_matched),]
Estimated_returns_heckman <- Estimated_returns[!is.na(HeckmanIMR1_matched),]
BSC1998_coefficients_heckman <- BSC1998_event_study_coeffs(Estimated_returns_heckman,company_features_heckman,timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
BSC1998_completemodel_heckman_nodummies =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients_heckman)
rownames(BSC1998_completemodel_heckman_nodummies) <- c("Intercept", cross_regressions_variables_complete,"Heckman Inv. Mills")
rownames(BSC1998_completemodel_heckman_nodummies)[which(rownames(BSC1998_completemodel_heckman_nodummies) == "project.main.IV.variable")] <- project.main.IV.variable.name

company_features_heckman = company_features_all[,c(cross_regressions_variables_complete,"year_dummies_cross","industry_dummies_cross_2digit")]
company_features_heckman$HeckmanIMR1 = HeckmanIMR1_matched
company_features_heckman = company_features_heckman[!is.na(HeckmanIMR1_matched),]
Estimated_returns_heckman <- Estimated_returns[!is.na(HeckmanIMR1_matched),]
BSC1998_coefficients_heckman <- BSC1998_event_study_coeffs(Estimated_returns_heckman,company_features_heckman,timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
BSC1998_completemodel_heckman =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients_heckman)
rownames(BSC1998_completemodel_heckman) <- c("Intercept", cross_regressions_variables_complete,"Heckman Inv. Mills")
rownames(BSC1998_completemodel_heckman)[which(rownames(BSC1998_completemodel_heckman) == "project.main.IV.variable")] <- project.main.IV.variable.name

rm("HeckmanIMR1","tmp","HeckmanIMR1_matched","Estimated_returns_heckman",
   "BSC1998_coefficients_heckman","company_features_heckman","rownames_second_stage_rename",
   "Estimated_returns", "logistic_data_all", "probit_regression_buyback_glm","useonly")


#######################################################################################################################
# Save the data now
save(list = setdiff(ls(all = TRUE),initial_vars), file = paste("board_diversity.",project_var_name ,".Rdata", sep=""))

########################################################################################################
# Generate the report
if (exists("generate_all_appendices")){
  # Generate the report
  library(knitr); knit2pdf('JFEmanuscript.Rnw', texi2dvi='texi2dvi'); file.remove(paste("JFEmanuscript",c(".aux",".tex",".log",".out"),sep=""))
  #file.rename("board_diversity.pdf", "JFE Manuscript.pdf")
  #file.rename("board_diversity.pdf", paste("board_diversity.", project_var_name, ".pdf", sep=""))
}
