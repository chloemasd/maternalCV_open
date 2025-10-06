# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Abbreviations often used in the scripts:
# BP: blood pressure, includes SBP and DBP
# DBP: diastolic blood pressure
# DP: double product, equals to SBP*HR
# HR: heart rate
# Hte: hematocrit
# SBP: systolic blood pressure

# Load targets to run the analysis plan
library(targets)
library(tarchetypes)

# Import functions
source("R/github_figures.R")
source("R/github_tables.R")
source("R/github_stat_models.R")
source("R/gihtub_preparation_wide.R") # some functions were hidden
source("R/github_preparation_long.R") # some functions were hidden
source("R/github_helpers.R") # some function were hidden

targets::tar_config_set(store = "_targets")

# Session info
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# 
# locale:
# [1] LC_COLLATE=French_France.utf8 
# [2] LC_CTYPE=French_France.utf8   
# [3] LC_MONETARY=French_France.utf8
# [4] LC_NUMERIC=C                  
# [5] LC_TIME=French_France.utf8    
# 
# time zone: Europe/Paris
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices datasets  utils    
# [6] methods   base     
# 
# loaded via a namespace (and not attached):
# [1] compiler_4.4.1    tools_4.4.1      
# [3] rstudioapi_0.17.0 renv_1.0.7   

# Specify packages used the during analyses
tar_option_set(
  packages = c(
    "data.table",  # Data manipulation
    "dlnm",        # Distributed Lag Non-linear Model (DLNM)
    "dplyr",       # Data manipulation
    "forestplot",  # Data visualization - Forest plot
    "ggcorrplot",  # Data visualization - Heat-map
    "ggplot2",     # Data visualization
    "ggpubr",      # Data visualization
    "gridExtra",   # Data visualization
    "gt",          # Data table description
    "gtsummary",   # Data table description
    "janitor",     # Data manipulation
    "kableExtra",  # Data visualization - Data tables 
    "lme4",        # Generalized linear models (GLM)
    "lubridate",   # Data management - Dates manipulation
    "mice",        # Data management - Missing data imputation 
    "naniar",      # Data visualization - Missing data
    "purrr",       # Nested data manipulation
    "RColorBrewer",# Data visualization - Warming strips plot
    "reshape2",    # Data management - Wide and long formats
    "rio",         # Data management - Import raw data
    "splines",     # DLNM - splines
    "stringr",     # Data manipulation
    "tibble",      # Data management - Nested data
    "tidyverse"    # Data manipulation and visualization
  )
)

# Research article

# Title ----
# Impacts of ambient temperature on maternal blood pressure and hematocrit levels
# during pregnancy show fetal sex differences: insights from the EDEN French cohort

# Abstract ----

# Keywords ----

# Introduction ----

# Analysis plan
tar_plan(
  
  # Methods ----
  ## Import raw data 
  
  ### Study population (hidden) ----
  data_raw_eden1 = rio::import("data/"), # hidden
  data_raw_eden2 = rio::import("data/"), # hidden
  data_raw_eden3 = rio::import("data/"), # hidden
  data_raw_eden4 = rio::import("data/"), # hidden
  data_raw_eden5 = rio::import("data/"), # hidden
  
  ## Maternal cardiovascular health in pregnancy (hidden) ----
  ### BP Z-scores
  data_raw_zscores_eden = rio::import("data/"), # hidden
  data_lms_sbp_eden = rio::import("data/"), # hidden
  data_lms_dbp_eden = rio::import("data/"), # hidden
  ### EDI
  data_edi_eden = rio::import("data/Expo/"), # hidden
  
  ## Environmental exposure assessment (hidden) ----
  ### Temperature
  data_tmax_eden = rio::import("data/Expo/"), # hidden
  data_tmin_eden = rio::import("data/Expo/"), # hidden
  data_tmean_eden = rio::import("data/Expo/"), # hidden
  data_date_eden = rio::import("data/Expo/"), # hidden
  ### Air pollution (PM2.5 and PM10)
  data_pm25_eden = rio::import("data/Expo/"), # hidden
  data_pm10_eden = rio::import("data/Expo/"), # hidden
  ### Relative humidity
  data_rhum_eden = rio::import("data/Expo/"), # hidden
  ### Nitrogen dioxide (NO2)
  data_no2_eden = rio::import("data/Expo/"), # hidden
  ### Ozone (O3)
  data_o3_eden = rio::import("data/Expo/"), # hidden
  ### NDVI
  data_ndvi_eden = rio::import("data/Expo/"), # hidden
  
  ## Covariates ---- 
  ## All these lists are based on the directed acyclic graph (DAG) and the 
  ## literature
  
  # Covariates for BP models
  list_cov_bp = c(
    "center",                        # Recruitment center
    "ch_sex",                        # Child sex
    "mo_age_cat4",                   # Maternal age at conception, categorical (4 modes)
    "mo_par_cat3",                   # Maternal parity, categorical (3 modes)
    "mo_bmi_bepr_cat3",              # Pre-pregnancy BMI, categorical (3 modes)
    "mo_weight_gain_spline1",        # Maternal weight gain at measurement, first spline component (time-dependent)
    "mo_weight_gain_spline2",        # Maternal weight gain at measurement, second spline component (time-dependent)
    "mo_dipl",                       # Maternal educational attainment
    "mo_tabacp",                     # Maternal second-hand smoking at trimester of measurement (time-dependent)
    "mo_tabac",                      # Maternal tobacco smoke at trimester of measurement (time-dependent)
    "diag_htn_precon_all",           # Hypertension before or during previous pregnancy
    "mo_med_ghtn3",                  # Hypertension x medication at measurement (time-dependent)
    "date_exam_sin",                 # Time trend at measurement, sine component (time-dependent)
    "date_exam_cos",                 # Time trend at measurement, cosine component (time-dependent)
    "edi_mean_4_tert",               # European Deprivation Index, tertiles
    "buffer100m_ndvi_ete_gped_tert"  # Normalized Difference Vegetation Index, tertiles
  ),
  
  # Covariates for HR models
  list_cov_hr = c(
    list_cov_bp[! list_cov_bp %in% c("diag_htn_precon_all","mo_med_ghtn3")],
    "mo_coffee"                      # Maternal caffeine intake at trimester of measurement (time-dependent)
  ),
  
  # Covariates for Hte models
  list_cov_hte = c(                  # Same as BP models
    list_cov_bp                    
  ),
  
  # Covariates for descriptive purpose
  list_cov_all = c(
    list_cov_bp,
    "tmean_mean_4",        # Mean temperature exposure, whole study period
    "tmin_mean_4",         # Minimal temperature exposure, whole study period
    "tmax_mean_4",         # Maximal temperature exposure, whole study period
    "po_monthe",           # Month of examination
    "po_gd_m",             # Gestational duration (mixed/imputed)
    "po_seasonc",          # Season of conception
    "diag_hdp_all",        # Hypertensive disorder of pregnancy (before and after 20 WA)
    "diag_other_all2",     # Anit-hypertensive or preventive aspirin medication during pregnancy
    "diag_gdiab",          # Gestational diabetes
    "mo_tabac_any",        # Maternal tobacco smoking in pregnancy
    "mo_tabacp_any",       # Maternal second-hand smoking in pregnancy
    "mo_weight_gain_pr",   # Maternal total weight gain in pregnancy
    "info_expo_200m"       # Urbanicity
  ),
  
  # Statistical analyses ----

  ## Data management : prepare the data (hidden) ----
  
  ### * Initial data sets ----
  data_wide_eden = prep_data_clean(data_raw_eden1, data_raw_eden2, data_raw_eden3, data_raw_eden4, data_raw_eden5, data_temp_eden, data_tmean_eden), # Cleaned EDEN data, all outcomes (hidden)
  data_zscores_eden = prep_data_zscores(data_raw_zscores_eden), # Cleaned BP Z-scores (hidden)
  data_weight_eden = prep_data_weight(data_raw_eden5), # Cleaned weight gain during pregnancy (hidden)
  
  ### * Un-nested data sets ----
  # Filtering study population for each outcome
  data_hr_eden = prep_data_final(data_wide_eden, "hr", data_flag_resid_eden, data_weight_eden, data_date_eden), # HR (hidden)
  data_hte_eden = prep_data_final(data_wide_eden, "hte", data_flag_resid_eden, data_weight_eden, data_date_eden), # Hte (hidden)
  data_bp_eden = prep_data_final(data_wide_eden, "bp", data_flag_resid_eden, data_weight_eden, data_date_eden, data_zscores_eden), # SBP and DBP (repeated measures; hidden)
  # Adding environmental exposures based on measurements dates
  data_hr_expo_eden = prep_add_expo(data_hr_eden, data_date_eden, data_ndvi_eden, data_edi_eden, data_rhum_eden, data_tmin_eden, data_tmax_eden, data_tmean_eden, data_pm10_eden, data_pm25_eden, data_no2_eden, data_o3_eden), # HR with exposure data (hidden)
  data_hte_expo_eden = prep_add_expo(data_hte_eden, data_date_eden, data_ndvi_eden, data_edi_eden, data_rhum_eden, data_tmin_eden, data_tmax_eden, data_tmean_eden, data_pm10_eden, data_pm25_eden, data_no2_eden, data_o3_eden), # Hte with exposure data (hidden)
  data_bp_expo_eden = prep_add_expo(data_bp_eden, data_date_eden, data_ndvi_eden, data_edi_eden, data_rhum_eden, data_tmin_eden, data_tmax_eden, data_tmean_eden, data_pm10_eden, data_pm25_eden, data_no2_eden, data_o3_eden, bp=TRUE), # BP (repeated measurements) with exposure data (hidden)
  
  ## Missing data imputation ----
  data_hr_imp_eden = prep_imputation(data_hr_expo_eden, list_cov_hr), # HR data imputation
  data_hte_imp_eden = prep_imputation(prep_complete(data_hr_imp_eden, data_hte_expo_eden), list_cov_hte), # Hte data imputation (using results from the HR imputation when possible)
  data_bp_imp_eden = prep_final_imputation_bp(data_bp_expo_eden, data_hte_imp_eden, data_hr_imp_eden, list_cov_bp), # BP data imputation (using results from the HR and Hte imputation when possible)
  
  # Nested data sets ready for analyses
  # With used nested data frames to duplicate analyses based on:
  # - the outcome when several were analysed
  # - the time periods (W0,W1,W2,W3,W4,W5)
  # - the exposure (Tmean, Tmax, Tmin, Tvar)
  # - the df for the dose-response (dfvar)
  # - the df for the lag-response (dflag)
  # - the temperature threshold to define heat and cold exposure (percentiles)
  
  ## Associations between ambient temperature exposure and zBP levels ----
  ### * Data preparation : Nested data frames on W0,W1,W2,W3,W4,W5 and numerical balance ----
  data_bp_nest_eden = prep_group(data_bp_imp_eden), # zBP nested data frame (W0,W1,W2,W3,W4,W5)
  data_bp_final_eden = prep_balance_window(data_bp_nest_eden, seed=1024), # zBP nested and numerically balanced intervals (W0,W2,W3,W4,W5)
  data_bp_finalw1_eden = prep_balance_w1_window(data_bp_nest_eden, seed=1024), # zBP nested (W1; not balanced: single measurement)
  ### * Data preparation : Filtering the study period (W0 or other) ----
  data_bp_w_eden = prep_filter(data_bp_final_eden, c("w")), # zBP analysis on {W0}
  data_bp_wx_eden = prep_filter(data_bp_final_eden, c("w1","w2","w3","w4","w5")), # zBP analysis on {W1,W2,W3,W4,W5}
  ### * Models ---- 
  model_bp_w_tt = run_model(data_bp_w_eden, list_cov_bp), # Main analysis on {W0} (random effects)
  model_bp_wx_tt = run_model(data_bp_wx_eden, list_cov_bp), # Main analysis on {W2,W3,W4,W5} (random effects)
  model_bp_w1_tt = run_model(data_bp_finalw1_eden, list_cov_bp, random.effect=FALSE), # Main analysis on {W1} (fixed effects)
  
  ## Associations between ambient temperature exposure and HR and Hte ---- 
  ### * Data preparation : Nested data frames on W0 only ----
  data_hr_final_eden = prep_group_other(data_hr_imp_eden, outcome="hr"), # HR final data frame (nested)
  data_hte_final_eden = prep_group_other(data_hte_imp_eden, outcome="hte"), # Hte final data frame (nested)
  ### * Models ----
  model_hr_tt = run_model(data_hr_final_eden, list_cov_hr, random.effect=F), # Main analysis on {W0} for HR
  model_hte_tt = run_model(data_hte_final_eden, list_cov_hte, random.effect=F), # Main analysis on {W0} for Hte
  
  ## Stratified analyses ---- 
  
  ### i. Sex stratification ----
  #### * Data preparation ----
  data_bp_f_eden = prep_exclude(data_bp_imp_var="Female", outcome="bp"), # Stratification on girls in BP analyses
  data_bp_m_eden = prep_exclude(data_bp_imp_var="Male", outcome="bp"),   # Stratification on boys in BP analyses
  data_hr_f_eden = prep_exclude(data_hr_imp_var="Female", outcome="hr"), # Stratification on girls in HR analyses
  data_hr_m_eden = prep_exclude(data_hr_imp_var="Male", outcome="hr"), # Stratification on boys in HR analyses
  data_hte_f_eden = prep_exclude(data_hte_imp_var="Female", outcome="hte"), # Stratification on girls in Hte analyses
  data_hte_m_eden = prep_exclude(data_hte_imp_var="Male", outcome="hte"), # Stratification on boys in Hte analyses
  #### * Models ----
  model_bp_f_tt = run_model(data_bp_f_eden, list_cov_bp[!list_cov_bp %in% c("ch_sex")]), # Stratification on girls in BP analysis, model on {W0}
  model_bp_m_tt = run_model(data_bp_m_eden, list_cov_bp[!list_cov_bp %in% c("ch_sex")]), # Stratification on boys in BP analysis, model on {W0}
  model_hr_f_tt = run_model(data_hr_f_eden, list_cov_hr[!list_cov_hr%in%c("ch_sex")], random.effect=F), # Stratification in girls for HR 
  model_hr_m_tt = run_model(data_hr_m_eden, list_cov_hr[!list_cov_hr%in%c("ch_sex")], random.effect=F), # Stratification on boys for HR 
  model_hte_f_tt = run_model(data_hte_f_eden, list_cov_hte[!list_cov_hte%in%c("ch_sex")], random.effect=F), # Stratification on girls for Hte
  model_hte_m_tt = run_model(data_hte_m_eden, list_cov_hte[!list_cov_hte%in%c("ch_sex")], random.effect=F), # Stratification on boys for Hte
  #### * Intervals recalculation ----
  model_bp_diff_sex_tt = moderation_test(model_bp_m_tt, model_bp_f_tt), # Recalculation of confidence intervals for sex-stratified analysis on BP
  model_hr_diff_sex_tt = moderation_test(model_hr_m_tt, model_hr_f_tt), # Recalculation of confidence intervals for sex-stratified analysis on HR
  model_hte_diff_sex_tt = moderation_test(model_hte_m_tt, model_hte_f_tt), # Recalculation of confidence intervals for sex-stratified analysis on Hte
  
  ### ii. Stratification on co-exposure levels ----
  #### * Data preparation ----
  data_bp_pm25_low_eden = prep_airpol_strat(data_bp_imp_eden, "pm25", level=1), # stratification on low PM2.5 in BP analyses
  data_bp_pm25_high_eden = prep_airpol_strat(data_bp_imp_eden, "pm25", level=3), # stratification on high PM2.5 in BP analyses
  data_bp_pm10_low_eden = prep_airpol_strat(data_bp_imp_eden, "pm10", level=1), # stratification on low PM10 in BP analyses
  data_bp_pm10_high_eden = prep_airpol_strat(data_bp_imp_eden, "pm10", level=3), # stratification on high PM10  in BP analyses
  data_bp_no2_low_eden = prep_airpol_strat(data_bp_imp_eden, "no2", level=1), # stratification on low NO2 in BP analyses
  data_bp_no2_high_eden = prep_airpol_strat(data_bp_imp_eden, "no2", level=3), # stratification on high NO2 in BP analyses
  data_bp_o3_low_eden = prep_airpol_strat(data_bp_imp_eden, "o3", level=1), # stratification on low O3 in BP analyses
  data_bp_o3_high_eden = prep_airpol_strat(data_bp_imp_eden, "o3", level=3), # stratification on high O3 in BP analyses
  data_bp_ndvi_low_eden = prep_ndvi_strat(data_bp_imp_level=1), # stratification on low NDVI in BP analyses
  data_bp_ndvi_high_eden = prep_ndvi_strat(data_bp_imp_level=3), # stratification on high NDVI in BP analyses
  #### * Models ----
  model_bp_pm25_low_tt = run_model(data_bp_pm25_low_eden, list_cov_bp), # Stratification on low PM2.5 exposure, model on {W0}  for BP
  model_bp_pm25_high_tt = run_model(data_bp_pm25_high_eden, list_cov_bp), # Stratification on high PM2.5 exposure, model on {W0} for BP
  model_bp_pm10_low_tt = run_model(data_bp_pm10_low_eden, list_cov_bp), # Stratification on low PM10 exposure, model on {W0} for BP
  model_bp_pm10_high_tt = run_model(data_bp_pm10_high_eden, list_cov_bp), # Stratification on high PM10 exposure, model on {W0} for BP
  model_bp_no2_low_tt = run_model(data_bp_no2_low_eden, list_cov_bp), # Stratification on low NO2 exposure, model on {W0} for BP
  model_bp_no2_high_tt = run_model(data_bp_no2_high_eden, list_cov_bp), # Stratification on high NO2 exposure, model on {W0} for BP
  model_bp_o3_low_tt = run_model(data_bp_o3_low_eden, list_cov_bp), # Stratification on low O3 exposure, model on {W0} for BP
  model_bp_o3_high_tt = run_model(data_bp_o3_high_eden, list_cov_bp), # Stratification on high O3 exposure, model on {W0} for BP
  model_bp_ndvi_low_tt = run_model(data_bp_ndvi_low_eden, list_cov_bp[!list_cov_bp%in%c("buffer100m_ndvi_ete_gped_tert")]), # Stratification on low NDVI exposure, model on {W0} for BP
  model_bp_ndvi_high_tt = run_model(data_bp_ndvi_high_eden, list_cov_bp[!list_cov_bp%in%c("buffer100m_ndvi_ete_gped_tert")]), # Stratification on high NDVI exposure, model on {W0} for BP
  #### * Intervals recalculation ----
  model_bp_diff_pm25_tt = moderation_test(model_bp_pm25_high_tt, model_bp_pm25_low_tt),
  model_bp_diff_pm10_tt = moderation_test(model_bp_pm10_high_tt, model_bp_pm10_low_tt),
  model_bp_diff_no2_tt = moderation_test(model_bp_no2_high_tt, model_bp_no2_low_tt),
  model_bp_diff_o3_tt = moderation_test(model_bp_o3_high_tt, model_bp_o3_low_tt),
  model_bp_diff_ndvi_tt = moderation_test(model_bp_ndvi_high_tt, model_bp_ndvi_low_tt),
  model_bp_diff2_pm25_tt = moderation_test(model_bp_pm25_high2_tt, model_bp_pm25_low2_tt),
  model_bp_diff2_pm10_tt = moderation_test(model_bp_pm10_high2_tt, model_bp_pm10_low2_tt),
  model_bp_diff2_no2_tt = moderation_test(model_bp_no2_high2_tt, model_bp_no2_low2_tt),
  model_bp_diff2_o3_tt = moderation_test(model_bp_o3_high2_tt, model_bp_o3_low2_tt),
  model_bp_diff2_ndvi_tt = moderation_test(model_bp_ndvi_high2_tt, model_bp_ndvi_low2_tt),
  
  ## Secondary analyses ---- 
  ### i. Diurnal temperature range ----
  #### These were already included in the data sets
  
  ### ii. Binary daily temperature ----
  model_bp_w_tt_binary = run_model_binary(data_bp_w_eden, list_cov_bp), # Analysis on {W0}, random effects
  model_bp_wx_tt_binary = run_model_binary(data_bp_wx_eden, list_cov_bp), # Analysis on {W2,W3,W4,W5}, random effects
  model_bp_w1_tt_binary = run_model_binary(data_bp_finalw1_eden, list_cov_bp, random.effect=F), # Analysis on {W1}, fixed effects
  model_hr_tt_binary = run_model_binary(data_hte_final_eden, list_cov_hr, random.effect=F), # Analysis on {W0} for HR
  model_hte_tt_binary = run_model_binary(data_hte_final_eden, list_cov_hte, random.effect=F), # Analysis on {W0} for Hte
  
  ### iii. DP as secondary outcome ----
  #### * Data preparation ---- 
  data_dp_final_eden = prep_group_other(data_hr_imp_eden, outcome="dp"), # DP final data frame (nested)
  #### * Models ----
  model_dp_tt = run_model(data_dp_final_eden, c(list_cov_hr,"diag_htn_precon_all","mo_med_ghtn3"), random.effect=F), # Main analysis on {W0} for DP
  model_dp_tt_binary = run_model_binary(data_hte_final_eden, c(list_cov_hr,"diag_htn_precon_all","mo_med_ghtn3"), random.effect=F), # Binary exposure analysis on {W0} for DP
  model_dp_f_tt = run_model(data_dp_f_eden, c(list_cov_hr[!list_cov_hr%in%c("ch_sex")],"diag_htn_precon_all","mo_med_ghtn3"), random.effect=F), # DP analysis, stratification on girls
  model_dp_m_tt = run_model(data_dp_m_eden, c(list_cov_hr[!list_cov_hr%in%c("ch_sex")],"diag_htn_precon_all","mo_med_ghtn3"), random.effect=F), # DP analysis, stratification on boys
  
  ## Sensitivity analyses ----
  
  ### i. Extension of heat and cold definitions to other thresholds ----
  #### These were already included in the data sets
  
  ### ii. Other combinations of degrees of freedom ----
  #### These were already included in the data sets
  
  ### iii. Additional adjustment ----
  #### * Adjustment for air pollution exposure ----
  model_bp_pm25_tt = run_model_ap_spe(data_bp_w_eden[which(data_bp_w_eden$dfvar==2 & data_bp_w_eden$dflag==3 & data_bp_w_eden$p_hot==95),], "pm25", list_cov_bp), # Adjusting for PM2.5 exposure on {W0} for BP, selecting p95 and df(2,3)
  model_bp_pm10_tt = run_model_ap_spe(data_bp_w_eden[which(data_bp_w_eden$dfvar==2 & data_bp_w_eden$dflag==3 & data_bp_w_eden$p_hot==95),], "pm10", list_cov_bp), # Adjusting for PM10 exposure on {W0} for BP, selecting p95 and df(2,3)
  model_bp_no2_tt = run_model_ap_spe(data_bp_w_eden[which(data_bp_w_eden$dfvar==2 & data_bp_w_eden$dflag==3 & data_bp_w_eden$p_hot==95),], "no2", list_cov_bp), # Adjusting for NO2 exposure on {W0} for BP, selecting p95 and df(2,3)
  model_bp_o3_tt = run_model_ap_spe(data_bp_w_eden[which(data_bp_w_eden$dfvar==2 & data_bp_w_eden$dflag==3 & data_bp_w_eden$p_hot==95),], "o3", list_cov_bp), # Adjusting for O3 exposure on {W0} for BP, selecting p95 and df(2,3)
  model_hr_ap_tt = run_model_ap(data_hr_final_eden, list_cov_hr, random.effect=FALSE), # Adjusting for all air pollution exposures (PM2.5, PM10, NO2 and O3) for HR
  model_hte_ap_tt = run_model_ap(data_hte_final_eden, list_cov_hte, random.effect=FALSE), # Adjusting for all air pollution exposures (PM2.5, PM10, NO2 and O3) for Hte
  #### * Adjustment for gestation diabetes ----
  model_bp_gdiab_tt = run_model(data_bp_w_eden[which(data_bp_w_eden$dfvar==2 & data_bp_w_eden$dflag==3 & data_bp_w_eden$p_hot==95),], c(list_cov_bp,"diag_gdiab")), # Model on {W0} for BP, selecting p95 and df(2,3)
  model_hr_gdiab_tt = run_model(data_hr_final_eden, c(list_cov_hr,"diag_gdiab"), random.effect=F), # Model for HR
  model_hte_gdiab_tt = run_model(data_hte_final_eden, c(list_cov_hte,"diag_gdiab"), random.effect=F), # Model for Hte
  #### * Remove time trend adjustment ----
  model_bp_w = run_model(data_bp_w_eden[which(data_bp_w_eden$p_hot==95),], list_cov_bp[!list_cov_bp %in% c("date_exam_sin","date_exam_cos")]), # Model on {W0} for BP
  model_hr = run_model(data_hr_final_eden, list_cov_hr[!list_cov_hr%in%c("date_exam_sin","date_exam_cos")], random.effect=F), # Model for HR
  model_hte = run_model(data_hte_final_eden, list_cov_hte[!list_cov_hte%in%c("date_exam_sin","date_exam_cos")], random.effect=F), # Model for Hte
  #### * Mutually adjusting for other cardiovascular indicators ----
  data_bp_hradj_eden = prep_Hadj(data_bp_imp_eden, "hr_mean_all_c24"), # Adjusting for HR in BP analyses, data preparation
  data_bp_hteadj_eden = prep_Hadj(data_bp_imp_eden, "hte_nfs"), # Adjusting for Hte in BP analyses, data preparation
  model_bp_hradj_tt = run_model(data_bp_hradj_eden, c(list_cov_bp,"outcome_res")), # Adjusting for HR in BP analyses, model on {W0}
  model_bp_hteadj_tt = run_model(data_bp_hteadj_eden, c(list_cov_bp,"outcome_res")), # Adjusting for Hte in BP analysis, model on {W0}
  
  ### iv. Exclusion of some participants ----
  #### (i) Women diagnosed with hypertension during pregnancy ----
  data_bp_noHTN_eden = prep_exclude(data_bp_imp_eden, var="diag_ghtn_all", outcome="bp"), # Data preparation
  model_bp_noHTN_tt = run_model(data_bp_noHTN_eden, list_cov_bp[!list_cov_bp %in% c("mo_med_ghtn3")]), # Model on {W0}
  #### (ii) Women with hypertension before pregnancy ----
  data_bp_nopreHTN_eden = prep_exclude(data_bp_imp_eden, var="diag_htn_precon_all", outcome="bp"), # Data preparation
  model_bp_noTTT_tt = run_model(data_bp_noTTT_eden, list_cov_bp[!list_cov_bp %in% c("mo_med_ghtn3")]), # Model on {W0}
  #### (iii) Women taking either anti-hypertensive medication or preventive aspirin ----
  data_bp_noTTT_eden = prep_exclude(data_bp_imp_eden, var="diag_other_all2", outcome="bp"), # Data preparation
  model_bp_nopreHTN_tt = run_model(data_bp_nopreHTN_eden, list_cov_bp[!list_cov_bp %in% c("diag_htn_precon_all")]), # Model on {W0}

  # * Figure 1 ----
  # Title: Workflow of the study investigating the effects of ambient temperature
  # on maternal cardiovascular function during pregnancy in the EDEN mother-child cohort.
  # Median and IQR for weeks of gestation at measurement were reported.
  # Figure 1 does not rely on R code
  
  # Results ----
  
  ## Descriptive statistics ----
  
  ### Population characteristics ----
  
  # * Table 1 ----
  # Title: Characteristics of the study population: 1854 pregnant women from the
  # EDEN cohort with at least 1 BP measurement
  tbl_desc_bp_eden = make_tbl_desc_var(data_bp_imp_eden$final, var="center", repeated=T),
  
  ### Ambient temperature exposure ----
  
  # * Figure 2 ----
  # Title: Warming strip plots of averaged temperature exposure in the 28 days 
  # preceding each blood pressure measurement, 1854 pregnant women from the EDEN
  # cohort (A) Average Tmean exposure (°C). (B) Average Tmax exposure (°C). (C)
  # Average Tmin exposure (°C).
  fig_strip_tmean_eden = make_warming_strips(data_bp_imp_eden$final, column = "tmean_mean_4"), # (A)
  fig_strip_tmax_eden = make_warming_strips(data_bp_imp_eden$final, column = "tmax_mean_4"), # (B)
  fig_strip_tmin_eden = make_warming_strips(data_bp_imp_eden$final, column = "tmin_mean_4"), # (C)
  
  ## Associations between subacute heat exposure and maternal CV function ----
  
  # * Figure 3 ----
  # Title: zBP overall cumulative and lagged associations with subacute heat and
  # cold exposure in the 28 days preceding measurement. Overall cumulative association
  # between Tmean in the 28 days preceding both SBP (A) and DBP (B) measurement
  # over W0, including lagged association for p5 (4°C) and p95 (20°C) compared to
  # the median (11°C). Shaded areas represent range of the confidence intervals.
  # Associations correspond to DLNM including random effects, with 2 and 3 df for
  # dose- and lag-response curves respectively. Models were adjusted for: recruitment
  # center, maternal age at conception, maternal educational attainment, maternal
  # parity, maternal pre-pregnancy BMI, maternal weight gain at measurement, maternal
  # smoking habits, hypertension before pregnancy, hypertension during pregnancy
  # coupled to medication, relative humidity (same lag structure as the temperature)
  # exposure), time trend at measurement, EDI and NDVI.
  # Check figure_dlnm.R file
  
  ## Associations between subacute cold exposure and maternal CV function ----
  
  ## Sex-specific relationships between ambient temperature, zSBP and Hte levels ----
  
  # * Figure 4 ----
  # Title: zSBP associations with subacute heat and cold exposure in the 28 days 
  # preceding measurement by fetal sex. Overall cumulative association (top panel) 
  # between Tmean in the 28 days preceding SBP measurement over W0 for sex-stratified 
  # analyses. Lag-response associations at p95 (20°C; bottom-right panel) and p5 
  # (4°C; bottom-left panel) compared to the median (11°C). Shaded areas represent 
  # range of the confidence intervals. Associations correspond to DLNM including 
  # random effects, with 2 and 3 df for dose- and lag-response curves respectively. 
  # Models were adjusted for: recruitment center, maternal age at conception, 
  # maternal educational attainment, maternal parity, maternal pre-pregnancy BMI, 
  # maternal weight gain at measurement, maternal smoking habits, hypertension before pregnancy, 
  # hypertension during pregnancy coupled to medication, relative humidity 
  # (same lag structure as the temperature exposure), time trend at measurement, 
  # EDI and NDVI.
  # Check figure_dlnm.R file
  
  ## Air pollution and vegetation: adjusted and stratified analyses ---- 
  
  # * Figure 5 ----
  # Air pollution-adjusted associations between zSBP levels and Tmean over W0. 
  # Overall cumulative associations between Tmean in the 28 days preceding SBP 
  # measurement at W0 for main and air pollution-adjusted analyses. Air pollutants 
  # included: PM2.5, PM10, NO2 and O3 and were modelled with same lag structure 
  # as the main exposure. Shaded areas represent range of the confidence intervals. 
  # Associations correspond to DLNM including random effects, with 2 df for the 
  # dose-response curve. Models were adjusted for: recruitment center, maternal 
  # age at conception, maternal educational attainment, maternal parity, 
  # maternal pre-pregnancy BMI, maternal weight gain at measurement, maternal 
  # smoking habits, hypertension before pregnancy, hypertension during pregnancy 
  # coupled to medication, relative humidity (same lag structure as the temperature 
  # exposure), time trend at measurement, EDI and NDVI, and individual air pollutant.
  # Check figure_dlnm.R file
  
  ## Sensitivity analyses ---- 
  
  # Supplementary Materials ----
  
  ## Additional file 1: Supplementary Figures ----
  
  # * Supplementary Figure 1 ----
  # Title: Flowchart of the study. A total of 1,854 pregnant women from the EDEN
  # cohort participated with 12,289 blood pressure measurements across pregnancy
  # (W0) among whom 1,840 and 1,832 were included in heart rate and hematocrit
  # analyses, respectively.
  # Supplementary Figure 1 does not rely on R code
  
  # * Supplementary Figure 2 ----
  # Title: Distribution of the number of measurements through gestational weeks 
  # of pregnancy for each indicator of maternal cardiovascular function. (left) 
  # Distribution of gestational ages at repeated blood pressure (BP) measurements 
  # for the 1,854 mothers included in BP analysis. (middle) Distribution of 
  # gestational ages at heart rate (HR) measurement for the 1,840 mothers included 
  # in HR analysis. (right) Distribution of gestational ages at hematocrit (Hte) 
  # measurement for the 1,832 mothers included in Hte analysis. Vertical dashed 
  # lines outline the 5 windows investigated in the BP analysis (W1, W2, W3, W4 and W5).
  fig_hist_bp_ga_eden = make_hist_ga(data_bp_imp_eden$final, out="bp"), # (left)
  fig_hist_hr_ga_eden = make_hist_ga(data_hr_imp_eden$final, out="hr"), # (middle)
  fig_hist_hte_ga_eden = make_hist_ga(data_hte_imp_eden$final, out="hte"), # (right)
  
  # * Supplementary Figure 3 ----
  # Title: Smoothed trajectories of zSBP and zDBP through gestational weeks of 
  # pregnancy at measurement according to mothers’ hypertensive status during 
  # pregnancy: no hypertensive disorder of pregnancy (no HDP; brown), hypertension 
  # during pregnancy (light blue) or pre-eclampsia (dark blue). As expected, zSBP 
  # (left panel) and zDBP (right panel) of women with hypertensive disorders of 
  # pregnancy were greater than those of the normotensive mothers (no HDP group). 
  # Shaded areas represent range of the confidence intervals.
  fig_zbp_htn_eden = make_zbp(data_bp_imp_eden$final, "htn"),
  
  # * Supplementary Figure 4 ----
  # Title: Natural splines for maternal weight gain at measurement. Our analyses 
  # were adjusted for maternal weight gain at measurement, including a natural 
  # spline with 2 df. Left panel (respectively right panel) represents smoothed 
  # curves of weight gain at measurements against zSBP (respectively zDBP) values 
  # across gestational ages. Shaded areas represent range of the confidence intervals.
  fig_weight_spline_sbp = make_smooth(data_bp_imp_eden$final, exposure = "mo_weight_gain_cont", outcome = "sbp_z_adapted", df=2), # (left)
  fig_weight_spline_dbp = make_smooth(data_bp_imp_eden$final, exposure = "mo_weight_gain_cont", outcome = "dbp_z_adapted", df=2), # (right)
  
  # * Supplementary Figure 5 ----
  # Title: Pearson correlation between environmental exposures up to the 4th week 
  # before BP measurement. Correlation matrix includes: ambient temperature indicators 
  # (Tmax, Tmean and Tmin), relative humidity (RH), and air pollution exposure 
  # (PM2.5, PM10, NO2 and O3) over each of the 4 weeks preceding BP measurements. 
  fig_corplot_expo_eden = make_cor_expo(data_bp_imp_eden$final),
  
  # * Supplementary Figure 6 ----
  # Title: Overview of the maternal CV function through months of measurement 
  # for each CV health indicator. Calendar months were reported in the chronological 
  # order (from January to December). (top-left) Description of SBP levels (mmHg) 
  # through month of measurement. (top-right) Description of DBP levels (mmHg) 
  # through month of measurement. (mid-left) Description of zSBP (SD) through 
  # month of measurement. (mid-right) Description of zDBP (SD) through month of 
  # measurement. (bottom-left) Description of HR levels (bpm) through month of 
  # measurement. (bottom-right) Description of Hte levels (%) through month of 
  # measurement.
  fig_plot_sbp_eden = make_plot_hemo(data_bp_imp_eden$final, "cs_tas", "po_monthe"), # (top-left)
  fig_plot_sbpz_eden = make_plot_hemo(data_bp_imp_eden$final, "sbp_z_adapted", "po_monthe"), # (mid-left)
  fig_plot_dbp_eden = make_plot_hemo(data_bp_imp_eden$final, "cs_tad", "po_monthe"), # (top-right)
  fig_plot_dbpz_eden = make_plot_hemo(data_bp_imp_eden$final, "dbp_z_adapted", "po_monthe"), # (mid-right)
  fig_plot_hr_eden = make_plot_hemo(data_hr_imp_eden$final, "hr", "po_monthe"), # (bottom-left)
  fig_plot_hte_eden = make_plot_hemo(data_hte_imp_eden$final, "hte", "po_monthe"), # (bottom-right)
  
  # * Supplementary Figure 7 ----
  # Title: Distribution of the number of days during which mothers were exposed 
  # to heat or cold in the 28 days preceding blood pressure assessment. Heat 
  # (respectively cold) was defined as any day exceeding (respectively below) 
  # the 95th (respectively 5th) percentile of the local temperature distributions 
  # in Nancy (respectively Poitiers) area. 
  fig_plot_n_expo_eden = make_count_expo(data_bp_w_eden$data_expo_yn_h[[1]],data_bp_w_eden$data_expo_yn_c[[1]]),
  
  # * Supplementary Figure 8 ----
  # Title: Overall cumulative associations (0-28 days) between Tmean and Hte levels 
  # in main and sex-stratified analyses. Overall cumulative association at W0 for 
  # the whole study population including moderation test statistics for sex differences 
  # between male and female offspring (gray dashed CI; top panel). Overall cumulative 
  # associations in women pregnant with males (bottom-left panel) and females 
  # (bottom-right) panel between Tmean and Hte levels. Shaded areas represent range 
  # of the confidence intervals. Associations correspond to DLNM including random 
  # effects with 2 and 3 df for dose- and lag-response curves respectively. 
  # Models were adjusted for: recruitment center, maternal age at conception, 
  # maternal educational attainment, maternal parity, maternal pre-pregnancy BMI, 
  # maternal weight gain at measurement, maternal smoking habits, hypertension 
  # before pregnancy, hypertension during pregnancy coupled to medication, 
  # relative humidity (same lag structure as the temperature exposure), time trend 
  # at measurement, EDI and NDVI.
  # Check figure_dlnm.R file
  
  # * Supplementary Figure 9 ----
  # Title: Overall cumulative associations (0-28 days) between Tmean and zSBP levels, 
  # stratified by air pollution and NDVI exposure levels over the whole study period. 
  # High (respectively low) levels were defined as the third (respectively first) 
  # tertiles of each air pollution or NDVI exposure distribution for the whole study 
  # population. Shaded areas represent range of the confidence intervals. 
  # Associations correspond to DLNM including random effects with 2 and 3 df for 
  # the dose- and lag-response curves respectively. Models were adjusted for: 
  # recruitment center, sex of the offspring, maternal age at conception, maternal 
  # educational attainment, maternal parity, maternal pre-pregnancy BMI, maternal 
  # weight gain at measurement, maternal smoking habits at trimester of measurement, 
  # hypertension before pregnancy, hypertension during pregnancy coupled to medication 
  # status, time trend at measurement, relative humidity, EDI and NDVI (except for 
  # analyses stratified on NDVI levels).
  # Check figure_dlnm.R file
  
  # * Supplementary Figure 10 ----
  # Title: Overall cumulative associations (0-28 days) between Tmean and zSBP levels, 
  # stratified by air pollution and NDVI exposure levels over the whole study period. 
  # High (respectively low) levels were defined as the third (respectively first) 
  # tertiles of each air pollution or NDVI exposure distribution for the whole study 
  # population. Shaded areas represent range of the confidence intervals. Associations 
  # correspond to DLNM including random effects with 2 and 3 df for the dose- and 
  # lag-response curves respectively. Models were adjusted for: recruitment center, 
  # sex of the offspring, maternal age at conception, maternal educational attainment, 
  # maternal parity, maternal pre-pregnancy BMI, maternal weight gain at measurement, 
  # maternal smoking habits at trimester of measurement, hypertension before pregnancy, 
  # hypertension during pregnancy coupled to medication status, time trend at measurement, 
  # relative humidity, EDI and NDVI (except for analyses stratified on NDVI levels).
  # Figure created outside the targets pipeline
  # Check figures_sensitivity_analyses.R file
  
  ## Additional file 2: Supplementary Tables ----
  
  # * Supplementary Table 1 ----
  # Title: Akaike Information Criterion (AIC) used for degrees of freedom (df) 
  # selection for mean temperature. dfvar indicates the number of df tested for 
  # the dose-response curve while dflag indicates the number of df tested for the 
  # lag-response curve.
  tbl_aic_sbp = make_tbl_aic(model_bp_w_tt, "sbp_z_adapted"), # SBP on W0
  tbl_aic_sbp_wx = make_tbl_aic(model_bp_wx_tt, "sbp_z_adapted"), # SBP on {W1,W2,W3,W4,W5}
  tbl_aic_dbp = make_tbl_aic(model_bp_w_tt, "dbp_z_adapted"), # DBP on W0
  tbl_aic_dbp_wx = make_tbl_aic(model_bp_wx_tt, "dbp_z_adapted"), # DBP on {W1,W2,W3,W4,W5}
  tbl_aic_hr = make_tbl_aic(model_hr_tt, "hr"), # HR on W0
  tbl_aic_hte = make_tbl_aic(model_hte_tt, "hte"), # Hte on W0
  
  # * Supplementary Table 2 ----
  # Title: Characteristics of the study population: 1840 and 1832 pregnant women 
  # from the EDEN cohort with one measurement of HR and Hte, respectively.
  tbl_desc_hr_eden = make_tbl_desc_var(data_hr_imp_eden$final, var="center"), # Heart rate
  tbl_desc_hte_eden = make_tbl_desc_var(data_hte_imp_eden$final, var="center"), # Hematocrit
  
  # * Supplementary Table 3 ----
  # Title: Characteristics of the study population by sex of the offspring
  tbl_desc_bp_sex_eden = make_tbl_desc_var(data_bp_imp_eden$final, var="ch_sex", repeated=T),

  # * Supplementary Table 4 ----
  # Title: Description of temperature exposure at various percentiles thresholds
  # for each study center. Unless otherwise indicated, bold cells indicate the
  # thresholds used in our analyses.
  
  # * Supplementary Table 5 ----
  # Title: Characteristics of the study population experiencing heat or cold exposure 
  # in the 28 days preceding maternal BP measurement for Tmean. Heat is defined 
  # as an exposure to ambient temperature exceeding the 95th percentile (20°C) of 
  # the temperature distribution in Nancy area and cold exposure is characterized 
  # by an exposure to ambient temperature below the 5th percentile of the temperature 
  # distribution around Poitiers agglomeration (4°C). The reference group included 
  # mothers that did not have their averaged Tmean exposure above the above-mentioned 
  # heat threshold nor below the above-mentioned cold threshold.
  tbl95_expo_eden = make_tbl_desc_temp(data_bp_imp_eden$final, p=95, repeated=T),
  
  # * Supplementary Table 6 ----
  # Title: Overall cumulative predicted association between ambient temperature 
  # exposure and zSBP through various susceptibility windows and exposure thresholds. 
  # Results were adjusted for: recruitment center, sex of the offspring, maternal 
  # age, parity, maternal pre-pregnancy BMI, weight gain at measurement, maternal 
  # educational attainment, maternal smoking habits at trimester of measurement, 
  # hypertension before pregnancy, hypertension during pregnancy coupled to medication 
  # status (at W0, W4 and W5 only), time trend at measurement, RH, NDVI and EDI. 
  # Dose-response relationship was modeled with 2 df while lag-response relationship 
  # was modeled based on 3 df. For both heat and cold exposures, associations with 
  # zSBP at various thresholds (99th, 97th and 95th percentile values in Nancy and 
  # 1st, 3rd and 5th percentile values in Poitiers) and for several temperature 
  # indicators were reported as compared to the median of the total temperature 
  # distribution (11°C). Bold text indicates statistically significant overall 
  # cumulative associations.
  tbl_res_w_23_zsbp = make_tbl_res_overall(model_bp_w_tt, dfv=2, dfl=3, out="sbp_z_adapted"), # SBP, W0
  tbl_res_wx_23_zsbp = make_tbl_res_overall(model_bp_wx_tt, dfv=2, dfl=3, out="sbp_z_adapted"), # SBP, {W2,W3,W4,W5}
  tbl_res_w1_23_zsbp = make_tbl_res_overall(model_bp_w1_tt, dfv=2, dfl=3, out="sbp_z_adapted"), # SBP, W1
  
  # * Supplementary Table 7 ----
  # Title: Overall cumulative predicted association between ambient temperature 
  # exposure and zDBP through various susceptibility windows and exposure thresholds. 
  # Results were adjusted for: recruitment center, sex of the offspring, maternal 
  # age, parity, maternal pre-pregnancy BMI, weight gain at examination, maternal 
  # educational attainment, maternal smoking habits at trimester of measurement, 
  # hypertension before pregnancy, hypertension during pregnancy coupled to medication 
  # status (at W0, W4 and W5 only), time trend at measurement, RH, NDVI and EDI. 
  # Dose-response relationship was modeled with 2 df while lag-response relationship 
  # was modeled based on 3 df. For both heat and cold exposures, associations with 
  # zDBP at various thresholds (99th, 97th and 95th percentile values in Nancy and 
  # 1st, 3rd and 5th percentile values in Poitiers) and for several temperature 
  # indicators were reported as compared to the median of the total temperature 
  # distribution (11°C). Bold text indicates statistically significant overall 
  # cumulative associations.
  tbl_res_w_23_zdbp = make_tbl_res_overall(model_bp_w_tt, dfv=2, dfl=3, out="dbp_z_adapted"), # DBP, W0
  tbl_res_wx_23_zdbp = make_tbl_res_overall(model_bp_wx_tt, dfv=2, dfl=3, out="dbp_z_adapted"), # DBP, {W2,W3,W4,W5}
  tbl_res_w1_23_zdbp = make_tbl_res_overall(model_bp_w1_tt, dfv=2, dfl=3, out="dbp_z_adapted"), # DBP, W1
  
  # * Supplementary Table 8 ----
  # Title: Overall cumulative predicted association between heat and cold exposure 
  # with HR, Hte and DP levels. Results were adjusted for: recruitment center, 
  # sex of the offspring, maternal age, parity, maternal pre-pregnancy BMI, weight 
  # gain at examination, maternal educational attainment, maternal smoking habits 
  # at trimester of measurement, maternal caffeine intake at trimester of measurement 
  # (HR and DP models only), hypertension before pregnancy (Hte and DP models only), 
  # hypertension during pregnancy coupled to medication status (Hte and DP models only), 
  # time trend at measurement, RH, NDVI and EDI. Dose-response relationship was 
  # modeled with 2 df while lag-response relationship was modeled based on 3 df 
  # at W0. For all three CV indicators, associations with subacute heat and cold 
  # exposure at various thresholds were reported as compared to the median of the 
  # temperature distribution (11°C). Bold text indicates statistically significant 
  # overall cumulative associations.
  tbl_res_23_hr = make_tbl_res_overall(model_hr_tt, dfv=2, dfl=3, out="hr"), # HR
  tbl_res_23_dp = make_tbl_res_overall(model_dp_tt, dfv=2, dfl=3, out="dp"), # DP
  tbl_res_23_hte = make_tbl_res_overall(model_hte_tt, dfv=2, dfl=3, out="hte") # Hte
  
  ## Additional file 3: Supplementary Text ----
  
)
