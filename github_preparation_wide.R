# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Data preparation for maternal cardiovascular health analysis
# This script includes:
# - Variables renaming and coding
# - Exclusion of participants based on exposure/outcome availability
# - Data set creation for each outcomes
# - Missing data imputation

# 1. Raw data preparation ----

#' Prepare and Clean Data for Analysis
#'
#' This function performs on raw data a series of cleaning 
#' and transformation steps. It renames variables containing specific patterns,
#' recodes and categorizes variables, combines variables based on certain
#' conditions.
#'
#' @param data_rawx Several data frames containing raw data (x=1,...,5) to be cleaned and prepared
#' @param data_temp A data frame containing temperature exposure data
#' @param data_tmean A data frame containing information about the model used to estimate exposure 
#' @return A cleaned and transformed data frame ready for analysis
prep_data_clean <- function(data_raw1, data_raw2, data_raw3, data_raw4, data_raw5, data_temp, data_tmean){
  ## hidden
}

#' Prepare and Clean Weights Data for Analysis
#'
#' This function performs on cleaned weights data (EDEN team work) a series of 
#' imputations so that weights are available for each BP, HR and Hte measurement
#' 
#' @param data_weight A data frame containing data on weights during pregnancy
#' @return A cleaned and transformed data frame of weights during pregnancy, ready for analysis
prep_data_weight <- function(data_weight){
  ## hidden
}

#' Prepare and Clean BP Z-scores Data for Analysis
#'
#' This function performs on cleaned BP Z-scores data. This functions mainly
#' renamed columns such that new Z-scores data fit the former data base of Z-scores
#' 
#' @param data_zscores A data frames containing BP Z-scores data
#' @return A cleaned data frame of BP Z-scores, ready for analysis
prep_data_zscores <- function(data_zscores){
  ## hidden
}


# 2. Maternal cardiovascular function ----

#' Prepare and Clean Data for BP Analysis
#'
#' This function performs on cleaned data a series of cleaning and 
#' filtering steps so that BP Z-scores data are ready for analysis
#'
#' @param data_in A data frames containing cleaned data 
#' @param data_zscores A data frame containing Z-scores of BP
#' @return A cleaned BP Z-scores data frame ready for analysis
prep_zbp <- function(data_in, data_zscores){
  ## hidden
}

#' Prepare and Clean Data for HR Analysis
#'
#' This function performs on cleaned data a series of cleaning and 
#' filtering steps so that HR data are ready for analysis
#'
#' @param data_in A data frame containing cleaned data
#' @return A cleaned HR data frame ready for analysis
prep_hr_eden <- function(){
  ## hidden
}

#' Prepare and Clean Data for Hte Analysis
#'
#' This function performs on cleaned data a series of cleaning and 
#' filtering steps so that Hte data are ready for analysis
#'
#' @param data_in A data frame containing cleaned data
#' @return A cleaned Hte data frame ready for analysis
prep_hte_eden <- function(data_in){
  ## hidden
}

#' Participants exclusion (residential historic quality)
#'
#' a. Exclusion of participants with low reliable residential historic
#' b. Exclusion of participants with imputed moving-in dates 
#' 
#' @param data_in A data frame containing cleaned data
#' @param data_flag A data frame containing residential historic quality data
#' @return A cleaned data frame with exclusion of some participants because of unreliable exposure data
prep_data_exclusion <- function(data_in, data_flag){
  ## hidden
}

#' Participants exclusion (outcome)
#'
#' a. Filtering participants with outcome not measured
#' b. Filtering participants with missing date of measurement
#' c. Filtering participants with outcome measured outside the study time period
#' 
#' @param data_in A data frame containing cleaned data
#' @return A cleaned data frame with exclusion of some participants because of missing outcome data
prep_data_filter <- function(data_in){
  
  # Identify the outcome of interest
  outcome = ifelse(!"outcome" %in% colnames(data_in),"sbp_z_adapted","outcome") 
  
  # Filtering
  data_out <- data_in |>
    dplyr::filter(
      !is.na(.data[[outcome]]),   # Outcome not measured
      !is.na(date_exam))          # Missing time of measurement
  
  return(data_out)
}

#' Maternal cardiovascular health, final data sets
#'
#' @param data_in A data frame containing the cleaned data 
#' @param outcome The outcome of interest
#' @param data_flag A data frame containing residential historic quality data
#' @param data_weight A data frame containing cleaned/imputed weight gains in pregnancy
#' @param data_date A data frame containing exposure data 
#' @param data_zbp A data frame containing BP Z-scores, if the outcome of interest is BP
#'  Default to NULL
#' @return A data frame ready for analyses
prep_data_final <- function(data_in, outcome, data_flag, data_weight, data_date, data_zbp = NULL){
  
  # Checks for adequate inputs
  if(is.null(data_zbp) & outcome=="bp") stop("Computation of BP data frame requires BP Z-scores as input")
  if(!outcome %in% c("bp","hr","hte")) stop("Inadequate outcome provided")
  
  # Performs relevant preparation based on the outcome
  if(outcome=="bp"){ 
    data_prep = prep_zbp(data_in, data_zbp) # BP data set preparation
  }
  if(outcome=="hr"){ 
    data_prep = prep_hr_eden(data_in) # HR data set preparation
  }
  if(outcome=="hte"){ 
    data_prep = prep_hte_eden(data_in) # Hte data set preparation
  }
  
  # Exclusion of participants
  data_out = prep_data_filter(data_prep) # Exclusion with regard to the outcome measurement         
  data_out = prep_data_exclusion(data_out, data_flag) # Exclusion with regard to the residential historic quality
  
  # Add cleaned/imputed weights gain in pregnancy
  ## hidden
  
  return(data_out)
}


# 3. Missing data imputation ----

#' Missing data imputation
#' 
#' This function performs single missing data imputation based on chained 
#' equations method (MICE)
#' 
#' @param data_in A data frame containing the data on which imputation has 
#' to be performed, in wide format (no long format allowed)
#' @param list_cov_imp All covariates which have used in the imputation process
#' @return A list containing results of the imputation
prep_imputation <- function(data_in, list_cov_imp){
  
  # Covariates
  #
  # Prepare covariates for imputation
  #
  # For BP models with repeated measurements, time-dependent covariates cannot be imputed as such, and are replaced by other covariates
  # removed 'date_exam_sin','date_exam_cos' and replaced by 'po_monthc'
  # removed 'mo_tabac' and replaced by 'mo_tabac_any'
  # removed 'mo_tabacp' and replaced by 'mo_tabacp_any'
  # removed 'mo_weight_gain_spline1' and 'mo_weight_gain_spline2' and replaced by 'mo_weight_bepr'
  # removed 'mo_med_ghtn3' and replaced by 'diag_ghtn_all', 'diag_htn_trait' and 'diag_other_aspi'
  #
  # Meteorological-related covariates (all models):
  #   - Tmean averaged over the 4 weeks before measurement (exposure), non-linear: natural spline and 2 df
  #   - Relative humidity averaged over the 4 weeks before measurement, non-linear: natural spline and 2 df
  #
  # Outcome (all models):
  
  # 0. Remove and replace 
  list_cov1 <- list_cov_imp[! list_cov_imp %in% c("mo_tabac","mo_tabacp")]
  list_cov1 <- c(list_cov1,"mo_tabac_any","mo_tabacp_any")
  
  # 1. For BP models
  if(!"outcome"%in%colnames(data_in$final)){
    # Excluding time-dependent covariates
    list_cov1 <- list_cov1[! list_cov1 %in% c("date_exam_sin","date_exam_cos","mo_weight_gain_spline1","mo_weight_gain_spline2","mo_med_ghtn3")]
    # Replace it by their proxy for imputation model
    list_cov1 <- c(list_cov1,"po_monthc","mo_weight_bepr","diag_ghtn_all","diag_other_all2")
    # Adding the outcome
    list_cov2 <- c(list_cov1,"mean_zsbp")
    # Adding meteorological-related covariates
    list_cov2 <- c(list_cov2,"ns_tmean1","ns_tmean2","ns_rhum1","ns_rhum2")
    
    ns_tmean <- splines::ns(data_in$final$tmean_overall,2)
    ns_rhum <- splines::ns(data_in$final$rhum_overall,2)
  }
  
  # 2. For other models (HR, Hte): Excluding time-dependent covariates
  if("outcome"%in%colnames(data_in$final)){
    # Adding the outcome and gestational age at measurement
    list_cov2 <- c(list_cov1,"outcome","cs_ga")
    # Adding meteorological-related covariates
    list_cov2 <- c(list_cov2,"ns_tmean1","ns_tmean2","ns_rhum1","ns_rhum2")
    
    ns_tmean <- splines::ns(data_in$final$tmean_mean_4,2)
    ns_rhum <- splines::ns(data_in$final$rhum_mean_4,2)
  }
  
  # Adding meteorological-related covariates under natural splines, with 2 df
  data_in$final$ns_tmean1 <- ns_tmean[,1]
  data_in$final$ns_tmean2 <- ns_tmean[,2]
  data_in$final$ns_rhum1 <- ns_rhum[,1]
  data_in$final$ns_rhum2 <- ns_rhum[,2]
  
  ## Data frame containing covariates not included in the imputation regressions 
  # but still relevant for descriptive purpose later on
  data_left_cov <- data_in$final |>
    dplyr::select(!all_of(list_cov2))
  
  ## Data frame containing covariates included in the imputation regressions
  data <- data_in$final |>
    dplyr::select(
      all_of(list_cov2) # Keep covariates included in imputation regression only
    ) 
  
  ## Imputation using mice
  nimpute <- 1 # Single imputation
  
  data_imp0 <- mice(data, maxit = 0) # First empty imputation to initialize parameters
  
  ### Parameters initialization
  # Methods checks
  # 'logreg' for binary covariates
  # 'polyreg' for categorical covariates, more than 2 classes
  meth <- data_imp0$method 
  pred_mat <- matrix(rep(1), nrow = ncol(data), ncol = ncol(data)) 
  # Predictive matrix
  names <- colnames(data)
  rownames(pred_mat) <- names
  colnames(pred_mat) <- names
  diag(pred_mat) <- 0
  # Priors for predictions
  post <- data_imp0$post # no priors
  
  ## MICE imputation
  data_imp <- mice(
    data = data,
    m = nimpute,
    maxiter = 10,
    predictorMatrix = pred_mat,
    seed = 1024,
    method = meth,
    post = post
  )
  
  ## Complete the data frame with no more missing data
  data_out <- complete(data_imp)
  data_out <- data_out |>
    dplyr::mutate(
      id = data_left_cov$id, # Get maternal unique identifiers
      across(all_of(list_cov2), # Adding informing variables about the imputation process
             list( c = ~ ifelse(meth[match(dplyr::cur_column(), list_cov2)] != "", 
                                meth[match(dplyr::cur_column(), list_cov2)], NA)),
             .names = "imp_method_{.col}")) 
  data_out <- full_join(data_out, data_left_cov, by="id") # Get other covariates not used for the imputation
  
  ## Imputed values
  data_values <- rbindlist(data_imp$imp, fill=TRUE, use.names=TRUE, idcol="files") # Formating results of predictions
  
  # Need to pass missing data imputation to time-dependent variables
  ## hidden 
  
  list_out <- list(
    "final" = data_out,                         # Complete data set
    "tmin" = data_in$tmin,
    "tmax" = data_in$tmax,
    "tmean" = data_in$tmean,
    "tvar" = data_in$tvar,
    "rhum" = data_in$rhum,
    "pm25" = data_in$pm25,
    "pm10" = data_in$pm10,
    "no2" = data_in$no2,
    "o3" = data_in$o3,
    "imputation" = data_values,                 # Imputed values
    "location" = data_imp$where,                # Location of imputed values
    "covariates" = data_imp$visitSequence       # Covariates used for imputation regression 
  )
  
  return(list_out)
}

#' Missing data imputation
#' 
#' This function performs single missing data imputation based on chained 
#' equations method (MICE) for BP data sets (repeated measurements). This function
#' will be used at last choice after wide format data sets imputation
#' 
#' @param data_in A data frame containing the BP data on which imputation has 
#' to be performed, in long format (no wide format allowed)
#' @param data_imp1 A data frame containing already-imputed data set (wide format, for another outcome)
#' @param data_imp2 A data frame containing already-imputed data set (wide format, for another outcome)
#' @param list_cov_imp All covariates which have used in the imputation process
#' @return A list containing results of the imputation
prep_final_imputation_bp <- function(data_in, data_imp1, data_imp2, list_cov_imp){
  
  # Complete the long-format data set of BP measurements based on previous imputations on HR and Hte
  # This method helps to avoid multiple imputations of same missing data
  data_help <- prep_complete(data_imp1, prep_complete(data_imp2, data_in, cohort), cohort)
  
  # If specific-BP measurements with missing data on some covariates
  # Compute of wide-format data set of BP measurements
  help <- data_help$final |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      mean_zsbp = mean(sbp_z_adapted),
      tmean_overall = mean(tmean_mean_4),
      rhum_overall = mean(rhum_mean_4)
    ) |>
    dplyr::slice_head(n=1)
  data_help2 = data_help
  data_help2$final = help
  
  # Adding the outcome
  
  # Impute the wide-format data set
  data_imp = prep_imputation(data_help2, list_cov_imp)
  # Again, complete the long-format data set of BP measurements based on previous imputation on BP 
  data_out = prep_complete(data_imp, data_help)
  
  return(data_out)
}