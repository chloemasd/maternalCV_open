# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# 1. Coding variables ----

#' Assign the susceptibility window
#'
#' This function assigns the corresponding susceptibility window as defined
#' in the statistical analysis plan upon the gestational age of the exam
#'
#' @param vec_ga A vector of gestational ages to be assigned to a susceptibility 
#' window
#' @return A vector of corresponding susceptibility windows 
help_get_window <- function(vec_ga){
  
  vec_out <- case_when( # Susceptibility window attribution
    vec_ga >= 4 & vec_ga < 11 ~ "w1",  # W1 : 4-10 weeks of gestation
    vec_ga >= 11 & vec_ga< 18 ~ "w2",  # W2 : 11-17 weeks of gestation
    vec_ga >= 18 & vec_ga < 25 ~ "w3", # W3 : 18-24 weeks of gestation
    vec_ga >= 25 & vec_ga < 32 ~ "w4", # W4 : 25-31 weeks of gestation
    vec_ga >= 32 & vec_ga < 39 ~ "w5", # W5 : 32-38 weeks of gestation
    TRUE ~ NA) 
  return(vec_out)
}

#' Assign the trimester
#'
#' This function assigns the corresponding trimester of pregnancy upon the 
#' gestational age of the exam
#'
#' @param vec_ga A vector of gestational ages to be assigned to a trimester
#' @return A vector of corresponding trimesters 
help_get_trimester <- function(vec_ga){
  
  vec_out <- case_when(  # Trimester of measurement attribution
    vec_ga < 14 ~ "t1",  # First trimester
    vec_ga >= 28 ~ "t3", # Third trimester
    TRUE ~ "t2")         # Otherwise, second trimester
  return(vec_out)
}

# 2. Adding exposure data ----

#' Assign column numbers in the exposure data upon date of examination 
#'
#' Takes a data frame in long format and returns the corresponding column
#' numbers of the exposure data to be picked up to calculate the starting
#' point of the exposure (date of examination)
#' Note: this function can have a high computation time
#' 
#' @param data_long A data frame in long format containing dates of
#' examinations which served as the reference for exposure calculation
#' @param data_date A data frame containing daily dates of exposure measures
#' @return A vector containing the column numbers in the exposure data frame to
#' start the window of exposure from
help_get_expo <- function(data_long, data_date){
  ## hidden
}

#' Compute weekly averaged exposure upon specified period of time 
#'
#' Takes a data frame and returns the corresponding column
#' numbers of the exposure data to be picked up to calculate the starting
#' point of the exposure (date of measurement)
#' 
#' @param data_long A data frame
#' @param data_expo A data frame containing daily values of exposure 
#' @param col_start A vector of columns indices in the exposure data frame 
#' corresponding to the date of measurement
#' @param col_end A vector of columns indices in the exposure data frame
#' corresponding to the end date of studied period
#' @return A vector containing the mean exposure values on the specified period
#' of time
help_get_expo_week <- function(data_long, data_expo, col_start, col_end){
  ## hidden
}

#' Compute daily exposure upon specified period of time 
#'
#' Takes a data frame in long format and returns the corresponding column
#' numbers of the exposure data to be picked up to calculate the starting
#' point of the exposure (date of measurement)
#' 
#' @param data_long A data frame in long format
#' @param data_expo A data frame containing daily values of exposure 
#' @param col_start A vector of columns indices of the exposure data frame 
#' corresponding to the date of measurement
#' @param days The length of the time window in days
#'  Default to 28
#' @param bp A boolean indicating whether outcome of interest is BP
#' @return A vector containing the mean exposure values on the specified period
#' of time
help_get_expo_day <- function(data_long, data_expo, col_start, days = 28, bp){
  ## hidden
}

#' Compute Temperature Variability exposure
#'
#' Takes two data frames of ambient minimum and maximum temperature exposure
#' and creates the data frame of exposure to temperature variability defined as:
#' Tmax - Tmin.
#'
#' @param data_tmax A data frame containing maximum temperature exposure
#' @param data_tmin A data frame containing minimum temperature exposure
#' @return A data frame of temperature variability exposure
help_get_tvar <- function(data_tmax, data_tmin){
  
  columns = as.character(rev(seq(29:1)))
  
  tmax <- data_tmax |> dplyr::select(all_of(columns))
  tmin <- data_tmin |> dplyr::select(all_of(columns))
  
  data_tvar <- tmax - tmin # Calculate Tvar
  # Adding information (identifier, timing)
  data_tvar$window = data_tmax$window
  data_tvar$id = data_tmax$id
  data_tvar$number = data_tmax$number
  data_tvar$idxnb = data_tmax$idxnb
  # Adding the outcome
  if("sbp_z_adapted"%in%colnames(data_tmax)){
    data_tvar$sbp_z_adapted = data_tmax$sbp_z_adapted
    data_tvar$dbp_z_adapted = data_tmax$dbp_z_adapted }
  if("cs_hr"%in%colnames(data_tmax)){
    data_tvar$cs_hr = data_tmax$cs_hr }
  
  return(data_tvar)
}

#' Compute binary exposure to Heat
#'
#' Takes a continuous value of temperature exposure and binarizes it based on a
#' threshold value
#'
#' @param val A temperature exposure value (continuous)
#' @param perc A temperature threshold (continuous) corresponding to a percentile
#' for heat exposure
#' @return 1 for heat exposure, 0 otherwise
help_yn_hot <- function(val, perc) {
  return(ifelse(val>=perc,1,0))
}

#' Compute binary exposure to Cold
#'
#' Takes a continuous value of temperature exposure and binarizes it based on a
#' threshold value
#'
#' @param val A temperature exposure value (continuous)
#' @param perc A temperature threshold (continuous) corresponding to a percentile
#' for cold exposure
#' @return 1 for cold exposure, 0 otherwise
help_yn_cold <- function(val, perc) {
  return(ifelse(val<=perc,1,0))
}

help_round_expo <- function(percentile){
  
  digit <- percentile %% 1 
  
  p_out <- ifelse(digit>0.5 & digit<=0.75, floor(percentile)+0.5,
            ifelse(digit>0.25 & digit<=0.5, floor(percentile)+0.5,
             ifelse(digit<=0.25, floor(percentile), ceiling(percentile))))
  return(p_out)
}


# 3. Handle long data frame formats ----

#' Transform data frame in wide format into long format
#'
#' Takes a data frame in wide format and transforms the data frame into long 
#' format upon one given variable
#'
#' @param data_in A data frame containing data in wide format
#' @param var A string indicating the variable to transform the long data on
#' @return A data frame in the long format with corresponding values
help_wide_to_long <- function(data_in, var){
  ## hidden
}

#' Create a nested data frame
#'
#' Takes a data frame and nests it based based on susceptibility windows and outcome.
#' The final nested data frame has 3 columns: 'window' indicating on which 
#' susceptibility window analyses need to be run over; 'outcome' to indicate the
#' outcome of interest in the analysis; 'data_' a data frame containing the data
#' for the analysis (whether covariates or exposure data)
#'
#' @param data_in A data frame containing data in wide format
#' @param col_name A string indicating the type of data to be nested (covariates or exposure)
#' @return A nested data frame 
help_data_nest <- function(data_in, col_name){
  
  data_unnest <- data_in |> # Long format data frame based on the outcomes (SBP and DBP)
    tidyr::pivot_longer(cols = c("sbp_z_adapted","dbp_z_adapted"), # Outcome
                        names_to = "outcome",
                        values_to = "value_bp_z_adapted") 
  
  if(col_name!="data"){ # For exposure data frames only
    data_unnest <- data_unnest |>
      dplyr::select( # Select only columns containing exposure values
        -value_bp_z_adapted,-id,-number,-idxnb) }
  
  # Final nested data frame for susceptibility windows W1,W2,W3,W4 and W5
  data_nest_win <- data_unnest |> # Nested data frame based on: the outcome and the susceptibility window
    dplyr::group_by(window, outcome) |> 
    tidyr::nest()
  
  # We need to add to our nested data frame the analysis based on the whole study period (W0)
  # --> we create another nested data frame to be bind to the final nested data frame later on
  data_nest_all <- data_unnest |> 
    dplyr::group_by(outcome) |> # Again, group on outcome
    dplyr::select(-window) |> # We do not need susceptibility window anymore
    tidyr::nest() |> # Nest
    dplyr::mutate(window = as.character("w")) |> # Whole study period 
    dplyr::select(window, everything()) # To reorder columns 
  
  # Nested tables join to duplicate analyses on the whole study period (W0) not only susceptibility windows
  data_nest = rbind(data_nest_win, data_nest_all)
  colnames(data_nest) <- c("window","outcome",col_name)
  
  return(data_nest)
}

#' Create a nested data frame for analyses with repeated measurements
#'
#' Takes a data frame and nests it based based on susceptibility windows and outcome.
#' The final nested data frame has 3 columns: 'window' indicating on which 
#' susceptibility window analyses need to be run over; 'outcome' to indicate the
#' outcome of interest in the analysis; 'data_' a data frame containing the data
#' for the analysis (whether covariates or exposure data)
#'
#' @param data_in A data frame containing data in wide format
#' @param col_name A string indicating the type of data to be nested (covariates or exposure)
#' @return A nested data frame 
help_data_nest_bp <- function(data_in, col_name){
  
  # Repeated measurements on SBP and DBP
  data_unnest <- data_in |> # Long format data frame based on the outcomes
      tidyr::pivot_longer(cols = c("sbp_z_adapted","dbp_z_adapted"), # SBP and DBP
                          names_to = "outcome",
                          values_to = "value_bp_z_adapted")
  
  if(col_name!="data"){ # For exposure data frames only
    data_unnest <- data_unnest |>
      dplyr::select( # Select only columns containing exposure values
        -value_bp_z_adapted) }
  
  # Final nested data frame for susceptibility windows W1,W2,W3,W4 and W5
  data_nest_win <- data_unnest |> # Nested data frame based on: the outcome and the susceptibility window
    dplyr::group_by(window, outcome) |> 
    tidyr::nest()
  
  # We need to add to our nested data frame the analysis based on the whole study period (W0)
  # --> we create another nested data frame to be bind to the final nested data frame later on
  data_nest_all <- data_unnest |>
    dplyr::group_by(outcome) |> # Again, group on outcome
    dplyr::select(-window) |> # We do not need susceptibility window anymore
    tidyr::nest() |> # Nest
    dplyr::mutate(window = as.character("w")) |> # Whole study period
    dplyr::select(window, everything()) # To reorder columns 
  
  # Nested tables join to duplicate analyses on the whole study period (W0) not only susceptibility windows
  data_nest = rbind(data_nest_win, data_nest_all)
  colnames(data_nest) <- c("window","outcome",col_name)
  
  return(data_nest)
}

#' Get last known value
#'
#' This function helps imputing longitudinal data based on last known value
#' 
#' @param data_in A data frame containing longitudinal data
#' @param ident A string indicating the participant's identifier 
#' @param num A string indicating the examination number for which value is missing
#' @param var A string indicating the name of the variable for which value is missing
#' @return A data frame with missing value imputed to the last known values
help_get_last <- function(data_in, ident, num, var){
  ## hidden
}

# 4. Statistical analyses ----

#' Recalculate 95% Confidence Intervals for Cumulative Stratified Associations
#'
#' This function recalculates 95% confidence intervals for stratified analyses using two input data sets.
#' It is designed to harmonize and update confidence intervals across strata.
#'
#' @param data1 A data frame containing the first set of stratified results
#' @param data2 A data frame containing the second set of stratified results to be compared with \code{data1}.
#'
#' @return A data frame with recalculated 95% confidence intervals.
#' The output includes lower and upper bounds of the CI, along with the original estimates.
help_test_moderation <- function(data1, data2){
  
  # Need to make sure same values were predicted in both strata
  index1 <- which(!data1$predvar %in% data2$predvar) # Get values not predicted in the first stratum
  index2 <- which(!data2$predvar %in% data1$predvar) # Get values not predicted in the second stratum
  
  if(length(index1)>0){ # First strata
    data1$predvar <- data1$predvar[-index1] # Remove unpredicted values
    data1$allfit <- data1$allfit[-index1] # Remove unpredicted estimate
    data1$alllow <- data1$alllow[-index1] # Remove unpredicted lower bound
    data1$allhigh <- data1$allhigh[-index1] } # Remove unpredicted upper bound 
  if(length(index2)>0){ # Second strata
    data2$predvar <- data2$predvar[-index2] # Remove unpredicted values
    data2$allfit <- data2$allfit[-index2] # Remove unpredicted estimate
    data2$alllow <- data2$alllow[-index2] # Remove unpredicted lower bound
    data2$allhigh <- data2$allhigh[-index2] } # Remove unpredicted upper bound
  
  if(!all(data1$predvar%in%data2$predvar)){
    stop("Data length differ") } # Check both strata share same predicted values
  
  data_out <- data.frame(
    "b_m" = data1$allfit,"b_f" = data2$allfit, # Estimate
    "lower_m" = data1$alllow,"lower_f" = data2$alllow, # Lower bound
    "upper_m" = data1$allhigh,"upper_f" = data2$allhigh) # Upper bound
  data_out$predvar <- rownames(data_out)
  data_out <- data_out |>
    dplyr::mutate(
      se_m = (upper_m - lower_m)/3.92, # Standard-error (first stratum)
      se_f = (upper_f - lower_f)/3.92, # Standard-error (second stratum)
      b = b_m - b_f, # Estimate, recalculated
      lower = (b_m - b_f) - 1.96*sqrt(se_m**2 + se_f**2), # Lower bound, recalculated
      upper = (b_m - b_f) + 1.96*sqrt(se_m**2 + se_f**2)) # Upper bound, recalculated
  
  return(data_out)
}

#' Recalculate 95% Confidence Intervals for Lagged Stratified Associations
#'
#' This function recalculates 95% confidence intervals for stratified analyses using two input data sets.
#' It is designed to harmonize and update confidence intervals across strata.
#'
#' @param data1 A data frame containing the first set of stratified results
#' @param data2 A data frame containing the second set of stratified results to be compared with \code{data1}.
#'
#' @return A data frame with recalculated 95% confidence intervals.
#' The output includes lower and upper bounds of the CI, along with the original estimates.
help_test_moderation_lag <- function(data1, data2){
  
  d1 <- data.frame(data1$matfit)
  d1 <- tibble::rownames_to_column(d1, "predvar")
  d1_low <- data.frame(data1$matlow)
  d1_low <- tibble::rownames_to_column(d1_low, "predvar")
  d1_upp <- data.frame(data1$mathigh)
  d1_upp <- tibble::rownames_to_column(d1_upp, "predvar")
  d2 <- data.frame(data2$matfit)
  d2 <- tibble::rownames_to_column(d2, "predvar")
  d2_low <- data.frame(data2$matlow)
  d2_low <- tibble::rownames_to_column(d2_low, "predvar")
  d2_upp <- data.frame(data2$mathigh)
  d2_upp <- tibble::rownames_to_column(d2_upp, "predvar")
  
  if(!all(d1$predvar%in%d2$predvar)){
    d1 <- d1 |> dplyr::filter(predvar%in%d2$predvar)
    d2 <- d2 |> dplyr::filter(predvar%in%d1$predvar)
    d1_low <- d1_low |> dplyr::filter(predvar%in%d1$predvar)
    d1_upp <- d1_upp |> dplyr::filter(predvar%in%d1$predvar)
    d2_low <- d2_low |> dplyr::filter(predvar%in%d2$predvar)
    d2_upp <- d2_upp |> dplyr::filter(predvar%in%d2$predvar) }
  
  # Updates columns name
  colnames(d1) <- paste0("b_m_",colnames(d1))
  colnames(d1)[1] <- "predvar"
  colnames(d1_low) <- paste0("lower_m_",colnames(d1_low))
  colnames(d1_low)[1] <- "predvar"
  colnames(d1_upp) <- paste0("upper_m_",colnames(d1_upp))
  colnames(d1_upp)[1] <- "predvar"
  colnames(d2) <- paste0("b_f_",colnames(d2))
  colnames(d2)[1] <- "predvar"
  colnames(d2_low) <- paste0("lower_f_",colnames(d2_low))
  colnames(d2_low)[1] <- "predvar"
  colnames(d2_upp) <- paste0("upper_f_",colnames(d2_upp))
  colnames(d2_upp)[1] <- "predvar"
  
  df_list <- list(d1,d1_low,d1_upp,d2,d2_low,d2_upp)
  data_out <- df_list %>% purrr::reduce(full_join, by="predvar")
  
  data_out <- data_out |>
    dplyr::mutate( # For all lags, one by one
      se_m_lag0 = (upper_m_lag0 - lower_m_lag0)/3.92,
      se_f_lag0 = (upper_f_lag0 - lower_f_lag0)/3.92,
      b_lag0 = b_m_lag0 - b_f_lag0,
      lower_lag0 = (b_m_lag0 - b_f_lag0) - 1.96*sqrt(se_m_lag0**2 + se_f_lag0**2),
      upper_lag0 = (b_m_lag0 - b_f_lag0) + 1.96*sqrt(se_m_lag0**2 + se_f_lag0**2),
      se_m_lag1 = (upper_m_lag1 - lower_m_lag1)/3.92,
      se_f_lag1 = (upper_f_lag1 - lower_f_lag1)/3.92,
      b_lag1 = b_m_lag1 - b_f_lag1,
      lower_lag1 = (b_m_lag1 - b_f_lag1) - 1.96*sqrt(se_m_lag1**2 + se_f_lag1**2),
      upper_lag1 = (b_m_lag1 - b_f_lag1) + 1.96*sqrt(se_m_lag1**2 + se_f_lag1**2),
      se_m_lag2 = (upper_m_lag2 - lower_m_lag2)/3.92,
      se_f_lag2 = (upper_f_lag2 - lower_f_lag2)/3.92,
      b_lag2 = b_m_lag2 - b_f_lag2,
      lower_lag2 = (b_m_lag2 - b_f_lag2) - 1.96*sqrt(se_m_lag2**2 + se_f_lag2**2),
      upper_lag2 = (b_m_lag2 - b_f_lag2) + 1.96*sqrt(se_m_lag2**2 + se_f_lag2**2),
      se_m_lag3 = (upper_m_lag3 - lower_m_lag3)/3.92,
      se_f_lag3 = (upper_f_lag3 - lower_f_lag3)/3.92,
      b_lag3 = b_m_lag3 - b_f_lag3,
      lower_lag3 = (b_m_lag3 - b_f_lag3) - 1.96*sqrt(se_m_lag3**2 + se_f_lag3**2),
      upper_lag3 = (b_m_lag3 - b_f_lag3) + 1.96*sqrt(se_m_lag3**2 + se_f_lag3**2),
      se_m_lag4 = (upper_m_lag4 - lower_m_lag4)/3.92,
      se_f_lag4 = (upper_f_lag4 - lower_f_lag4)/3.92,
      b_lag4 = b_m_lag4 - b_f_lag4,
      lower_lag4 = (b_m_lag4 - b_f_lag4) - 1.96*sqrt(se_m_lag4**2 + se_f_lag4**2),
      upper_lag4 = (b_m_lag4 - b_f_lag4) + 1.96*sqrt(se_m_lag4**2 + se_f_lag4**2),
      se_m_lag5 = (upper_m_lag5 - lower_m_lag5)/3.92,
      se_f_lag5 = (upper_f_lag5 - lower_f_lag5)/3.92,
      b_lag5 = b_m_lag5 - b_f_lag5,
      lower_lag5 = (b_m_lag5 - b_f_lag5) - 1.96*sqrt(se_m_lag5**2 + se_f_lag5**2),
      upper_lag5 = (b_m_lag5 - b_f_lag5) + 1.96*sqrt(se_m_lag5**2 + se_f_lag5**2),
      se_m_lag6 = (upper_m_lag6 - lower_m_lag6)/3.92,
      se_f_lag6 = (upper_f_lag6 - lower_f_lag6)/3.92,
      b_lag6 = b_m_lag6 - b_f_lag6,
      lower_lag6 = (b_m_lag6 - b_f_lag6) - 1.96*sqrt(se_m_lag6**2 + se_f_lag6**2),
      upper_lag6 = (b_m_lag6 - b_f_lag6) + 1.96*sqrt(se_m_lag6**2 + se_f_lag6**2),
      se_m_lag7 = (upper_m_lag7 - lower_m_lag7)/3.92,
      se_f_lag7 = (upper_f_lag7 - lower_f_lag7)/3.92,
      b_lag7 = b_m_lag7 - b_f_lag7,
      lower_lag7 = (b_m_lag7 - b_f_lag7) - 1.96*sqrt(se_m_lag7**2 + se_f_lag7**2),
      upper_lag7 = (b_m_lag7 - b_f_lag7) + 1.96*sqrt(se_m_lag7**2 + se_f_lag7**2),
      se_m_lag8 = (upper_m_lag8 - lower_m_lag8)/3.92,
      se_f_lag8 = (upper_f_lag8 - lower_f_lag8)/3.92,
      b_lag8 = b_m_lag8 - b_f_lag8,
      lower_lag8 = (b_m_lag8 - b_f_lag8) - 1.96*sqrt(se_m_lag8**2 + se_f_lag8**2),
      upper_lag8 = (b_m_lag8 - b_f_lag8) + 1.96*sqrt(se_m_lag8**2 + se_f_lag8**2),
      se_m_lag9 = (upper_m_lag9 - lower_m_lag9)/3.92,
      se_f_lag9 = (upper_f_lag9 - lower_f_lag9)/3.92,
      b_lag9 = b_m_lag9 - b_f_lag9,
      lower_lag9 = (b_m_lag9 - b_f_lag9) - 1.96*sqrt(se_m_lag9**2 + se_f_lag9**2),
      upper_lag9 = (b_m_lag9 - b_f_lag9) + 1.96*sqrt(se_m_lag9**2 + se_f_lag9**2),
      se_m_lag10 = (upper_m_lag10 - lower_m_lag10)/3.92,
      se_f_lag10 = (upper_f_lag10 - lower_f_lag10)/3.92,
      b_lag10 = b_m_lag10 - b_f_lag10,
      lower_lag10 = (b_m_lag10 - b_f_lag10) - 1.96*sqrt(se_m_lag10**2 + se_f_lag10**2),
      upper_lag10 = (b_m_lag10 - b_f_lag10) + 1.96*sqrt(se_m_lag10**2 + se_f_lag10**2),
      se_m_lag11 = (upper_m_lag11 - lower_m_lag11)/3.92,
      se_f_lag11 = (upper_f_lag11 - lower_f_lag11)/3.92,
      b_lag11 = b_m_lag11 - b_f_lag11,
      lower_lag11 = (b_m_lag11 - b_f_lag11) - 1.96*sqrt(se_m_lag11**2 + se_f_lag11**2),
      upper_lag11 = (b_m_lag11 - b_f_lag11) + 1.96*sqrt(se_m_lag11**2 + se_f_lag11**2),
      se_m_lag12 = (upper_m_lag12 - lower_m_lag12)/3.92,
      se_f_lag12 = (upper_f_lag12 - lower_f_lag12)/3.92,
      b_lag12 = b_m_lag12 - b_f_lag12,
      lower_lag12 = (b_m_lag12 - b_f_lag12) - 1.96*sqrt(se_m_lag12**2 + se_f_lag12**2),
      upper_lag12 = (b_m_lag12 - b_f_lag12) + 1.96*sqrt(se_m_lag12**2 + se_f_lag12**2),
      se_m_lag13 = (upper_m_lag13 - lower_m_lag13)/3.92,
      se_f_lag13 = (upper_f_lag13 - lower_f_lag13)/3.92,
      b_lag13 = b_m_lag13 - b_f_lag13,
      lower_lag13 = (b_m_lag13 - b_f_lag13) - 1.96*sqrt(se_m_lag13**2 + se_f_lag13**2),
      upper_lag13 = (b_m_lag13 - b_f_lag13) + 1.96*sqrt(se_m_lag13**2 + se_f_lag13**2),
      se_m_lag14 = (upper_m_lag14 - lower_m_lag14)/3.92,
      se_f_lag14 = (upper_f_lag14 - lower_f_lag14)/3.92,
      b_lag14 = b_m_lag14 - b_f_lag14,
      lower_lag14 = (b_m_lag14 - b_f_lag14) - 1.96*sqrt(se_m_lag14**2 + se_f_lag14**2),
      upper_lag14 = (b_m_lag14 - b_f_lag14) + 1.96*sqrt(se_m_lag14**2 + se_f_lag14**2),
      se_m_lag15 = (upper_m_lag15 - lower_m_lag15)/3.92,
      se_f_lag15 = (upper_f_lag15 - lower_f_lag15)/3.92,
      b_lag15 = b_m_lag15 - b_f_lag15,
      lower_lag15 = (b_m_lag15 - b_f_lag15) - 1.96*sqrt(se_m_lag15**2 + se_f_lag15**2),
      upper_lag15 = (b_m_lag15 - b_f_lag15) + 1.96*sqrt(se_m_lag15**2 + se_f_lag15**2),
      se_m_lag16 = (upper_m_lag16 - lower_m_lag16)/3.92,
      se_f_lag16 = (upper_f_lag16 - lower_f_lag16)/3.92,
      b_lag16 = b_m_lag16 - b_f_lag16,
      lower_lag16 = (b_m_lag16 - b_f_lag16) - 1.96*sqrt(se_m_lag16**2 + se_f_lag16**2),
      upper_lag16 = (b_m_lag16 - b_f_lag16) + 1.96*sqrt(se_m_lag16**2 + se_f_lag16**2),
      se_m_lag17 = (upper_m_lag17 - lower_m_lag17)/3.92,
      se_f_lag17 = (upper_f_lag17 - lower_f_lag17)/3.92,
      b_lag17 = b_m_lag17 - b_f_lag17,
      lower_lag17 = (b_m_lag17 - b_f_lag17) - 1.96*sqrt(se_m_lag17**2 + se_f_lag17**2),
      upper_lag17 = (b_m_lag17 - b_f_lag17) + 1.96*sqrt(se_m_lag17**2 + se_f_lag17**2),
      se_m_lag18 = (upper_m_lag18 - lower_m_lag18)/3.92,
      se_f_lag18 = (upper_f_lag18 - lower_f_lag18)/3.92,
      b_lag18 = b_m_lag18 - b_f_lag18,
      lower_lag18 = (b_m_lag18 - b_f_lag18) - 1.96*sqrt(se_m_lag18**2 + se_f_lag18**2),
      upper_lag18 = (b_m_lag18 - b_f_lag18) + 1.96*sqrt(se_m_lag18**2 + se_f_lag18**2),
      se_m_lag19 = (upper_m_lag19 - lower_m_lag19)/3.92,
      se_f_lag19 = (upper_f_lag19 - lower_f_lag19)/3.92,
      b_lag19 = b_m_lag19 - b_f_lag19,
      lower_lag19 = (b_m_lag19 - b_f_lag19) - 1.96*sqrt(se_m_lag19**2 + se_f_lag19**2),
      upper_lag19 = (b_m_lag19 - b_f_lag19) + 1.96*sqrt(se_m_lag19**2 + se_f_lag19**2),
      se_m_lag20 = (upper_m_lag20 - lower_m_lag20)/3.92,
      se_f_lag20 = (upper_f_lag20 - lower_f_lag20)/3.92,
      b_lag20 = b_m_lag20 - b_f_lag20,
      lower_lag20 = (b_m_lag20 - b_f_lag20) - 1.96*sqrt(se_m_lag20**2 + se_f_lag20**2),
      upper_lag20 = (b_m_lag20 - b_f_lag20) + 1.96*sqrt(se_m_lag20**2 + se_f_lag20**2),
      se_m_lag21 = (upper_m_lag21 - lower_m_lag21)/3.92,
      se_f_lag21 = (upper_f_lag21 - lower_f_lag21)/3.92,
      b_lag21 = b_m_lag21 - b_f_lag21,
      lower_lag21 = (b_m_lag21 - b_f_lag21) - 1.96*sqrt(se_m_lag21**2 + se_f_lag21**2),
      upper_lag21 = (b_m_lag21 - b_f_lag21) + 1.96*sqrt(se_m_lag21**2 + se_f_lag21**2),
      se_m_lag22 = (upper_m_lag22 - lower_m_lag22)/3.92,
      se_f_lag22 = (upper_f_lag22 - lower_f_lag22)/3.92,
      b_lag22 = b_m_lag22 - b_f_lag22,
      lower_lag22 = (b_m_lag22 - b_f_lag22) - 1.96*sqrt(se_m_lag22**2 + se_f_lag22**2),
      upper_lag22 = (b_m_lag22 - b_f_lag22) + 1.96*sqrt(se_m_lag22**2 + se_f_lag22**2),
      se_m_lag23 = (upper_m_lag23 - lower_m_lag23)/3.92,
      se_f_lag23 = (upper_f_lag23 - lower_f_lag23)/3.92,
      b_lag23 = b_m_lag23 - b_f_lag23,
      lower_lag23 = (b_m_lag23 - b_f_lag23) - 1.96*sqrt(se_m_lag23**2 + se_f_lag23**2),
      upper_lag23 = (b_m_lag23 - b_f_lag23) + 1.96*sqrt(se_m_lag23**2 + se_f_lag23**2),
      se_m_lag24 = (upper_m_lag24 - lower_m_lag24)/3.92,
      se_f_lag24 = (upper_f_lag24 - lower_f_lag24)/3.92,
      b_lag24 = b_m_lag24 - b_f_lag24,
      lower_lag24 = (b_m_lag24 - b_f_lag24) - 1.96*sqrt(se_m_lag24**2 + se_f_lag24**2),
      upper_lag24 = (b_m_lag24 - b_f_lag24) + 1.96*sqrt(se_m_lag24**2 + se_f_lag24**2),
      se_m_lag25 = (upper_m_lag25 - lower_m_lag25)/3.92,
      se_f_lag25 = (upper_f_lag25 - lower_f_lag25)/3.92,
      b_lag25 = b_m_lag25 - b_f_lag25,
      lower_lag25 = (b_m_lag25 - b_f_lag25) - 1.96*sqrt(se_m_lag25**2 + se_f_lag25**2),
      upper_lag25 = (b_m_lag25 - b_f_lag25) + 1.96*sqrt(se_m_lag25**2 + se_f_lag25**2),
      se_m_lag26 = (upper_m_lag26 - lower_m_lag26)/3.92,
      se_f_lag26 = (upper_f_lag26 - lower_f_lag26)/3.92,
      b_lag26 = b_m_lag26 - b_f_lag26,
      lower_lag26 = (b_m_lag26 - b_f_lag26) - 1.96*sqrt(se_m_lag26**2 + se_f_lag26**2),
      upper_lag26 = (b_m_lag26 - b_f_lag26) + 1.96*sqrt(se_m_lag26**2 + se_f_lag26**2),
      se_m_lag27 = (upper_m_lag27 - lower_m_lag27)/3.92,
      se_f_lag27 = (upper_f_lag27 - lower_f_lag27)/3.92,
      b_lag27 = b_m_lag27 - b_f_lag27,
      lower_lag27 = (b_m_lag27 - b_f_lag27) - 1.96*sqrt(se_m_lag27**2 + se_f_lag27**2),
      upper_lag27 = (b_m_lag27 - b_f_lag27) + 1.96*sqrt(se_m_lag27**2 + se_f_lag27**2),
      se_m_lag28 = (upper_m_lag28 - lower_m_lag28)/3.92,
      se_f_lag28 = (upper_f_lag28 - lower_f_lag28)/3.92,
      b_lag28 = b_m_lag28 - b_f_lag28,
      lower_lag28 = (b_m_lag28 - b_f_lag28) - 1.96*sqrt(se_m_lag28**2 + se_f_lag28**2),
      upper_lag28 = (b_m_lag28 - b_f_lag28) + 1.96*sqrt(se_m_lag28**2 + se_f_lag28**2))
  
  return(data_out)
  
}
