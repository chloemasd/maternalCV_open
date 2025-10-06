# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Data preparation for maternal cardiovascular health analysis,
# This script includes:
# - Ambient temperature exposures and other meteorological-related covariates
#     including NDVI, EDI, relative humidity and air pollution data preparation
# - Nested data frame management 

# 1. Adding exposure variables ----

#' Adding Normalized Difference Vegetation Index (NDVI) Exposure
#'
#' This function takes clean data frame in the long format and add to each
#' measurement corresponding NDVI exposures. 
#' In case of missing data for NDVI, we impute to the last known value of NDVI 
#' for the mother. 
#'
#' @param data_long A data frame containing cleaned data 
#' @param data_ndvi A data frame containing NDVI data
#' @return A data frame of maternal cardiovascular function measurements with corresponding 
#'  NDVI exposures 
prep_add_ndvi <- function(data_long, data_ndvi){
  ## hidden
}

#' Adding Exposure Variables
#'
#' This function takes raw clean data frame and add to each measurement 
#' preceding 4-week exposures for temperature exposure (Tmean, Tmax, Tmin and Tvar),
#' air pollution exposure (PM2.5, PM10, NO2 and O3), relative humidity and European
#' Deprivation Index (EDI)
#' 
#' @param data_long A data frame containing cleaned data
#' @param data_date A data frame containing dates for exposure data
#' @param data_ndvi A data frame containing NDVI exposure
#' @param data_edi A data frame containing EDI exposure
#' @param data_rhum A data frame containing relative humidity exposure
#' @param data_tmin A data frame containing Tmin exposure
#' @param data_tmax A data frame containing Tmax exposure
#' @param data_tmean A data frame containing Tmean exposure
#' @param data_pm10 A data frame containing PM10 exposure
#' @param data_pm25 A data frame containing PM2.5 exposure
#' @param data_no2 A data frame containing NO2 exposure
#' @param bp A boolean indicating whether outcome of interest is blood pressure
#'  Default to FALSE
#' @return A data frame of maternal cardiovascular measurements, and corresponding exposures
#' up to 4-weeks before measurement (one data frame for each exposure)
prep_add_expo <- function(data_long, data_date, data_ndvi, data_edi, data_rhum, data_tmin, data_tmax, data_tmean, data_pm10, data_pm25, data_no2, data_o3, bp = FALSE){
  
  # Step 1 - Starting dates of exposure
  # Identify starting dates in the data_date table based on dates of maternal cardiovascular measurements
  # data_long has as many rows as measurements
  ## hidden
  
  # Step 2 - Add NDVI 
  # Add the corresponding NDVI values for each measurement
  data_out <- prep_add_ndvi(data_long = data_long, data_ndvi = data_ndvi) 
  
  # Step 3 - Compute exposures 
  # Based on starting dates we compute the exposures
  ## hidden
  
  # Step 4 - Co-exposure data imputation
  # If any missing data in the co-exposure (air pollution, RH, EDI, NDVI) --> imputation
  # Checks:
  # No missing data for: air pollution (PM2.5, PM10, NO2, O3), EDI or RH
  # Some NAs for NDVI (3%) but mainly for participants with an history of NDVI
  data_imputed = prep_imputation_long(data_in = data_out, 
           columns = c("buffer100m_ndvi_ete_gped","edi_mean_4","rhum_mean_4", 
                       "pm10_mean_4","pm25_mean_4","no2_mean_4","o3_mean_4"))

  # Compute final data set
  # Excluding women with missing exposure data
  ## hidden
  
  # Step 7 - Need new start_col because of missing exposure filtering
  # Compute exposure data sets for participants included in the analysis
  data_out_tmin = help_get_expo_day(data_long = data_out, data_expo = data_tmin, col_start = col_start_final, bp=bp)
  data_out_tmax =  help_get_expo_day(data_long = data_out, data_expo = data_tmax, col_start = col_start_final, bp=bp)
  data_out_tmean = help_get_expo_day(data_long = data_out, data_expo = data_tmean, col_start = col_start_final, bp=bp)
  data_out_tvar = help_get_tvar(data_tmax = data_out_tmax, data_tmin = data_out_tmin) # Adding Tvar exposure
  data_out_rhum = help_get_expo_day(data_long = data_out, data_expo = data_rhum, col_start = col_start_final, bp=bp)
  data_out_pm25 = help_get_expo_day(data_long = data_out, data_expo = data_pm25, col_start = col_start_final, bp=bp)
  data_out_pm10 = help_get_expo_day(data_long = data_out, data_expo = data_pm10, col_start = col_start_final, bp=bp)
  data_out_no2 = help_get_expo_day(data_long = data_out, data_expo = data_no2, col_start = col_start_final, bp=bp)
  data_out_o3 = help_get_expo_day(data_long = data_out, data_expo = data_o3, col_start = col_start_final, bp=bp)
  
  # Results: a list of data frames 
  list_res = list(
    "final" = data_out,   # Outcomes and covariates
    "tmin" = data_out_tmin,   # Tmin exposure
    "tmax" = data_out_tmax,   # Tmax exposure
    "tmean" = data_out_tmean, # Tmean exposure
    "tvar" = data_out_tvar,   # Tvar exposure
    "rhum" = data_out_rhum,   # Relative humidity exposure
    "pm25" = data_out_pm25,   # PM2.5 exposure
    "pm10" = data_out_pm10,   # PM10 exposure
    "no2" = data_out_no2,     # NO2 exposure
    "o3" = data_out_o3        # O3 exposure
  )
  return(list_res)
}


# 2. Missing data imputation ----

#' Missing data imputation for time-varying covariates
#' 
#' This function performs imputation of time-varying covariates, especially
#' exposure-related covariates (NDVI, EDI). Missing data are imputed to the last
#' known value otherwise to the median.
#' 
#' @param data_in A data frame containing the data on which imputation has 
#'  to be performed
#' @param columns A string indicating the variable to be imputed
#' @return A data frame containing imputed data for exposure-related covariates
prep_imputation_long <- function(data_in, columns){
  
  data_out <- data_in 
  
  for (x in seq_along(columns)){
  
    for (i in 1:nrow(data_in)){ # from 1 to nrow(data_in)
      if(is.na(data_in[i,columns[x]])){ # Missing value
        
        res = help_get_last(data_in = data_in, ident = data_in$id[i], 
                            num = data_in$number[i], var = columns[x]) # Get the last known value
        data_out[i,columns[x]] <- res # Replace by the imputed values, otherwise kept as NA
      }
    } # end from 1 to nrow(data_in)
  }
  
  # All values that could not be imputed are replaced by the median
  data_out <- data_out |>
    dplyr::mutate(
      across(all_of(columns), 
             ~replace_na(., median(., na.rm=TRUE))))
  
}

#' Complete data set based on previously imputed data
#' 
#' This function performs imputation of missing data based on another data set
#' on which imputation was already performed. This function matches missing data
#' already imputed to same missing data still missing in complete the data set. 
#' 
#' @param data_imp A data frame containing imputed data
#' @param data A data frame containing the data on which imputation has 
#'  to be performed
#' @return A list of data frames containing completed data and exposure data
prep_complete <- function(data_imp, data){
  
  imputed_cov = unique(data_imp$imputation$files) # Covariates that were already imputed
  id_imp = data_imp$final$id # Ids of mothers included in the previous imputation process
  data_help = data$final # Already-imputed data set
  
  for (v in seq_along(imputed_cov)){ # run over imputed covariates
    
    for (i in 1:nrow(data$final)){ # run over participants
      
      if(is.na(data_help[i,imputed_cov[[v]]]) & data_help$id[i]%in%id_imp){ # Missing data found
        
        data_imp_help <- data_imp$final |>
          dplyr::filter(id==data_help$id[i]) |> # Select the mother
          dplyr::select(imputed_cov[[v]]) # Select the covariates to be imputed
        
        data_help[i,imputed_cov[[v]]] <- data_imp_help[1,1] # Impute the missing data
        
      } # end
    } # end nrow(data)
  } # end seq_along(imputed_cov)
  
  # Need to pass missing data imputation to time-dependent variables
  ## hidden

  list_out <- list(
    "final" = data_out, # Complete data set
    "tmin" = data$tmin,
    "tmax" = data$tmax,
    "tmean" = data$tmean,
    "tvar" = data$tvar,
    "rhum" = data$rhum,
    "pm25" = data$pm25,
    "pm10" = data$pm10,
    "no2" = data$no2,
    "o3" = data$o3)
  
  return(list_out)
  
}

# 3. Nested data frames ----

#' Create nested data frame ready for analyses - repeated measurements
#'
#' This function takes clean data frame and nest the data upon: the outcome (SBP,
#' DBP), the susceptibility windows (W0,W1,W2,W3,W4 or W5), the exposure
#' (tmean, tmin, tmax, tvar), heat/cold definition (percentiles), dfvar (from 2
#' to 3) and dflag (from 2 to 5)
#' 
#' @param data_long A data frame containing un-nested clean data
#' @return A nested data frame ready for statistical models
prep_group <- function(data_long){
  
  # Nest all data sets so that they can be added to the final nested data frame
  data_tmin_unnest <- help_data_nest_bp(data_in = data_long$tmin, col_name = "tmin", cohort=cohort) # Tmin exposure data set
  data_tmax_unnest <- help_data_nest_bp(data_in = data_long$tmax, col_name = "tmax", cohort=cohort) # Tmax exposure data set
  data_tmean_unnest <- help_data_nest_bp(data_in = data_long$tmean, col_name = "tmean", cohort=cohort) # Tmean exposure data set
  data_tvar_unnest <- help_data_nest_bp(data_in = data_long$tvar, col_name = "tvar", cohort=cohort) # Tvar exposure data set
  data_rhum_unnest <- help_data_nest_bp(data_in = data_long$rhum, col_name = "data_rhum", cohort=cohort) # RH exposure data set
  data_pm25_unnest <- help_data_nest_bp(data_in = data_long$pm25, col_name = "data_pm25", cohort=cohort) # PM2.5 exposure data set
  data_pm10_unnest <- help_data_nest_bp(data_in = data_long$pm10, col_name = "data_pm10", cohort=cohort) # PM10 exposure data set
  data_no2_unnest <- help_data_nest_bp(data_in = data_long$no2, col_name = "data_no2", cohort=cohort) # NO2 exposure data set
  data_o3_unnest <- help_data_nest_bp(data_in = data_long$o3, col_name = "data_o3", cohort=cohort) # O3 exposure data set
  data_cov_unnest <- help_data_nest_bp(data_in = data_long$final, col_name = "data", cohort=cohort) # Covariates and outcome data set
  # Tables join
  df_list <- c(data_tmin_unnest, data_tmax_unnest, data_tmean_unnest, data_tvar_unnest,
               data_rhum_unnest, data_pm10_unnest, data_pm25_unnest, data_no2_unnest,
               data_o3_unnest, data_cov_unnest)
  data_nest <- reduce(df_list, full_join, by=c("window","outcome")) # Final nested data frame
  
  # Refine the nested data frame
  data_out <- data_nest |>
    tidyr::pivot_longer(
      cols = c("tmin","tmax","tmean","tvar"), # Duplicate rows based on exposure (tmean, tmax, tmin)
      names_to = "name_expo",
      values_to = "data_expo"
    ) |>
    dplyr::mutate(
      lag = as.integer(28),      # Define the lag (28 days)
      p_hot_99 = as.integer(99), # Define heat exposure threshold: 99th perc.
      p_hot_97 = as.integer(97), # Define heat exposure threshold: 97th perc.
      p_hot_95 = as.integer(95), # Define heat exposure threshold: 95th perc.
      p_hot_90 = as.integer(90), # Define heat exposure threshold: 90th perc.
      dfvar2 = as.integer(2),    # Define df for dose-response relationship: 2
      dfvar3 = as.integer(3),    # Define df for dose-response relationship: 3
      dflag2 = as.integer(2),    # Define df for lag-response relationship: 2 
      dflag3 = as.integer(3),    # Define df for lag-response relationship: 3
      dflag4 = as.integer(4),    # Define df for lag-response relationship: 4
      dflag5 = as.integer(5)     # Define df for lag-response relationship: 5
    ) |>
    tidyr::pivot_longer(
      cols = c("p_hot_99","p_hot_97","p_hot_95","p_hot_90"), # Duplicate rows based on heat exposure (percentile values)
      names_to = "p_hot_val",
      values_to = "p_hot"
    ) |>
    tidyr::pivot_longer(
      cols = c("dfvar2","dfvar3"), # Duplicate rows based on degrees of freedom for dose-response relationship
      names_to = "dfvar_val",
      values_to = "dfvar"
    ) |>
    tidyr::pivot_longer(
      cols = c("dflag2","dflag3","dflag4","dflag5"), # Duplicate rows based on degrees of freedom for lag-response relationship
      names_to = "dflag_val",
      values_to = "dflag"
    ) |>
    dplyr::mutate(
      
      p_cold = as.integer(100 - p_hot), # Define cold exposure thresholds based on heat exposure threshold
      
      # Define the value of heat exposure (°C) for all women
      p_hot_all = case_when( 
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tmax"  ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=as.numeric(p_hot/100)))
      ),
      p_hot_all = case_when(
        p_hot==99 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[1]])),
        p_hot==97 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[9]])),
        p_hot==95 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[17]])),
        p_hot==90 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[25]]))
      ),
      
      # Define the value of cold exposure (°C) for all women
      p_cold_all = case_when( 
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tmax"  ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=as.numeric(p_cold/100)))
      ),
      p_cold_all = case_when(
        p_cold==1 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[1]])),
        p_cold==3 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[9]])),
        p_cold==5 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[17]])),
        p_cold==10 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[25]]))
      ),
      
      # Define the value of median of exposure reference; °C) for all women
      p50_all = case_when(
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=0.5)),
        name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=0.5)),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=0.5)),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=0.5))
      ),
      
      # Define the value of the 10th percentile of exposure (°C) for all women
      p10_all = case_when( 
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=0.1)),
        name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=0.1)),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=0.1)),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=0.1))
      )) |>
    dplyr::select(-p_hot_val,-dfvar_val,-dflag_val) |> # Remove un-relevant information
    dplyr::mutate(
        
        # Define the value of heat exposure (°C) for women recruited in Poitiers
        p_hot_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100)))
        ),
        p_hot_p = case_when(
          p_hot==99 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[1]])),
          p_hot==97 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[9]])),
          p_hot==95 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[17]])),
          p_hot==90 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[25]]))
        ),
        
        # Define the value of cold exposure (°C) for women recruited in Poitiers
        p_cold_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100)))
        ),
        p_cold_p = case_when(
          p_cold==1 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[1]])),
          p_cold==3 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[9]])),
          p_cold==5 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[17]])),
          p_cold==10 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[25]]))
        ),
        
        # Define the value of heat exposure (°C) for women recruited in Nancy
        p_hot_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100)))
        ),
        p_hot_n = case_when(
          p_hot==99 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[1]])),
          p_hot==97 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[9]])),
          p_hot==95 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[17]])),
          p_hot==90 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[25]]))
        ),
        
        # Define the value of cold exposure (°C) for women recruited in Nancy
        p_cold_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100)))
        ),
        p_cold_n = case_when(
          p_cold==1 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[1]])),
          p_cold==3 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[9]])),
          p_cold==5 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[17]])),
          p_cold==10 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[25]]))
        ),
        
        # Define the value of median of exposure (°C) for women recruited in Poitiers
        p50_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=0.5))),
        
        # Define the value of median of exposure (°C) for women recruited in Nancy
        p50_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=0.5)))
      )
    
    # Compute binary exposure data frame
    # To account for local temperature distribution 
    
    data_expo_yn_hot = lapply( # Heat exposure yes/no
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]] # Exposure df
        col_id = help$id # Get maternal unique id
        col_num = help$number # Get measurement number
        col_idxnb = help$idxnb # Get id x measurement number
        help <- help |>
          dplyr::select(-id,-number,-idxnb) # Remove identifying variables from exposure df
        help |>
          dplyr::mutate(
            center = data_out$data[[x]]$center, # Add recruitment center column to exposure df
            perc = ifelse(center=="Nancy",data_out$p_hot_n[[x]],data_out$p_hot_p[[x]]), # Get the local percentile value for heat
            across(colnames(help),
                   ~ help_yn_hot(., perc))) |> # Compute heat exposure for local percentile value
          dplyr::select(-center, -perc) |>
          dplyr::mutate( # Add identifying variables bakc to exposure df
            id = col_id,
            number = col_num,
            idxnb = col_idxnb
          )
      }
    )
    
    data_expo_yn_cold = lapply( # Cold exposure yes/no 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]] # Exposure df
        col_id = help$id # Get maternal unique id
        col_num = help$number # Get measurement number
        col_idxnb = help$idxnb # Get id x measurement number
        help <- help |>
          dplyr::select(-id,-number,-idxnb) # Remove identifying variables from exposure df
        help |>
          dplyr::mutate(
            center = data_out$data[[x]]$center, # Add recruitment center column to exposure df
            perc = ifelse(center=="Nancy",data_out$p_cold_n[[x]],data_out$p_cold_p[[x]]), # Get the local percentile value for cold
            across(colnames(help),
                   ~ help_yn_cold(., perc))) |> # Compute cold exposure for local percentile value
          dplyr::select(-center, -perc) |>
          dplyr::mutate( # Add identifying variables bakc to exposure df
            id = col_id,
            number = col_num,
            idxnb = col_idxnb
          )
      }
    )
  
  # Add binary exposure variable
  data_out$data_expo_yn_h = data_expo_yn_hot # Add heat exposure df
  data_out$data_expo_yn_c = data_expo_yn_cold # Add cold exposure df
  
  return(data_out)
}

#' Create nested data frame ready for analyses - un-repeated measurements
#'
#' This function takes clean data frame and nest the data upon: the outcome 
#' (Hte, HR or DP), the susceptibility windows (W0,W1,W2,W3,W4 or W5), the exposure
#' (tmean, tmin, tmax, tvar), heat/cold definition (percentiles), dfvar (from 2
#' to 3) and dflag (from 2 to 5)
#' 
#' @param data_in A data frame containing un-nested clean data
#' @param outcome A string indicating which outcome to create the nested data on
#' @return A nested data frame ready for statistical models
prep_group_other <- function(data_in, outcome){
  
  # Introducing DP analyses
  if(outcome=="dp"){
    data_in$final <- data_in$final |>
      dplyr::mutate(
        outcome = as.numeric(outcome*sbp_mean_all_c24) # DP = SBP*HR
      )
  }
  
  # Get residuals
  # Rationale: we did not estimate Z-scores for other outcomes (HR, Hte or DP)
  #  therefore, to correct for gestational age at measurement, we regress the 
  #  outcome values on gestational age at measurement (rather than directly 
  #  adjusting for gestational age at measurement)
  model = lm(outcome ~ cs_ga, data = data_in$final)
  data_w <- data_in$final |>
    dplyr::mutate(outcome_res = as.numeric(model$residuals))
  
  # Nest meteorological-related data frames
  ## Tmin
  data_tmin_unnest <- data_in$tmin |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$tmin))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_tmin_unnest) <- c("window","tmin")
  ## Tmax
  data_tmax_unnest <- data_in$tmax |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$tmax))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_tmax_unnest) <- c("window","tmax")
  ## Tmean
  data_tmean_unnest <- data_in$tmean |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$tmean))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_tmean_unnest) <- c("window","tmean")
  ## Tvar
  data_tvar_unnest <- data_in$tvar |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$tvar))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_tvar_unnest) <- c("window","tvar")
  ## Relative humidity
  data_rhum_unnest <- data_in$rhum |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$rhum))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_rhum_unnest) <- c("window","data_rhum")
  ## PM2.5
  data_pm25_unnest <- data_in$pm25 |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$pm25))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_pm25_unnest) <- c("window","data_pm25")
  ## PM10
  data_pm10_unnest <- data_in$pm10 |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$pm10))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_pm10_unnest) <- c("window","data_pm10")
  ## NO2
  data_no2_unnest <- data_in$no2 |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$no2))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_no2_unnest) <- c("window","data_no2")
  ## O3
  data_o3_unnest <- data_in$o3 |>
    dplyr::select(-id,-number,-idxnb) |>
    dplyr::mutate(window = rep("w",nrow(data_in$o3))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  colnames(data_o3_unnest) <- c("window","data_o3")
  
  # Covariates and outcome data
  data_cov_unnest <- data_w |>
    dplyr::mutate(window = rep("w",nrow(data_w))) |>
    dplyr::group_by(window) |>
    tidyr::nest()
  
  # Join all nested data frames
  df_list <- c(data_tmin_unnest, data_tmax_unnest, data_tmean_unnest, data_rhum_unnest,
               data_pm10_unnest, data_pm25_unnest, data_no2_unnest, data_o3_unnest,
               data_cov_unnest)
  data_nest <- reduce(df_list, full_join, by=c("window")) # Final nested data frame
  
  # Prepare the nested data frame
  data_out <- data_nest |>
    tidyr::pivot_longer(
      cols = c("tmin","tmax","tmean","tvar"), # Duplicate rows based on exposure (tmean, tmax, tmin)
      names_to = "name_expo",
      values_to = "data_expo"
    ) |>
    dplyr::mutate(
      outcome = as.character(outcome),
      lag = as.integer(28),      # Define the lag
      p_hot_99 = as.integer(99), # Define heat exposure threshold: 99th perc.
      p_hot_97 = as.integer(97), # Define heat exposure threshold: 97th perc.
      p_hot_95 = as.integer(95), # Define heat exposure threshold: 95th perc.
      p_hot_90 = as.integer(90), # Define heat exposure threshold: 90th perc.
      dfvar2 = as.integer(2),    # Define df for dose-response relationship: 2
      dfvar3 = as.integer(3),    # Define df for dose-response relationship: 3
      dflag2 = as.integer(2),    # Define df for lag-response relationship: 2 
      dflag3 = as.integer(3),    # Define df for lag-response relationship: 3
      dflag4 = as.integer(4),    # Define df for lag-response relationship: 4
      dflag5 = as.integer(5)     # Define df for lag-response relationship: 5
    ) |>
    tidyr::pivot_longer(
      cols = c("p_hot_99","p_hot_97","p_hot_95","p_hot_90"), # Duplicate rows based on heat exposure (percentile values)
      names_to = "p_hot_val",
      values_to = "p_hot"
    ) |>
    tidyr::pivot_longer(
      cols = c("dfvar2","dfvar3"), # Duplicate rows based on degrees of freedom for dose-response relationship
      names_to = "dfvar_val",
      values_to = "dfvar"
    ) |>
    tidyr::pivot_longer(
      cols = c("dflag2","dflag3","dflag4","dflag5"), # Duplicate rows based on degrees of freedom for lag-response relationship
      names_to = "dflag_val",
      values_to = "dflag"
    ) |>
    
    dplyr::mutate(
      
      p_cold = as.integer(100 - p_hot), # Define cold exposure thresholds based on heat exposure threshold
      
      # Define the value of heat exposure (°C) for all women
      p_hot_all = case_when(
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tmax"  ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=as.numeric(p_hot/100))),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=as.numeric(p_hot/100)))
      ),
      p_hot_all = case_when(
        p_hot==99 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[1]])),
        p_hot==97 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[9]])),
        p_hot==95 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[17]])),
        p_hot==90 ~ purrr::map(p_hot_all, ~ as.numeric(.x[[25]]))
      ),
      
      # Define the value of cold exposure (°C) for all women
      p_cold_all = case_when(
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tmax"  ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=as.numeric(p_cold/100))),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=as.numeric(p_cold/100)))
      ),
      p_cold_all = case_when(
        p_cold==1 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[1]])),
        p_cold==3 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[9]])),
        p_cold==5 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[17]])),
        p_cold==10 ~ purrr::map(p_cold_all, ~ as.numeric(.x[[25]]))
      ),
      
      # Define the value of median of exposure reference; °C) for all women
      p50_all = case_when(
        name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4, probs=0.5)),
        name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4, probs=0.5)),
        name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4, probs=0.5)),
        name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4, probs=0.5))
      )) |>
    dplyr::select(-p_hot_val,-dfvar_val,-dflag_val) |>
    dplyr::mutate(
        p_hot_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_hot/100)))
        ),
        p_hot_p = case_when(
          p_hot==99 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[1]])),
          p_hot==97 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[9]])),
          p_hot==95 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[17]])),
          p_hot==90 ~ purrr::map(p_hot_p, ~ as.numeric(.x[[25]]))
        ),
        p_cold_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=as.numeric(p_cold/100)))
        ),
        p_cold_p = case_when(
          p_cold==1 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[1]])),
          p_cold==3 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[9]])),
          p_cold==5 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[17]])),
          p_cold==10 ~ purrr::map(p_cold_p, ~ as.numeric(.x[[25]]))
        ),
        p_hot_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_hot/100)))
        ),
        p_hot_n = case_when(
          p_hot==99 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[1]])),
          p_hot==97 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[9]])),
          p_hot==95 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[17]])),
          p_hot==90 ~ purrr::map(p_hot_n, ~ as.numeric(.x[[25]]))
        ),
        p_cold_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100))),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=as.numeric(p_cold/100)))
        ),
        p_cold_n = case_when(
          p_cold==1 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[1]])),
          p_cold==3 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[9]])),
          p_cold==5 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[17]])),
          p_cold==10 ~ purrr::map(p_cold_n, ~ as.numeric(.x[[25]]))
        ),
        # Centering values
        p50_p = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Poitiers")], probs=0.5)),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Poitiers")], probs=0.5))),
        p50_n = case_when(
          name_expo == "tmin" ~ purrr::map(data, ~ quantile(.x$tmin_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tmax" ~ purrr::map(data, ~ quantile(.x$tmax_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tmean" ~ purrr::map(data, ~ quantile(.x$tmean_mean_4[which(.x$center=="Nancy")], probs=0.5)),
          name_expo == "tvar" ~ purrr::map(data, ~ quantile(.x$tvar_mean_4[which(.x$center=="Nancy")], probs=0.5)))
      )
    
    # Binary exposure data frame
    data_expo_yn_hot = lapply( # Heat exposure yes/no
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]]
        help |>
          dplyr::mutate(
            center = data_out$data[[x]]$center,
            perc = ifelse(center=="Nancy",data_out$p_hot_n[[x]],data_out$p_hot_p[[x]]),
            across(colnames(help),
                   ~ help_yn_hot(., perc))) |>
          dplyr::select(-center, -perc)
      }
    )
    data_expo_yn_cold = lapply( # Cold exposure yes/no 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]]
        help |>
          dplyr::mutate(
            center = data_out$data[[x]]$center,
            perc = ifelse(center=="Nancy",data_out$p_cold_n[[x]],data_out$p_cold_p[[x]]),
            across(colnames(help),
                   ~ help_yn_cold(., perc))) |>
          dplyr::select(-center, -perc)
      }
    )
    
  data_out$data_expo_yn_h = data_expo_yn_hot # Add heat exposure df
  data_out$data_expo_yn_c = data_expo_yn_cold # Add cold exposure df
  
  return(data_out)
}

#' Balanced susceptibility windows
#' 
#' This function takes a nested data frame and balances each susceptibility 
#' window based on the number of measurements within each window to allow
#' mixed linear models to be run.
#' 
#' @param data_in A nested data frame with unbalanced susceptibility windows
#' @param seed A seed to allow reproducible windows balancing
#' @return A nested data frame with balanced susceptibility windows
prep_balance_window <- function(data_in, seed){
  
  set.seed(seed) # Enables to have reproducible balanced windows
  
  # Balancing susceptibility windows
    
    # W2 & W3 - Excluding the 4th measurements, if any
    # W2 & W3 - Women with 3 measurements randomly enriching women with 2 measurements
    data_w2_sbp <- data_in |> dplyr::filter(window=="w2" & outcome=="sbp_z_adapted")
    data_w2_sbp <- data_w2_sbp$data[[1]]
    data_w2_sbp <- data_w2_sbp |> dplyr::group_by(id) |> dplyr::mutate(nb=n()) |> dplyr::filter(nb<4) |> dplyr::slice_sample(n=2)
    data_w2_dbp <- data_in |> dplyr::filter(window=="w2" & outcome=="dbp_z_adapted")
    data_w2_dbp <- data_w2_dbp$data[[1]]
    data_w2_dbp <- data_w2_dbp |> dplyr::filter(idxnb %in% data_w2_sbp$idxnb) # Keep same measurements as in SBP 
    
    data_w3_sbp <- data_in |> dplyr::filter(window=="w3" & outcome=="sbp_z_adapted")
    data_w3_sbp <- data_w3_sbp$data[[1]]
    data_w3_sbp <- data_w3_sbp |> dplyr::group_by(id) |> dplyr::mutate(nb=n()) |> dplyr::filter(nb<4) |> dplyr::slice_sample(n=2)
    data_w3_dbp <- data_in |> dplyr::filter(window=="w3" & outcome=="dbp_z_adapted")
    data_w3_dbp <- data_w3_dbp$data[[1]]
    data_w3_dbp <- data_w3_dbp |> dplyr::filter(idxnb %in% data_w3_sbp$idxnb) # Keep same measurements as in SBP 
    
    # W4 & W5 - Excluding the 5th measurements, if any
    # W4 & W5 - Women with 4 measurements randomly enriching women with 3 measurements
    data_w4_sbp <- data_in |> dplyr::filter(window=="w4" & outcome=="sbp_z_adapted")
    data_w4_sbp <- data_w4_sbp$data[[1]]
    data_w4_sbp <- data_w4_sbp |> dplyr::group_by(id) |> dplyr::mutate(nb=n()) |> dplyr::filter(nb<5) |> dplyr::slice_sample(n=3)
    data_w4_dbp <- data_in |> dplyr::filter(window=="w4" & outcome=="dbp_z_adapted")
    data_w4_dbp <- data_w4_dbp$data[[1]]
    data_w4_dbp <- data_w4_dbp |> dplyr::filter(idxnb %in% data_w4_sbp$idxnb) # Keep same measurements as in SBP 
    
    data_w5_sbp <- data_in |> dplyr::filter(window=="w5" & outcome=="sbp_z_adapted")
    data_w5_sbp <- data_w5_sbp$data[[1]]
    data_w5_sbp <- data_w5_sbp |> dplyr::group_by(id) |> dplyr::mutate(nb=n()) |> dplyr::filter(nb<5) |> dplyr::slice_sample(n=3)
    data_w5_dbp <- data_in |> dplyr::filter(window=="w5" & outcome=="dbp_z_adapted")
    data_w5_dbp <- data_w5_dbp$data[[1]]
    data_w5_dbp <- data_w5_dbp |> dplyr::filter(idxnb %in% data_w5_sbp$idxnb) # Keep same measurements as in SBP 
    
    data_balanced <- data_in |> # Balancing all data sets of covariates in the nested data frame
      # according to the susceptibility window
      dplyr::mutate(
        data = case_when(
          window=="w2" & outcome=="sbp_z_adapted" ~ purrr::map(data, ~ data_w2_sbp),
          window=="w2" & outcome=="dbp_z_adapted" ~ purrr::map(data, ~ data_w2_dbp),
          window=="w3" & outcome=="sbp_z_adapted" ~ purrr::map(data, ~ data_w3_sbp),
          window=="w3" & outcome=="dbp_z_adapted" ~ purrr::map(data, ~ data_w3_dbp),
          window=="w4" & outcome=="sbp_z_adapted" ~ purrr::map(data, ~ data_w4_sbp),
          window=="w4" & outcome=="dbp_z_adapted" ~ purrr::map(data, ~ data_w4_dbp),
          window=="w5" & outcome=="sbp_z_adapted" ~ purrr::map(data, ~ data_w5_sbp),
          window=="w5" & outcome=="dbp_z_adapted" ~ purrr::map(data, ~ data_w5_dbp),
          TRUE ~ purrr::map(data, ~ .x) # Nothing to do for W0 and W1 which are kept unbalanced
        )
      )
  
  # Compute new exposure data frames
  ## Main exposure
  data_expo_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## Relative humidity exposure
  data_rhum_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_rhum[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## PM2.5 exposure
  data_pm25_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_pm25[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## PM10 exposure
  data_pm10_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_pm10[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## NO2 epxosure
  data_no2_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_no2[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## O3 exposure
  data_o3_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_o3[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## Cold exposure (binary)
  data_expo_yn_c_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo_yn_c[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb)
    }
  )
  ## Heat exposure (binary)
  data_expo_yn_h_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo_yn_h[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb)
    }
  )
  
  # Computes the new nested data frame with balanced susceptibility windows
  data_balanced$data_expo = data_expo_new
  data_balanced$data_rhum = data_rhum_new
  data_balanced$data_pm25 = data_pm25_new
  data_balanced$data_pm10 = data_pm10_new
  data_balanced$data_no2 = data_no2_new
  data_balanced$data_o3 = data_o3_new
  data_balanced$data_expo_yn_h = data_expo_yn_h_new
  data_balanced$data_expo_yn_c = data_expo_yn_c_new
  
  return(data_balanced)
}

#' Balance the first susceptibility window
#' 
#' This function takes a nested data frame and balances the first susceptibility 
#' window band keep only one measurement per mother. Only first window is returned
#' 
#' @param data_in A data frame with unbalanced first susceptibility window
#' @param seed A seed to allow reproducible windows balancing
#' @return A data frame with balanced first susceptibility window
prep_balance_w1_window <- function(data_in, seed){
  
  set.seed(seed) # Enables to have reproducible balanced grouped windows
  
  data_in <- data_in |> dplyr::filter(window == "w1") # Keep only grouped windows

  # W1 - Women with 2 measurements randomly enriching women with 1 measurement 
  data_w1_sbp <- data_in |> dplyr::filter(outcome=="sbp_z_adapted")
  data_w1_sbp <- data_w1_sbp$data[[1]]
  data_w1_sbp <- data_w1_sbp |> dplyr::group_by(id) |> dplyr::slice_sample(n=1)
  data_w1_dbp <- data_in |> dplyr::filter(outcome=="dbp_z_adapted")
  data_w1_dbp <- data_w1_dbp$data[[1]]
  data_w1_dbp <- data_w1_dbp |> dplyr::filter(idxnb %in% data_w1_sbp$idxnb) # Keep same measurements as in SBP 
    
  data_balanced <- data_in |> # Balancing all data sets of covariates in the nested data frame
    # according to the susceptibility window
    dplyr::mutate(
      data = case_when(
        outcome=="sbp_z_adapted" ~ purrr::map(data, ~ data_w1_sbp),
        outcome=="dbp_z_adapted" ~ purrr::map(data, ~ data_w1_dbp)
      )
    )
  
  # Compute new exposure data frames
  ## Main exposure
  data_expo_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## Relative humidity exposure
  data_rhum_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_rhum[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## PM2.5 exposure
  data_pm25_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_pm25[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## PM10 exposure
  data_pm10_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_pm10[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## NO2 exposure
  data_no2_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_no2[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## O3 exposure
  data_o3_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_o3[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb) 
    }
  )
  ## Cold exposure (binary)
  data_expo_yn_c_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo_yn_c[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb)
    }
  )
  ## Heat exposure (binary)
  data_expo_yn_h_new = lapply( 
    1:nrow(data_balanced),
    function(x) {
      help = data_balanced$data_expo_yn_h[[x]]
      help |>
        dplyr::filter(idxnb %in% data_balanced$data[[x]]$idxnb) |>
        dplyr::select(-id, -number, -idxnb)
    }
  )
  
  # Computes the new nested data frame with balanced susceptibility windows
  data_balanced$data_expo = data_expo_new
  data_balanced$data_expo_yn_h = data_expo_yn_h_new
  data_balanced$data_expo_yn_c = data_expo_yn_c_new
  data_balanced$data_rhum = data_rhum_new
  data_balanced$data_pm25 = data_pm25_new
  data_balanced$data_pm10 = data_pm10_new
  data_balanced$data_no2 = data_no2_new
  data_balanced$data_o3 = data_o3_new
  
  return(data_balanced)
}

#' Keep only some susceptibility windows
#'
#' This function takes nested data frame and select some susceptibility windows.
#' Rationale: some nested data frames become very heavy to be load/read
#' 
#' @param data_nested A nested data frame with all susceptibility windows
#' @param list_win A list of susceptibility windows to select
#' @return A nested data frame with some susceptibility windows
prep_filter <- function(data_nest, list_win){
  
  data_out <- data_nest |>
    dplyr::filter(window %in% list_win)
  
  return(data_out)
}

#' Excluding some participants for sensitivity analyses
#'
#' This function takes nested data frame and removes some participants based on
#' an exclusion criteria to perform sensitivity/stratified analysis
#' 
#' @param data_in A nested data frame with all participants from the main analysis
#' @param var A string indicating on which variable to filtered participants out
#' @param outcome A string indicating the outcome of interest
#' @return A nested data frame ready for sensitivity/stratified analyses
prep_exclude <- function(data_in, var, outcome){
  
  # Check that the input 'var' can be handled by the function
  if(!var %in% c("diag_ghtn_all","diag_htn_precon_all",
                 "diag_other_all2","Male","Female")){
    stop("Unadequate variable")
  }
  
  # Variable of interest for filtering
  # If 'var' refers to child sex (whether Male or Female)
  var_new <- ifelse(var%in%c("Male","Female"),"ch_sex",var)
  
  # Mode of the variable of interest for filtering
  str <- ifelse(var=="diag_other_all2","No medication",
          ifelse(var %in% c("Male","Female"),var,"No hypertension"))
  
  # Covariate and outcome data frame
  data_excl <- data_in$final |>
    dplyr::filter(get(var_new) == str)
  
  # Exposure data frame: Tmin
  data_excl_tmin = data_in$tmin |>
    dplyr::mutate(new_col = data_in$final[[var_new]]) |>
    dplyr::filter(new_col == str) |>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tmax
  data_excl_tmax = data_in$tmax |>
    dplyr::mutate(new_col = data_in$final[[var_new]]) |>
    dplyr::filter(new_col == str)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tmean
  data_excl_tmean = data_in$tmean |>
    dplyr::mutate(new_col = data_in$final[[var_new]]) |>
    dplyr::filter(new_col == str)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tvar
  data_excl_tvar = data_in$tvar |>
    dplyr::mutate(new_col = data_in$final[[var_new]]) |>
    dplyr::filter(new_col == str)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: relative humidity
  data_excl_rhum = data_in$rhum |>
    dplyr::mutate(new_col = data_in$final[[var_new]]) |>
    dplyr::filter(new_col == str)|>
    dplyr::select(-new_col)
  
  res_excl = list(
    "final" = data_excl,                         
    "tmin" = data_excl_tmin,
    "tmax" = data_excl_tmax,
    "tmean" = data_excl_tmean,
    "tvar" = data_excl_tvar,
    "rhum" = data_excl_rhum,
    "pm25" = data_in$pm25,  # Not needed for sex-stratified analyses, not updated
    "pm10" = data_in$pm10,  # Not needed for sex-stratified analyses, not updated
    "no2" = data_in$no2,    # Not needed for sex-stratified analyses, not updated
    "o3" = data_in$o3       # Not needed for sex-stratified analyses, not updated
  )
  
  if(outcome=="bp"){ # For BP models
    # Needs to reformat the nested data frame
    data_out <- prep_group(res_excl)
    
    data_expo_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]]
        help |>
          dplyr::select(-id, -number, -idxnb) 
      }
    )
    data_rhum_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_rhum[[x]]
        help |>
          dplyr::select(-id, -number, -idxnb) 
      }
    )
    data_expo_yn_c_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo_yn_c[[x]]
        help |>
          dplyr::select(-id, -number, -idxnb)
      }
    )
    data_expo_yn_h_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo_yn_h[[x]]
        help |>
          dplyr::select(-id, -number, -idxnb)
      }
    )

    data_out$data_expo = data_expo_new
    data_out$data_expo_yn_h = data_expo_yn_h_new
    data_out$data_expo_yn_c = data_expo_yn_c_new
    data_out$data_rhum = data_rhum_new
  } # end outcome=="bp"
  
  if(outcome!="bp"){ # For other models
    data_out <- prep_group_other(res_excl, outcome=outcome)}
  
  data_out = data_out |>
    dplyr::filter(window=="w",dfvar == 2,dflag == 3,p_hot == 95) |> # Keep only main results
    dplyr::select(-data_pm25, -data_pm10, -data_no2, -data_o3) # Air pollution exposure data frame not needed anymore
  
  return(data_out)
}

#' Prepare data set for sensitivity analysis adjusting for other cardiovascular indicators
#'
#' This function takes nested data frame and adapt the covariates data so that 
#' it includes other cardiovascular indicators (HR and Hte) to further adjust the model for
#' 
#' @param data_in A nested data frame with all participants from the main analysis
#' @param var A string indicating on which variable to filtered participants out
#' @return A nested data frame ready for sensitivity analyses
prep_Hadj <- function(data_in, var){
  
  # Gestational age at measurement of the other cardiovascular indicator
  var_ga = ifelse(var=="hr_mean_all_c24","diff_c24_date","diff_nfs_date")
  
  # Filtering participants with missing data on the other CVD indicator
  data_out <- data_in$final[which(!is.na(data_in$final[[var]])),] 
  
  # Get residuals
  model_h = lm(get(var) ~ get(var_ga), data = data_out)
  data_out <- data_out |>
    dplyr::mutate(outcome_res = model_h$residuals)
  
  # Compute new data sets
  data_Hadj_tmin = data_in$tmin |>
    dplyr::mutate(var=data_in$final[[var]]) |>
    dplyr::filter(!is.na(var)) |>
    dplyr::select(-var)
  data_Hadj_tmax = data_in$tmax |>
    dplyr::mutate(var=data_in$final[[var]]) |>
    dplyr::filter(!is.na(var))|>
    dplyr::select(-var)
  data_Hadj_tmean = data_in$tmean |>
    dplyr::mutate(var=data_in$final[[var]]) |>
    dplyr::filter(!is.na(var))|>
    dplyr::select(-var)
  data_Hadj_tvar = data_in$tvar |>
    dplyr::mutate(var=data_in$final[[var]]) |>
    dplyr::filter(!is.na(var))|>
    dplyr::select(-var)
  data_Hadj_rhum = data_in$rhum |>
    dplyr::mutate(var=data_in$final[[var]]) |>
    dplyr::filter(!is.na(var))|>
    dplyr::select(-var)
  
  res_Hadj = list(
    "final" = data_out,                         
    "tmin" = data_Hadj_tmin,
    "tmax" = data_Hadj_tmax,
    "tmean" = data_Hadj_tmean,
    "tvar" = data_Hadj_tvar,
    "rhum" = data_Hadj_rhum,
    "pm25" = data_in$pm25, # Not needed for sensitivity analyses, not updated
    "pm10" = data_in$pm10, # Not needed for sensitivity analyses, not updated
    "no2" = data_in$no2,   # Not needed for sensitivity analyses, not updated
    "o3" = data_in$o3      # Not needed for sensitivity analyses, not updated
  )
  
  # Needs to reformat the nested data frame
  data_out = prep_group(res_Hadj)
  
  data_expo_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_rhum_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_rhum[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_expo_yn_c_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo_yn_c[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_expo_yn_h_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo_yn_h[[x]]
      help |> dplyr::select(-id, -number, -idxnb)})
  
  data_out$data_expo = data_expo_new
  data_out$data_expo_yn_h = data_expo_yn_h_new
  data_out$data_expo_yn_c = data_expo_yn_c_new
  data_out$data_rhum = data_rhum_new
  
  data_out <- data_out |>
    dplyr::filter(window=="w",dfvar == 2,dflag == 3 ) |> # Keep only main results
    dplyr::select(-data_pm10, -data_pm25, -data_no2, -data_o3)
  
  return(data_out)
}

#' Excluding some participants for sensitivity analyses
#'
#' This function takes nested data frame and removes some participants based on
#' their air pollution exposure to perform stratified analysis on air pollution
#' exposure levels
#' 
#' @param data_in A nested data frame with all participants from the main analysis
#' @param airpol A string indicating on which air pollutant to stratified data on
#' @param level An integer indicating the exposure level to stratify on
#' @return A nested data frame with stratified data sets on air pollution level
prep_airpol_strat <- function(data_in, airpol, level){
  
  # Get averaged air pollution exposure over the study time period
  data_out <- data_in$final |>
    dplyr::mutate(
      across(all_of(c("pm25_mean_4","pm10_mean_4","no2_mean_4","o3_mean_4")),
            ~ ntile(.,3),
            .names = "{.col}_tert"))
  help_col <- "tert" 
  airpol_col <- paste0(airpol,"_mean_4_",help_col) # Get the desired air pollution exposure
  
  data_airpol <- data_out[which(data_out[[airpol_col]]==level),]
  
  data_airpol_tmin = data_in$tmin |>
    dplyr::mutate(airpol_tert=data_out[[airpol_col]]) |>
    dplyr::filter(airpol_tert == level) |>
    dplyr::select(-airpol_tert)
  data_airpol_tmax = data_in$tmax |>
    dplyr::mutate(airpol_tert=data_out[[airpol_col]]) |>
    dplyr::filter(airpol_tert == level)|>
    dplyr::select(-airpol_tert)
  data_airpol_tmean = data_in$tmean |>
    dplyr::mutate(airpol_tert=data_out[[airpol_col]]) |>
    dplyr::filter(airpol_tert == level)|>
    dplyr::select(-airpol_tert)
  data_airpol_tvar = data_in$tvar |>
    dplyr::mutate(airpol_tert=data_out[[airpol_col]]) |>
    dplyr::filter(airpol_tert == level)|>
    dplyr::select(-airpol_tert)
  data_airpol_rhum = data_in$rhum |>
    dplyr::mutate(airpol_tert=data_out[[airpol_col]]) |>
    dplyr::filter(airpol_tert == level)|>
    dplyr::select(-airpol_tert)
  
  res_airpol = list(
    "final" = data_airpol,                         
    "tmin" = data_airpol_tmin,
    "tmax" = data_airpol_tmax,
    "tmean" = data_airpol_tmean,
    "tvar" = data_airpol_tvar,
    "rhum" = data_airpol_rhum,
    "pm25" = data_in$pm25, # Not needed for stratified analyses
    "pm10" = data_in$pm10, # Not needed for stratified analyses
    "no2" = data_in$no2,   # Not needed for stratified analyses
    "o3" = data_in$o3      # Not needed for stratified analyses
  )
  
  data_out <- prep_group(res_airpol)
  data_expo_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_rhum_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_rhum[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_expo_yn_c_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo_yn_c[[x]]
      help |> dplyr::select(-id, -number, -idxnb) })
  data_expo_yn_h_new = lapply( 
    1:nrow(data_out),
    function(x) {
      help = data_out$data_expo_yn_h[[x]]
      help |> dplyr::select(-id, -number, -idxnb)})
  
  data_out$data_expo = data_expo_new
  data_out$data_expo_yn_h = data_expo_yn_h_new
  data_out$data_expo_yn_c = data_expo_yn_c_new
  data_out$data_rhum = data_rhum_new
  
  data_out = data_out |>
    dplyr::filter(window=="w",dfvar == 2,dflag == 3,p_hot == 95) |> # Keep only main results
    dplyr::select(-data_pm25, -data_pm10, -data_no2, -data_o3)
  
  return(data_out)
}

#' Excluding some participants for sensitivity analyses
#'
#' This function takes nested data frame and removes some participants based on
#' their NDVI exposure to perform stratified analysis on NDVI
#' exposure levels
#' 
#' @param data_in A nested data frame with all participants from the main analysis
#' @param level An integer indicating the exposure level to stratify on
#'  Default to FALSE
#' @return A nested data frame with stratified data sets on NDVI level
prep_ndvi_strat <- function(data_in, level){

  data_out <- data_in$final |>
    dplyr::mutate( # Create NDVI variable
      buffer100m_ndvi_ete_gped_tert = case_when(
        buffer100m_ndvi_ete_gped_tert=="Low" ~ 1,
        buffer100m_ndvi_ete_gped_tert=="Medium" ~ 2,
        buffer100m_ndvi_ete_gped_tert=="High" ~ 3))
  
  # Based on median or tertile levels
  var_ndvi <- "buffer100m_ndvi_ete_gped_tert"
  
  # Covariate and outcome data frame
  data_excl <- data_out |>
    dplyr::filter(get(var_ndvi) == level)
  
  # Exposure data frame: Tmin
  data_excl_tmin = data_in$tmin |>
    dplyr::mutate(new_col = data_out[[var_ndvi]]) |>
    dplyr::filter(new_col == level) |>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tmax
  data_excl_tmax = data_in$tmax |>
    dplyr::mutate(new_col = data_out[[var_ndvi]]) |>
    dplyr::filter(new_col == level)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tmean
  data_excl_tmean = data_in$tmean |>
    dplyr::mutate(new_col = data_out[[var_ndvi]]) |>
    dplyr::filter(new_col == level)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: Tvar
  data_excl_tvar = data_in$tvar |>
    dplyr::mutate(new_col = data_out[[var_ndvi]]) |>
    dplyr::filter(new_col == level)|>
    dplyr::select(-new_col)
  
  # Exposure data frame: relative humidity
  data_excl_rhum = data_in$rhum |>
    dplyr::mutate(new_col = data_out[[var_ndvi]]) |>
    dplyr::filter(new_col == level)|>
    dplyr::select(-new_col)
  
  res_excl = list(
    "final" = data_excl,                         
    "tmin" = data_excl_tmin,
    "tmax" = data_excl_tmax,
    "tmean" = data_excl_tmean,
    "tvar" = data_excl_tvar,
    "rhum" = data_excl_rhum,
    "pm25" = data_in$pm25,  # Not needed for NDVI-stratified analyses, not updated
    "pm10" = data_in$pm10,  # Not needed for NDVI-stratified analyses, not updated
    "no2" = data_in$no2,    # Not needed for NDVI-stratified analyses, not updated
    "o3" = data_in$o3       # Not needed for NDVI-stratified analyses, not updated
  )
  
  # For BP models
  # Needs to reformat the nested data frame
  data_out <- prep_group(res_excl)
    
    data_expo_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo[[x]]
        help |> dplyr::select(-id, -number, -idxnb) })
    data_rhum_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_rhum[[x]]
        help |> dplyr::select(-id, -number, -idxnb) })
    data_expo_yn_c_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo_yn_c[[x]]
        help |> dplyr::select(-id, -number, -idxnb) })
    data_expo_yn_h_new = lapply( 
      1:nrow(data_out),
      function(x) {
        help = data_out$data_expo_yn_h[[x]]
        help |> dplyr::select(-id, -number, -idxnb) })
    
    data_out$data_expo = data_expo_new
    data_out$data_expo_yn_h = data_expo_yn_h_new
    data_out$data_expo_yn_c = data_expo_yn_c_new
    data_out$data_rhum = data_rhum_new
  
  data_out = data_out |>
    dplyr::filter(window=="w",dfvar == 2,dflag == 3,p_hot == 95) |> # Keep only main results
    dplyr::select(-data_pm25, -data_pm10, -data_no2, -data_o3) # Air pollution exposure data frame not needed anymore
  
  return(data_out)
}