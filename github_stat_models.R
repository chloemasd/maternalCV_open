# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Functions for statistical models ----

## Compute the AIC ----

mod_select_dlnm <- function(data_in, data_expo, data_rhum, list_cov, window, random.effect = TRUE){
  
  # Compute exposure matrices
  expo = rev(data_expo) # Ambient temperature exposure
  rhum = rev(data_rhum) # Relative humidity exposure
  
  # Compute the outcome of interest 
  out = ifelse(!"outcome"%in%colnames(data_in),
               "value_bp_z_adapted",            # Outcome: blood pressure (sbp or dbp), repeated measurements
               "outcome_res")                   # Outcome: heart rate or hematocrit
  
  # Compute the covariates based on susceptibility window
  if(window%in%c("w1","w2","w3")){
    list_cov <- list_cov[!list_cov%in%c("mo_med_ghtn3")]
  }
  
  # Modelling strategy: mixed linear models
  form1 = " ~ cb_temp + cb_rhum + " # Cross-basis for ambient heat and relative humidity
  form = ifelse(random.effect,
                paste0(out,form1," (1|id) + "), # add random effects on participants
                paste0(out,form1))              # fixed effects only
  
  # Pairs of df to be tested (from 2 to 6)
  loop = expand.grid(i = 2:6, j = 2:6)
  
  res_selection = apply(loop, 1, function(x) {
    
    ## Cross-basis for ambient heat exposure
    cb_temp = dlnm::crossbasis(
      x = expo, 
      lag = 28, 
      argvar = list(fun = "ns", df = x[["i"]]), 
      arglag = list(fun = "ns", df = x[["j"]])
    )
    
    ## Cross-basis for relative humidity exposure
    cb_rhum = dlnm::crossbasis(
      x = rhum,
      lag = 28,
      argvar = list(fun = "ns", df = x[["i"]]),
      arglag = list(fun = "ns", df = x[["j"]])
    )
    
    ## Model
    if(random.effect){
      model = lme4::lmer(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Mixed linear model
    }
    if(!random.effect){
      model = lm(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Linear model
    }
    
    ## Compute AIC 
    res_aic = AIC(model)
  }
  )
  
  data_out = data.frame(loop, res_selection)
  colnames(data_out) <- c("df_argvar", "df_arglag", "exposure")
  
  return(data_out)
}

## DLNM ----

mod_dlnm <- function(data_in, data_expo, data_rhum, df_var, df_lag, cen, list_cov, window, random.effect = TRUE){
  
  # Compute exposure matrices
  expo = rev(data_expo) # Ambient temperature exposure
  rhum = rev(data_rhum) # Relative humidity exposure
  
  # Compute the outcome of interest 
  out = ifelse(!"outcome"%in%colnames(data_in),
               "value_bp_z_adapted",            # Outcome: blood pressure (sbp or dbp)
               "outcome_res")                   # Outcome: heart rate or hematocrit
  
  # Compute the covariates based on susceptibility window
  if(window%in%c("w1","w2","w3")){
    list_cov <- list_cov[!list_cov%in%c("mo_med_ghtn3")]
  }
  
  # Modelling strategy: (mixed) linear models
  form1 = " ~ cb_temp + cb_rhum + " # Cross-basis for ambient heat and relative humidity
  form = ifelse(random.effect,
                paste0(out,form1," (1|id) + "), # add random effects on participants
                paste0(out,form1))              # fixed effects only
  
  ## Cross-basis for ambient heat exposure
  cb_temp = crossbasis(expo, # Cross-basis for the temperature exposure
                       lag = 28,
                       argvar = list(fun = "ns", df = df_var),                  # Natural cubic spline for dose-response function
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline for lag-response function                    
  )
  ## Cross-basis for relative humidity exposure
  cb_rhum = crossbasis(rhum, # Cross-basis for relative humidity
                       lag = 28,
                       argvar = list(fun = "ns", df = df_var),                  # Natural cubic spline
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline
  )
  ## Model
  if(random.effect){
    model = lme4::lmer(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Mixed linear model
  }
  if(!random.effect){
    model = lm(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Linear model
  }
  
  # Prediction using DLNM
  cb_pred = crosspred(cb_temp, 
                      model,
                      cen = cen,
                      by = 0.5, # along the temperature space with an increment of 0.5°C
                      bylag = 1, # along the lag space with an increment of 1 day
                      cumul = TRUE # incremental cumulative associations along lags must be included
  )
  
  list_res = list(
    "cb_temp" = cb_temp,
    "cb_rhum" = cb_rhum,
    "model" = model,
    "prediction" = cb_pred
  )
  return(list_res)
}

mod_dlnm_ap <- function(data_in, data_expo, data_rhum, data_ap, df_var, df_lag, cen, list_cov, window, random.effect = TRUE){
  
  # Compute exposure matrices
  expo = rev(data_expo) # Ambient temperature exposure
  rhum = rev(data_rhum) # Relative humidity exposure
  airpol = rev(data_ap) # Ambient air pollution exposure
  
  # Compute the outcome of interest 
  out = ifelse(!"outcome"%in%colnames(data_in),
               "value_bp_z_adapted",            # Outcome: blood pressure (sbp or dbp)
               "outcome_res")                   # Outcome: heart rate or hematocrit
  
  # Compute the covariates based on susceptibility window
  if(window%in%c("w1","w2","w3")){
    list_cov <- list_cov[!list_cov%in%c("mo_med_ghtn3")]
  }
  
  # Modelling strategy: (mixed) linear models
  form1 = " ~ cb_temp + cb_rhum + cb_ap + " # Cross-basis for ambient heat, relative humidity and air pollution
  form = ifelse(random.effect,
                paste0(out,form1," (1|id) + "), # add random effects on participants
                paste0(out,form1))              # fixed effects only
  
  ## Cross-basis for ambient heat exposure
  cb_temp = crossbasis(expo, # Cross-basis for the exposure
                       lag = 28,
                       argvar = list(fun = "ns", df = df_var),                  # Natural cubic spline for dose-response function
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline for lag-response function
  )
  
  ## Cross-basis for relative humidity exposure
  cb_rhum = crossbasis(rhum, # Cross-basis for relative humidity
                       lag = 28,
                       argvar = list(fun = "ns", df = df_var),                  # Natural cubic spline 
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline
  )
  
  ## Cross-basis for ambient air pollution exposure
  cb_ap = crossbasis(airpol, # Cross-basis for air pollution
                     lag = 28,
                     argvar = list(fun = "ns", df = df_var),                    # Natural cubic spline 
                     arglag = list(fun = "ns", df = df_lag)                     # Natural cubic spline 
  )
  
  ## Model
  if(random.effect){
    model = lme4::lmer(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Mixed linear model
  }
  if(!random.effect){
    model = lm(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Linear model
  }
  
  # Prediction
  cb_pred = crosspred(cb_temp, 
                      model,
                      cen = cen, 
                      by = 0.5, # along the temperature space with an increment of 0.5°C
                      bylag = 1, # along the lag space with an increment of 1
                      cumul = TRUE # incremental cumulative associations along lags must be included
  )
  
  list_res = list(
    "cb_temp" = cb_temp,
    "cb_rhum" = cb_rhum,
    "cb_ap" = cb_ap,
    "model" = model,
    "prediction" = cb_pred
  )
  return(list_res)
}

mod_dlnm_yn <- function(data_in, data_expo_yn, data_rhum, df_var, df_lag, window, n_class, list_cov, random.effect){
  
  # Compute exposure matrices
  expo_yn = rev(data_expo_yn) # Ambient heat or cold temperature exposure
  rhum = rev(data_rhum) # Relative humidity exposure
  
  # Compute the outcome of interest 
  out = ifelse(!"outcome"%in%colnames(data_in),
               "value_bp_z_adapted",            # Outcome: blood pressure (sbp or dbp)
               "outcome_res")                   # Outcome: heart rate or hematocrit
  
  # Compute the covariates based on susceptibility window
  if(window%in%c("w1","w2","w3")){
    list_cov <- list_cov[!list_cov%in%c("mo_med_ghtn3")]
  }
  
  # Modelling strategy: (mixed) linear models
  form1 = " ~ cb_temp + cb_rhum + " 
  form = ifelse(!random.effect,
                paste0(out,form1),              # fixed effects only
                paste0(out,form1," (1|id) + ")) # add random effects on participants)             
  
  # Cross-basis for ambient temperature and relative humidity exposures
  # with random effects for repeated measurements
  
  # Classification of exposure: binary of with 3 classes
  if(n_class==3){
    list_breaks <- c(0.5,1.5)
  }
  if(n_class==2){
    list_breaks <- c(0.5)
  }
  
  ## Cross-basis for ambient heat/cold exposure
  cb_temp = crossbasis(expo_yn, # Cross-basis for the temperature exposure
                       lag = 28,
                       argvar = list(fun = "strata", breaks=list_breaks),       # Binary exposure: yes/no above/below the percentile value            
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline for the lag-response function          
  )
  
  ## Cross-basis for relative humidity exposure
  cb_rhum = crossbasis(rhum, # Cross-basis for relative humidity
                       lag = 28,
                       argvar = list(fun = "ns", df = df_var),                  # Natural cubic spline 
                       arglag = list(fun = "ns", df = df_lag)                   # Natural cubic spline    
  ) 
  
  ## Model
  if(random.effect){
    model = lme4::lmer(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Mixed linear model
  }
  if(!random.effect){
    model = lm(as.formula(paste(form, paste(list_cov, collapse = " + "))), data = data_in) # Linear model
  }
  
  # Prediction
  cb_pred = crosspred(cb_temp, 
                      model,
                      bylag = 1, # along the lag space with an increment of 1
                      cumul = TRUE # incremental cumulative associations along lags must be included
  )
  
  list_res = list(
    "cb_temp" = cb_temp,
    "cb_rhum" = cb_rhum,
    "model" = model,
    "prediction" = cb_pred
  )
  return(list_res)
}

run_model <- function(data_nest, list_cov, random.effect = TRUE){
  
  # These model does not adjust for air pollution exposure
  
  # Compute AIC
  res_select = lapply( # Compute AIC to choose df
    1:nrow(data_nest),
    function(x) {
      mod_select_dlnm(data_in = data_nest$data[[x]], 
                      data_expo = data_nest$data_expo[[x]], 
                      data_rhum = data_nest$data_rhum[[x]],
                      list_cov = list_cov,
                      window = data_nest$window[[x]],
                      random.effect = random.effect)
    }
  )
  
  # DLNM
  ## Continuous exposure
  res_pred = lapply( # Prediction
    1:nrow(data_nest),
    function(x) {
      mod_dlnm(data_in = data_nest$data[[x]],
               data_expo = data_nest$data_expo[[x]],
               data_rhum = data_nest$data_rhum[[x]],
               df_var = data_nest$dfvar[[x]],
               df_lag = data_nest$dflag[[x]],
               cen = data_nest$p50_all[[x]],
               list_cov = list_cov,
               window = data_nest$window[[x]],
               random.effect = random.effect)
    }
  )
  
  data_out = data_nest
  data_out$mod_selection = res_select # AIC
  data_out$mod_prediction = res_pred # DLNM
  
  data_out <- data_out |>
    dplyr::select(
      -starts_with("data")
    )
  
  return(data_out)
}

run_model_ap_spe <- function(data_nest, airpol, list_cov, random.effect = TRUE){
  
  var_data_airpol = paste0("data_",airpol)
  var_model_airpol = paste0("mod_prediction_",airpol)
  
  res_pred_airpol = lapply( 
    1:nrow(data_nest),
    function(x) {
      mod_dlnm_ap(data_in = data_nest$data[[x]],
                  data_expo = data_nest$data_expo[[x]],
                  data_rhum = data_nest$data_rhum[[x]],
                  data_ap = data_nest[[var_data_airpol]][[x]],
                  df_var = data_nest$dfvar[[x]], 
                  df_lag = data_nest$dflag[[x]], 
                  cen = data_nest$p50_all[[x]],
                  list_cov = list_cov, 
                  window = data_nest$window[[x]],
                  random.effect = random.effect)
    }
  )
  
  data_out = data_nest
  data_out <- data_out |>
    dplyr::select(
      -data,-data_expo,-data_expo_yn_c,-data_expo_yn_h,-data_expo_yn_3,
      -data_rhum,-data_pm25,-data_pm10,-data_no2,-data_o3
    )
  
  data_out[[var_model_airpol]] = res_pred_airpol
  
  return(data_out)
}

run_model_binary <- function(data_nest, list_cov, random.effect=TRUE){
  
  # DLNM
  
  ## Binary exposure: heat
  res_pred_hot_2 = lapply( # Prediction, binary heat exposure with df_var=2, df_lag=2, cen=p50
    1:nrow(data_nest),
    function(x) {
      mod_dlnm_yn(data_in = data_nest$data[[x]],
                  data_expo_yn = data_nest$data_expo_yn_h[[x]], # Main exposure: heat
                  data_rhum = data_nest$data_rhum[[x]],
                  df_var = data_nest$dfvar[[x]],
                  df_lag = data_nest$dflag[[x]],
                  window = data_nest$window[[x]],
                  n_class = 2,
                  list_cov = list_cov,
                  random.effect = random.effect)
    }
  )
  
  ## Binary exposure: cold
  res_pred_cold_2 = lapply( # Prediction, binary cold exposure with df_var=2, df_lag=2, cen=p50
    1:nrow(data_nest),
    function(x) {
      mod_dlnm_yn(data_in = data_nest$data[[x]],
                  data_expo_yn = data_nest$data_expo_yn_c[[x]], # Main exposure: cold
                  data_rhum = data_nest$data_rhum[[x]],
                  df_var = data_nest$dfvar[[x]],
                  df_lag = data_nest$dflag[[x]],
                  window = data_nest$window[[x]],
                  n_class = 2,
                  list_cov = list_cov,
                  random.effect = random.effect)
    }
  )
  
  data_out <- data_nest
  # DLNM
  data_out$mod_hot_2 = res_pred_hot_2
  data_out$mod_cold_2 = res_pred_cold_2
  
  data_out <- data_out |>
    dplyr::select(
      -starts_with("data")
    )
  
  return(data_out)
}

## Recalculation of confidence intervals ----

moderation_test <- function(model_m, model_f){
  
  test_mod_all = lapply( 
    1:nrow(model_m),
    function(x) {
      help_test_moderation(data1 = model_m$mod_prediction[[x]]$prediction,
                           data2 = model_f$mod_prediction[[x]]$prediction)
    }
  )
  test_mod_lag = lapply(
    1:nrow(model_m),
    function(x) {
      help_test_moderation_lag(data1 = model_m$mod_prediction[[x]]$prediction,
                               data2 = model_f$mod_prediction[[x]]$prediction)
    }
  )
  
  data_out = model_m
  data_out$moderation_test <- test_mod_all
  data_out$moderation_test_lag <- test_mod_lag
  data_out <- data_out |>
    dplyr::select(-mod_prediction)
  
  return(data_out)
}