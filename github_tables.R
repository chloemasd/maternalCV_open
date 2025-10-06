# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Tables functions ----

#' Generate study population descriptive statistics grouped by a variable
#'
#' @param data_in A data frame containing the study population characteristics
#' @param var A string indicating the variable to group the description on
#' @param repeated A boolean indicating whether the data are repeated measurements
#'  Default to FALSE
#' @return A table of study population descriptive statistics grouped by a variable
make_tbl_desc_var <- function(data_in, var, repeated=FALSE){
  
  if(repeated){ # Need to slice the df if the input includes repeated measurements
    data_in <- data_in |>
      dplyr::group_by(id) |> # Group by maternal ids
      dplyr::slice_head(n=1) # Slice the df to keep only the first row
  }
  
  # All covariates to be included in the description table (time-independent)
  cov = c(
    "center",
    "ch_sex",   
    "mo_age_cat4",     
    "mo_par_cat3",       
    "mo_bmi_bepr_cat3",  
    "mo_dipl",             
    "mo_tabacp_any",           
    "mo_tabac_any",
    "mo_coffee",
    "diag_gdiab",
    "diag_htn_precon_all",
    "diag_hdp_all",
    "diag_other_all2",
    "po_seasone",
    "info_expo_200m",
    "po_gd_m",
    "cs_ga",
    "po_gd_lmp",
    "diag_htn_ga",
    "diag_htn_trait_ga",
    "diag_other_aspi_ga",
    "mo_weight_gain_pr_cont",
    "rhum_mean_4",
    "buffer100m_ndvi_ete_gped",
    "pm25_mean_4",
    "pm10_mean_4",
    "no2_mean_4",
    "o3_mean_4",
    "edi_mean_4"
  )
  cov <- cov[cov %in% colnames(data_in)] # Select available covariates
  
  # Adding air pollution and NDVI strata
  data_tbl <- data_in |>
    dplyr::mutate(
      across(all_of(c("pm25_mean_4","pm10_mean_4","no2_mean_4","o3_mean_4")),
             ~ dplyr::ntile(.,3),
             .names = "{.col}_tert"
      ),
      across(all_of(c("pm25_mean_4","pm10_mean_4","no2_mean_4","o3_mean_4","buffer100m_ndvi_ete_gped")),
             ~ dplyr::ntile(.,2),
             .names = "{.col}_med"
      ))
  
  # Create a descriptive statistics table, grouped by var
  tbl <- tbl_summary(
          data = data_tbl,
          include = all_of(cov), # Include all covariates defined above
          by = !!sym(var), # Group by var
          missing = "ifany"
          ) |>
        gtsummary::add_p() |> # Add p-values
        gtsummary::add_overall() # Add overall column
  
  return(tbl)
}

#' Generate study population descriptive statistics grouped by heat/cold exposure
#'
#' @param data_in A data frame containing the study population characteristics
#' @param p The threshold for heat definition (percentile)
#' @param repeated A boolean indicating whether the data are repeated measurements
#' (default to FALSE)
#' @return A table of study population descriptive statistics grouped by heat/cold exposure
make_tbl_desc_temp <- function(data_in, p, repeated=FALSE){
  
  if(repeated){ # Need to slice the df if the input includes repeated measurements
    data_in <- data_in |>
      dplyr::group_by(id) |> # Group by maternal ids
      dplyr::slice_head(n=1) # Slice the df to keep only the first row
  }
  
  # All covariates to be included in the description table (time-independent)
  cov = c(
    "center",
    "ch_sex",   
    "mo_age_cat4",     
    "mo_par_cat3",       
    "mo_bmi_bepr_cat3",  
    "mo_dipl",             
    "mo_tabacp_any",           
    "mo_tabac_any",
    "mo_coffee",
    "diag_gdiab",
    "diag_htn_precon_all",
    "diag_hdp_all",
    "diag_other_all2",
    "info_expo_200m",
    "po_gd_m",
    "diag_htn_ga",
    "diag_htn_trait_ga",
    "diag_other_aspi_ga"
  )
  cov <- cov[cov %in% colnames(data_in)] # Select available covariates
  
  data_p <- data_in |> # Df with mothers in Poitiers study area
    dplyr::filter(
      center=="Poitiers"
    )
  q <- 100 - p # Percentile for cold exposure
  p_cold_p <- quantile(data_p$tmean_mean_4, probs=q/100) # Cold, Poitiers
    
  data_n <- data_in |> # Df with mothers in Nancy study area
    dplyr::filter(
      center=="Nancy"
    )
  p_hot_n <- quantile(data_n$tmean_mean_4, probs=p/100) # Heat, Nancy
    
  data_in <- data_in |>
    dplyr::mutate(
      tmean_cat = case_when(
        tmean_mean_4 >= p_hot_n ~ "Heat exposure",
        tmean_mean_4 <= p_cold_p ~ "Cold exposure",
        TRUE ~ "Reference"
      )
    )
  
  data_tbl = data_in
  # Create a descriptive statistics table for maternal covariates, grouped
  # by tmean_cat
  
  tbl <- tbl_summary(
    data = data_tbl,
    include = all_of(cov), # Include all covariates defined above
    by = tmean_cat, # Group by heat/cold exposure
    missing = "no"
  ) |>
    gtsummary::add_p() |> # Add p-values
    gtsummary::add_overall() # Add overall column
  
  return(tbl)
}

make_tbl_aic <- function(model, out, binary = FALSE){
  
  select_type = "mod_selection"
  select_type = ifelse(binary,paste0(select_type,"_yn"),select_type)
  
  model <- dplyr::arrange(model, window) # Sort by windows
  
  list_win = unique(model$window)

  if(length(list_win)==1){
    
    model_tmean_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmean",window == list_win[1])
    model_tmax_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmax",window == list_win[1])
    model_tmin_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmin",window == list_win[1])
    model_tvar_aic <- model |> dplyr::filter(outcome == out,name_expo == "tvar",window == list_win[1])
    
    aic_tmean <- model_tmean_aic[[select_type]]
    aic_tmax <- model_tmax_aic[[select_type]]
    aic_tmin <- model_tmin_aic[[select_type]]
    aic_tvar <- model_tvar_aic[[select_type]]
    
    data_aic <- data.frame(aic_tmean[[1]],
                           aic_tmax[[1]]$exposure,
                           aic_tmin[[1]]$exposure,
                           aic_tvar[[1]]$exposure)
    colnames(data_aic) <- c("df_var","df_lag",
                            paste0("Tmean.",list_win[1]),paste0("Tmax.",list_win[1]),
                            paste0("Tmin.",list_win[1]),paste0("Tvar.",list_win[1]))
    
    res_aic <- knitr::kable(data_aic) %>%
      kableExtra::column_spec(3, background = spec_color(data_aic[,3], option = "D", direction = -1)) %>%
      kableExtra::column_spec(4, background = spec_color(data_aic[,4], option = "D", direction = -1)) %>%
      kableExtra::column_spec(5, background = spec_color(data_aic[,5], option = "D", direction = -1)) %>%
      kableExtra::column_spec(6, background = spec_color(data_aic[,6], option = "D", direction = -1))
    
    return(res_aic)
    
  } # end length(list_win)==1
  
  if(length(list_win>1)){
    
    data_aic_tmean <- data.frame()
    data_aic_tmax <- data.frame()
    data_aic_tmin <- data.frame()
    data_aic_tvar <- data.frame()
    
    for(x in seq_along(list_win)){
      
      model_tmean_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmean",window==list_win[x])
      model_tmax_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmax",window==list_win[x])
      model_tmin_aic <- model |> dplyr::filter(outcome == out,name_expo == "tmin",window==list_win[x])
      model_tvar_aic <- model |> dplyr::filter(outcome == out,name_expo == "tvar",window==list_win[x])
    
      aic_tmean <- model_tmean_aic[[select_type]]
      aic_tmax <- model_tmax_aic[[select_type]]
      aic_tmin <- model_tmin_aic[[select_type]]
      aic_tvar <- model_tvar_aic[[select_type]]
    
      colnames(aic_tmean[[1]]) <- c("df_var","df_lag",list_win[x])
      colnames(aic_tmax[[1]]) <- c("df_var","df_lag",list_win[x])
      colnames(aic_tmin[[1]]) <- c("df_var","df_lag",list_win[x])
      colnames(aic_tvar[[1]]) <- c("df_var","df_lag",list_win[x])
      
      if(x==1){ # Empty AIC table
        # We need to keep the df_var and df_lag columns
        data_aic_tmean <- data.frame(aic_tmean[[1]])
        data_aic_tmax <- data.frame(aic_tmax[[1]])
        data_aic_tmin <- data.frame(aic_tmin[[1]])
        data_aic_tvar <- data.frame(aic_tvar[[1]])
      }
      if(x>1){ # Non-empty AIC table
        # No need to add df_var and df_lag again
        data_aic_tmean <- data.frame(data_aic_tmean,aic_tmean[[1]][[list_win[x]]])
        colnames(data_aic_tmean)[length(colnames(data_aic_tmean))] <- list_win[x]
        
        data_aic_tmax <- data.frame(data_aic_tmax,aic_tmax[[1]][[list_win[x]]])
        colnames(data_aic_tmax)[length(colnames(data_aic_tmax))] <- list_win[x]
        
        data_aic_tmin <- data.frame(data_aic_tmin,aic_tmin[[1]][[list_win[x]]])
        colnames(data_aic_tmin)[length(colnames(data_aic_tmin))] <- list_win[x]
        
        data_aic_tvar <- data.frame(data_aic_tvar,aic_tvar[[1]][[list_win[x]]])
        colnames(data_aic_tvar)[length(colnames(data_aic_tvar))] <- list_win[x]
      }
    }
    
    if(length(list_win)==5){
      res_aic_tmean <-  knitr::kable(data_aic_tmean) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmean[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmean[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmean[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmean[,6], option = "D", direction = -1)) %>%
        kableExtra::column_spec(7, background = spec_color(data_aic_tmean[,7], option = "D", direction = -1))
      res_aic_tmax <-  knitr::kable(data_aic_tmax) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmax[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmax[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmax[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmax[,6], option = "D", direction = -1)) %>%
        kableExtra::column_spec(7, background = spec_color(data_aic_tmax[,7], option = "D", direction = -1))
      res_aic_tmin <-  knitr::kable(data_aic_tmin) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmin[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmin[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmin[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmin[,6], option = "D", direction = -1)) %>%
        kableExtra::column_spec(7, background = spec_color(data_aic_tmin[,7], option = "D", direction = -1))
      res_aic_tvar <-  knitr::kable(data_aic_tvar) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tvar[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tvar[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tvar[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tvar[,6], option = "D", direction = -1)) %>%
        kableExtra::column_spec(7, background = spec_color(data_aic_tvar[,7], option = "D", direction = -1))
    }
    if(length(list_win)==4){
      res_aic_tmean <-  knitr::kable(data_aic_tmean) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmean[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmean[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmean[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmean[,6], option = "D", direction = -1))
      res_aic_tmax <-  knitr::kable(data_aic_tmax) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmax[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmax[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmax[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmax[,6], option = "D", direction = -1))
      res_aic_tmin <-  knitr::kable(data_aic_tmin) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmin[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmin[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tmin[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tmin[,6], option = "D", direction = -1))
      res_aic_tvar <-  knitr::kable(data_aic_tvar) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tvar[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tvar[,4], option = "D", direction = -1)) %>%
        kableExtra::column_spec(5, background = spec_color(data_aic_tvar[,5], option = "D", direction = -1)) %>%
        kableExtra::column_spec(6, background = spec_color(data_aic_tvar[,6], option = "D", direction = -1))
    }
    if(length(list_win)==2){
      res_aic_tmean <-  knitr::kable(data_aic_tmean) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmean[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmean[,4], option = "D", direction = -1)) 
      res_aic_tmax <-  knitr::kable(data_aic_tmax) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmax[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmax[,4], option = "D", direction = -1)) 
      res_aic_tmin <-  knitr::kable(data_aic_tmin) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tmin[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tmin[,4], option = "D", direction = -1)) 
      res_aic_tvar <-  knitr::kable(data_aic_tvar) %>%
        kableExtra::column_spec(3, background = spec_color(data_aic_tvar[,3], option = "D", direction = -1)) %>%
        kableExtra::column_spec(4, background = spec_color(data_aic_tvar[,4], option = "D", direction = -1)) 
    }
    
    res_aic <- list(
      "tmean" = res_aic_tmean,
      "tmax" = res_aic_tmax,
      "tmin" = res_aic_tmin,
      "tvar" = res_aic_tvar
    )
    return(res_aic)
    
  } # end length(list_win)>1
}    

make_tbl_res_overall <- function(model, dfv, dfl, out, ap = NULL){
  
  column_ap = ifelse(is.null(ap),"",paste0("_",ap))
  column = paste0("mod_prediction",column_ap)
  list_win <- unique(model$window)
  
  model <- model |>
    dplyr::filter(dfvar==as.numeric(dfv) & dflag==as.numeric(dfl) & outcome==out)
  
  column_ind_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model[[column]][[x]]$prediction$predvar
      which(help == help_round_expo(model$p_hot_n[[x]]))
    }
  )
  column_ind_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model[[column]][[x]]$prediction$predvar
      which(help == help_round_expo(model$p_cold_p[[x]]))
    }
  )
  model$ind_hot <- column_ind_hot
  model$ind_cold <- column_ind_cold
  
  column_t_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      model[[column]][[x]]$prediction$predvar[help]
    }
  )
  column_t_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      model[[column]][[x]]$prediction$predvar[help]
    }
  )
  model$t_hot <- column_t_hot
  model$t_cold <- column_t_cold
  
  column_beta_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$allfit[help],digits=2)
    }
  )
  column_beta_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$allfit[help],digits=2)
    }
  )
  model$beta_hot <- column_beta_hot
  model$beta_cold <- column_beta_cold
  
  column_lower_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$alllow[help],digits=2)
    }
  )
  column_lower_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$alllow[help],digits=2)
    }
  )
  model$lower_hot <- column_lower_hot
  model$lower_cold <- column_lower_cold
  
  column_upper_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$allhigh[help],digits=2)
    }
  )
  column_upper_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$allhigh[help],digits=2)
    }
  )
  model$upper_hot <- column_upper_hot
  model$upper_cold <- column_upper_cold
  
  data_tbl = data.frame(
    "window" = unlist(model$window),
    "expo" = unlist(model$name_expo),
    "p_hot" = unlist(model$p_hot),
    "T_hot" = unlist(model$t_hot),
    "beta_hot" = unlist(model$beta_hot),
    "lower_hot" = unlist(model$lower_hot),
    "upper_hot" = unlist(model$upper_hot),
    "T_cold" = unlist(model$t_cold),
    "beta_cold" = unlist(model$beta_cold),
    "lower_cold" = unlist(model$lower_cold),
    "upper_cold" = unlist(model$upper_cold)
  )
  data_tbl = data_tbl |>
    dplyr::mutate(ci_hot = paste0("[", lower_hot, "; ", upper_hot, "]"),
                  ci_cold = paste0("[", lower_cold, "; ", upper_cold, "]"),
                  all_hot = paste0(beta_hot,ci_hot),
                  all_cold = paste0(beta_cold," ",ci_cold)) |>
    dplyr::select(expo, window, p_hot, starts_with(c("T_","all_")))
  data_tbl = reshape(data_tbl, idvar = c("expo","p_hot"), timevar = "window", direction = "wide")
  
  gt_tbl <- gt(data_tbl) |>
    cols_label(expo = "Exposure",
              p_hot = "Threshold",
              starts_with("all_") ~ html("&beta;"),
              starts_with("T_") ~ "Value (°C)") |>
    tab_spanner(
      label = "Heat exposure",
      columns = starts_with(c("all_hot","T_hot")),
      id = "w_hot"
    ) |>
    tab_spanner(
      label = "Cold exposure",
      columns = starts_with(c("all_cold","T_cold")),
      id = "w_cold"
    ) |>
    tab_spanner(
      label = "W",
      columns = ends_with("w"),
      id = "W"
    ) |>
    tab_spanner(
      label = "W1",
      columns = ends_with("w1"),
      id = "W1"
    ) |>
    tab_spanner(
      label = "W2",
      columns = ends_with("w2"),
      id = "W2"
    ) |>
    tab_spanner(
      label = "W3",
      columns = ends_with("w3"),
      id = "W3"
    ) |>
    tab_spanner(
      label = "W4",
      columns = ends_with("w4"),
      id = "W4"
    ) |>
    tab_spanner(
      label = "W5",
      columns = ends_with("w5"),
      id = "W5"
    ) 
  
  return(gt_tbl)
}
