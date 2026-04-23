# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Sensitivity analyses ----

## Load packages ----
library(targets)
library(ggforestplot)
library(ggplot2)
library(stringr)
library(ggpubr)

source("C:/Users/chloe/Documents/these/maternalCVD/R/helpers.R") # help_round_expo function

## Parameters ----
dfv=2 # Degree of freedom for the dose-response
dfl=3 # Degree of freedom for the lag-response
p=95  # Heat exposure: 95th percentile; Cold exposure: 5th percentile

column = "mod_prediction" # Column name where the results are stored

## Function ----
compute_res <- function(model){
  
  model <- model |>
    dplyr::filter(dfvar==as.numeric(dfv) & dflag==as.numeric(dfl) & p_hot==as.numeric(p))
  
  column_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model[[column]][[x]]$prediction$predvar
      which(help == help_round_expo(model$p_hot_n[[x]]))
    }
  )
  column_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model[[column]][[x]]$prediction$predvar
      which(help == help_round_expo(model$p_cold_p[[x]]))
    }
  )
  model$ind_hot <- column_hot
  model$ind_cold <- column_cold
  
  column_beta_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$allfit[help],digits=3)
    }
  )
  column_beta_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$allfit[help],digits=3)
    }
  )
  model$beta_hot <- column_beta_hot
  model$beta_cold <- column_beta_cold
  
  column_lower_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$alllow[help],digits=3)
    }
  )
  column_lower_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$alllow[help],digits=3)
    }
  )
  model$lower_hot <- column_lower_hot
  model$lower_cold <- column_lower_cold
  
  column_upper_hot = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_hot[[x]]
      round(model[[column]][[x]]$prediction$allhigh[help],digits=3)
    }
  )
  column_upper_cold = lapply( 
    1:nrow(model),
    function(x) {
      help = model$ind_cold[[x]]
      round(model[[column]][[x]]$prediction$allhigh[help],digits=3)
    }
  )
  model$upper_hot <- column_upper_hot
  model$upper_cold <- column_upper_cold
  
  return(model)
}

# Supplementary Figure 10 ----

## Load results ----
tar_load(model_bp_w_tt) # Main analysis
tar_load(model_bp_hradj_tt) # BP analysis, adjusted for HR
tar_load(model_bp_hteadj_tt) # BP analysis, adjusted for Hte
tar_load(model_bp_gdiab_tt) # BP analysis, adjusted for gestational diabetes
tar_load(model_bp_w) # BP analysis, without time trend adjustment
tar_load(model_bp_noHTN_tt) # BP analysis, excluding hypertension during pregnancy
tar_load(model_bp_noTTT_tt) # BP analysis, excluding participants under medication
tar_load(model_bp_nopreHTN_tt) # BP analysis, excluding hypertension before pregnancy

## Extract results ----
model_main <- compute_res(model_bp_w_tt)
model_hr <- compute_res(model_bp_hradj_tt)
model_hte <- compute_res(model_bp_hteadj_tt)
model_gdiab <- compute_res(model_bp_gdiab_tt)
model_nott <- compute_res(model_bp_w)
model_noHTN <- compute_res(model_bp_noHTN_tt)
model_noTTT <- compute_res(model_bp_noTTT_tt)
model_nopreHTN <- compute_res(model_bp_nopreHTN_tt)

## Name results ----
model_main$sens = rep("Main model",nrow(model_main))
model_hr$sens = rep("Adjusted for heart rate",nrow(model_hr))
model_hte$sens = rep("Adjusted for hematocrit",nrow(model_hte))
model_gdiab$sens = rep("Adjusted for gestational diabetes",nrow(model_gdiab))
model_nott$sens = rep("Adjustment for time trend removed",nrow(model_nott))
model_noHTN$sens = rep("Exclude women with hypertension during pregnancy",nrow(model_noHTN))
model_noTTT$sens = rep("Exclude women under medication",nrow(model_noTTT))
model_nopreHTN$sens = rep("Exclude women with hypertension before pregnancy",nrow(model_nopreHTN))

## Bind redults ----
model <- rbind(model_main, model_hr,model_hte,model_gdiab,model_nott,model_noHTN,model_noTTT,model_nopreHTN)
model <- model |>
  dplyr::mutate(
    name_expo = stringr::str_to_title(name_expo),
    outcome = paste0("z",toupper(str_remove(outcome, "_z_adapted")))
  )
  
## Plot results ----
data_plt = data.frame(
    "outcome" = unlist(model$outcome), # Outcome (SBP or DBP)
    "expo" = unlist(model$name_expo), # Exposure
    "p_hot" = as.character(unlist(model$p_hot)), # Percentile for heat exposure
    "sens" = unlist(model$sens), # Name of the sensitivity analysis
    "beta_hot" = unlist(model$beta_hot), # Estimate for heat effect
    "lower_hot" = unlist(model$lower_hot), # Lower bound of heat effect
    "upper_hot" = unlist(model$upper_hot), # Upper bound of heat effect
    "beta_cold" = unlist(model$beta_cold), # Estimate of cold effect
    "lower_cold" = unlist(model$lower_cold), # Lower bound of cold effect
    "upper_cold" = unlist(model$upper_cold) # Upper bound of cold effect
)
data_plt = data_plt |>
    dplyr::mutate(se_hot = as.numeric((upper_hot - lower_hot)/3.92), 
                  se_cold = (upper_cold - lower_cold)/3.92,
                  ci_hot = paste0("[", lower_hot, "; ", upper_hot, "]"), # Confidence interval for heat effect
                  ci_cold = paste0("[", lower_cold, "; ", upper_cold, "]"), # Confidence interval for cold effect
                  all_hot = paste0(beta_hot,ci_hot),
                  all_cold = paste0(beta_cold," ",ci_cold)) |>
    dplyr::select(outcome, sens, expo, p_hot, starts_with(c("beta_","all_","se_","ci_","lower_","upper_")))

g_hot <- ggforestplot::forestplot(
  df = data_plt,
  name = expo,
  estimate = beta_hot,
  se = se_hot,
  colour = outcome,
  xlab = expression(beta),
  title = "Heat exposure"
) +
ggforce::facet_col(
    facets = ~ sens,
    scales = "free_y",
    space = "free"
) + guides(colour=guide_legend(title="Outcome")) +
  scale_colour_manual(
    values = c("zSBP" = "mediumpurple","zDBP" = "black")
  )
g_cold <- ggforestplot::forestplot(
  df = data_plt,
  name = expo,
  estimate = beta_cold,
  se = se_cold,
  colour = outcome,
  xlab = expression(beta),
  title = "Cold exposure"
) +
ggforce::facet_col(
    facets = ~ sens,
    scales = "free_y",
    space = "free"
) +
guides(colour=guide_legend(title="Outcome")) +
scale_colour_manual(
     values = c("zSBP" = "mediumpurple","zDBP" = "black")
   )

## Final figure ----
ggarrange(g_hot, g_cold, ncol=2, nrow=1, common.legend = TRUE, legend="right")
