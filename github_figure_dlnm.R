# Impacts of ambient temperature on pregnant women's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# DLNM plots ----

## Load Packages ----
library(targets)
library(dlnm)
library(pryr)
library(gplots)
library(scales)

## Define colors ----
col_hot <- rgb(234, 99, 18, maxColorValue = 255)
col_cold <- rgb(133, 160, 213, maxColorValue = 255)

## Functions ----
source("C:/Users/chloe/Documents/these/maternalCVD/R/helpers.R") # help_round_expo function

#' Overall cumulative association - Blood pressure
#
#' This function displays the overall cumulative association for blood pressure
#' (systolic and diastolic) and temperature (tmin, tmax and tmean) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response 
#' association and 6 different study time periods)
#'
#' @param model A nested data frame containing all the models to generate the plot on
#' @param outcome A string indicating the outcome
#' @param win A boolean indicating whether to include all windows of vulnerability from W1 to W5
#' @return Generated plots for overall cumulative associations in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_overall <- function(model, outcome, win){
  
  file = deparse(substitute(model))
  path_file = paste0("figs/results/",file,"/")
  
  comb = expand.grid(
    out = outcome,
    window = win,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model$dfvar)
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["window"]])
    print(x[["dfvar"]])
    
    mod <- model |> dplyr::filter(
      outcome == x[["out"]] & 
        window == x[["window"]] & 
        name_expo == x[["expo"]] &
        dfvar == x[["dfvar"]] & dflag==3 
    )
    
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",
                             ifelse(x[["out"]]=="cs_hr","hr",x[["out"]])))
    
    plot_name = paste0("fig_res_",out_name,"_dfvar",as.character(x[["dfvar"]]),"_",x[["expo"]],"_",x[["window"]],".png")
    
    title_expo = ifelse(x[["expo"]]=="tmean","Mean Temperature",
                        ifelse(x[["expo"]]=="tmax","Maximal Temperature",
                               ifelse(x[["expo"]]=="tmin","Minimal Temperature","Temperature variation")))
    title_x = paste0("Daily ",title_expo," (°C)")
    
    png(file = paste0(path_file,plot_name))
    
    plot(mod$mod_prediction[[1]]$prediction,
         "overall",
         xlab = title_x,
         ylab = expression(beta),
         xaxs = "i",
         ci.arg = list(col = gray(0.7)),
         main = paste0("Cumulative association for 28 days in ",title_expo)
    )
    dev.off()
    rm(mod)
  }
  )
}

#' Lag-response association for blood pressure
#
#' This function displays the lag-response association for blood pressure
#' (systolic and diastolic) and temperature (tmin, tmax, tmean and tvar) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response and  
#' lag-response associations and 6 different study time periods) for various
#' definition of heat and cold exposure (percentiles)
#'
#' @param model A nested data frame containing all the models to generate the plot on
#' @param outcome A string indicating the outcome
#' @param win A boolean indicating whether to include all windows of vulnerability from W1 to W5
#' @param cohort A string indicating the cohort 
#'  Default to "EDEN"
#' @return Generated plots for time-response curves in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_lag <- function(model, outcome, win, cohort="EDEN"){
  
  column_hot <- ifelse(cohort=="EDEN","p_hot_n","p_hot_all")
  column_cold <- ifelse(cohort=="EDEN","p_cold_p","p_cold_all")
  
  file = deparse(substitute(model))
  path_file = paste0("figs/results/",file,"/")
  
  comb = expand.grid(
    out = outcome,
    window = win,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model$dfvar),
    dflag = unique(model$dflag),
    p_cold = unique(as.character(model$p_cold))
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["window"]])
    print(x[["dfvar"]])
    print(x[["dflag"]])
    print(x[["p_cold"]])
    
    mod <- model |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        window == x[["window"]] & 
        dfvar == x[["dfvar"]] & 
        dflag == x[["dflag"]] & 
        p_cold == x[["p_cold"]]
    )
    
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",
                             ifelse(x[["out"]]=="cs_hr","hr",x[["out"]])))
    upper_out_name = ifelse(out_name=="hte","Hte",out_name)
    
    plot_name = paste0("fig_res_",out_name,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",x[["p_cold"]],"_",x[["expo"]],"_",x[["window"]],".png")
    
    title_y = ifelse(out_name=="sbp","Systolic blood pressure",
                     ifelse(out_name=="dbp","Diastolic blood pressure",
                            ifelse(out_name=="hr","Heart rate",
                                   ifelse(out_name=="hte","Hematocrit",
                                          ifelse(out_name=="dp","Double product")))))
    
    title_x = paste0("Days before ",upper_out_name," measurement")
    title_expo = ifelse(x[["expo"]]=="tmean","Mean Temperature",
                        ifelse(x[["expo"]]=="tmax","Maximal Temperature",
                               ifelse(x[["expo"]]=="tmin","Minimal Temperature","Temperature variation")))
    
    # Cold exposure
    png(file = paste0(path_file,plot_name))
    
    plot(mod$mod_prediction[[1]]$prediction,
         "slices",
         var = help_round_expo(as.numeric(mod[[column_cold]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xlim = c(28,0), 
         xaxs = "i",
         xaxt = "n",
         col = "dodgerblue3",
         lwd = 2,
         ci.arg = list(col = col_cold),
         main = paste0(x[["p_cold"]],"th percentile vs the median"),
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    dev.off()
    
    # Heat exposure
    plot_name = paste0("fig_res_",out_name,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",as.character(mod$p_hot[[1]]),"_",x[["expo"]],"_",x[["window"]],".png")
    png(file = paste0(path_file,plot_name))
    
    plot(mod$mod_prediction[[1]]$prediction,
         "slices",
         var = help_round_expo(as.numeric(mod[[column_hot]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xlim = c(28,0), 
         xaxs = "i",
         xaxt = "n",
         col = "firebrick1",
         lwd = 2,
         ci.arg = list(col = col_hot),
         main = paste0(as.character(mod$p_hot[[1]]),"th percentile vs the median"),
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    dev.off()
    
    rm(mod)
  })
  
}

#' Overall cumulative association - Blood pressure adjusted for air pollution
#
#' This function displays the overall cumulative association for blood pressure
#' (systolic and diastolic) and temperature (tmin, tmax and tmean) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response 
#' association and 6 different study time periods). All models were adjusted for
#' air pollution exposure including: PM2.5, PM10, NO2 and O3
#'
#' @param model A nested data frame containing all the models to generate the plot on
#' @param outcome A string indicating the outcome.s to be plotted
#' @param airpol A string indicating the air pollutant
#' @param win A list of windows of susceptibility to be plotted
#' @return Generated plots for overall cumulative associations in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_overall_ap <- function(model, outcome, airpol, win){
  
  bp_files = c("model_bp_pm25_tt","model_bp_pm10_tt","model_bp_no2_tt","model_bp_o3_tt")
  file = deparse(substitute(model))
  file2 = ifelse(file%in%bp_files,paste0("model_bp_ap_tt/",airpol,"/"),paste0(file,"/",airpol,"/"))
  path_file = paste0("figs/results/",file2)
  
  comb = expand.grid(
    out = outcome,
    window = win,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model$dfvar)
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["window"]])
    print(x[["dfvar"]])
    
    mod <- model |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        window == x[["window"]] &
        dfvar == x[["dfvar"]] & dflag==3
    )
    mod_var = paste0("mod_prediction_",airpol)
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",
                             ifelse(x[["out"]]=="cs_hr","hr",x[["out"]])))
    
    plot_name = paste0("fig_res_",out_name,"_",airpol,"_dfvar",as.character(x[["dfvar"]]),"_",x[["expo"]],"_",x[["window"]],".png")
    
    title_expo = ifelse(x[["expo"]]=="tmean","Mean Temperature",
                        ifelse(x[["expo"]]=="tmax","Maximal Temperature",
                               ifelse(x[["expo"]]=="tmin","Minimal Temperature","Temperature variation")))
    title_x = paste0("Daily ",title_expo," (°C)")
    
    png(file = paste0(path_file,plot_name))
    
    plot(mod[[mod_var]][[1]]$prediction,
         "overall",
         xlab = title_x,
         ylab = expression(beta),
         xaxs = "i",
         ci.arg = list(col = grey(0.7)),
         main = paste0("Cumulative association for 28 days in ",title_expo)
    )
    dev.off()
  }
  )
}

#' Lag-response association for blood pressure adjusted for air pollution
#
#' This function displays the lag-response association for blood pressure
#' (systolic and diastolic) and temperature (tmin, tmax, tmean and tvar) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response and  
#' lag-response associations) for various definition of heat and cold exposure 
#' (percentiles). All models were adjusted for air pollution exposure including: PM2.5, PM10, NO2 and O3
#'
#' @param model A nested data frame containing all the models to generate the plot on
#' @param outcome A string indicating the outcome.s to be plotted
#' @param airpol A string indicating the air pollutant to be plotted
#' @param win A list of windows of susceptibility to be plotted
#' @param cohort A string indicating the cohort 
#'  Default to "EDEN"
#' @return Generated plots for time-response curves in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_lag_ap <- function(model, outcome, airpol, win, cohort="EDEN"){
  
  bp_files = c("model_bp_pm25_tt","model_bp_pm10_tt","model_bp_no2_tt","model_bp_o3_tt")
  file = deparse(substitute(model))
  file2 = ifelse(file%in%bp_files,paste0("model_bp_ap_tt/",airpol,"/"),paste0(file,"/",airpol,"/"))
  path_file = paste0("figs/results/",file2)
  
  column_hot <- ifelse(cohort=="EDEN","p_hot_n","p_hot_all")
  column_cold <- ifelse(cohort=="EDEN","p_cold_p","p_cold_all")
  
  comb = expand.grid(
    out = outcome,
    window = win,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model$dfvar),
    dflag = unique(model$dflag),
    p_cold = unique(as.character(model$p_cold))
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["window"]])
    print(x[["dfvar"]])
    print(x[["dflag"]])
    print(x[["p_cold"]])
    
    mod <- model |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        window == x[["window"]] & 
        dfvar == x[["dfvar"]] & 
        dflag == x[["dflag"]] &
        p_cold == x[["p_cold"]]
    )
    mod_var = paste0("mod_prediction_",airpol)
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",
                             ifelse(x[["out"]]=="cs_hr","hr",x[["out"]])))
    
    plot_name = paste0("fig_res_",out_name,"_",airpol,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",x[["p_cold"]],"_",x[["expo"]],"_",x[["window"]],".png")
    title_x = paste0("Days before ",toupper(out_name)," measurement")
    title_expo = ifelse(x[["expo"]]=="tmean","Mean Temperature",
                        ifelse(x[["expo"]]=="tmax","Maximal Temperature",
                               ifelse(x[["expo"]]=="tmin","Minimal Temperature","Temperature variation")))
    
    # Cold exposure
    png(file = paste0(path_file,plot_name))
    
    plot(mod[[mod_var]][[1]]$prediction,
         "slices",
         var = help_round_expo(as.numeric(mod[[column_cold]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xlim = c(28,0), 
         xaxs = "i",
         xaxt = "n",
         col = "dodgerblue3",
         lwd = 2,
         ci.arg = list(col = col_cold),
         main = paste0(x[["p_cold"]],"th percentile vs the median"),
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    dev.off()
    
    # Heat exposure
    plot_name = paste0("fig_res_",out_name,"_",airpol,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",as.character(mod$p_hot[[1]]),"_",x[["expo"]],"_",x[["window"]],".png")
    png(file = paste0(path_file,plot_name))
    
    plot(mod[[mod_var]][[1]]$prediction,
         "slices",
         var = help_round_expo(as.numeric(mod[[column_hot]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xlim = c(28,0), 
         xaxs = "i",
         xaxt = "n",
         col = "firebrick1",
         lwd = 2,
         ci.arg = list(col = col_hot),
         main = paste0(as.character(mod$p_hot[[1]]),"th percentile vs the median"),
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    dev.off()
    
    rm(mod)
  })
  
}

#' Overall cumulative association for sex-stratified analyses
#
#' This function superposes the overall cumulative associations for sex-stratified
#' analyses (green: boys, purple: girls) for:
#' temperature (tmin, tmax, tmean and tvar) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response and  
#' lag-response associations).
#'
#' @param model_m A nested data frame containing sex-stratified models on boys
#' @param model_f A nested data frame containing sex-stratified models on girls
#' @param outcome A string indicating the outcome.s to be plotted
#' @return Generated plots for dose-response curves in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_overall_combined <- function(model_m, model_f, outcome){
  
  main_outcome <- ifelse("sbp_z_adapted"%in%outcome,"bp",outcome[1])
  path_file = paste0("figs/results/model_",main_outcome,"_sex/")
  
  comb = expand.grid(
    out = outcome,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model_m$dfvar)
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["dfvar"]])
    
    mod_m <- model_m |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        dfvar == x[["dfvar"]] & dflag==3
    )
    mod_f <- model_f |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        dfvar == x[["dfvar"]] & dflag==3
    )
    
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",x[["out"]]))
    
    plot_name = paste0("fig_res_",out_name,"_dfvar",as.character(x[["dfvar"]]),"_",x[["expo"]],"_w.png")
    title_expo = stringr::str_to_sentence(x[["expo"]])
    title_x = paste0(title_expo," (°C)")
    
    yl_sup <- ifelse(out_name=="hr",15,ifelse(out_name=="hte",3,0.8))
    yl_inf <- ifelse(out_name=="hr",-15,ifelse(out_name=="hte",-3,-0.8))
    
    png(file = paste0(path_file,plot_name))
    
    plot(mod_m$mod_prediction[[1]]$prediction, 
         "overall",
         xlab = title_x,
         ylab = expression(beta),
         xaxs="i", 
         col = "green4",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(-10,30),
         ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.35)),
         main="")
    par(new=T)
    plot(mod_f$mod_prediction[[1]]$prediction, 
         "overall", 
         xlab = title_x,
         ylab = expression(beta),
         xaxs="i",
         col = "purple",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(-10,30),
         ci.arg=list(col= adjustcolor("orchid3", alpha.f = 0.35)), 
         main = paste0("Overall cumulative association for 28 days in ",title_expo))
    legend(x = "bottomleft",
           title = "  Sex-stratification  ",
           legend = c("Boys","Girls"),
           col = c("green4","purple"),
           bty = "n",
           pch = 16,
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F, 
           inset = c(0.1, 0.1))
    dev.off()
  })
}

#' Lag-response association for sex-stratified analyses
#
#' This function superposes the lag-response associations for sex-stratified
#' analyses (green: boys, purple: girls) for:
#' temperature (tmin, tmax, tmean and tvar) for different 
#' parameters of the DLNM (various degrees of freedom for the dose-response and  
#' lag-response associations) for various definition of heat and cold exposure 
#' (percentiles).
#'
#' @param model_m A nested data frame containing sex-stratified models on boys
#' @param model_f A nested data frame containing sex-stratified models on girls
#' @param outcome A string indicating the outcome.s to be plotted
#' @return Generated plots for lag-response curves in the figs/results/ dedicated folder with appropriated file names
dlnm_plot_lag_combined <- function(model_m, model_f, outcome, cohort="EDEN"){
  
  main_outcome = ifelse("sbp_z_adapted"%in%outcome,"bp",outcome[1])
  path_file = paste0("figs/results/model_",main_outcome,"_sex/")
  
  column_hot <- ifelse(cohort=="EDEN","p_hot_n","p_hot_all")
  column_cold <- ifelse(cohort=="EDEN","p_cold_p","p_cold_all")
  
  comb = expand.grid(
    out = outcome,
    expo = c("tmean","tmax","tmin","tvar"),
    dfvar = unique(model_m$dfvar),
    dflag = unique(model_m$dflag),
    p_cold = unique(as.character(model_m$p_cold))
  )
  
  fig = apply(comb, 1, function(x) {
    
    print(x[["out"]])
    print(x[["expo"]])
    print(x[["dfvar"]])
    print(x[["dflag"]])
    print(x[["p_cold"]])
    
    mod_m <- model_m |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        dfvar == x[["dfvar"]] & 
        dflag == x[["dflag"]] 
    )
    mod_f <- model_f |> dplyr::filter(
      outcome == x[["out"]] &
        name_expo == x[["expo"]] &
        dfvar == x[["dfvar"]] & 
        dflag == x[["dflag"]] &
        p_cold == x[["p_cold"]]
    )
    
    out_name = ifelse(x[["out"]]=="sbp_z_adapted","sbp",
                      ifelse(x[["out"]]=="dbp_z_adapted","dbp",x[["out"]]))
    
    plot_name = paste0("fig_res_",out_name,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",as.character(x[["p_cold"]]),"_",x[["expo"]],"_w.png")
    title_x = paste0("Day before ",toupper(out_name)," measurement")
    title_expo = stringr::str_to_sentence(x[["expo"]])
    cold_title = bquote("Lag-response curve at " ~ .(x[["p_cold"]]) ^ "th" ~ " perc. vs. the median")
    heat_title = bquote("Lag-response curve at " ~ .(mod_f$p_hot[[1]]) ^ "th" ~ " perc. vs. the median")
    
    yl_sup <- ifelse(out_name=="hr",1,ifelse(out_name=="hte",0.3,0.05))
    yl_inf <- ifelse(out_name=="hr",-1,ifelse(out_name=="hte",-0.3,-0.05))
    
    png(file = paste0(path_file,plot_name))
    
    # Cold exposure
    plot(mod_m$mod_prediction[[1]]$prediction, 
         "slice",
         var = help_round_expo(as.numeric(mod_m[[column_cold]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xaxs = "i", 
         xaxt = "n",
         col = "green4",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(28,0),
         ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.35)),
         main = cold_title,
         col.main = "black"
    )
    par(new=T)
    plot(mod_f$mod_prediction[[1]]$prediction, 
         "slice", 
         var = help_round_expo(as.numeric(mod_f[[column_cold]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xaxs = "i",
         xaxt = "n",
         col = "purple",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(28,0),
         ci.arg=list(col= adjustcolor("orchid3", alpha.f = 0.35)), 
         main = cold_title,
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    legend(x = "bottomleft",
           title = "  Sex-stratification  ",
           legend = c("Boys","Girls"),
           col = c("green4","purple"),
           bty = "n",
           pch = 16,
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F, 
           inset = c(0.1, 0.1))
    dev.off()
    
    # Heat exposure
    plot_name = paste0("fig_res_",out_name,"_",paste0(as.character(x[["dfvar"]]),as.character(x[["dflag"]])),
                       "_p",as.character(mod_m$p_hot[[1]]),"_",x[["expo"]],"_w.png")
    png(file = paste0(path_file,plot_name))
    
    plot(mod_m$mod_prediction[[1]]$prediction, 
         "slice",
         var = help_round_expo(as.numeric(mod_m[[column_hot]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xaxs = "i", 
         xaxt = "n",
         col = "green4",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(28,0),
         ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.35)),
         main = heat_title,
         col.main = "black"
    )
    par(new=T)
    plot(mod_f$mod_prediction[[1]]$prediction, 
         "slice", 
         var = help_round_expo(as.numeric(mod_f[[column_hot]][[1]])),
         ylab = expression(beta),
         xlab = title_x,
         xaxs = "i",
         xaxt = "n",
         col = "purple",
         lwd = 2,
         ylim = c(yl_inf,yl_sup),
         xlim = c(28,0),
         ci.arg=list(col= adjustcolor("orchid3", alpha.f = 0.35)), 
         main = heat_title,
         col.main = "black"
    )
    box(col = "black")
    axis(1, at = 28:0, labels = c(28:0))
    legend(x = "bottomleft",
           title = "  Sex-stratification  ",
           legend = c("Boys","Girls"),
           col = c("green4","purple"),
           bty = "n",
           pch = 16,
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F, 
           inset = c(0.1, 0.1))
    dev.off()
    
    rm(mod_m)
    rm(mod_f)
  }
  )
}

# Figure 3 ----

outcome <- c("sbp_z_adapted","dbp_z_adapted")

tar_load(model_bp_w_tt) # Blood pressure main models
dlnm_plot_overall(model_bp_w_tt, outcome, c("w")) # Overall cumulative association
dlnm_plot_lag(model_bp_w_tt, outcome, c("w")) # Lag-response curve
rm(model_bp_w_tt)

# Figure 4 ----

tar_load(model_bp_f_tt)
tar_load(model_bp_m_tt)
tar_load(model_bp_w_tt)

mod_m <- model_bp_m_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")
mod_f <- model_bp_f_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")
mod <- model_bp_w_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")

png(file = "figs/article/Figure 4 overall.png")

plot(mod_m$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "green4",
     lwd = 2,
     cex.axis = 1.25,
     cex.lab = 1.25,
     ylim = c(-.7,.7),
     xlim = c(-11.5,32.5),
     ci.arg=list(col= adjustcolor("palegreen3", alpha.f = .5)),
     main="")
par(new=T)
plot(mod_f$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs = "i",
     col = "purple",
     lwd = 2,
     cex.axis = 1.25,
     cex.lab = 1.25,
     ylim = c(-.7,.7),
     xlim = c(-11.5,32.5),
     ci.arg = list(col = adjustcolor("orchid3", alpha.f = .5)), 
     main = "")
par(new=T)
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray3",
     lwd = 2,
     cex.axis = 1.25,
     cex.lab = 1.25,
     ylim = c(-.7,.7),
     xlim = c(-11.5,32.5),
     ci.arg=list(col= adjustcolor("gray35", alpha.f = .5)),
     main="")
# legend(x = "topright",
#        title = "  Sex-stratification  ",
#        legend = c("Boys","Girls","All"),
#        col = c("green4","purple","gray35"),
#        bty = "n",
#        pch = 16,
#        xpd = TRUE,
#        pt.cex = 2,
#        cex = 1.2,
#        text.col = "black",
#        # horiz = F,
#        inset = c(.1, .1))
dev.off()

cold_title = bquote(.("5") ^ "th" ~ " perc. vs. the median")
heat_title = bquote(.("95") ^ "th" ~ " perc. vs. the median")

png(file = "figs/article/Figure 4 lag cold.png")

# Cold exposure
plot(mod_m$mod_prediction[[1]]$prediction, 
     "slice",
     var = help_round_expo(as.numeric(mod_m$p_cold_p[[1]])),
     ylab = expression(beta),
     xlab = "Day before SBP measurement",
     xaxs = "i", 
     xaxt = "n",
     col = "green4",
     cex.axis = 1.5,
     cex.lab = 1.25,
     cex.main =2,
     lwd = 2,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.35)),
     main = cold_title,
     col.main = "black"
)
par(new=T)
plot(mod_f$mod_prediction[[1]]$prediction, 
     "slice", 
     var = help_round_expo(as.numeric(mod_f$p_cold_p[[1]])),
     ylab = expression(beta),
     xlab = "",
     xaxs = "i",
     xaxt = "n",
     cex.axis= 1.5,
     cex.lab = 1.25,
     col = "purple",
     lwd = 2,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("orchid3", alpha.f = 0.35)), 
     main = "",
     col.main = "black"
)
par(new=T)
plot(mod$mod_prediction[[1]]$prediction, 
     "slice", 
     var = help_round_expo(as.numeric(mod$p_cold_p[[1]])),
     ylab = expression(beta),
     xlab = "",
     xaxs = "i",
     xaxt = "n",
     cex.axis= 1.5,
     cex.lab = 1.25,
     col = "gray3",
     lwd = 2,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("gray35", alpha.f = 0.35)), 
     main = "",
     col.main = "black"
)
box(col = "black")
axis(1, at = 28:0, labels = c(28:0), cex.axis=1.5)
dev.off()

# Heat exposure
png(file = "figs/article/Figure 4 lag hot.png")

plot(mod_m$mod_prediction[[1]]$prediction, 
     "slice",
     var = help_round_expo(as.numeric(mod_m$p_hot_n[[1]])),
     ylab = expression(beta),
     xlab = "Day before SBP measurement",
     xaxs = "i", 
     xaxt = "n",
     cex.axis= 1.5,
     cex.lab = 1.25,
     cex.main = 2,
     col = "green4",
     lwd = 2,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.35)),
     main = heat_title,
     col.main = "black"
)
par(new=T)
plot(mod_f$mod_prediction[[1]]$prediction, 
     "slice", 
     var = help_round_expo(as.numeric(mod_f$p_hot_n[[1]])),
     ylab = expression(beta),
     xlab = "",
     xaxs = "i",
     xaxt = "n",
     col = "purple",
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("orchid3", alpha.f = 0.35)), 
     main = "",
     col.main = "black"
)
par(new=T)
plot(mod$mod_prediction[[1]]$prediction, 
     "slice", 
     var = help_round_expo(as.numeric(mod$p_hot_n[[1]])),
     ylab = expression(beta),
     xlab = "",
     xaxs = "i",
     xaxt = "n",
     col = "gray3",
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.04,.04),
     xlim = c(28,0),
     ci.arg=list(col= adjustcolor("gray35", alpha.f = 0.35)), 
     main = "",
     col.main = "black"
)
box(col = "black")
axis(1, at = 28:0, labels = c(28:0), cex.axis=1.5)
dev.off()

# Figure 5 ----

tar_load(model_bp_w_tt)
mod <- model_bp_w_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

# PM2.5
tar_load(model_bp_pm25_tt)
mod_pm <- model_bp_pm25_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Figure 5 pm25.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm$mod_prediction_pm25[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "darkorange",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("darkorange3", alpha.f = 0.35)),
     main = "")
# legend(x = "bottomleft",
#        title = "Adjustment to air pollution exposure",
#        legend = c(expression("PM"[2.5]),expression("PM"[10]),expression("NO"[2]),expression("O"[3]),"No adjustment"),
#        col = c(adjustcolor("darkorange3", alpha.f = 0.75),
#                adjustcolor("goldenrod4", alpha.f = 0.75),
#                adjustcolor("coral4", alpha.f = 0.75),
#                adjustcolor("plum4", alpha.f = 0.75),
#                adjustcolor("gray40", alpha.f = 0.75)),
#        bty = "n",
#        pch = 16,
#        pt.cex = 2,
#        cex = 1.2,
#        text.col = "black",
#        horiz = F,
#        inset = c(0.4, 0.6))
dev.off()

# PM10
tar_load(model_bp_pm10_tt)
mod_pm <- model_bp_pm10_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Figure 5 pm10.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm$mod_prediction_pm10[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "goldenrod",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("goldenrod4", alpha.f = 0.35)),
     main = "")
dev.off()

# NO2
tar_load(model_bp_no2_tt)
mod_pm <- model_bp_no2_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Figure 5 no2.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm$mod_prediction_no2[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "coral",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("coral4", alpha.f = 0.35)),
     main = "")
dev.off()

# O3
tar_load(model_bp_o3_tt)
mod_pm <- model_bp_o3_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Figure 5 o3.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm$mod_prediction_o3[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "mediumpurple",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-.6,.6),
     xlim = c(-10,32),
     ci.arg=list(col= adjustcolor("plum", alpha.f = 0.75)),
     main = "")
dev.off()

# Supplementary Figure 8 ----

tar_load(model_hte_tt)
tar_load(model_hte_f_tt)
tar_load(model_hte_m_tt)
tar_load(model_hte_diff_sex_tt)

m <- model_hte_tt |> dplyr::filter(name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")
mod_m <- model_hte_m_tt |> dplyr::filter(name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")
mod_f <- model_hte_f_tt |> dplyr::filter(name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")
mod <- model_hte_diff_sex_tt |> dplyr::filter(name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95 & window=="w")

png(file = "figs/article/Supplementary Figure 8 girls.png")
plot(mod_f$mod_prediction[[1]]$prediction,
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     col = "purple",
     xaxs = "i",
     lwd = 2, 
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-3,3.1),
     ci.arg = list(col = alpha("orchid3", .35)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 8 boys.png")
plot(mod_m$mod_prediction[[1]]$prediction,
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     col = "green3",
     xaxs = "i",
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-3,3.1),
     ci.arg = list(col = alpha("palegreen3", .35)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 8 overall.png")
plot(m$mod_prediction[[1]]$prediction,
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i",
     lwd = 2,
     cex.axis = 1.5,
     cex.lab = 1.25,
     ylim = c(-4,4),
     xlim = c(-10,32.5),
     ci.arg = list(col = alpha("gray32", .5)),
     main="")
# legend(x = "bottomleft",
#        title = "  Sex-stratification  ",
#        legend = c("Boys","Girls","Overall"),
#        col = c("green4","purple","gray35"),
#        bty = "n",
#        pch = 16,
#        xpd = TRUE,
#        pt.cex = 2,
#        cex = 1.2,
#        text.col = "black",
#        # horiz = F,
#        inset = c(.1, .1))
dev.off()

# Supplementary Figure 9 ----

tar_load(model_bp_w_tt)
mod <- model_bp_w_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

## PM2.5 ----
tar_load(model_bp_pm25_high_tt)
tar_load(model_bp_pm25_low_tt)
mod_pm_high <- model_bp_pm25_high_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)
mod_pm_low <- model_bp_pm25_low_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Supplementary Figure 9 pm25 high.png") 
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-10,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_high$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "darkorange",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-10,31),
     ci.arg=list(col= adjustcolor("darkorange3", alpha.f = 0.35)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 9 pm25 low.png") 
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-6.5,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_low$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "darkorange",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-6.5,31),
     ci.arg=list(col= adjustcolor("darkorange3", alpha.f = 0.35)),
     main = "")
# legend(x = "bottomleft",
#        title = "Stratification by air pollution exposure levels",
#        legend = c(expression("PM"[2.5]),expression("PM"[10]),expression("NO"[2]),expression("O"[3]),"NDVI","No stratification"),
#        col = c(adjustcolor("darkorange3", alpha.f = 0.75),
#                adjustcolor("goldenrod4", alpha.f = 0.75),
#                adjustcolor("coral4", alpha.f = 0.75),
#                adjustcolor("plum4", alpha.f = 0.75),
#                adjustcolor("palegreen3", alpha.f = 0.75),
#                adjustcolor("gray40", alpha.f = 0.75)),
#        bty = "n",
#        pch = 16,
#        pt.cex = 2,
#        cex = 1.2,
#        text.col = "black",
#        horiz = F,
#        inset = c(0.3, 0.1))
dev.off()

## PM10 ----
tar_load(model_bp_pm10_high_tt)
tar_load(model_bp_pm10_low_tt)
mod_pm_high <- model_bp_pm10_high_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)
mod_pm_low <- model_bp_pm10_low_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Supplementary Figure 9 pm10 high.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-10,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_high$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "goldenrod",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-10,31),
     ci.arg=list(col= adjustcolor("goldenrod4", alpha.f = 0.35)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 9 pm10 low.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-8,30),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_low$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "goldenrod",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-8,30),
     ci.arg=list(col= adjustcolor("goldenrod4", alpha.f = 0.35)),
     main = "")
dev.off()

## NO2 ----
tar_load(model_bp_no2_high_tt)
tar_load(model_bp_no2_low_tt)
mod_pm_high <- model_bp_no2_high_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)
mod_pm_low <- model_bp_no2_low_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Supplementary Figure 9 no2 high.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-9.5,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_high$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "coral",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-9.5,31),
     ci.arg=list(col= adjustcolor("coral4", alpha.f = 0.35)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 9 no2 low.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-6.5,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_low$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "coral",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-6.5,31),
     ci.arg=list(col= adjustcolor("coral4", alpha.f = 0.35)),
     main = "")
dev.off()

## O3 ----
tar_load(model_bp_o3_high_tt)
tar_load(model_bp_o3_low_tt)
mod_pm_high <- model_bp_o3_high_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)
mod_pm_low <- model_bp_o3_low_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Supplementary Figure 9 o3 high.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-.5,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_high$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "mediumpurple",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-.5,31),
     ci.arg=list(col= adjustcolor("plum", alpha.f = 0.75)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 9 o3 low.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-.5,20.5),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_low$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "mediumpurple",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-.5,20.5),
     ci.arg=list(col= adjustcolor("plum", alpha.f = 0.75)),
     main = "")
dev.off()

## NDVI ----
tar_load(model_bp_ndvi_high_tt)
tar_load(model_bp_ndvi_low_tt)
mod_pm_high <- model_bp_ndvi_high_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)
mod_pm_low <- model_bp_ndvi_low_tt |> dplyr::filter(outcome=="sbp_z_adapted" & name_expo=="tmean" & dfvar==2 & dflag==3 & p_hot==95)

png(file = "figs/article/Supplementary Figure 9 ndvi high.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-.5,31),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_high$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "darkgreen",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.1,.7),
     xlim = c(-.5,31),
     ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.75)),
     main = "")
dev.off()
png(file = "figs/article/Supplementary Figure 9 ndvi low.png")
plot(mod$mod_prediction[[1]]$prediction, 
     "overall",
     xlab = "Tmean (°C)",
     ylab = expression(beta),
     xaxs="i", 
     col = "gray32",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-.5,20.5),
     ci.arg=list(col= adjustcolor("gray40", alpha.f = 0.35)),
     main="")
par(new=T)
plot(mod_pm_low$mod_prediction[[1]]$prediction, 
     "overall", 
     xlab = "",
     ylab = expression(beta),
     xaxs="i",
     col = "darkgreen",
     lwd = 2,
     lwd = 2,
     cex.axis= 1.5,
     cex.lab = 1.25,
     ylim = c(-1.5,.5),
     xlim = c(-.5,20.5),
     ci.arg=list(col= adjustcolor("palegreen3", alpha.f = 0.75)),
     main = "")
dev.off()
