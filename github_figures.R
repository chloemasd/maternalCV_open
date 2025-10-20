# Impacts of ambient temperature on pregnant woemn's cardiovascular function
# and variations related to fetal sex
# Author: Chloé Masdoumier
# Last update: 2025-10-06

# Figures functions ----

## Descriptive plots ----

### Cardiovascular health ----

#' Histogram of gestational ages at measurement
#'
#' This function produces bar-plots of the distribution of measurements
#' through gestational ages
#' 
#' @param data_in A data frame containing data to plot
#' @param out A string indicating the outcome
#' @return Bar-plots of gestational ages at measurement 
make_hist_ga <- function(data_in, out){
  
  column = ifelse(!"outcome"%in%colnames(data_in),"sbp_z_adapted","outcome")
  cols = c(column,"cs_ga")
  
  ylab = ifelse(column=="sbp_z_adapated","Number of measurements","")
  tit = ifelse(out=="bp","Blood pressure (SD)",
               ifelse(out=="hr","Heart rate (bpm)","Hematocrit (%)"))
  
  data_plot <- data_in |>
    group_by(across(all_of(cols))) |> # Group the outcome of interest by gestational age at measurement
    summarise(count = n()) # Count the number of measurements for each gestational age
  
  p <- data_plot |>
    ggplot(aes(
      y = count, # Number of measurements
      x = as.numeric(cs_ga), # Gestational age at measurement
      fill = .data[[column]],
      label = count
    )) +
    geom_bar(stat="identity", alpha=0.8) +
    geom_vline(xintercept = c(4,10,17,24,31,38), color="black", linetype="dashed") + # Outline the susceptibility windows investigated
    ylim(c(0,700)) + 
    labs(x = "", y = ylab, title=tit) +
    guides(fill= "none") 
  
  return(p)
}

#' Box-plots of maternal CVD function
#'
#' This function produces box-plots of maternal CVD indicator among the outcome
#' of interest in our analyses (SBP, DBP, HR, DP and Hte) through calendar time
#' (months) and colored upon the season of measurement/conception
#' 
#' @param data_in A data frame containing data to plot
#' @param outcome A string indicating which outcome has to be plotted
#' @param column A string indicating the time-varying variable to be used
#' @return Box-plots of maternal CVD function
make_plot_hemo <- function(data_in, outcome, column){
  
  if(!column %in% c("po_monthe","po_monthc")){ # Only 'po_monthe' and 'po_monthc' are allowed
    stop("Unadequate 'column' argument")
  }
  
  # Prepare axis labels
  ## X-axis
  xlabs = ifelse(stringr::str_detect(column,"monthe"),"Month of measurement","")
  xlabs = ifelse(stringr::str_detect(column,"monthc"),"Month of conception",xlabs)
  ## Y-axis
  ylabs = ifelse(stringr::str_detect(outcome,"z"),"(SD)","(mmHg)")
  ylabs = ifelse(stringr::str_detect(outcome,"sbp") | stringr::str_detect(outcome,"tas"),paste0("SBP ",ylabs),paste0("DBP ",ylabs))
  ylabs = ifelse(stringr::str_detect(outcome,"z"),paste0("z",ylabs),ylabs)
  if(outcome=="hr"){ylabs = "HR (bpm)"}
  if(outcome=="hte"){ylabs = "Hte (%)"}
  if(outcome%in%c("hr","hte")){outcome = "outcome"}
  
  # Prepare the fill argument based on season of measurement/conception
  column2 = ifelse(stringr::str_detect(column,"monthe"),"po_seasone","")
  column2 = ifelse(stringr::str_detect(column,"monthc"),"po_seasonc",column2)
  
  # Prepare x-axis labels
  xlabels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  # Box-plots
  p <- data_in |>
    ggplot(aes(x = .data[[column]], y = .data[[outcome]], fill = .data[[column2]])) + # Fill by season of measurement/conception
    geom_boxplot() + 
    scale_fill_manual(values = alpha(c("darkgreen", "gold4", "darkred", "dodgerblue"), 0.3)) + 
    scale_x_discrete(label = xlabels) + 
    labs(y = ylabs, x = "", fill = "Season") # Axis labels
  
  return(p)
}

#' Z-scores of blood pressure trajectory 
#'
#' This function produces trajectories of SBP and DBP Z-scores through various 
#' modes of a given covariate to help evaluate accuracy and range of BP Z-scores 
#' within the study population.
#' 
#' @param data_in A data frame containing data to plot
#' @param subgroup A string indicating which covariate to plot
#' @return Trajectories of BP Z-scores through modes of a given covariates 
make_zbp <- function(data_in, subgroup){
  
  # The function only supports age, parity, BMI, diabetes and HTN as covariate
  if(!subgroup %in% c("htn","gdm","age","bmi","parity")){
    stop("Input variable not supported")
  }
  
  col_name <- ifelse(subgroup=="htn","diag_hdp_all",
                     ifelse(subgroup=="gdm","diag_gdiab",
                            ifelse(subgroup=="age","mo_age_cat4",
                                   ifelse(subgroup=="bmi","mo_bmi_bepr_cat3",
                                          ifelse(subgroup=="parity","mo_par_cat3")))))
  
  max <- max(max(data_in$sbp_z_adapted),max(data_in$dbp_z_adapted))
  min <- min(min(data_in$sbp_z_adapted),min(data_in$dbp_z_adapted))
  
  data_plot <- data_in |>
    dplyr::mutate(
      across(c("diag_hdp_all","diag_gdiab","mo_age_cat4","mo_bmi_bepr_cat3","mo_par_cat3"),
             ~ factor(.),
             .names = "{.col}"))
  data_plot$grouping <- data_plot[[col_name]]
  data_plot <- data_plot |>
    dplyr::group_by(grouping,weeks) |> # Group Z-scores by gestational ages at measurements and mode of the covariate
    dplyr::summarize(mean_s = mean(sbp_z_adapted), # Mean of SBP Z-scores
                     mean_d = mean(dbp_z_adapted)) # Mean of DBP Z-scores
  
  # SBP plot
  psbp <- ggplot(data_plot, aes(x = weeks, y = mean_s, color = grouping)) +
    geom_point() +
    geom_smooth() + # Trajectory with CI
    geom_hline(yintercept = 0, color="black", linetype="dashed") +
    ylim(c(min,max)) +
    labs(x = "Week of gestation at measurement",
         y = "zSBP",
         color = "Individual characteristics")
  
  # DBP plot
  pdbp <- ggplot(data_plot, aes(x = weeks, y = mean_d, color = grouping)) +
    geom_point() +
    geom_smooth() + # Trajectory with CI
    geom_hline(yintercept = 0, color="black", linetype="dashed") +
    ylim(c(min,max)) +
    labs(x = "Week of gestation at measurement",
         y = "zDBP")
  
  if(subgroup=="htn"){
    psbp <- psbp + scale_color_manual(labels=c("No HDP","Hypertension during pregnancy","Pre-eclampsia"), values=c("brown","lightskyblue3","dodgerblue3"))
    pdbp <- pdbp + scale_color_manual(labels=c("No HDP","Hypertension during pregnancy","Pre-eclampsia"), values=c("brown","lightskyblue3","dodgerblue3"))
  }
  # Arrange SBP and DBP plots side-by-side, with shared legend
  p <- ggarrange(psbp,pdbp,ncol=2,nrow=1,common.legend=TRUE, legend="top")
  return(p)
}

### Temperature exposure ----

#' Bar-plots of number of exposure days distribution
#'
#' This function produces bar-plots of the distribution of number of days during
#' which mothers were exposed to heat
#' 
#' @param data_heat A data frame containing binary exposure to heat (yes/no)
#' @param data_cold A data frame containing binary exposure to cold (yes/no)
#' @return Bar-plots of days of exposure distribution
make_count_expo <- function(data_heat, data_cold){
  
  p_heat = as.data.frame(rowSums(data_heat))
  p_cold = as.data.frame(rowSums(data_cold))
  colnames(p_heat) <- c("nb_Heat")
  colnames(p_cold) <- c("nb_Cold")
  col_hot = adjustcolor("firebrick1", alpha.f = 0.75)
  col_cold = adjustcolor("dodgerblue3", alpha.f = 0.75)
  
  
  data_plot <- data.frame(cbind(p_heat,p_cold))
  data_plot <- data_plot |>
    tidyr::pivot_longer(
      cols = c(nb_Heat,nb_Cold),
      names_to = "expo",
      values_to = "nb"
    ) |>
    dplyr::group_by(nb, expo) |>
    dplyr::mutate(
      sum = n(),
      expo = str_remove(expo, "nb_")
    ) |>
    dplyr::slice_head(n=1) |>
    dplyr::filter(!nb==0)
  
  p <- ggplot(data = data_plot, aes(x = nb, y=sum, fill=expo)) +
    geom_bar(stat = "identity", width = .5, position="dodge") +
    scale_fill_manual(values = c("Heat"=col_hot,"Cold"=col_cold)) +
    guides(fill = guide_legend(title="Exposure")) + 
    labs(x = "Number of days", y = "Number of measurements")
  
  return(p)
}

#' Warming strips plots for temperature exposure
#'
#' This function produces a warming strips plot for temperature exposure in the
#' 28 days preceding maternal CVD measurement
#' 
#' @param data_in A data frame containing data with missing data
#' @param coulmn A string indicating which temperature indicator to be plotted
#' @return A warming strips plot 
make_warming_strips <- function(data_in, column){
  
  # Adapted from:
  # https://dominicroye.github.io/blog/how-to-create-warming-stripes-in-r/index.html
  
  leg_title <- ifelse(column=="tmean_mean_4","Tmean",
                      ifelse(column=="tmax_mean_4","Tmax",
                             ifelse(column=="tmin_mean_4","Tmin","Tvar")))
  
  data_plot <- data_in[order(data_in$date_exam),] # Order df by time of measurement
  
  col_strip <- RColorBrewer::brewer.pal(11, "RdBu") # Colors settings
  
  maxmin <- range(data_plot[[column]]) # Range of exposure
  md <- mean(data_plot[[column]]) # Mean exposure
  max <- max(data_plot$tmax_mean_4) # Max exposure for plot ranges (tmax)
  min <- min(data_plot$tmin_mean_4) # Min exposure for plot ranges (tmin)
  
  p <- ggplot(data_plot, aes(date_exam, y = 1, fill = get(column))) +
    geom_tile() + # Strips
    scale_x_date( # X-axis: date of measurement
      date_breaks = "6 months", # Indicate the date every 6 months
      date_labels = "%m-%Y", # Only display the mm/yy date format
      expand = c(0, 0)) +
    scale_fill_gradientn(colors = rev(col_strip),                               # Warming strips
                         values = scales::rescale(c(maxmin[1], md, maxmin[2])),
                         limits = c(min,max), # Fixed range for all plots
                         breaks = c(0,10,20,30),
                         na.value = "gray80") +
    coord_cartesian(expand = FALSE) +
    guides(fill = guide_colourbar(title=leg_title)) + 
    theme_minimal() %+replace%
    theme(
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.x = element_text(vjust = 3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold")
    )
  p
  return(p)
  
}

#' Correlation plots for environmental exposure
#'
#' This function produces a correlation plot for each environmental exposure 
#' considered in our analyses and for each week of exposure (from 1 to 4). This
#' includes: temperature (max, min and mean), air pollutants (PM2.5, PM10, NO2,
#' O3) and RH.
#' 
#' @param data_in A data frame containing data with missing data
#' @param coulmn A string indicating which temperature indicator to be plotted
#' @return A warming strips plot 
make_cor_expo <- function(data_in){
  
  labs <- c(
    tmin_4w = "Tmin, 4th week",
    tmin_3w = "Tmin, 3rd week",
    tmin_2w = "Tmin, 2nd week",
    tmin_1w = "Tmin, 1st week",
    tmean_4w = "Tmean, 4th week",
    tmean_3w = "Tmean, 3rd week",
    tmean_2w = "Tmean, 2nd week",
    tmean_1w = "Tmean, 1st week",
    tmax_4w = "Tmax, 4th week",
    tmax_3w = "Tmax, 3rd week",
    tmax_2w = "Tmax, 2nd week",
    tmax_1w = "Tmax, 1st week",
    pm25_4w = expression("PM"[2.5]~", 4th week"),
    pm25_3w = expression("PM"[2.5]~", 3rd week"),
    pm25_2w = expression("PM"[2.5]~", 2nd week"),
    pm25_1w = expression("PM"[2.5]~", 1st week"),
    pm10_4w = expression("PM"[10]~", 4th week"),
    pm10_3w = expression("PM"[10]~", 3rd week"),
    pm10_2w = expression("PM"[10]~", 2nd week"),
    pm10_1w = expression("PM"[10]~", 1st week"),
    no2_4w = expression("NO"[2]~", 4th week"),
    no2_3w = expression("NO"[2]~", 3rd week"),
    no2_2w = expression("NO"[2]~", 2nd week"),
    no2_1w = expression("NO"[2]~", 1st week"),
    o3_4w = expression("O"[3]~", 4th week"),
    o3_3w = expression("O"[3]~", 3rd week"),
    o3_2w = expression("O"[3]~", 2nd week"),
    o3_1w = expression("O"[3]~", 1st week"),
    rhum_4w = "RH, 4th week",
    rhum_3w = "RH, 3rd week",
    rhum_2w = "RH, 2nd week",
    rhum_1w = "RH, 1st week"
  )
  
  data_plot <- data_in |>
    dplyr::select(
      starts_with(c("tmean_","tmax_","tmin_","pm25_","pm10_","no2_","o3_","rhum_")) & 
        !contains("pregnancy") & !ends_with("mean_4")
    ) 
  data_plot <- data_plot[,order(colnames(data_plot))]
  cor_data_plot <- cor(data_plot)
  
  # Plot the heatmap
  p <- ggcorrplot(cor_data_plot, 
                  type="upper", 
                  tl.cex = 8,
                  legend.title = "Correlation",
                  colors = c("steelblue3","cornsilk2","firebrick2"),
                  ggtheme = ggplot2::theme_gray) +
    scale_x_discrete(labels = labs) +
    scale_y_discrete(labels = labs) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

### Covariates ----

make_smooth <- function(data_in, exposure, outcome, df){
  p <- ggplot(data = data_in, aes_string(x = exposure, y = outcome)) +
    geom_point() + 
    geom_smooth(method = lm,
                formula = y ~ splines::ns(x,df))
  return(p)

}
