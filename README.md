# Impacts of ambient temperature on pregnant women's cardiovascular function and variations related to fetal sex.

This is the code supporting the analyses presented in DOI: 10.1016/j.envres.2026.124742.

Author: Chloé Masdoumier

Date: 2025-10-06

## Abstract
**Background** Rising global temperatures affect several aspects of health, yet the effects on maternal cardiovascular physiology and potential windows of vulnerability remain largely unexplored. **Methods** We investigated subacute (0-28 days) heat and cold exposure, estimated using a high-resolution spatiotemporal temperature model, in relation to heart rate, hematocrit levels, and repeated systolic and diastolic blood pressure measurements (transformed into gestational age-specific Z-scores, zSBP, zDBP) among 1,854 pregnant women from the French EDEN cohort. Distributed lag non-linear models were used to model both cumulative and lagged effects of heat (95th percentile; 20°C compared to 11°C) and cold (5th percentile; 4°C compared to 11°C). Models accounted for air pollution, vegetation and humidity and were stratified by gestational age and fetal sex. **Results** Heat was associated with a sharp decrease in zSBP for up to 10 days after exposure (-0.14 SD [-0.20; -0.08] 95%-CI). Cold was associated with higher hematocrit (0.28% [0.04; 0.51]) and a modest increase in zSBP (0.06 SD [0.02; 0.09]) over 2-6 days after exposure, with zSBP associations observed only in prengancies with female fetuses. Subacute effects of heat were not modified by gestational age, nor by air pollution and vegetation. **Conclusion** This study underscores the role of short-term heat exposure in shaping maternal cardiovascular responses during pregnancy, while suggesting mor elimited effects of cold exposure, with variations by fetal sex. These results provide insgihts into the biological plausibility of temperature extremes effects on pregnancy complications.

## Code architecture
Due to ethical and legal restrictions, some parts of the code were hidden to respect participants' data privacy. 

-  *github_targets.R* : R script in which the pipeline of the analysis is defined, and the different targets are specified.
  
-  *github_preparation_wide.R* : R script of data management for wide-format data frames (prefix: prep_). Some functions were hidden.
  
-  *github_preparation_long.R* : R script of data management for long-format data frames (prefix: prep_). Some functions were hidden.
  
-  *github_helpers.R* : R script of help functions used for the analysis (prefix: help_). Some functions were hidden.
  
-  *github_stat_models.R* : R script of statiscal analyses functions, including DLNM encoding functions (prefix: mod_ and run_).
  
-  *github_tables.R* : R script of functions to produced summary tables (prefix: make_tbl).
  
-  *github_figures.R* : R script of functions to produced graphics (prefix: make_fig).

-  *github_figure_dlm.R* : R script of functions to produced DLNM-related graphics (local use, outside the targets pipeline).
  
-  *github_figure_S10.R* : R script of functions to produced supplementary figure 10 (local use, outside the targets pipeline).

## Contact
For any questions or further information, please contact Chloé Masdoumier at chloe.masdoumier@univ-grenoble-alpes.fr.
