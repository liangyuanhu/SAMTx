##########################################################################################
## Simulations
##
## Jiayi Ji
## Last updated: 12/02/2020
##
##########################################################################################
library(tidyverse)
library(BART)
source("code/json_parser.R")
source("code/sensitivity_analysis_algorithm.R")

# A simple illustrative simulation ------------------------------------
for (i in 1:100){
  set.seed(111+i)
  df_simple_X_truth = json_parser("data/SA_simple_X_truth.json") # read in data
  correction_result_simple_X_truth = sensitivity_analysis(
    df_simple_X_truth$measured_covariates,
    y = df_simple_X_truth$obs_y,
    A = df_simple_X_truth$assignment,
    alpha = df_simple_X_truth$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_X_truth, file = paste0("Rdata/correction_result_simple_X_truth_",i, ".Rdata"))
  
  df_simple_X_move_goal_posts = json_parser("data/SA_simple_X_move_goal_posts.json") # read in data
  correction_result_simple_X_move_goal_posts = sensitivity_analysis(
    df_simple_X_move_goal_posts$measured_covariates,
    y = df_simple_X_move_goal_posts$obs_y,
    A = df_simple_X_move_goal_posts$assignment,
    alpha = df_simple_X_move_goal_posts$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_X_move_goal_posts, file = paste0("Rdata/correction_result_simple_X_move_goal_posts_",i, ".Rdata"))
  
  df_simple_X_interval = json_parser("data/SA_simple_X_interval.json") # read in data
  correction_result_simple_X_interval = sensitivity_analysis(
    df_simple_X_interval$measured_covariates,
    y = df_simple_X_interval$obs_y,
    A = df_simple_X_interval$assignment,
    alpha = df_simple_X_interval$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_X_interval, file = paste0("Rdata/correction_result_simple_X_interval_",i, ".Rdata"))
  
  df_simple_scalar_truth = json_parser("data/SA_simple_scalar_truth.json") # read in data
  correction_result_simple_scalar_truth = sensitivity_analysis(
    df_simple_scalar_truth$measured_covariates,
    y = df_simple_scalar_truth$obs_y,
    A = df_simple_scalar_truth$assignment,
    alpha = df_simple_scalar_truth$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_truth, file = paste0("Rdata/correction_result_simple_scalar_truth_",i, ".Rdata"))
  
  df_simple_scalar_move_goal_posts = json_parser("data/SA_simple_scalar_move_goal_posts.json") # read in data
  correction_result_simple_scalar_move_goal_posts = sensitivity_analysis(
    df_simple_scalar_move_goal_posts$measured_covariates,
    y = df_simple_scalar_move_goal_posts$obs_y,
    A = df_simple_scalar_move_goal_posts$assignment,
    alpha = df_simple_scalar_move_goal_posts$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_move_goal_posts, file = paste0("Rdata/correction_result_simple_scalar_move_goal_posts_",i, ".Rdata"))
  
  df_simple_scalar_interval = json_parser("data/SA_simple_scalar_interval.json") # read in data
  correction_result_simple_scalar_interval = sensitivity_analysis(
    df_simple_scalar_interval$measured_covariates,
    y = df_simple_scalar_interval$obs_y,
    A = df_simple_scalar_interval$assignment,
    alpha = df_simple_scalar_interval$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_interval, file = paste0("Rdata/correction_result_simple_scalar_interval_",i, ".Rdata"))
  
  df_simple_scalar_interval_truncated_normal_highsd = json_parser("data/SA_simple_scalar_interval_truncated_normal_highsd.json") # read in data
  correction_result_simple_scalar_interval_truncated_normal_highsd = sensitivity_analysis(
    df_simple_scalar_interval_truncated_normal_highsd$measured_covariates,
    y = df_simple_scalar_interval_truncated_normal_highsd$obs_y,
    A = df_simple_scalar_interval_truncated_normal_highsd$assignment,
    alpha = df_simple_scalar_interval_truncated_normal_highsd$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_interval_truncated_normal_highsd, file = paste0("Rdata/correction_result_simple_scalar_interval_truncated_normal_highsd_",i, ".Rdata"))
  
  df_simple_scalar_interval_truncated_normal_lowsd = json_parser("data/SA_simple_scalar_interval_truncated_normal_lowsd.json") # read in data
  correction_result_simple_scalar_interval_truncated_normal_lowsd = sensitivity_analysis(
    df_simple_scalar_interval_truncated_normal_lowsd$measured_covariates,
    y = df_simple_scalar_interval_truncated_normal_lowsd$obs_y,
    A = df_simple_scalar_interval_truncated_normal_lowsd$assignment,
    alpha = df_simple_scalar_interval_truncated_normal_lowsd$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_interval_truncated_normal_lowsd, file = paste0("Rdata/correction_result_simple_scalar_interval_truncated_normal_lowsd_",i, ".Rdata"))
  
  df_simple_scalar_move_goal_posts_truncated_normal_highsd = json_parser("data/SA_simple_scalar_move_goal_posts_truncated_normal_highsd.json") # read in data
  correction_result_simple_scalar_move_goal_posts_truncated_normal_highsd = sensitivity_analysis(
    df_simple_scalar_move_goal_posts_truncated_normal_highsd$measured_covariates,
    y = df_simple_scalar_move_goal_posts_truncated_normal_highsd$obs_y,
    A = df_simple_scalar_move_goal_posts_truncated_normal_highsd$assignment,
    alpha = df_simple_scalar_move_goal_posts_truncated_normal_highsd$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_move_goal_posts_truncated_normal_highsd, file = paste0("Rdata/correction_result_simple_scalar_move_goal_posts_truncated_normal_highsd_",i, ".Rdata"))
  
  df_simple_scalar_move_goal_posts_truncated_normal_lowsd = json_parser("data/SA_simple_scalar_move_goal_posts_truncated_normal_lowsd.json")
  correction_result_simple_scalar_move_goal_posts_truncated_normal_lowsd = sensitivity_analysis(
    df_simple_scalar_move_goal_posts_truncated_normal_lowsd$measured_covariates,
    y = df_simple_scalar_move_goal_posts_truncated_normal_lowsd$obs_y,
    A = df_simple_scalar_move_goal_posts_truncated_normal_lowsd$assignment,
    alpha = df_simple_scalar_move_goal_posts_truncated_normal_lowsd$alpha,
    n_p = 10,
    sensitivity_correction = T
  )
  save(correction_result_simple_scalar_move_goal_posts_truncated_normal_lowsd, file = paste0("Rdata/correction_result_simple_scalar_move_goal_posts_truncated_normal_lowsd_",i, ".Rdata"))
}

# Contextualizing simulation in multiple treatment settings -----------
for (i in 1:100){
  set.seed(111+i)
  df = json_parser(data_path = "N_1500.json",sensitivity_path = "UMC_I.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_1500_UMC_I = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_1500_UMC_I, file = paste0("correction_result_N_1500_UMC_I_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_1500.json",sensitivity_path = "UMC_II.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_1500_UMC_II = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_1500_UMC_II, file = paste0("correction_result_N_1500_UMC_II_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_1500.json",sensitivity_path = "UMC_III.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_1500_UMC_III = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_1500_UMC_III, file = paste0("correction_result_N_1500_UMC_III_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_weak.json",sensitivity_path = "UMC_I.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_weak_UMC_I = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_weak_UMC_I, file = paste0("correction_result_N_10000_weak_UMC_I_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_weak.json",sensitivity_path = "UMC_II.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_weak_UMC_II = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_weak_UMC_II, file = paste0("correction_result_N_10000_weak_UMC_II_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_weak.json",sensitivity_path = "UMC_III.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_weak_UMC_III = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_weak_UMC_III, file = paste0("correction_result_N_10000_weak_UMC_III_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_moderate.json",sensitivity_path = "UMC_I.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_moderate_UMC_I = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_moderate_UMC_I, file = paste0("correction_result_N_10000_moderate_UMC_I_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_moderate.json",sensitivity_path = "UMC_II.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_moderate_UMC_II = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_moderate_UMC_II, file = paste0("correction_result_N_10000_moderate_UMC_II_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_moderate.json",sensitivity_path = "UMC_III.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_moderate_UMC_III = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_moderate_UMC_III, file = paste0("correction_result_N_10000_moderate_UMC_III_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_strong.json",sensitivity_path = "UMC_I.json", overlap_plot = F)
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_strong_UMC_I = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_strong_UMC_I, file = paste0("correction_result_N_10000_strong_UMC_I_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_strong.json",sensitivity_path = "UMC_II.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_strong_UMC_II = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_strong_UMC_II, file = paste0("correction_result_N_10000_strong_UMC_II_",i, ".Rdata"))
  
  set.seed(111+i)
  df = json_parser(data_path = "N_10000_strong.json",sensitivity_path = "UMC_III.json", overlap_plot = F) # read in data
  covariates = df$covariates
  measured_covariates = df$measured_covariates
  y = df$obs_y
  A = df$assignment
  alpha = df$alpha
  n = df$alpha_repeat
  correction_result_N_10000_strong_UMC_III = sensitivity_analysis(measured_covariates, y, A, alpha, n_p = 10, sensitivity_correction = T)
  save(correction_result_N_10000_strong_UMC_III, file = paste0("correction_result_N_10000_strong_UMC_III_",i, ".Rdata"))
}