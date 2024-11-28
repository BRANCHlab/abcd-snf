setwd("/hpf/projects/awheeler/Users/pvelayudhan/subtyping/research")

library(readr)
library(abcdutils)
library(here)
library(metasnf)
library(rlist)

proc_path <- path_maker(here(paste0("data/abcd/results/processed")))


# CHECKPOINT
matched_unij_dl_a_s1 <- list.load(proc_path("2024_11_28_matched_unij_dl_a_s1.rds"))

print("hi")



#matched_unij_dl_a_p1 <- list.load(proc_path("2024_11_28_matched_unij_dl_a_p1.rds"))
#matched_unij_ol_a_s1 <- list.load(proc_path("2024_11_28_matched_unij_ol_a_s1.rds"))
#matched_unij_ol_a_p1 <- list.load(proc_path("2024_11_28_matched_unij_ol_a_p1.rds"))
#matched_unij_dl_b_s1 <- list.load(proc_path("2024_11_28_matched_unij_dl_b_s1.rds"))
#matched_unij_dl_b_p1 <- list.load(proc_path("2024_11_28_matched_unij_dl_b_p1.rds"))
#matched_unij_ol_b_s1 <- list.load(proc_path("2024_11_28_matched_unij_ol_b_s1.rds"))
#matched_unij_ol_b_p1 <- list.load(proc_path("2024_11_28_matched_unij_ol_b_p1.rds"))
#matched_unij_dl_c_s1 <- list.load(proc_path("2024_11_28_matched_unij_dl_c_s1.rds"))
#matched_unij_dl_c_p1 <- list.load(proc_path("2024_11_28_matched_unij_dl_c_p1.rds"))
#matched_unij_ol_c_s1 <- list.load(proc_path("2024_11_28_matched_unij_ol_c_s1.rds"))
#matched_unij_ol_c_p1 <- list.load(proc_path("2024_11_28_matched_unij_ol_c_p1.rds"))
#matched_unij_dl_d_s1 <- list.load(proc_path("2024_11_28_matched_unij_dl_d_s1.rds"))
#matched_unij_dl_d_p1 <- list.load(proc_path("2024_11_28_matched_unij_dl_d_p1.rds"))
#matched_unij_ol_d_s1 <- list.load(proc_path("2024_11_28_matched_unij_ol_d_s1.rds"))
#matched_unij_ol_d_p1 <- list.load(proc_path("2024_11_28_matched_unij_ol_d_p1.rds"))
#
#my_distance_metrics <- generate_distance_metrics_list(
#    continuous_distances = list(
#        "standard_norm_euclidean" = sn_euclidean_distance
#    ),
#    discrete_distances = list(
#        "standard_norm_euclidean" = sn_euclidean_distance
#    ),
#    ordinal_distances = list(
#        "standard_norm_euclidean" = sn_euclidean_distance
#    ),
#)
#
#matched_settings_matrix <- read_csv(proc_path("2024_11_28_matched_settings_matrix.csv"))
#
#unij_sm_a_s1 <- batch_snf(matched_unij_dl_a_s1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#
#write_csv(unij_sm_a_s1, proc_path("unij_sm_a_s1.csv", TRUE))
#unij_sm_a_p1 <- batch_snf(matched_unij_dl_a_p1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_a_p1, proc_path("unij_sm_a_p1.csv", TRUE))
#unij_sm_b_s1 <- batch_snf(matched_unij_dl_b_s1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_b_s1, proc_path("unij_sm_b_s1.csv", TRUE))
#unij_sm_b_p1 <- batch_snf(matched_unij_dl_b_p1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_b_p1, proc_path("unij_sm_b_p1.csv", TRUE))
#unij_sm_c_s1 <- batch_snf(matched_unij_dl_c_s1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_c_s1, proc_path("unij_sm_c_s1.csv", TRUE))
#unij_sm_c_p1 <- batch_snf(matched_unij_dl_c_p1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_c_p1, proc_path("unij_sm_c_p1.csv", TRUE))
#unij_sm_d_s1 <- batch_snf(matched_unij_dl_d_s1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_d_s1, proc_path("unij_sm_d_s1.csv", TRUE))
#unij_sm_d_p1 <- batch_snf(matched_unij_dl_d_p1, matched_settings_matrix, processes = "max", distance_metrics_list = my_distance_metrics)
#write_csv(unij_sm_d_p1, proc_path("unij_sm_d_p1.csv", TRUE))
#
#unij_sm_a_s1 <- read_csv(proc_path("2024_11_28_unij_sm_a_s1.csv"))
#unij_sm_a_p1 <- read_csv(proc_path("2024_11_28_unij_sm_a_p1.csv"))
#unij_sm_b_s1 <- read_csv(proc_path("2024_11_28_unij_sm_b_s1.csv"))
#unij_sm_b_p1 <- read_csv(proc_path("2024_11_28_unij_sm_b_p1.csv"))
#unij_sm_c_s1 <- read_csv(proc_path("2024_11_28_unij_sm_c_s1.csv"))
#unij_sm_c_p1 <- read_csv(proc_path("2024_11_28_unij_sm_c_p1.csv"))
#unij_sm_d_s1 <- read_csv(proc_path("2024_11_28_unij_sm_d_s1.csv"))
#unij_sm_d_p1 <- read_csv(proc_path("2024_11_28_unij_sm_d_p1.csv"))
#
#matched_unij_esm_a_s1 <- extend_solutions(unij_sm_a_s1, data_list = matched_unij_dl_a_s1, target_list = matched_unij_ol_a_s1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_a_s1, proc_path("matched_unij_esm_a_s1.csv", TRUE))
#matched_unij_esm_a_p1 <- extend_solutions(unij_sm_a_p1, data_list = matched_unij_dl_a_p1, target_list = matched_unij_ol_a_p1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_a_p1, proc_path("matched_unij_esm_a_p1.csv", TRUE))
#matched_unij_esm_b_s1 <- extend_solutions(unij_sm_b_s1, data_list = matched_unij_dl_b_s1, target_list = matched_unij_ol_b_s1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_b_s1, proc_path("matched_unij_esm_b_s1.csv", TRUE))
#matched_unij_esm_b_p1 <- extend_solutions(unij_sm_b_p1, data_list = matched_unij_dl_b_p1, target_list = matched_unij_ol_b_p1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_b_p1, proc_path("matched_unij_esm_b_p1.csv", TRUE))
#matched_unij_esm_c_s1 <- extend_solutions(unij_sm_c_s1, data_list = matched_unij_dl_c_s1, target_list = matched_unij_ol_c_s1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_c_s1, proc_path("matched_unij_esm_c_s1.csv", TRUE))
#matched_unij_esm_c_p1 <- extend_solutions(unij_sm_c_p1, data_list = matched_unij_dl_c_p1, target_list = matched_unij_ol_c_p1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_c_p1, proc_path("matched_unij_esm_c_p1.csv", TRUE))
#matched_unij_esm_d_s1 <- extend_solutions(unij_sm_d_s1, data_list = matched_unij_dl_d_s1, target_list = matched_unij_ol_d_s1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_d_s1, proc_path("matched_unij_esm_d_s1.csv", TRUE))
#matched_unij_esm_d_p1 <- extend_solutions(unij_sm_d_p1, data_list = matched_unij_dl_d_p1, target_list = matched_unij_ol_d_p1, min_pval = 1e-10, processes = "max")
#write_csv(matched_unij_esm_d_p1, proc_path("matched_unij_esm_d_p1.csv", TRUE))
