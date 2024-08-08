library(metasnf)

###############################################################################
# Functions to parse imputed data
###############################################################################
extract_imputed <- function(original_df, imputed_df, partition) {
    return(imputed_df[imputed_df$".imp" == partition, colnames(original_df)])
}

parse_imputations <- function(original_df_list, complete_imputed_df, imps) {
    parsed_imputations <- original_df_list |>
        lapply(
            function(x) {
                imputations <- list()
                for (i in seq_len(imps)) {
                    imputations[[i]] <- extract_imputed(
                        x,
                        complete_imputed_df,
                        i - 1
                    )
                }
                names(imputations) <- paste0("imp_", seq_len(imps) - 1)
                return(imputations)
            }
        )
    names(parsed_imputations) <- gsub("_mtbi$", "", names(parsed_imputations))
    names(parsed_imputations) <- gsub("_uninj$", "", names(parsed_imputations))
    return(parsed_imputations)
}

build_dls <- function(parsed_imputations) {
    data_lists <- lapply(
        seq_len(5),
        function(x) {
            generate_data_list(
                list(
                    parsed_imputations$"as_mem_daze"[[x + 1]],
                    "as_mem_daze",
                    "AS",
                    "discrete"
                ),
                list(
                    parsed_imputations$"as_mechanism"[[x + 1]],
                    "as_mechanism",
                    "AS",
                    "categorical"
                ),
                list(
                    parsed_imputations$"as_loc"[[x + 1]],
                    "as_loc",
                    "AS",
                    "discrete"
                ),
                list(
                    parsed_imputations$"d_interview_age"[[x + 1]],
                    "d_age",
                    "D",
                    "discrete"
                ),
                list(
                    parsed_imputations$"d_mtbi_age"[[x + 1]],
                    "d_mtbi_age",
                    "D",
                    "discrete"
                ),
                list(
                    parsed_imputations$"d_sex"[[x + 1]],
                    "d_sex",
                    "D",
                    "discrete"
                ),
                list(
                    parsed_imputations$"d_income"[[x + 1]],
                    "d_income",
                    "D",
                    "ordinal"
                ),
                list(
                    parsed_imputations$"d_race"[[x + 1]],
                    "d_race",
                    "D",
                    "categorical"
                ),
                list(
                    parsed_imputations$"d_pubertal_status"[[x + 1]],
                    "d_pubertal",
                    "D",
                    "discrete"
                ),
                list(
                    parsed_imputations$"mh_headaches"[[x + 1]],
                    "mh_headaches",
                    "MH",
                    "discrete"
                ),
                list(
                    parsed_imputations$"mh_mtbi_count"[[x + 1]],
                    "mh_mtbi_count",
                    "MH",
                    "discrete"
                ),
                list(
                    parsed_imputations$"sm_subc_v"[[x + 1]],
                    "sm_subc_v",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"sm_cort_t"[[x + 1]],
                    "sm_cort_t",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"sm_cort_sa"[[x + 1]],
                    "sm_cort_sa",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"dm_all_wmnd"[[x + 1]],
                    "dm_all_wmnd",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"rm_gord_cor"[[x + 1]],
                    "rm_gord_cor",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"rm_subc_cor"[[x + 1]],
                    "rm_subc_cor",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"p_loneliness"[[x + 1]],
                    "p_loneliness",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_screen_time"[[x + 1]],
                    "p_screen_time",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_sports"[[x + 1]],
                    "p_sports",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_exercise"[[x + 1]],
                    "p_exercise",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_prosocial"[[x + 1]],
                    "p_prosocial",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_fam_fn"[[x + 1]],
                    "p_fam_fn",
                    "P",
                    "discrete"
                ),
                list(
                    parsed_imputations$"p_parent_psych"[[x + 1]],
                    "p_parent_psych",
                    "P",
                    "discrete"
                ),
                uid = "subjectkey"
            )
        }
    )
    names(data_lists) <- paste0("imp_", seq_len(5))
    return(data_lists)
}

build_ols <- function(parsed_imputations) {
    oom_lists <- lapply(
        seq_len(5),
        function(x) {
            generate_data_list(
                list(
                    parsed_imputations$"cbcl"[[x + 1]],
                    "cbcl",
                    "B",
                    "discrete"
                ),
                list(
                    parsed_imputations$"sds_total_probs"[[x + 1]],
                    "sds_total_probs",
                    "B",
                    "discrete"
                ),
                uid = "subjectkey"
            )
        }
    )
    names(oom_lists) <- paste0("imp_", seq_len(5))
    return(oom_lists)
}
