library(metasnf)

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
                    parsed_imputations$"sm_subc_v_qc"[[x + 1]],
                    "sm_subc_v",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"sm_cort_t_qc"[[x + 1]],
                    "sm_cort_t",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"sm_cort_sa_qc"[[x + 1]],
                    "sm_cort_sa",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"dm_all_wmnd_qc"[[x + 1]],
                    "dm_all_wmnd",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"rm_gord_cor_qc"[[x + 1]],
                    "rm_gord_cor",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"rm_subc_cor_qc"[[x + 1]],
                    "rm_subc_cor",
                    "N",
                    "continuous"
                ),
                list(
                    parsed_imputations$"p_friends"[[x + 1]],
                    "p_friends",
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
                    parsed_imputations$"sds"[[x + 1]],
                    "sds",
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
