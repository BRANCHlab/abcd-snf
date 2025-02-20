library(metasnf)

build_dls <- function(parsed_imputations) {
    data_lists <- lapply(
        seq_len(5),
        function(x) {
            data_list(
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

build_unij_dls <- function(parsed_imputations) {
    data_lists <- lapply(
        seq_len(5),
        function(x) {
            data_list(
                list(
                    parsed_imputations$"d_interview_age"[[x + 1]],
                    "d_age",
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
            data_list(
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

# Comparison based purely on effect size
compare_fn_a <- function(d, sig, thresh = 0.2) {
    order <- case_when(
        d >= thresh ~ ">",
        d <= -thresh ~ "<",
        abs(d) < thresh ~ "=",
        TRUE ~ NA
    )
    return(order)
}

# Comparison based on effect size and significance
compare_fn_b <- function(d, sig, thresh = 0.2) {
    order <- case_when(
        d >= thresh ~ ">",
        d <= -thresh ~ "<",
        d > 0 & sig ~ ">",
        d < 0 & sig ~ "<",
        abs(d) < thresh & !sig ~ "=",
        TRUE ~ NA
    )
}

generalizable_features <- function(tops_df, comparison_fn, t = 0.2) {
    # Remove unnecessary columns
    if ("rank" %in% colnames(tops_df)) {
        tops_df <- dplyr::select(tops_df, -rank)
    }
    if ("summary" %in% colnames(tops_df)) {
        tops_df <- dplyr::select(tops_df, -summary)
    }
    # Calculate significance for every row
    tops_df <- tops_df |>
        mutate(
            c12_sig = as.logical(significant_es(es, c12_l, c12_u)),
            c13_sig = as.logical(significant_es(es, c13_l, c13_u)),
            c23_sig = as.logical(significant_es(es, c23_l, c23_u))
        )
    # Create d-values for every row
    tops_df <- tops_df |>
        dplyr::mutate(
            c12_d = dplyr::case_when(
                es == "d" ~ c12_es,
                es == "or" ~ suppressWarnings(log(c12_es)) * sqrt(3) / pi
            ),
            c13_d = dplyr::case_when(
                es == "d" ~ c13_es,
                es == "or" ~ suppressWarnings(log(c13_es)) * sqrt(3) / pi
            ),
            c23_d = dplyr::case_when(
                es == "d" ~ c23_es,
                es == "or" ~ suppressWarnings(log(c23_es)) * sqrt(3) / pi
            )
        )
    # Calculate orderings
    tops_df <- tops_df |>
        dplyr::mutate(
            c12_order = comparison_fn(c12_d, c12_sig, thresh = t),
            c13_order = comparison_fn(c13_d, c13_sig, thresh = t),
            c23_order = comparison_fn(c23_d, c23_sig, thresh = t)
        )
    # Now find conistency across D, V, F for each feature
    features <- unique(tops_df$"feature")
    gen_df <- data.frame()
    for (ft in features) {
        feature_df <- dplyr::filter(tops_df, feature == ft)
        c12 <- feature_df$"c12_order"
        c13 <- feature_df$"c13_order"
        c23 <- feature_df$"c23_order"
        if (any(is.na(c12)) | length(unique(c12)) > 1) {
            c12o <- NA
        } else {
            c12o <- unique(c12)
        }
        if (any(is.na(c13)) | length(unique(c13)) > 1) {
            c13o <- NA
        } else {
            c13o <- unique(c13)
        }
        if (any(is.na(c23)) | length(unique(c23)) > 1) {
            c23o <- NA
        } else {
            c23o <- unique(c23)
        }
        gen_df <- rbind(
            gen_df,
            data.frame(
                feature = ft,
                c1v2 = c12o,
                c1v3 = c13o,
                c2v3 = c23o
            )
        )
    }
    return(gen_df)
}

# FUNCTION: Calculate if an ES is significant
significant_es <- function(type, l, u) {
    thresh <- as.numeric(type %in% c("or", "OR"))
    return(as.numeric(thresh < l | thresh > u))
}

# FUNCTION: Format topologies
format_tops <- function(tops) {
    tops <- data.frame(tops) |> dplyr::mutate(
        c12_sig = significant_es(es, c12_l, c12_u),
        c13_sig = significant_es(es, c13_l, c13_u),
        c23_sig = significant_es(es, c23_l, c23_u)
    )
    # Strips whitespace by converting number columns to numeric format
    tops <- metasnf:::numcol_to_numeric(tops)
    tops <- tops |> dplyr::mutate(
        "C1 vs. C2" = paste0(c12_es, " [", c12_l, ", ", c12_u, "]"),
        "C1 vs. C3" = paste0(c13_es, " [", c13_l, ", ", c13_u, "]"),
        "C2 vs. C3" = paste0(c23_es, " [", c23_l, ", ", c23_u, "]"),
        .keep = "unused"
    )
    tops <- tops |> dplyr::mutate(
        "C1 vs. C2" = ifelse(c12_sig == 1, paste0("textbf{", `C1 vs. C2`, "}"), `C1 vs. C2`),
        "C1 vs. C3" = ifelse(c13_sig == 1, paste0("textbf{", `C1 vs. C3`, "}"), `C1 vs. C3`),
        "C2 vs. C3" = ifelse(c23_sig == 1, paste0("textbf{", `C2 vs. C3`, "}"), `C2 vs. C3`),
        .keep = "unused"
    )
    # Remove scientific notation and trailing zeros
    tops <- format(data.frame(tops), scientific = FALSE, drop0trailing = TRUE)
    colnames(tops) <- c("Feature", "Summary", "C1", "C2", "C3", "ES", "C1 vs. C2", "C1 vs. C3", "C2 vs. C3")
    return(tops)
}

tstat <- function(d, n1, n2) {
    t <- d * sqrt((n1 * n2) / (n1 + n2))
    return(t)
}
pooled_sd <- function(n1, s1, n2, s2) {
    numerator <- ((n1 - 1) * (s1^2)) + ((n2 - 1) * (s2^2))
    denominator <- n1 + n2 - 2
    s <- sqrt(numerator / denominator)
    return(s)
}
cohens_d <- function(mean1, mean2, pooled_sd) {
    d <- (mean1 - mean2) / pooled_sd
    return(d)
}

calc_des <- function(x1, x2) {
    n1 <- length(x1)
    n2 <- length(x2)
    s1 <- sd(x1)
    s2 <- sd(x2)
    s <- pooled_sd(n1, s1, n2, s2)
    mean1 <- mean(x1)
    mean2 <- mean(x2)
    d <- cohens_d(mean1, mean2, s)
    d_full <- compute.es::des(
        d,
        n1,
        n2,
        verbose = FALSE
    )
    return(
        list(
            "es" = d_full$"d",
            "l" = d_full$"l.d",
            "u" = d_full$"u.d"
        )
    )
}

calc_propes <- function(x1, x2) {
    d_full <- compute.es::propes(
        mean(x1),
        mean(x2),
        length(x1),
        length(x2),
        verbose = FALSE
    )
    return(
        list(
            "es" = d_full$"OR",
            "l" = d_full$"l.or",
            "u" = d_full$"u.or"
        )
    )
}

dummy_non_numeric <- function(df) {
    to_dummy <- colnames(df)[!sapply(df, is.numeric)]
    if (length(to_dummy) == 0) {
        return(df)
    }
    dummied_df <- dummy_cols(
        df,
        select_columns = to_dummy,
        remove_selected_columns = TRUE
    )
    return(dummied_df)
}

is_binary <- function(x) {
    return(isTRUE(all.equal(sort(unique(x)), c(0, 1))))
}

topologizer <- function(df) {
    df <- data.frame(df)
    df <- metasnf:::numcol_to_numeric(df)
    df <- dummy_non_numeric(dplyr::select(df, -"uid"))
    summary_df <- data.frame(
        "feature" = as.character(),
        "type" = as.character(),
        "summary" = as.character(),
        "c1_mean" = as.numeric(),
        "c2_mean" = as.numeric(),
        "c3_mean" = as.numeric(),
        "c1_c2_es" = as.numeric(),
        "c1_c2_l" = as.numeric(),
        "c1_c2_u" = as.numeric(),
        "c1_c3_es" = as.numeric(),
        "c1_c3_l" = as.numeric(),
        "c1_c3_u" = as.numeric(),
        "c2_c3_es" = as.numeric(),
        "c2_c3_l" = as.numeric(),
        "c2_c3_u" = as.numeric()
    )
    c1 <- df[df$"cluster" == 1, ]
    c2 <- df[df$"cluster" == 2, ]
    c3 <- df[df$"cluster" == 3, ]
    features <- df |>
        dplyr::select(-"cluster") |>
        colnames()
    ###########################################################################
    # Check if features are binary (unique values are 0 and 1) or not
    ###########################################################################
    for (feature in features) {
        f1 <- c1[, feature]
        f2 <- c2[, feature]
        f3 <- c3[, feature]
        means <- c("1" = mean(f1), "2" = mean(f2), "3" = mean(f3))
        full_summary <- paste(
            names(sort(means, decreasing = TRUE)),
            collapse = " > "
        )
        if (is_binary(df[, feature])) {
            sum_1_v_2 <- calc_propes(f1, f2)
            sum_1_v_3 <- calc_propes(f1, f3)
            sum_2_v_3 <- calc_propes(f2, f3)
            type <- "or"
        } else {
            sum_1_v_2 <- calc_des(f1, f2)
            sum_1_v_3 <- calc_des(f1, f3)
            sum_2_v_3 <- calc_des(f2, f3)
            type <- "d"
        }
        summary_df <- rbind(
            summary_df,
            data.frame(
                "feature" = feature,
                "summary" = full_summary,
                "c1" = signif(mean(f1), 3),
                "c2" = signif(mean(f2), 3),
                "c3" = signif(mean(f3), 3),
                "es" = type,
                "c12_es" = sum_1_v_2$"es",
                "c12_l" = sum_1_v_2$"l",
                "c12_u" = sum_1_v_2$"u",
                "c13_es" = sum_1_v_3$"es",
                "c13_l" = sum_1_v_3$"l",
                "c13_u" = sum_1_v_3$"u",
                "c23_es" = sum_2_v_3$"es",
                "c23_l" = sum_2_v_3$"l",
                "c23_u" = sum_2_v_3$"u"
            )
        )
    }
    return(summary_df)
}

png_save <- function(img, path, w = 1250, h = 1000, r = 125) {
    grDevices::png(
        filename = path,
        width = w,
        height = h,
        res = r
    )
    print(img)
    grDevices::dev.off()
}

