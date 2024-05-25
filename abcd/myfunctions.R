title_case <- function(string) {
    words <- strsplit(string, "_")[[1]]
    words <- sapply(
        words,
        function(x) {
            paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
        }
    )
    result <- paste(words, collapse = " ")
    result <- gsub("Mtbi", "mTBI", result)
    result <- gsub("Loc", "LOC", result)
    result <- gsub("Cbcl", "CBCL", result)
    result <- gsub("Sds", "SDS", result)
    result <- gsub("Wm", "WM", result)
    result <- gsub("Ndi", "NDI", result)
    result <- gsub("Fmri", "fMRI", result)
    result <- gsub("Cortical Sa", "Cortical SA (mm2)", result)
    result <- gsub("Thickness", "Thickness (mm3)", result)
    result <- gsub("Volume", "Volume (mm3)", result)
    return(result)
}

all_plots <- function(plot_list, plotname) {
    pw <- patchwork::wrap_plots(plot_list)
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_all_plots.png"),
        width = 30,
        height = 30,
        dpi = 72
    )
}

outcome_plots <- function(plot_list, plotname) {
    pw <- list(
        plot_list$"cbcl_anxdep",
        plot_list$"cbcl_withdep",
        plot_list$"cbcl_somatic",
        plot_list$"cbcl_social",
        plot_list$"cbcl_thought",
        plot_list$"cbcl_attention",
        plot_list$"cbcl_rulebreak",
        plot_list$"cbcl_aggressive",
        plot_list$"sds_sleep"
    ) |> patchwork::wrap_plots()
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_outcome_plots.png"),
        width = 15,
        height = 15
    )
}

demographic_plots <- function(plot_list, plotname) {
    plot_list <- list(
        plot_list$"age",
        plot_list$"mtbi_age",
        plot_list$"household_income",
        plot_list$"sex",
        plot_list$"pubertal_status",
        plot_list$"race"
    )
    plot_list <- Filter(Negate(is.null), plot_list)
    pw <- patchwork::wrap_plots(plot_list)
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_demographic_plots.png"),
        width = 15,
        height = 10
    )
}

neuroimaging_plots <- function(plot_list, plotname) {
    layout <- c(
        area(t = 1, l = 1, b = 2, r = 2),
        area(t = 1, l = 3, b = 2, r = 4),
        area(t = 1, l = 5, b = 2, r = 6),
        area(t = 3, l = 2, b = 4, r = 3),
        area(t = 3, l = 4, b = 4, r = 5),
        area(t = 5, l = 2, b = 6, r = 3),
        area(t = 5, l = 4, b = 6, r = 5)
    )
    pw <- plot_list$"brain_volume" +
        plot_list$"cortical_sa" +
        plot_list$"cortical_thickness" +
        plot_list$"major_wm_ndi" +
        plot_list$"pericortical_wm_ndi" +
        plot_list$"fmri_cort_cors" +
        plot_list$"fmri_cort_subcort_cors" +
        plot_layout(design = layout)
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_neuroimaging_plots.png"),
        width = 15,
        height = 15
    )
}

acute_symptom_plots <- function(plot_list, plotname) {
    pw <- list(
        plot_list$"mtbi_mem_daze",
        plot_list$"mtbi_loc",
        plot_list$"mtbi_mechanism"
    )
    filtered_list <- Filter(Negate(is.null), pw)
    if (length(filtered_list) >= 1) {
        as_plots <- patchwork::wrap_plots(filtered_list)
        ggplot2::ggsave(
            plot = as_plots,
            filename = paste0(plotname, "_as_plots.png"),
            width = 20,
            height = 7
        )
    } else {
        print("No acute symptom plots in list.")
    }
}

parent_psych_plots <- function(plot_list, plotname) {
    pw <- list(
        plot_list$"parent_perstr",
        plot_list$"parent_anxdep",
        plot_list$"parent_withdrawn",
        plot_list$"parent_somatic",
        plot_list$"parent_thought",
        plot_list$"parent_attention",
        plot_list$"parent_aggressive",
        plot_list$"parent_intrusive",
        plot_list$"parent_depress",
        plot_list$"parent_anxdisord",
        plot_list$"parent_avoidant",
        plot_list$"parent_antisocial",
        plot_list$"parent_hyperactive"
    ) |> patchwork::wrap_plots()
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_parent_psych_plots.png"),
        width = 20,
        height = 13
    )
}

family_env_plots <- function(plot_list, plotname) {
    pw <- list(
        plot_list$"family_fight",
        plot_list$"family_angry",
        plot_list$"family_throw",
        plot_list$"family_temper",
        plot_list$"family_criticize",
        plot_list$"family_hit",
        plot_list$"family_peaceful",
        plot_list$"family_outdo",
        plot_list$"family_yell"
    ) |> patchwork::wrap_plots()
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_family_env_plots.png"),
        width = 15,
        height = 15
    )
}

prosocial_plots <- function(plot_list, plotname) {
    layout <- c(
        area(t = 1, l = 1, b = 2, r = 2),
        area(t = 1, l = 3, b = 2, r = 4),
        area(t = 1, l = 5, b = 2, r = 6),
        area(t = 1, l = 7, b = 2, r = 8),
        area(t = 3, l = 2, b = 4, r = 3),
        area(t = 3, l = 4, b = 4, r = 5),
        area(t = 3, l = 6, b = 4, r = 7)
    )
    pw <-
        plot_list$"same_sex_friend" +
        plot_list$"opp_sex_friend" +
        plot_list$"same_sex_close_friend" +
        plot_list$"opp_sex_close_friend" +
        plot_list$"prosocial_considerate" +
        plot_list$"prosocial_helps_hurt" +
        plot_list$"prosocial_helpful" +
        plot_layout(design = layout)
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_prosocial_plots.png"),
        width = 15,
        height = 8
    )
}

healthy_habits_plots <- function(plot_list, plotname) {
    pw <-
        plot_list$"weekday_screentime" +
        plot_list$"school_programs" +
        plot_list$"non_school_programs" +
        plot_list$"programs_this_year" +
        plot_list$"private_instruction" +
        plot_list$"exercise_time"
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_healthy_habits_plots.png"),
        width = 14,
        height = 8
    )
}

medical_history_plots <- function(plot_list, plotname) {
    plot_list <- list(
        plot_list$"headache_history",
        plot_list$"previous_mtbis"
    )
    plot_list <- Filter(Negate(is.null), plot_list)
    pw <- patchwork::wrap_plots(plot_list)
    ggplot2::ggsave(
        plot = pw,
        filename = paste0(plotname, "_medical_history_plots.png"),
        width = 6 * length(plot_list),
        height = 6
    )
}

characterize_solution <- function(solution = NULL,
                                  cluster_df = NULL,
                                  data_list,
                                  plotname,
                                  individual_plots = TRUE,
                                  return_plots = TRUE,
                                  group_plots = TRUE) {
    # Formatting
    solution <- data.frame(solution)
    # Generating the required full dataframe
    if (is.null(cluster_df)) {
        cluster_df <- metasnf::get_cluster_df(solution)
    } else {
        cluster_df <- data.frame(cluster_df)
    }
    data_df <- metasnf::collapse_dl(data_list)
    full_data <- dplyr::inner_join(cluster_df, data_df, by = "subjectkey")
    full_data$"cluster" <- factor(full_data$"cluster")
    # Re-coding variables
    full_data$"sex"[full_data$"sex" == 0] <- "F"
    full_data$"sex"[full_data$"sex" == 1] <- "M"
    if (return_plots == FALSE) {
        return(full_data)
    }
    # Identifying features to plot (first cols are cluster and subjectkey)
    features <- colnames(full_data)[3:length(colnames(full_data))]
    # Generating plot for every variable
    plot_list <- list()
    for (i in seq_along(features)) {
        feature <- features[[i]]
        feature_col <- full_data[, feature]
        if (is.numeric(feature_col) && length(unique(feature_col)) > 2) {
            plot <- jitter_plot(full_data, feature)
            plot_width <- 4
        } else {
            plot <- bar_plot(full_data, feature)
            plot_width <- 5.5
        }
        if (individual_plots) {
            print(
                paste0(
                    i, "/", length(features), ": ",
                    "Plotting ",
                    feature
                )
            )
            ggsave(
                plot = plot,
                filename = paste0(plotname, "_", feature, ".png"),
                width = plot_width,
                height = 4
            )
        }
        plot_list[[i]] <- plot
        names(plot_list)[[i]] <- feature
    }
    # Generating group plots
    print("Plotting all-plot grid")
    all_plots(plot_list, plotname)
    if (group_plots) {
        print("Plotting outcome plots")
        outcome_plots(plot_list, plotname)
        print("Plotting demographic plots")
        demographic_plots(plot_list, plotname)
        print("Plotting neuroimaging plots")
        neuroimaging_plots(plot_list, plotname)
        print("Plotting acute symptom plots")
        acute_symptom_plots(plot_list, plotname)
        print("Plotting parent psych plots")
        parent_psych_plots(plot_list, plotname)
        print("Plotting family env plots")
        family_env_plots(plot_list, plotname)
        print("Plotting prosocial plots")
        prosocial_plots(plot_list, plotname)
        print("Plotting healthy habits plots")
        healthy_habits_plots(plot_list, plotname)
        print("Plotting medical history plots")
        medical_history_plots(plot_list, plotname)
    }
    print("Done")
    return(plot_list)
}

my_manhattan_plot <- function(solutions_matrix,
                              aris_order = NULL,
                              split_vector = NULL,
                              sorted_labels = NULL,
                              mcs = NULL,
                              data_list,
                              target_list,
                              colours) {
    if (is.null(sorted_labels)) {
        # Managing the MC labels
        reordered_labels <- label_splits(split_vector, 2000)
        names(aris_order) <- reordered_labels
        sorted_labels <- sort(aris_order)
    }
    # Reformatting the data lists to pull names
    data_list_renamed <- data_list |> lapply(
        function(x) {
            x$"domain" <- paste0("I-", x$"domain")
            return(x)
        }
    )
    target_list_renamed <- target_list |> lapply(
        function(x) {
            x$"domain" <- paste0("O-", x$"domain")
            return(x)
        }
    )
    data_list <- c(data_list_renamed, target_list_renamed)
    # Extract p-values
    target_pvals <- pval_select(solutions_matrix)
    target_pvals$"mc_label" <- names(sorted_labels)
    target_pvals <- dplyr::select(
        target_pvals,
        -dplyr::starts_with(c("mean", "min"))
    )
    # Assign all MCs if none specified
    if (is.null(mcs)) {
        mcs <- sorted_labels |>
            names() |>
            unique() |>
            sort()
    }
    # Main call to manhattan plot
    mc_associations <- manhattan_plot(
        target_pvals,
        threshold = 0.05,
        bonferroni_line = FALSE,
        data_list = data_list,
        mc_labels = mcs,
        colours = colours
    )
    return(mc_associations)
}

split_letters <- function(split_vec, n = 2000) {
    split_letters <- split_vec |>
        label_splits(2000) |>
        unique() |>
        sort()
    return(split_letters)
}

