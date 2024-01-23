library(metasnf)
library(patchwork)
library(ggplot2)

data_list_metadata <- function(data_list) {
    merged_df <- metasnf::collapse_dl(data_list)
    merged_df <- merged_df[, colnames(merged_df) != "subjectkey"]
    ###########################################################################
    # Build data.frame containing the types of variables in merged_df
    ###########################################################################
    types <- data_list |>
        lapply(
            function(x) {
                rep(x$"type", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    domains <- data_list |>
        lapply(
            function(x) {
                rep(x$"domain", ncol(x$"data") - 1)
            }
        ) |>
        unlist()
    var_names <- colnames(merged_df[, colnames(merged_df) != "subjectkey"])
    metadata <- data.frame(
        name = var_names,
        type = types,
        domain = domains
    )
    return(metadata)
}

rename_data_list <- function(data_list, name_mapping) {
    data_list <- data_list |> lapply(
        function(x) {
            old_colnames <- colnames(x$"data")
            new_colnames <- old_colnames |> lapply(
                function(old_name) {
                    if (old_name %in% name_mapping) {
                        name_match <- which(name_mapping == old_name)
                        new_name <- names(name_mapping)[name_match]
                    } else {
                        new_name <- old_name
                    }
                    return(new_name)
                }
            )
            colnames(x$"data") <- new_colnames
            return(x)
        }
    )
    return(data_list)
}

keep_numeric <- function(df) {
    df <- metasnf::numcol_to_numeric(df)
    classes <- as.vector(sapply(df, class))
    df <- df[, classes == "numeric", drop = FALSE]
    return(df)
}

correlation_data <- function(data_list, order = NULL) {
    dl_df <- metasnf::collapse_dl(data_list)
    numeric_dl_df <- keep_numeric(dl_df)
    correlation_matrix <- cor(numeric_dl_df)
    if (is.null(order)) {
        heatmap <- ComplexHeatmap::Heatmap(correlation_matrix)
    } else {
        heatmap <- ComplexHeatmap::Heatmap(
            correlation_matrix[order, order],
            cluster_rows = FALSE,
            cluster_columns = FALSE
        )
    }
    drawn_heatmap <- ComplexHeatmap::draw(heatmap)
    order <- ComplexHeatmap::row_order(drawn_heatmap)
    results <- list(
        "correlation_matrix" = correlation_matrix,
        "heatmap" = heatmap,
        "order" = order
    )
    return(results)
}

pval_heatmap <- function(pval_df, order, max_red = FALSE) {
    pval_df <- pval_df |> dplyr::select(-"row_id")
    pval_matrix <- as.matrix(pval_df[order, ])
    hm <- ComplexHeatmap::Heatmap(
        pval_matrix,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        show_row_names = FALSE,
        col = hm_colours(pval_matrix, max_red)
    )
    return(hm)
}

save_pdf <- function(heatmap, path, width, height) {
    grDevices::pdf(
        path,
        width,
        height
    )
    heatmap
    grDevices::dev.off()
}

save_png <- function(heatmap, path, width, height, res = 300) {
    tryCatch(
        {
            grDevices::png(
                filename = path,
                width = width,
                height = height,
                res = res
            )
            ComplexHeatmap::draw(heatmap)
            grDevices::dev.off()
        }, error = function(err) {
            message("Error saving png - does the heatmap exist?")
            grDevices::dev.off()
        }
    )
}

pval_select <- function(extended_solutions_matrix,
                        keep_summary = TRUE,
                        negative_log = FALSE,
                        recalculate_summary = TRUE) {
    pval_df <- extended_solutions_matrix |>
        dplyr::select(
            "row_id",
            dplyr::ends_with("_p"),
            dplyr::contains("p_val")
        ) |>
        data.frame() |>
        metasnf::numcol_to_numeric()
    if (!keep_summary) {
        pval_df <- pval_df |>
            dplyr::select(-c("min_p_val", "mean_p_val"))
    }
    if (negative_log) {
        neg_log_pval_df <- -log(pval_df)
        neg_log_pval_df$"row_id" <- pval_df$"row_id"
        pval_df <- neg_log_pval_df
        if (recalculate_summary) {
            mini_df <- pval_df |> dplyr::select(
                dplyr::ends_with("_p")
            )
            pval_df$"mean_neglog_p" <- apply(mini_df, 1, FUN = mean)
            pval_df$"max_neglog_p" <- apply(mini_df, 1, FUN = max)
        }
    }
    return(pval_df)
}

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

scatter_plot <- function(df, feature) {
    df <- df |> dplyr::rename("keycol" = !!feature)
    plot <- df |>
        dplyr::select(
            cluster,
            keycol,
        ) |>
        ggplot(
            aes(
                x = cluster,
                y = keycol,
                colour = cluster
            )
        ) +
        geom_violin(
            alpha = 0.4
        ) +
        geom_jitter(
            height = 0,
            width = 0.1,
            alpha = 0.8,
            size = 3,
        ) +
        stat_summary(
            fun = "mean",
            geom = "point",
            colour = "black",
            size = 5
        ) +
        labs(
            x = "Cluster",
            y = title_case(feature),
            color = "Cluster"
        ) +
        theme_bw() +
        theme(
            text = element_text(size = 20),
            legend.position = "none",
            panel.grid.major.x = element_blank()
        )
    return(plot)
}

jitter_plot <- function(df, feature) {
    df <- df |> dplyr::rename("keycol" = !!feature)
    plot <- df |>
        dplyr::select(
            cluster,
            keycol
        ) |>
        ggplot(
            aes(
                x = cluster,
                y = keycol,
                color = cluster
            )
        ) + 
        geom_violin(
            alpha = 0.4
        ) +
        geom_jitter(
            height = 0.1,
            width = 0.2,
            alpha = 0.8,
            size = 3
        ) +
        stat_summary(
            fun = "mean",
            geom = "point",
            colour = "black",
            size = 5
        ) +
        labs(
            x = "Cluster",
            y = title_case(feature),
            color = "Cluster"
        ) +
        theme_bw() +
        theme(
            text = element_text(size = 20),
            legend.position = "none",
            panel.grid.major.x = element_blank()
        )
    return(plot)
}

bar_plot <- function(df, feature) {
    df <- df |>
        dplyr::rename("keycol" = !!feature) |>
        dplyr::select(cluster, keycol) |>
        dplyr::group_by(cluster) |>
        dplyr::count(keycol) |>
        dplyr::mutate(percent = n / sum(n) * 100)
    df$"keycol" <- factor(df$"keycol")
    plot <- df |>
        ggplot(
            aes(
                x = cluster,
                y = percent,
                fill = keycol
            )
        ) +
        geom_bar(
            stat = "identity",
            position = ggplot2::position_stack()
        ) +
        geom_text(
            aes(
                label = n,
                y = percent
            ),
            size = 6,
            position = position_stack(vjust = 0.5)
        ) +
        labs(
            x = "Cluster",
            y = "%",
            fill = title_case(feature)
        ) +
        theme_bw() +
        theme(
            text = element_text(size = 20),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "top",
            panel.grid.major.x = element_blank()
        )
    return(plot)
}

characterize_solution <- function(solution,
                                  data_list,
                                  plotname,
                                  return_plot = TRUE) {
    # Generating the required full dataframe
    cluster_df <- metasnf::get_cluster_df(solution)
    data_df <- metasnf::collapse_dl(data_list)
    full_data <- dplyr::inner_join(cluster_df, data_df, by = "subjectkey")
    full_data$"cluster" <- factor(full_data$"cluster")
    # Re-coding variables
    full_data$"sex"[full_data$"sex" == 0] <- "F"
    full_data$"sex"[full_data$"sex" == 1] <- "M"
    full_data$"race"[full_data$"race" == 0] <- "White"
    full_data$"race"[full_data$"race" == 1] <- "Non-White"
    if (return_plot == FALSE) {
        return(full_data)
    }
    # Identifying features to plot 
    features <- colnames(full_data)[3:length(colnames(full_data))]
    plot_list <- list()
    for (i in 1:length(features)) {
        feature <- features[[i]]
        feature_col <- full_data[, feature]
        nvals <- length(unique(feature_col))
        if (is.numeric(feature_col)) {
            plot <- jitter_plot(full_data, feature)
        } else {
            plot <- bar_plot(full_data, feature)
        }
        ggsave(
            plot = plot,
            fig_path(
                paste0(
                    plotname,
                    "_",
                    feature,
                    ".png"
                ),
                TRUE
            ),
            width = 7,
            height = 7
        )
        plot_list[[i]] <- plot
        names(plot_list)[[i]] <- feature
    }
    return(plot_list)
}

outcome_plots <- function(plot_list) {
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
    ) |> wrap_plots()
    pw
}

demographic_plots <- function(plot_list) {
    pw <- list(
        plot_list$"age",
        plot_list$"mtbi_age",
        plot_list$"household_income",
        plot_list$"sex",
        plot_list$"pubertal_status",
        plot_list$"race"
    ) |> wrap_plots()
    pw
}

neuroimaging_plots <- function(plot_list) {
    layout <- c(
        area(t = 1, l = 1, b = 2, r = 2),
        area(t = 1, l = 3, b = 2, r = 4),
        area(t = 1, l = 5, b = 2, r = 6),
        area(t = 3, l = 2, b = 4, r = 3),
        area(t = 3, l = 4, b = 4, r = 5),
        area(t = 5, l = 2, b = 6, r = 3),
        area(t = 5, l = 4, b = 6, r = 5)
    )
    plot_list$"brain_volume" +
        plot_list$"cortical_sa" +
        plot_list$"cortical_thickness" +
        plot_list$"major_wm_ndi" +
        plot_list$"pericortical_wm_ndi" +
        plot_list$"fmri_cort_cors" +
        plot_list$"fmri_cort_subcort_cors" +
        plot_layout(design = layout)
}

acute_symptom_plots <- function(plot_list) {
    pw <- list(
        plot_list$"mtbi_mem_daze",
        plot_list$"mtbi_loc",
        plot_list$"mtbi_mechanism"
    ) |> wrap_plots()
    pw
}

parent_psych_plots <- function(plot_list) {
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
        #plot_list$"parent_anxdisord",
        plot_list$"parent_avoidant",
        plot_list$"parent_antisocial",
        plot_list$"parent_hyperactive"
    ) |> wrap_plots()
    pw
}

family_env_plots <- function(plot_list) {
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
    ) |> wrap_plots()
    pw
}

prosocial_plots <- function(plot_list) {
    layout <- c(
        area(t = 1, l = 1, b = 2, r = 2),
        area(t = 1, l = 3, b = 2, r = 4),
        area(t = 1, l = 5, b = 2, r = 6),
        area(t = 1, l = 7, b = 2, r = 8),
        area(t = 3, l = 2, b = 4, r = 3),
        area(t = 3, l = 4, b = 4, r = 5),
        area(t = 3, l = 6, b = 4, r = 7)
    )
    plot_list$"same_sex_friend" +
    plot_list$"opp_sex_friend" +
    plot_list$"same_sex_close_friend" +
    plot_list$"opp_sex_close_friend" +
    plot_list$"prosocial_considerate" +
    plot_list$"prosocial_helps_hurt" +
    plot_list$"prosocial_helpful" + plot_layout(design = layout)
}

healthy_habits_plots <- function(plot_list) {
    plot_list$"weekday_screentime" +
    plot_list$"school_programs" +
    plot_list$"non_school_programs" +
    plot_list$"programs_this_year" +
    plot_list$"private_instruction" +
    plot_list$"exercise_time"
}

medical_history_plots <- function(plot_list) {
    pw <- list(
        plot_list$"headache_history",
        plot_list$"previous_mtbis"
    ) |> wrap_plots()
    pw
}

save_plot_list <- function(plot_list, prefix) {
    all_plots <- wrap_plots(plot_list)
    #outcome_plots <- outcome_plots(plot_list)
    #demographic_plots <- demographic_plots(plot_list)
    #neuroimaging_plots <- neuroimaging_plots(plot_list)
    #acute_symptom_plots <- acute_symptom_plots(plot_list)
    #parent_psych_plots <- parent_psych_plots(plot_list)
    #family_env_plots <- family_env_plots(plot_list)
    #prosocial_plots <- prosocial_plots(plot_list)
    #healthy_habits_plots <- healthy_habits_plots(plot_list)
    #medical_history_plots <- medical_history_plots(plot_list)
    ggsave(
        plot = all_plots,
        fig_path(paste0(prefix, "_all_plots.png"), TRUE),
        width = 33,
        height = 33
    )
    #ggsave(
    #    plot = outcome_plots,
    #    fig_path(paste0(prefix, "_outcome_plots.png"), TRUE),
    #    width = 15,
    #    height = 15
    #)
    #ggsave(
    #    plot = demographic_plots,
    #    fig_path(paste0(prefix, "_demographic_plots.png"), TRUE),
    #    width = 15,
    #    height = 10
    #)
    #ggsave(
    #    plot = neuroimaging_plots,
    #    fig_path(paste0(prefix, "_neuroimaging_plots.png"), TRUE),
    #    width = 15,
    #    height = 15
    #)
    #ggsave(
    #    plot = acute_symptom_plots,
    #    fig_path(paste0(prefix, "_acute_symptom_plots.png"), TRUE),
    #    width = 20,
    #    height = 7
    #)
    #ggsave(
    #    plot = parent_psych_plots,
    #    fig_path(paste0(prefix, "_parent_psych_plots.png"), TRUE),
    #    width = 20,
    #    height = 13
    #)
    #ggsave(
    #    plot = family_env_plots,
    #    fig_path(paste0(prefix, "_family_env_plots.png"), TRUE),
    #    width = 15,
    #    height = 15
    #)
    #ggsave(
    #    plot = prosocial_plots,
    #    fig_path(paste0(prefix, "_prosocial_plots.png"), TRUE),
    #    width = 15,
    #    height = 8
    #)
    #ggsave(
    #    plot = healthy_habits_plots,
    #    fig_path(paste0(prefix, "_healthy_habits_plots.png"), TRUE),
    #    width = 14,
    #    height = 8
    #)
    #ggsave(
    #    plot = medical_history_plots,
    #    fig_path(paste0(prefix, "_medical_history_plots.png"), TRUE),
    #    width = 12,
    #    height = 6
    #)
}
