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
        heatmap <- ComplexHeatmap::Heatmap(
            correlation_matrix,
            heatmap_legend_param = list(
                title = "Corr"
            )
        )
    } else {
        heatmap <- ComplexHeatmap::Heatmap(
            correlation_matrix[order, order],
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            heatmap_legend_param = list(
                title = "Corr"
            )
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

diff_heatmap <- function(corr1, corr2, abs = FALSE) {
    # keeping shared columns only
    cols1 <- colnames(corr1) %in% colnames(corr2)
    cols2 <- colnames(corr2) %in% colnames(corr1)
    corr1 <- corr1[cols1, cols1]
    corr2 <- corr2[cols2, cols2]
    # calculate difference
    if (abs) {
        diff <- abs(corr1) - abs(corr2)
        caption <- "Δ|Corr|"
    } else {
        diff <- corr1 - corr2
        caption <- "ΔCorr"
    }
    # plot heatmap
    heatmap <- ComplexHeatmap::Heatmap(
        diff,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        heatmap_legend_param = list(
            title = caption
        )
    )
    return(heatmap)
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

save_png <- function(heatmap, path, width, height, res = 150) {
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
            alpha = 0.5,
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
    cluster <- ""
    keycol <- ""
    percent <- ""
    n <- ""
    df <- df |>
        dplyr::rename("keycol" = !!feature) |>
        dplyr::select(cluster, keycol) |>
        dplyr::group_by(cluster) |>
        dplyr::count(keycol) |>
        dplyr::mutate(percent = n / sum(n) * 100)
    df$"keycol" <- factor(df$"keycol")
    if (all(df$"keycol" %in% c(0, 1))) {
        levels(df$"keycol") <- c("1", "0")
    }
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

divergent_colours <- function(column)  {
    values <- unique(column)
    colours <- RColorBrewer::brewer.pal(n = length(values), name = "Set3")
    names(colours) <- values
    return(colours)
}

colour_scale <- function(data, min_colour = "black", max_colour = "red") {
    colours <- circlize::colorRamp2(
        c(min(data), max(data)),
        c(min_colour, max_colour)
    )
    return(colours)
}

hm_colours <- function(data, max_red = TRUE) {
    if (max_red) {
        colours <- circlize::colorRamp2(
            c(min(data), max(data)),
            c("black", "red")
        )
    } else {
        colours <- circlize::colorRamp2(
            c(min(data), max(data)),
            c("red", "black")
        )
    }
    return(colours)
}

split_at <- function(vector, nrow) {
    split_vec <- rep("A", nrow)
    if (vector[length(vector)] != nrow) {
        vector <- c(vector, nrow)
    }
    for (i in 1:(length(vector) - 1)) {
        start <- vector[i]
        end <- vector[i + 1]
        split_vec[start:end] <- LETTERS[i + 1]
    }
    return(split_vec)
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
        } else {
            plot <- bar_plot(full_data, feature)
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
                width = 7,
                height = 7
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

all_plots <- function(plot_list, plotname) {
    pw <- wrap_plots(plot_list)
    ggsave(
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
    ) |> wrap_plots()
    ggsave(
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
    pw <- wrap_plots(plot_list)
    ggsave(
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
    ggsave(
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
        as_plots <- wrap_plots(filtered_list)
        ggsave(
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
    ) |> wrap_plots()
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
    ) |> wrap_plots()
    ggsave(
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
    ggsave(
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
    ggsave(
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
    pw <- wrap_plots(plot_list)
    ggsave(
        plot = pw,
        filename = paste0(plotname, "_medical_history_plots.png"),
        width = 6 * length(plot_list),
        height = 6
    )
}

extract_imputed <- function(original_df, imputed_df, partition) {
    return(imputed_df[imputed_df$".imp" == partition, colnames(original_df)])
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

effect_size_raw <- function(x1, x2) {
    n1 <- length(x1)
    n2 <- length(x2)
    s1 <- sd(x1)
    s2 <- sd(x2)
    s <- pooled_sd(n1, s1, n2, s2)
    mean1 <- mean(x1)
    mean2 <- mean(x2)
    d <- cohens_d(mean1, mean2, s)
    return(d)
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

tstat <- function(d, n1, n2) {
    t <- d * sqrt((n1 * n2) / (n1 + n2))
    return(t)
}

es_bound <- function(d, n1, n2) {
    t <- tstat(d, n1, n2)
    es_data <- compute.es::tes(
        t = t,
        n.1 = n1,
        n.2 = n2,
        verbose = FALSE
    )
    return(
        list(
            "lower" = es_data$"l.d",
            "upper" = es_data$"u.d"
        )
    )
}

cluster_t_test <- function(data, main_group, ref_group, var) {
    x1 <- data[data$"cluster" %in% main_group, var] |>
        unlist() |>
        as.numeric()
    x2 <- data[data$"cluster" %in% ref_group, var] |>
        unlist() |>
        as.numeric()
    p_val <- t.test(
        x1,
        x2,
        alternative = "two.sided",
        var.equal = FALSE,
        conf.level = 0.95
    )$"p.value"
    return(p_val)
}

cluster_d <- function(data, main_group, ref_group, var) {
    x1 <- data[data$"cluster" %in% main_group, var] |>
        unlist() |>
        as.numeric()
    x2 <- data[data$"cluster" %in% ref_group, var] |>
        unlist() |>
        as.numeric()
    sp <- pooled_sd(
        length(x1),
        sd(x1),
        length(x2),
        sd(x2)
    )
    d <- cohens_d(
        mean(x1),
        mean(x2),
        sp
    )
    bounds <- es_bound(
        d,
        length(x1),
        length(x2)
    )
    results <- c(d, bounds$"lower", bounds$"upper")
    names(results) <- c("d", "lower", "upper")
    return(results)
}

manhattan_plot <- function(data,
                           key_mode = FALSE,
                           threshold = NULL,
                           bonferroni_line = FALSE,
                           data_list,
                           mc_labels = NULL,
                           colours = NULL) {
    row_id <- ""
    variable <- ""
    pval <- ""
    domain <- ""
    mean_pval <- ""
    mc_label <- ""
    sd_pval <- ""
    var_cols <- colnames(data)[endsWith(colnames(data), "_pval")]
    data[, var_cols] <- data[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                -log10(x)
                raw_pval <- -log10(x)
                new_x <- dplyr::case_when(
                    raw_pval > 5 ~ 5,
                    TRUE ~ raw_pval
                )
                new_x
            }
        )
    data$"row_id" <- factor(data$"row_id")
    data$"mc_label" <- factor(data$"mc_label")
    data <- data |>
        tidyr::pivot_longer(
            !(c(row_id, mc_label)),
            names_to = "variable",
            values_to = "pval"
        ) |>
        data.frame()
    summary_data <- data |>
        dplyr::group_by(
            mc_label,
            variable
        ) |>
        dplyr::summarize(
            mean_pval = mean(pval),
            .groups = "drop"
        )
    summary_data$"variable" <- sub("_pval$", "", summary_data$"variable")
    ###########################################################################
    # Merge the summmary plot with domain information from the data_list
    ###########################################################################
    dl_metadata <- data_list_metadata(data_list) |> dplyr::select(-"type")
    summary_data <- merge(
        summary_data,
        dl_metadata,
        by.x = "variable",
        by.y = "name",
        all.x = TRUE
    )
    summary_data <- summary_data |> dplyr::arrange(domain)
    summary_data$"variable" <- factor(
        summary_data$"variable",
        levels = unique(summary_data$"variable")
    )
    if (!is.null(mc_labels)) {
        summary_data <- summary_data[summary_data$"mc_label" %in% mc_labels, ]
        summary_data$"mc_label" <- factor(
            summary_data$"mc_label",
            levels = mc_labels
        )
    }
    plot <- summary_data |>
        ggplot2::ggplot(ggplot2::aes(x = domain, y = mean_pval)) +
        ggplot2::geom_jitter(
            mapping = ggplot2::aes(
                group = domain,
                x = variable,
                y = mean_pval,
                colour = domain
            ),
            height = 0,
            width = 0,
            size = 5
        ) +
        ggplot2::labs(
            x = "Variable",
            y = expression("-log"[10] * "(p)"),
            colour = "Domain"
        ) +
        ggplot2::ylim(0, 5) +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "Discretized -log10(p)") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5),
            text = element_text(size = 20)
        )
    if (!is.null(colours)) {
        plot <- plot + ggplot2::scale_colour_manual(values = colours)
    }
    plot <- plot + ggplot2::facet_grid(mc_label ~ .)
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
        if (bonferroni_line) {
            plot <- plot + ggplot2::geom_hline(
                yintercept = -log10(threshold / nrow(data)),
                linetype = "dashed",
                colour = "black"
            )
        }
    } else if (bonferroni_line) {
        stop(
            "Please specify threshold p-value which will be used",
            " for calculting the Bonferroni-corrected line."
        )
    }
    return(plot)
}

manhattan_plot2 <- function(esm,
                            threshold = NULL,
                            data_list,
                            target_list,
                            variable_order = NULL,
                            xints = "outcomes",
                            colours = NULL) {
    ###########################################################################
    # Suppress warnings related to non-standard evaluation
    ###########################################################################
    row_id <- ""
    variable <- ""
    pval <- ""
    domain <- ""
    mean_pval <- ""
    mc_label <- ""
    sd_pval <- ""
    ###########################################################################
    # Formatting esm as dataframe
    ###########################################################################
    esm <- data.frame(esm)
    ###########################################################################
    # Select row_id, label, and p-value related columns only
    ###########################################################################
    if (!"label" %in% colnames(esm)) {
        esm$"label" <- esm$"row_id"
    }
    esm <- esm |>
        dplyr::select(
            "row_id",
            "label",
            dplyr::ends_with("_pval")
        )
    ###########################################################################
    # Convert row_id and label to factors
    ###########################################################################
    esm$"row_id" <- factor(esm$"row_id")
    esm$"label" <- factor(esm$"label")
    ###########################################################################
    # Re-assign names to the data list and target list
    ###########################################################################
    if (!is.null(target_list)) {
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
    }
    ###########################################################################
    # Columns that end with _p are truncated by the threshold of p = 1e-5
    ###########################################################################
    var_cols <- colnames(esm)[endsWith(colnames(esm), "_pval")]
    cutoff_var_cols <- esm[, var_cols] |>
        apply(
            MARGIN = 2,
            FUN = function(x) {
                p <- -log10(x)
                if (length(p) == 1) {
                    if (p > 5) {
                        p <- 5
                    }
                } else {
                    p[p > 5] <- 5
                }
                return(p)
            }
        ) |>
            as.matrix()
    if (dim(cutoff_var_cols)[2] == 1) {
        cutoff_var_cols <- t(cutoff_var_cols)
    }
    esm[, var_cols] <- cutoff_var_cols
    summary_data <- esm |>
        tidyr::pivot_longer(
            !(c(row_id, label)),
            names_to = "variable",
            values_to = "neg_log_pval"
        ) |>
        data.frame()
    summary_data$"variable" <- sub("_pval$", "", summary_data$"variable")
    ###########################################################################
    # Merge the summmary plot with domain information from the data_list
    ###########################################################################
    dl_metadata <- data_list_metadata(data_list) |> dplyr::select(-"type")
    n_outcomes <- length(which(startsWith(dl_metadata$"domain", "O")))
    n_vars <- nrow(dl_metadata)
    summary_data <- merge(
        summary_data,
        dl_metadata,
        by.x = "variable",
        by.y = "name",
        all.x = TRUE
    )
    summary_data <- summary_data |> dplyr::arrange(domain)
    ###########################################################################
    # Proper ordering of variables through factor level assignment
    ###########################################################################
    if (is.null(variable_order)) {
        summary_data$"variable" <- factor(
            summary_data$"variable",
            levels = unique(summary_data$"variable")
        )
    } else {
        involved_vars <- unique(summary_data$"variable")
        variable_order <- variable_order[variable_order %in% involved_vars]
        summary_data$"variable" <- factor(
            summary_data$"variable",
            levels = variable_order
        )
    }
    labels <- unique(esm$"label")
    plot <- summary_data |>
        ggplot2::ggplot(ggplot2::aes(x = domain, y = mean_pval)) +
        ggplot2::geom_jitter(
            mapping = ggplot2::aes(
                group = domain,
                x = variable,
                y = neg_log_pval,
                colour = domain
            ),
            height = 0,
            width = 0,
            size = 5
        ) +
        ggplot2::labs(
            x = NULL,
            y = expression("-log"[10] * "(p)"),
            colour = "Domain"
        ) +
        ggplot2::ylim(0, 5) +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "-log10(p)") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 1
            ),
            plot.title = ggplot2::element_text(hjust = 0.5),
            text = element_text(size = 20)
        ) +
        ggplot2::scale_colour_manual(
            values = c(
                "I-AS" = "#85929e",
                "I-D" = "#bb8fce",
                "I-N" = "#e59866",
                "I-MH" = "#7fb3d5",
                "I-P" = "#52be80",
                "O-S" = "#d98880",
                "O-B" = "#ff3131"
            )
        ) +
        ggplot2::facet_grid(label ~ .)
    ###########################################################################
    # Prepping x-intercept positions
    ###########################################################################
    if (!is.null(xints)) {
        if (identical(xints, "outcomes")) {
            plot <- plot + ggplot2::geom_vline(
                xintercept = n_vars - n_outcomes + 0.5
            )
        } else {
            xints <- xints + 0.5
            plot <- plot + ggplot2::geom_vline(
                xintercept = xints
            )
        }
    }
    ###########################################################################
    # Add p-value threshold if requested
    ###########################################################################
    if (!is.null(threshold)) {
        plot <- plot + ggplot2::geom_hline(
            yintercept = -log10(threshold),
            linetype = "dashed",
            colour = "red"
        )
    }
    return(plot)
}

my_similarity_matrix_heatmap <- function(aris,
                                         aris_order,
                                         extended_solutions,
                                         split_vector = NULL,
                                         show_clusters = TRUE) {
    outcome_pvals <- extended_solutions |>
        get_pvals(negative_log = TRUE) |>
        dplyr::select(
            "row_id",
            dplyr::starts_with(c("cbcl", "sds"))
        ) |>
        summarize_pvals()
    extended_solutions$"mean_pval" <- outcome_pvals$"mean_pval"
    extended_solutions$"nclust2" <- extended_solutions$"nclust" - 2
    if (!is.null(split_vector)) {
        splits <- split_at(vector = split_vector, nrow = 2000)
    } else {
        splits <- NULL
    }
    if (show_clusters) {
        mc_heatmap <- similarity_matrix_heatmap(
            aris,
            order = aris_order,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            log_graph = FALSE,
            data = extended_solutions,
            top_hm = list(
                "Scheme" = "snf_scheme",
                "Impairment Separation" = "mean_pval"
            ),
            left_hm = list(
                "Pooled" = "pooled",
                "Imputation" = "imputation"
            ),
            top_bar = list(
                "Number of Clusters - 2" = "nclust2"
            ),
            scale_diag = "none",
            heatmap_height = grid::unit(19, "cm"),
            heatmap_width = grid::unit(19, "cm"),
            annotation_colours = list(
                "Pooled" = c(
                    "yes" = "orange",
                    "no" = "purple"
                ),
                "Imputation" = divergent_colours(
                    extended_solutions$"imputation"
                ),
                "Scheme" = c(
                    "1" = "#7FC97F",
                    "2" = "#BEAED4",
                    "3" = "#FDC086"
                ),
                "Impairment Separation" = hm_colours(
                    extended_solutions$"mean_pval"
                )
            ),
            col = circlize::colorRamp2(
                c(min(aris), max(aris)),
                c("navy", "red")
            ),
            row_split = splits,
            column_split = splits
        )
    } else {
        mc_heatmap <- similarity_matrix_heatmap(
            aris,
            order = aris_order,
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            log_graph = FALSE,
            data = extended_solutions,
            top_hm = list(
                "Scheme" = "snf_scheme",
                "Impairment Separation" = "mean_pval"
            ),
            left_hm = list(
                "Pooled" = "pooled",
                "Imputation" = "imputation"
            ),
            scale_diag = "none",
            heatmap_height = grid::unit(19, "cm"),
            heatmap_width = grid::unit(19, "cm"),
            annotation_colours = list(
                "Pooled" = c(
                    "yes" = "orange",
                    "no" = "purple"
                ),
                "Imputation" = divergent_colours(
                    extended_solutions$"imputation"
                ),
                "Scheme" = c(
                    "1" = "#7FC97F",
                    "2" = "#BEAED4",
                    "3" = "#FDC086"
                ),
                "Impairment Separation" = hm_colours(
                    extended_solutions$"mean_pval"
                )
            ),
            col = circlize::colorRamp2(
                c(min(aris), max(aris)),
                c("navy", "red")
            ),
            row_split = splits,
            column_split = splits
        )
    }
    return(mc_heatmap)
}

lite_similarity_matrix_heatmap <- function(aris,
                                           aris_order,
                                           extended_solutions,
                                           split_vector = NULL,
                                           show_clusters = TRUE) {
    outcome_pvals <- extended_solutions |>
        get_pvals(negative_log = TRUE) |>
        dplyr::select(
            "row_id",
            dplyr::starts_with(c("cbcl", "sds"))
        ) |>
        summarize_pvals()
    extended_solutions$"mean_pval" <- outcome_pvals$"mean_pval"
    extended_solutions$"nclust2" <- extended_solutions$"nclust" - 2
    if (!is.null(split_vector)) {
        splits <- split_at(vector = split_vector, nrow = 2000)
    } else {
        splits <- NULL
    }
    mc_heatmap <- similarity_matrix_heatmap(
        aris,
        order = aris_order,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        log_graph = FALSE,
        data = extended_solutions,
        top_hm = list(
            "Outcome Separation" = "mean_pval"
        ),
        top_bar = list(
            # "Number of Clusters - 2" = "nclust2"
            "Number of Clusters" = "nclust"
        ),
        scale_diag = "none",
        heatmap_height = grid::unit(19, "cm"),
        heatmap_width = grid::unit(19, "cm"),
        annotation_colours = list(
            "Outcome Separation" = colour_scale(
                extended_solutions$"mean_pval",
                "white",
                "darkgreen"
            )
        ),
        col = circlize::colorRamp2(
            c(min(aris), max(aris)),
            c("navy", "red")
        ),
        row_split = splits,
        column_split = splits,
        column_title_gp = grid::gpar(fontsize = 20),
        row_title_gp = grid::gpar(fontsize = 20)
    )
    return(mc_heatmap)
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
        reordered_labels <- split_at(split_vector, 2000)
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
        split_at(2000) |>
        unique() |>
        sort()
    return(split_letters)
}

my_corr_plot <- function(data_list, target_list, order) {
    # Merging and dummying the data lists
    full_data_list <- c(data_list, target_list)
    if ("d_race" %in% summarize_dl(full_data_list)$"name") {
        race_i <- which(summarize_dl(full_data_list)$"name" == "d_race")
        full_data_list[[race_i]]$"data" <- full_data_list[[race_i]]$"data" |>
            fastDummies::dummy_cols(
                "race",
                remove_first_dummy = TRUE,
                remove_selected_columns = TRUE
            )
    }
    if ("as_mechanism" %in% summarize_dl(full_data_list)$"name") {
        mech_i <- which(summarize_dl(full_data_list)$"name" == "as_mechanism")
        full_data_list[[mech_i]]$"data" <- full_data_list[[mech_i]]$"data" |>
            fastDummies::dummy_cols(
                "mtbi_mechanism",
                remove_first_dummy = TRUE,
                remove_selected_columns = TRUE
            )
    }
    ###########################################################################
    order <- order |>
        unlist() |>
        as.character()
    ###########################################################################
    # First, make sure that the main order is restricted to just those vars
    # present in the join data_list.
    dl_df <- collapse_dl(full_data_list)
    df_cols <- colnames(dl_df)[-1] # the first col is subjectkey
    order <- order[order %in% df_cols]
    new_order <- match(order, df_cols)
    corr_data <- correlation_data(full_data_list, order = new_order)
    return(corr_data)
}

representative_mc <- function(split_vector,
                              aris,
                              ari_order,
                              solutions_matrix,
                              group_name,
                              possible_data,
                              individual_plots = TRUE,
                              group_plots = TRUE) {
    # Formatting
    ari_order <- unlist(ari_order)
    # Sorting
    aris <- aris[ari_order, ari_order]
    solutions_matrix <- solutions_matrix[ari_order, ]
    # Assigning meta clusters to the solutions matrix and ARI matrix
    solutions_matrix$"mc" <- split_at(split_vector, nrow(solutions_matrix))
    aris$"mc" <- split_at(split_vector, nrow(solutions_matrix))
    mcs <- split_at(split_vector, nrow(solutions_matrix)) |>
        unique()
    for (mc in mcs) {
        mc_sm <- solutions_matrix[solutions_matrix$"mc" == mc, ]
        mc_ari <- aris[aris$"mc" == mc, ]
        mc_ari$"mc" <- NULL
        rep_mc <- which(rowSums(mc_ari) == max(rowSums(mc_ari)))[1]
        rep_sol <- mc_sm[rep_mc, ]
        sol_row_id <- rep_sol$"row_id"
        sol_imp <- rep_sol$"imputation"
        sol_name <- paste0("mc_", mc, "_row_id_", sol_row_id, "_", sol_imp)
        sol_name <- paste0(group_name, "_", sol_name)
        sol_name <- tolower(sol_name)
        write_csv(rep_sol, proc_path(paste0(sol_name, ".csv"), TRUE))
        characterize_solution(
            solution = rep_sol,
            data_list = possible_data[[sol_imp]],
            plotname = fig_path(sol_name,  TRUE),
            individual_plots = individual_plots,
            group_plots = group_plots
        )
    }
}

# Restrict a set of solutions matrix to the most representative solutions
# based on their adjusted Rand Index.
get_rep_solutions <- function(ari,
                              split_vector,
                              ari_order,
                              solutions_matrix,
                              labels = NULL,
                              exclude_mcs = NULL,
                              include_mcs = NULL,
                              restriction_function = NULL) {
    ###########################################################################
    # Re-sort the solutions matrix based on the aris
    ###########################################################################
    ari_order <- unlist(ari_order)
    ari <- data.frame(ari[ari_order, ari_order])
    solutions_matrix <- solutions_matrix[ari_order, ]
    ###########################################################################
    # Extract and assign meta cluster labels
    ###########################################################################
    mcs <- unique(split_at(split_vector, nrow(solutions_matrix)))
    solutions_matrix$"mc" <- split_at(split_vector, nrow(solutions_matrix))
    ari$"mc" <- split_at(split_vector, nrow(solutions_matrix))
    ###########################################################################
    # Iterate through the meta clusters and keep the representative solution
    ###########################################################################
    rep_solutions <- data.frame()
    for (mc in mcs) {
        # Subset to just those solutions and ARIs within the MC
        mc_sm <- solutions_matrix[solutions_matrix$"mc" == mc, ]
        mc_ari <- ari[ari$"mc" == mc, ]
        mc_ari$"mc" <- NULL
        if (!is.null(restriction_function)) {
            mc_indices <- restriction_function(mc_sm)
            mc_ari <- mc_ari[mc_indices, ]
            mc_sm <- mc_sm[mc_indices, ]
        }
        # The most representative solution
        rep_mc <- which(rowSums(mc_ari) == max(rowSums(mc_ari)))[1]
        rep_solution <- mc_sm[rep_mc, ]
        rep_solutions <- rbind(rep_solutions, rep_solution)
    }
    ###########################################################################
    # Assign labels to the representative solutions
    ###########################################################################
    if (is.null(labels)) {
        labels <- rep_solutions$"mc"
    }
    rep_solutions$"label" <- labels
    ###########################################################################
    # Restrict representative solutions based on the inclusion/exclusion lists
    ###########################################################################
    if (!is.null(exclude_mcs) & !is.null(include_mcs)) {
        warning(
            "`exclude_mcs` and `include_mcs` cannot both be specified.",
            " Representative solutions will not be filtered."
        )
        return(rep_solutions)
    }
    if (!is.null(exclude_mcs)) {
        rep_solutions <- rep_solutions[!rep_solutions$"mc" %in% exclude_mcs, ]
    }
    if (!is.null(include_mcs)) {
        rep_solutions <- rep_solutions[rep_solutions$"mc" %in% include_mcs, ]
    }
    return(rep_solutions)
}

subject_filter_dl <- function(data_list, subject_vector) {
    subject_vector <- paste0("subject_", subject_vector)
    data_list <- data_list |>
        lapply(
            function(x) {
                x$"data" <-
                    x$"data"[x$"data"$"subjectkey" %in% subject_vector, ]
                return(x)
            }
        )
    return(data_list)
}

ari_order <- function(ari_matrix) {
    heatmap <- similarity_matrix_heatmap(
        ari_matrix,
        log_graph = FALSE,
        cluster_rows = TRUE,
        cluster_columns = TRUE
    )
    heatmap <- ComplexHeatmap::draw(heatmap)
    order <- ComplexHeatmap::row_order(heatmap)
    return(order)
}

my_manhattan_save <- function(manhattan_plot, name) {
    mcs <- length(levels(manhattan_plot[[1]]$"mc_label"))
    ggsave(
        plot = manhattan_plot,
        filename = name,
        width = 15,
        height = mcs * 2.5
    )
}

my_manhattan_bulk_save <- function(split_vector,
                                   solutions_matrix,
                                   ari_order,
                                   data_list,
                                   target_list,
                                   prefix) {
    full_manhattan <- my_manhattan_plot(
        solutions_matrix = solutions_matrix,
        aris_order = ari_order,
        split_vector = split_vec,
        data_list = data_list,
        target_list = target_list,
        colours = c(
            "I-AS" = "#1B9E77",
            "I-D" = "#D95F02",
            "I-N" = "#7570B3",
            "I-MH" = "#E7298A",
            "I-P" = "#66A61E",
            "O-S" = "#E6AB02",
            "O-B" = "#A6761D"
        )
    )
    print(
        paste0(
            "Saving full plot at ",
            fig_path(paste0(prefix, "_manhattan.png"), TRUE)
        )
    )
    my_manhattan_save(
        full_manhattan,
        fig_path(paste0(prefix, "_manhattan.png"), TRUE)
    )
    print("Saving individual MC plots...")
    for (mc in split_letters(split_vector)) {
        print(mc)
        mc_plot <- my_manhattan_plot(
            solutions_matrix = solutions_matrix,
            aris_order = ari_order,
            split_vector = split_vec,
            mcs = mc,
            data_list = data_list,
            target_list = target_list,
            colours = c(
                "I-AS" = "#1B9E77",
                "I-D" = "#D95F02",
                "I-N" = "#7570B3",
                "I-MH" = "#E7298A",
                "I-P" = "#66A61E",
                "O-S" = "#E6AB02",
                "O-B" = "#A6761D"
            )
        )
        mc_path <- paste0(
            prefix,
            "_mc_",
            gsub("-", "_", tolower(mc)),
            "_manhattan.png"
        )
        ggsave(
            plot = mc_plot,
            fig_path(mc_path, TRUE),
            width = 15,
            height = 6
        )
    }
}

mc_heatmap_save <- function(heatmap, path) {
    grDevices::png(
        filename = path,
        width = 1100,
        height = 1000,
        res = 110
    )
    print(heatmap)
    grDevices::dev.off()
}

select_solution <- function(solutions_matrix, row, imp) {
    solution <- solutions_matrix |>
        dplyr::filter(
            solutions_matrix$"row_id" == row,
            solutions_matrix$"imputation" == imp
        )
    return(solution)
}

dist_plots <- function(data_list = NULL, df = NULL, size = 4) {
    if (is.null(df)) {
        df <- collapse_dl(data_list)
    }
    vars <- colnames(df)[-1]
    plot_list <- list()
    for (var in vars) {
        plot <- df |>
            ggplot(aes(x = TRUE, y = !!sym(var))) +
            geom_jitter(size = size, alpha = 0.8, height = 0.1) +
            labs(title = var, x = "") +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )
        plot_list[[length(plot_list) + 1]] <- plot
    }
    all_plots <- wrap_plots(plot_list)
    return(all_plots)
}

# Scatter plots of any two variables in a data_list
dl_scatter <- function(data_list, var1, var2) {
    dl_df <- data.frame(collapse_dl(data_list))
    plot <- dl_df |>
        ggplot(aes(x = dl_df[, var1], y = dl_df[, var2])) +
        geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = var1, y = var2) +
        theme_bw()
    return(plot)
}

dl_feature_restrict <- function(data_list, inclusion_features) {
    inclusion_features <- c("subjectkey", inclusion_features)
    filtered_dl <- lapply(
        data_list,
        function(x) {
            keep_cols <- colnames(x$"data") %in% inclusion_features
            x$"data" <- x$"data"[, keep_cols, drop = FALSE]
            if (ncol(x$"data") > 1) {
                return(x)
            } else {
                return(NULL)
            }
        }
    )
    # Remove totally NULL elements
    filtered_dl <- Filter(Negate(is.null), filtered_dl)
    return(filtered_dl)
}

plot_solutions_matrix <- function(solutions_matrix,
                           group_name,
                           possible_data,
                           individual_plots = TRUE,
                           group_plots = TRUE) {
    for (row in seq_len(nrow(solutions_matrix))) {
        solution <- solutions_matrix[row, ]
        id <- solution$"row_id"
        imp <- solution$"imputation"
        mc <- solution$"mc"
        sol_name <- paste0("mc_", mc, "_row_id_", id, "_", imp)
        sol_name <- paste0(group_name, "_", sol_name)
        sol_name <- tolower(sol_name)
        characterize_solution(
            solution = solution,
            data_list = possible_data[[imp]],
            plotname = fig_path(sol_name,  TRUE),
            individual_plots = individual_plots,
            group_plots = group_plots
        )
    }
}

extend_solutions_imp <- function(solutions_matrix,
                                 imputed_targets,
                                 cat_test = "chi_squared",
                                 calculate_summaries = TRUE,
                                 min_pval = NULL,
                                 processes = 1) {
    ###########################################################################
    # Ensure imputations are formatted properly
    ###########################################################################
    esm_imps <- sort(unique(solutions_matrix$"imputation"))
    target_imps <- sort(names(imputed_targets))
    if (is.null(esm_imps) || !all(esm_imps %in% target_imps)) {
        stop(
            "solutions_matrix must contain a column named 'imputation' with",
            "values present in the names of the imputed_targets parameter.",
            "Imputed targets should be a named list of target_lists, where",
            "each list corresponds to a distinc timputation label."
        )
    }
    ###########################################################################
    # Calculate vector of all feature names across all imputations
    ###########################################################################
    all_features <- imputed_targets |>
        lapply(
            function(x) {
                x_df <- collapse_dl(x)
                colnames(x_df)[-1]
            }
        ) |>
        unlist() |>
        as.character() |>
        unique()
    ###########################################################################
    # Construct base of extended solutions matrix by adding columns for
    # p-values of all features
    ###########################################################################
    # Specifying the dataframe structure avoids tibble-related errors
    esm <- solutions_matrix |>
        data.frame() |>
        add_columns(
            paste0(all_features, "_pval"),
            fill = NA
        )
    ###########################################################################
    # Sequential extension
    ###########################################################################
    if (processes == 1) {
        # Iterate across rows of the solutions matrix
        for (i in seq_len(nrow(esm))) {
            imp <- esm[i, "imputation"]
            target_list <- imputed_targets[[imp]]
            ###################################################################
            ## Calculate vector of all feature types
            ###################################################################
            features <- target_list |>
                lapply(
                    function(x) {
                        colnames(x[[1]])[-1]
                    }
                ) |>
                unlist()
            feature_types <- target_list |>
                lapply(
                    function(x) {
                        n_features <- ncol(x$"data") - 1
                        outcome_type <- rep(x$"type", n_features)
                        return(outcome_type)
                    }
                ) |>
                unlist()
            ###################################################################
            ## Single DF to contain all features to calculate p-values for
            ###################################################################
            merged_df <- target_list |>
                lapply(
                    function(x) {
                        x[[1]]
                    }
                ) |>
                merge_df_list()
            print(paste0("Processing row ", i, " of ", nrow(esm)))
            clustered_subs <- get_clustered_subs(esm[i, ])
            for (j in seq_along(features)) {
                current_outcome_component <- merged_df[, c(1, j + 1)]
                current_outcome_name <- colnames(current_outcome_component)[2]
                suppressWarnings(
                    p_value <- get_cluster_pval(
                        clustered_subs,
                        current_outcome_component,
                        feature_types[j],
                        features[j],
                        cat_test = cat_test
                    )
                )
                target_col <- which(
                    paste0(current_outcome_name, "_pval") == colnames(esm)
                )
                esm[i, target_col] <- p_value
            }
        }
    } else {
        #######################################################################
        # Parallel extension
        #######################################################################
        max_cores <- future::availableCores()
        if (processes == "max") {
            processes <- max_cores
        } else if (processes > max_cores) {
            print(
                paste0(
                    "Requested processes exceed available cores.",
                    " Defaulting to the max avaiilable (", max_cores, ")."
                )
            )
            processes <- max_cores
        }
        # Iterate across rows of the solutions matrix
        future::plan(future::multisession, workers = processes)
        esm_rows <- future.apply::future_lapply(
            seq_len(nrow(esm)),
            function(i) {
                imp <- esm[i, "imputation"]
                target_list <- imputed_targets[[imp]]
                ###############################################################
                ## Calculate vector of all feature types
                ###############################################################
                features <- target_list |>
                    lapply(
                        function(x) {
                            colnames(x[[1]])[-1]
                        }
                    ) |>
                    unlist()
                feature_types <- target_list |>
                    lapply(
                        function(x) {
                            n_features <- ncol(x$"data") - 1
                            outcome_type <- rep(x$"type", n_features)
                            return(outcome_type)
                        }
                    ) |>
                    unlist()
                ###############################################################
                ## Single DF to contain all features to calculate p-values for
                ###############################################################
                merged_df <- target_list |>
                    lapply(
                        function(x) {
                            x[[1]]
                        }
                    ) |>
                    merge_df_list()
                clustered_subs <- get_clustered_subs(esm[i, ])
                for (j in seq_along(features)) {
                    current_outcome_component <- merged_df[, c(1, j + 1)]
                    current_outcome_name <-
                        colnames(current_outcome_component)[2]
                    suppressWarnings(
                        pval <- get_cluster_pval(
                            clustered_subs,
                            current_outcome_component,
                            feature_types[j],
                            features[j],
                            cat_test = cat_test
                        )
                    )
                    target_col <- which(
                        paste0(current_outcome_name, "_pval") == colnames(esm)
                    )
                    esm[i, target_col] <- pval
                }
                return(esm[i, ])
            }
        )
        future::plan(future::sequential)
        esm <- do.call("rbind", esm_rows)
    }
    ###########################################################################
    # If min_pval is assigned, replace any p-value less than this with min_pval
    ###########################################################################
    if (!is.null(min_pval)) {
        esm <- esm |>
            numcol_to_numeric() |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::ends_with("_pval"),
                    ~ ifelse(. < min_pval, min_pval, .)
                )
            )
    }
    if (calculate_summaries) {
        esm <- summarize_pvals(esm)
    }
    return(esm)
}
