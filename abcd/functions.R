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
