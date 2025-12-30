library(abcdutils)
library(here)
library(readr)
library(metasnf)
library(mice)
library(rlist)
library(visdat)

# The path_maker function helps with managing and date stamping long paths. The
# functions are just used here to help with reading and writing data. See
# ?path_maker for more information.
input_path <- path_maker(here(paste0("data/abcd/inputs")))
core_path <- path_maker(here(paste0("data/abcd/core")))
sub_path <- path_maker(here(paste0("data/abcd/subjects")))
fig_path <- path_maker(here(paste0("data/abcd/results/figures")))
proc_path <- path_maker(here(paste0("data/abcd/results/processed")))

iread <- function(path) {
    return(read_csv(input_path(path)))
}

missing_columns <- function(x) names(which(colSums(is.na(x)) > 0))

pairwise_correlations <- function(df, threshold = 0) {
    df <- na.omit(df)
    non_numeric_cols <- sapply(df, function(x) !is.numeric(x))
    df <- df[, !non_numeric_cols]
    correlations <- cor(df)
    diag(correlations) <- 0
    abs_cor <- abs(correlations)
    pairs <- t(combn(colnames(abs_cor), 2))
    paired_cors <- data.frame(pairs, cor = abs_cor[pairs])
    paired_cors <- paired_cors[paired_cors$"cor" > threshold, ]
    return(paired_cors)
}
top_correlating_vars <- function(df, threshold = 0) {
    df <- pairwise_correlations(df, threshold)
    max_cor_df <- data.frame(
        variable = as.character(),
        max_cor = as.numeric()
    )
    i <- 0
    while (nrow(df) > 0) {
        i <- i + 1
        row_with_max_cor <- which(df$"cor" == max(df$"cor"))[1]
        var_with_max_cor <- df[row_with_max_cor, "X1"]
        max_cor <- df[row_with_max_cor, "cor"]
        max_cor_df <- rbind(
            max_cor_df,
            data.frame(
                variable = var_with_max_cor,
                max_cor = max_cor
            )
        )
        df <- dplyr::filter(
            df,
            df$"X1" != var_with_max_cor & df$"X2" != var_with_max_cor
        )
    }
    return(max_cor_df)
}
build_pred_mat <- function(df, n_preds_dropped = 1) {
    pred_mat <- matrix(
        nrow = ncol(df),
        ncol = ncol(df),
        data = 1
    )
    colnames(pred_mat) <- colnames(df)
    rownames(pred_mat) <- colnames(df)
    diag(pred_mat) <- 0
    pred_mat_vars <- top_correlating_vars(df)
    vars_to_drop <- pred_mat_vars$"variable"[seq_len(n_preds_dropped)]
    print(paste0("Dropping: ", paste(vars_to_drop, collapse = ", ")))
    pred_mat[, vars_to_drop] <- 0
    return(pred_mat)
}
run_imputation <- function(df,
                           pred_mat_exclusions = 0,
                           visit_sequence = NULL,
                           parallel = FALSE,
                           ...) {
    df <- dplyr::select(df, -"subjectkey")
    df <- char_to_fac(df)
    if (!is.null(visit_sequence)) {
        df <- df[, visit_sequence[visit_sequence %in% colnames(df)]]
    } else {
        df <- df[, sample(colnames(df))]
    }
    pred_mat <- build_pred_mat(df, pred_mat_exclusions)
    if (!parallel) {
        imputed <- mice(
            df,
            m = 5,
            maxit = 10,
            predictorMatrix = pred_mat,
            ...
        )
    } else {
        imputed <- futuremice(
            df,
            m = 5,
            maxit = 10,
            parallelseed = 42,
            n.core = 5,
            predictorMatrix = pred_mat,
            ...
        )
    }
    return(
        list(
            visit_sequence = colnames(df),
            pred_mat = pred_mat,
            imputed = imputed
        )
    )
}

unij_l_df <- data.frame(iread("2025_06_09_unij_l_df.csv"))

set.seed(42)
unij_imputed_l <- run_imputation(unij_l_df, 1, nnet.MaxNWts = 3000)
saveRDS(unij_imputed_l, proc_path("unij_imputed_l.rds", TRUE))
complete(unij_imputed_l$"imputed", action = "long", include = FALSE) |>
    missing_columns()
