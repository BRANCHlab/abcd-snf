library(abcdutils)
library(here)
library(dplyr)
library(readr)
library(metasnf)
library(mice)
library(rlist)

input_path <- path_maker(here("data/abcd/inputs"))
core_path <- path_maker(here("data/abcd/core"))
sub_path <- path_maker(here("data/abcd/subjects"))
fig_path <- path_maker(here("data/abcd/results/figures"))
proc_path <- path_maker(here("data/abcd/results/processed"))

# Subjects
mtbi_subs <- read_subjects(sub_path("2025_05_22_mtbi_subs.csv"))
unij_subs <- read_subjects(sub_path("2025_05_22_unij_subs.csv"))

read <- function(path) {
    read_csv(input_path(path))
}

import <- function(path) {
    return(abcdutils::abcd_load(core_path(path)))
}

# CHECKPOINT
mtbi_train_df <- data.frame(read_csv(proc_path("2025_05_26_mtbi_train_df.csv")))
mtbi_test_df <- data.frame(read_csv(proc_path("2025_05_26_mtbi_test_df.csv")))
unij_df <- data.frame(read_csv(proc_path("2025_05_26_unij_df.csv")))

# Function that checks which variables have the biggest correlations
pairwise_correlations <- function(df, threshold = 0) {
    # Remove missing data to help calculate correlations
    df <- na.omit(df)
    # Remove non-numeric columns
    non_numeric_cols <- sapply(df, function(x) !is.numeric(x))
    df <- df[, !non_numeric_cols]
    # Calculate correlations
    correlations <- cor(df)
    # Ignore self-correlations
    diag(correlations) <- 0
    # Take absolute value of the correlations, as strong anti-correlations also
    # lead to strong linear dependence
    abs_cor <- abs(correlations)
    # Create dataframe of all pairs of correlations
    pairs <- t(combn(colnames(abs_cor), 2))
    paired_cors <- data.frame(pairs, cor = abs_cor[pairs])
    # Filter to only correlations bigger than a specified threshold
    paired_cors <- paired_cors[paired_cors$"cor" > threshold, ]
    return(paired_cors)
}

# Function to help sort variables by pairwise correlations
top_correlating_vars <- function(df, threshold = 0) {
    # Dataframe highlighting strongest correlations
    df <- pairwise_correlations(df, threshold)
    # Iteratively build up an ordered correlations matrix by:
    #   - Record one of the two variables contributing to the largest
    #     pairwise correlation
    #   - Remove that variables from the list of all pairs of correlations
    #   - Repeat until all variables have been removed
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

# Prediction matrix construction
build_pred_mat <- function(df, n_predictors = 1) {
    pred_mat <- matrix(
        nrow = ncol(df),
        ncol = ncol(df),
        data = 1
    )
    colnames(pred_mat) <- colnames(df)
    rownames(pred_mat) <- colnames(df)
    diag(pred_mat) <- 0
    pred_mat_vars <- top_correlating_vars(df)
    vars_to_drop <- pred_mat_vars$"variable"[seq_len(n_predictors)]
    print(paste0("Dropping: ", paste(vars_to_drop, collapse = ", ")))
    pred_mat[, vars_to_drop] <- 0
    return(pred_mat)
}

# pred_mat_exclusions controls how many of the top most correlated variables
# are excluded as predictors from the predictor matrix.
run_imputation <- function(df,
                           pred_mat_exclusions = 0,
                           visit_sequence = NULL,
                           parallel = FALSE) {
    df <- dplyr::select(df, -"subjectkey")
    df <- char_to_fac(df)
    if (!is.null(visit_sequence)) {
        df <- df[, visit_sequence[visit_sequence %in% colnames(df)]]
    } else {
        df <- df[, sample(colnames(df))]
    }
    # Construct predictor matrix
    pred_mat <- build_pred_mat(df, pred_mat_exclusions)
    # Construct methods vector and set scanner_id to ""
    if (!parallel) {
        imputed <- mice(
            df,
            m = 5,
            maxit = 10,
            predictorMatrix = pred_mat
        )
    } else {
        imputed <- futuremice(
            df,
            m = 5,
            maxit = 10,
            parallelseed = 42,
            n.core = 5,
            predictorMatrix = pred_mat
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

# Copy visit sequence
mtbi_train_imputed <- readRDS(proc_path("2025_05_26_mtbi_train_imputed.rds"))
vs <- mtbi_train_imputed$"visit_sequence"

#Uninjured
set.seed(42)
start <- Sys.time()
unij_imputed <- run_imputation(unij_df, 1, vs, FALSE)
print(Sys.time() - start)
saveRDS(unij_imputed, proc_path("unij_imputed.rds", TRUE))
