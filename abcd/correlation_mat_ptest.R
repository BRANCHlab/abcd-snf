# df: A data frame to be converted into a correlation matrix. Must contain
#     only numeric feature columns (no ID / group / etc. columns)
# method: Can either be "spearman" or "pearson". Specifies which type of
#     correlation method to use.
# fisher_z: Control whether or not the correlations should be fisher Z
#     transformed.
calculate_cor_mat <- function(df, method = "pearson", fisher_z = FALSE) {
    if (method == "spearman") {
        future::plan("multisession")
        n <- ncol(df)
        spearman_mat <- matrix(NA, n, n)
        colnames(spearman_mat) <- rownames(spearman_mat) <- colnames(df)
        results <- pbapply::pblapply(
            1:n,
            function(i) {
                sapply(
                    i:ncol(df),
                    function(j) {
                        cor(df[[i]], df[[j]], method = "spearman")
                    }
                )
            },
            cl = future::availableCores()
        )
        for (i in 1:n) {
            spearman_mat[i, i:n] <- results[[i]]
            spearman_mat[i:n, i] <- results[[i]]
        }
        mat <- spearman_mat
        future::plan("sequential")
    } else if (method == "pearson") {
        mat <- cor(df, method = "pearson")
    }
    if (fisher_z) {
        mat <- atanh(mat) 
    }
    diag(mat) <- NA
    return(mat)
}

# This function is set up as an overall test to see if the correlation
# structures between two data frames are significantly different.
# We are looking at absolute value of element wise comparisons between
# all the correlations. 
# It is testing if the average magnitude of pairwise correlation differences
# is greater than expected by chance.
permutation_test_corrs <- function(df1,
                                   df2,
                                   method = "pearson",
                                   fisher_z = FALSE,
                                   n_perms = 1000) {
    cor1 <- calculate_cor_mat(df1, method, fisher_z)
    cor2 <- calculate_cor_mat(df2, method, fisher_z)
    cor1_upper <- cor1[upper.tri(cor1)]
    cor2_upper <- cor2[upper.tri(cor2)]
    observed_diff <- mean(abs(cor1_upper - cor2_upper), na.rm = TRUE)
    n1 <- nrow(df1)
    n2 <- nrow(df2)
    n <- n1 + n2
    perm_diffs <- lapply(
        seq_len(n_perms),
        function(i) {
            merged_df <- rbind(df1, df2)[sample(n), ]
            perm_cor1 <- calculate_cor_mat(merged_df[1:n1, ], method, fisher_z)
            perm_cor2 <- calculate_cor_mat(merged_df[(n1+1):n, ], method, fisher_z)
            perm_cor1_upper <- perm_cor1[upper.tri(perm_cor1)]
            perm_cor2_upper <- perm_cor2[upper.tri(perm_cor2)]
            return(mean(abs(perm_cor1_upper - perm_cor2_upper), na.rm = TRUE))
        }
    ) |> unlist()
    p_value <- mean(perm_diffs >= observed_diff)
    return(
        list(
            observed_diff = observed_diff,
            p_value = p_value,
            perm_diffs = perm_diffs
        )
    )
}

set.seed(42) # <- make sure you set a random seed! otherwise you won't get the same results each time.

# Example 1: mtcars vs. two times mtcars (p value should be 1 because no difference)
example_df_1 <- mtcars
example_df_2 <- mtcars * 2
results_ex1 <- permutation_test_corrs(example_df_1, example_df_2, n_perms = 1000)
results_ex1$"p_value" < 0.05 # FALSE

# Example 2: mtcars vs. random noise (p value should be < 0.05 because low correlation in noise)
random_df <- data.frame(matrix(rnorm(121), nrow = 11))
colnames(random_df) <- colnames(example_df_1)
results_ex2 <- permutation_test_corrs(example_df_1, random_df, n_perms = 1000)
results_ex2$"p_value" < 0.05 # TRUE

#------------------------------------------------------------------------------

# This one is doing permutation tests for each individual correlation
# Can be used to check which specific correlations are significantly different
permutation_test_corrs2 <- function(df1,
                                    df2,
                                    method = "pearson",
                                    fisher_z = TRUE,
                                    n_perms,
                                    procs = 8,
                                    style = "loop") {
    cor1 <- calculate_cor_mat(df1, method, fisher_z)
    cor2 <- calculate_cor_mat(df2, method, fisher_z)
    obs_diff <- cor1 - cor2
    n1 <- nrow(df1)
    n2 <- nrow(df2)
    n <- n1 + n2
    if (style == "lapply") {
        perm_mat_diffs <- lapply(
            seq_len(n_perms),
            function(i) {
                merged_df <- rbind(df1, df2)[sample(n), ]
                perm_cor1 <- calculate_cor_mat(merged_df[1:n1, ], method, fisher_z)
                perm_cor2 <- calculate_cor_mat(merged_df[(n1+1):n, ], method, fisher_z)
                return(perm_cor1 - perm_cor2)
            }
        )
    }
    if (style == "loop") {
        perm_mat_diffs <- vector("list", n_perms)
        for (i in seq_len(n_perms)) {
            merged_df <- rbind(df1, df2)[sample(n), ]
            perm_cor1 <- calculate_cor_mat(merged_df[1:n1, ], method, fisher_z)
            perm_cor2 <- calculate_cor_mat(merged_df[(n1+1):n, ], method, fisher_z)
            perm_mat_diffs[[i]] <- perm_cor1 - perm_cor2
        }
    }
    perm_mat_diffs <- append(perm_mat_diffs, list(obs_diff) , after = 0)
    return(
        list(
            cor1 = cor1,
            cor2 = cor2,
            obs_diff = cor1 - cor2,
            perm_mat_diffs = perm_mat_diffs
        )
    )
}

flatten_mats <- function(perm_results) {
    mat_list <- perm_results$"perm_mat_diffs"
    pairwise_combos <- combn(
        colnames(mat_list[[1]]),
        2,
        simplify = FALSE
    )
    names(pairwise_combos) <- lapply(
        pairwise_combos,
        function(x) {
            paste(x, collapse = "_vs_")
        }
    )
    elewise_perm_diffs <- lapply(
        pairwise_combos,
        function(combo) {
            lapply(
                mat_list,
                function(mat) {
                    mat[combo[[1]], combo[[2]]]
                }
            ) |> unlist() 
        }
    )
    elewise_obs_diffs <- lapply(
        pairwise_combos,
        function(combo) {
            obs_diff <- perm_results$"obs_diff"[combo[[1]], combo[[2]]]
        }
    )
    stronger_p_values <- lapply(
        seq_along(pairwise_combos),
        function(x) {
            od <- elewise_obs_diffs[[x]]
            pd <- elewise_perm_diffs[[x]]
            stronger_p <- mean(abs(od) <= abs(pd))
            return(stronger_p)
        }
    )
    weaker_p_values <- lapply(
        seq_along(pairwise_combos),
        function(x) {
            od <- elewise_obs_diffs[[x]]
            pd <- elewise_perm_diffs[[x]]
            weaker_p <- mean(abs(od) >= abs(pd))
            return(weaker_p)
        }
    )
    more_pos_p_values <- lapply(
        seq_along(pairwise_combos),
        function(x) {
            od <- elewise_obs_diffs[[x]]
            pd <- elewise_perm_diffs[[x]]
            # Is the observed diff more positive than expected?
            more_pos_p <- mean(od <= pd)
            return(more_pos_p)
        }
    )
    more_neg_p_values <- lapply(
        seq_along(pairwise_combos),
        function(x) {
            od <- elewise_obs_diffs[[x]]
            pd <- elewise_perm_diffs[[x]]
            # Is the observed diff more negative than expected?
            more_neg_p <- mean(od >= pd)
            return(more_neg_p)
        }
    )
    names(stronger_p_values) <- names(pairwise_combos)
    names(weaker_p_values) <- names(pairwise_combos)
    names(more_pos_p_values) <- names(pairwise_combos)
    names(more_neg_p_values) <- names(pairwise_combos)
    return(
        list(
            stronger_p_values = stronger_p_values,
            weaker_p_values = weaker_p_values,
            more_pos_p_values = more_pos_p_values,
            more_neg_p_values = more_neg_p_values
        )
    )
}

# Example 3: which correlations in mtcars were stronger than data generated by random noise?
results_ex3 <- permutation_test_corrs2(example_df_1, random_df, n_perms = 1000)
elewise_results <- flatten_mats(results_ex2)
elewise_results$"stronger_p_values" < 0.05
