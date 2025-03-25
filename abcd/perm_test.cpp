#include <RcppArmadillo.h>
//#include <algorithm> // For std::shuffle
#include <random>    // For random number generation

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat cor_fn_cpp(NumericMatrix df) {
    // Compute correlation matrix using Armadillo
    arma::mat mat = as<arma::mat>(df);
    return cor(mat);
}

// [[Rcpp::export]]
double mean_cor_cpp(arma::mat cor_mat) {
    // Extract upper triangular part, compute absolute mean
    uvec upper_indices = trimatu_ind(size(cor_mat), 1);
    return mean(abs(cor_mat(upper_indices)));
}

// [[Rcpp::export]]
List perm_test_cpp(NumericMatrix df1, NumericMatrix df2, int n) {
    int n1 = df1.nrow();
    int n2 = df2.nrow();
    int total_rows = n1 + n2;
    
    // Compute observed mean correlation difference
    double mean_cor_1 = mean_cor_cpp(cor_fn_cpp(df1));
    double mean_cor_2 = mean_cor_cpp(cor_fn_cpp(df2));
    double mean_cor_diff = mean_cor_1 - mean_cor_2;
    
    // Preallocate results vector
    NumericVector diffs(n);
    
    // Stack datasets once
    NumericMatrix stacked_df(total_rows, df1.ncol());
    for (int i = 0; i < n1; i++) stacked_df(i, _) = df1(i, _);
    for (int i = 0; i < n2; i++) stacked_df(n1 + i, _) = df2(i, _);

    // Use R's RNG via Rcpp::RNGScope
    Rcpp::RNGScope scope;  // Ensures R's RNG is used

    //// Random number generator for shuffling
    //std::random_device rd;
    //std::mt19937 g(rd());
    //IntegerVector indices = seq_len(total_rows) - 1; // 0-based indexing

    
    // Permutation test loop
    for (int i = 0; i < n; i++) {
        IntegerVector indices = Rcpp::sample(total_rows, total_rows, false) - 1; // sample from 0 to total_rows-1
        //std::shuffle(indices.begin(), indices.end(), g);

        // Create new shuffled datasets
        NumericMatrix random_df1(n1, df1.ncol());
        NumericMatrix random_df2(n2, df2.ncol());

        for (int j = 0; j < n1; j++) random_df1(j, _) = stacked_df(indices[j], _);
        for (int j = 0; j < n2; j++) random_df2(j, _) = stacked_df(indices[n1 + j], _);

        // Compute mean correlations
        double perm_mean_cor_1 = mean_cor_cpp(cor_fn_cpp(random_df1));
        double perm_mean_cor_2 = mean_cor_cpp(cor_fn_cpp(random_df2));

        diffs[i] = perm_mean_cor_1 - perm_mean_cor_2;
    }

    // Compute p-value
    double p_value = mean(diffs > mean_cor_diff);

    return List::create(
        _["observed_diff"] = mean_cor_diff,
        _["simulated_diffs"] = diffs,
        _["p_value"] = p_value
    );
}
