mh_p_cbcl <- read_csv("mh_p_cbcl.csv")

some_contributing_qs <- function(mh_p_cbcl, subscale, dsm) {
    if (dsm == FALSE) {
        missing_var <- paste0("cbcl_scr_syn_", subscale, "_m")
    } else if (dsm == TRUE) {
        missing_var <- paste0("cbcl_scr_dsm5_", subscale, "_m")
    }
    missing_qs <- mh_p_cbcl[, missing_var] |>
        unique() |>
        lapply(
            function(x) {
                if (nchar(x) > 2) {
                    return(x)
                } else {
                    return(NULL)
                }
            }
        )
    missing_qs <- missing_qs[lengths(missing_qs) != 0] |>
        unique() |>
        toString() # combine responses across subjects
    missing_qs <- gsub(",", "", missing_qs)
    missing_qs <- gsub("asr_", "", missing_qs)
    missing_qs <- gsub("cbcl_", "", missing_qs)
    missing_qs <- gsub("_p", "", missing_qs)
    missing_qs <- gsub("q", "", missing_qs)
    missing_qs <- missing_qs |>
        strsplit(split = " ") |>
        unlist() |>
        unique()
        print(
            paste0(length(missing_qs), " contributing questions found.")
        )
    missing_qs <- missing_qs |>
        paste0(collapse = " ")
    return(missing_qs)
}

some_contributing_qs(mh_p_cbcl, "anxdep", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "withdep", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "somatic", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "social", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "thought", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "attention", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "rulebreak", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "aggressive", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "internal", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "external", dsm = FALSE)
some_contributing_qs(mh_p_cbcl, "totprob", dsm = FALSE)

some_contributing_qs(mh_p_cbcl, "depress", dsm = TRUE)
some_contributing_qs(mh_p_cbcl, "anxdisord", dsm = TRUE)
some_contributing_qs(mh_p_cbcl, "somaticpr", dsm = TRUE)
some_contributing_qs(mh_p_cbcl, "adhd", dsm = TRUE)
some_contributing_qs(mh_p_cbcl, "opposit", dsm = TRUE)
some_contributing_qs(mh_p_cbcl, "conduct", dsm = TRUE)
