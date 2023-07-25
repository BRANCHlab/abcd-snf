library(dplyr)
library(abcdutils)
library(metasnf)
library(readr)
library(here)
library(ggplot2)
library(pheatmap)
library(rlist)

## IMPORTING DATA #################################################

# Functions to import data from carbon
proc_path <- path_maker(here(paste0("data/abcd/results/processed")))

# Structure containing all input data
data_list_0_0_train <-
    list.load(proc_path("2023_07_17_data_list_0_0_train.rdata"))

# Structure containing all outcome data
outcome_list_0_0_train <-
    list.load(proc_path("2023_07_17_outcome_list_0_0_train.rdata"))

## EXTEND THE OMs OF THE NON-FAIR DATA ############################

# Outcome matrix (full data)
om_0_0_train <- read_csv(proc_path("2023_07_25_om_0_0_train.csv"))

# Outcome matrix (tight full data)
om_tight_0_0_train <- read_csv(proc_path("2023_07_25_om_tight_0_0_train.csv"))

# Extending the output matrix to include CBCL p-values
eom_0_0_train <- extend_om(om_0_0_train, outcome_list_0_0_train)

# Exporting and reloading the output matrix
write_csv(eom_0_0_train, proc_path("eom_0_0_train.csv", date = TRUE))

# Extending the tight output matrix to include CBCL p-values
eom_tight_0_0_train <- extend_om(om_tight_0_0_train, outcome_list_0_0_train)

# Exporting and reloading the output matrix
write_csv(
    eom_tight_0_0_train, proc_path("eom_tight_0_0_train.csv", date = TRUE)
)


## RE-DO SNF FOR FAIR DATA

## No race and SES
to_remove <- c("race", "income")

data_list_fair_0_0_train <- list_remove(
    data_list_0_0_train,
    "race",
    "income"
)

# Building the design matrix which contains all settings for 100 SNF runs
design_matrix_fair <- generate_design_matrix(
    data_list_fair_0_0_train,
    nrow = 1000,
    min_removed = 1,
    seed = 42
)

# Exporting and reloading the design matrix
write_csv(
    design_matrix_fair,
    proc_path("design_matrix_fair.csv", date = TRUE)
)

# Building the design matrix which contains all settings for 100 SNF runs
design_matrix_tight_fair <- generate_design_matrix(
    data_list_fair_0_0_train,
    nrow = 1000,
    min_removed = 1,
    max_removed = 3,
    seed = 42
)

# Exporting and reloading the design matrix
write_csv(
    design_matrix_tight_fair,
    proc_path("design_matrix_tight_fair.csv", date = TRUE)
)

## THE ACTUAL SNF

# Generating output matrix
om_fair_0_0_train <- execute_design_matrix(
   data_list_fair_0_0_train,
   design_matrix_fair,
   processes = "max"
)

# Exporting and reloading the output matrix
write_csv(om_fair_0_0_train, proc_path("om_fair_0_0_train.csv", date = TRUE))

om_fair_0_0_train <- read_csv(proc_path("2023_07_25_om_fair_0_0_train.csv"))


# Generating tight output matrix
om_tight_fair_0_0_train <- execute_design_matrix(
   data_list_fair_0_0_train,
   design_matrix_tight_fair,
   processes = "max"
)

# Exporting and reloading the output matrix
write_csv(om_tight_fair_0_0_train, proc_path("om_tight_fair_0_0_train.csv", date = TRUE))

om_tight_fair_0_0_train <- read_csv(proc_path("2023_07_25_om_tight_fair_0_0_train.csv"))

## EXTENDING THESE OMS

# Extending the output matrix to include CBCL p-values
eom_fair_0_0_train <- extend_om(om_fair_0_0_train, outcome_list_0_0_train)

# Exporting and reloading the output matrix
write_csv(eom_fair_0_0_train, proc_path("eom_fair_0_0_train.csv", date = TRUE))


# Extending the output matrix to include CBCL p-values
eom_tight_fair_0_0_train <- extend_om(om_tight_fair_0_0_train, outcome_list_0_0_train)

# Exporting and reloading the output matrix
write_csv(eom_tight_fair_0_0_train, proc_path("eom_tight_fair_0_0_train.csv", date = TRUE))

print("All done!")
