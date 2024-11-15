library(readr)
library(here)
library(abcdutils)
library(dplyr)
library(caret)
library(xgboost)
library(randomForest)
library(fastDummies)
library(skimr)
library(doParallel)
library(future)
library(future.apply)
library(progressr)

input_path <- path_maker(here(paste0("data/abcd/inputs")))
proc_path <- path_maker(here(paste0("data/abcd/results/processed")))
fig_path <- path_maker(here(paste0("data/abcd/results/figures")))
sub_path <- path_maker(here(paste0("data/abcd/subjects")))
model_path <- path_maker(here(paste0("data/abcd/results/models")))


# checkpoint
train_classification_df <- read_csv(proc_path("2024_11_07_train_classification_df.csv"))

# checkpoint
prep_data <- function(df, standardize = FALSE) {
    df <- dplyr::select(df, -"subjectkey")
    df <- dummy_cols(
        df,
        select_columns = "race",
        remove_selected_columns = TRUE,
        remove_most_frequent_dummy = TRUE
    )
    ordered_factors <- c(
        "sex",
        "household_income",
        "pubertal_status_p",
        "family_fight_p",
        "family_outdo_p",
        "helps_hurt_p",
        "helpful_y",
        "considerate_y",
        "helps_hurt_y",
        "family_criticize_p",
        "family_outdo_y",
        "family_angry_p",
        "helpful_p",
        "family_throw_p",
        "family_hit_p",
        "family_peaceful_y",
        "family_throw_y",
        "race_black",
        "race_hispanic",
        "race_other" 
    )
    df[ordered_factors] <- lapply(df[ordered_factors], as.ordered)
    if (standardize) {
        df[] <- lapply(df, as.numeric)
        df <- as.data.frame(scale(df))
    }
    df$"cluster" <- as.factor(df$"cluster")
    return(df)
}

## Caret CV

# checkpoint
train_control <- caret::trainControl(
    method = "cv",             # Cross-validation
    number = 5,                # 5-fold CV
    search = "grid",           # Grid search
    verboseIter = TRUE
)

### XGBoost
xgb_tune_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.1, 0.2),
  gamma = c(0, 0.25, 0.5),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 5),
  subsample = c(0.5, 0.75, 1)
)

xgb_model <- train(
    cluster ~ .,
    data = prep_data(train_classification_df),
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_tune_grid
)

saveRDS(xgb_model, model_path("xgb_subtype_classifier_1.rds", TRUE))
xgb_model <- readRDS(model_path("2024_11_07_xgb_subtype_classifier_1.rds"))

saveRDS(rf_model, model_path("rf_subtype_classifier_1.rds", TRUE))
rf_model <- readRDS(model_path("2024_11_07_rf_subtype_classifier_1.rds"))
