###################################################################################################
################################### Data Modeling #################################################

# Evaluating class balance (proportions of fraud vs. legitimate transactions). ---------------------
credit_fraud.df %>%
  count(TRANS_FRAUD) %>%
  mutate(Proportion = n / sum(n) * 100)


# Getting the main dataset for modeling. -----------------------------------------------------------
data_modeling.df <- credit_fraud.df %>%
  select(TRANS_FRAUD, CUSTOMER_ID, TERMINAL_ID, TRANS_AMOUNT, TRANS_DAY_OF_WEEK,
         TRANS_HOUR_OF_DAY, TRANS_WEEKEND, TRANS_NIGHT, CT_AVG_DAY_AMOUNT, 
         CT_AVG_DAY_TRANS, CT_TOT_DAY_AMOUNT, CT_TOT_DAY_TRANS, CT_FRAUD_HIST,
         TM_AVG_DAY_AMOUNT, TM_AVG_DAY_TRANS, TM_TOT_DAY_AMOUNT, TM_TOT_DAY_TRANS,
         TM_FRAUD_HIST, TM_CLIENTELE, CT_PURCHASE_TM)


# Splitting the dataset into fraud and non-fraud dataframes. ---------------------------------------
fraud_df <- data_modeling.df %>% filter(TRANS_FRAUD == 1)
non_fraud_df <- data_modeling.df %>% filter(TRANS_FRAUD == 0)


# Counting the number of fraudulent transactions. --------------------------------------------------
num_fraud <- nrow(fraud_df)

# Randomly sample from non-fraudulent transactions. ------------------------------------------------
set.seed(123)
non_fraud_sampled_df <- sample_n(non_fraud_df, num_fraud)

# Combining the undersampled non-fraud with fraud dataframes.
balanced_df <- bind_rows(fraud_df, non_fraud_sampled_df)

# Checking the balance of the new dataset.
table(balanced_df$TRANS_FRAUD)

rm(fraud_df)
rm(non_fraud_df)
rm(num_fraud)
rm(non_fraud_sampled_df)


# Validating balanced dataset with distribution. --------------------------------------------------
ggplot(balanced_df, aes(x = TRANS_AMOUNT, fill = factor(TRANS_FRAUD))) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6) +
  labs(x = "Log of Transaction Amount", fill = "Fraud Status") +
  facet_wrap(~TRANS_FRAUD)


# Transforming target variable as factor. ---------------------------------------------------------

balanced_df$TRANS_FRAUD <- as.factor(balanced_df$TRANS_FRAUD)

# Splitting the data into training and testing sets. -----------------------------------------------
set.seed(123)
data_split <- initial_split(balanced_df, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define the 5-fold cross-validation.
cv_folds <- vfold_cv(train_data, v = 5, strata = TRANS_FRAUD)


# Logistic Regression specification. ---------------------------------------------------------------
logistic_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Random Forest specification. ---------------------------------------------------------------------
rf_spec <- rand_forest(mtry = 3, trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# GBM specification. -------------------------------------------------------------------------------
gbm_spec <- boost_tree(trees = 1000) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# SVM specification. -------------------------------------------------------------------------------
svm_spec <- svm_rbf(cost = 1, rbf_sigma = 0.1) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Neural Network specification. --------------------------------------------------------------------
nnet_spec <- mlp(hidden_units = 10, penalty = 0.1) %>%
  set_engine("nnet") %>%
  set_mode("classification")


# List of models to iterate over. -----------------------------------------------------------------
models <- list(logistic = logistic_spec, 
               random_forest = rf_spec, 
               gbm = gbm_spec, 
               svm = svm_spec, 
               neural_network = nnet_spec)

# Function to fit models and perform cross-validation. --------------------------------------------
fit_models <- function(model, folds) {
  workflow() %>%
    add_model(model) %>%
    add_formula(TRANS_FRAUD ~ .) %>%
    fit_resamples(
      resamples = folds,
      metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
      control = control_resamples(save_pred = TRUE)
    )
}

# Map over the list of models to train and perform CV
results <- map(models, fit_models, folds = cv_folds)
