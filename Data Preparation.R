#install packages and library
install.packages("tidyverse")
library(tidyverse)

# Read in Data
app_train <- read_csv("/content/application_train.csv", show_col_types = FALSE)
app_test  <- read_csv("/content/application_test.csv",  show_col_types = FALSE)

bureau <- read_csv("/content/bureau.csv", show_col_types = FALSE)
prev   <- read_csv("/content/previous_application.csv", show_col_types = FALSE)
instå   <- read_csv("/content/installments_payments.csv", show_col_types = FALSE)

fit_params <- function(train_df) {
  params <- list()
  
  # EXT_SOURCE medians (train only)
  params$ext_median <- train_df %>%
    summarise(
      EXT_SOURCE_1 = median(EXT_SOURCE_1, na.rm = TRUE),
      EXT_SOURCE_2 = median(EXT_SOURCE_2, na.rm = TRUE),
      EXT_SOURCE_3 = median(EXT_SOURCE_3, na.rm = TRUE)
    )
  
  # Binning thresholds (example: age bins, income bins)
  # Use quantiles from train only (no leakage)
  params$age_bins <- quantile((-train_df$DAYS_BIRTH) / 365.25,
                              probs = c(0, .2, .4, .6, .8, 1),
                              na.rm = TRUE)
  
  params$inc_bins <- quantile(train_df$AMT_INCOME_TOTAL,
                              probs = c(0, .25, .5, .75, 1),
                              na.rm = TRUE)
  
  # Store factor levels from train so test aligns
  cat_cols <- c("NAME_CONTRACT_TYPE","CODE_GENDER","FLAG_OWN_CAR","FLAG_OWN_REALTY",
                "NAME_INCOME_TYPE","NAME_EDUCATION_TYPE","NAME_FAMILY_STATUS",
                "NAME_HOUSING_TYPE","OCCUPATION_TYPE","ORGANIZATION_TYPE")
  params$cat_levels <- map(cat_cols, ~ sort(unique(train_df[[.x]])))
  names(params$cat_levels) <- cat_cols
  
  params
}

transform_app <- function(df, params, is_train = FALSE) {
  
  out <- df %>%
    mutate(
      # 2A) Fix DAYS_EMPLOYED anomaly (365243 placeholder)
      DAYS_EMPLOYED_ANOM = if_else(DAYS_EMPLOYED == 365243, 1L, 0L),
      DAYS_EMPLOYED = if_else(DAYS_EMPLOYED == 365243, NA_real_, DAYS_EMPLOYED),
      
      # 2B) Convert day-based demographics to positive units
      AGE_YEARS = (-DAYS_BIRTH) / 365.25,
      EMP_YEARS = (-DAYS_EMPLOYED) / 365.25,
      
      # 2C) EXT_SOURCE missing indicators + impute with TRAIN medians
      EXT1_MISS = if_else(is.na(EXT_SOURCE_1), 1L, 0L),
      EXT2_MISS = if_else(is.na(EXT_SOURCE_2), 1L, 0L),
      EXT3_MISS = if_else(is.na(EXT_SOURCE_3), 1L, 0L),
      
      EXT_SOURCE_1 = if_else(is.na(EXT_SOURCE_1), params$ext_median$EXT_SOURCE_1, EXT_SOURCE_1),
      EXT_SOURCE_2 = if_else(is.na(EXT_SOURCE_2), params$ext_median$EXT_SOURCE_2, EXT_SOURCE_2),
      EXT_SOURCE_3 = if_else(is.na(EXT_SOURCE_3), params$ext_median$EXT_SOURCE_3, EXT_SOURCE_3),
      
      # 2D) Common financial ratios (guard against divide-by-zero)
      CREDIT_TO_INCOME = AMT_CREDIT / pmax(AMT_INCOME_TOTAL, 1),
      ANNUITY_TO_INCOME = AMT_ANNUITY / pmax(AMT_INCOME_TOTAL, 1),
      GOODS_TO_CREDIT = AMT_GOODS_PRICE / pmax(AMT_CREDIT, 1),
      
      LOG_INCOME = log1p(AMT_INCOME_TOTAL),
      LOG_CREDIT = log1p(AMT_CREDIT),
      LOG_ANNUITY = log1p(AMT_ANNUITY),
      LOG_GOODS = log1p(AMT_GOODS_PRICE),
      
      ORGANIZATION_TYPE = collapse_rare_levels(ORGANIZATION_TYPE, min_n = 200),
      OCCUPATION_TYPE   = collapse_rare_levels(OCCUPATION_TYPE,   min_n = 200),
      # Loan-to-value proxy (Home Credit doesn’t have property value; goods_price is often used)
      LTV_PROXY = AMT_CREDIT / pmax(AMT_GOODS_PRICE, 1),
      
      # Down payment rate proxy
      DOWNPAY_RATE = (AMT_CREDIT - AMT_GOODS_PRICE) / pmax(AMT_CREDIT, 1),
      
      # Payment burden proxy (inverse affordability)
      CREDIT_TERM = AMT_CREDIT / pmax(AMT_ANNUITY, 1),
      
      # 2E) Interaction examples (keep it small—don’t explode columns)
      EXT_AVG = (EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3) / 3,
      EXT_STD = pmap_dbl(list(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3),
                         ~ sd(c(..1, ..2, ..3))),
      AGE_x_EXTAVG = AGE_YEARS * EXT_AVG,
      
      # 2F) Binned variables (bins computed from TRAIN only)
      AGE_BIN = cut(AGE_YEARS, breaks = unique(params$age_bins), include.lowest = TRUE),
      INCOME_BIN = cut(AMT_INCOME_TOTAL, breaks = unique(params$inc_bins), include.lowest = TRUE),
      
      # 2G) Generic missing indicators for some high-missing columns you found in EDA
      OCCUPATION_MISS = if_else(is.na(OCCUPATION_TYPE), 1L, 0L),
      OWN_CAR_AGE_MISS = if_else(is.na(OWN_CAR_AGE), 1L, 0L)
    )
  
  # 2H) Force categorical columns to have train levels (test-safe)
  for (col in names(params$cat_levels)) {
    if (col %in% names(out)) {
      out[[col]] <- factor(out[[col]], levels = params$cat_levels[[col]])
      out[[col]] <- fct_explicit_na(out[[col]], na_level = "Missing")
      # Removed the line with fct_unknown as it's not a valid function.
      # Levels not in params$cat_levels[[col]] become NA, which are then handled by fct_explicit_na.
    }
  }
  
  out
}

params <- fit_params(app_train)

train_app_clean <- transform_app(app_train, params, is_train = TRUE)
test_app_clean  <- transform_app(app_test,  params, is_train = FALSE)


train_full <- train_full %>%
  mutate(
    INCOME_PER_PERSON = AMT_INCOME_TOTAL / pmax(CNT_FAM_MEMBERS, 1),
    ANNUITY_PER_PERSON = AMT_ANNUITY / pmax(CNT_FAM_MEMBERS, 1),
    CHILDREN_RATIO = CNT_CHILDREN / pmax(CNT_FAM_MEMBERS, 1)
  )

test_full <- test_full %>%
  mutate(
    INCOME_PER_PERSON = AMT_INCOME_TOTAL / pmax(CNT_FAM_MEMBERS, 1),
    ANNUITY_PER_PERSON = AMT_ANNUITY / pmax(CNT_FAM_MEMBERS, 1),
    CHILDREN_RATIO = CNT_CHILDREN / pmax(CNT_FAM_MEMBERS, 1)
  )

bureau_agg <- bureau %>%
  mutate(
    IS_ACTIVE = if_else(CREDIT_ACTIVE == "Active", 1L, 0L),
    IS_CLOSED = if_else(CREDIT_ACTIVE == "Closed", 1L, 0L),
    OVERDUE = pmax(AMT_CREDIT_SUM_OVERDUE, 0, na.rm = TRUE),
    DEBT = pmax(AMT_CREDIT_SUM_DEBT, 0, na.rm = TRUE),
    LIMIT = pmax(AMT_CREDIT_SUM_LIMIT, 0, na.rm = TRUE),
    CREDIT_SUM = pmax(AMT_CREDIT_SUM, 0, na.rm = TRUE),
    DEBT_RATIO = DEBT / pmax(CREDIT_SUM, 1)
  ) %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    BUREAU_COUNT = n(),
    BUREAU_ACTIVE_COUNT = sum(IS_ACTIVE, na.rm = TRUE),
    BUREAU_CLOSED_COUNT = sum(IS_CLOSED, na.rm = TRUE),
    BUREAU_OVERDUE_SUM = sum(OVERDUE, na.rm = TRUE),
    BUREAU_DEBT_SUM = sum(DEBT, na.rm = TRUE),
    BUREAU_DEBT_RATIO_MEAN = mean(DEBT_RATIO, na.rm = TRUE),
    .groups = "drop"
  )

prev_agg <- prev %>%
  mutate(
    IS_APPROVED = if_else(NAME_CONTRACT_STATUS == "Approved", 1L, 0L),
    IS_REFUSED  = if_else(NAME_CONTRACT_STATUS == "Refused", 1L, 0L)
  ) %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    PREV_APP_COUNT = n(),
    PREV_APPROVED_RATE = mean(IS_APPROVED, na.rm = TRUE),
    PREV_REFUSED_COUNT = sum(IS_REFUSED, na.rm = TRUE),
    PREV_AMT_CREDIT_MEAN = mean(AMT_CREDIT, na.rm = TRUE),
    PREV_AMT_ANNUITY_MEAN = mean(AMT_ANNUITY, na.rm = TRUE),
    .groups = "drop"
  )
inst_agg <- inst %>%
  mutate(
    DAYS_LATE = pmax(DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT, 0, na.rm = TRUE),
    IS_LATE = if_else(DAYS_ENTRY_PAYMENT > DAYS_INSTALMENT, 1L, 0L),
    PAY_RATIO = AMT_PAYMENT / pmax(AMT_INSTALMENT, 1)
  ) %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    INST_COUNT = n(),
    LATE_PCT = mean(IS_LATE, na.rm = TRUE),
    DAYS_LATE_MEAN = mean(DAYS_LATE, na.rm = TRUE),
    PAY_RATIO_MEAN = mean(PAY_RATIO, na.rm = TRUE),
    PAY_RATIO_MIN  = min(PAY_RATIO, na.rm = TRUE),
    .groups = "drop"
  )

train_full <- train_app_clean %>%
  left_join(bureau_agg, by = "SK_ID_CURR") %>%
  left_join(prev_agg,   by = "SK_ID_CURR") %>%
  left_join(inst_agg,   by = "SK_ID_CURR")

test_full <- test_app_clean %>%
  left_join(bureau_agg, by = "SK_ID_CURR") %>%
  left_join(prev_agg,   by = "SK_ID_CURR") %>%
  left_join(inst_agg,   by = "SK_ID_CURR")

train_full <- train_full %>%
  mutate(
    NO_BUREAU = if_else(is.na(BUREAU_COUNT), 1L, 0L),
    BUREAU_COUNT = replace_na(BUREAU_COUNT, 0),
    PREV_APP_COUNT = replace_na(PREV_APP_COUNT, 0),
    INST_COUNT = replace_na(INST_COUNT, 0)
  )

test_full <- test_full %>%
  mutate(
    NO_BUREAU = if_else(is.na(BUREAU_COUNT), 1L, 0L),
    BUREAU_COUNT = replace_na(BUREAU_COUNT, 0),
    PREV_APP_COUNT = replace_na(PREV_APP_COUNT, 0),
    INST_COUNT = replace_na(INST_COUNT, 0)
  )
train_y <- train_full$TARGET

train_x <- train_full %>% select(-TARGET)
test_x  <- test_full

combined <- bind_rows(
  train_x %>% mutate(.is_train = 1L),
  test_x  %>% mutate(.is_train = 0L)
)

# Explicitly convert character columns to factors
combined <- combined %>% 
  mutate(across(where(is.character), as.factor))

# Impute remaining NAs before model.matrix
# Impute numeric NAs with 0
combined <- combined %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Ensure factor levels are consistent and NAs are explicit for all factors
combined <- combined %>% 
  mutate(across(where(is.factor), ~ fct_explicit_na(., na_level = "Missing")))

# model.matrix creates consistent dummies across combined set
# Use na.action = na.pass to ensure no rows are dropped
X <- model.matrix(~ . -1, data = combined %>% select(-SK_ID_CURR), na.action = na.pass) %>% as.data.frame()

# Impute any NAs in the resulting design matrix X with 0
X <- X %>% mutate(across(where(is.numeric), ~ replace_na(., 0)))

X$.is_train <- combined$.is_train
X$SK_ID_CURR <- combined$SK_ID_CURR

X_train <- X %>% filter(.is_train == 1L) %>% select(-.is_train)
X_test  <- X %>% filter(.is_train == 0L) %>% select(-.is_train)

# Put TARGET back only for train
train_ready <- X_train %>% mutate(TARGET = train_y)
test_ready  <- X_test

#TARGET only in train
stopifnot("TARGET" %in% names(train_ready))
stopifnot(!("TARGET" %in% names(test_ready)))

#Same feature columns (excluding TARGET)
stopifnot(setequal(setdiff(names(train_ready), "TARGET"), names(test_ready)))

#No leakage: bins/medians came from params computed on train only
params$ext_median
params$age_bins

