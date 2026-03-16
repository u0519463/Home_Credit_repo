
# Home Credit Default Risk Project

**Business Analytics – Data Preparation & Modeling**

---

# Project Overview

This project predicts loan default risk using the **Home Credit Default Risk** dataset.  
The objective is to build a clean, reproducible pipeline that prepares raw financial data and trains predictive models capable of identifying borrowers likely to default.

The project follows the **CRISP‑DM framework**:

1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modeling
5. Evaluation

The repository contains both the **data preparation pipeline** and the **modeling notebook** used to evaluate multiple machine‑learning approaches and generate predictions.

---

# Data Preparation Script

File: `data_preparation.R`

## Purpose

The `data_preparation.R` script builds a modeling‑ready dataset from raw Home Credit data using **reusable functions**. It ensures that:

- Transformations are consistent between training and test data
- No information from the test set leaks into training
- Train and test datasets contain identical feature columns (except `TARGET`)

---

# What the Data Preparation Script Does

## 1. Cleans Application Data

### Fixes Data Issues Identified in EDA

- Replaces `DAYS_EMPLOYED == 365243` placeholder values with `NA`
- Creates anomaly indicator: `DAYS_EMPLOYED_ANOM`
- Converts negative day-based features to positive years:

  - `AGE_YEARS`
  - `EMP_YEARS`

---

## 2. Handles Missing Values

### EXT_SOURCE Variables

- Imputes `EXT_SOURCE_1`, `EXT_SOURCE_2`, `EXT_SOURCE_3` using **training-only medians**
- Adds missing indicators:

  - `EXT1_MISS`
  - `EXT2_MISS`
  - `EXT3_MISS`

### Additional Missing Indicators

- `OCCUPATION_MISS`
- `OWN_CAR_AGE_MISS`
- Indicators for missing credit history

---

## 3. Engineers Financial Ratios

The pipeline creates several common credit‑risk ratios:

- `CREDIT_TO_INCOME`
- `ANNUITY_TO_INCOME`
- `PAYMENT_RATE` (annuity / credit)
- `CREDIT_TERM` (credit / annuity proxy)
- `LTV_PROXY` (credit / goods price)
- `INCOME_PER_PERSON`
- `ANNUITY_PER_PERSON`
- `CHILDREN_RATIO`

Log‑transformed monetary features:

- `LOG_INCOME`
- `LOG_CREDIT`
- `LOG_ANNUITY`
- `LOG_GOODS`

---

## 4. Feature Engineering

Additional engineered features include:

- EXT source average/min/max
- Interaction term: `AGE_YEARS * EXT_AVG`
- Age and income binning (learned from training data only)
- Rare category collapsing for high‑cardinality categorical variables

---

## 5. Aggregates Supplementary Tables

All supplementary tables are aggregated to the applicant level (`SK_ID_CURR`).

### bureau.csv

- Count of previous credits
- Active vs closed credit ratios
- Overdue totals
- Debt ratios

### previous_application.csv

- Previous application counts
- Approval rate
- Refusal count
- Average credit amounts

### installments_payments.csv

- Installment count
- Late payment percentage
- Average days late
- Payment ratio statistics

---

## 6. Ensures Train/Test Consistency

The pipeline:

- Learns medians, bins, and categorical levels from **training data only**
- Stores parameters in a reusable `params` object
- Applies identical transformations to test data
- Creates identical feature columns for both datasets
- Ensures `TARGET` appears only in the training output

Safety checks confirm:

- identical feature columns
- no leakage
- correct dataset structure

---

# Modeling Notebook

File: `home_credit_modeling.Rmd`

## Purpose

The modeling notebook trains and evaluates several machine‑learning models to estimate the probability that a borrower will default.

Because only about **8% of borrowers default**, model performance is evaluated primarily using **Area Under the ROC Curve (AUC)** rather than raw accuracy.

---

# Modeling Workflow

The notebook performs the following steps:

1. Establish a baseline benchmark
2. Compare multiple model families
3. Test class‑imbalance strategies
4. Tune hyperparameters for the best‑performing model
5. Train a final model and generate predictions

---

# Models Compared

Three modeling approaches were evaluated.

### Logistic Regression

A logistic regression model provides a simple and interpretable baseline.

### Random Forest

A random forest model was trained on a **sampled subset of the dataset** to reduce runtime while still providing a meaningful comparison.

### XGBoost

Gradient boosted trees using **XGBoost** provided the best predictive performance.  
Hyperparameters were tuned using cross‑validation on a **5,000‑row subsample** for efficiency.

---

# Model Results

Model performance was evaluated using cross‑validated AUC.

| Model | AUC |
|------|------|
| XGBoost | ~0.776 |
| Logistic Regression | ~0.725 |
| Random Forest | ~0.723 |

The XGBoost model substantially outperformed the baseline models, demonstrating the effectiveness of nonlinear tree‑based methods and engineered credit‑history features.

---

# Final Model

The final model is a tuned **XGBoost classifier** trained on the full feature matrix.

Predicted probabilities for the test dataset are exported to:

```
home_credit_submission.csv
```

The submission file contains:

- `SK_ID_CURR`
- predicted `TARGET` probability

---

# How to Run the Project

1. Run the data preparation pipeline:

```
source("data_preparation.R")
```

2. Open and run the modeling notebook:

```
home_credit_modeling.Rmd
```

3. The notebook will:

- train multiple models
- compare performance
- tune XGBoost
- generate predictions
- export `home_credit_submission.csv`

---

# Summary

This project demonstrates a complete **credit‑risk modeling pipeline**, including:

- reproducible data preparation
- feature engineering
- model comparison
- hyperparameter tuning
- production‑ready prediction output

The final tuned XGBoost model achieves strong predictive performance while maintaining a clean, reproducible workflow consistent with the CRISP‑DM methodology.
