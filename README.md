# Home Credit Default Risk Project

**Business Analytics – Data Preparation, Modeling, and Model Documentation**

---

# Project Overview

This project predicts loan default risk using the **Home Credit Default Risk** dataset.
The repository contains a reproducible workflow for:

1. preparing raw Home Credit data for modeling,
2. training and comparing multiple machine-learning models, and
3. documenting the final model in a stakeholder-facing **model card**.

The work follows the **CRISP-DM framework**:

1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modeling
5. Evaluation
6. Deployment Communication

The repository currently includes three main project components:

- `data_preparation.R` – reusable feature engineering and preprocessing pipeline
- `home_credit_modeling.Rmd` – modeling notebook used to compare models and train the final model
- `model_card.Rmd` – model documentation notebook written for a business stakeholder audience

---

# Data Preparation Script

File: `data_preparation.R`

## Purpose

The `data_preparation.R` script builds a modeling-ready dataset from raw Home Credit data using reusable functions. It is designed to ensure that:

- transformations are applied consistently to training and test data,
- no information from the test set leaks into training, and
- train and test datasets contain aligned feature columns except for `TARGET`.

## What the Data Preparation Script Does

### 1. Cleans Application Data

- Replaces `DAYS_EMPLOYED == 365243` placeholder values with `NA`
- Creates an anomaly indicator for employment days
- Converts day-based features into more interpretable age and employment measures

### 2. Handles Missing Values

- Imputes `EXT_SOURCE_1`, `EXT_SOURCE_2`, and `EXT_SOURCE_3` using training-only statistics
- Adds missingness indicators for high-value fields such as external scores and occupation-related variables

### 3. Engineers Financial Ratios

The preparation pipeline creates credit-risk features such as:

- `CREDIT_TO_INCOME`
- `ANNUITY_TO_INCOME`
- `PAYMENT_RATE`
- `CREDIT_TERM`
- `LTV_PROXY`
- `INCOME_PER_PERSON`
- `ANNUITY_PER_PERSON`
- `CHILDREN_RATIO`

### 4. Aggregates Supplementary Tables

Supplementary Home Credit tables are aggregated to the applicant level (`SK_ID_CURR`) so they can be joined to the main application data. These include engineered summaries from:

- `bureau.csv`
- `previous_application.csv`
- `installments_payments.csv`

### 5. Ensures Train/Test Consistency

The script learns preprocessing parameters from the training data, stores them in a reusable object, and applies identical transformations to the test data.

---

# Modeling Notebook

File: `home_credit_modeling.Rmd`

## Purpose

The modeling notebook trains and evaluates several machine-learning models to estimate the probability that a borrower will default.

Because the target is highly imbalanced, model performance is evaluated primarily using **Area Under the ROC Curve (AUC)** rather than raw accuracy.

## Modeling Workflow

The notebook performs the following steps:

1. Establish a baseline benchmark
2. Compare multiple model families
3. Test class-imbalance strategies
4. Tune hyperparameters for the best-performing model
5. Train a final model and generate a Kaggle submission

## Models Compared

Three modeling approaches are compared in the notebook:

- **Logistic Regression** – baseline interpretable model
- **Random Forest** – nonlinear tree-based comparison model trained on a sampled subset for runtime efficiency
- **XGBoost** – gradient boosted trees and the strongest-performing final model

## Final Model

The final model documented in this repository is a tuned **XGBoost binary classifier** trained on the engineered feature matrix created from the Home Credit application data and supplementary aggregated tables.

The modeling notebook also records a **Kaggle public AUC of 0.76421** for the final submission.

Predicted probabilities for the Kaggle test set are exported to:

```text
home_credit_submission.csv
```

---

# Model Card Notebook

File: `model_card.Rmd`

## Purpose

The model card notebook summarizes the final Home Credit default-risk model for a **business stakeholder deciding whether and how to use the model**. It is written as documentation rather than as a coding exercise and is intended to be compiled to HTML with code hidden and outputs displayed.

## What the Model Card Covers

The model card notebook includes the following sections:

- **Model Details** – model type, version, training data summary, and final hyperparameters
- **Intended Use** – what the model is designed to support and what it should not be used for
- **Performance Metrics** – internal holdout AUC, tuning-stage AUC when available, and the recorded Kaggle public AUC
- **Decision Threshold Analysis** – recommended operating threshold selected from Home Credit holdout-sample performance
- **Explainability (SHAP)** – SHAP-based feature importance using a 1,000-row sample for speed
- **Adverse Action Mapping** – translation of technical feature names into human-readable denial-reason language
- **Fairness Analysis** – approval-rate comparisons by `CODE_GENDER` and `NAME_EDUCATION_TYPE`
- **Limitations and Risks** – known constraints of the data and model
- **Executive Summary / Business Recommendation** – deployment-oriented summary for a senior stakeholder

## Important Constraint

The current `model_card.Rmd` is written to use only the **Home Credit Kaggle project data** and the real model objects created in the modeling workflow. It does **not** introduce outside lender-profit assumptions or unsupported dollar-impact estimates.

The file expects the following objects from the modeling workflow to be available in the R session or loaded from an artifact file:

- `final_model`
- `best_xgb`
- `x_full`
- `y`
- `train_full_processed`
- `final_params`

---

# How to Run the Project

## 1. Run the data preparation pipeline

```r
source("data_preparation.R")
```

## 2. Run the modeling notebook

Open and run:

```text
home_credit_modeling.Rmd
```

This notebook creates the final model, compares candidate models, and generates the Kaggle submission file.

## 3. Run or load the modeling objects for the model card

Before knitting `model_card.Rmd`, either:

- run the modeling notebook in the same R session, or
- save and load an `.RData` artifact containing the required modeling objects.

## 4. Knit the model card

Compile:

```text
model_card.Rmd
```

to HTML so the final deliverable shows narrative text, tables, and figures with code hidden.

---

# Repository Summary

This repository documents a full Home Credit credit-risk workflow, including:

- reproducible data preparation,
- feature engineering from main and supplementary tables,
- model comparison across multiple algorithms,
- final XGBoost training and Kaggle scoring, and
- a stakeholder-facing model card for threshold selection, explainability, fairness review, and model limitations.

The final documented model is a tuned XGBoost classifier with a recorded **Kaggle public AUC of 0.76421**.
