
# Home Credit Default Risk Project

**Business Analytics – Data Preparation & Modeling**

## Project Overview

This project focuses on predicting loan default risk using the **Home Credit Default Risk** dataset. The goal is to build a clean, reproducible, and production-ready data preparation pipeline that transforms raw data into a modeling-ready dataset while preventing data leakage.

The workflow follows the CRISP-DM framework:

1. Business Understanding
2. Data Understanding
3. Data Preparation
4. Modeling
5. Evaluation

This repository contains the reusable data preparation pipeline used to generate consistent train and test datasets for modeling.

---

# Data Preparation Script

File: `data_preparation.R`

## Purpose

The `data_preparation.R` script builds a modeling-ready dataset from raw Home Credit data using **reusable functions**. It ensures that:

* All transformations are consistent between training and test data
* No information from the test set is used during training (no leakage)
* Train and test datasets contain identical feature columns (except `TARGET`)

---

# What the Script Does

## 1. Cleans Application Data

### Fixes Data Issues Identified in EDA

* Replaces `DAYS_EMPLOYED == 365243` placeholder values with `NA`
* Creates anomaly indicator: `DAYS_EMPLOYED_ANOM`
* Converts negative day-based features to positive years:

  * `AGE_YEARS`
  * `EMP_YEARS`

---

## 2. Handles Missing Values

### EXT_SOURCE Variables

* Imputes `EXT_SOURCE_1`, `EXT_SOURCE_2`, `EXT_SOURCE_3` using **training-only medians**
* Adds missing indicators:

  * `EXT1_MISS`
  * `EXT2_MISS`
  * `EXT3_MISS`

### Additional Missing Indicators

* `OCCUPATION_MISS`
* `OWN_CAR_AGE_MISS`
* Indicators for no bureau/previous/installment history

---

## 3. Engineers Financial Ratios

The script creates commonly used credit-risk ratios, including:

* `CREDIT_TO_INCOME`
* `ANNUITY_TO_INCOME`
* `PAYMENT_RATE` (annuity / credit)
* `CREDIT_TERM` (credit / annuity proxy)
* `LTV_PROXY` (credit / goods price)
* `INCOME_PER_PERSON`
* `ANNUITY_PER_PERSON`
* `CHILDREN_RATIO`

Log-transformed monetary features:

* `LOG_INCOME`
* `LOG_CREDIT`
* `LOG_ANNUITY`
* `LOG_GOODS`

---

## 4. Feature Engineering

* Average/min/max of EXT sources
* Interaction: `AGE_YEARS * EXT_AVG`
* Age and income binning (computed from training data only)
* Rare category collapsing for high-cardinality variables

---

## 5. Aggregates Supplementary Tables

All supplementary data are aggregated to the applicant level (`SK_ID_CURR`).

### bureau.csv

* Count of prior credits
* Active vs closed credit counts
* Overdue amount totals
* Debt ratios

### previous_application.csv

* Total previous applications
* Approval rate
* Refusal count
* Average credit and annuity

### installments_payments.csv

* Installment count
* Late payment percentage
* Average days late
* Payment ratio statistics

---

## 6. Ensures Train/Test Consistency

The script:

* Learns medians, bins, and categorical levels from **training data only**
* Stores these values in a `params` object
* Applies identical transformations to test data
* Generates identical dummy-variable columns for both datasets
* Ensures `TARGET` exists only in training output

Safety checks are included to verify:

* Identical feature columns
* No leakage
* Correct output structure

---

# How to Use

```r
source("data_preparation.R")

library(readr)

app_train <- read_csv("data/application_train.csv", show_col_types = FALSE)
app_test  <- read_csv("data/application_test.csv",  show_col_types = FALSE)
bureau    <- read_csv("data/bureau.csv", show_col_types = FALSE)
prev      <- read_csv("data/previous_application.csv", show_col_types = FALSE)
inst      <- read_csv("data/installments_payments.csv", show_col_types = FALSE)

prep <- prepare_home_credit_data(app_train, app_test, bureau, prev, inst)

train_ready <- prep$train_ready   # Contains TARGET
test_ready  <- prep$test_ready    # Does not contain TARGET
```




