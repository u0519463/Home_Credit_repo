# Home Credit Default Risk Portfolio

**Heidi Hatch**  
Individual analytics portfolio project for the **Home Credit Default Risk** Kaggle competition

---

## Project Summary

This repository documents my individual work on the Home Credit Default Risk project. The goal of the project is to predict which loan applicants are at higher risk of default so that a lender can make more informed underwriting decisions.

This portfolio is organized as a reproducible analytics workflow rather than a single notebook. It shows how I moved from data preparation to model development and then to model documentation for stakeholder communication. The work emphasizes both predictive performance and clear explanation of results.

From a business perspective, the value of this project is in helping a lender better identify credit risk before issuing a loan. A stronger risk model can support more consistent lending decisions, reduce avoidable defaults, and improve how credit decisions are communicated to non-technical stakeholders.

---

## Portfolio Contents

This repository includes my individual notebooks and scripts created while working on the Home Credit project.

### `data_preparation.R`
Reusable data-preparation pipeline for the Home Credit dataset. This script:
- cleans and standardizes raw application data
- handles missing values using training-only logic
- engineers credit-risk features and ratios
- aggregates supplementary tables to the applicant level
- ensures train/test consistency for modeling

### `home_credit_modeling.Rmd`
Modeling notebook used to train and compare machine learning models for default prediction. This notebook:
- establishes a baseline
- compares multiple model families
- evaluates class imbalance strategies
- tunes the strongest model
- produces a final Kaggle submission file

### `model_card.Rmd`
Model documentation notebook for the final model. This notebook summarizes:
- model details and intended use
- performance metrics and operating threshold analysis
- SHAP-based explainability
- adverse action reason mapping
- fairness checks across selected demographic groups
- limitations, risks, and deployment considerations

---

## Problem Framing

Home Credit serves applicants who may have limited traditional credit history. In this setting, default prediction is a high-value analytics problem because poor credit decisions are expensive, while overly conservative decisions may reject applicants who would repay successfully.

The project therefore focuses on building a model that ranks risk effectively and can be explained clearly enough to support real decision-making.

---

## Data and Method

The work uses the **Home Credit Default Risk** Kaggle dataset, including the main application files and supplementary relational tables.

My workflow follows a practical CRISP-DM structure:

1. **Business Understanding** – frame the lending problem and define the modeling objective  
2. **Data Understanding** – examine the target, missingness, anomalies, and predictive signal  
3. **Data Preparation** – clean data, engineer features, and aggregate external tables  
4. **Modeling** – compare candidate models and tune the best approach  
5. **Evaluation** – assess predictive performance and document the final model

---

## Modeling Results

The modeling work compared logistic regression, random forest, and XGBoost.

Based on the modeling notebook, the tuned **XGBoost** model performed best.

### Cross-validated model comparison
- **XGBoost:** approximately **0.776 AUC**
- **Logistic Regression:** approximately **0.725 AUC**
- **Random Forest:** approximately **0.723 AUC**

### Kaggle performance
- **Public leaderboard AUC:** **0.76421**

These results suggest that nonlinear tree-based methods combined with engineered credit-risk features were more effective than the baseline alternatives for this dataset.

---

## Business Value

This project demonstrates how machine learning can be used to support lending decisions in a structured and reproducible way.

The main business contributions of this work are:
- improving identification of higher-risk applicants
- building a reusable preparation pipeline instead of one-off analysis
- documenting model behavior so results are easier to explain
- connecting model outputs to stakeholder-facing documentation through a model card

---

## What I Would Discuss in an Interview

This portfolio is intended to show both technical skill and communication ability. Key talking points include:

- how I translated exploratory findings into reusable data-preparation code
- how I prevented train/test leakage during feature engineering
- why AUC was the primary metric given class imbalance
- why XGBoost outperformed the simpler baseline models
- how SHAP can be used to explain model behavior
- how model documentation helps bridge technical modeling and business deployment
- what limitations remain when working with a competition dataset rather than live production data

---

## Reproducibility Notes

This repo is organized around source notebooks and scripts rather than rendered HTML because GitHub does not render HTML notebooks well for portfolio review. The main portfolio artifacts are the `.Rmd` and `.R` files that document my individual work.

Model artifacts and output files should be kept locally and excluded from version control when appropriate. The notebooks and scripts in this repo are intended to show the full analytic workflow in a reproducible, portfolio-ready format.

---


