---
title: "ATP Tennis Match - Set Prediction Project"
author: "Leonor Gouveia"
output: github_document
---

## Project Overview

This repository contains a Data Science project developed for the **Applied Project in Data Science** course at **ISCTE - Instituto Universitário de Lisboa**.

The goal is to predict the number of sets in professional tennis matches (ATP) by leveraging historical match data, player rankings, and physical attributes. The project has a hybrid architecture, using **MongoDB** for handling large datasets (over 1.1M records) and **R** for advanced statistical modeling.

## Technical Features

### 1. Data Engineering & NoSQL (MongoDB)

-   **High Volume Handling:** Processed a raw dataset of 1.1 million documents.

-   **ETL:** Used MongoDB Aggregation Pipelines to:

    -   Clean and standardize player names and locations.

    -   Merge player statistics with match results.

    -   Filter specific geographic contexts .

-   **Script:** `db_extraction_etl.js`

### 2. Data Pre-processing & EDA (R)

-   **Feature Engineering:** Creation of variables such as Ranking Gap, Player Age, Height Difference, and Surface Specialty.

-   **Data Cleaning:** Extensive handling of "Walkovers" (W/O) and retired matches to ensure model quality.

-   **Libraries:** `dplyr`, `tidyr`, `lubridate`, `ggplot2`.

-   **Script:** `01_eda_and_cleaning.R`

### 3. Machine Learning Models

Implemented and compared multiple classification algorithms to predict the number of sets:

-   **XGBoost** (Extreme Gradient Boosting)

-   **KNN** (K-Nearest Neighbors)

-   **Naive Bayes**

-   **Logistic Regression**

-   **Oversampling Technique:** Used `UBL` (SMOTE/Importance Sampling) to handle class imbalance in match durations.

-   **Script:** `02_predictive_modeling.R`

## Performance & Results

The models were evaluated using **Cross-Validation** and **Confusion Matrices**. The study focused on identifying the most relevant features, such as the difference in ranking between players and the type of tournament.

## Repository Structure

-   `db_extraction_etl.js`: MongoDB shell scripts for data cleaning and transformation.

-   `01_eda_and_cleaning.R`: R script for Exploratory Data Analysis and final data preparation.

-   `02_predictive_modeling.R`: R script for training and evaluating Machine Learning models.

-   `Project_Report_PT.pdf`: Full academic report detailing the methodology (Portuguese).

## How to Run

1.  **Database:** Import the ATP JSON data into a MongoDB instance named `atp`. Run `db_extraction_etl.js`.

2.  **Analysis:** Open `01_eda_and_cleaning.R` in RStudio to process the exported data.

3.  **Modeling:** Run `02_predictive_modeling.R` to train the models and see the evaluation metrics.
