# Health Insurance Cost Prediction Analysis

## Overview

This project involves an analysis of health insurance costs using a dataset containing demographic and health-related information. The primary objective is to build a predictive model for insurance charges based on various features such as age, sex, BMI, number of children, smoking status, and region. The process includes exploratory data analysis (EDA), data preprocessing, feature selection, and Bayesian hierarchical modeling.

## Dataset

The dataset `insurance.csv` contains the following columns:
- `age`: Age of the primary beneficiary.
- `sex`: Gender of the primary beneficiary (male/female).
- `bmi`: Body mass index.
- `children`: Number of children/dependents covered by the insurance.
- `smoker`: Smoking status (yes/no).
- `region`: Residential region (northeast/northwest/southeast/southwest).
- `charges`: Insurance charges (target variable).

## Steps Involved

### 1. Exploratory Data Analysis (EDA)

- Inspecting the dataset structure and summary statistics.
- Visualizing distributions of numerical variables (age, BMI, charges).
- Visualizing distributions of categorical variables (sex, children, smoker, region).
- Scatter plots and box plots to explore relationships between features and charges.

### 2. Data Cleaning and Preprocessing

- Removing outliers based on the Interquartile Range (IQR) method for numerical variables (age, BMI, children, charges).
- Encoding categorical variables (`sex` and `smoker`) as binary variables.
- One-hot encoding the `region` variable.

### 3. Feature Engineering

- Creating interaction terms between selected features (e.g., age and smoker, BMI and smoker).

### 4. Bayesian Hierarchical Modeling

- Fitting a Bayesian hierarchical model using the `brms` package.
- Including interaction terms and random effects for regions.

### 5. Model Evaluation

- Predicting insurance charges using the fitted model.
- Calculating Mean Absolute Error (MAE), Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and R-squared metrics to evaluate model performance.

## Results

The Bayesian hierarchical model provides predictions for insurance charges with the following performance metrics:
- Mean Absolute Error (MAE): 
- Mean Squared Error (MSE): 
- Root Mean Squared Error (RMSE): 
- R-squared: 

## Conclusion

This analysis demonstrates the process of predicting health insurance charges using various demographic and health-related features. The Bayesian hierarchical model, combined with feature engineering and careful data preprocessing, provides a robust approach to understanding the factors influencing insurance costs.
