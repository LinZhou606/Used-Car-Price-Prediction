# Used Car Price Prediction Project

## Overview
 The primary objective was to predict used car prices using advanced data preprocessing, feature engineering, and machine learning models. Throughout the project, I explored several approaches for handling categorical data and experimented with both **Random Forest** and **XGBoost** models to find the optimal configuration for accurate predictions.

## Key Features
### 1. Data Tidying and Preprocessing
- **Handling Missing Values**:
  - For categorical variables, missing values were imputed using the mode.
  - For numerical variables, missing values were filled with the median value.
  - Boolean variables were standardized to 0 and 1 values where needed.

- **Feature Extraction and Transformation**:
  - Parsed complex variables into multiple columns, enhancing model interpretability. For example:
    - Extracted `engine_type` to include cylinder configuration and number.
    - Parsed the description text for indicators of discounts or special deals.
    - Generated features like `car_age` and various transmission details.
    
- **Grouping and Encoding**:
  - Categorical data with a high number of unique values were grouped to reduce dimensionality, while integer encoding, target encoding, and one-hot encoding were applied to make the data compatible with machine learning algorithms.

### 2. Feature Selection
- **Random Forest**: Selected the top 41 most influential variables from 50 predictors.
- **XGBoost**: Extracted the 50 most impactful features out of 61 predictors for further refinement.

### 3. Model Selection and Tuning
- **Random Forest Model**:
  - Hyperparameters were optimized using grid search and cross-validation, reducing RMSE from an initial 2300 to 1830.
  - Experimented with varying the number of trees (`num.trees`) and other parameters to fine-tune model performance.
  
- **XGBoost Model**:
  - Initial attempts with XGBoost showed high overfitting and lower predictive power, despite extensive tuning.
  - The best result achieved an RMSE of 2300, but due to persistent overfitting issues, the model was ultimately excluded from the final solution.



## Figures
- **Figure 1: Feature Importance from Random Forest Model**  
  ![Feature Importance from Random Forest Model](https://github.com/LinZhou606/Used-Car-Price-Prediction/blob/main/Results/Feature%20Importance%20from%20Random%20Forest%20Model.png)

- **Figure 2: RMSE Reduction Plot**  
  ![RMSE Reduction Plot](https://github.com/LinZhou606/Used-Car-Price-Prediction/blob/main/Results/RMSE.png)

## Key Technologies Used
- **Languages**: R
- **Libraries**: `dplyr`, `ranger` (for Random Forest), `xgboost`, `tidyverse`

📄 [Detailed Project Report](https://github.com/LinZhou606/Used-Car-Price-Prediction/blob/main/Results/Report.pdf)

## Lessons Learned
- **Importance of Feature Engineering**: Custom feature extraction and transformation significantly influenced model accuracy.
- **Efficient Model Tuning**: Grid search with cross-validation greatly enhanced the final model's performance.
- **Challenges with XGBoost**: Overfitting was a persistent issue, indicating further refinement is needed for encoding strategies.
