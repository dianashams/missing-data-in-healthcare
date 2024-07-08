# Prediction of time-to-event outcomes in the presence of missing data. 

## Review of the existing methods of handling missing data

Current version can be found here [Missing Data Handling. Report for NIHR Maudsley Biomedical Research Centre](https://github.com/dianashams/missing-data-in-healthcare/blob/gh-pages/Handling%20missing%20data%20for%20clinical%20prediction%20models.pdf).

## Which combinations of imputation and prediction algorithms work best?  

##### Keywords: prediction modelling, missing data imputation, survival analysis, machine learning

The project is a comparison study investigating the performance of the Cox model and Survival Random Forest while using different imputation methods (e.g. MICE/ missForest / single or multiple imputations), in R or in Python. It is an MSc project within the [MSc “Statistical Modelling and Health Informatics”](https://www.kcl.ac.uk/study/postgraduate-taught/courses/applied-statistical-modelling-health-informatics) at King's College London, 2024.

### PROJECT AIMS 
Review existing missing data imputation techniques, and investigate which techniques result in a better performance and higher stability of the survival methods (Cox Proportionate Hazard Model, Survival Random Forest and/or XGBoost) while predicting mental health outcomes.  

### PROJECT OUTLINE 
Survival analysis is a collection of methods used for time-to-event outcomes such as death, disease onset, recovery, and others. Classical Cox Proportionate Hazard model developed over 50 years ago has proved to be one of the most popular survival analysis models. In the past decade, machine learning methods such as classification trees, random forests and neural networks have been extended to accommodate survival data. With such an extended toolkit, researchers are now able to find methods to fit various types of data and achieve good predictive performance. However, any model's performance depends on the training data quality, and in particular, its completeness.  

Given that missing data is one of the key issues in clinical research, in this project a student will 1) review existing imputation methods, and 2) investigate which combination of the imputation and prediction models work best. The underlying data will be a combination of simulated and real-life (cancer survival / psychosis onset / diabetes onset) data. 

### Data 

* Simulated data using the functions  simulate_linear(), simulate_nonlinear(), and simulate_crossterms() from the R package 'survcompare'.
* The English Longitudinal Study of Ageing data with the outcome of new diagnosis of type 2 diabetes.
  
### Method

* Missing values will be created using customized functions to allow simulating different 
  * missingness mechanisms (MAR, MCAR, MNAR),
  * percentage of missing data (0% - 50%).

* Prediction models
    * CoxPH Model ('survival' R package);
    * Cox-Lasso Model ('glmnet' R package);
    * Survival Random Forest (SRF) ('randomforestSRC' R package).
 
* Imputation methods
    * Complete cases analysis (column-wise deletion of predictors with >50% missingness, then, row-wise deletion of missing data);
    * Mean imputation (replace missing value with an observed mean value for this predictor);
    * Multivariate Imputation with Chained Equations (MICE), (an iterative imputation process using Random Forest)
    * MissForest (an iterative process using Random Forest)
    * K-neighbours imputation (with k tuned for the data from 5 to the sqrt(n), n - sample size)

* Models validation will be performed using Nested Cross-Validation, that is, we
    1. _Simulate_ missing data in the whole dataset; 
    2. Split the data into k (k=5) folds; 
    3. Set aside one fold for testing, the rest for model tuning
    4. _Impute_ missing data in the train data, use trained imputer to impute test data;
    5. Tune hyperparameters using via an internal cross-validation loop for Cox-Lasso, or out-of-bag predictions for SRF  ('lambda' in CoxLasso; 'max_depth', 'min_leaf_size' in SRF). 
    6. Predict for the test data using the tuned model and compute the performance metrics;
    7. Repeate the steps iii - vi alternating the testing fold (k times);
    8. Average the performance metrics across the folds.
    9. Repeate the steps i-viii for all combinations of the missingness mechanisms and percentage of missing data.

* Performance metrics:
     * Condordance index (Harrel's c-index for survival outcomes);
     * Time-dependent AUC-ROC (using 'PROC' R package);
     * Calibration slope (how well the predicted survival probabilities correspond to the obseerved survival rates);
     * (TBD)Time-dependent Brier score (mean squared error for binary outcome of survived/ not survived), Bernoulli likelihood (mean absolute binary error).

  
