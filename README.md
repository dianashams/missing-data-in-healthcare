# Prediction of time-to-event outcomes in the presence of missing data. 
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

* The missing data will be created by using the function 'ampute', from a widely used imputation package ['mice'](https://rianneschouten.github.io/mice_ampute/vignette/ampute.html).
* The missing data will be simulated with various
  * missingness mechanisms (MAR, MCAR, MNAR)
  * percentage of missing data (0% - 50%)

* The prediction models are
    * CoxPH Model ('survival' R package) 
    * Cox-Lasso Model ('glmnet' R package)
    * Survival Random Forest (SRF) ('randomforestSRC' R package) 

* Models validation will be performed using Nested Cross-Validation, that is, we
    * Split the data into k (k=5) folds 
    * Using one fold as a testing data, while the rest is used for the hyperparameter tuning using an internal cross-validation loop for Cox-Lasso, or out-of-bag predictions for SRF
    * The tuned model will be used to predict the outcome in the testing data, and the predictive performance metrics will be computed using the predicted and actual outcomes.
    * Steps 1-3 are repeated for each of the k folds, after which the performance metrics are averaged across the folds.

* Performance metrics:
     * Condordance index (Harrel's c-index for survival outcomes)
     * Time-dependent AUC-ROC (using 'PROC' R package)
     * Calibration slope (how well the predicted survival probabilities correspond to the obseerved survival rates)
     * Time-dependent Brier score (mean squared error for binary outcome of survived/ not survived), Bernoulli likelihood (mean binary error)

  
