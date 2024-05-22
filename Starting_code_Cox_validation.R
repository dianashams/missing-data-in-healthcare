library(survival)

ddd = read.csv("diabetes_data_for_msc_2024.csv")
ddd$event
ddd$time
names(ddd)

# [1] "idauniq"            participant id
# [2] "baseline_wealth" # accumulated wealth , low/medium/high 0/1/2 -> better recode into 0/1 for med and high
# [3] "baseline_bmi"     # BMI
# [4] "baseline_cvd"     # past or present cardiovascular disease (1/0)
# [5] "baseline_B_dep"   # presence of severe depressive symptoms (1/0) 
# [6] "baseline_hyp"     # past or present hypertension (1/0)[7] "baseline_stroke"  # past or present stroke (1/0)
  
# blood test resuls at baseline 
# [8] "baseline_trig"    # triglicerydes
# [9] "baseline_hdl"     # HDL 
# [10] "baseline_fglu"    # fasting blood glucose 
# [11] "baseline_hba1c"   # HBA1c
# [12] "Education"        # level of Education 0/1/2
# [13] "sex"              # 1-female, 0 - male
# [14] "baseline_age"     # age in years at start
# [15] "baseline_exercise" #exercise regime, no or low/ medium/frequent 0/1/2 -> consider continuous
# (ignore) [16] "t2dm_"         # polygenic scores for type 2 diabetes,l normalised   
# (ignore) [17-20] "pc1_", "pc2_", "pc3_",  "pc4_"   # principal component for genotype, normalised, include if use t2dm_

# You should describe this particular data (N, man/women etc) 
# that you'll use in the thesis (and not ELSA participants in general). You can start from "describe(ddd)"

### To analyse the imputation methods, and be able to create 
# missing data MAR and MCAR and control % missing, 
# we can start from a sample with no NA
## only BMI had missing values in the baseline model, without any blood test results,
# so we drop those without BMI 
baseline_predictors <- c("baseline_age", "sex", "baseline_bmi",
                "baseline_hyp", "baseline_cvd", "baseline_exercise", 
                "baseline_wealth", "Education", "baseline_B_dep")
ddd_complete <- ddd[!is.na(ddd$baseline_bmi),]
dim(ddd); dim(ddd_complete) # 5957 -> 5603 (-354 observations)
sum(is.na(ddd_complete[baseline_predictors])) #0

#save this data separately 
#write.csv(ddd_complete[c("time", "event",predictors)],
#         "C:/Users/public/Downloads/COMPLETE_diabetes_data_for_msc_2024.csv")


#######################################################################
# The analysis can start from the basic Cox model, e.g. 

#recode wealth into 0/1/2 and consider as continous (1 unit = 1 higher wealth tercile)
ddd$baseline_wealth <- ifelse(ddd$baseline_wealth=="low", 0, 
                              ifelse(ddd$baseline_wealth=="med", 1,2))

basic_model = coxph( Surv(time, event)~ baseline_age+ sex + baseline_bmi+ baseline_hyp+
                       baseline_cvd+ baseline_exercise + baseline_wealth +
                       Education + baseline_B_dep, data =ddd)

s = summary(basic_model)
s$concordance
# C      se(C) 
# 0.73317022 0.01169702 

# Compute number of missing data in each column
apply(is.na(ddd), 2, sum)
# X           idauniq   baseline_wealth 
# 0                 0                 0 
# baseline_bmi      baseline_cvd    baseline_B_dep 
# 354                 0                 0 
# baseline_hyp     baseline_trig      baseline_hdl 
# 0               396               398 
# baseline_fglu    baseline_hba1c   baseline_stroke 
# 2339               414                 0 
# time             event         Education 
# 0                 0                 0 
# sex      baseline_age baseline_exercise 
# 0                 0                 0 
# t2dm_              pc1_              pc2_ 
# 0                 0                 0 
# pc3_              pc4_ 
# 0                 0 


basic_coxph_cv <- function(ddd,  #data
                           cv_number = 10, #cv 
                           random_seed = 42 #random seed to use 
                           ) {
  # perform 10-fold CV for the basic Cox model
  set.seed(random_seed)
  
  # create folds using caret
  #use caret to split into k-folds = cv_steps
  cv_folds = caret::createFolds(ddd$event, k = cv_number, list = FALSE)
  
  # placeholders for cindex in the test and train datasets for each CV fold
  test_cindex = rep(NA, cv_number)
  train_cindex = rep(NA, cv_number)
  
  # CV loop
  for (cv_iteration in 1:cv_number) {
    df_train_cv = ddd[cv_folds != cv_iteration,]
    dim(df_train_cv)
    df_test_cv  = ddd[cv_folds == cv_iteration,]
    dim(df_test_cv)
    
    #fit cox ph to train data
    model_cv = coxph(
      Surv(time, event) ~  baseline_age + sex + baseline_bmi + baseline_hyp +
        baseline_cvd + baseline_exercise + baseline_wealth +
        Education + baseline_B_dep,
      data = df_train_cv
    )
    
    #compute c-index for train and test data
    # cindex_train = summary(model_cv)$concordance
    p_train = 1 - predict(model_cv, newdata = df_train_cv, type = "lp")
    #train data c-index (apparent performance)
    cindex_train = concordance(Surv(time, event) ~ p_train, df_train_cv)$concordance
    
    #test data c-index (test performance)
    p_test = 1 - predict(model_cv, newdata = df_test_cv)
    cindex_test = concordance(Surv(time, event) ~ p_test, df_test_cv)$concordance
    
    test_cindex[cv_iteration] = cindex_test
    train_cindex[cv_iteration] = cindex_train
  }
    
    # see the results
    results = cbind(test_cindex, train_cindex)
    # test_cindex train_cindex
    # [1,]   0.7354601    0.7321569
    # [2,]   0.6959244    0.7353001
    # [3,]   0.7865657    0.7252983
    # [4,]   0.7322892    0.7334139
    # [5,]   0.7423551    0.7310465
    # [6,]   0.7525938    0.7314556
    # [7,]   0.6861026    0.7395091
    # [8,]   0.7690755    0.7300890
    # [9,]   0.6547166    0.7425040
    # [10,]   0.7322104    0.7323093
    
    #Internally validated test performance: 
    print("Mean and SD of the cindex for test data:")
    print(round(mean(test_cindex),3))
    print(round(sd(test_cindex),3)) #0.7287 0.039
    
    # mean train performance
    mean(train_cindex) #0.7333

  return(results)
}

# run internal cross-validation
basic_coxph_cv(ddd)

#########################
Impute BMI and check how prediction performance changes 

library(missForest)
mf <- missForest::missForest(ddd)
imputed_data <- mf$ximp

# check there is nothing missing in the imputed data
apply(is.na(imputed_data), 2, sum)

#perform cross-validation for the imputed data
results2 <- basic_coxph_cv(imputed_data)

############################################################
# create some missing data (completely at random)
ddd_missing = ddd_complete
n = dim(ddd)[1] #how many patients there are

create_mcar<- function(p, frq=0.1){  # 10% missing data to create 
    n <- length(p)
    rows_to_delete = sample(1:n, round(frq*n,0))  #select 10% rows at random 
    p[rows_to_delete] = NA
return (p)

ddd_missing$hyp_0 = create_mcar(ddd_missing$hyp_0)

# AND create MAR 
# e.g. using 
#https://cran.r-project.org/web/packages/missMethods/vignettes/Generating-missing-values.html
#https://rdrr.io/cran/mice/man/ampute.html
# https://rianneschouten.github.io/mice_ampute/vignette/ampute.html

# try different ways to impute  (mean, missForest, MICE, ... )
# validate how the prediction performance changes 
