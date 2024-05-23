
# missing data percentage by column
mv <- function(df){round(apply(apply(df, 2, is.na), 2, sum)/dim(df)[1],4)}

##########################################################################

impute_mean<- function(df, params, randomseed = NULL){
  means <- apply(df[params], 2, mean, na.rm=TRUE)
  for (c in params){
    if (c %in% names(df)){
      df[is.na(df[c]), c] = means[c]}
  }
  output = list(); output$df = df
  return(output)
}

impute_mean_test <- function(dftrain ,dftest, params, randomseed = NULL){
  means <- apply(dftrain[params], 2, mean, na.rm=TRUE)
  for (c in params){
    if (c %in% names(dftest)){
        dftest[is.na(dftest[c]), c] = means[c]}
  }
  output = list(); output$df = dftest
  return(output)
}

impute_cc <- function(df, params, randomseed = NULL) {
  md <- apply(apply(df[params], 2, is.na), 2, sum) / dim(df)[1]
  params_cc <- params
  # exclude missing data > 50%
  for (c in params) {
    # if (md[c] > 0.51) {
      # params_cc = params_cc[params_cc != c]
    # } else{
      # if <50% missing, exclude row-wise
    if (c %in% names(df)){df <- df[!is.na(df[c]),]}
    # }
  }
  output = list()
  output$df = df
  output$params = params
  output$params_cc = params_cc
  return(output)
}

# d2<- impute_mice(diabetes, params)$df
# mv(d2[params])

impute_mice <- function(df, params, randomseed = NULL) {
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  d3 <- mice::mice(
    df[, params],
    m = 1,
    seed = randomseed,
    ignore = NULL
    #method = "norm.nob"
  )
  df[params] <- complete(d3)
  output=list(); output$df = df
  return(output)
}


impute_mice_test <- function(dftrain, dftest, params, randomseed = NULL) {
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  df_all <- data.frame(cbind(dftest[params], dftrain[params]))
  d3 <- mice::mice(
    df_all[, params],
    m = 1,
    seed = randomseed,
    ignore = NULL,
    method = "norm.nob",
    printFlag = FALSE
  )
  dftest[params]<- complete(d3)[1:dim(dftest)[1], ]
  output=list(); output$df = dftest
  return(output)
}

impute_missforest<- function(df, params, randomseed = NULL, correctwealth= TRUE){
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  {set.seed(randomseed);  mf<- missForest::missForest(df[params],verbose = FALSE)}
  df[params] <- mf$ximp
  
  # categorise wealth back
  if (correctwealth) {
    df$baseline_wealth_n <- round(df$baseline_wealth_n, 0)
    df$wealth_high <- ifelse(df$baseline_wealth_n == 2,1,0)
    df$wealth_med <- ifelse(df$baseline_wealth_n == 1,1,0)
    df$wealth_low <- ifelse(df$baseline_wealth_n == 0,1,0)
  } 
  output=list(); output$df = df
  return(output)
}

impute_missforest_test<- function(dftrain, dftest, params, randomseed = NULL){
  # https://rpubs.com/lmorgan95/MissForest idea from here 
  # Impute the training data using missForest(): train_X →imp_train_X
  # Build a random forest rf predicting creditability (the response) on imp_train_X
  # Combine imp_train_X & test_X →
  # train_test_X
  # Run missForest() to on train_test_X, then extract imp_test_X (the test observations only)
  # Use rf to get the probability predictions on the test data (using the imputed data, imp_test_X)
  df_all <- data.frame(cbind(dftest[params], dftrain[params]))
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  {set.seed(randomseed);  mf<- missForest::missForest(df_all)}
  dftest[params] = mf$ximp[1:dim(dftest)[1], ]
  output=list(); output$df = dftest
  return(output)
}


############################

# diabetes<-read.csv('diabetes_data_for_msc_2024.csv')
# diabetes <- diabetes[names(diabetes)!="X"]
# names(diabetes)
# summary(diabetes)
# dim(diabetes) #5957 23 
# 
# table(diabetes$event) # 456 cases
# sum(is.na(diabetes)) # 3901
# sum(diabetes$event==1)  
# 
# #binary for high and low wealth
# diabetes$wealth_high<- ifelse(diabetes$baseline_wealth=="high",1,0)
# diabetes$wealth_low<- ifelse(diabetes$baseline_wealth=="low",1,0)
# 
# #define params
# params_impute <- c("baseline_age", "sex", "baseline_bmi",
#             "baseline_hyp", "baseline_cvd", 
#             "baseline_exercise" , "Education", 
#             "baseline_B_dep",  "baseline_hba1c",     
#             "wealth_low",  "wealth_high", "wealth_med")
# 
# # diabetes for analyses 
# diabetes1<- impute_cc(diabetes, params)$df

# diabetes1$wealth_high <- as.factor(diabetes$wealth_high)
# diabetes1$wealth_low <- as.factor(diabetes$wealth_low)
# diabetes1$wealth_med <- as.factor(diabetes$wealth_med)
# write.csv(diabetes1, "diabetes_data_for_analyses.csv")

# load the data
diabetes<-read.csv('diabetes_data_for_analyses.csv')

# 0-1-2 for low, med. high wealth
diabetes$baseline_wealth_n <- ifelse(diabetes$baseline_wealth == "low", 0, 
                                     ifelse(diabetes$baseline_wealth == "med", 1,
                                            2))
  
params <- c("baseline_age", "sex", "baseline_bmi",
            "baseline_hyp", "baseline_cvd", 
            "baseline_exercise" , "Education", 
            "baseline_B_dep",  "baseline_hba1c",     
            "wealth_low",  "wealth_high")
params_impute <- c("baseline_age", "sex", "baseline_bmi",
                                "baseline_hyp", "baseline_cvd", 
                                "baseline_exercise" , "Education", 
                                "baseline_B_dep",  "baseline_hba1c",  
                                 "baseline_wealth_n")                 
create_missing <-
  function(df,
           params,
           mech = "MNAR",
           prop = 0.3,
           randomseed = NULL,
           pattern = NULL) {
    # set random seed
    if (is.null(randomseed)) {randomseed = sample(1:1e9, 1)}
    # if not given, use all params for missing values
    if (is.null(pattern)) {pattern = c(rep(0, length(params)))}
    {set.seed(randomseed)
      amp_train <- 
        mice::ampute(df[params],prop = prop,pattern = pattern,mech = mech)$amp
    }
    df[params] = amp_train
    return(df)
}

#################  AMPUTE  ################

#install.packages("mice") 
library(mice)
library(survival)

mechanisms<-c('MCAR', 'MAR','MNAR')
missing_p <- seq(0, 0.6, 0.1)

#create missingness in any variable
default_pattern_any = c(rep(0,10))

# missing in the last 5 variables only
default_pattern_1 = c(rep(1,5), rep(0,5))

# overall number of variables
nall = dim(df_train_cv[params])[1]*length(params)

#################################################

# CREATE DATA WITH MISSING DATA
d1 <- create_missing(diabetes, params_impute, "MNAR", 0.3, 2024)

d1_mice<- impute_mice(d1,params_impute,randomseed = 2024)$df
d1_missForest<- impute_missforest(d1,params_impute,randomseed = 2022)$df

table(diabetes$baseline_wealth_n, d1_missForest$baseline_wealth_n)
#            0    1    2
# low /0  1116  499    0
# med /1     0 1779    0
# high/2     0  604 1349

table(diabetes$baseline_wealth_n, d1_mice$baseline_wealth_n)
#     0    1    2
# 0 1274  169  172
# 1  139 1442  198
# 2  182  206 1565

cv_number = 3 
cv_folds = caret::createFolds(d1$event, k = cv_number, list = FALSE)

one_fold_run<- function(k, cv_folds, d1, d1_mice,d1_missForest){ 
    df_train_cv = d1[cv_folds != k,]
    df_test_cv  = d1[cv_folds == k,]
    
    df_train_0 <- impute_cc(df_train_cv, params_impute)$df
    df_test_0 <-  impute_cc(df_test_cv, params_impute)$df
    dim(df_train_0) #2987 29
    dim(df_test_0)  #742 29
    
    df_train_1 <- impute_mean(df_train_cv, params_impute)$df
    df_test_1 <-  impute_mean_test(df_train_cv, df_test_cv, params_impute)$df
    
    df_train_2 <- impute_mice(df_train_cv, params_impute)$df
    df_test_2 <-  d1_mice[cv_folds==k, ]
    
    df_train_3 <- impute_missforest(df_train_cv, params_impute,correctwealth = FALSE)$df
    df_test_3 <-  d1_missForest[cv_folds==k, ]
    
    getcindex <- function(df_train_x, df_test_x) {
      
      mcox <- survcompare::survcox_train(df_train_x, params)
      mcoxlasso <- glmnet::cv.glmnet(
        x = as.matrix(df_train_x[, params]),
        y = Surv(df_train_x$time, df_train_x$event),
        family = "cox"
      )
      msrf<- survcompare::survsrf_train(df_train_x, params)
      
      pcox0 <- predict(mcox, df_test_x[params], type = "lp")
      plasso0 <-
        predict(mcoxlasso,as.matrix(df_test_x[params]),
                lambda = lambda.min,type = "link")
      psrf0 <- survcompare::survsrf_predict(msrf, df_test_x, fixed_time = 10)
      
      y_test = Surv(df_test_x$time, df_test_x$event)
      c0_cox <- concordancefit(y = y_test, x = 1 - pcox0)$concordance
      
      c0_lasso <- concordancefit(y = y_test, x = 1 - plasso0)$concordance
      c0_srf <- concordancefit(y = y_test, x = 1- psrf0)$concordance
      remove(mcox, mcoxlasso, msrf)
      
      return(c(c0_cox, c0_lasso, c0_srf))
    }
    #  CC 
    cind_0<- getcindex(df_train_0, df_test_0) # 87 85 86
    cind_1<- getcindex(df_train_1, df_test_1) # 73 71 
    cind_2<- getcindex(df_train_2, df_test_2) # 72 71
    cind_3<- getcindex(df_train_3, df_test_3) # 70 72
    r<- rbind(cind_0, cind_1, cind_2, cind_3)
    colnames(r)<- c("CoxPH", "CoxLasso", "SRF")
    return(r)
} 

one_cv_run <-
  function(df, 
           params_impute,
           params,
           p,
           mechi,
           pattern = NULL,
           cv_number=3) {
    
    if (is.null(pattern)){ pattern = rep(0, length(params_impute))}

    d1 <- create_missing(df = df, params = params_impute,
        mech = mechi,prop = p, pattern = pattern)
    print(mv(d1))
    d1_mice <- impute_mice(d1, params_impute)$df
    d1_missForest <-
      impute_missforest(d1, params_impute, correctwealth = FALSE)$df
    
    cv_folds = caret::createFolds(d1$event, k = cv_number, list = FALSE)
    
    results_cv <- list()

    for (j in (1:cv_number)) {
      results_cv[[j]] <- one_fold_run(k=j, cv_folds, d1, d1_mice, d1_missForest)
    }
    
    results_cv_means <-  results_cv[[1]]/cv_number
    for (j in (2:cv_number)) {
      results_cv_means <- results_cv_means + results_cv[[j]]/cv_number
    }  
    
    remove(d1, d1_mice, d1_missForest)
    return(results_cv_means)
    #average over cv iterations 
  } 


#######################################################
mechanisms<-c("MCAR", "MAR","MNAR")
missing_p <- seq(0, 0.6, 0.15)
#create missingness in any variable
default_pattern_any = c(rep(0,length(params)))
n <- 1:1
default_pattern_1 = rbind(c(1,1,0,0,0,0,1,1,0,0),c(1,1,1,1,0,0,0,0,1,1))
grid1<- expand.grid("n" = n, "p" = missing_p, "mech" = mechanisms)


# Use parallel calculations 
library(parallel)
cl <- makeCluster(4)
# Pass information and functions to the cluster environment 
clusterEvalQ(cl, {library(survcompare)})
clusterEvalQ(cl, {library(survival)})
clusterEvalQ(cl, {library(caret)})
clusterEvalQ(cl, {library(missForest)})
clusterEvalQ(cl, {library(mice)})

clusterExport(cl, c('impute_mean', 'impute_mean_test','impute_cc', 
                    'impute_mice', 'impute_mice_test',
                    'impute_missforest', 'impute_missforest_test', 
                    'create_missing',
                    'params', 'params_impute', 'diabetes', 
                    'default_pattern_any', 
                    'grid1' , 'df', 'one_fold_run', 'one_cv_run',
                    'run_function', 'mv'), 
              envir = .GlobalEnv)

run_function <- function(X){
  p = grid1[X, "p"]
  mechi = as.character(grid1[X, "mech"])
  trial1<-one_cv_run(df, params_impute, params, p, mechi)
  return(trial1)
}

df<- survcompare::simulate_nonlinear(500)
params <- c("age", "bmi", "hyp", "sex")
params_impute <- c("age", "bmi", "hyp", "sex")
# i=12
# p = grid1[i, "p"]
# mechi = as.character(grid1[i, "mech"])
# trial1<- one_cv_run(df, params_impute, params, p, mechi)

parLapply(cl, X = 1:dim(grid1)[1], fun = run_function)

stopCluster(cl)

