
# missing data percentage by column
mv <- function(df){apply(apply(df, 2, is.na), 2, sum)/dim(df)[1]}

# load the data
diabetes<-read.csv('diabetes_data_for_msc_2024.csv')
diabetes <- diabetes[names(diabetes)!="X"]
names(diabetes)
summary(diabetes)
dim(diabetes) #5603 23 

table(diabetes$event) # 424 cases
sum(is.na(diabetes)) #0

#binary for high and low wealth
diabetes$wealth_high<- ifelse(diabetes$baseline_wealth=="high",1,0)
diabetes$wealth_low<- ifelse(diabetes$baseline_wealth=="low",1,0)

#define params
params <- c("baseline_age", "sex", "baseline_bmi",
            "baseline_hyp", "baseline_cvd", 
            "baseline_exercise" , "Education", 
            "baseline_B_dep",  "baseline_hba1c",     
            "wealth_low",  "wealth_high")

cv_number = 5
cv_folds = caret::createFolds(diabetes$event, k = cv_number, list = FALSE)

df_train_cv = diabetes[cv_folds != 1,]
df_test_cv  = diabetes[cv_folds == 1,]
dim(df_test_cv); dim(df_train_cv) # 1121 / 4482   test / train

sum(is.na(diabetes))

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

{set.seed(123)
  p = 0.3 # percent missing
  missmech = "MNAR" #missingness mechanism
  amp_train <-
    mice::ampute(df_train_cv[params],
           prop = p,
           pattern = default_pattern_any,
           mech = missmech)$amp
  amp_test <-
    mice::ampute(df_test_cv[params],
           prop = p,
           pattern = default_pattern_any,
           mech = missmech)$amp
}
#overall missingness
sum(is.na(amp_train))/nall #0.3003

# percent of missing data by column
apply(apply(amp_train, 2, is.na), 2, sum) / dim(df_train_cv[params])[1]



# hist before and after 
hist(amp_train[!is.na(amp_train$baseline_age), "baseline_age"])
hist(diabetes$baseline_age)

library(survcompare)

d2<- impute_cc(diabetes, params)$df
d1<- impute_mean(diabetes, params)


impute_mean<- function(df, params,andomseed = NULL){
  for (c in params){
    if (c %in% names(df)){
          print(c)
          print(mean(df[,c], na.rm = TRUE))
          df[is.na(df[c]), c] = mean(df[,c], na.rm = TRUE)
    }
  }
  output = list()
  output$df = df
  return(output)
}
  
impute_cc<- function(df, params, randomseed = NULL){
  md <- apply(apply(df, 2, is.na), 2, sum)/dim(df)[1]  
  params_cc <- params
  # exclude missing data > 50% 
  for (c in params ){
    if (md[c]>0.5) {params_cc = params_cc[params_cc != c]
    }else{
      # if <50% missing, exclude row-wise
      df<- df[!is.na(df[c]), ]
    }
  }
  output = list()
  output$params = params
  output$params_cc = params_cc
  output$df = df
  return(output)
}

impute_mice <- function(df, params, randomseed = NULL) {
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  d3 <- mice::mice(
    df[, params],
    m = 1,
    seed = randomseed,
    ignore = NULL,
    method = "norm.nob"
  )
  output=list()
  output$df = complete(d3)
  return(output)
}


impute_missforest<- function(df, params, randomseed = NULL){
  if (is.null(randomseed)) {randomseed = sample(1:1e9,1)}
  {set.seed(randomseed);  mf<- missForest::missForest(df[params])}
  df[params] <- mf$ximp
  output=list()
  output$df = df
  return(output)
}

impute_missforest_test<- function(dftrain, dftest, params, randomseed = NULL)){
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
  output=list()
  output$df = dftest
  return(output)
}

