
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
# # CREATE DATA WITH MISSING DATA
# d1 <- create_missing(diabetes, params_impute, "MAR", 0.5, 2024,
#                      pattern = default_pattern_any)
# d1_mice <- impute_mice(d1, params_impute, randomseed = 2024)$df
# d1_missForest <-
#   impute_missforest(d1, params_impute, randomseed = 2022)$df
# table(diabetes$baseline_wealth_n, d1_missForest$baseline_wealth_n)
# #            0    1    2
# # low /0  1116  499    0
# # med /1     0 1779    0
# # high/2     0  604 1349
# table(diabetes$baseline_wealth_n, d1_mice$baseline_wealth_n)
# #     0    1    2
# # 0 1274  169  172
# # 1  139 1442  198
# # 2  182  206 1565
# params <- c("baseline_age", "sex", "baseline_bmi",
#             "baseline_hyp", "baseline_cvd",
#             "baseline_exercise" , "Education",
#             "baseline_B_dep",  "baseline_hba1c",
#             "wealth_low",  "wealth_high")

######################### START HERE ############################
# load the data
diabetes<-read.csv('diabetes_data_for_analyses.csv')

# 0-1-2 for low, med. high wealth
diabetes$baseline_wealth_n <-
  ifelse(diabetes$baseline_wealth == "low", 0,
         ifelse(diabetes$baseline_wealth == "med", 1,2))

df<- diabetes
params <- c("baseline_age", "sex", "baseline_bmi",
                   "baseline_hyp", "baseline_cvd",
                   "baseline_exercise" , "Education",
                   "baseline_B_dep",  "baseline_hba1c",
                   "baseline_wealth_n")
cols_to_miss <- c("baseline_bmi", "baseline_hba1c", "baseline_cvd")
params_impute<- params
dep_cols <- c("baseline_age", "sex", "Education")


# grid for tests
mechanisms<-c("MCAR", "MAR", "MNAR")
missing_p <- c(0, 0.2, 0.4, 0.6, 0.8) # seq(0.1, 0.7, 0.2)
n <- 1:4 # trials for each combination of MCAR & p
grid1<- expand.grid("n" = n, "p" = missing_p, "mech" = mechanisms)
dim(grid1)
source("Missing_data_utils.R")


##############   Non - parallel way ##############################
library(survival)
library(mice)
library(missForest)
library(caret)
library(survcompare)
# non-parallel way
t1<- Sys.time()
myrun <- lapply(1:dim(grid1)[1], FUN = run_function)
t1<- Sys.time()-t1

a <- run_function(1)
# CoxPH  CoxLasso       SRF   p mech n
# CC      0.8552415 0.8331400 0.8552415 0.2 MCAR 1
# Mean    0.8138497 0.7987222 0.8138497 0.2 MCAR 1
# MICE    0.7907970 0.7772495 0.7907970 0.2 MCAR 1
# mForest 0.8135985 0.7985560 0.8135985 0.2 MCAR 1
a2 <- run_function(2)

a3 <- run_function(3)
#   n   p mech
# 3 1 0.8 MCAR

a <- run_function(4)
# CoxPH  CoxLasso       SRF   p mech n
# CC      0.8535243 0.8398696 0.8535243 0.2  MAR 1
# Mean    0.8117550 0.7892792 0.8117550 0.2  MAR 1
# MICE    0.7897411 0.7707404 0.7897411 0.2  MAR 1
# mForest 0.8106608 0.7905417 0.8106608 0.2  MAR 1
a7 <- run_function(7)
#   n   p mech
# 7 1 0.2 MNAR
# CoxPH  CoxLasso       SRF   p mech n
# CC      0.8283368 0.8108159 0.8283368 0.2 MNAR 1
# Mean    0.8288210 0.8112455 0.8288210 0.2 MNAR 1
# MICE    0.8159598 0.8000880 0.8159598 0.2 MNAR 1
# mForest 0.8188861 0.8022296 0.8188861 0.2 MNAR 1
a9 <- run_function(9)
#   n   p mech
# 9 1 0.8 MNAR
# CoxPH  CoxLasso       SRF   p mech n
# CC      0.8762452 0.5397834 0.8762452 0.8 MNAR 1
# Mean    0.7963549 0.7656033 0.7963549 0.8 MNAR 1
# MICE    0.6867655 0.6605364 0.6867655 0.8 MNAR 1
# mForest 0.6900024 0.6645695 0.6900024 0.8 MNAR 1

print(t1) # 36 sec for 100 observations vs 18sec in parallel
myrun

######################################################

# Use parallel calculations
library(parallel)
cl <- makeCluster(4)
# Pass information and functions to the cluster environment
clusterEvalQ(cl, {library(survcompare)})
clusterEvalQ(cl, {library(survival)})
clusterEvalQ(cl, {library(caret)})
clusterEvalQ(cl, {library(missForest)})
clusterEvalQ(cl, {library(mice)})
clusterExport(cl, c('impute_mean',
                    'impute_mean_test',
                    'impute_cc',
                    'impute_mice',
                    'impute_mice_test',
                    'impute_missforest',
                    'impute_missforest_test',
                    'create_missing',
                    'params',
                    'params_impute',
                    'cols_to_miss',
                    'grid1',
                    'df',
                    'one_fold_run',
                    'one_cv_run',
                    'run_function',
                    'mv',
                    'dep_cols'),
              envir = .GlobalEnv)

# RUN
t1<- Sys.time()
myrun_diabetes <- parLapply(cl, X = 1:dim(grid1)[1], fun = run_function)
t1<- Sys.time()-t1
print(t1) #49 min for n=1000 and grid size 120 nonlinear(25sec/cv)
stopCluster(cl)

postanalysis(myrun_diabetes, grid1, save = TRUE,
             mainDir = "C:/Users/dinab/Desktop/PhD Projects/Ensemble methods/GitHub_App/missing-data-in-healthcare/Results",
             subDir = "2024_07_08_Diabetes_ext")


