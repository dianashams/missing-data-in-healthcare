
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
                    'diabetes', 
                    'default_pattern_any', 
                    'grid1',
                    'df', 
                    'one_fold_run', 
                    'one_cv_run',
                    'run_function',
                    'mv'), 
              envir = .GlobalEnv)

# RUN
t1<- Sys.time()
myrun_diabetes <- parLapply(cl, X = 1:dim(grid1)[1], fun = run_function)
t1<- Sys.time()-t1
print(t1) #49 min for n=1000 and grid size 120 nonlinear(25sec/cv)
stopCluster(cl)

postanalysis(myrun_diabetes, grid1, save = TRUE,
             mainDir = "~/Documents/GitHub/missing-data-in-healthcare/Results/",
             subDir = "2024_05_25_Diabetes")
