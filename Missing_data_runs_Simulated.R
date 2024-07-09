
# grid for simulations 
mechanisms<-c("MAR") #c("MCAR", "MAR", "MNAR") 
missing_p <- c(0.2,0.5,0.8 ) # seq(0.1, 0.7, 0.2)
n <- 1 # trials for each combination of MCAR & p
grid1<- expand.grid("n" = n, "p" = missing_p, "mech" = mechanisms)
dim(grid1) 
grid1
# define the data df, params for prediction, params for imputations
df<- survcompare::simulate_nonlinear(1000)
params <- c("age", "bmi", "hyp", "sex")
cols_to_miss <- c("age", "hyp")
params_impute<- params
dep_cols <- c("bmi", "sex")


t1<- Sys.time()
myrun_0 <- apply(cl, X = 1:dim(grid1)[1], FUN = run_function)
t1<- Sys.time()-t1



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
myrun <- parLapply(cl, X = 1:dim(grid1)[1], fun = run_function)
t1<- Sys.time()-t1
print(t1) #49 min for n=1000 and grid size 120 nonlinear(25sec/cv)
stopCluster(cl)

postanalysis(myrun, grid1, save = TRUE,
             mainDir = "C:/Users/dinab/Desktop/PhD Projects/Ensemble methods/GitHub_App/missing-data-in-healthcare/Results",
             subDir = "2024_06_28")
