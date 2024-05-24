
# grid for simulations 
mechanisms<-c("MCAR", "MAR", "MNAR") 
missing_p <- seq(0, 0.6, 0.15)
n <- 1:5 # trials for each combination of MCAR & p
grid1<- expand.grid("n" = n, "p" = missing_p, "mech" = mechanisms)
dim(grid1) 

# define the data df, params for prediction, params for imputations
df<- survcompare::simulate_nonlinear(1000)
params <- c("age", "bmi", "hyp", "sex")
params_impute <- c("age", "bmi", "hyp", "sex")

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
myrun <- parLapply(cl, X = 1:dim(grid1)[1], fun = run_function)
t1<- Sys.time()-t1
print(t1) #49 min for n=1000 and grid size 120 nonlinear(25sec/cv)
stopCluster(cl)

postanalysis(myrun, grid1, save = TRUE,
             mainDir = "~/Documents/GitHub/missing-data-in-healthcare/Results/",
             subDir = "2024_05_25")
