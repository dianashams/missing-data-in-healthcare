#####################
#Importing Libraries#
#####################
library(plyr)
library(dplyr)
library(survival)
library(survminer)
library(mice)
library(missMethods)
library(ggplot2)
library(VIM)
library(missForest)
library(caret)
library(ClustImpute)
library(pmcalibration)
library(naniar)
library(imputeTS)
library(glmnet)
library(reshape2)
suppressPackageStartupMessages(require(MASS))
suppressPackageStartupMessages(require(norm))
suppressPackageStartupMessages(require(ggplot2))
library(devtools)
library(hdnom)
library(CalibrationCurves)
library(TH.data)

#####################
## Reading-in Data ##
#####################

setwd('/Users/harperclees-baron/Desktop/KCL/Thesis')
diabetes<-read.csv('diabetes.csv')
dialysis<-read.csv('Dialysis.csv')
cancer<-GBSG2
names(cancer)[names(cancer) == 'cens'] <- 'event'
View(cancer)
cancer$tgrade_n <- ifelse(cancer$tgrade == "I", 1, ifelse(cancer$tgrade == "II", 2, ifelse(cancer$tgrade == "III", 3, 0)))
cancer$tgrade<-cancer$tgrade_n
cancer$tgrade_n<-NULL
dummy<-dummyVars("~.", data = cancer)
cancer <- data.frame(predict(dummy, newdata = cancer))
names(cancer)[names(cancer) == "horTh.no"] <- "horThno"
names(cancer)[names(cancer) == "horTh.yes"] <- "horThyes"
names(cancer)[names(cancer) == "menostat.Pre"] <- "menostatpre"
names(cancer)[names(cancer) == "menostat.Post"] <- "menostatpost"

#####################
### Data Cleaning ###
#####################

diabetes$c_age<-diabetes$baseline_age-mean(diabetes$baseline_age) #centering age
diabetes$baseline_age<-NULL #removing previous age variable

#Dichotimizing wealth variable
diabetes$baseline_wealth_high<- ifelse(diabetes$baseline_wealth=="high",1,0)
diabetes$baseline_wealth_low<- ifelse(diabetes$baseline_wealth=="low",1,0)
diabetes$baseline_wealth_med<- ifelse(diabetes$baseline_wealth=="med",1,0)
diabetes$baseline_wealth <- NULL

##########################################
### Plotting Baseline Characteristics ###
##########################################

#histogram of age in df
mean(diabetes$baseline_age)
sd(diabetes$baseline_age)
hist(diabetes$baseline_age, col = "darkolivegreen", main = "Age Distribution in ELSA", xlab = "Age in Years", breaks = 50)
text(x = 80, y = 270, label = "Mean Age: 65 years")
text(x = 79, y = 255, label = "SD Age: 9.2 years")

#histogram of BMI in df
mean(diabetes$baseline_bmi, na.rm = T)
sd(diabetes$baseline_bmi, na.rm = T)
hist(diabetes$baseline_bmi, col = "darkolivegreen", main = "BMI Distribution in ELSA", xlab = "Body Mass Index", breaks = 50, xlim = c(15, 60))
text(x = 42, y = 500, label = "Mean BMI: 27.6")
text(x = 40.6, y = 475, label = "SD BMI: 4.7 ")

#pie chart of education levels in df
prop.table(table(diabetes["Education"]))
lbls <- paste(c('Low Education', 'Medium Education', 'High Education'), " ", c(31.5, 28.8, 39.8) , "%", sep="")
pie(prop.table(table(diabetes$Education)), labels = lbls, main = 'Education Category Distribution', col = c('darkseagreen1', 'darkseagreen', 'darkseagreen4'))

##removing incomplete cases from analysis set
diabcomplete <- diabetes[complete.cases(diabetes), ]

#editing dialysis dataset
dialysis['fac_disease.factor'] <-factor(dialysis$fac_disease)
dialysis$fac_disease<-NULL

###########################
### Defining functions ###
###########################

##MCAR SIMULATION FUNCTION
MCAR<-function(complete_df, proportion){
  eventtime_drop<- c(which(colnames(complete_df)=='event'), which(colnames(complete_df)=='time')) #removing time and event variables from missingness columns
  allcol<-setdiff(seq(ncol(complete_df)), eventtime_drop)
  for (i in allcol){ #iterating through columns of dataframe
    column<-complete_df[,i] #selecting column
    mask<-rbinom(length(column), size = 1, prob = proportion) #creating mask generated from binomial 
    column[mask == 1]<-NA #removing observations based on mask
    complete_df[,i]<-column #putting column with simulated missingness into dataframe
  }
  return(complete_df)
}


###MNAR SIMULATION
MNAR<-function(complete_df, proportion){
  eventtime_drop<- c(which(colnames(complete_df)=='event'), which(colnames(complete_df)=='time')) #removing time and event variables from missingness columns
  allcol<-setdiff(seq(ncol(complete_df)), eventtime_drop)
  for (i in allcol){ #iterating through columns of dataframe
    column<-complete_df[,i] #select a column
    scaled<-(column-mean(column))/sd(column) #scale data in that column
    shift <-log((1-proportion)/proportion) #calculate shift in logit function to get desired % missing
    prob<-1/(1+exp(1.3*scaled+shift)) #calculate probability of missingness relative to values
    mask<-rbinom(length(column), size = 1, prob = prob) #binomial mask 
    column[mask == 1]<-NA #removing observations based on mask
    complete_df[,i]<-column #putting column with simulated missingness into dataframe
  }
  return(complete_df)
}

###MAR SIMULATION
MAR<-function(complete_df, proportion){
  scaled<-scale(complete_df) #scale data in that column
  eventtime_drop<- c(which(colnames(complete_df)=='event'), which(colnames(complete_df)=='time')) #removing time and event variables from missingness columns
  allcol<-setdiff(seq(ncol(complete_df)), eventtime_drop)
  for (i in allcol){  #iterating through columns of dataframe
    column<-complete_df[,i] #select a column
    shift <-log((1-proportion)/proportion) #calculate shift in logit function to get desired % missing
    selector<-setdiff(seq(ncol(complete_df)), i)
    prob<-1/(1+exp(1.5*scaled[,selector[1]]+1.5*scaled[,selector[2]]+shift)) #calculate probability of missingness relative to values
    mask<-rbinom(length(column), size = 1, prob = prob) #binomial mask 
    column[mask == 1]<-NA #removing observations based on mask
    complete_df[,i]<-column #putting column with simulated missingness into dataframe
  }
  return(complete_df)
}

###Calibration Slope calculation function
mean_calibration_10_years<-function(cx, test_dat, survobj){
  set.seed(550+iteration) #explicitly setting random state
  calibframe<-data.frame(matrix(nrow=1045)) #empty matrix of length of test set
  calibtime<-10 #specifying 10 years as event time of interest
  minmod<-glmnet(model.matrix(survobj~., data = test_dat[, params]), survobj, family="cox", alpha=.5, lambda=cx$lambda.min) #extracting model with best lambda value based on training data
  sc<-glmnet_survcurve(minmod, survobj[,1], survobj[,2], x =model.matrix(survobj~., data = test_dat[, params]), survtime = calibtime) #Obtaining predicted probabilities of survival past 10 years in test data
  calibframe["pred_p"]<-1-sc$p
  calibframe["Survival"]<-ifelse((survobj[,1]<calibtime & survobj[,2] == 1),1, 0) #binary variable for if survival time is less than 10 years and participant has event (ie is not censored)
  #calibframe<-calibframe[censored_before_calib_time == 0, ] #removing those who are censored before 10 years or have survival times greater than 10 years
  cal<-val.prob.ci.2(calibframe$pred_p, calibframe$Survival)$stats
  return(c(cal["Slope"]))
}


##########################################################
### Creating empty vectors to store simulation results ###
##########################################################

##mean imputation results vector
meanresults<-data.frame(matrix(nrow = 0, ncol = 6))
colnames(meanresults)<-c('Miss_percent', 'Mechanism','Concordance', 'obsmissprop', 'iteration', 'calibration')

##missforest results vector
missforestresults<-data.frame(matrix(nrow = 0, ncol = 6))
colnames(missforestresults)<-c('Miss_percent', 'Mechanism','Concordance', 'obsmissprop', 'iteration', 'calibration')


##knn imputation results vector
knnresults<-data.frame(matrix(nrow = 0, ncol = 7)) #note extra unnused column here to allow for changing k-value in future research
colnames(knnresults)<-c('Miss_percent', 'Mechanism', 'numNN','Concordance', 'obsmissprop', 'iteration', 'calibration')

##MICE imputation  results vector
miceresults<-data.frame(matrix(nrow = 0, ncol = 6))
colnames(miceresults)<-c('Miss_percent', 'Mechanism','Concordance', 'obsmissprop', 'iteration', 'calibration')


#################################################
#### specifying simulation characteristics  ###
##################################################

#selecting variables of interest for modelling
params_diab <- c(
  "c_age",
  "sex",
  "baseline_bmi",
  "baseline_hyp",
  "baseline_cvd",
  "baseline_exercise" ,
  "Education",
  "baseline_B_dep",
  "baseline_wealth_low",
  "baseline_wealth_high"
)

params_dial <- c(
  "num_age",
  "num_begin",
  "fac_center",
  "fac_disease.factor"
)

params_cancer <- c(
  "horThno",
  "horThyes",
  "age",
  "menostatpre",
  "menostatpost",
  "tsize",
  "tgrade",
  "pnodes",
  "progrec",
  "estrec"
)

micemethods<-c('sample', 'logreg', 'pmm', 'sample', 'logreg', 'pmm', 'pmm', 'pmm', 'pmm', 'pmm', '', '')
#parameters to use for loop
params<-params_diab

##lambda grid
lambdas<-2^seq(0,-10, by=-0.1) ##should be by -0.1 

##params for rf
maxiter <- 10 #should normally be 10
ntree <- 50 #should normally be 50

##params for knn
neighbors <- c(25) #should normally be 5, 10, 15

##number of iterations
number_of_iterations<- 15 #should be 50

iteration<-1 #initialize iteration at 1, should always be 1



###change random seed for every split
##################################################################
###################### SIMULATION LOOP ###########################
##################################################################
for (iteration in seq(number_of_iterations)){
  print('#########!!!!!!!!!!##########!!!!!!')
  print(iteration)
  print('#########!!!!!!!!!!##########!!!!!!')
  set.seed(550+iteration) ##iterate through random states for nested cross validation
  #splitting test/train data
  train.index <- createDataPartition(diabcomplete$event, p = .7, list = FALSE) ##stratified 70/30 data split by outcome for test and train
  fulltrain <- diabcomplete[ train.index,]
  fulltest  <- diabcomplete[-train.index,]
  
  ### try such that imputation methods are blind to outcome data      
  
  
  for (o in c('MCAR','MAR','MNAR')){ #iterate through missingness mechanism
    for (m in seq(0, .61, by = .2)){ #iterate through missingness percentage
      print(m)
      print(o)
      
      ##Calling relevant missingness simulation function
      if (o == 'MCAR') {
        #may sometimes need to add [c(params, 'time', 'event')] after fulltrains
        train <- MCAR(fulltrain, proportion = m)
        test <- MCAR(fulltest, proportion = m)
      } else if (o == 'MAR') {
        train <- MAR(fulltrain, proportion = m)
        test <- MAR(fulltest, proportion = m)
      } else if (o == 'MNAR') {
        train <- MNAR(fulltrain, proportion = m)
        test <- MNAR(fulltest, proportion = m)
      } else {
        print('select valid missingness mechanism')
      }
      missprop<-5
     
      ################### 
      # mean imputation #
      ###################
      trainMean <-na_mean(train) #impute on train
      testMean <- na_mean(test) #impute on test
      print(c("meanimputed at", m))
      survtrainMean<-Surv(trainMean$time, trainMean$event) #training data survival object
      survtestMean<-Surv(testMean$time, testMean$event) #test data survival object
      cx<-cv.glmnet(model.matrix(survtrainMean~., data = trainMean[, params]), survtrainMean, alpha=.5, family='cox',nfolds = 5,lambda = lambdas, type.measure = "C") #5-fold cross validated hyperparameter tuning on training data
      linpredictions<-predict(cx,newx=model.matrix(survtestMean~., data = testMean[, params]),s="lambda.min",type="link") #this is beta * x, linear predictor for test data
      cindex_test = 1-concordancefit(survtestMean, linpredictions)$concordance #model concordance
      calibration <- mean_calibration_10_years(cx = cx, test_dat = testMean, survobj = survtestMean) #model calibration
      meanresults[nrow(meanresults) + 1,] = c(m, o, cindex_test, missprop, iteration, calibration) #storing results as a new row in results vector
      
      ################### 
      # missforest imp  #
      ################### 
      
      trainMisspreds <-missForest(train[c(params)], verbose = F, maxiter = maxiter, ntree = ntree)$ximp #impute on train
      testMisspreds <- missForest(test[c(params)], verbose = F, maxiter = maxiter, ntree = ntree)$ximp #impute on test
      trainMiss<-cbind(trainMisspreds, train[c('event', 'time')])
      testMiss<-cbind(testMisspreds, test[c('event', 'time')])
      print(c("missimputed at", m))
      survtrainMiss<-Surv(trainMiss$time, trainMiss$event) #training data survival object
      survtestMiss<-Surv(testMiss$time, testMiss$event) #test data survival object
      cx<-cv.glmnet(model.matrix(survtrainMiss~., data = trainMiss[, params]), survtrainMiss, alpha=.5, family='cox',nfolds = 5,lambda = lambdas, type.measure = "C") #5-fold cross validated hyperparameter tuning on training data
      linpredictions<-predict(cx,newx=model.matrix(survtestMiss~., data = testMiss[, params]),s="lambda.min",type="link") #this is beta * x, linear predictor for test data    
      cindex_test = 1-concordancefit(survtestMiss, linpredictions)$concordance #model concordance
      calibration<-mean_calibration_10_years(cx = cx, test_dat = testMiss, survobj = survtestMiss) #model calibration
      missforestresults[nrow(missforestresults) + 1,] <- c(m, o, cindex_test, missprop, iteration, calibration) #storing results as a new row in results vector
      
      ################### 
      # knn imputation  #
      ################### 
      for (k in neighbors){ #can iterate through number of neighbors if desired
        trainknnpreds <- kNN(data=train[c(params)], k = k, imp_var="FALSE", numFun = median,catFun = maxCat) #impute on train
        testknnpreds  <- kNN(data=test[c(params)], k = k, imp_var="FALSE", numFun = median,catFun = maxCat) #impute on test
        trainknn<-cbind(trainknnpreds, train[c('event', 'time')])
        testknn<-cbind(testknnpreds, test[c('event', 'time')])
        print(c("knnimputed at", m))
        survtrainknn<-Surv(trainknn$time, trainknn$event) #training data survival object
        survtestknn<-Surv(testknn$time, testknn$event) #test data survival object
        cx<-cv.glmnet(model.matrix(survtrainknn~., data = trainknn[, params]), survtrainknn, alpha=.5, family='cox',nfolds = 5,lambda = lambdas, type.measure = "C") #5-fold cross validated hyperparameter tuning on training data
        linpredictions<-predict(cx,newx=model.matrix(survtestknn~., data = testknn[, params]),s="lambda.min",type="link") #this is beta * x, linear predictor for test data  
        cindex_test = 1-concordancefit(survtestknn, linpredictions)$concordance  #model concordance
        calibration<-mean_calibration_10_years(cx = cx, test_dat = testknn, survobj = survtestknn) #model calibration
        knnresults[nrow(knnresults) + 1,] = c(m, o, k, cindex_test, missprop, iteration, calibration) #storing results as a new row in results vector
      }

      ################### 
      # MICE imputation #
      ###################      
        trainmicepreds<-complete(mice(train[c(params)], m = 1, print = F, remove.collinear = F)) #impute on train
        testmicepreds<-complete(mice(test[c(params)], m = 1, print = F,  remove.collinear = F)) #impute on test
        trainmice<-cbind(trainmicepreds, train[c('event', 'time')])
        testmice<-cbind(testmicepreds, test[c('event', 'time')])  
        print(c("miceimputed at", m))
        survtrainmice<-Surv(trainmice$time, trainmice$event) #training data survival object
        survtestmice<-Surv(testmice$time, testmice$event)  #test data survival object
        cx<-cv.glmnet(model.matrix(survtrainmice~., data = trainmice[, params]), survtrainmice, alpha=.5, family='cox',nfolds = 5,lambda = lambdas, type.measure = "C") #5-fold cross validated hyperparameter tuning on training data
        linpredictions<-predict(cx,newx=model.matrix(survtestmice~., data = testmice[, params]),s="lambda.min",type="link") #this is beta * x, linear predictor  for test data 
        cindex_test = 1-  concordancefit(survtestmice, linpredictions)$concordance #model concordance
        calibration<-mean_calibration_10_years(cx = cx, test_dat = testmice, survobj = survtestmice) #model calibration
        miceresults[nrow(miceresults) + 1,] = c(m, o, cindex_test, missprop, iteration, calibration)  #storing results as a new row in results vector
    }
  }
}



#####################
# Reshaping results #
#####################

allresults<-as.data.frame(c(meanresults, missforestresults, knnresults, miceresults, iteration)) #storing all results by iteration

#removing redundant columns in results 
allresults[, grep(c("Miss_percent."), names(allresults))]<-NULL 
allresults[, grep(c("obsmissprop."), names(allresults))]<-NULL
allresults[, grep(c("Mechanism."), names(allresults))]<-NULL
allresults[, grep(c("iteration."), names(allresults))]<-NULL

#renaming  concordance results by corresponding imputation method
names(allresults)[which(names(allresults)=='Concordance')]<-'mean_C'
names(allresults)[which(names(allresults)=='Concordance.1')]<-'missfor_C'
names(allresults)[which(names(allresults)=='Concordance.2')]<-'knn_C'
names(allresults)[which(names(allresults)=='Concordance.3')]<-'mice_C'

#renaming calibration results by corresponding imputation method
names(allresults)[which(names(allresults)=='calibration')]<-'mean_cal'
names(allresults)[which(names(allresults)=='calibration.1')]<-'missfor_cal'
names(allresults)[which(names(allresults)=='calibration.2')]<-'knn_cal'
names(allresults)[which(names(allresults)=='calibration.3')]<-'mice_cal'

##saving results to csv
write.csv(allresults,"allresults_cancer_feb52025.csv", row.names = FALSE)

###dividing results into separate frames for concordance and calibration ##(NEED TO ADD mice_C and mice_cal BACK IN )
allresmelt_con<-melt(allresults, id.vars = c('Miss_percent', 'Mechanism', 'iteration'), measure.vars = c('mean_C', 'missfor_C', 'knn_C', 'mice_C'))
allresmelt_cal<-melt(allresults, id.vars = c('Miss_percent', 'Mechanism', 'iteration'), measure.vars = c('mean_cal','missfor_cal','knn_cal', 'mice_cal'))
allresmelt_con$variable<-gsub("_C$", "", allresmelt_con$variable)
allresmelt_cal$variable<-gsub("_cal$", "", allresmelt_cal$variable)
allresmelt<-merge(allresmelt_con, allresmelt_cal, by=c('Miss_percent', 'Mechanism','variable','iteration'), suffixes = c('Concordance', 'Calibration'))
allresmelt$valueConcordance<-as.numeric(allresmelt$valueConcordance)
allresmelt$valueCalibration<-as.numeric(allresmelt$valueCalibration)


##summarizing over iteration for concordance and calibration to get means and SDs for each combination of percentage, mechanism, and imputation type
iteration_summary<-allresmelt %>%
  group_by(Miss_percent, Mechanism, variable) %>%
  summarise(avC = mean(valueConcordance), sdC = sd(valueConcordance), avCal = mean(valueCalibration, na.rm = T), sdCal = sd(valueCalibration, na.rm = T))

###################
# Plotting results#
###################

###concordance plot
iteration_summary['upper']<-((1.96/sqrt(number_of_iterations))*iteration_summary$sdC+iteration_summary$avC) #upper bound of 95% CI
iteration_summary['lower']<-(-(1.96/sqrt(number_of_iterations))*iteration_summary$sdC+iteration_summary$avC) #lower bound of 95% CI
mechfacit<-ggplot(iteration_summary, aes(Miss_percent, y = avC, color = variable)) + geom_point() +
  facet_wrap('Mechanism', nrow = 3) + geom_errorbar(ymin = iteration_summary$lower, ymax = iteration_summary$upper) +
  ylab('Concordance') + xlab('Percent Missing')+ylim(c(0.50, 0.75))
mechfacit<-mechfacit+ggtitle('Concordance by Imputation Method and % Missing')+ labs(color = "Imputation Method") 
#+ scale_color_discrete(labels = c("KNN", "Mean", "MissForest", "mice")) ##adding title and legend
mechfacit

##calibration plot
iteration_summary['upper']<-((1.96/sqrt(number_of_iterations))*iteration_summary$sdCal+iteration_summary$avCal) #upper bound of 95% CI
iteration_summary['lower']<-(-(1.96/sqrt(number_of_iterations))*iteration_summary$sdCal+iteration_summary$avCal) #lower bound of 95% CI
mechfacit<-ggplot(iteration_summary, aes(Miss_percent, y = avCal, color = variable)) + geom_point() +
  facet_wrap('Mechanism', nrow = 3) + geom_errorbar(ymin = iteration_summary$lower, ymax = iteration_summary$upper) +
  ylab('Calibration') + xlab('Percent Missing')
mechfacit<-mechfacit+ggtitle('Calibration by Imputation Method and % Missing')+  labs(color = "Imputation Method") +
  scale_color_discrete(labels = c("KNN", "Mean", "MissForest", "MICE")) ##adding title and legend
mechfacit



