library(mice)
library(naniar)

n=100
x = data.frame(matrix(data = rnorm(n*n),nrow =  n, ncol = n))
dim(x)
pattern1 <- 1-diag(n)
head(pattern1)
pattern0<- c(rep(0, round(n/2)),rep(1, n-round(n/2)))

npatterns=50
pattern0.1<- data.frame(data =matrix(rbinom(n*npatterns,1,0.1), 
                    nrow = npatterns, 
                    ncol =n))
pattern0.2<- data.frame(data =matrix(rbinom(n*npatterns,1,0.2), 
                                     nrow = npatterns, 
                                     ncol =n))
pattern0.3<- data.frame(data =matrix(rbinom(n*npatterns,1,0.3), 
                                     nrow = npatterns, 
                                     ncol =n))
pattern0.4<- data.frame(data =matrix(rbinom(n*npatterns,1,0.4), 
                                     nrow = npatterns, 
                                     ncol =n))
pattern0.5<- data.frame(data =matrix(rbinom(n*npatterns,1,0.5), 
                                     nrow = npatterns, 
                                     ncol =n))
patterngrid <- rbind(pattern0.1, pattern0.2, pattern0.3, pattern0.4,pattern0.5)
dim(patterngrid) #250 x 100 
sum(patterngrid)/n/npatterns/5 #0.30 - number of missing on average in all patterns

xm <-  mice::ampute(x,prop = 0.1,pattern = patterngrid, mech = "MCAR",bycases = FALSE)$amp
sum(is.na(xm)) / n/n
naniar::vis_miss(xm)
apply(data.frame(xm), 1, FUN = function(x) {sum(is.na(x))})
apply(data.frame(xm), 2, FUN = function(x) {sum(is.na(x))})
# percent of observations with COMPLETE data
sum(apply(data.frame(xm), 1, FUN = function(x) {sum(is.na(x))})==0)/dim(x)[1]

xm1 <-  mice::ampute(x,prop = 0.3,bycases = FALSE, mech = "MAR", pattern = patterngrid)$amp
sum(is.na(xm1)) / n/n
naniar::vis_miss(xm1)
# by row
apply(data.frame(xm1), 1, FUN = function(x) {sum(is.na(x))})
# by column
apply(data.frame(xm1), 2, FUN = function(x) {sum(is.na(x))})
0.3/25

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

