#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3


train <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("xgboost")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
    nr <- 6
  } else {
    depth <- par$depth
    nr <- par$nr
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgb <- xgboost(data = as.matrix(featMat), 
                       label = as.matrix(labMat),
                       max_depth=depth, 
                       nrounds = nr,
                       verbose=0,
                       objective = "reg:linear",
                       eval_metric = "rmse", 
                       lambda = 1, 
                       alpha = 0)
#    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_xgb)
  }
  
  return(modelList)
}