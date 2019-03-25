########################
### Super-resolution ###
########################

### Author: Group 7
### Project 3

##helper function that calculates MSE  
mse <- function(x_hat,x) {   
  mean((x_hat-x)^2)  
}  


#calculate PSNR  
psnr <- function(x_hat,x) {  
  10 * log10(1/mse(x_hat,x))  
}  




calculate_psnr <- function(pred_dir, true_dir){  
  # library(OpenImageR) 
  library("EBImage")
  n_pred <- length(list.files(pred_dir))  
  n_true <- length(list.files(true_dir))  
  if(n_pred != n_true){  
    stop("input lists must be same length")  
    }  
  list <- matrix(0, nrow = (n_pred + 1), ncol = 1)
  name <- c("Total") 
  
  for (i in 1:n_pred){
    imgPred <- readImage(paste0(pred_dir,  "img_", sprintf("%04d", i), ".jpg")) 
    imgTrue <- readImage(paste0(true_dir,  "img_", sprintf("%04d", i), ".jpg"))  
    list[i+1,] <- psnr(imgPred, imgTrue)  
    name <- c(name, paste0("img_", sprintf("%04d", i)))  
  }  
  
  list[1,] <- mean(list[-1,1]) 
  row.names(list) <- name
  colnames(list) <- substring(pred_dir,18) 
  return(list)  
}  