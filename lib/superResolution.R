########################
### Super-resolution ###
########################


### Project 3

### GBM

superResolution <- function(LR_dir, HR_dir, modelList){
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  PSNR <- NULL
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    print(i)
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    pixel <- LR_nrow * LR_ncol
    featMat <- array(NA, c(pixel, 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    r <- (1 : pixel - 1) %% LR_nrow + 1
    c <- (1 : pixel - 1) %/% LR_nrow + 1
    
    for (k in c(1:3)) {
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],
                                            imgLR[LR_nrow, LR_ncol, k]))
      center=supp_imgLR[cbind(r+1, c+1)]
      
      ### fill the featM 
      featMat[,  1, k] <- supp_imgLR[cbind(r,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r,c + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r,c + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 1,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 1,c + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c + 2)] - center
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    
    predMat <- array(predMat, dim = c(dim(imgLR)[1]*dim(imgLR)[2],4,3))
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    
    predMat[, , 1] <- predMat[, , 1] + imgLR[, ,1][cbind(r,c)]
    predMat[, , 2] <- predMat[, , 2] + imgLR[, ,2][cbind(r,c)]
    predMat[, , 3] <- predMat[, , 3] + imgLR[, ,3][cbind(r,c)]
    
    
    imgHR_fit <- array(0, c(LR_nrow*2, LR_ncol*2, 3))
    imgHR_fit <- Image(imgHR_fit, colormode = Color)
    
    base_row <- seq(1, 2 * LR_nrow, 2)
    base_col <- seq(1, 2 * LR_ncol, 2)
    imgHR_fit[base_row, base_col, ] <- predMat[, 1, ]
    imgHR_fit[base_row, base_col + 1, ] <- predMat[, 2, ]
    imgHR_fit[base_row + 1, base_col, ] <- predMat[, 3, ]
    imgHR_fit[base_row + 1, base_col + 1, ] <- predMat[, 4, ]
    
    # calculate MSE and PSNR
    mse <- sum((imgHR - imgHR_fit)^2)/(3*pixel)
    psnr <- 20*log10(range(imgHR)[2]) - 10*log10(mse)
    PSNR <- append(PSNR, psnr)
    #setwd(save_path)
    writeImage(Image(imgHR_fit), paste0("../data/test_set1/SR/","img_gbm_fit_", sprintf("%04d", i), ".jpeg"))
  }
  PSNR <- sum(PSNR)/n_files
  return(PSNR)
}











### XGB

superResolution_xgboost <- function(LR_dir,HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  PSNR <- NULL
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    #pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    pixel <- LR_nrow * LR_ncol
    featMat <- array(NA, c(pixel, 8, 3))
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    r <- (1 : pixel - 1) %% LR_nrow + 1
    c <- (1 : pixel - 1) %/% LR_nrow + 1
    
    for (k in c(1:3)) {
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      center=supp_imgLR[cbind(r+1, c+1)]
      
      ### fill the featM 
      featMat[,  1, k] <- supp_imgLR[cbind(r,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r,c + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r,c + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 1,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 1,c + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(r + 2,c + 2)] - center
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    predMat <- array(predMat, dim = c(dim(imgLR)[1]*dim(imgLR)[2],4,3))
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    predMat[, , 1] <- predMat[, , 1] + imgLR[, ,1][cbind(r,c)]
    predMat[, , 2] <- predMat[, , 2] + imgLR[, ,2][cbind(r,c)]
    predMat[, , 3] <- predMat[, , 3] + imgLR[, ,3][cbind(r,c)]
    
    imgHR_fit <- array(0, c(LR_nrow*2, LR_ncol*2, 3))
    imgHR_fit <- Image(imgHR_fit, colormode = Color)
    
    base_row <- seq(1, 2 * LR_nrow, 2)
    base_col <- seq(1, 2 * LR_ncol, 2)
    imgHR_fit[base_row, base_col, ] <- predMat[, 1, ]
    imgHR_fit[base_row, base_col + 1, ] <- predMat[, 2, ]
    imgHR_fit[base_row + 1, base_col, ] <- predMat[, 3, ]
    imgHR_fit[base_row + 1, base_col + 1, ] <- predMat[, 4, ]
    #calculate MSE and PSNR
    mse <- sum((imgHR - imgHR_fit)^2)/(3*pixel)
    psnr <- 20*log10(1) - 10*log10(mse)
    PSNR <- append(PSNR, psnr)
    #setwd(save_path)
    writeImage(Image(imgHR_fit), paste0("../data/test_set/SR/","img_xgboost_fit_", sprintf("%04d", i), ".jpeg"))
  }
  PSNR <- sum(PSNR)/n_files
  return(PSNR)
}