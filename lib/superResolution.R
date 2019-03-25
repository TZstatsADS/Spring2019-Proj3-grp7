########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    n_row <- dim(imgLR)[1]
    n_col <- dim(imgLR)[2]
    imgLR_padding <- array(0, dim(imgLR) + c(2,2,0))
    imgLR_padding[1:n_row + 1, 1:n_col + 1, ] <- imgLR
    
    n_row_padding <- n_row + 2
    n_col_padding <- n_col + 2
    ind_row_padding <- c(rep(1, n_col_padding), rep(2:(n_row_padding-1), each=2), rep(n_row_padding, n_col_padding))
    ind_col_padding <- c(1:n_col_padding, rep(c(1, n_col_padding), n_row), 1:n_col_padding)
    ind_padding <- ind_row_padding + (ind_col_padding - 1) * n_row_padding
    
    ind_all <- 1:(n_row_padding * n_col_padding)
    ind_origin <- setdiff(ind_all, ind_padding)
    
    n_points <- n_row * n_col
    
    for(channel in 1:3){
      samp_ind <- ind_origin
      
      ## X3  X2  x1
      ## x4  ct  x8
      ## x5  x6  x7
      x1_ind <- samp_ind - 1 + n_row_padding
      x2_ind <- samp_ind - 1
      x3_ind <- samp_ind - 1 - n_row_padding
      x4_ind <- samp_ind - n_row_padding
      x5_ind <- samp_ind + 1 - n_row_padding
      x6_ind <- samp_ind + 1
      x7_ind <- samp_ind + 1 + n_row_padding
      x8_ind <- samp_ind + n_row_padding
      samplesLR <- imgLR_padding[,, channel][c(samp_ind, x1_ind, x2_ind, x3_ind, x4_ind, x5_ind, x6_ind, x7_ind, x8_ind)]
      samplesLR.mat <- matrix(samplesLR, nrow=n_points)
      X <- samplesLR.mat[,-1] - samplesLR.mat[,1]
      
      featMat[, , channel] <- X
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    predMat_array <- array(predMat, c(n_col*n_row, 4, 3))
    LRcenter <- array(imgLR, c(n_col*n_row, 1, 3))

    imgHR_recov <- array(NA, c(2*n_row, 2*n_col, 3))
    
  
    for(channel in 1:3){
      y23 <- as.vector(t(predMat_array[, c(2,3),channel] + LRcenter[,,channel]))
      y14 <- as.vector(t(predMat_array[, c(1,4),channel] + LRcenter[,,channel]))
      
      imgHR_recov[, seq(1, 2*n_col-1, by=2), channel] <- y23
      imgHR_recov[, seq(2, 2*n_col, by=2), channel] <- y14
    }
    
    imgHR_recov <- Image(imgHR_recov, colormode="Color")
    writeImage(imgHR_recov, paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    
    
    
  }
}