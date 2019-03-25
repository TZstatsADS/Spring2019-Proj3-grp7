#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  #print(n_files)
  
  lr_f<- function(lr_x, lr_y){
    f_t<- array(0,c(8, 3))
    LC_t0<- LC[c(lr_y-1,lr_y,lr_y+1), c(lr_x-1, lr_x, lr_x+1), ]
    LC_t<- LC_t0 - rep(LC_t0[2,2, ], each=9) 
    f_t[1:3, ]<- LC_t[1, , ]
    f_t[4:5, ]<- LC_t[2, c(1, 3), ]
    f_t[6:8, ]<- LC_t[3, , ]
    return(f_t)
  }
  
  
  hr_f<- function(hr_x, hr_y){
    f_t<- array(0, c(4,3))
    HR_t0<- HR_a[c(hr_y-1, hr_y), c(hr_x-1, hr_x), ]
    HR_t<- HR_t0 - rep(LC[hr_y/2+1, hr_x/2+1, ], each=4)
    f_t[1:2, ]<- HR_t[1, , ]
    f_t[3:4, ]<- HR_t[2, , ]
    return(f_t)
  }
 
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    ### step 1. sample n_points from imgLR
    lrxd<- dim(imgLR)[2]
    lryd<- dim(imgLR)[1]
    
    LR_x<- sample(lrxd, n_points, replace=TRUE)
    LR_y<- sample(lryd, n_points, replace=TRUE)
    
    
    LC1<- rbind(rep(0, lrxd+2), cbind(rep(0, lryd), as.matrix(imgLR[ , , 1]), rep(0, lryd)), rep(0, lrxd+2))
    LC2<- rbind(rep(0, lrxd+2), cbind(rep(0, lryd), as.matrix(imgLR[ , , 2]), rep(0, lryd)), rep(0, lrxd+2))
    LC3<- rbind(rep(0, lrxd+2), cbind(rep(0, lryd), as.matrix(imgLR[ , , 3]), rep(0, lryd)), rep(0, lrxd+2))
    LC<- array(0, c(lryd+2, lrxd+2, 3))
    LC[ , , 1]<- LC1
    LC[ , , 2]<- LC2
    LC[ , , 3]<- LC3
    
    ### step 2. for each sampled point in imgLR,
    
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        ###           tips: padding zeros for boundary points
    b<- 1000*(i-1)+1
    e<- 1000*i
    MID<- mapply(lr_f, LR_x+1, LR_y+1)
    featMat[b:e, , 1]<- t(MID[1:8, ])
    featMat[b:e, , 2]<- t(MID[9:16, ])
    featMat[b:e, , 3]<- t(MID[17:24, ])
    
        ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    HR_x<- 2*LR_x
    HR_y<- 2*LR_y
    HR_a<- as.array(imgHR)
    
    MID<- mapply(hr_f, HR_x, HR_y)
    labMat[b:e, , 1]<- t(MID[1:4, ])
    labMat[b:e, , 2]<- t(MID[5:8, ])
    labMat[b:e, , 3]<- t(MID[9:12, ])
    
    ### step 3. repeat above for three channels
      
  }
  return(list(feature = featMat, label = labMat))
}
