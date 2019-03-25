#divide data into testing and training set by 20:80

setwd("/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/") 
train_dir <- "../data/train_set/" # This will be modified for different data sets.
train_LR_dir <- paste(train_dir, "LR/", sep="")
train_HR_dir <- paste(train_dir, "HR/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="") 

LR=list.files(path="/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/LR")

HR=list.files(path="/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/LR")

df=data.frame(LR,HR)
set.seed(2018)
sample <-sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
training_set=df[sample,]
test_set=df[-sample,]

current.folder_LR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/LR"
current.folder_HR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/HR"
LR<-paste0("/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/LR/",list.files(current.folder_LR))
HR<-paste0("/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set/HR/",list.files(current.folder_HR))

train_LR <- LR[sample]
train_HR<-HR[sample]
test_LR<-LR[-sample]
test_HR<-HR[-sample]
new.folder_train_LR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set1/LR"
new.folder_train_HR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/train_set1/HR"
new.folder_test_LR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/test_set1/LR"
new.folder_test_HR<-"/Users/zhongming/Documents/GitHub/Spring2019-Proj3-spring2019-proj3-grp7/data/test_set1/HR"
file.copy(train_LR, new.folder_train_LR)
file.copy(train_HR,new.folder_train_HR)
file.copy(test_LR,new.folder_test_LR)
file.copy(test_HR,new.folder_test_HR)

setwd(new.folder_test_HR)
file.rename(list.files(path=new.folder_test_HR), paste0("img", "_", sprintf("%04d", 1:375), ".jpg"))


