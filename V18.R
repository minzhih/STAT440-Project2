setwd("/Users/renachoi/Desktop/STAT 440/project2/data")
train <- read.csv("train.csv")
#rawdata <- read.csv("train.csv")
test <- read.csv("test.csv")
valid <- read.csv("valid.csv")
pred <- read.csv("pred.csv")

rawdata <- read.csv("train.csv")

library(tidyverse)

f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
#  if (length(na.pos) %in% c(0, N)) {
#    return(dat)
#  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}


########V14
train$V14 <- f1(train$V14)
index<-valid[valid$co%in%"V14",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V14",3]
a<-0
for(i in id){
  if(train[index[i],"V14"]==value[i]){
    a=a+1
  }
}
a/length(index)

#############V13
train$V13 <- f1(train$V13)
index<-valid[valid$co%in%"V13",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V13",3]
a<-0
for(i in id){
  if(train[index[i],"V13"]==value[i]){
    a=a+1
  }
}
a/length(index)

##############V12
train$V12 <- f1(train$V12)
index<-valid[valid$co%in%"V12",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V12",3]
a<-0
for(i in id){
  if(train[index[i],"V12"]==value[i]){
    a=a+1
  }
}
a/length(index)

####################V06 - year block
#index6<-pred[gsub(".:","",pred$Id)%in%"V06",1]
#index6<-gsub(":.","",index6)
#index6<-as.numeric(index6)
#dataa <- matrix(ncol=2, nrow=length(index6))
#index<-which(is.na(train$V06))
##a<-0
### V6 free now 
#for(i in index){
#  if(train[i-2,6]==train[i+2,6]){
#    train[i,6]<-train[i-2,6]
#    #a=a+1
#  }
#}

rawdata$V06 <- f1(rawdata$V06)

index<-valid[valid$co%in%"V06",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V06",3]
a<-0
for(i in id){
  if(rawdata[index[i],"V06"]==value[i]){
    a=a+1
  }
}
a/length(index)


####################V10
#alternative option: reorder and use f1

for(i in 1:nrow(train)){
  n <-  is.na(train$V10[i])
  if(n == TRUE){
    if(isTRUE(train$V10[i-1] == train$V10[i+1])){
      train$V10[i] = train$V10[i-1]
    }
    else{
      C10 <- rep(0,4)
      R10 <- c(train$V10[i-2],train$V10[i-1],train$V10[i+1],train$V10[i+2])
      R9 <- c(train$V09[i-2],train$V09[i-1],train$V09[i+1],train$V09[i+2])
      for(j in 1:length(R10)){
        C10[j] <- abs(R9[j]-train$V09[i])
      }
      train$V10[i] = R10[which.min(C10)]
    }
  }
}


index<-valid[valid$co%in%"V10",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V10",3]
a<-0
for(i in id ){
  if(train[index[i],"V10"]==value[i])
  {
    a=a+1
  }
}

a/length(index) #yields accuracy of 0.9776

#################V07 - PRUID
for(i in 1:nrow(train)){
  n <-  is.na(train$V07[i])
  if(n == TRUE){
    if(isTRUE(train$V07[i-1] == train$V07[i+1])){
      train$V07[i] = train$V07[i-1]
    }
    else{
      C7 <- rep(0,4)
      R7 <- c(train$V07[i-2],train$V07[i-1],train$V07[i+1],train$V07[i+2])
      R8 <- c(train$V08[i-2],train$V08[i-1],train$V08[i+1],train$V08[i+2])
      for(j in 1:length(R7)){
        C7[j] <- abs(R8[j]-train$V08[i])
      }
      train$V07[i] = R7[which.min(C7)]
    }
  }
}

index<-valid[valid$co%in%"V07",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V07",3]
a<-0
for(i in id ){
  if(train[index[i],"V07"]==value[i])
  {
    a=a+1
  }
}

a/length(index)


######################V08 - CUID

train$V08 <- f1(train$V08)

index<-valid[valid$co%in%"V08",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V08",3]
a<-0
for(i in id ){
  if(isTRUE(train[index[i],"V08"]==value[i]))
  {
    a=a+1
  }
}

a/length(index) #yields 0.9645043 accuracy

##################V09
train$V09 <- f1(train$V09)

index<-valid[valid$co%in%"V09",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V09",3]
a<-0
for(i in id ){
  if(round(train[index[i],"V09"],4)==round(value[i],4))
  {
    a=a+1
  }
}

a/length(index) #yields 0.8992537 accuracy

##############V11
train$V11 <- f1(train$V11)

index<-valid[valid$co%in%"V11",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V11",3]
a<-0
for(i in id ){
  if(round(train[index[i],"V11"],4)==round(value[i],4))
  {
    a=a+1
  }
}

a/length(index) #yields 0.4346821 accuracy

#################V1-V5

data_imp <- mice(train, method="norm.predict",m=1)
data_det <- complete(data_imp)
any(is.na(data_det))


#####CREATE THE CLUSTER BY V07

#evaluate the model
value = rep(0,dim(valid)[1])
for(i in 1:dim(valid)[1]){
  row = valid[i,1]
  col = valid[i,2]
  value[i] = data_det[row,col]
}
mae = mean(abs(value - valid[,3]))
mae

#impute the result into the test set
for(i in 1:dim(test)[1]){
  row = test[i,1]
  col= test[i,2]
  value = data_det[row,col]
  test[i,3] = value
  pred[i, 2] = value
  #cat(sprintf("%d:%s,%f\n",row,col,value))
}


write.table(pred, file = "pred_v8.csv",
            sep = ',',
            quote = FALSE,
            row.names = FALSE)
