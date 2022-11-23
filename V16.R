setwd("/Users/renachoi/Desktop/STAT 440/project2/data")
train <- read.csv("train.csv")
#rawdata <- read.csv("train.csv")
test <- read.csv("test.csv")
valid <- read.csv("valid.csv")
pred <- read.csv("pred.csv")

library(tidyverse)

########V14
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
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

train$V14[1270:1278]
train[1270:1278,]

#############V13
length(unique(train$V13))
unique(train$V13)

train %>% 
  rownames_to_column() %>% 
  group_by(V13) %>%
  filter(!duplicated(V13))

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
length(unique(train$V12))
unique(train$V12)

train %>% 
  rownames_to_column() %>% 
  group_by(V12) %>%
  filter(!duplicated(V12))

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
index6<-pred[gsub(".:","",pred$Id)%in%"V06",1]
index6<-gsub(":.","",index6)
index6<-as.numeric(index6)
dataa <- matrix(ncol=2, nrow=length(index6))
index<-which(is.na(train$V06))
#a<-0
## V6 free now 
for(i in index){
  if(train[i-2,6]==train[i+2,6]){
    train[i,6]<-train[i-2,6]
    #a=a+1
  }
}

index<-valid[valid$co%in%"V06",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V06",3]
a<-0
for(i in id){
  if(train[index[i],"V06"]==value[i]){
    a=a+1
  }
}
a/length(index)


####################V10
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
for(i in 1:nrow(train)){
  n <-  is.na(train$V08[i])
  if(n == TRUE){
    if(isTRUE(train$V08[i-1] == train$V08[i+1])){
      train$V08[i] = train$V08[i-1]
    }
    else if(isFALSE(is.na(train$V08[i-1])) && isFALSE(is.na(train$V08[i+1]))){
      meandata = c(train$V08[i+1],train$V08[i-1])
      train$V08[i] = mean(meandata)
    }
    else{
      C78 <- rep(0,4)
      R88 <- c(train$V07[i-2],train$V07[i-1],train$V07[i+1],train$V07[i+2])
      R78 <- c(train$V08[i-2],train$V08[i-1],train$V08[i+1],train$V08[i+2])
      for(j in 1:length(R88)){
        C78[j] <- abs(R78[j]-train$V07[i])
      }
      train$V08[i] = R88[which.min(C78)]
    }
  }
}

index<-valid[valid$co%in%"V08",1]
id<-c(1:length(index))
value<-valid[valid$co%in%"V08",3]
a<-0
for(i in id ){
  if(train[index[i],"V08"]==value[i])
  {
    a=a+1
  }
}

a/length(index) #yields 0.9314565 accuracy

##################V09
unique(train$V09)
which(is.na(train$V09))

for(i in 1:nrow(train)){
  n <-  is.na(train$V09[i])
  if(n == TRUE){
    if(isTRUE(train$V09[i-1] == train$V09[i+1])){
      train$V09[i] = train$V09[i-1]
    }
    else if(isFALSE(is.na(train$V09[i-1])) && isFALSE(is.na(train$V09[i+1]))){
      meandata = c(train$V09[i+1],train$V09[i-1])
      train$V09[i] = mean(meandata)
    }
    else{
      C109 <- rep(0,4)
      R109 <- c(train$V10[i-2],train$V10[i-1],train$V10[i+1],train$V10[i+2])
      R910 <- c(train$V09[i-2],train$V09[i-1],train$V09[i+1],train$V09[i+2])
      for(j in 1:length(R88)){
        C109[j] <- abs(R109[j]-train$V10[i])
      }
      train$V09[i] = R910[which.min(C109)]
    }
  }
}

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

a/length(index) #yields 0.8644279 accuracy

train[1593272:1593278,]
train[1305774:1305780,]
train[1626538:1626542,]
train[552493:552499,]
train[37728:37733,]
train[5761:5767,]

#HOW ARE WE SUPPOSED TO IMPUTE THE NA?
#OPTION 1. MULTINOMIAL LOSGISTIC REGRESSION
#OPTION 2. KNN 
#OPTION 3.

library(mice)
imp_det <- mice(train, method = "norm.predict", m = 1) # Impute data
data_det <- complete(imp_det) # Store data
any(is.na(data_det))


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


write.table(pred, file = "pred_v7.csv",
            sep = ',',
            quote = FALSE,
            row.names = FALSE)
