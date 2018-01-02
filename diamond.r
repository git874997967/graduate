install.packages(c(rpart,maptree,tree,randomForest,ranger,gridExtra,grid,scales,memisc,raster,sqldf,dplyr,e1071))
library(rpart)
library(car)
library(ggplot2)
library(maptree)
library(tree)
library(randomForest)
library(ranger)
library(gridExtra)
library(grid)
library(scales)
library(memisc)
library (raster)
#library(sqldf)
library(dplyr)
library(httr)
library(rvest)
library(kknn)
# library(foreach)
# library(doParallel)
# library(parallel)
library(e1071)
#library(caret)
library(recommenderlab)
# mxnet gpu 
cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/GPU"
options(repos = cran)
install.packages("mxnet")
install.packages('devtools', repo = 'https://cloud.r-project.org/')
# datawash    delete the index and the measurement 
setwd('../Desktop/Graduate')
diamondsBig=read.csv("diamonds.csv",strip.white=TRUE)
diamond=na.omit(diamondsBig)[c(-1,-9)]
filter(diamond,diamond$table!='0')
filter(diamond,diamond$depth!='0')
# new function   renew the index
#distinct(diamond)
# diamond=trim(diamond)
# ??trim
# get the relationship between cart and price 
ggpairs(diamond, wrap = c(shape = I('.'), outlier.shape = I('.')))
ggplot(diamond,aes(x=carat,y=price))+
  geom_point(color='blue',fill='orange',alpha=1/2,size=1,position = 'jitter') + 
  # xlim(0, diamonds$carat)+
  # ylim(0, diamonds$price)+
  ggtitle('Diamond price vs. carat')

ggplot(diamond,aes(x=carat,y=price))+
  geom_point(color='blue',fill='orange',alpha=1/2,size=1,position = 'jitter')+
  #xlim(0,quantile(diamonds$carat,0.99))+
  #ylim(0,quantile(diamonds$price,0.99))+
  ggtitle('Diamond price vs. carat')


# change the price as log10(price make it more smooth)
 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ggplot(diamond,aes(x=price))+
  geom_histogram(color='blue',fill = 'orange',binwidth=100)+
  scale_x_continuous(breaks=seq(300,19000,1000),limit=c(300,19000))+
  ggtitle('Price')
  ggplot(diamonds,aes(x=price))+
  geom_histogram(color='blue',fill='orange',alpha=0.4,binwidth=0.01)+
  
  scale_x_log10(breaks=seq(300,19000,1000),limit=c(300,19000))+
  ggtitle('Price(log10)')
#grid.arrange(plot1,plot2,ncol=2)

### Create a new function to transform the carat variable
 caratBreak=c(0.2, 0.5, 1, 1.5,2,2.5,3,4,5,6,7,8,9,10)
 priceBreak=c(350, 1000, 5000, 10000, 15000,20000,40000,60000,80000,100000,120000)
 cuberoot_trans = function() trans_new( 'cuberoot',
                                       transform = function(x) x^(1/3),
                                       inverse = function(x) x^3)
 log10_trans = function() trans_new( 'cuberoot',
                                        transform = function(x) log10(x),
                                        inverse = function(x) 10^x)
 
### Use the cuberoot_trans function
ggplot(aes(carat^(1/3), log10(price)), data = diamond) + 
  geom_point(color='blue',fill='orange',alpha=1/2,size=1,position = 'jitter') + 
  scale_x_continuous( 
                    breaks = caratBreak) + 
  scale_y_continuous( 
                     breaks =  priceBreak) +
  ggtitle('Price (log10) by Cube-Root of Carat')

Dis=function(featureName){
  ggplot(aes(carat^(1/3), log10(price)), data = diamond) + 
    geom_point(alpha = 0.5, size = 1, position = 'jitter',aes(color=featureName)) +
      scale_color_brewer(type = 'div',
                         guide = guide_legend(title = featureName, reverse = T,
                                              override.aes = list(alpha = 0.8, size = 2))) +                         
    scale_x_continuous(#trans = cuberoot_trans(), 
                       breaks = caratBreak) + 
    scale_y_continuous(#trans = log10_trans(), 
                       breaks =  priceBreak) +
    ggtitle('Price (log10) by Cube-Root of Carat and',featureName)
}

ggplot(aes(carat^(1/3), log10(price)), data = diamond) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter',aes(color=cut)) +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 0.8, size = 8))) +                         
  scale_x_continuous(#trans = cuberoot_trans(), 
    breaks = caratBreak) + 
  scale_y_continuous(#trans = log10_trans(), 
    breaks =  priceBreak) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')
Dis('cut')
Dis('color')
Dis('clarity')
Dis('cert')
# divide the data into two part train dataset and test dataset
sample_data=sample(2,nrow(diamond),replace=TRUE,prob=c(0.85,0.15))

test=diamond[sample_data==2,]
train=diamond[sample_data==1,]

#plot(test$price,type="l",col=2)
#1  lm 

lm1=lm(price~., data=train)
summary(lm1)
steplm1=step(lm1)
summary(steplm1)
testPred1=predict(steplm1,test,level=0.95)

# Call:
#   lm(formula = I(log10(price)) ~ I(carat^(1/3)) + carat + cut + 
#        clarity + color + cert + x + y + z, data = train)
lm2 <- lm(I(log10(price)) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x*y*z , data = train) ## good
summary(lm2)
steplm2=step(lm2)
#   after step lm2 the variable x was deleted 
summary(steplm2)
mtable(lm2) 
 
testPred2=predict.lm(lm2,test,level=0.95)

# regression tree 

tm3=tree(price~.,data=train)
# 
  plot(tm3)
  text(tm3,all=TRUE,use.m=TRUE,fancy=TRUE,color=blue)
  summary(tm3)
testPred3=predict(tm3,test)

# tree -2
tm3=rpart(price~.,data=train)
rsq.rpart(tm3)
draw.tree(tm3)
tm3=prune(tm3,cp=0.01)
tm4=rpart(I(log10(price)) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z,data=train)
draw.tree(tm4)
rsq.rpart(tm4)
tm4<- prune(tm4,cp=0.01)
#draw.tree(tm4)
testPred4=predict(tm4,test)
testPred3=predict(tm3,test)
#   ranger forest
 m =ncol(train)
# 
# for (i in 1:(m-1)){
#   test_model <- randomForest(price~.,data=train,mtry=i,importance=TRUE,
#                              proximity=TRUE)
#   mse <- mean(test_model$mse)
#   print(mse)
# }
  
# cl=makeCluster(detectCores(logical = TRUE))
# registerDoParallel(cl)
# 
# test_model=foreach(ntree=rep(25,8),
#                    .combine=c,
#                    .packages='randomForest') %dopar%
#                    randomForest(price~.,data=train,ntree=ntree) 
# stopCluster(cl)
 

# with ranger
fm6 <- ranger(price ~ ., data = train, write.forest = TRUE,num.trees = 600)
testPred5 <- predict(fm6, data =test, interval = "prediction",level = .95)
#####svm  pretty slow
  cl=makeCluster(detectCores(logical = TRUE))
  registerDoParallel(cl)
    svm6=svm(price~.,data=train)
    stopCluster(cl)


####### with kknn
    kknnModel=kknn(price~.,train,test, k = 7, 
                   kernel =   "optimal" , distance = 2)

    with(test,{
      plot(price,type="l",main=" price VS predicted price",
           ylab="price",
           xlab="diamond index"
      )
      points(kknnModel$fitted.values,type="l",col=2)
      legend("topleft",c("price","price.predict"),col=1:2,lty=1)
    })

##draw the plot
withTest=function(modelName){
    if(modelName=='testPred6'){
    mainTitle=' price VS predicted price( forest model with ranger)'
    model=testPred6$predictions
    }
  else if(modelName=='testPred4'){
    mainTitle=" price VS predicted price( regression tree2)"
    model=10^testPred4
  }
  else if(modelName=='testPred3'){
    mainTitle=" price VS predicted price( regression tree1)"
    model=testPred3
  }
  else if(modelName=='testPred2'){
    mainTitle=" price VS predicted price( improved multi linear)"
    model=10^testPred2
  }
  else if(modelName=='testPred1'){
    mainTitle=" price VS predicted price( multi linear)"
    model=testPred1
  }
  else if(modelName=='kknnModel$fitted.values'){
    mainTitle='price VS predicted price( k-nearest neigbours)'
    model=kknnModel$fitted.values
  }
  with(test,{
    plot(price,type="l",main=mainTitle,
         ylab="price",
         xlab="diamond index"
    )
    points(model ,type="l",col=3)
    legend("topleft",c("price","price.predict"),col=1:6,lty=1)
  })
}

withTest('testPred6')
##### test part

cal_rms_error <- function(model,train_data,test_data,yval){
  
 # if(model=='lm2'){
   #  train_data.yhat <- predict(object=model,newdata=train_data)
   #  test_data.yhat <- predict(object=model,newdata=test_data)
   #  train_data.y <- with(train_data,get(yval))
   #  test_data.y <- with(test_data,get(yval))
   #  train_data.err <- sqrt(mean((10^train_data.yhat-train_data.y)^2))
   #  test_data.err <- sqrt(mean((10^test_data.yhat-test_data.y)^2))
   #  c(train_data.err=train_data.err,test_data.err)
   # }
  # 
  # if(model=='lm5'){
  train_data.yhat <- predict(object=model,newdata=train_data)
  test_data.yhat <- predict(object=model,newdata=test_data)
  train_data.y <- with(train_data,get(yval))
  test_data.y <- with(test_data,get(yval))
  train.err <- sqrt(mean((train.yhat$predictions-train.y)^2))
  test.err <- sqrt(mean((test.yhat$predictions-test.y)^2))
  c(train_data.err=train_data.err,test_data.err)
  # }else{
    # train_data.yhat <- predict(object=model,newdata=train_data)
    # test_data.yhat <- predict(object=model,newdata=test_data)
    # train_data.y <- with(train_data,get(yval))
    # test_data.y <- with(test_data,get(yval))
    # train_data.err <- sqrt(mean((train_data.yhat-train_data.y)^2))
    # test_data.err <- sqrt(mean((test_data.yhat-test_data.y)^2))
    # c(train_data.err=train_data.err,test_data.err)
  # }
}

test1=predict(steplm1,head(test),inteveral='prediction',level=0.95)
test2=predict(steplm2,head(test),level=0.95)
test3=predict(tm3,head(test),level=0.95)
test4=predict(tm4,head(test),level=0.95)
test5=predict(lm5,head(test),level=0.95)
test1
10^test2
test3
test4
test5$predictions
head(test$price)
 

cal_rms_error(lm1,train,test,'price')
cal_rms_error(lm2,train,test,'price')
cal_rms_error(tm3,train,test,'price')
cal_rms_error(tm4,train,test,'price')
cal_rms_error(lm5,train,test,'price')
###############################
maefun <- function(pred, obs) mean(abs(pred - obs))  
msefun <- function(pred, obs) mean((pred - obs)^2)  
nmsefun <- function(pred, obs) mean((pred - obs)^2)/mean((mean(obs) - obs)^2)  
# test
#maefun(testPred1,test$price)
maefun(10^testPred2,test$price)
#maefun(testPred3,test$price)
#maefun(testPred4,test$price)
maefun(testPred5$predictions,test$price)
#msefun(testPred1,test$price)
msefun(10^testPred2,test$price)
#msefun(testPred3,test$price)
#msefun(testPred4,test$price)
msefun(testPred5$predictions,test$price)
#nmsefun(testPred1,test$price)
nmsefun(10^testPred2,test$price)
#nmsefun(testPred3,test$price)
nmsefun(testPred5$predictions,test$price)
kknnModel=kknn(price~.,train,test, k = 7, 
               kernel =   "optimal" , distance = 2)
nmsefun(testPred5$predictions,test$price)
maefun(kknnModel$fitted.values,test$price)
msefun(kknnModel$fitted.values,test$price)
nmsefun(kknnModel$fitted.values,test$price)
 
