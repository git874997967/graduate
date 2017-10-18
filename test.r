library(e1071)
library(ggplot2)
library(GGally)
library(scales)
library(doParallel)
library(lattice)  
library(class)
library(kknn)
library( nnet)
library(neuralnet)
library(RSNNS)
sample_data=sample(2,nrow(diamonds),replace=TRUE,prob=c(0.85,0.15))
test=diamonds[sample_data==2,]
train=diamonds[sample_data==1,]
#### basic analysis
diasamp = diamonds[sample(1:length(diamonds$price), 10000),]
ggpairs(diamonds, wrap = c(shape = I('.'), outlier.shape = I('.')))
cubroot_trans = function() trans_new('cubroot', transform= function(x) x^(1/3), inverse = function(x) x^3 )

p = ggplot( data=diamonds, aes(carat, price)) +
  geom_point(alpha = 0.5, size = .75, position='jitter') +
  scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  theme_bw() +
  ggtitle('Price (log10) by Cubed-Root of Carat')
p

  ## 进行标准化
 train1=train[c(-2,-3,-4)]
 test1=test[c(-2,-3,-4)]
train1=scale(train1)
test1=scale(test1)
Knn=knn(train,test,k=13,cl)
 
################# knn 版本失败  转用 第三方  kknn
a=iris[-5]
a=scale(a)
iris_train<-a[c(1:25,50:75,100:125),]
iris_test<-a[c(26:49,76:99,126:150),]#测试集
iris_train_lab<-iris[c(1:25,50:75,100:125),5]
iris_test_lab<-iris[c(26:49,76:99,126:150),5]
pre_result=knn(train,test,cl=iris_train_lab,k=13)
table(pre_result,iris_test_lab)
##################kknn   有继续调优的余地  
kknnModel=kknn(price~.,train,test,kernel='gaussian')
plot(kknnModel$fitted.values)

with(test,{
  plot(price,type="l",main=" price VS predicted price",
       ylab="price",
       xlab="diamond index"
  )
  points(kknnModel$fitted.values,type="l",col=2)
  legend("topleft",c("price","price.predict"),col=1:2,lty=1)
})
#######神经网络  nnet 参数众多 
nnetModel<-nnet(price~.,train,size=9,decay=0.015,maxit=100,linout=T,trace=F,MaxNWts=8000)
nnetPredict=predict(nnetModel,test)
with(test,{
  plot(price,type="l",main=" price VS predicted price",
       ylab="price",
       xlab="diamond index"
  )
  points(nnetPredict,type="l",col=2)
  legend("topleft",c("price","price.predict"),col=1:2,lty=1)
})

############# 神经网络  neuralnet 
#####网上说  输入一定要全部都是 数字类型   有待考证
neural_model=neuralnet(price~carat+cut+clarity+color+table+depth+x+y+z,data=train,hidden=9)
neural_predict=predict(neural_model,test)
  
######################## test the microbenchmark and compiler 
library(microbenchmark)
library(compiler)

f1<-function(){
     x=1:100
    for(i in 1:100){
          x[i]=x[i]+1
       }
 }
 f2<-function(){
     x=1:100
     x+1
    }
  f3<-cmpfun(f1)
  f4<-cmpfun(f2)

  microbenchmark(f1(),f2(),f3(),f4())
######openblas
  data <- rnorm(10000*1000) ##??????n�m??????
  dim(data) <- c(10000, 1000) ##?????????n�m?????????
  system.time(dataSum1 <- apply(data, 1, sum)) ##??????????????????
   
  system.time(dataSum2 <- rowSums(data)) ##????????????
  x<-matrix(1:(6000*6000),6000,6000)  
  system.time(tmp<-x%*%x) 







