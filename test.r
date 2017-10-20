library(e1071)
library(ggplot2)
library(GGally)
library(scales)
library(doParallel)
library(lattice)  
library(class)
library(kknn)
library(arules)
library( nnet)
library(neuralnet)
library(RSNNS)
library(sqldf)
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

## è¿è¡æ åå
train1=train[c(-2,-3,-4)]
test1=test[c(-2,-3,-4)]
train1=scale(train1)
test1=scale(test1)
Knn=knn(train,test,k=13,cl)

################# knn çæ¬å¤±è´¥  è½¬ç¨ ç¬¬ä¸æ¹  kknn
a=iris[-5]
a=scale(a)
iris_train<-a[c(1:25,50:75,100:125),]
iris_test<-a[c(26:49,76:99,126:150),]#æµè¯é
iris_train_lab<-iris[c(1:25,50:75,100:125),5]
iris_test_lab<-iris[c(26:49,76:99,126:150),5]
pre_result=knn(train,test,cl=iris_train_lab,k=13)
table(pre_result,iris_test_lab)
##################kknn   æç»§ç»�è°ä¼çä½å°  
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
#######ç¥ç»ç½ç»  nnet åæ°ä¼å¤ 
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

############# ç¥ç»ç½ç»  neuralnet 
#####ç½ä¸è¯´  è¾å¥ä¸å®è¦å¨é¨é½æ¯ æ°å�ç±»å   æå¾èè¯
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
data <- rnorm(10000*1000) ##生成n×m个数
dim(data) <- c(10000, 1000) ##转换成n×m的矩阵
system.time(dataSum1 <- apply(data, 1, sum)) ##自己编写函数

system.time(dataSum2 <- rowSums(data)) ##内置函数
x<-matrix(1:(6000*6000),6000,6000)  
system.time(tmp<-x%*%x) 
########  ggplot2 exercise
diamonds
qplot(carat,price,data=diamonds,geom=c('point','smooth'))
qplot(x*y*z,price,data=diamonds,geom=c('point','smooth'))
qplot(carat,log(price),data=diamonds,geom=c('point','smooth'))
qplot(log(x*y*z),log(price),data=diamonds,geom=c('point','smooth'))
lm1=lm(log(price)~.,data=diamonds)
summary(lm1)
lm2=lm(I(log(price))~ carat+color+clarity+cut+depth+table+(x*y*z)+x+y+z,data=diamonds)
lm3=lm(log10(price)~I(carat^(1/3))+carat+color+clarity+cut+depth+table+(x*y*z)+x+y+z,data=diamonds)
summary(lm3)
####random pick the sample
set.seed(100)
dataSmall=diamonds[sample(nrow(diamonds),10000),]

qplot(data=dataSmall,carat,price,color=color)
diamonds$xyz=log(diamonds$x*diamonds$y*diamonds$z)
diamonds=diamonds[diamonds$xyz<6.5,]

# facents can describe three class variables
qplot(color,data=diamonds,facets=.~clarity,geom='bar',fill=cut,main='this title',xlab='this is lab for x axis',ylab='this is lab for y axis')
##the mapping from data to decoration
p=qplot(displ,hwy,data=mpg,color=factor(cyl),main="abc",xlab='engine size',ylab='hwy mpg',facets=.~year,geom=c('smooth','point'))
summary(p)  
### 首先创造 ggplot 对象  接着在对象画板上进行渲染
q=ggplot(diamonds,aes(x=carat))+
  geom_histogram(binwidth = 2,fill='steelblue')+
  stat_bin()
 q
summary(q)
qplot(sleep_rem/ sleep_total,awake,data=msleep)+geom_point()+geom_smooth()
msleep
trans_mtcars=transform(mtcars,mpg=mpg^2)
p1=ggplot(mtcars,aes(mpg,wt,color=cyl),engine='jitter',facet_grid=.~mtcars$gear)+geom_smooth()
p1%+%trans_mtcars
p1
 ggplot(diamonds,aes(carat,price) )+geom_smooth()+geom_point(aes(carat,price), colour="red")+geom_jitter(width = 0.1, height = 0.1)
 ggplot(diamonds,aes(clarity,fill=color))+geom_bar(position='dodge')+facet_grid(facets=.~cut)
 ggplot(diamonds,aes(carat))+stat_bin(aes(ymax=..count..),binwidth = 0.1,geom='area')
 
 