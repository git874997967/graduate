library(GGally)
library(rpart)
library(doParallel)
library(randomForest)
library(parallel)
library(mlbench)
library(kknn)
library(MASS)
###the file is test for some new models and skills about the graduate project.
setwd('../Desktop/graduate')
diamondsBig=read.csv('diamonds.csv')
diamond=na.omit(diamondsBig[c(-1,-9)])
# filter(diamond,diamond$table!=0)
# dia=filter(diamond,diamond$depth!=0)
# filter(diamond,diamond$y!='0')
# 
# filter(diamond,diamond$x!='0')
# filter(diamond,diamond$z!='0')
#####whether unique or not ???? 
 # cl=detectCores()
 #   registerDoParallel(cl) 
    
  ggpairs(diamond, title=' basic info about the diamond dataset',
          lower = list(continuous = wrap("points", shape = I('.'))), 
          upper = list(combo = wrap("box", outlier.shape = I('.'))))
   
 
  # stopCluster(cl)
 
## parallel is not useful with the ggpairs
 
 
 #  cl <- makeCluster(8, type = "SOCK")
 #  registerDoSNOW(cl)
 # #ggpairs(diamond,columns = 1:9,title='basic info about diamond dataset')
 #  rf5=randomForest(price~.,data=train,ntree=30)
 #pairs(diamonds)
 
# stopCluster(cl)
pairPlot
sampleData=sample(2,nrow(diamond),replace=TRUE,prob=c(0.85,0.15))
test=diamond[sampleData==2,]
train=diamond[sampleData==1,]
lm1=lm(I(log10(price))~ I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z,data=train)
lm2 <- lm(I(log10(price)) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+I((x*y*z)^(1/3)), data = train)
### I((x*y*z)^(1/3)) is uncessary  lm3 is the final lm model with R square 0.98
lm3 <- lm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train)
lm4=rlm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train)
summary(lm4)
       lm4Pre=predict(lm4,test)
       lm3Pre=predict(lm3,test)
       
       with(test,{
         plot(price,type="l",main=" price in test dataset",
              ylab="price",
              xlab="diamond index"
         )
         # points(10^lm4Pre,type="l",col=3)
         # legend("topleft",c("price","price.predict"),col=1:3,lty=1)
       })
       
       
  #### regresssion tree)
####benefits
# 决策树的够造不需要任何领域知识，就是简单的IF...THEN...思想 ；
# 
# 2）决策树能够很好的处理高维数据，并且能够筛选出重要的变量；
# 
# 3）由决策树产生的结果是易于理解和掌握的；
# 
# 4）决策树在运算过程中也是非常迅速的；
# 
# 5）一般而言，决策树还具有比较理想的预测准确率。
tm4=rpart(price~., data=train,method="anova",control='cp=0.001',xval=10)
printcp(tm4) # display the results 
plotcp(tm4) # visualize cross-validation results 
summary(tm4) # detailed summary of splits

# plot tree 
plot(tm4, uniform=TRUE, 
     main="Regression tree for price")
text(tm4, use.n=TRUE, all=TRUE, cex=.8)
######prune is to provent overfitting
#cp: complexity parameter, 复杂性参数，
#用来设定修剪用的参数，一般遵循1-SE规则。
#http://f.dataguru.cn/thread-315284-1-1.html
### 使用1-SE法则选出最优cp值:找到xerror最小的行，得到误差阈值为该行的xerror+xstd
## 找到所有xerror小于这个阈值的行，取其中最大值的上限为prune的阈值
# in this model we use post -pruning
# CP: complexity parameter
# nsplit: the number of splits
# rel error: the relative error rate for
# predictions
# for the data that generated the tree
# xerror: cross-validated error.x in xerror is
# an abbreviation for cross-validated
# xstd: the standard derivation of crossvalidated
ptm4=prune(tm4,cp=0.011574)
plot(ptm4, uniform=TRUE, 
     main="Pruned Regression Tree for price1")
plot(ptm4, uniform=T, branch=0.6, compress=T)
text(ptm4, use.n=TRUE, all=TRUE, cex=.8)




##### regression tree example
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(pfit) # visualize cross-validation results    

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
#### prune tree
# prune the tree 
pfit<- prune(fit, cp=0.010000) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


# kknn   knn one of the easist algo  in the ML
# one example of kknn
# https://www.rdocumentation.org/packages/kknn/versions/1.3.1/topics/kknn
data(ionosphere)
ionosphere.learn <- diamonds[1:200,1:10]
ionosphere.valid <- diamonds[-c(1:200),1:10]
 
fit.kknn <- kknn(price ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(price ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$price)
(fit.train2 <- train.kknn(price ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$price)
  


