library(GGally)
library(rpart)
library(doParallel)
library(parallel)
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
 
  ggpairs(diamond[c(1,2,3),])
  ggpairs(diamond, title=' basic info about the diamond dataset',
          lower = list(continuous = wrap("points", shape = I('.'))), 
          upper = list(combo = wrap("box", outlier.shape = I('.'))))
  
pairs(diamond)
## parallel is not useful with the ggpairs
system.time({
  #cl=makeCluster(detectCores())
#registerDoParallel(cl)
#ggpairs(diamonds,columns = 1:9,title='basic info about diamond dataset')
 pairs(diamonds)
})
stopCluster(cl)
pairPlot
sampleData=sample(2,nrow(diamond),replace=TRUE,prob=c(0.85,0.15))
test=diamond[sampleData==2,]
train=diamond[sampleData==1,]
lm1=lm(I(log10(price))~ I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z,data=train)
lm2 <- lm(I(log10(price)) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+I((x*y*z)^(1/3)), data = train)
### I((x*y*z)^(1/3)) is uncessary  lm3 is the final lm model with R square 0.98
lm3 <- lm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train)
summary(lm3)
summary(step(lm3))  
glm1=glm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train,family=gaussian(link = "identity"))
glm2=glm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train,family=quasipoisson(link = "log"))
glm3=glm(log10(price) ~I(carat^(1/3))+carat+cut+clarity+color+cert+table+depth+x+y+z+x*y*z, data = train,family=quasi(link = "identity", variance = "constant"))
         #### regresssion tree)
summary

tm4=rpart(price~., data=train,method="anova",control='cp=0.001',xval=10)
printcp(tm4) # display the results 
plotcp(tm4) # visualize cross-validation results 
summary(tm4) # detailed summary of splits

# plot tree 
plot(tm4, uniform=TRUE, 
     main="Regression tree for price")
text(tm4, use.n=TRUE, all=TRUE, cex=.8)
######prune is to provent overfitting
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
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
#### prune tree
# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#randomForest   no enough memory
rf5=randomForest(price~.,data=train)
  
