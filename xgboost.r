library(regclass)
library(caret)
library(glmnet)
library(pROC)
library(dplyr)
library(Matrix)
library(magrittr)
library(xgboost)
data(LAUNCH)
View(LAUNCH)
launch=LAUNCH
launch$success=factor(ifelse(launch$Profit>4.5,"yes",'no'))
launch$Profit=NULL
launch$x12=log10(launch$x12)
to.delete=which(lapply(launch,function(x)length(unique(x)))==1)
launch=launch[,-to.delete]
set.seed(1)
rf=randomForest(success~.,data=launch,ntree=800)
barplot(summarize_tree(rf)$importance)
abline(v=100)
# these two lines will keep the success column and the top 100 predictors according to the random forest
good.columns=c('success',names(summarize_tree(rf)$importance[1:100]))
launch=launch[,good.columns]

y=launch$success
launch$success=NULL
#convert categorical variables into indicator variables with the folling 
# command 
#Note: ther are no categorical variables here so this command leaves the data unchanged

launch=model.matrix(~.-1,data=launch)
head(launch)
# refine launch as scaled version of itself using scale 
# the class of launch will now be a matrix instead of a data frame
launch=scale(launch)
##############
data=read.csv(file.choose(),header=T)
data$rank=as.factor(data$rank)
inc=sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train=data[inc==1,]
test=data[inc==2,]
####create the matrix and label
View(train)
trainm=sparse.model.matrix(admit~.-1,data=train)
testm=sparse.model.matrix(admit~.-1,data=test)
trainl=train[,'admit']
testl=test[,'admit']
train_matrix=xgb.DMatrix(data=as.matrix(trainm),label=trainl)
test_matrix=xgb.DMatrix(data=as.matrix(testm),label=testl)
train_matrix
###parameters
nc=length(unique(trainl))
xgb_params=list('objective'='multi:softprob',
                'eval_metric'='mlogloss',
                  'num_class'=nc)
watch_list=list(train=train_matrix,test=test_matrix)
#### boosting model
bst_model=xgb.train(params=xgb_params,data=train_matrix,nrounds=10,watchlist=watch_list,nthread=8)
###train and test error plot 
e=data.frame(bst_model$evaluation_log)
plot(e$vectors,e$train_mlogloss)
 