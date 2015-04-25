# Predicting Assignment

# TEST DATA 
TEST <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


library(caret)
dat <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

# will only use columns with complete data, some of these are blank, others have NA, others have DIV/0!
# how awkward. 
# incomplete cols (visual inspection): 12-36, 50-59, 69-83, 87-101, 103-112, 125-139, 141-150

# subset out incomplete cols
dat <- dat[,-c(12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)]

# get into training/test sets
inTrain <- createDataPartition(y = dat$classe,
                               p = 0.6, list = FALSE)
traindat <- dat[inTrain,]
testdat <- dat[-inTrain,]


# initial exploration of training data
f1 <- featurePlot(x = traindat[,8:20], y = traindat$classe)
f2 <- featurePlot(x = traindat[,21:35], y = traindat$classe)
f3 <- featurePlot(x = traindat[,36:50], y = traindat$classe)
f4 <- featurePlot(x = traindat[,51:59], y = traindat$classe)

# not gleaning too much information from these plots. no real magnitude difference in between classes, but perhaps variability has something to do with it? 

# linear discriminant analysis as first guess
library(MASS)
a <- lda(classe ~., data = traindat)
# if no cross validation
p <- predict(a, newdata = testdat)
confusionMatrix(data = p$class, reference = testdat$classe)
# 100% accuracy. suspicious. 
# with x-validation
ctab <- table(a$class, traindat$classe)
sum(ctab[row(ctab) == col(ctab)]) / sum(ctab)
# 99.98 % accuracy with cross validation term


# trees
library(rattle)
mod <- train(traindat, traindat$classe, method = "rpart")
fancyRpartPlot(mod$finalModel)
tp <- predict(mod, newdata = testdat)
confusionMatrix(tp, testdat$classe)
# 66% accuracy...not so good.  


# ways to speed this stuff up:
train_control <- trainControl(method="boot", number=2, allowParallel=TRUE)
# decrease # of trees built ntree = 250


# bagging
bagfit <- train(traindat, traindat$classe, method = "bagEarth")


# random forests
library(party)
rfmod <- randomForest(traindat[,8:59], traindat$classe)
rfp <- predict(rfmod, newdata = testdat)
confusionMatrix(rfp, testdat$classe)
# 99.35% accurate.

TEST <- TEST[,-c(12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)]

tpd <- predict(rfmod, newdata = TEST)

answers <- as.character(tpd)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)


rfit <- train(model here, data=training, method="rf", trControl=train_control, ntree=500, other arguments here)

# boosting 
library(ada)

boostfit <- train(traindat, traindat$classe, method = "gbm", trControl = train_control)
boostp <- predict(boostfit, newdata = testdat)
confusionMatrix(boostp, testdat$classe)
# also 100% accuracy, but takes quite a while.

# not happenin
abfit <- train(traindat, traindat$classe, method = "ada")
adap <- predict(abfit, newdata = testdat)
confusionMatrix(adap, testdat$classe)



