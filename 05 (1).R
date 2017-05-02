install.packages("caret", lib="/Users/Jo?o/Downloads/Teste")
library(caret, lib="/Users/Jo?o/Downloads/Teste")


install.packages("ggplot2", lib="/Users/Jo?o/Downloads/Teste")
library(ggplot2, lib="/Users/Jo?o/Downloads/Teste")

set.seed(1234)

train <- read.csv("train.csv", header=T)
test  <- read.csv("test.csv", header=T)

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}


##### Removing categorical features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) < 25) {
    cat(f, "is categorical in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]

train$TARGET <- train.y


amostra1<-sample(1:sum(train$TARGET),as.integer(
  sum(train$TARGET)))
amostra_1<-as.data.frame(train[train$TARGET==0,])
amostra_11<-amostra_1[amostra1,]

amostra_2<-as.data.frame(train[train$TARGET==1,])


amostraf<-rbind(amostra_11,amostra_2)


library(ROCR)
library(nnet)
library(caret)
library(e1071)
library(randomForest)


arv2<-rpart(as.factor(amostraf$TARGET)~., amostraf, method="class")
Pred_arv<-predict(arv2, test, type="prob")

nn <- train(as.factor(amostraf$TARGET) ~ .,method=nnet,data=amostraf[,-201],linout = T)
Pred_nn<-predict(nn, test, type="prob")

glm <- glm(as.factor(amostraf$TARGET)  ~ ., data = amostraf,family=binomial)
Pred_glm<-predict(glm,test, type="response")



train <- sparse.model.matrix(amostraf$TARGET ~ ., data = amostraf)

dtrain <- xgb.DMatrix(data=train, label=amostraf$TARGET)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.01,
                max_depth           = 7,
                subsample           = 1,
                colsample_bytree    = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 800,
                    verbose             = 0,
                    watchlist           = watchlist,
                    maximize            = FALSE
)


test$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test)

library(randomForest)
fit <- randomForest(amostraf$TARGET ~., data=amostraf, importance=TRUE, ntree=2000)



submission <- data.frame(ID=test.id, TARGET=preds)
write.csv(submission, file = "submissionXGB2.csv", row.names = F)