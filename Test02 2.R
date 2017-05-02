library(rpart)
library(class)
library(party)
library(e1071)
library(RWeka)
library(devtools)
library(xgboost)
library(Matrix)

set.seed(1234)

train <- read.csv("train.csv", header = TRUE)
test  <- read.csv("test.csv", header=TRUE)

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


feature.names <- names(train)
toRemove <- c()
for (fn in feature.names) {
  formula0 <- paste0(fn," ~.")
  fit <- lm(formula0, train)
  coe <-  ifelse(is.na(fit$coefficients[]),0,fit$coefficients[])
  coeP1 <- ifelse(abs(round(coe[])-coe[]) < 0.001 & coe[]>0, round(coe[]),0)
  coeM1 <- ifelse(abs(round(coe[])-coe[]) < 0.001 & coe[]<0, round(coe[]),0)
  coeP10 <- ifelse(coe[]>0, round(coe[]),0)
  coeM10 <- ifelse(coe[]<0, round(coe[]),0)
  sumP1 <- sum(coeP1)
  sumM1 <- sum(coeM1)
  if (sumP1>1 & sumM1==0 & coeP1==coeP10 & coeM1==coeM10) {
    toRemove <- c(toRemove,fn)
    cat(fn,"is a function of other features (sum of coeff =",sumP1,")\n")
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")
### 3 #################################################
### Removing highly correlated features
ncol0<-ncol(train)
cat("removing highly correlated features\n")
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if ((cor(train[[f1]] , train[[f2]])) > 0.999) {
      cat(f1, "and", f2, "are highly correlated \n")
      toRemove <- c(toRemove, f2)
    }
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")
####################################################
removed <- ncol0-ncol(train)
cat("\n ",removed," features have been removed\n")

treino<-cbind(train,train.y)


library(randomForest)
fit <- randomForest(as.factor(treino$train.y)~., data=treino, importance=TRUE, ntree=2000)
importance(fit, type=1, scale=TRUE)
varImpPlot(fit)

imp <- importance(fit, type=1)

rownames(imp)[which(imp > 10)]

newtrain<-train[,rownames(imp)[which(imp > 10)]]

newtrain$TARGET <- train.y

#adaboost

#xmgboost

train <- sparse.model.matrix(TARGET ~ ., data = newtrain)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.01,
                max_depth           = 7,
                subsample           = 0.8,
                colsample_bytree    = 0.7
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)



newtest<-test[,rownames(imp)[which(imp > 3)]]
newtest$TARGET <- -1

test2 <- sparse.model.matrix(TARGET ~ ., data = newtest)

preds <- predict(clf, test2)
submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "submissionRFXGB.csv", row.names = F)
