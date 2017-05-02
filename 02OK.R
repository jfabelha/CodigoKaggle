library(xgboost, lib="/Users/João/Downloads/Teste")
library(Matrix)

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

amostra1<-sample(1:nrow(train),as.integer(
  0.3*nrow(train)))
amostra_teste<-train[amostra1,]
amostra_treino<-train[-amostra1,]
length(amostra_teste)
length(amostra_treino)

install.packages('randomForest', lib="/Users/João/Downloads/Teste")
library(randomForest, lib="/Users/João/Downloads/Teste")

fit <- randomForest(as.factor(train$TARGET)~., data=train[,-308], importance=TRUE, ntree=20)
importance(fit, type=1, scale=TRUE)
varImpPlot(fit)

imp <- importance(fit, type=1)

rownames(imp)[which(imp > 3)]

newtrain<-train[,rownames(imp)[which(imp > 3)]]

newtrain$TARGET <- train.y

train <- sparse.model.matrix(TARGET ~ ., data = newtrain)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.7,
                colsample_bytree    = 0.7
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 560, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)



newtest<-test[,rownames(imp)[which(imp > 3)]]
newtest$TARGET <- -1

test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test)
submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)