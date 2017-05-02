install.packages("xgboost", lib="/Users/João/Downloads/Teste")
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

train$TARGET <- train.y


glm <- glm(train$TARGET  ~ ., data = train,family=binomial)
Pred_glm<-predict(glm,test, type="response")

train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0013,
                max_depth           = 6,
                subsample           = 0.65,
                colsample_bytree    = 0.55
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 800,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)


test$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test)

submission <- data.frame(ID=test.id, TARGET=pred[,2])
write.csv(submission, file = "submissionsmooth.csv", row.names = F)