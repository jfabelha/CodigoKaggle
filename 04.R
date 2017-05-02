install.packages("caret", lib="/Users/João/Downloads/Teste")
library(caret, lib="/Users/João/Downloads/Teste")


install.packages("ggplot2", lib="/Users/João/Downloads/Teste")
library(ggplot2, lib="/Users/João/Downloads/Teste")

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


amostra1<-sample(1:sum(train$TARGET),as.integer(
  sum(train$TARGET)))
amostra_1<-as.data.frame(train[train$TARGET==0,])
amostra_11<-amostra_1[amostra1,]

amostra_2<-as.data.frame(train[train$TARGET==1,])


amostraf<-rbind(amostra_11,amostra_2)

install.packages("ROCR", lib="/Users/João/Downloads/Teste")
library(ROCR, lib="/Users/João/Downloads/Teste")
library(nnet)
install.packages("e1071", lib="/Users/João/Downloads/Teste")
library(e1071, lib="/Users/João/Downloads/Teste")
install.packages("randomForest", lib="/Users/João/Downloads/Teste")
library(randomForest, , lib="/Users/João/Downloads/Teste")
nn <- train(as.factor(amostraf$TARGET) ~ .,amostraf[,-308],linout = T)
Pred_nn<-predict(nn, test, type="prob")

submission <- data.frame(ID=test.id, TARGET=Pred_nn)
write.csv(submission, file = "submissionRF.csv", row.names = F)