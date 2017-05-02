library(rpart)
library(class)
library(party)
library(e1071)
library(RWeka)
library(devtools)


library()
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

amostra1<-sample(1:nrow(treino),as.integer(
  0.3*nrow(treino)))
amostra_teste<-treino[amostra1,]
amostra_treino<-treino[-amostra1,]
length(amostra_teste$train.y)
length(amostra_treino$train.y)

str(amostra_treino, list.len=ncol(amostra_treino))

arv2<-rpart(amostra_treino$train.y~., amostra_treino, method="anova")
plot(arv2)
text(arv2)

library(class)
library(e1071)

nb<- naiveBayes(amostra_treino$train.y ~ ., data = amostra_treino,method="anova")
lm <- lm(amostra_treino$train.y  ~ ., data = amostra_treino)
#lmok <- na.omit(lm$coefficients)

Pred_arv<-predict(arv2, amostra_treino[,-258])
Pred_nb<-predict(nb, amostra_treino[,-258])
#Pred_lm<-predict(na.omit(lm), amostra_treino[,-258])

pred <- prediction(Pred_arv, amostra_treino$train.y)
perf <- performance(pred,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")

pred2 <- prediction(Pred_nb, amostra_treino$train.y)
perf2 <- performance(pred2,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf2,col="black",lty=3, lwd=3)
# calculating AUC
auc2 <- performance(pred2,"auc")
auc2 <- unlist(slot(auc2, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc2<-min(round(auc2, digits = 2))
maxauc2<-max(round(auc2, digits = 2))
minauct2 <- paste(c("min(AUC2)  = "),minauc2,sep="")
maxauct2 <- paste(c("max(AUC2) = "),maxauc2,sep="")
legend(0.3,0.6,c(minauct2,maxauct2,"\n"),border="white",cex=1.7,box.col = "white")


library(ROCR)
# data(ROCR.simple)
# pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
# perf <- performance(pred, "prec", "rec")
# plot(perf)


table(Pred_nb,amostra_treino$train.y)
diag(table(Pred_nb,amostra_teste$train.y))
tx.acerto_nb<-sum(diag(table(Pred_nb,amostra_treino$train.y)))/length(amostra_treino$train.y)
tx.acerto_nb

treino$V15<-factor(treino$V15, labels = c("0","1"))

trainpca <- prcomp(train, center = TRUE, scale = TRUE)
trainpca1<-predict(trainpca,amostra_treino)
ex<-as.data.frame(trainpca)

arv3<-rpart(amostra_treino$train.y~., ex[,1:100], method="anova")

nb2<- naiveBayes(amostra_treino$train.y ~ ., data = ex[,1:100],method="anova")
Pred_nb2<-predict(nb2, ex[,1:100])

pred4 <- prediction(Pred_nb2, amostra_treino$train.y)
perf4 <- performance(pred4,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf4,col="black",lty=3, lwd=3)
# calculating AUC
auc4 <- performance(pred4,"auc")
auc4 <- unlist(slot(auc4, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc4<-min(round(auc4, digits = 2))
maxauc4<-max(round(auc4, digits = 2))
minauct4 <- paste(c("min(AUC4)  = "),minauc4,sep="")
maxauct4 <- paste(c("max(AUC4) = "),maxauc4,sep="")
legend(0.3,0.6,c(minauct4,maxauct4,"\n"),border="white",cex=1.7,box.col = "white")



lm2 <- lm(amostra_treino$train.y  ~ ., data = ex[,1:100])
Pred_lm<-predict(lm2, ex[,1:100])

pred3 <- prediction(Pred_lm, amostra_treino$train.y)
perf3 <- performance(pred3,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf3,col="black",lty=3, lwd=3)
# calculating AUC
auc3 <- performance(pred3,"auc")
auc3 <- unlist(slot(auc3, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc3<-min(round(auc3, digits = 2))
maxauc3<-max(round(auc3, digits = 2))
minauct3 <- paste(c("min(AUC3)  = "),minauc3,sep="")
maxauct3 <- paste(c("max(AUC3) = "),maxauc3,sep="")
legend(0.3,0.6,c(minauct3,maxauct3,"\n"),border="white",cex=1.7,box.col = "white")

#redes neuronais
library(nnet)
nn <- nnet(amostra_treino$train.y ~ .,data=ex[,1:100], MaxNWts=2591,hidden=c(5,3),size=10 , maxit = 1000,  linout = T)
Pred_nn<-predict(nn, amostra_treino)

pred5 <- prediction(Pred_nn, amostra_treino$train.y)
perf5 <- performance(pred5,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf5,col="black",lty=3, lwd=3)
# calculating AUC
auc5 <- performance(pred5,"auc")
auc5 <- unlist(slot(auc5, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc5<-min(round(auc5, digits = 2))
maxauc5<-max(round(auc5, digits = 2))
minauct5 <- paste(c("min(AUC5)  = "),minauc5,sep="")
maxauct5 <- paste(c("max(AUC5) = "),maxauc5,sep="")
legend(0.3,0.6,c(minauct5,maxauct5,"\n"),border="white",cex=1.7,box.col = "white")


#svm


#random forest
library(randomForest)
fit <- randomForest(amostra_treino$train.y~., data=amostra_treino, importance=TRUE, ntree=2000)


#adaboost

#xmgboost


MyData<-summary(amostra_treino)
write.csv(MyData, file = "MyData.csv")
a<-str(amostra_treino)
write.csv(a, file = "a.csv")

head(treino)