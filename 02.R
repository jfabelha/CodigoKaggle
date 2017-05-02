treino <- read.csv("amostratreino.csv",header = TRUE, sep = ,)
ex<-read.csv("ex.csv",header = F, sep = ,)

install.packages("ROSE")
library(ROSE)
library(rpart)

trainpca11<-predict(trainpca,train)
ex22<-as.data.frame(trainpca11)
exok<-cbind(ex22[,1:100],train.y)
exok2<-as.data.frame(exok)

set.seed(1234)
amostra1<-sample(1:sum(exok2$train.y),as.integer(
  sum(exok2$train.y)))
amostra_1<-as.data.frame(exok2[exok2$train.y==0,])
amostra_11<-amostra_1[amostra1,]

amostra_2<-as.data.frame(exok2[exok2$train.y==1,])

amostraf<-rbind(amostra_11,amostra_2)

arv<-rpart(train.y~., amostraf, method="class")
plot(arv)
text(arv)

arv2<-rpart(train.y~., amostraf, method="class", control = rpart.control(cp = 0.04))
Pred_arv<-predict(arv2, as.data.frame(trainpca11), type = c("prob"))


pred <- prediction(Pred_arv[,2], train.y)
perf <- performance(pred,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")

lm22 <- lm(train.y  ~ ., data = amostraf)
Pred_lm<-predict(lm22, as.data.frame(trainpca11))


pred <- prediction(Pred_lm, train.y)
perf <- performance(pred,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")

nb22<- naiveBayes(train.y ~ ., data = amostraf,method="class")
Pred_nb22<-predict(nb22, as.data.frame(trainpca11), type="raw")

pred <- prediction(Pred_nb22, train.y)
perf <- performance(pred,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")

nn2 <- nnet(as.factor(train.y) ~ .,data=amostraf, MaxNWts=2591,method="class",hidden=c(5,3),size=10 , maxit = 1000)
Pred_nn2<-predict(nn2,as.data.frame(trainpca11),type = c("prob"))

pred <- prediction(Pred_nn2, train.y)
perf <- performance(pred,"tpr","fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 4))
maxauc<-max(round(auc, digits = 4))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")

traintest<-predict(trainpca,test)
Pred_nnteste<-predict(nn2, as.data.frame(traintest))
Pred_nnteste*trainpca11
