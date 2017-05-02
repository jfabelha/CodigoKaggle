train <- read.csv("train.csv", header = TRUE)
test  <- read.csv("test(1).csv", header=TRUE)

summary(train)

amostra1<-sample(1:nrow(train),as.integer(
  0.3*nrow(train)))
amostra_teste<-train[amostra1,]
amostra_treino<-train[-amostra1,]
length(amostra_teste$SalePrice)
length(amostra_treino$SalePrice)

summary(amostra_treino)

library(rpart)
library(class)
library(party)
library(e1071)
library(RWeka)
library(devtools)
library(xgboost)
library(Matrix)
library(hydroGOF)

set.seed(1234)

arv1<-rpart(amostra_treino$SalePrice~., amostra_treino, method="anova")
plot(arv1)
text(arv1)
pred <- predict(arv1, amostra_teste[,-amostra_teste$SalePrice])
rmse(log(pred),log(amostra_teste$SalePrice))

arv1<-rpart(amostra_treino$SalePrice~., amostra_treino, method="anova")
plot(arv1)
text(arv1)
pred <- predict(arv1, amostra_teste[,-amostra_teste$SalePrice])
rmse(log(pred),log(amostra_teste$SalePrice))

features=names(train)

#convert character into integer
for(f in features){
        if(class(train_test[[f]])=="character"){
                levels=sort(unique(train_test[[f]]))
                train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
        }
}



#row.has.na <- apply(final, 1, function(x){any(is.na(x))})
#This returns logical vector with values denoting whether there is any NA in a row. You can use it to see how many rows you'll have to drop:

namesr<-c("Alley","PoolQC","Fence","MiscFeature")
namest<-names(amostra_treino)
test.names   <- setdiff(namest, namesr)
test2         <- amostra_treino[,test.names]



library(randomForest)
fit <- randomForest(test2$SalePrice[complete.cases(test2)==T]~., 
                    data=test2[complete.cases(test2)==T,], importance=TRUE, ntree=2000)
importance(fit, type=1, scale=TRUE)
varImpPlot(fit)
#predi<-predict(fit,amostra_teste)
#rmse(log(predi),log(amostra_teste$SalePrice))

imp <- importance(fit, type=1)

asa<-rownames(imp)[which(imp > 30)]

arv2<-rpart(amostra_treino$SalePrice~., amostra_treino[,asa], method="anova")
plot(arv2)
text(arv2)
pred2 <- predict(arv2, amostra_teste[,-amostra_teste$SalePrice])
rmse(log(pred2),log(amostra_teste$SalePrice))

library(nnet)
library(DMwR)
nums <- sapply(amostra_treino, is.numeric)
at<-amostra_treino[,nums]
att<-scale(at)
at2<-as.data.frame(att)
nn <- nnet(at2$SalePrice~., data=at2[,c("MSSubClass","LotArea")],MaxNWts=3000,hidden=c(10,4),size=10 , maxit = 1000,  linout = T)
Pred_nn<-predict(nn, amostra_teste)
pred_nnok<-unscale(Pred_nn,att)
rmse(log(pred_nnok),log(amostra_teste$SalePrice))

lm<-lm(amostra_treino$SalePrice~., amostra_treino[,asa])
pred_lm <- predict(lm, amostra_teste[,-amostra_teste$SalePrice])
rmse(log(pred_lm),log(amostra_teste$SalePrice))

predok<-predict(lm,test)

library(gbm)
traini<-train[,asa]
traini$SalePrice <- train$SalePrice
model <- gbm(SalePrice ~., data = traini, distribution = "laplace",
             shrinkage = 0.05,
             interaction.depth = 5,
             bag.fraction = 0.66,
             n.minobsinnode = 1,
             cv.folds = 100,
             keep.data = F,
             verbose = F,
             n.trees = 300)
predict <- predict(model, test, n.trees = 300)


# RMSE
RMSE3 <- RMSE(predict, test$SalePrice)
RMSE3 <- round(RMSE3, digits = 3)
plot3 <- predict-test$SalePrice

submission <- data.frame(ID=test$Id, SalePrice=round(predict,1))
cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)


train.y<-test2$SalePrice
train2 <- sparse.model.matrix(SalePrice ~.-1, data = test2)
dtrain <- xgb.DMatrix(data=train2, label=train.y)
watchlist <- list(train2=dtrain)


param <- list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.0234567,
                max_depth           = 5,
                subsample           = 0.85,
                colsample_bytree    = 0.95
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 312,
                    verbose             = 0,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

teste2<-test[,asa]
test2$TARGET <- -1

test3 <- sparse.model.matrix(TARGET ~ ., data = test2)

preds <- predict(clf, test3)