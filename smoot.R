install.packages("DMwR")
library("DMwR")




train$TARGET <- as.factor(train$TARGET)
trainSplit <- SMOTE(TARGET  ~ ., train, perc.over = 100, perc.under=200)



train2 <- sparse.model.matrix(TARGET ~ ., data = trainSplit)

dtrain <- xgb.DMatrix(data=train2, label=trainSplit$TARGET)
watchlist <- list(train=dtrain)


ctrl <- trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     allowParallel=T)
tbmodel <- train(trainSplit$TARGET  ~ ., data = trainSplit, method = "rf",
                 trControl = ctrl)

tbmodel <- train(factor(trainSplit$TARGET, levels=c(0,1), labels=c('n', 'y')) ~ ., data=trainSplit
                , maximize            = FALSE ,objective           = "binary:logistic",eval_metric = "auc" , method = "xgbTree", nrounds=800, max_depth=6, eta=0.3,colsample_bytree=0.7, trControl = ctrl)
tbmodel <- train(factor(trainSplit$TARGET, levels=c(0,1), labels=c('n', 'y')) ~ ., data = trainSplit, method = "glm",
                 trControl = ctrl)



pred <- predict(tbmodel, test, type=c("prob"))

submission <- data.frame(ID=test.id, TARGET=pred[,2])
write.csv(submission, file = "submissionRFS.csv", row.names = F)