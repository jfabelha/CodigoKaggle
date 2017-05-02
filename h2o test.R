library(h2o)
library(h2oEnsemble)
h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # Clean slate - just in case the cluster was already running
train <- h2o.importFile(path = normalizePath("train2.csv"))
test <- h2o.importFile(path = normalizePath("test2.csv"))
y <- "TARGET"
x <- setdiff(names(train), y)
train[,y] <- as.factor(train[,y])  
test[,y] <- as.factor(test[,y])




learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"

fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

pred <- predict(fit, test)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
submission <- data.frame(ID=teste$ID, TARGET=predictions)
write.csv(submission, file = "submissionth2o.csv", row.names = F)

predictions2 <- as.data.frame(pred$basepred[4]) 
submission <- data.frame(ID=teste$ID, TARGET=predictions2)
write.csv(submission, file = "submissionnn2.csv", row.names = F)

predictions3 <- as.data.frame(te) 
submission <- data.frame(ID=teste$ID, TARGET=predictions3)
write.csv(submission, file = "submissionmean.csv", row.names = F)