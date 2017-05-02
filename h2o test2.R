set.seed(1234)

train <- read.csv("train.csv", header = TRUE)
test2  <- read.csv("test.csv", header=TRUE)

##### Removing IDs
train$ID <- NULL
test.id <- test2$ID
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

write.csv(test, file = "test2.csv", row.names = F)
write.csv(treino, file = "train2.csv", row.names = F)

library(h2o)
library(h2oEnsemble)
h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # Clean slate - just in case the cluster was already running
train <- h2o.importFile(path = normalizePath("train2.csv"))
test <- h2o.importFile(path = normalizePath("test2.csv"))
y <- "train.y"
x <- setdiff(names(train), y)
train[,y] <- as.factor(train[,y])  

h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 200, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)

learner <- c("h2o.glm.wrapper", "h2o.randomForest.3", "h2o.deeplearning.5")
metalearner <- "h2o.gbm.4"

fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

pred <- predict(fit, test)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
submission <- data.frame(ID=test2$ID, TARGET=predictions)
write.csv(submission, file = "submissionth2o2.csv", row.names = F)

predictions2 <- as.data.frame(pred$basepred[3]) 
submission2 <- data.frame(ID=test2$ID, TARGET=predictions2)
write.csv(submission2, file = "submissiondl2.csv", row.names = F)

te<-(pred$basepred[1]+pred$basepred[3])/2
predictions3 <- as.data.frame(te) 
submission3 <- data.frame(ID=test2$ID, TARGET=predictions3)
write.csv(submission3, file = "submissionmean2.csv", row.names = F)