set.seed(1234)

train <- read.csv("train.csv", header = TRUE)
test  <- read.csv("test.csv", header=TRUE)

amostra1<-sample(1:nrow(train),as.integer(
  0.001/3*nrow(train)))
newtrain<-train[amostra1,]

write.csv(newtrain, file = "newtrain.csv", row.names = F)

newtrain<-read.csv("newtrain.csv", header=TRUE)