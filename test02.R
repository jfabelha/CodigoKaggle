set.seed(1234)

traino <- read.csv("train.csv", header = TRUE)
test  <- read.csv("test.csv", header=TRUE)
train <- traino[traino$is_booking==1,]
test$date_time<-as.factor(format(strptime(test$date_time, "%Y-%m-%d %H:%M:%S"),"%m"))
test$srch_ci<-as.factor(format(as.Date(test$srch_ci, "%Y-%m-%d"),"%m" ))
test$srch_co<-as.factor(format(as.Date(test$srch_co, "%Y-%m-%d"),"%m"))

amostra1<-sample(1:nrow(train),as.integer(
  0.01/3*nrow(train)))
newtrain<-train[amostra1,]

amostra2<-sample(1:nrow(train),as.integer(
  0.01/3*nrow(train)))
newtest1<-train[- amostra1,]
newtest<-newtest1[amostra2,]

newtest$date_time<-as.factor(format(strptime(newtest$date_time, "%Y-%m-%d %H:%M:%S"),"%m"))
newtest$srch_ci<-as.factor(format(as.Date(newtest$srch_ci, "%Y-%m-%d"),"%m" ))
newtest$srch_co<-as.factor(format(as.Date(newtest$srch_co, "%Y-%m-%d"),"%m"))

write.csv(newtrain,file="newtrain.csv", row.names = F)
write.csv(newtest,file="newtest.csv", row.names = F)

rm(train)
rm(newtest1)
rm(traino)

library(rpart)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

newtrain<-read.csv("newtrain.csv", header=TRUE)
newtest<-read.csv("newtest.csv", header=TRUE)
newtrain$date_time<-as.factor(format(strptime(newtrain$date_time, "%Y-%m-%d %H:%M:%S"),"%m"))
newtrain$srch_ci<-as.factor(format(as.Date(newtrain$srch_ci, "%Y-%m-%d"),"%m" ))
newtrain$srch_co<-as.factor(format(as.Date(newtrain$srch_co, "%Y-%m-%d"),"%m"))

str(newtrain)
summary(newtrain)

newtrain$station<-NULL
newtrain$station[newtrain$srch_ci=="12"]<-"Wint"
newtrain$station[newtrain$srch_ci=="01"]<-"Wint"
newtrain$station[newtrain$srch_ci=="02"]<-"Wint"
newtrain$station[newtrain$srch_ci=="03"]<-"Spri"
newtrain$station[newtrain$srch_ci=="04"]<-"Spri"
newtrain$station[newtrain$srch_ci=="05"]<-"Spri"
newtrain$station[newtrain$srch_ci=="06"]<-"Summ"
newtrain$station[newtrain$srch_ci=="07"]<-"Summ"
newtrain$station[newtrain$srch_ci=="08"]<-"Summ"
newtrain$station[newtrain$srch_ci=="09"]<-"Autu"
newtrain$station[newtrain$srch_ci=="10"]<-"Autu"
newtrain$station[newtrain$srch_ci=="11"]<-"Autu"

newtest$station<-NULL
newtest$station[newtest$srch_ci=="12"]<-"Wint"
newtest$station[newtest$srch_ci=="1"]<-"Wint"
newtest$station[newtest$srch_ci=="2"]<-"Wint"
newtest$station[newtest$srch_ci=="3"]<-"Spri"
newtest$station[newtest$srch_ci=="4"]<-"Spri"
newtest$station[newtest$srch_ci=="5"]<-"Spri"
newtest$station[newtest$srch_ci=="6"]<-"Summ"
newtest$station[newtest$srch_ci=="7"]<-"Summ"
newtest$station[newtest$srch_ci=="8"]<-"Summ"
newtest$station[newtest$srch_ci=="9"]<-"Autu"
newtest$station[newtest$srch_ci=="10"]<-"Autu"
newtest$station[newtest$srch_ci=="11"]<-"Autu"

jt<-transform(newtrain, newvar=paste(newtrain$station, newtrain$user_location_country, newtrain$hotel_country))
jtt<-transform(newtest, newvar=paste(newtest$station, newtest$user_location_country, newtest$hotel_country))


#tentar criar frequências 
#as<-table(jt$newvar,jt$hotel_cluster)
#top<-as.data.frame(as)
#t<-c()
#for (i in 1:length(as)) {
  #t[i]<-tail(as.data.frame(order(top[i,], decreasing=TRUE),5))
  #}

#library(plyr)
#x <- daply(jt, .(jt$newvar), function(x)return(x))

ztx<-unique(as.vector(jt$newvar))
a<-list()
j<-0
for (j in 1:length(unique(ztx))) {
    a[[j]]<-as.data.frame(jt[jt$newvar==ztx[j],])
}

ztx2<-unique(as.vector(jtt$newvar))
at<-list()
j<-0
for (j in 1:length(unique(ztx2))) {
  at[[j]]<-as.data.frame(jtt[jtt$newvar==ztx2[j],])
}

#i<-0
#for(i in 1:length(a)) {
#  assign(paste0("a.", i), as.data.frame(a[[i]]))
#}

#library(rpart)
#b<-list()
#for(i in 1:length(a)){
#  as<-a[[i]]
#if (nrow(as)>5){
#  b[[i]] <- rpart(as$hotel_cluster ~ ., data=as, method="class")
#}
#  else{
#    b[[i]] <- 1
#  }
#}
library(randomForest)

jtt2<-transform(newtrain, newvar=paste(newtrain$user_location_country, newtrain$hotel_country))

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
jtt2$hotel_cluster <- relevel(as.factor(jtt2$hotel_cluster) , ref = "91")
modreg <- multinom(hotel_cluster ~ srch_children_cnt +hotel_country, data = jtt2)

r<-0
h<-0
i<-0
id<-list()
pred<-list()
dt1<-data.frame(0,0,0,0,0,0)
names(dt1)=c('id','p1','p2','p3','p4','p5')
ptr<-list()
dt<-data.frame(0,0,0,0,0,0)
names(dt)=c('id','p1','p2','p3','p4','p5')
for(i in 1:length(ztx2)){
  dt2<-dt1
  dat<-as.data.frame(at[[i]])
  t<-factor(unique(dat$newvar))
  ad<-which(ztx==t)
  if (length(ad)>0){
    as<-as.data.frame(a[[ad]])
    if (length(unique((as$hotel_cluster)))>5){
      mod <- randomForest(as.factor(hotel_cluster) ~ srch_adults_cnt+srch_children_cnt, data=as, ntree=100)
      pred[[i]]<-as.data.frame(predict(mod, dat, type="prob"))
      #id[[i]]<-dat$user_id
        for (h in 1:length(dat$user_id)){
          ptr[[h]]<-(head(order(pred[[i]][h,], decreasing = T),5))-1
          dt[r+h,]=c(dat$user_id[h],ptr[[h]])
        }
    } else{
    pred[[i]] <- rep(c(91,49,60,43,17), length(dat$user_id))
    for (k in 1:length(dat$user_id)){
      ptr[[k]]<-c(91,49,60,43,17)
      dt[r+k,]=c(dat$user_id[k],ptr[[k]])
    }
    #id[[i]]<-dat$user_id
  }} else{
    pred[[i]] <- predict(modreg, dat, type="prob")
      for (o in 1:length(dat$user_id)){
        l=1
          if (length(dat$user_id)==1) {
            ptr[[o]]<-c(91,49,60,43,17)
            dt[r+1,]=c(dat$user_id[o],ptr[[o]])
        } else {
        ptr[[o]]<-(head(order(pred[[i]][o,], decreasing = T),5))-1
        dt[r+o,]=c(dat$user_id[o],ptr[[o]])
        }
        l=1+o
      }
    #id[[i]]<-dat$user_id
  }
  r=r+o+k+h
}

table(newtrain$hotel_cluster)
pp<-as.vector(head(order(table(newtrain$hotel_cluster), decreasing = T),5)-1)


head(order(xcv[1,], decreasing = T),5)
which(xcv[1,]==max(xcv[1,]))

modedt<-rpart(as.factor(newtrain$hotel_cluster) ~ ., newtrain, method="class")
plot(modedt)
text(modedt)

Preddt<-predict(modedt, newtest, type="prob")

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
a1$hotel_cluster <- relevel(a1$hotel_cluster , ref = "91")
modreg <- multinom(hotel_cluster ~ srch_children_cnt +hotel_country, data = a1)


Pred<-predict(modreg, newtest, type="prob")

