gt<-data.frame(1,2,3,4,5)
names(gt)<-c("p1","p2","p3","p4","p5")
g<-list()
o<-0
i<-0
yiu<-0
iuy<-data.frame()
for (i in 1:length(pred)){
  yiu<-length(pred[[i]])
  iuy<-pred[[i]]
  if (yiu==5){
    g[[i]]<-iuy
  } else{
    #cat(i, "ok2")
    rew<-nrow(iuy)
    if (is.null(rew)==FALSE){
      #cat(i, "ok")
    for (o in 1:nrow(iuy)){
      #cat(o)
      gt[o,]<-(head(order(iuy[o,], decreasing = T),5))-1
    }} else{
      #cat(i, "yu")
      gt[1,]<-(head(order(iuy, decreasing = T),5))-1
    }
    g[[i]]<-gt
  }
  gt<-data.frame(1,2,3,4,5)
  names(gt)<-c("p1","p2","p3","p4","p5")
}



for (i in 1:length(at)){
row.names(g[[i]])<-row.names(at[[i]])
names(g[[i]])<-c("p1","p2","p3","p4","p5")
}


#for (i in 1:length(at)){
#  cat(nrow(g[[i]])==nrow(at[[i]]))
#}

library(reshape)
library(plyr)

b<-0
a<-data.frame(1,2,3,4,5)
names(a)<-c("p1","p2","p3","p4","p5")
for (i in 1:length(g)){
  tre<-rbind(g[[i]],a)
  a<-tre
}
a[nrow(a),]<-NULL
tail(a)
a[,6]<-as.numeric(row.names(a))
a<-a[order(a[,6]),]
a[,6]<-NULL


library(Metrics)

mapk(5, as.vector(newtest$hotel_cluster), as.vector(a))
mp<-c()
for (i in 1:nrow(newtest)){
mp[i]<-mapk(5, newtest$hotel_cluster[i], a[i,])
}

ok<-data.frame(newtest$id)
for (i in 1:nrow(a)){
  ok[i,2]<-paste(a[i,1:5],collapse=" ")
}
  
  
submission <- data.frame(id=ok[1:2528243,1], hotel_cluster=ok[1:2528243,2])
write.csv(submission, file = "submissiona.csv", row.names = F)
