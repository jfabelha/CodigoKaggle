gt<-data.frame(1,2,3,4,5)
names(gt)<-c("p1","p2","p3","p4","p5")
g<-list()
o<-0
i<-0
for (i in 1:length(pred)){
  yiu<-length(pred[[i]])
  iuy<-as.data.frame(pred[[i]])
  if (yiu==5){
    g[[i]]<-iuy
  } else{
    for (o in 1:nrow(iuy)){
      gt[o,]<-(head(order(iuy[o,], decreasing = T),5))-1
    }
    g[[i]]<-gt
  }
  gt<-data.frame(1,2,3,4,5)
  names(gt)<-c("p1","p2","p3","p4","p5")
}


for (i in 1:length(pred)){
row.names(g[[i]])<-row.names(pred[[i]])
names(g[[i]])<-c("p1","p2","p3","p4","p5")
}

library(reshape)
library(plyr)


df<-rbind.fill(g)

b<-0
a<-data.frame(1,2,3,4,5)
names(a)<-c("p1","p2","p3","p4","p5")
for (i in 1:length(g)){
  tre<-rbind(g[[i]],a)
  a<-nrow(g[[i]])+b
  b<-a
}

for (i in 1:length(pred)){
  a<-nrow(pred[[i]])+b
  b<-a
}