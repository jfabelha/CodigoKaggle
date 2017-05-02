g<-data.frame(1,2,3,4,5,6)
names(g)<-c("id","p1","p2","p3","p4","p5")
ptr<-data.frame(2,3,4,5,6)
names(ptr)<-c("p1","p2","p3","p4","p5")
gt<-data.frame(1,2,3,4,5,6)
names(gt)<-c("id","p1","p2","p3","p4","p5")
o<-0
i<-0
for (i in 1:length(pred)){
  yiu<-length(pred[[i]])
  if (yiu==5){
    g<-data.frame(cbind(as.matrix(id[[i]]),as.matrix(pred[[i]])))
  } else{
    iuy<-as.data.frame(pred[[i]])
    yt<-id[[i]]
    for (o in 1:length(yt)){
      ptr[o,]<-(head(order(iuy[o,], decreasing = T),5))-1
    }
    g<-data.frame(cbind(as.matrix(yt),as.matrix(ptr)))
  }
  gt<-data.frame(rbind(as.matrix(g),as.matrix(gt)))
  ptr<-data.frame(2,3,4,5,6)
  names(ptr)<-c("p1","p2","p3","p4","p5")
}