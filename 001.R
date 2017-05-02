g<-data.frame()
ptr<-data.frame(1,2,3,4,5)
names(ptr)<-c("p1","p2","p3","p4","p5"
gt<-data.frame(1,2,3,4,5)
names(g)<-c("p1","p2","p3","p4","p5")

o<-0
i<-0
for (i in 1:length(pred)){
  yiu<-length(pred[[i]])
  if (yiu==5){
    g<-pred[[i]]
    names(g)<-c("p1","p2","p3","p4","p5")
  } else{
    iuy<-pred[[i]]
    yt<-id[[i]]
    for (o in 1:length(yt)){
      ptr[o,]<-(head(order(iuy[o,], decreasing = T),5))-1
    }
    g<-ptr
    names(g)<-c("p1","p2","p3","p4","p5")
  }
  gt<-rbind(g,gt)
  ptr<-data.frame()
  g<-data.frame()
}