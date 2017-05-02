o<-data.frame()
w<-0
i<-0
id<-list()
pred<-list()
d<-list()
predi<-data.frame(,check.rows= TRUE)
for(i in 1:length(ztx2)){
  dat<-as.data.frame(at[[i]])
  t<-factor(unique(dat$newvar))
  ad<-which(ztx==t)
  if (length(ad)>0){
    as<-as.data.frame(a[[ad]])
    if (length(unique((as$hotel_cluster)))>5){
      mod <- randomForest(as.factor(hotel_cluster) ~ srch_adults_cnt+srch_children_cnt, data=as, ntree=100)
      pred[[i]]<-as.data.frame(predict(mod, dat, type="prob"))
      id[[i]]<-dat$user_id
      o<-as.data.frame(cbind(as.data.frame(id[[i]]),pred[[i]]))
    } else{
      for (w in 1:length(dat$user_id)){
        predi[w,] <- c(91,49,60,43,17)
      }
      id[[i]]<-dat$user_id
      o<-as.data.frame(cbind(as.data.frame(id[[i]]),as.data.frame(predi)))
    }} else{
      pred[[i]] <- predict(modreg, dat, type="prob")
      id[[i]]<-dat$user_id
      o<-as.data.frame(cbind(as.data.frame(id[[i]]),pred[[i]]))
    }
  d[[i]]<-o
}