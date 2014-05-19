classNames=c("earn","acq","money-fx","grain","crude",
             "trade","interest","ship","wheat","corn")
allClasses=matrix(data=0,nrow=length(combinedCorpus),ncol=length(classNames),
                       dimnames=list(c(1:length(combinedCorpus)),classNames))
for (i in 1:length(combinedCorpus)){
  allClasses[i,match(tm::meta(combinedCorpus[[i]],tag="Topics"),classNames)]=1
}

DTM_combC<-DocumentTermMatrix(combinedCorpus)
y<-DTM_combC[,bestPerfFeats]
y<-as.data.frame(as.matrix(y))

# Hierarchical Agglomerative 
distance<- dist(y, method = "euclidean") # or binary,canberra, maximum, manhattan
fit1 <- hclust(distance, method="ward")
groups <- cutree(fit1, k=10) # cut tree into 5 clusters
groups1 <- as.data.frame(groups)
plot(fit1, labels = NULL, hang = 0.1,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # display dendogram
rect.hclust(fit1, k=10, border="red")

plot(prcomp(y)$x, col=groups, pch=20, cex=0.5,xlim =c(-3.5,10.5),ylim=c(-10,5))


# K-means
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
#df <- scale(y)
wssplot(y) 

fit2 <- kmeans(y, 10)
plot(prcomp(y)$x, col=fit2$cl,pch=20, cex=0.5,xlim =c(-3.5,10.5),ylim=c(-10,5))


# EM clustering

library(mclust)

ptm <- proc.time()
fit3 <- Mclust(y,G=10)
proc.time() - ptm

plot(prcomp(y)$x, col=fit3$cl,pch=20, cex=0.5,xlim =c(-3.5,10.5),ylim=c(-10,5))
#summary(fit3) # display the best model

clustCMf <- function(groups,classes){
  clustCM<-matrix(0,10,12)
  colnames(clustCM)=c("earn","acq","money-fx","grain","crude","trade","interest","ship","wheat","corn","others","total")
  
  for(i in 1:dim(classes)[1])
  {
    if (sum(classes[i,])==0){clustCM[groups$groups[i],11]=clustCM[groups$groups[i],11]+1; } 
    else
    {clustCM[groups$groups[i],1:10]= clustCM[groups$groups[i],1:10]+classes[i,1:10]}
    
  }
  
  for(i in 1:10)
 {
    clustCM[i,1:11]=clustCM[i,1:11]/length(which(groups$groups==i))
    clustCM[i,12]=length(which(groups$groups==i))
  }
    
 
  return (clustCM)
}
#cbind(trainingClasses,rep(0, length(trainingClasses) ))
table1<- clustCMf(groups1,allClasses)
groups2<-as.data.frame(fit2$cl)
colnames(groups2)<-"groups"
table2<- clustCMf(groups2,allClasses)
groups3<-as.data.frame(fit3$cl)
colnames(groups3)<-"groups"
table3<- clustCMf(groups3,allClasses)

