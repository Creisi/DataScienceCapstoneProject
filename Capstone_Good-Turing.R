GTCount<-matrix(c(seq(0,6,1),rep(0,21)),nrow=7,ncol=4,
                   dimnames=list(c(seq(0,6,1)),c("Cnt","uni","bi","tri")))
uniLength<-length(unigramDF$uni)
uniFreqFreq<-data.frame(uni=table(unigramDF$Cnt))

#Calculate probability of unsing unigram
GTCount[1,2]<-uniFreqFreq[1,2]
GTCount[2:7,2]<-uniFreqFreq[1:6,2]

kFactor<-6*GTCount[7,2]/GTCount[2,2] #k=5
for(i in 0:5) {
   num<-((i+1)*GTCount[i+2,2]/GTCount[i+1,2])-(i)*kFactor
   den<-1-kFactor
   GTCount[i+1,2]<-num/den
   }
rm(uniFreqFreq)

biLength<-length(bigramDF)
biFreqFreq<-data.frame(bi=table(bigramDF$Cnt))

GTCount[1,3]<-uniLength^2 - biLength
GTCount[2:7,3]<-biFreqFreq[1:6,2]

kFactor<-6*GTCount[7,3]/GTCount[2,3]
for (i in 0:5) {
   num<-(i+1)*GTCount[i+2,3]/GTCount[i+1,3]-(i)*kFactor
   den<-1-kFactor
   GTCount[i+1,3]<-num/den
   }
rm(biFreqFreq)  

triLength<-length(trigramDF)
triFreqFreq<-data.frame(tri=table(trigramDF$Cnt))

GTCount[1,4]<-uniLength^3 - triLength
GTCount[2:7,4]<-triFreqFreq[1:6,2]

kFactor<-6*GTCount[7,4]/GTCount[2,4]
for(i in 0:5) {
   num<-(i+1)*GTCount[i+2,4]/GTCount[i+1,4]-(i)*kFactor
   den<-1-kFactor
   GTCount[i+1,4]<-num/den
}
rm(triFreqFreq,biLength,den,i,kFactor,num,triLength,uniLength,bigramDF,unigramDF)
write.csv(GTCount,file="~/CapstoneClass/GTCount.csv")