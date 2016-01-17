setwd("~/CapstoneClass")
library(tm)
library(stringr)
library(RWeka)
library(qdap)
library(lsa)

#create teh Tokenize Function
tokenizeFUN <- function(x, minNG, maxNG) {
   NGramTokenizer(x, Weka_control(min = minNG, max = maxNG))
}

mCorpus<-unlist(masterCorpus)
rm(masterCorpus)
num<-as.integer(length(mCorpus))

#Create tokenized lists
uniGram<-tokenizeFUN(mCorpus,1,1)
biGram<-tokenizeFUN(mCorpus,2,2)
triGram<-tokenizeFUN(unlist(mCorpus),3,3)

#Create Data frames
unigramTDM<-as.TermDocumentMatrix(uniGram)
unigram<-as.matrix(unigramTDM)
unigramDF<-as.data.frame(unigram)
names(unigramDF)<-"Cnt"
unigramDF$uni<-as.vector(row.names(unigram))
unigramDF<-unigramDF[order(-unigramDF$Cnt,unigramDF$uni),]
row.names(unigramDF)<-1:nrow(unigramDF)
rm(unigramTDM,unigram,unifreq,uniGram)

bigramTDM<-table(biGram)
bigram<-as.matrix(bigramTDM)
bigramDF<-as.data.frame(bigram)
names(bigramDF)<-"Cnt"
bigramDF$bi<-as.vector(row.names(bigram))
bigramDF<-bigramDF[order(-bigramDF$Cnt,bigramDF$bi),]
row.names(bigramDF)<-1:nrow(bigramDF)
rm(bigramTDM,bigram,bifreq,biGram)

trigramTDM<-table(triGram)
trigram<-as.matrix(trigramTDM)
rm(trigramTDM)
trigramDF<-as.data.frame(trigram)
names(trigramDF)<-"Cnt"
trigramDF$tri<-as.vector(row.names(trigram))
trigramDF<-trigramDF[order(-trigramDF$Cnt,trigramDF$tri),]
row.names(trigramDF)<-1:nrow(trigramDF)
rm(trigramTDM,trigram,trifreq,triGram)
# Write just the trigram to csv to use in the predicstion model.
# the unigram and bigram will get used to create the Good-Turning table and then removed
write.csv(trigramDF,"~/CapstoneClass/trigramDF.csv")

