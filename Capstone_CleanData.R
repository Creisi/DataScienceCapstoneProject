setwd("~/CapstoneClass")
library(tm)
library(stringr)
library(RWeka)
library(qdap)

#Create the clean up function
cleanupFun<-function(x){
   x <- tm_map(x, qdap::clean)
   x <- tm_map(x, qdap::scrubber)
   x <- tm_map(x, qdap::replace_symbol)
   remove.URL <- function(y) { gsub("http[[:alnum:]]*", "", y) }
   x <- tm_map(x, remove.URL)
   remove.Amp <- function(y) { gsub("&amp;", "", y) }
   x <- tm_map(x, remove.Amp)
   remove.WWW <- function(y) { sub("www[[:alnum:]]*", "", y) }
   x <- tm_map(x, remove.WWW)
   remove.equals <- function(y) { gsub("=", "", y) }
   x <- tm_map(x, remove.equals)
   x <- tm_map(x, removeNumbers)
   x <- tm_map(x, removePunctuation)
   x <- tm_map(x, tolower)
   badwords <- readLines("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
   x <- tm_map(x, removeWords, badwords)
   x <- tm_map(x,stripWhitespace)
}


# Blogs information left out since the rubric says to test with news and twitter
# # Read in the blogs data already stored as text files
# blogsData<-readLines("~/CapstoneClass/blogs.txt",encoding="UTF-8")
# #Create the blogs Corpus
# blogsCorpus<-VCorpus(VectorSource(blogsData),
#                      readerControl=list(language="en"))
# rm(blogsData)
# #Clean up the blogsCorpus
# blogsCorpus<-cleanupFun(blogsCorpus)
# #further cleaning to remove UTF-8 punctuation
# for(j in seq(blogsCorpus)){
#    blogsCorpus[[j]][[1]]<-iconv(blogsCorpus[[j]][[1]],"UTF-8","ASCII","?")
#    blogsCorpus[[j]][[1]]<-gsub("[[:punct:]]", "", blogsCorpus[[j]][[1]])
# }
# rm(j)

# Read In News data
newsData<-readLines("~/CapstoneClass/news.txt",encoding="UTF-8")
#Create News Corpus
newsCorpus<-VCorpus(VectorSource(newsData),
                    readerControl=list(language="en"))
rm(newsData)
#Clean up the newsCorpus
newsCorpus<-cleanupFun(newsCorpus)
#further cleaning of the newsCorpus
for(j in seq(newsCorpus)){
   newsCorpus[[j]][[1]]<-iconv(newsCorpus[[j]][[1]],"UTF-8","ASCII","?")
   newsCorpus[[j]][[1]]<-gsub("[[:punct:]]", "", newsCorpus[[j]][[1]])
}
rm(j)

# Read In twitter data
twitterData<-readLines("~/CapstoneClass/twitter.txt",encoding="UTF-8")
#Create twitter Corpus
twitterCorpus<-VCorpus(VectorSource(twitterData),
                    readerControl=list(language="en"))
rm(twitterData)
#Clean up the newsCorpus
twitterCorpus<-cleanupFun(twitterCorpus)
#further cleaning of the newsCorpus
for(j in seq(twitterCorpus)){
   twitterCorpus[[j]][[1]]<-iconv(twitterCorpus[[j]][[1]],"UTF-8","ASCII","?")
   twitterCorpus[[j]][[1]]<-gsub("[[:punct:]]", "", twitterCorpus[[j]][[1]])
}
rm(j)

#Combine all into one Corpus and write to disk
#masterCorpus<-c(blogsCorpus,newsCorpus,twitterCorpus)
masterCorpus<-c(newsCorpus,twitterCorpus)
write(unlist(masterCorpus),"~/CapstoneClass/master.txt")
#writeLines(unlist(masterCorpus), con="~/CapstoneClass/master2.txt")
rm(cleanupFun,blogsCorpus,newsCorpus,fileConn,twitterCorpus)
