#setwd("~/CapstoneClass/ShinyApp")
library(tm)
library(stringr)
library(RWeka)
library(qdap)
library(shiny)
#library(rsconnect)
#library(shinyapps)
#deployApp(appName="DataScienceCapstoneProject")
#,appDir = getwd()


options(digits=4)
options(mc.cores=1)

#Bring in the Trigram information
trigramDF<-read.csv("trigramDF.csv")[,-1]
#trigramDF<-read.csv("https://dl.dropboxusercontent.com/u/81409190/CapstoneShinyData/trigramDF.csv")[,-1]

#Create trigam model with bigrams and unigrams for look up
bigram<-sub(" ","@@@@",trigramDF$tri)
bigram<-sub(" .*","",bigram)
trigramDF$bi<-sub("@@@@"," ",bigram)
rm(bigram)
trigramDF$uni<-sub(".* ","",trigramDF$bi)
trigramDF$w3<-sub(".* ","",trigramDF$tri)

#Bring in the Good-Turing Model.
GTCount<-(read.csv("GTCount.csv")[,-1])
#GTCount<-(read.csv("https://dl.dropboxusercontent.com/u/81409190/CapstoneShinyData/GTCount.csv")[,-1])

#bring in the Corpus for the document term matrix used in the Freq function
masterData<-readLines("master.txt",encoding="UTF-8")
#masterData<-readLines("https://dl.dropboxusercontent.com/u/81409190/CapstoneShinyData/master.txt",encoding="UTF-8")
masterCorpus<-VCorpus(VectorSource(masterData),
                      readerControl=list(language="en"))
rm(masterData)
docs<-tm_map(masterCorpus,PlainTextDocument)
tdm<-DocumentTermMatrix(masterCorpus)
rm(docs,masterCorpus)

#Clean up the user input
cleanInput<-function(word) {
   word<-tolower(word)
   word<-gsub("[^[:alnum:][:space:]\']","",word)
   word<-gsub("^[ ]{1,10}","",word)
   word<-gsub("[ ]{2,10}"," ",word)
   return(word)
}

#Extract last two words from the input
lastTwo<-function(phrase) {
   space<-max(gregexpr(pattern =" ",phrase)[[1]])
   substring(phrase,space)<-"}"
   phrase<-tail(strsplit(phrase,split=" ")[[1]],1)
   phrase<-sub("}"," ",phrase)
   return(phrase)   
}


# Function that matches the first part of the input with the documen term matrix
# to, hopfully, add some accuracy.
freq<-function(phrase) {
   freq<-findAssocs(tdm, rm_stopwords(strip(phrase), tm::stopwords("english"))[[1]], corlimit=0.01)
   freqM<-data.frame(unlist(freq))
   rm(freq)
   freqM$comb<-row.names(freqM)
   freqM$word<-substr(freqM$comb,1,gregexpr(pattern ="[.]",freqM$comb)[[1]][[1]]-1)
   freqM$match<-substr(freqM$comb,gregexpr(pattern ="[.]",freqM$comb)[[1]][[1]]+1,
                       length(freqM$comb))
   row.names(freqM)<-1:nrow(freqM)
   names(freqM)<-c("corr","comb","word","w3")
   freqM$corr<-as.numeric(freqM$corr)
   freqDF<-data.frame(tapply(freqM$corr,freqM$w3,sum))
   rm(freqM)
   freqDF$w3<-row.names(freqDF)
   names(freqDF)<-c("corr","w3")
   freqDF<-freqDF[order(-freqDF$corr),]
   freqDF$corr<-freqDF$corr*100
   row.names(freqDF)<-1:nrow(freqDF)
   return(freqDF)
}

# Create a table of output with the top [cluster] results
buildTable2<-function(subTri,cluster=7) {
   rows<-nrow(subTri)
   useTri<-subTri
   useTri$rnk<-1
   rowU<-1
   if(rows==1){
      #useTri<-subTri
      return(useTri[,1:2])
   } else {
      for (i in 1:(rows-1)){
         if ((useTri[i,2]-useTri[i+1,2])>.00001) {
            (useTri[i+1,3]<-useTri[i,3]+1)
         } else {
            useTri[i+1,3]<-useTri[i,3]
         }
      }
      useTri<-useTri[which(useTri$rnk<=cluster),]
      return(useTri[,1:2])
   } 
}

# Main prediction function 
predict<-function(input,answer,cluster=7,freqOp=FALSE){# his has cluster=7
   orig.input<-input
   oirg.answer<-answer
   input<-cleanInput(input)
   input<-lastTwo(input)
   answer<-cleanInput(answer)
   inputSize<-length(strsplit(input, " ")[[1]])
   if (inputSize!=2) stop("There needs to be at least two words")
   nCount<-sum(trigramDF[which(trigramDF$bi==input),1])
   if(nCount==0) {
      input<-gsub(".* ","",input)
      nCount<-sum(trigramDF[which(trigramDF$uni==input),1])
      if(nCount==0)  stop ("This phrase goes beyond my ability to build an\n",
                           "algorithm to predict. Don't be proud you stumped me,\n",
                           "it wasn't that hard.")
      #subset all the bigrams taht begin with the unigram
      seekTri<-grepl(paste("^",input,"$",sep=""),trigramDF$uni)
      subTri<-trigramDF[seekTri,]
      subTri<-aggregate(subTri$Cnt,list(subTri$w3),sum)
      names(subTri)<-c("w3","Cnt")
      # Put in the Freq table stuff here.
      if (freqOp==TRUE) {
         freqDF<-freq(orig.input)
         combine<-merge(freqDF,subTri)
         combine$NewCnt<-combine$Cnt+combine$corr
         combine<-combine[order(-combine$NewCnt),]
         subTri<-combine[,c(1,4)]
         names(subTri)<-c("w3","Cnt")
         row.names(subTri)<-1:nrow(subTri)
      }
      subTri<-subTri[order(subTri$Cnt,decreasing=T),]
      useTri<-buildTable2 (subTri)
      for (i in 1:length(useTri$Cnt)){
         count=useTri[i,2]
         if(count<=5) {
            useTri[i,2]<-GTCount[count+1,2]
         }
      }
   } else {
      # use all recorded trigrams taht begin with bigram
      seekTri<-grepl(paste("^",input,"$",sep=""),trigramDF$bi)
      subTri<-trigramDF[seekTri,]
      subTri<-aggregate(subTri$Cnt,list(subTri$w3),sum)
      names(subTri)<-c("w3","Cnt")
      if (freqOp==TRUE) {
         freqDF<-freq(orig.input)
         combine<-merge(freqDF,subTri)
         combine$NewCnt<-combine$Cnt+combine$corr
         combine<-combine[order(-combine$NewCnt),]
         subTri<-combine[,c(1,4)]
         names(subTri)<-c("w3","Cnt")
         row.names(subTri)<-1:nrow(subTri)
      }   
      subTri<-subTri[order(-subTri$Cnt),]
      useTri<-buildTable2(subTri)
      for (i in 1:length(useTri$Cnt)) {
         count=useTri[i,2]
         if(count<=5) {
            useTri[i,2]<-GTCount[count+1,4]
         }
      }
   }
   options(digits=4)
   
   predictWord<-data.frame(Word=useTri$w3,
                           probability=(useTri$Cnt/nCount)*100,stringsAsFactors=FALSE)
   print(paste("Words that might complete your phrase: ",orig.input))
   if (answer %in% predictWord$Word) {
   print(paste("Your answer [",oirg.answer,"] is in row " 
               ,grep(answer,predictWord$Word)
               ," in the list."))
      } else {""}
   print(predictWord)
   if (answer %in% predictWord$Word) {
      print("Hey look I got it right!")
   } else {cat("OK, you stumped my algorithm. \nDon't be too proud, it wasn't that hard to do.")}
   #   return(predictWord)   
}

#source('server.R', local=TRUE)

shinyServer(
   function(input, output) {
      output$PredictFUN<-renderPrint({

         predict(input$input,input$answer,freqOp=input$freq)
         
         withProgress(message="Hold on, it's thinking",
                     min=1, max=15,
                     value = 0,
                     {n<-if(input$freq==FALSE){5} else {100}
                        for (i in 1:n) {
                        incProgress(1/n)
                        Sys.sleep(0.25)
                        }
                  })
         
         })
      output$trigramPlot<-renderPlot({
         barplot(trigramDF[grepl(lastTwo(input$input),trigramDF$bi),1],
                 main="Counts of trigrams that match your phrase", 
                 col=rainbow(nlevels(as.factor(trigramDF[grepl(lastTwo(input$input),trigramDF$bi),2]
                 ))),horiz=TRUE,xlab="Counts",cex.names=.75,las=2,names.arg=(trigramDF[grepl(lastTwo(input$input),trigramDF$bi),2]))
      })
               
   }
)
