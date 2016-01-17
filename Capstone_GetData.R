#Check for directory or create it, set to working directory
if(!file.exists("~/CapstoneClass")) {
   dir.create("~/CapstoneClass")
}
setwd("~/CapstoneClass")

#download the data to a windows machine (method="curl" was NOT used for you mac users)
dataURL<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("~/CapstoneClass/Coursera-SwiftKey.zip")) {
   download.file(dataURL,destfile="~/CapstoneClass/Coursera-SwiftKey.zip")
   unzip("~/CapstoneClass/Coursera-SwiftKey.zip")
   dateDownloaded<-date()
}

#Open the necessary packages
library(tm)
library(R.utils)
library(iterators)
set.seed(2500000)

# Did not load blogs data since the rubric said to test with news or twitter.
# #Load Data with scan function
# blogs<-scan("~/CapstoneClass/final/en_US/en_US.blogs.txt",what = "character", sep = "\n")
# #sample it
# blogs.lines<-sample(1:length(blogs),size=.01*length(blogs),replace=FALSE)
# blogs.smpl<-blogs[blogs.lines]
# rm(blogs,blogs.lines)

#load in News
news<-scan("~/CapstoneClass/final/en_US/en_US.news.txt",what = "character", sep = "\n")
#sample the news
news.lines<-sample(1:length(news),size=.2*length(news),replace=FALSE)
news.smpl<-news[news.lines]
rm(news,news.lines)

#load in twitter
twitter<-scan("~/CapstoneClass/final/en_US/en_US.twitter.txt",what = "character", sep = "\n")
#sameple twitter
twitter.lines<-sample(1:length(twitter),size=.05*length(twitter),replace=FALSE)
twitter.smpl<-twitter[twitter.lines]
rm(twitter,twitter.lines)

#Write files to .txt
# fileConn<-file("~/CapstoneClass/blogs.txt")
# writeLines(blogs.smpl, fileConn)
# close(fileConn)
# rm(blogs.smpl)

fileConn<-file("~/CapstoneClass/news.txt")
writeLines(news.smpl, fileConn)
close(fileConn)
rm(news.smpl)

fileConn<-file("~/CapstoneClass/twitter.txt")
writeLines(twitter.smpl, fileConn)
close(fileConn)
rm(twitter.smpl)
rm(list=ls())
