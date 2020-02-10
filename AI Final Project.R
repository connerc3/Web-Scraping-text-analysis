

## Libraries


#install.packages("pracma")
#install.packages("sentimentr")
#install.packages("quanteda")
#install.packages("reshape2")
#install.packages("GGally")
#install.packages("purrr")
library(pracma)
library(RSelenium)
library(sentimentr)
library(lexicon)
library(rtweet)
library(tidytext)
library(topicmodels)
library(pdftools)
library(ldatuning)
library(dplyr)
library(qdapDictionaries)
library(rpart)
library(rpart.plot)
library(caTools)
library(quantmod)
library(tidyr)
library(tm)
library(tidyverse)
library(quanteda)
library(reshape2)
library(GGally)
library(purrr)





##########################################################################
##########################################################################
##########################################################################
### Part 1: The Web Scraper


dates<-c()
titles<-c()
texts<-c()


for (i in 1:np) {
  # runs through each page after getting data from each article
  
  for(j in 2:11){
    #clicks on each article
    el<-remDr$findElements(using="tag","article")
    el<-el[[j]]
    el<-el$findChildElements(using="tag","a")[[2]]
    titletest<-el$getElementText()
    
    #we ran into a page load error with encripted articles
    #this code skips these 3 articles
    if(strcmpi(as.vector(encrypt1),as.vector(titletest))==FALSE){
      
      el$clickElement() #article is only clicked when it is not encrypted
      Sys.sleep(.25)
    
      #once in each article pull the title, date, and text
      ti<-remDr$findElements(using="tag","h1")[[2]]
      title<-ti$findChildElement(using="class","field__item")
      titles<-c(titles,title$getElementText())
      
      date<-remDr$findElements(using="class name","h4")[[1]]
      dates<-c(dates,date$getElementText())
      
      text<-remDr$findElement(using="class","node__content")
      child<-text$findChildElements(using = "tag","p")
      CE<-length(child)
      kids<-c()
      
      if(CE>0){
        for(k in 1:CE){
          text<-remDr$findElement(using="class","node__content")
          child<-text$findChildElements(using = "tag","p")[[k]]
          kids<-c(kids,child$getElementText())
          kids<-toString(kids)}
          } else {
          text<-remDr$findElement(using="class","node__content")
          kids<-c(kids,text$getElementText())
          kids<-toString(kids)
          }

      texts<-c(texts,kids)
      
      #exit each page
      el$goBack() 
    }
    
  }
  # code to go to next page
  nextpage<-remDr$findElement(using = "class","pager__item--next")
  nextpage$clickElement()
}
#on last page for loop runs into "Error in el[[j]] : subscript out of bounds"
#when this is reached, move on to writing the data



#write all of the collected data into a data frame and csv
data<-data.frame(unlist(dates),unlist(titles),unlist(texts))
data<-setNames(data, c("dates", "titles", "text"))
write.csv(data,"the_data.csv")


#Close Rselenium
#rd$server$stop()















##########################################################################
##########################################################################
##########################################################################
### Part 2: Text Analysis

#load in data
data <- read.csv("the_data.csv", header=TRUE)
colnames(data)

#remove some special characters
data <- data %>%
  mutate_at(vars(titles, text), function(x){gsub('[^ -~]', '', x)})

#checking
head(data$dates)
head(data$titles)

# Storing data in tibble
data_sentiment <- as_data_frame(setNames(list(
  as.character(data$dates),as.character(data$titles),
  as.character(data$text)),c("date","title","text")))
head(data_sentiment)


# Title Sentiment
title_sentiment  <- (out <- with(data_sentiment, sentiment_by(
  get_sentences(title), list(date, title, text))))

TitleSent <-data.frame("Date" = title_sentiment$date,
                       "Title Sentiment" = title_sentiment$ave_sentiment)
# Reverse order
TitleSent <- TitleSent[order(as.Date(TitleSent$Date, format="%B %d, %Y")),] %>% 
  map_df(rev)

# Title Sentiment ## Tried w/ no result

## returns 192
#title_sent <- sentiment(as.character(data_sentiment$title),
#polarity_dt = hash_sentiment_sentiword)
# Date / Avg article sentiment / Word Count
#TitleSent <-data.frame(TitleSent)
#head(TitleSent)

## returns 17
#title.df <- as_data_frame(setNames(list(
#  as.character(data$title)),
#  c("title")))

#title_sent2 <- sentiment(as.character(title.df),
#                         polarity_dt = hash_sentiment_sentiword)


# Avg. Article Sentiment     # sentimentr
article_sentiment  <- (out <- with(data_sentiment, sentiment_by(
  get_sentences(text), list(date, title, text))))

AvgArtSent <-data.frame("Date" = article_sentiment$date, 
                        "Avg Article Sentiment" = article_sentiment$ave_sentiment,
                        "Word Count" = article_sentiment$word_count)

# Reverse order
AvgArtSent <- AvgArtSent[order(as.Date(AvgArtSent$Date, format="%B %d, %Y")),] %>% 
  map_df(rev)

# Date / Avg article sentiment / Word Count
AvgArtSent <-data.frame(AvgArtSent)
head(AvgArtSent)


# Sentence count

sentence_count <- nsentence(data_sentiment$text)
sentence_count <- data.frame("Sentence Count" = sentence_count)


# Computing overall sentiment of all text (Title and article text)    # hash_sentiment_sentiword lexicon 
overall_sentiment<- sentiment(as.character(data_sentiment),polarity_dt = hash_sentiment_sentiword)
overall_sentiment<- as.data.frame(overall_sentiment)
head(overall_sentiment)



# Final Table

table2 <- data.frame("Date" = AvgArtSent$Date, 
                     "TitleSentiment" = TitleSent$Title.Sentiment,
                     "AvgArticleSentiment" = AvgArtSent$Avg.Article.Sentiment,
                     "WordCount" = AvgArtSent$Word.Count,
                     "SentenceCount" = sentence_count$Sentence.Count)
head(table2)









##########################################################################
##########################################################################
##########################################################################
### Part 3: Download Stock Data

#stock data starting at Jan 14, 2010 to present day
stock.dates <- data$dates
stock.dates <- as.Date(stock.dates, format = "%B %d, %Y") # re-formating the dates to YYYY-MM-DD in order to pull dates 
#dates <- data.frame("Date" = dates) #decided to store as a list and not change to data frame
#dates



#open lists for storing needed variables
high<-c()
low<-c()
open<-c()
close<-c()
num<-as.numeric(length(stock.dates))


# pulling stock high low open and close from article ranges
#loops uses list of pulled dates and and assignes them to the to and from date used by quantmod
for(i in 1:num){
  date1 <- stock.dates[i]
  if(i < num){
    date2 <- stock.dates[i + 1]
  }else{
    # If at last row, use todays date
    date2 <- Sys.Date()
  }

  tryCatch({
    Stock <- data.frame(getSymbols("MCD",auto.assign=F, from = date2, to =date1))
    # Get Open Price of the period
    open <- c(open, Stock[row.names(Stock) == min(row.names(Stock)), "MCD.Open"])
    # Get Close Price of the period
    close <- c(close, Stock[row.names(Stock) == max(row.names(Stock)), "MCD.Close"])
    # Get High Price of the period
    high <- c(high,max(Stock[, "MCD.High"]))
    # Get Low Price of the period
    low <- c(low, min(Stock[, "MCD.Low"]))    
    
  }, error= function(err) {
    #print(err)
    #quantmod does not like when the date range is the same (multiple articles in one day)
    print(paste("Can't get Stock data for dates:",date2,date1))
    # Set NA if an error
    open <<- c(open, NA)
    high <<- c(high, NA)
    low <<- c(low, NA)  
  })

}

#write the stock data into a data frame and label the column names
stockdata<-data.frame(unlist(open),unlist(high),unlist(low))
stockdata<-setNames(stockdata, c("open","high","low"))

#compute the percent of change between high/open and low/open and the composite score
stockdata$openhigh<-((stockdata$high-stockdata$open)/stockdata$open)
stockdata$openlow<-((stockdata$low-stockdata$open)/stockdata$low)
stockdata$composite<-((stockdata$openhigh+stockdata$openlow)/2)

#write the file
write.csv(stockdata,"stockdata.csv")













##########################################################################
##########################################################################
##########################################################################
#Part 4: Analyzing the Data

#take the parts of section 1-3 that are needed for analysis and make one data frame out of them
Finaldata<- data.frame("dates"= stock.dates,
                       "articlesentiment"=table2$AvgArticleSentiment,
                       "titlesentiment"=table2$TitleSentiment,
                       "wordcount"=table2$WordCount,
                       "sentencecount"=table2$SentenceCount,
                       "openhigh"=stockdata$openhigh,
                       "openlow"=stockdata$openlow,
                       "composite"=stockdata$composite)
Finaldata<-na.omit(as.data.frame(Finaldata))




######## RETURN COMPOSITE ########

#Estimating linear model to predict return composite
Lmodel1 <- lm(composite~articlesentiment+titlesentiment+wordcount+sentencecount, data = Finaldata)
summary(Lmodel1)


#decision tree for return composite
tree1<- rpart(composite~articlesentiment+titlesentiment+wordcount+sentencecount, data=Finaldata)
prp(tree1,digits=4)
#For highest return composite of 0.02806 the word count should be greater/equal to 600 but less than 882 and the sentence count should be greater/equal to 21
#for the next highest return composite of 0.0148 the word count should be greater/equal to 600 but less than 882,
    #the sentence count should be less than 21 and the article sentiment should be less than 0.3701
#for the third highest return composite the word count should be less than 600 with article sentiment greater/equal to 0.2925, a word count less than 581, 
    #a sentence count less than 10 a title sentiment greater/equal to 0.09269 and greater/equal to 0.1786

#the following should be avoided as it produces the lowest composite score of -0.01559 word found less than 600, article sentiment greater/equal to 0.2925,
    #word count greater/equal to 581


#test prediction
fv1<- predict(tree1)
fv1
plot(Finaldata$composite,fv1)
sum((Finaldata$composite-fv1)^2)
#SSE = 0.04332832





######## Open-High ########

#Estimating linear model to predict open-high
Lmodel2 <- lm(openhigh~articlesentiment+titlesentiment+wordcount+sentencecount, data = Finaldata)
summary(Lmodel2)


#decision tree for open-high
tree2<- rpart(openhigh~articlesentiment+titlesentiment+wordcount+sentencecount, data=Finaldata)
prp(tree2,digits=4)
#for highest percent of change between open and high of 0.07854 the word count should be greater/equal to 679 but less than 882, 
    #and sentence count should be greater than 21
#for the next highest score of 0.05382 word count sould be less than 679 and greater than 39, title sentiment should be greater/equal to -0.139 
    #and greater/equal to 0.1018 and less than 0.162
#for the third highest score of 0.04753 the word count should be less than 679 and less than 39

#the following should be avoided as it produces the smallest percent of change of 0.008164 word count between [39,679), title sentiment should be 
    #greater/equal to -0.139 and greater/equal to 0.1018 and greater/equal to 0.162, and sentence count greater/equal to 16


#test prediction
fv2<- predict(tree2)
fv2
plot(Finaldata$openhigh,fv2)
sum((Finaldata$openhigh-fv2)^2)
#SSE = 0.08688374




######## Open-low ########

#Estimating linear model to predict open-low
Lmodel3 <- lm(openlow~articlesentiment+titlesentiment+wordcount+sentencecount, data = Finaldata)
summary(Lmodel3)


#decision tree for open-low
tree3<- rpart(openlow~articlesentiment+titlesentiment+wordcount+sentencecount, data=Finaldata)
prp(tree3,digits=4)
#for highest percent of change between open and low of -0.04578 the word count should be greater/equal to 292, article sentiment less than 0.5732,
    #title sentiment less than 0.7246 and less than 0.4396, less than 34 sentences, greater/equal to 13 sentences, with article sentiment between [0.3342,0.377)
#for next highest score of -0.04419 the word count should be less than 292 and the article sentiment less than 0.3316
#for the third highest score of -0.03398 the word count should be greater/equal to 292 and article sentiment greater/equal to 0.5732

#The followimg should be avoided as it produces the smallest percent of change of -0.007754 the word count should be greater/ equal to 292, 
    #article sentiment less than 0.5732, title sentiment less than 0.7249 and less than 0.4396, and sentence count greater/equal to 34


#test prediction
fv3<- predict(tree3)
fv3
plot(Finaldata$openlow,fv3)
sum((Finaldata$openlow-fv3)^2)
#SSE = 0.07552198
