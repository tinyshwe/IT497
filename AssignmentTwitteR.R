install.packages("twitteR")
install.packages("httr")
install.packages("ROAuth")
install.packages("plyr")
install.packages("stringr")
library(twitteR)
library(httr)
library(ROAuth)
library(plyr)
library(stringr)
my.key<-'3wvUMbl0yakiTmMscPLPrKzMa'
my.secret<-'fTuSNJhg84PkU9qx2K2KkFnGV7Oce1T0Pdoq1LEqaiLcozAXwq'
access.token<-'704317240745836544-Ve2x8iAx45hd4HJECJ3CV4ZcHxEJuJX'
access.token.secret<-'Z4X3O0ovfwaMH22AyXJe2gd52sYh9bZiB9RvGOcK7ghwv'
setup_twitter_oauth(my.key,my.secret,access.token,access.token.secret)
tweets<-searchTwitter('machineLearning',n=100,lang='en')
length(tweets)
head(tweets,5)
tweets<-(strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE))
length(tweets)
df = twListToDF(tweets)

tweets.text<-laply(tweets,function(t) t$getText())
head(tweets.text,5)
