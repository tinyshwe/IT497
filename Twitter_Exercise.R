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
install.packages("ggplot2")
library(ggplot2)
my.key<-'3wvUMbl0yakiTmMscPLPrKzMa'
my.secret<-'fTuSNJhg84PkU9qx2K2KkFnGV7Oce1T0Pdoq1LEqaiLcozAXwq'
access.token<-'704317240745836544-Ve2x8iAx45hd4HJECJ3CV4ZcHxEJuJX'
access.token.secret<-'Z4X3O0ovfwaMH22AyXJe2gd52sYh9bZiB9RvGOcK7ghwv'
setup_twitter_oauth(my.key,my.secret,access.token,access.token.secret)
tweets<-searchTwitter('Diwali',n=100,lang='en')
length(tweets)
head(tweets,5)
tweets<-(strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE))
length(tweets)
df = twListToDF(tweets)
names(df)

tweets.text<-laply(tweets,function(t) t$getText())
head(tweets.text,25)

# Packages for getting Tweets
##
library(twitteR)
library(httr)
library(ROAuth)
##

## Packages for working with Tweet data
##
library(plyr)
library(stringr)
##

## Package for graphing data
library(ggplot2)
##

# This sets up the OAuth credentials for a twitteR session
# Here are my codes -- please insert your own
my.key <-'EEJrnUQoN1rs7FgURGnQW9gzF'
my.secret <-'XsGD40IvWLUAm7L2o1hiWd3gUDhgmtTz9ZNJHaaaJ4JEW8viVa'
access.token <- '39905943-JdcJ774abauDdT1Xo0qODJfaAuOwGzZLVHC6RQHf0'
access.token.secret <- 'FZSL36Lb65cS0FXgwGZwQkZw5eoV270VGmCrO4EBY40Oo'

# Create Twitter Connection
# The OAuth authentication handshake
setup_twitter_oauth(my.key, my.secret, access.token, access.token.secret) 

# Search for @BatmanvSuperman Tweets 
tweets <- searchTwitter('ADHM', n=100, lang='en')

# See how many tweets we found
length(tweets)

# Show first 25 tweets -- sort of
head(tweets,25)

# Strip out Retweets
tweets <- (strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE))

# See how many original tweets we found
length(tweets)

# I will come back to this
df = twListToDF(tweets)

# Let's take a look at the twListToDF function
names(df)

# Loop over tweets and extract text
tweets.text <- laply(tweets, function(t) t$getText())

# Show text from first 25 tweets
head(tweets.text, 25)

# Read in dictionary of positive and negative works
yay <- scan('positive-words.txt',
            what='character', comment.char=';')
boo <- scan('negative-words.txt',
            what='character', comment.char=';')

# Add a few twitter and #BvS-specific negative phrases
bad_text <- c(boo, 'wtf', 'epicfail', 'trainwreck', 'sadaffleck', 'daredevil')
good_text <- c(yay, 'epic', 'dope', 'fans')



############################################################################
# Here is our sentiment score function  
############################################################################
score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores <- laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence <- tolower(sentence)        
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, good_text)
    neg.matches <- match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

############################################################################
## 
############################################################################

# Call the function and return a data frame
BvS <- score.sentiment(tweets.text, good_text, bad_text, .progress='text')
BvS$name <- 'ADHM'
plotdat <- BvS
# Cut the text, just gets in the way
plotdat <- BvS[c("name", "score")]

# Remove neutral values of 0
plotdat <- plotdat[!plotdat$score == 0, ]

# Nice little quick plot
qplot(factor(score), data=plotdat, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score")