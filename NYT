# install.packages("rtimes")
# library(rtimes)
# install.packages(tm)
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("lda")
# install.packages("austin", repos="http://r-forge.r-project.org", type="source")
# install.packages("ReadMe", repos = "http://r.iq.harvard.edu", type="source")
# install.packages("quadprog")
# install.packages("igraph")
# install.packages("stm")

library(rtimes)
library(tm)
library(SnowballC)
library(wordcloud)
library(lda)
library(austin)
library(ReadMe)
library(quadprog)
library(igraph)

nykey="4e796dba6b666c762981086357177f35:18:73474865"


res <- as_search(q="terrorism+war", begin_date = "19950101", end_date = '20101231',fq = "Front Page",key=nykey,page=0)

# n=res$meta$hits

n=1001

snippets=c()
dates=c()
headlines=c()

for(k in 1:(1+round(n/10))){

res <- as_search(q="terrorism", begin_date = "19950101", end_date = '20101231',key=nykey,page=k-1)

for(i in 1:10){

snippets=c(snippets,res$data[[i]][2])

if(length(res$data[[i]][2])>0){

dates=c(dates,res$data[[i]][11])

headlines=c(headlines,res$data[[i]][9]$headline$main)}}}


dates=as.vector(unlist(dates))[which(nchar(snippets)!=4)]
substr(dates,1,10)
dates=as.Date(dates)

headines=headlines[which(nchar(snippets)!=4)]
headlines=unlist(headlines)

snippets=snippets[which(nchar(snippets)!=4)]
snippets=unlist(snippets)


# We will start by using package "tm", which is one of the most popular text analysis packages.

articles=Corpus(VectorSource(snippets))

meta(articles)=headlines

# Now let's do some data cleaning

articles=tm_map(articles,stripWhitespace) # Get rid of large white spaces

articles=tm_map(articles, removePunctuation) # Remove all punctuation

articles=tm_map(articles, removeNumbers) # Remove numbers

articles=tm_map(articles, removeWords, c(stopwords("english"),"now","like","make","enter","can","will","shall","say","no","one","come","let","The","said")) # Stopwords has about three-hundred commonly used words in English. I added some other terms that it doesn't have after that.


# Now let's stem the words. This will require the package SnowBall

require(SnowballC)
artciles=tm_map(articles, stemDocument)

# Now let's make a term-document matrix

TDM=t(DocumentTermMatrix(articles))
inspect(TDM[1:20,1:20])

# Let's look at frequent terms

findFreqTerms(TDM, 10)

# Let's look for Obama's name in these articles

obama.count=as.numeric(inspect(TDM["obama",]))

plot(dates,obama.count, main="Obama's Name in Articles Related to Terrorism",ylab="mentions")


# Let's make a wordcloud of the words in the snippets

wordcloud(artciles,max.words=50,scale=c(3,0),colors=c(rep("cornflowerblue",15),rep("blue",20),rep("darkblue",15)))
