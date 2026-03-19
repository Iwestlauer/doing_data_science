# Loading the Data from the NYT API

#Information about NYT API
#https://developer.nytimes.com/faq#a11
######################

library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(tidyverse)

NYTIMES_KEY = ""


# Let's set some parameters
term <- "election+trump+statistics" # Need to use + to string together separate words

begin_date <- "20240101" #What is this date format?? utc?
end_date <- "20250131"

# note-- had to change http to https
baseurl <- paste0("https://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

baseurl

initialQuery <- jsonlite::fromJSON(baseurl)
# initialQuery$response$metadata$hits
maxPages <- round((initialQuery$response$metadata$hits[1] / 10)-1)

maxPages # if maxPages is 999 and you have to sleep 7 seconds between each page... about 2 hours, so just change the search term?
# changed the search term by adding more words until max pages is 26

pages <- list()
for(i in 0:5){
  nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(12) # to avoid http 429 error (too many requests)
} # even with sys.sleep(7), I still see 429 Unknown Error
# I had to google the rate limit, it says sleep 12 seconds between calls

allNYTSearch <- rbind_pages(pages)
# Question... how do you know if the script is still running or if it paused? is it by the presence of the Stop button?

head(allNYTSearch$response.docs.keywords)
# Visualize coverage by section
allNYTSearch %>% 
  group_by(response.docs.type_of_material) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()




allNYTSearch$NewsOrOther = ifelse(allNYTSearch$response.docs.type_of_material == "News","News","Other")
sum(is.na(allNYTSearch$NewsOrOther)) # check no NA

# Visualize coverage of News or Other
allNYTSearch[!is.na(allNYTSearch$NewsOrOther),] %>% 
  group_by(NewsOrOther) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=NewsOrOther, fill=NewsOrOther), stat = "identity") + coord_flip()



#Identify Article and Tokenize

#Article 4 is "Other"
#Artilce 9 is "News"
#Artilce 44 is "Other"

ArticleToClassify = allNYTSearch[4,] # Check out 4, 9 and 44
ArticleToClassify$response.docs.headline.main

trueType = ArticleToClassify$NewsOrOther[1]
trueType

library(tm) #text mining library provides the stopwords() function
stopwords()

#The [^[:alnum:] ] replaces all non alphanumeric characters with nulls.  
theText = unlist(str_split(str_replace_all(ArticleToClassify$response.docs.headline.main,"[^[:alnum:] ]", ""), boundary("word"))) #Take out all but alpha numeric characters from search string

theText

wordsToTakeOut = stopwords()
#put word boundaries stopwords so that we don't detect partial words later
wordsToTakeOut = str_c(wordsToTakeOut,collapse = "\\b|\\b") 
wordsToTakeOut = str_c("\\b",wordsToTakeOut,"\\b")
wordsToTakeOut

importantWords = theText[!str_detect(theText,regex(wordsToTakeOut,ignore_case = TRUE))]

importantWords
#Find Percentages in News and Other

newsArticles = allNYTSearch %>% filter(NewsOrOther == "News")
otherArticles = allNYTSearch %>% filter(NewsOrOther == "Other")

numNewsArticles = dim(newsArticles)[1]
numOtherArticles = dim(otherArticles)[1]

numNewsArticles
numOtherArticles

thePercentHolderNews = c()
thePercentHolderOther = c()

for(i in 1 : length(importantWords)) #for each important word in the headline
{
  #number of News articles that have the ith word in the headline of interest
  numNews = sum(str_count(newsArticles$response.docs.headline.main,importantWords[i]))
  #number of Other articles that have the ith word in the headline of interest
  numOther = sum(str_count(otherArticles$response.docs.headline.main,importantWords[i]))
  
  #percentage of News articles that have the ith word in the headline of interest 
  thePercentHolderNews[i] = numNews / numNewsArticles
  #percentage of Other articles that have the ith word in the headline of interest
  thePercentHolderOther[i] = numOther / numOtherArticles
  
  #all the News percentages (for each word)
  thePercentHolderNews
  #all the Other percentages (for each word)
  thePercentHolderOther
  
}

thePercentHolderNews
thePercentHolderOther

classifiedAs = if_else(sum(thePercentHolderNews)>sum(thePercentHolderOther),"News","Other")
sum(thePercentHolderNews)
sum(thePercentHolderOther)


Result = str_c("The ", trueType," article was classified as ", classifiedAs, " with a News score of: ",round(sum(thePercentHolderNews),4), " and an Other score of: ", round(sum(thePercentHolderOther),4), ".") 
Result


## VISUALIZE

articleStats = data.frame(Word = importantWords, newsScore = thePercentHolderNews, otherScore = thePercentHolderOther)

# Wide Form / Not Tidy
articleStats

#Tidy and Plot
articleStats[,c(2,3)] %>% gather(Type,Percent) %>% mutate(Word = rep(articleStats$Word,2)) %>% ggplot(aes(y = Percent, x = Type, fill = Word)) + geom_col()

articleStats[,c(2,3)] %>% gather(Type,Percent) %>% mutate(Word = rep(articleStats$Word,2)) %>% ggplot(aes(y = Percent, x = Type, fill = Word)) + geom_col() + facet_wrap(~Word)



