#Live Session 4 For Live Session Web Scraping Code

library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML

# XML
data <-getURL("https://www.w3schools.com/xml/simple.xml")
data
doc <- xmlParse(data)
doc
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
length(grep("covered",bfasts$description))
grepl("covered",bfasts$description)
sum(grepl("covered",bfasts$description))
which(grepl("covered",bfasts$description))

if (sum(bfasts$names == "French Toast") >= 1)
{
  print(bfasts[which(bfasts$names == "French Toast"),"price"])
}




# rvest

hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_nameR <- html_nodes(hp,"name")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8) #generates warning
hp_name = stri_sub(as.character(hp_nameR),7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
bfast = data.frame(hp_name,hp_price,hp_desc)
bfast
grep("toast", bfast$hp_desc)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))

# Scraping xml


#Breakout 1

#using xml ... what is the problem?
data <-"http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
data2 = getURL("http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
zipcodes <- xpathSApply(doc,"//zipcode",xmlValue)
councildistrict <- xpathSApply(doc,"//councildistrict",xmlValue)
rests = data.frame(names,zipcodes,councildistrict)
dim(rests)
restsDTown = rests[which(rests$councildistrict == "11"),]
grep("Sushi",rests$names,ignore.case = TRUE)
sum(grepl("Sushi",rests$names,ignore.case = TRUE))



#Using rvest

hp<-read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
hp_name2 <- html_nodes(hp,"name")
hp_zipcode2 <- html_nodes(hp,"zipcode")
hp_councildistrict2 <- html_nodes(hp,"councildistrict")

hp_name2 = stri_sub(as.character(hp_name2),7,-8)
hp_zipcode2 = stri_sub(hp_zipcode2,10,-11)
hp_councildistrict2 = stri_sub(hp_councildistrict2,18,-19)

hp_zipcode2 = as.numeric(hp_zipcode2)
hp_councildistrict2 = as.numeric(hp_councildistrict2)


#How many restaurants total 
restByDist = hist(hp_councildistrict2)
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants")
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Number of Restaurants",ylab = "Council District", horiz = TRUE)

RestaurantDF = data.frame(Name = hp_name2, Zip = hp_zipcode2, District = hp_councildistrict2)
RestaurantDF %>% ggplot(aes(x = District, fill = factor(District))) + geom_bar(stat = "count")
RestaurantDF %>% ggplot(aes(x = factor(District), fill = factor(District))) + geom_bar(stat = "count")
RestaurantDF %>% ggplot(aes(x = factor(District), fill = factor(District))) + geom_bar(stat = "count") + theme(legend.position = "none") + ggtitle("Number of Restaurants per District") + xlab("District") + ylab("Number of Restaurants")  
#https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2
RestaurantDF %>% ggplot(aes(x = factor(District), fill = factor(District))) + geom_bar(stat = "count") + theme(legend.position = "none") + ggtitle("Number of Restaurants per District") + xlab("District") + ylab("Number of Restaurants") + geom_text(stat='count', aes(label=..count..), vjust=-1)


#How many Sushi Restaurants in Downtown?
restsDTown = RestaurantDF %>% filter(District == "11")
grep("Sushi",restsDTown$Name,ignore.case = TRUE)
sum(grepl("Sushi",restsDTown$Name,ignore.case = TRUE))

restsDTown[81,]$Name
restsDTown[81,"Name"]

#Edo Sushi
#http://edosushimd.com/innerharbor.htm

#Bonus: Plot Number of Sushi Restaurants by District

SushiBaltimore = RestaurantDF %>% group_by(District) %>% summarize(Num = sum(grepl("Sushi",Name,ignore.case = TRUE)))

SushiBaltimore %>% ggplot(aes(x = factor(District), y = Num, fill = factor(District))) + geom_col() + theme(legend.position = "none") + ggtitle("Number of Sushi Restaurants per District") + xlab("District") + ylab("Number of Sushi Restaurants")  


# New extension to ggplot2!
#install.packages("patchwork")

library(patchwork)

Restaurant_plot = RestaurantDF %>% ggplot(aes(x = factor(District), fill = factor(District))) + geom_bar(stat = "count") + theme(legend.position = "none") + ggtitle("Number of Restaurants per District") + xlab("District") + ylab("Number of Restaurants") + geom_text(stat='count', aes(label=..count..), vjust=-1)
Restaurant_plot
#Sushi_plot = SushiBaltimore %>% ggplot(aes(x = factor(District), y = Num, fill = factor(District))) + geom_col() + theme(legend.position = "none") + ggtitle("Number of Sushi Restaurants per District") + xlab("District") + ylab("Number of Sushi Restaurants") + geom_text(stat='count', aes(label=Num), vjust=-1)
#Sushi_plot

SushiBaltimore %>% ggplot(aes(x = factor(District), y = Num, fill = factor(District))) + geom_col() + theme(legend.position = "none") + ggtitle("Number of Sushi Restaurants per District") + xlab("District") + ylab("Number of Sushi Restaurants") + geom_text(aes(label=Num), vjust=-1)
Sushi_plot=SushiBaltimore %>% ggplot(aes(x = factor(District), y = Num, fill = factor(District))) + geom_col() + theme(legend.position = "none") + ggtitle("Number of Sushi Restaurants per District") + xlab("District") + ylab("Number of Sushi Restaurants") + geom_text(aes(label=Num), vjust=-1)
Sushi_plot

Restaurant_plot + Sushi_plot

Restaurant_plot / Sushi_plot

(Restaurant_plot + Sushi_plot) / Restaurant_plot




# Break Out 2

#Harry Potter

#1A / 1B
hp<-read_html("http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1")
hp_table<-html_nodes(hp,"table")
derp<-html_table(hp_table)

# Find the right table
derp[3]

#1C - Cleaning
a<-data.frame(derp[3])
names(a) <- c("Blank", "Actor", "Blank2","Character")
df<-a[2:length(a$Actor),c("Actor", "Character")]
df$Character[10] <- "Griphook / Professor Filius Flitwick"

# 1D -Edit The Cast List
b<-df %>%
  slice(-92) %>% # Removes the row that is just noting the rest is alphabetical
  separate(Actor, into=c("FirstNames", "Surname"), sep="[ ](?=[^ ]+$)") # Separates the Last Name

#1E 
head(b, 10)




#Stars
stars<-read_html("http://www.espn.com/nhl/team/roster/_/name/dal/dallas-stars")
stars_table<-html_nodes(stars, "table")
stars_dfs<-html_table(stars_table, fill = TRUE)

Rost1 = stars_dfs[[1]]
Rost2 = stars_dfs[[2]]
Rost3 = stars_dfs[[3]]
Rost4 = stars_dfs[[4]]
Rost5 = stars_dfs[[5]]
 
Roster = rbind(Rost1,Rost2)
Roster = rbind(Roster,Rost3)
Roster = rbind(Roster, Rost4)
Roster = rbind(Roster, Rost5)

# Try Again

Roster = rbind(Rost1,Rost2)
Roster = rbind(Roster,Rost3)
Roster = rbind(Roster, Rost4)

Roster$Glove = "NA"
Rost5$Shot = "NA"
Rost5 = Rost5[,c(1,2,3,4,5,9,7,8,6)]
Roster = rbind(Roster, Rost5)
print(Roster, n = Inf) #Inf prints all rows

# Delete First Column (Probably should have done this first.)

Roster = Roster[,-1]
print(Roster, n = Inf) #Inf prints all rows


# API

#Help in Indicator Lookup: https://data.worldbank.org/indicator

install.packages("WDI")
## Install and load package
library(WDI)

## Search for fertilizer consumption data
WDIsearch("Data") %>% datatable()

## Use indicator number to gather data
FertConsumpData <- WDI(indicator="AG.CON.FERT.ZS")

MaleOFSD <- WDI(country = "US", indicator="UIS.ROFST.H.2.Q3.F", start = 2020, end = 2022)




# You Try It: Study the "Children out of school, primary, male" for 2018 through 2022 in the US ... Do the best you can. 
# Try and plot the information if you can

WDIsearch("Out of School")

MaleOutOfSchoolUS <- WDI(country = c("US"), indicator="SE.PRM.UNER.MA", start = 2017, end = 2022)

MaleOutOfSchoolUS %>% ggplot(x = SE.PRM.UNER.MA)

#Compare to Australia

MaleOutOfSchoolAU <- WDI(country = "AU", indicator="SE.PRM.UNER.MA", start = 2017, end = 2022)


#Compare to China

MaleOutOfSchoolCN = WDI(country = "CN", indicator="SE.PRM.UNER.MA", start = 2017, end = 2022)


# Compare All at once
MaleOutOfSchoolUS <- WDI(country = c("US","CN","AU"), indicator="SE.PRM.UNER.MA", start = 2017, end = 2022)

MaleOutOfSchoolUS %>% ggplot(aes(x = year, y = SE.PRM.UNER.MA, color = country)) + geom_smooth(method = "lm")


# Compare Percent Percent

#WDIsearch("Out of School")

MaleOutOfSchoolUS <- WDI(country = c("US","CN","AU"), indicator="SE.PRM.UNER.MA.ZS", start = 2017, end = 2022)

MaleOutOfSchoolUS %>% ggplot(aes(x = year, y = SE.PRM.UNER.MA.ZS, color = country)) + geom_smooth(method = "lm")







# World Bank Development Indicators

#Useful URL in explainging WDI XML and JSON data formats.
#https://datahelpdesk.worldbank.org/knowledgebase/articles/898599-indicator-api-queries

#Goal 1: Create a bar chart of topics relating to gdp.

#search for reports with "gdp" in the description
results = as.data.frame(WDIsearch("gdp"))

#Many reports have more than 4 parts of the indicator
# This is in contrast to this documentation: 
#https://datahelpdesk.worldbank.org/knowledgebase/articles/201175-how-does-the-world-bank-code-its-indicators
# We use a new function from a new package that we will cover later: str_count
# This function is in the stringr package and simply counts the number of a specific 
# character ("\\.") in a given string (indicator)
# The \\ means to literally look for the '.' which means something else in this context.
#This line will filter the data frame to leave only those with 4 pieces in the indicator.
resultsGoodIndicator = results %>% filter(str_count(indicator,"\\.")==3)

#Check out the new data frame with only 4 piece indicators
resultsGoodIndicator$indicator

# Break the indicator code up into 4 distinct columns. 
resultsGoodIndicator = as.data.frame(resultsGoodIndicator) %>% separate(indicator,c("topic","general","specific","extension"))

#plot the topic column in a bar chart to see the frequency of each topic.
#compare the expenditure (NE) and the income (NY)
resultsGoodIndicator %>% ggplot(aes(x = topic, fill = topic)) + geom_bar()



#Goal 2: Plot GDP (NY and GDP) per capita (PCAP) of Mexico, Canada and the US in constant US dollars (KD)

dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','CA','US'), start=1960, end=2012)

head(dat)

library(ggplot2)
ggplot(dat, aes(x = year, y = NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')



### NYC City Data

## NYC
#Example From:
#https://rpubs.com/bkostadi/data_analysis_cite2022

#RCode
library(RSocrata) # for loading the data from NYC Open Data
library(tidyverse) # for data analysis and visualizations
library(ggcharts) # for easy visualizations based on ggplot2
library(ggblanket) # for easy visualizations based on ggplot2
library(knitr) # for printing tables
library(DT) # for interactive tables in html format

# import the data directly into RStudio using url path
data <- read.socrata("https://data.cityofnewyork.us/resource/f6s7-vytj.csv")

# selected variables (columns)
columns <-c("district","name","borough","latitude","longitude",
            "coursepassrate","elaprof","mathprof","surveysafety",
            "totalstudents","gradespan","tophs1","tophs2","tophs3",
            "acceleratedclasses","electiveclasses","languageclasses",
            "diversityinadmissions")
mydata <- data |> 
  select(all_of(columns))


mydata %>% select(1:3) %>% slice(1:7) %>% kable()
  
#Check the Viewer
mydata %>% filter(borough == "MANHATTAN") %>% select(c(name,coursepassrate,mathprof,elaprof)) %>% arrange(desc(mathprof)) %>% slice(1:10) %>% kable()

#Check the Viewer
mydata %>% filter(district == 13 | district == 2) %>% select(c(district,name,mathprof)) %>% arrange(desc(mathprof)) %>% datatable() # for html only

bar_chart(district_stats, x=district, y=med_mathprof)

district_stats = mydata %>% group_by(district) %>% summarize(med_mathprof = median(mathprof, na.rm=TRUE), avg_mathprof = mean(mathprof, na.rm=TRUE)) 

district_stats %>% arrange(desc(med_mathprof)) %>% datatable()

bar_chart(district_stats, x=district, y=med_mathprof)


#Dallas 

Dallas_Crime = read.csv("https://www.dallasopendata.com/resource/pumt-d92b.csv")
Dallas_Crime

# Calculate the frequency of each level
freq_table <- table(Dallas_Crime$offensedescription)

# Sort the frequencies in descending order and extract the top 10
top_10_crime <- sort(freq_table, decreasing = TRUE)[1:10]

# Print the top 10 frequencies
print(top_10_crime)

# Convert the top 10 frequencies to a data frame
top_10_crime_df <- as.data.frame(top_10_crime)
colnames(top_10_crime_df) <- c("Level", "Frequency")

#Bar chart
ggplot(top_10_crime_df, aes(x = reorder(Level, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Most Frequent Crimes",
       x = "Level",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






#API and json code

######################

# Loading the Data from the NYT API

######################

library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(tidyverse)

NYTIMES_KEY = "OG89fUubcS8FXofVrLA4dmIOHh5omiFa" #Your Key Here … get from NYT API website

# Let's set some parameters
term <- "data+science" # Need to use + to string together separate words
begin_date <- "20230119"
end_date <- "20230202"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

baseurl

initialQuery <- jsonlite::fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)

pages <- list()
for(i in 0:maxPages){
  nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(9) 
}

allNYTSearch <- rbind_pages(pages)


#Segmentation

allNYTSearch %>% 
  ggplot() +
  geom_bar(aes(x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "count") + coord_flip()



# Visualize coverage by section
allNYTSearch %>% 
  group_by(response.docs.type_of_material) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()





# OLD twitteR package and API

api_key = "rkclWXRZYkZYZbdVdcvzP2ZcN "
api_secret = "ymjMYAkXhXVAL2ci4vTKi3ZFKg72abSKlzBNZq0y6rkXXltsdY"
access_token = "1105487041691815937-IIPDKMmlfGIuRvJgrRfCgiRLtQAfII"
access_token_secret = "mafeLvPRrI8SKBvyq4SJVozfx2wDD0rRkOrASfCoRJUyy"

#Load twitteR
library(twitteR)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Get tweets
tweets = searchTwitter("$appl", n = 10, lang = "en")

#Locations 
trend = availableTrendLocations()

#Get Trends for Location
getTrends(395269) # Caracas, Venezuela

getTrends(2487889) # San Diego, California

getTrends(44418) #  London, England

getTrends(2388929) #  Dallas, US

DallasTrends = getTrends(2388929) %>% select(name) #  Dallas, US

DallasTrends[1:10,]











#Open AI Example

library(httr)
library(jsonlite)

url <- "https://api.openai.com/v1/chat/completions"

response <- POST(
  url,
  add_headers(Authorization = paste("Bearer", 
                                    ""),
              `Content-Type` = "application/json"),
  body = toJSON(list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "system", content = "You are a helpful assistant."),
      list(role = "user", content = "What is your favorite R package?")
    )
  ), auto_unbox = TRUE)
)

result <- content(response, "parsed")
cat(result$choices[[1]]$message$content)










# Much Safer Example

Sys.setenv(OPENAI_API_KEY = "your_api_key_here")


library(httr)
library(jsonlite)

url <- "https://api.openai.com/v1/chat/completions"

response <- POST(
  url,
  add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
              `Content-Type` = "application/json"),
  body = toJSON(list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "system", content = "You are a helpful assistant."),
      list(role = "user", content = "Write an R function to calculate factorial.")
    )
  ), auto_unbox = TRUE)
)

result <- content(response, "parsed")
cat(result$choices[[1]]$message$content)


