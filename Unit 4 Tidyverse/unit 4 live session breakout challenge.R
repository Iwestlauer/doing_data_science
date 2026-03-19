
#Make a single data frame that holds all players and clearly
#shows their relevant stats. This may be challenging for
#goalies since they have the glove hand and no other players
#except for the goalie has that type of statistic


# library(XML) #xml_Parse
library(xml2)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML
url = "http://www.espn.com/nhl/team/roster/_/name/dal/dallas-stars.html"
page <- read_html(url)
page
starstable <- html_nodes(page, "table")
starstable

# Dr. Sadler's solution:
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


