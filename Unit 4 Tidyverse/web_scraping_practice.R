#unit 4 practice
library(xml2)
# library(XML)

# ingest the xml document
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- read_xml(url)

# rootNode <- xml_root(doc)
# rootNode
# rootNode[[1]][[1]]
# xmlSApply(rootNode, "//name", xmlValue)

# Extract the nodes name, zipcode, councildistrict -- found using view page source
names <- xml_text(xml_find_all(doc, "//name"))
zipcodes <- xml_text(xml_find_all(doc, "//zipcode")) #so large it doesn't output to the console
council_districts <- xml_text(xml_find_all(doc, "//councildistrict")) #too large to output to console

# Create a data frame
restaurants = data.frame(names, zipcodes, council_districts)
head(restaurants)

# Are there any sushi restaurants in Baltimore? all the data is in Baltimore
# NOTE the string to search is case-sensitive
sushi_df <- restaurants %>%
  filter(grepl("sushi", names), ignore.case = TRUE)

length(sushi_df$names)

# downtown sushi restaurants
downtown_restaurants <- filter(restaurants, council_districts == "11")
sushi_downtown <- downtown_restaurants %>%
  filter(grepl("SUSHI", names))
length(sushi_downtown$names)

# Make a barplot of the estimated number of restaurants (Sushi or otherwise) in each council.


# Gandrud notes
# csv is a plain-text format, xlsx is not
# TestData <- read.table("TestData.csv", sep = ",", header = TRUE)


