#api practice

library(httr2)
# library(tibble)
# library(dplyr)
# library(ggplot2)
library(tidyverse) #includes tibble, dplyr, ggplot2, tidyr, readr, stringr, ...

# Make an API GET request to sbdb_query.api

fields <- "rms,last_obs"
limit <- 100 # limit the number of records returned

resp <- request("https://ssd-api.jpl.nasa.gov/sbdb_query.api") |>
  req_url_query(fields = fields, limit = limit) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()
resp$status_code # 200 is good, 400 is a bad request

# parse json response into data object
data <- resp_body_json(resp)
str(data, max.level = 2)

# Convert to data frame, actually a tibble

rms_df <- tibble(
  rms = as.numeric(unlist(data$data))
)
# head(rms_df)
# summarize(rms_df)
data$fields
# Assign column names
colnames(rms_df) <- data$fields

# Convert RMS to numeric
rms_df$rms <- as.numeric(rms_df$rms)

# Check
# head(rms_df)
str(rms_df)

# plot(rms_df)
ggplot(rms_df, aes(x = rms)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Distribution of Orbit RMS values of small bodies", 
       x = "RMS", 
       y = "Count") +
  theme_minimal()

# Use a log scale to compress the long tail
ggplot(rms_df, aes(x = rms)) +
  geom_histogram(fill = "blue", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of RMS values (log scale)", x = "RMS (log10)", y = "Count") +
  theme_minimal()



