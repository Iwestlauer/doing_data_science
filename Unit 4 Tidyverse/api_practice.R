#api practice

library(httr2)
library(tidyverse) #includes tibble, dplyr, ggplot2, tidyr, readr, stringr, ...

# Make an API GET request to sbdb_query.api
fields <- "spkid,rms,last_obs"
limit <- 100 # limit the number of records returned

resp <- request("https://ssd-api.jpl.nasa.gov/sbdb_query.api") |>
  req_url_query(fields = fields, limit = limit) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()
resp$status_code # 200 is good, 400 is a bad request

# parse json response into data object
data <- resp_body_json(resp)
str(data$fields, max.level = 2)

# Convert to data frame, actually a tibble
rms_df <- as_tibble(
  do.call(rbind, data$data),
  .name_repair = "minimal"
)

# Assign column names
colnames(rms_df) <- data$fields

rms_df <- rms_df |>
  mutate(
    spkid = unlist(spkid),
    rms = unlist(rms),
    last_obs = unlist(last_obs)
  )

# peek at last_obs
unique(rms_df$last_obs)[1:10]

# change the data types, replace N/A with blank
rms_df <- rms_df |>
  mutate(
    spkid = as.character(spkid),
    rms = as.numeric(rms),
    last_obs = na_if(last_obs, ""),
    last_obs = as.Date(last_obs, format = "%Y-%m-%d")
  )

# inspect
head(rms_df)

# export to csv
write.csv(rms_df, file = "output.csv", row.names = FALSE)

# Check
# head(rms_df)
str(rms_df)

ggplot(rms_df, aes(x = rms)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Distribution of Orbit RMS values of Small Bodies",
       x = "RMS",
       y = "Count") +
  theme_minimal()

# Use a log scale to compress the long tail
ggplot(rms_df, aes(x = rms)) +
  geom_histogram(fill = "blue", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of RMS values (log scale)", x = "RMS (log10)", y = "Count") +
  theme_minimal()



