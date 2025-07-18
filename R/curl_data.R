library(httr2)
library(jsonlite)

project_id <- "<TO BE ADDED>"
anon_key <- "<ANON_KEY>"

# Supabase details
supabase_url <- paste("https://", PROJECT_ID, ".supabase.co", sep = "")


# Select table
table <- "stats19_accidents"

# How many rows does the table have

req <- request(paste0(supabase_url, "/rest/v1/", table)) %>%
  req_headers(
    `apikey` = anon_key,
    `Authorization` = paste("Bearer", anon_key),
    `Accept` = "application/json",
    `Prefer` = "count=exact",
    `Range-Unit` = "items",
    `Range` = "0-0" # Only request the first row
  ) %>%
  req_url_query(select = "accident_index") # or any column

resp <- req_perform(req)

# Extract total count from Content-Range header
count_header <- resp_headers(resp)[["content-range"]]
total_count <- as.numeric(sub(".*/", "", count_header))

print(paste("Total rows:", total_count))

# Partial download of table (can only downoad 1000 rows at a time,
# Note, uses 0 based indexing)
req <- request(paste0(supabase_url, "/rest/v1/", table)) %>%
  req_headers(
    `apikey` = anon_key,
    `Authorization` = paste("Bearer", anon_key),
    `Accept` = "application/json",
    `Range-Unit` = "items",
    `Range` = "0-9"
  ) %>%
  req_url_query(select = "*") # selects all columns

# Perform request
resp <- req_perform(req)

# Parse the JSON into a data frame
data <- fromJSON(resp_body_string(resp))
