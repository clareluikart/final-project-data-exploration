#libraries used
library(here)
library(sf)
library(tmap)
library(sp)
library(janitor)
library(dplyr)
library(ggplot2)
library(terra)
library(tidycensus)

datafile <- read.csv(file = here("DECENNIALPL2020.P1-Data.csv"), header = TRUE, sep = ",")
datafile_labels <- datafile[1,]
datafile <- datafile %>%
  clean_names(.)%>%
  filter(row_number() != 1)
datafile_labels <- clean_names(datafile_labels)
datafile$geo_id <- sub("^1000000US", "", datafile$geo_id)
datafile$geo_id <- substr(datafile$geo_id, 1, nchar(datafile$geo_id) - 3)
natl_sf <- st_read(here("cb_2020_06_bg_500k.shp")) #census file uses NAD83
natl_sf <- clean_names(natl_sf)

tm_shape(natl_sf) + tm_polygons(alpha = 0, border.col = "black")


merged_data <- inner_join(natl_sf, datafile, by = c("geoid" ="geo_id"))

tm_shape(merged_data) + tm_polygons(alpha = 0, border.col = "black")

#need to remove farallon islands and piece off alameda, treasure island and alcatraz
# Create bounding boxes and convert to polygons—help from chatgpt to figure out how to create exclusion zones
# help from bboxfinder.com to get coordinates
farallon_box <- st_as_sfc(st_bbox(c(xmin = -123.0165, ymin = 37.6927, xmax = -122.9945, ymax = 37.7068), crs = st_crs(merged_data)))
treasure_box <- st_as_sfc(st_bbox(c(xmin = -122.3819, ymin = 37.8061, xmax = -122.3576, ymax = 37.8352), crs = st_crs(merged_data)))
alcatraz_box <- st_as_sfc(st_bbox(c(xmin = -122.4296, ymin = 37.8238, xmax = -122.4156, ymax = 37.8325), crs = st_crs(merged_data)))
alameda_part <- st_as_sfc(st_bbox(c(xmin = -122.3406, ymin = 37.7731, xmax = -122.3117, ymax = 37.8023), crs = st_crs(merged_data)))

# Combine exclusion polygons into a single geometry
exclusion_zones <- st_union(c(farallon_box, treasure_box, alcatraz_box, alameda_part))

# Remove census blocks within exclusion zones
san_fran_sf <- merged_data %>%
  filter(!st_intersects(geometry, exclusion_zones, sparse = FALSE))

san_fran_race_sf <- san_fran_sf%>%
  select(13:18, 21:36)

tm_shape(san_fran_sf) + tm_polygons(alpha = 0, border.col = "black")

san_fran_dots <- as_dot_density(
  san_fran_race_sf,
  value="value",
  values_per_dot = 1,
  group = "variable"
)

# Identify variables for mapping
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Asian = "P2_008N"
)

# Get data from tidycensus
san_fran_race <- get_decennial(
  geography = "block",
  variables = race_vars,
  state = "CA",
  county = "San Francisco",
  geometry = TRUE,
  year = 2020
)

# Convert data to dots
san_fran_dots <- as_dot_density(
  san_fran_race,
  value = "value",
  values_per_dot = 1,
  group = "variable"
)

# Use one set of polygon geometries as a base layer
san_fran_base <- san_fran_race[san_fran_race$variable == "Hispanic", ]

# Map with ggplot2
ggplot() +
  geom_sf(data = san_fran_sf,
          fill = "white",
          color = "grey") +
  geom_sf(data = san_fran_dots,
          aes(color = variable),
          size = 0.0001) +
  theme_void()

# # Identify variables for mapping
# race_vars <- c(
#   Hispanic = "P2_002N",
#   White = "P2_005N",
#   Black = "P2_006N",
#   Asian = "P2_008N"
# )
# 
# # Get data from tidycensus
# alameda_race <- get_decennial(
#   geography = "block",
#   variables = race_vars,
#   state = "CA",
#   county = "Alameda",
#   geometry = TRUE,
#   year = 2020
# )
# alameda_race <- san_fran_race
# # Convert data to dots
# alameda_dots <- as_dot_density(
#   alameda_race,
#   value = "value",
#   values_per_dot = 5,
#   group = "variable"
# )
# 
# # Use one set of polygon geometries as a base layer
# alameda_base <- alameda_race[alameda_race$variable == "Hispanic", ]
# 
# # Map with ggplot2
# ggplot() +
#   geom_sf(data = alameda_base,
#           fill = "white",
#           color = "grey") +
#   geom_sf(data = alameda_dots,
#           aes(color = variable),
#           size = 0.0001) +
#   theme_void()

# tree data
tree_datafile <- read.csv(file = here("Street_Tree_List_20241125.csv"), header = TRUE, sep = ",")%>%
  clean_names(.)
tree_na_datafile <-  tree_datafile %>%
  filter(is.na(latitude))%>%
  mutate(q_address = ifelse(q_address == "", NA, q_address))%>%
  mutate(permit_notes = ifelse(permit_notes == "", NA, permit_notes))
tree_na_address <- tree_na_datafile %>%
  filter(is.na(q_address) & !is.na(permit_notes))
tree_na_address_permit <- tree_na_datafile %>%
  filter(is.na(q_address) & is.na(permit_notes))
tree_address <- tree_na_datafile %>%
  filter(!is.na(q_address)) %>%
  mutate(
    q_address = q_address %>%
      # Step 1: Remove 'x' that comes after a number and is followed by a space
      gsub(pattern = "(\\d)x ", replacement = "\\1 ", .) %>%
      gsub(pattern = "(\\d)X ", replacement = "\\1 ", .) %>%
      # Step 2: Remove all special characters that are not letters or numbers
      gsub(pattern = "[^a-zA-Z0-9 ]", replacement = "", .)
  )
api_request <- tree_address %>%
  select(tree_id, q_address)%>%
  rename(., id=tree_id, address=q_address)%>%
  mutate(., city="San Francisco")%>%
  mutate(., state="CA")%>%
  mutate(., zip="")
names(api_request) <- NULL

example <- read.csv(file = here("Addresses copy.csv"), sep = ",")
write.csv(api_request, file = "addresses.csv", row.names = FALSE)

install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# Set the API endpoint
batch_url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

# File paths
input_file <- here("addresses.csv")  # Your input CSV
output_file <- here("geocoded_results.csv")  # Output file to save results

# Upload the file to the Census Batch Geocoder
response <- POST(
  url = batch_url,
  body = list(
    addressFile = upload_file(input_file),  # Upload the input file
    benchmark = "Public_AR_Current"         # Specify benchmark
  ),
  encode = "multipart"
)

# Save the response content as a file
if (status_code(response) == 200) {
  writeBin(content(response, "raw"), output_file)
  message("Batch geocoding completed. Results saved to: ", output_file)
} else {
  stop("Error in batch geocoding: ", status_code(response))
}

tree_datafile <- tree_datafile %>%
  filter(!is.na(latitude))
  

tree_sf <- st_as_sf(tree_datafile, coords = c("longitude", "latitude"), crs = 4326)

ggplot(data = tree_sf) +
  geom_sf(color = "green", size = 0.5) +
  coord_sf() +  # Ensures correct map projection
  theme_minimal() +
  ggtitle("San Francisco Street Trees - Green Dot Map")