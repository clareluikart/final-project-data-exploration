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


### Understanding Census Stuff
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

san_fran_treefilter_sf <- san_fran_sf_neighborhoods%>%
  st_union(.)%>%
  st_transform(., crs=4326)

contained_trees <- tree_sf%>%
  st_contains(., san_fran_treefilter_sf, sparse=TRUE)
  
# Filter points that are within the San Francisco multipolygon
contained_trees <- tree_sf[st_within(tree_sf, san_fran_treefilter_sf, sparse = FALSE), ]
points_outside <- tree_sf[!st_within(tree_sf, san_fran_treefilter_sf, sparse = FALSE), ]

plot(san_fran_treefilter_sf)

ggplot(data = contained_trees) +
  geom_sf(color = "green", size = 0.1) +
  geom_sf(data = san_fran_treefilter_sf, fill = NA, color = "blue", size = 1) +
  coord_sf() +  # Ensures correct map projection
  theme_minimal() +
  ggtitle("San Francisco Street Trees - Green Dot Map")

landtemp <- read.csv(file = here("edsc_collection_results_export.csv"), header = TRUE, sep = ",")


#NOTE: Be aware that any reprojection of data from its source projection to a different projection will inherently change the data from its original format. All reprojections use GDAL's gdalwarp function in combination with the PROJ string listed above. For additional information, see the AρρEEARS help documentation .

# Load necessary libraries
library(terra)  # For working with raster data
library(ggplot2)  # For plotting
library(gridExtra)  # For arranging plots in a grid

# File paths to your .tif files
file1 <- "ECO_L2T_LSTE.002_LST_doy2024277191514_aid0001_10N.tif"
file2 <- "ECO_L2T_LSTE.002_LST_doy2024278014456_aid0001_10N.tif"
file3 <- "ECO_L2T_LSTE.002_LST_doy2024278182656_aid0001_10N.tif"
file4 <- "ECO_L2T_LSTE.002_LST_doy2024281005904_aid0001_10N.tif"

# Load the raster files
rast1 <- rast(file1)
rast2 <- rast(file2)
rast3 <- rast(file3)
rast4 <- rast(file4)

# Convert raster files to data frames for ggplot
rast_to_df <- function(rast) {
  df <- as.data.frame(rast, xy = TRUE, na.rm = TRUE)  # Extract values and coordinates
  colnames(df)[3] <- "value"  # Rename the raster value column to "value"
  return(df)
}

#df1 <- rast_to_df(rast1)
df2 <- rast_to_df(rast2)
df3 <- rast_to_df(rast3)
#df4 <- rast_to_df(rast4)

# Define a plotting function
plot_raster <- function(data, title) {
  ggplot(data) +
    geom_tile(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "plasma", name = "Temperature (K)") +
    theme_minimal() +
    coord_fixed() +
    labs(title = title, x = "Longitude", y = "Latitude")
}

# Create individual plots
#plot1 <- plot_raster(df1, "LST 1")
plot2 <- plot_raster(df2, "LST 2")
plot3 <- plot_raster(df3, "LST 3")
#plot4 <- plot_raster(df4, "LST 4")

# Arrange the plots in a 2x2 grid
#grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
grid.arrange(plot2, plot3, ncol = 2)

# Load necessary libraries
library(terra)
library(ggplot2)
library(viridis)

### This is for October lansat
# Read in the raster file
rastTempe <- rast(here::here("lansat/LC08_L2SP_044034_20241007_20241018_02_T1_ST_B10.TIF"))

# Convert raster files to data frames for ggplot
rast_to_df <- function(rastTempe) {
  df <- as.data.frame(rastTempe, xy = TRUE, na.rm = TRUE)  # Extract values and coordinates
  colnames(df)[3] <- "value"  # Rename the raster value column to "value"
  return(df)  # Ensure the data frame is returned
}

# Convert the raster to a data frame
dfTempe <- rast_to_df(rastTempe)

# Define a plotting function
plot_raster <- function(data, title) {
  ggplot(data) +
    geom_tile(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "plasma", name = "Temperature (K)") +
    theme_minimal() +
    coord_fixed() +
    labs(title = title, x = "Longitude", y = "Latitude")
}

# Create the plot with the correct data
plotTempe <- plot_raster(dfTempe, "LST 2")
plot(plotTempe)

# Load necessary libraries
library(raster)
library(sf)
library(tmap)
library(here)

###
# Read in the temperature raster (ensure that the file path is correct)
temp <- raster(here::here("lansat/LC08_L2SP_044034_20241007_20241018_02_T1_ST_B10.TIF"))

# https://geodata.lib.berkeley.edu/catalog/ark28722-s7tw2m
san_fran_sf <- read_sf("sf/sfoutline.shp")%>%
  st_transform(crs = crs(temp))
  
# Function to keep only the holes (removing the exterior)
polygons_with_only_holes <- lapply(st_geometry(multipolygon_with_holes), function(polygon) {
  # Check if the polygon has more than one ring (i.e., it has a hole)
  if (length(polygon[[1]]) > 1) {  # polygon[[1]] gives the list of rings for the polygon
    # Extract the holes, which are all rings except the first (exterior)
    holes <- polygon[[1]][-1]
    
    # Create a new polygon consisting of only the holes
    return(st_polygon(holes))
  } else {
    # If there are no holes, return NULL or an empty geometry (since no holes exist)
    return(NULL)
  }
})

# Filter out any NULL values (if some polygons didn't have holes)
polygons_with_only_holes <- Filter(Negate(is.null), polygons_with_only_holes)

# Convert the list back into an sf object
holes_sf <- st_sfc(polygons_with_only_holes)
st_crs(holes_sf) <- st_crs(san_fran_sf)


# we are taking the extent of california from shapefile
sf_extent <- extent(holes_sp)
# we are using the extent cropping the US raster down to California  
sf_cropped <- crop(temp, sf_extent)
# we are now masking the grids to the boundary of california
sf_masked <- mask(sf_cropped, holes_sp)

#https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1619_Landsat-8-9-C2-L2-ScienceProductGuide-v4.pdf
# DATE_ACQUIRED = 2024-10-07
# SCENE_CENTER_TIME = "18:45:56.6687720Z" sunset at 6:43
sf_cropped_celsius <- (sf_cropped * 0.00341802)+-124.15
sf_cropped_celsius_masked<- mask(sf_cropped_celsius, san_fran_sf_mask)

tm_shape(sf_cropped_celsius_masked) + tm_raster(style = "cont", title = "Surface temperature October 7, C", palette= "-Spectral")


# Read in the temperature raster (ensure that the file path is correct)
tempJuly <- raster(here::here("lansatJuly/LC08_L2SP_044034_20240703_20240711_02_T1_ST_B10.TIF"))
# we are using the extent cropping the US raster down to California  
sf_cropped_July <- crop(tempJuly, sf_extent)
# we are now masking the grids to the boundary of california
sf_masked <- mask(sf_cropped, holes_sp)


#### This is getting Lansat for July
#https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1619_Landsat-8-9-C2-L2-ScienceProductGuide-v4.pdf
# DATE_ACQUIRED = 2024-07-03
# SCENE_CENTER_TIME = "18:45:23.1096400Z" sunset at 8:30
sf_cropped_celsius_July <- (sf_cropped_July * 0.00341802)+-124.15
sf_cropped_celsius_July_masked<- mask(sf_cropped_celsius_July, san_fran_sf_mask)
# scale factor is found here: https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
tm_shape(sf_cropped_celsius_July_masked) + tm_raster(style = "cont", title = "Surface temperature July 3, C", palette= "-Spectral")

#https://geodata.lib.berkeley.edu/catalog/berkeley-s7s68p
san_fran_sf_neighborhoods <- read_sf("sf_neighborhoods/SanFrancisco_neighborhoods_2013.shp")

sf_average <- (sf_cropped_celsius_July + sf_cropped_celsius)/2
san_fran_sf_mask <- san_fran_sf_neighborhoods%>%
  st_union(.)%>%
  st_transform(., crs="+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")%>%
  as(., "Spatial") #maybe problem with 2 small holes in mask
sf_average_masked <- mask(sf_average, san_fran_sf_mask)
tm_shape(sf_average_masked) + tm_raster(style = "cont", title = "Surface temperature, C", palette= "-Spectral")+
  tm_shape(san_fran_sf_neighborhoods)+tm_polygons(alpha = 0, border.col = "black")










#### NDVI at too high of a resolution (because from lansat)
# Read in the temperature raster (ensure that the file path is correct)
infraredband <- raster(here::here("NVDI_calc/LC08_L2SP_044034_20240703_20240711_02_T1_SR_B5.TIF"))
redband <- raster(here::here("NVDI_calc/LC08_L2SP_044034_20240703_20240711_02_T1_SR_B4.TIF"))
# we are using the extent cropping the US raster down to California  
sf_cropped_redband <- crop(redband, sf_extent)
sf_cropped_infraredband <- crop(infraredband, sf_extent)

nvdi <- (sf_cropped_infraredband-sf_cropped_redband)/(sf_cropped_infraredband+sf_cropped_redband)
nvdi_masked<- mask(nvdi, san_fran_sf_mask)

reclass_matrix <- matrix(c(-Inf, 0.2, 0,  # <= 0.2 becomes 0
                           0.2, Inf, 1),  # > 0.2 becomes 1
                         ncol = 3, byrow = TRUE)

# Apply the reclassification
temp_reclassified <- reclassify(nvdi_masked, reclass_matrix)

tm_shape(nvdi_masked) + tm_raster(style = "cont", title = "NVDI", palette= "Greens")+
  tm_shape(san_fran_sf_neighborhoods)+tm_polygons(alpha = 0, border.col = "black")

tm_shape(temp_reclassified) + tm_raster(style = "cat", title = "NVDI", palette= c("lightgrey", "green"))+
  tm_shape(san_fran_sf_neighborhoods)+tm_polygons(alpha = 0, border.col = "black")

# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Elevation/1m/Projects/CA_SanFrancisco_B23/TIFF/
# Load the necessary libraries
library(raster)

### USGS Elevation (not the correct one)
# Load the raster files
#https://www.sciencebase.gov/catalog/item/66ce871ad34e98e8a92453cb
#https://www.sciencebase.gov/catalog/item/66ce871dd34e98e8a92453d1
#https://www.sciencebase.gov/catalog/item/66ce8720d34e98e8a92453d5
#https://www.sciencebase.gov/catalog/item/66ce8722d34e98e8a92453dc
raster1 <- raster("usgsb23/USGS_1M_10_x54y418_CA_SanFrancisco_B23.tif")
raster2 <- raster("usgsb23/USGS_1M_10_x54y419_CA_SanFrancisco_B23.tif")
raster3 <- raster("usgsb23/USGS_1M_10_x55y418_CA_SanFrancisco_B23.tif")
raster4 <- raster("usgsb23/USGS_1M_10_x55y419_CA_SanFrancisco_B23.tif")

san_fran_sf_mask2 <- san_fran_sf_neighborhoods%>%
  st_union(.)%>%
  st_transform(., crs="+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")%>%
  as(., "Spatial")

extent_san <- extent(san_fran_sf_mask2)

# Extend each raster to match the common extent, filling with NA outside
raster1_cropped <- crop(raster1, extent_san)
raster2_cropped <- crop(raster2, extent_san)
raster3_cropped <- crop(raster3, extent_san)
raster4_cropped <- crop(raster4, extent_san)

# Extend each raster to match the common extent, filling with NA outside
raster1_extended <- extend(raster1_cropped, san_fran_sf_mask2)
raster2_extended <- extend(raster2_cropped, san_fran_sf_mask2)
raster3_extended <- extend(raster3_cropped, san_fran_sf_mask2)
raster4_extended <- extend(raster4_cropped, san_fran_sf_mask2)

# Optionally, check that NA values are being applied
# Plot the extended raster
plot(raster1_extended)

# Now you can resample them if necessary, and stack them together
raster_stack <- stack(raster1_extended, raster2_extended, raster3_extended, raster4_extended)

# Plot the stacked raster
plot(raster_stack)


# Save the stacked raster to a new file if needed
writeRaster(raster_stack, "usgsb23/combined_raster.tif", format = "GTiff")

mosaic_raster <- mosaic(raster1_extended,raster2_extended,raster3_extended,raster4_extended, fun=mean)
writeRaster(mosaic_raster, "mosaic_raster.tif", format = "GTiff")
combined_raster <- raster(here("usgsb23/combined_raster.tif"))
tm_shape(mosaic_raster) + tm_raster(style = "cont", title = "elevation", palette= "Spectral", midpoint=NA)



library(terra)

# List all chunk files
chunk_files <- list.files(path = "python_test/output_tifs/", pattern = "chunk_.*\\.tif$", full.names = TRUE)

# Load and mosaic the rasters
rasters <- lapply(chunk_files, rast)
new_raster <- do.call(terra::mosaic, c(rasters, fun = "mean"))

# Save the final mosaic
writeRaster(new_raster, "san_francisco_dsm.tif", overwrite = TRUE)


##### this is correct dsm raster
dsm_raster <- raster(here("san_francisco_dsm.tif"))
plot(dsm_raster, main = "Digital Surface Model (1m Resolution)")

projected_dsm_raster <- projectRaster(dsm_raster, crs = raster::crs(combined_raster), res=1)
plot(projected_dsm_raster, main = "Digital Surface Model (1m Resolution)")

combined_raster_test <- crop(combined_raster, extent(projected_dsm_raster))
projected_dsm_raster_test <- crop(projected_dsm_raster, extent(combined_raster_test))

aligned_dsm_raster <- setExtent(projected_dsm_raster_test, combined_raster_test, keepres = TRUE)

ndsm <- aligned_dsm_raster - combined_raster_test

plot(ndsm, main="nDSM")

crop_extent <- extent(545000, 550000, 4176000, 4180000)

# Crop the nDSM raster
cropped_ndsm <- crop(ndsm, crop_extent)

# Check value distribution
hist(cropped_ndsm[], breaks = 50, main = "nDSM Value Distribution", xlab = "Height (meters)")

# Convert 5 feet to meters
breaks <- seq(-7, 23.48, by = 1.524)  # Up to 30 feet (~9 meters)

# Define colors: white to blue, then red
colors <- c("white", "lightblue", "blue", "lightgreen", "yellow", "orange", "red")

# Plot the raster with defined breaks and colors
plot(cropped_ndsm,
     breaks = breaks,
     col = colors,
     main = "nDSM Visualization with 5-Foot Breaks",
     xlab = "Longitude (meters, UTM Zone 10N)",
     ylab = "Latitude (meters, UTM Zone 10N)")

# Add legend
legend("topright",
       legend = c("0-5 ft", "5-10 ft", "10-15 ft", "15-20 ft", "20-25 ft", "25-30 ft", ">30 ft"),
       fill = colors,
       title = "Height Categories (feet)")

# Plot the cropped raster
plot(cropped_ndsm)



#### this is the correct ndsm

# Define the folder containing the .tif files
input_folder <- "nDSM_stuff/ca2023_sanfran_dem_Job1102345/"

# List all the .tif files in the directory that match the pattern
tif_files <- list.files(input_folder, pattern = "ca2023_sanfran_dem_J1102345_\\d{3}_\\d{3}.tif$", full.names = TRUE)

for (tif_file in tif_files) {
  # Load the current .tif file
  current_raster <- raster(tif_file)
  
  # Define the target resolution (1 meter)
  target_resolution <- 1
  
  # Create a template raster with the target resolution
  target_raster <- raster(current_raster)
  res(target_raster) <- target_resolution
  
  # Resample to 1-meter resolution using bilinear interpolation
  resampled_raster <- resample(current_raster, target_raster, method = "bilinear")
  
  # Define the output file name (same name but with a '_1m' suffix)
  output_file <- gsub(".tif$", "_1m.tif", basename(tif_file))
  output_path <- file.path(input_folder, output_file)
  
  # Save the resampled raster
  writeRaster(resampled_raster, output_path, format = "GTiff", overwrite = TRUE)
  
  # Print a message to show progress
  cat("Resampled and saved:", output_file, "\n")
}

# Print completion message
cat("All TIFF files have been resampled to 1-meter resolution.")


#### This is the correct nDSM
# Define the folder containing the resampled .tif files
input_folder <- "nDSM_stuff/ca2023_sanfran_dem_Job1102345/"

# List all the resampled .tif files in the directory that match the pattern
tif_files_1m <- list.files(input_folder, pattern = "ca2023_sanfran_dem_J1102345_\\d{3}_\\d{3}_1m.tif$", full.names = TRUE)

# Load all the rasters into a list
rasters_list <- lapply(tif_files_1m, raster)

# Initialize variables to store min and max values
xmin_all <- Inf
xmax_all <- -Inf
ymin_all <- Inf
ymax_all <- -Inf

# Loop through each raster and update the extent values
for (tif_file in tif_files) {
  r <- raster(tif_file)
  raster_extent <- extent(r)
  
  # Update the minimum and maximum values
  xmin_all <- min(xmin_all, raster_extent[1])
  xmax_all <- max(xmax_all, raster_extent[2])
  ymin_all <- min(ymin_all, raster_extent[3])
  ymax_all <- max(ymax_all, raster_extent[4])
}

# Create a unified extent based on the min and max values
unified_extent <- extent(xmin_all, xmax_all, ymin_all, ymax_all)

# Print the unified extent
cat("Unified extent is:\n")
cat("xmin:", xmin_all, "xmax:", xmax_all, "ymin:", ymin_all, "ymax:", ymax_all, "\n")

# Create a reference raster with the unified extent
reference_raster <- raster(unified_extent, crs = "+proj=lcc +lat_0=36.5 +lon_0=-120.5 +lat_1=38.4333333333333 +lat_2=37.0666666666667 +x_0=2000000.0001016 +y_0=500000.0001016 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")

# Set the resolution of the reference raster (1 meter resolution)
res(reference_raster) <- 1  # Set resolution to 1 meter

# Loop through each raster file and resample it to the unified extent and 1m resolution
rasters_list <- lapply(tif_files_1m, function(tif_file) {
  r <- raster(tif_file)
  # Resample the raster to the unified extent and resolution
  r_resampled <- resample(r, reference_raster, method = "bilinear")
  return(r_resampled)
})

# Mosaic the resampled rasters together
mosaicked_raster <- do.call(mosaic, c(rasters_list, fun = "mean"))

# Save the mosaicked raster
mosaic_output_file <- file.path(input_folder, "mosaicked_raster_1m.tif")
writeRaster(mosaicked_raster, mosaic_output_file, format = "GTiff", overwrite = TRUE)

# Print a message indicating success
cat("Mosaic completed and saved as", mosaic_output_file, "\n")
plot(mosaicked_raster, main = "NOAA DEM (1m Resolution)")

projected_mosaicked_raster <- projectRaster(mosaicked_raster, crs = raster::crs(dsm_raster), res=1)

projected_mosaicked_raster <- projected_mosaicked_raster * 0.3048 #changing it to feet

projected_mosaicked_raster_test <- crop(projected_mosaicked_raster, extent(dsm_raster))
dsm_raster_test <- crop(dsm_raster, extent(projected_mosaicked_raster_test))

noaa_aligned_dsm_raster <- setExtent(dsm_raster_test, projected_mosaicked_raster_test, keepres = TRUE)

noaa_ndsm <- noaa_aligned_dsm_raster - projected_mosaicked_raster_test

writeRaster(noaa_ndsm, "nDSM_1m.tif", format = "GTiff", overwrite = TRUE)

#### this is the correct ndsm, which I should have exported when I had the chance!!

plot(noaa_ndsm, main="Big test")

feet_to_meters <- function(feet) feet * 0.3048
threshold_1 <- feet_to_meters(1)   # 1 foot to meters
threshold_8 <- feet_to_meters(8)   # 8 feet to meters



# Reclassify raster
reclass_matrix <- matrix(c(
  -Inf, threshold_1, 1,   # Less than 1 foot -> class 1
  threshold_1, threshold_8, 2,   # 1-8 feet -> class 2
  threshold_8, Inf, 3     # Greater than 8 feet -> class 3
), ncol = 3, byrow = TRUE)

reclassified_raster <- reclassify(noaa_ndsm, reclass_matrix)

# Define colors for the classes: yellow (class 1), blue (class 2), green (class 3)
colors <- c("yellow", "green", "blue")

# Plot the reclassified raster
plot(reclassified_raster, 
     col = colors, 
     legend = TRUE, 
     main = "Reclassified nDSM (Height Categories)",
     axes = FALSE, box = FALSE)

plot(ndvi, col = "Greens", legend = TRUE, main = "NDVI")

# Load the GeoTIFF
tif <- stack("laip/m_3712212_se_10_060_20220519/m_3712212_se_10_060_20220519.tif")
print(tif)

# Number of bands
nlayers(tif)
# Extract individual bands
red_band <- tif[[1]]  # Band 1
green_band <- tif[[2]]  # Band 2
blue_band <- tif[[3]]  # Band 3
nir_band <- tif[[4]]  # Band 4

hi_res_ndvi <- (nir_band - red_band) / (nir_band + red_band)

plot(hi_res_ndvi, legend = TRUE, main = "NDVI")











#### NDVI at small enough resolution
# Step 1: Define input and output folders
input_folder <- "laip/" # Replace with your folder path
output_folder <- "laip/ndvi_output/" # Folder to save NDVI rasters

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder)

# Step 2: Load all TIFs
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# Step 3: Process each TIF to calculate NDVI and save
for (tif_file in tif_files) {
  cat("Processing:", tif_file, "\n")
  
  # Load the TIF
  tif <- stack(tif_file)
  
  # Extract red and NIR bands (adjust band numbers if needed)
  red_band <- tif[[1]]
  nir_band <- tif[[4]]
  
  # Calculate NDVI
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  
  # Define output file name
  output_file <- gsub(".tif$", "_NDVI.tif", basename(tif_file))
  output_path <- file.path(output_folder, output_file)
  
  # Save the NDVI raster
  writeRaster(ndvi, output_path, format = "GTiff", overwrite = TRUE)
  
  cat("Saved NDVI raster to:", output_path, "\n")
}

# Step 4: List all NDVI rasters
ndvi_files <- list.files(output_folder, pattern = "_NDVI\\.tif$", full.names = TRUE)

# Step 5: Determine unified extent
cat("Calculating unified extent...\n")
xmin_all <- Inf
xmax_all <- -Inf
ymin_all <- Inf
ymax_all <- -Inf

for (ndvi_file in ndvi_files) {
  r <- raster(ndvi_file)
  raster_extent <- extent(r)
  
  xmin_all <- min(xmin_all, raster_extent[1])
  xmax_all <- max(xmax_all, raster_extent[2])
  ymin_all <- min(ymin_all, raster_extent[3])
  ymax_all <- max(ymax_all, raster_extent[4])
}

# Create a unified extent
unified_extent <- extent(xmin_all, xmax_all, ymin_all, ymax_all)
cat("Unified extent is:\n")
cat("xmin:", xmin_all, "xmax:", xmax_all, "ymin:", ymin_all, "ymax:", ymax_all, "\n")

# Step 6: Create a reference raster
reference_raster <- raster(unified_extent, crs = "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
res(reference_raster) <- 1 # Set resolution to 1 meter

# Step 7: Resample all NDVI rasters to the unified extent and resolution
cat("Resampling rasters...\n")
resampled_rasters <- lapply(ndvi_files, function(ndvi_file) {
  r <- raster(ndvi_file)
  r_resampled <- resample(r, reference_raster, method = "bilinear")
  return(r_resampled)
})

# Step 8: Mosaic resampled NDVI rasters together
cat("Mosaicking rasters...\n")
mosaicked_raster_NDVI <- do.call(mosaic, c(resampled_rasters, fun = "mean"))

plot(mosaicked_raster_NDVI, col = "Greens", legend = TRUE, main = "NDVI")

# Step 9: Save the mosaicked NDVI raster
mosaic_output_file_NDVI <- file.path("laip/ndvi_output", "mosaicked_raster_NDVI.tif")
writeRaster(mosaicked_raster_NDVI, mosaic_output_file_NDVI, format = "GTiff", overwrite = TRUE)
cat("Mosaicked NDVI raster saved to:", mosaic_output_file_NDVI, "\n")