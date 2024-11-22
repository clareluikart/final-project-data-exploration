#libraries used
library(here)
library(sf)
library(tmap)
library(sp)
library(janitor)
library(dplyr)

datafile <- read.csv(file = here("DECENNIALPL2020.P1-Data.csv"), header = TRUE, sep = ",")
datafile_labels <- datafile[1,]%>%
datafile <- datafile %>%
  clean_names(.)%>%
  filter(row_number() != 1)
datafile_labels <- clean_names(datafile_labels)

natl_sf <- st_read(here("cb_2020_06_bg_500k.shp"))

