library(pacman)
library(bslib) 
library(lubridate)
p_load(
  DBI,
  sf,
  tmap,
  tidyverse,
  highcharter,
  plotly,
  shiny,
  shinyjs,
  shinyWidgets,
  shinydashboard,
  htmltools,
  leaflet,
  sf,
  scales,
  readxl,
  data.table,
  ggiraph,
  rKenyaCensus,
  data.table,
  DT,
  shinycssloaders,
  leaflet.extras,
  fontawesome,
  shinymanager,
  RColorBrewer,
  shinyauthr,
  reactable,
  memoise,
  ggplot2
)

# Importing data ----------------------------------------------------------
# clean dataset
data1<-st_read("data/Cleaned Animal data.csv")

# aggregated data
aggregare_sick = fread("data/aggregate_sick.csv")
aggregare_risk= fread("data/aggregate_risk.csv")
aggregare_humans= fread("data/aggregate_humansAffected.csv")
aggregare_dead = fread("data/aggregate_dead.csv")
aggregare_vaccinated = fread("data/aggregate_vaccinated.csv")
aggregare_groupings = fread("data/aggregate_groupings.csv")
aggregate_breed = fread("data/aggregate_breed.csv")
data_subcounty = fread("data/data_subcounty.csv")
data2 = fread("data/data2.csv")

# county
county1 <- st_read("shapefiles/County.shp", quiet = T)|> 
  rename(County = Name)

# subcounty
subcounty <- st_read("shapefiles/gadm36_KEN_2.shp", quiet = T)|> 
  rename(county = NAME_1, sub_county = NAME_2)

# cleaned data 
county <- st_read("shapefiles/County.shp", quiet = T)|> 
  rename(county = Name)

county_shapefile <- st_read("clean shapefiles/county_shapefile.shp", quiet = T)
subcounty_shapefile <- st_read("clean shapefiles/subcounty_shapefile.shp", quiet = T)

# report data
data <- fread("data/2024_ ND1 Reports.csv")
# location
location <- fread("data/clinic_locations.csv")
data_county <- fread("data/data_county.csv")
data_subcounty <- fread("data/data_subcounty.csv")
