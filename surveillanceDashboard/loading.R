library(pacman)
p_load(
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
  memoise
)

# Importing data ----------------------------------------------------------

# county
county <- st_read("shapefiles/County.shp", quiet = T)|> 
  rename(county = Name)

# location
location <- fread("data/clinic_locations.csv")

# subcounty
subcounty <- st_read("shapefiles/gadm36_KEN_2.shp", quiet = T)|> 
  rename(county = NAME_1, sub_county = NAME_2)

# report data
data <- fread("data/2024_ ND1 Reports.csv")

# cleaned data 
data_county <- fread("data/data_county.csv")
data_subcounty <- fread("data/data_subcounty.csv")
county_shapefile <- st_read("clean shapefiles/county_shapefile.shp", quiet = T)
subcounty_shapefile <- st_read("clean shapefiles/subcounty_shapefile.shp", quiet = T)



