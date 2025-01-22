library(pacman)
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
  memoise
)
# coordinates_owners <- fread("data/coordinates_owners.csv")
# county <- st_read("shapefiles/County.shp", quiet = T)
# county_no <- fread("data/facilities_percounty.csv")
# county_density <- fread("data/facilities_countypop.csv")
# location <- fread("data/facilities_location_qoc.csv")
# subcounty <- st_read("shapefiles/gadm36_KEN_2.shp", quiet = T)

# -------------------------------------------------------------------------


# diabetes_services <- readRDS('RData/diabetes_services.RDS')
# diabetes_items <- readRDS('RData/diabetes_items.RDS')
# diabetes_services_county <- readRDS('RData/diabetes_services_county.RDS')
# diabetes_items_county <- readRDS('RData/diabetes_items_county.RDS')
# diabetes_location <- readRDS("RData/diabetes_location.RDS")
# diabetes_facility <- readRDS("RData/diabetes_facility.RDS")
# diabetes_color_code <- readRDS('RData/diabetes_color_code.RDS')
# 
# cvds_services <- readRDS('RData/cvds_services.RDS')
# cvds_items <- readRDS('RData/cvds_items.RDS')
# cvds_services_county <- readRDS('RData/cvds_services_county.RDS')
# cvds_items_county <- readRDS('RData/cvds_items_county.RDS')
# cvds_location <- readRDS("RData/cvds_location.RDS")
# cvds_facility <- readRDS("RData/cvds_facility.RDS")
# cvds_color_code <- readRDS('RData/cvds_color_code.RDS')
# 
# crd_services <- readRDS('RData/crd_services.RDS')
# crd_items <- readRDS('RData/crd_items.RDS')
# crd_services_county <- readRDS('RData/crd_services_county.RDS')
# crd_items_county <- readRDS('RData/crd_items_county.RDS')
# crd_location <- readRDS("RData/crd_location.RDS")
# crd_facility <- readRDS("RData/crd_facility.RDS")
# crd_color_code <- readRDS('RData/crd_color_code.RDS')
# 
# cer_cancer_services <- readRDS('RData/cer_cancer_services.RDS')
# cer_cancer_items <- readRDS('RData/cer_cancer_items.RDS')
# cer_cancer_services_county <- readRDS('RData/cer_cancer_services_county.RDS')
# cer_cancer_items_county <- readRDS('RData/cer_cancer_items_county.RDS')
# cer_cancer_location <- readRDS("RData/cer_cancer_location.RDS")
# cer_cancer_facility <- readRDS("RData/cer_cancer_facility.RDS")
# cer_cancer_color_code <- readRDS('RData/cer_cancer_color_code.RDS')
# 
# b_cancer_services <- readRDS('RData/b_cancer_services.RDS')
# b_cancer_items <- readRDS('RData/b_cancer_items.RDS')
# b_cancer_services_county <- readRDS('RData/b_cancer_services_county.RDS')
# b_cancer_items_county <- readRDS('RData/b_cancer_items_county.RDS')
# b_cancer_location <- readRDS("RData/b_cancer_location.RDS")
# b_cancer_facility <- readRDS("RData/b_cancer_facility.RDS")
# b_cancer_color_code <- readRDS('RData/b_cancer_color_code.RDS')
# 
# col_cancer_services <- readRDS('RData/col_cancer_services.RDS')
# col_cancer_items <- readRDS('RData/col_cancer_items.RDS')
# col_cancer_services_county <- readRDS('RData/col_cancer_services_county.RDS')
# col_cancer_items_county <- readRDS('RData/col_cancer_items_county.RDS')
# col_cancer_location <- readRDS("RData/col_cancer_location.RDS")
# col_cancer_facility <- readRDS("RData/col_cancer_facility.RDS")
# col_cancer_color_code <- readRDS('RData/col_cancer_color_code.RDS')
# 
# pros_cancer_services <- readRDS('RData/pros_cancer_services.RDS')
# pros_cancer_items <- readRDS('RData/pros_cancer_items.RDS')
# pros_cancer_services_county <- readRDS('RData/pros_cancer_services_county.RDS')
# pros_cancer_items_county <- readRDS('RData/pros_cancer_items_county.RDS')
# pros_cancer_location <- readRDS("RData/pros_cancer_location.RDS")
# pros_cancer_facility <- readRDS("RData/pros_cancer_facility.RDS")
# pros_cancer_color_code <- readRDS('RData/pros_cancer_color_code.RDS')
# 
# esop_cancer_services <- readRDS('RData/esop_cancer_services.RDS')
# esop_cancer_items <- readRDS('RData/esop_cancer_items.RDS')
# esop_cancer_services_county <- readRDS('RData/esop_cancer_services_county.RDS')
# esop_cancer_items_county <- readRDS('RData/esop_cancer_items_county.RDS')
# esop_cancer_location <- readRDS("RData/esop_cancer_location.RDS")
# esop_cancer_facility <- readRDS("RData/esop_cancer_facility.RDS")
# esop_cancer_color_code <- readRDS('RData/esop_cancer_color_code.RDS')
# 
# pallit_services <- readRDS('RData/pallit_services.RDS')
# pallit_items <- readRDS('RData/pallit_items.RDS')
# pallit_services_county <- readRDS('RData/pallit_services_county.RDS')
# pallit_items_county <- readRDS('RData/pallit_items_county.RDS')
# pallit_location <- readRDS("RData/pallit_location.RDS")
# pallit_facility <- readRDS("RData/pallit_facility.RDS")
# pallit_color_code <- readRDS('RData/pallit_color_code.RDS')
# 
# mental_services <- readRDS('RData/mental_services.RDS')
# mental_items <- readRDS('RData/mental_items.RDS')
# mental_services_county <- readRDS('RData/mental_services_county.RDS')
# mental_items_county <- readRDS('RData/mental_items_county.RDS')
# mental_location <- readRDS("RData/mental_location.RDS")
# mental_facility <- readRDS("RData/mental_facility.RDS")
# mental_color_code <- readRDS('RData/mental_color_code.RDS')
# 
# mal_services <- readRDS('RData/mal_services.RDS')
# mal_items <- readRDS('RData/mal_items.RDS')
# mal_services_county <- readRDS('RData/mal_services_county.RDS')
# mal_items_county <- readRDS('RData/mal_items_county.RDS')
# mal_location <- readRDS("RData/mal_location.RDS")
# mal_facility <- readRDS("RData/mal_facility.RDS")
# mal_color_code <- readRDS('RData/mal_color_code.RDS')
# 
# tb_services <- readRDS('RData/tb_services.RDS')
# tb_items <- readRDS('RData/tb_items.RDS')
# tb_services_county <- readRDS('RData/tb_services_county.RDS')
# tb_items_county <- readRDS('RData/tb_items_county.RDS')
# tb_location <- readRDS("RData/tb_location.RDS")
# tb_facility <- readRDS("RData/tb_facility.RDS")
# tb_color_code <- readRDS('RData/tb_color_code.RDS')
# 
# mdt_services <- readRDS('RData/mdt_services.RDS')
# mdt_items <- readRDS('RData/mdt_items.RDS')
# mdt_services_county <- readRDS('RData/mdt_services_county.RDS')
# mdt_items_county <- readRDS('RData/mdt_items_county.RDS')
# mdt_location <- readRDS("RData/mdt_location.RDS")
# mdt_facility <- readRDS("RData/mdt_facility.RDS")
# mdt_color_code <- readRDS('RData/mdt_color_code.RDS')
# 
# hiv_services <- readRDS('RData/hiv_services.RDS')
# hiv_items <- readRDS('RData/hiv_items.RDS')
# hiv_services_county <- readRDS('RData/hiv_services_county.RDS')
# hiv_items_county <- readRDS('RData/hiv_items_county.RDS')
# hiv_location <- readRDS("RData/hiv_location.RDS")
# hiv_facility <- readRDS("RData/hiv_facility.RDS")
# hiv_color_code <- readRDS('RData/hiv_color_code.RDS')
# 
# sti_services <- readRDS('RData/sti_services.RDS')
# sti_items <- readRDS('RData/sti_items.RDS')
# sti_services_county <- readRDS('RData/sti_services_county.RDS')
# sti_items_county <- readRDS('RData/sti_items_county.RDS')
# sti_location <- readRDS("RData/sti_location.RDS")
# sti_facility <- readRDS("RData/sti_facility.RDS")
# sti_color_code <- readRDS('RData/sti_color_code.RDS')
# 
# ntds_services <- readRDS('RData/ntds_services.RDS')
# ntds_items <- readRDS('RData/ntds_items.RDS')
# ntds_services_county <- readRDS('RData/ntds_services_county.RDS')
# ntds_items_county <- readRDS('RData/ntds_items_county.RDS')
# ntds_location <- readRDS("RData/ntds_location.RDS")
# ntds_facility <- readRDS("RData/ntds_facility.RDS")
# ntds_color_code <- readRDS('RData/ntds_color_code.RDS')
# 
# 

