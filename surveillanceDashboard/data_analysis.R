

data_county1 <- data_county |> 
  summarise(
    value = sum(`Number at Risk`),
    .by = c(county, `Disease/ Condition`)
  )

data_subcounty1 <- data_subcounty |> 
  summarise(
    value = sum(`Number at Risk`),
    .by = c(county, sub_county, `Disease/ Condition`)
  )

fwrite(data_county1, "data/data_county1.csv", row.names = F)
fwrite(data_subcounty1, "data/data_subcounty1.csv", row.names = F)

county_shapefile <- county |> 
  rename(county = Name) |> 
  dplyr::select(county, geometry) |> 
  st_as_sf()

subcounty_shapefile <- subcounty |> 
  rename(county = NAME_1, sub_county = NAME_2) |> 
  dplyr::select(county, sub_county, geometry) |> 
  st_as_sf()

st_write(county_shapefile, "clean shapefiles/county_shapefile.shp", quiet = T)
st_write(subcounty_shapefile, "clean shapefiles/subcounty_shapefile.shp", quiet = T)
