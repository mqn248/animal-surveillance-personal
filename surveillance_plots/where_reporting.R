## read in the data for the dashboard 

dashboard <- readxl::read_excel("dummy_data/animal_data.xlsx",
                                sheet="Cleaned")

## variable we want is sub-county - need to see what this is called properly 

## read in the list of all possible sub-counties
counties_data <- readxl::read_excel("surveillance_plots/counties_data.xlsx")
counties <- c(counties_data$County)

sc <- data.table::tstrsplit(counties_data$Subcounty,split=",")
sub_counties <- data.frame("county" = rep(unique(counties),times = length(sc)),
                           "subcounty" = unlist(sc)) %>% 
  filter(!is.na(subcounty))

write.csv(sub_counties,"surveillance_plots/subcounties_tidy.csv")




## indicator for if the country is in the list or not

