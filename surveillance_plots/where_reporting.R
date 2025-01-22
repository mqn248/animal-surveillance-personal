## read in the data for the dashboard 

dashboard <- readxl::read_excel("dummy_data/animal_data.xlsx",
                                sheet="Cleaned")

## variable we want is sub-county - need to see what this is called properly 

## read in the list of all possible sub-counties
all_counties <- read.csv("")



## indicator for if the country is in the list or not

