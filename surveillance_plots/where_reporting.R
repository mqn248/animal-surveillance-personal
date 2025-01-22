library(tidyverse)
library(sf)

## read in the data for the dashboard 

## needs to be updated to read from database directly 
kabs_all <- readxl::read_excel("dummy_data/Cleaned Animal data.xlsx")

kabs <- kabs_all[sample(1:nrow(kabs_all),size=30,
                                  replace=FALSE),]


# conn <-  dbConnect(
#   RPostgres::Postgres(),
#   dbname = "postgres",
#   host = "animalsurveillance.craswukoq326.us-east-1.rds.amazonaws.com",
#   port = 5432,
#   user = "postgres",
#   password = "c3mA_hUb"
# )
# 
# kabs <- dbGetQuery(conn, "SELECT * FROM kabs_records")



## variable we want is sub-county - need to see what this is called properly 

# ## read in the list of all possible sub-counties
# counties_data <- readxl::read_excel("surveillance_plots/data/counties_data.xlsx")
# counties <- c(counties_data$County)
# 
# sc <- data.table::tstrsplit(counties_data$Subcounty,split=",")
# sub_counties <- data.frame("county" = rep(unique(counties),times = length(sc)),
#                            "subcounty" = unlist(sc)) %>% 
#   filter(!is.na(subcounty))
# 
# write.csv(sub_counties,"surveillance_plots/data/subcounties_tidy.csv")

sub_counties <- read.csv("surveillance_plots/data/subcounties_tidy.csv")

sub_counties <- sub_counties %>% mutate(
  reported = ifelse(subcounty %in% kabs$Sub_County, 1, 0)
)

# sub_counties$subcounty[which(sub_counties$subcounty=="Igambango'mbe")] <- "Igambang'ombe"
# sub_counties$subcounty[which(sub_counties$subcounty=="Sigowet/Soin sub")] <- "Sigowet/Soin"

length(unique(kabs$Sub_County))
sub_counties %>% select(reported) %>% sum()

## now read in the shape files 

sf_data <- sf::st_read("shapefiles-2/ke_subcounty.shp")

## quick test
ggplot(data = sf_data) +
  geom_sf() +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank())


## merge data together

## count goes 1 - 6
sf_data_tidy <- sf_data %>% mutate(sc_count = str_count(subcounty,'\\w+')) %>%
  mutate(sc_tidy = case_when(sc_count == 1 | sc_count == 2 ~ subcounty,
                             sc_count == 3 ~ word(subcounty,1),
                             sc_count == 4 ~ paste(word(subcounty,1),
                                                   word(subcounty,2)))) %>%
  rename(subcounty_raw = subcounty,
         subcounty = sc_tidy)

sf_data_comb <- merge(sf_data_tidy %>% mutate(subcounty = tolower(subcounty)),
                      sub_counties %>% mutate(subcounty = tolower(subcounty)),
                      by="subcounty",all.x=TRUE)

length(which(is.na(sf_data_comb$reported)))

sf_data_comb <- sf_data_comb %>% 
  mutate(reported = case_when(is.na(reported)|reported==0 ~ 0,
                              reported==1 ~ 1),
         reported_label = case_when(reported==0 ~ "No reports during period",
                                    reported==1 ~ "Reports during period")) 

length(which(is.na(sf_data_comb$reported)))

ggplot() +
  geom_sf(data = sf_data_comb,aes(fill=as.factor(reported_label))) +
  geom_point(data = kabs,aes(x=Longitude,y=Latitude),size=2,col="darkgreen")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill="")+
  scale_fill_manual(values = c("white","lightblue"))

