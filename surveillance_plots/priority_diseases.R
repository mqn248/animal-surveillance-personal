## plots for dashboard - animal surveillance 

library(tidyverse)
library(cowplot)
library(RColorBrewer)

# conn <-  dbConnect(
#   RPostgres::Postgres(),
#   dbname = "postgres",
#   host = "animalsurveillance.craswukoq326.us-east-1.rds.amazonaws.com",
#   port = 5432,
#   user = "postgres",
#   password = "c3mA_hUb"
# )
# 
# kabs <- dbGetQuery(conn, "SELECT * FROM Animal_records")

kabs <- readxl::read_excel("dummy_data/Cleaned Animal data.xlsx")

focus_cols <- c("County","Disease_Condition","Number_at_Risk","Number_Sick", 
                "Number_Dead","Number_Humans_Affected_zoonosis",
                "Latitude","Longitude")

focus_cols %in% colnames(kabs)

priority_diseases <- c("Anthrax","Rift Valley Fever (RVF)","Brucellosis","Rabies")

str(kabs)
#kabs$Number_Humans_Affected_zoonosis <- as.integer(kabs$Number_Humans_Affected_zoonosis)

## trends of priority disease in 2024 - line graph (might change to bar)

df <- kabs %>% filter(Report_Date>=as.Date("2024-01-01"),
                Disease_Condition %in% priority_diseases) %>%
  select(Report_Date,County,Disease_Condition,Number_at_Risk,Number_Sick,
         Number_Dead,Number_Humans_Affected_zoonosis) %>%
  mutate(month = factor(format(Report_Date,"%b"),
                        levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                 "Aug","Sep", "Oct","Nov","Dec")))
  # mutate(month = factor(format(Report_Date,"%B"),
  #                       levels=c("January","February","March","April",
  #                                "May","June","July","August","September",
  #                                "October","November","December"))) 

df1 <- pivot_longer(df,cols = c("Number_at_Risk","Number_Sick", 
                         "Number_Dead","Number_Humans_Affected_zoonosis")) %>%
  rename(variable = name) %>% 
  group_by(month,variable,Disease_Condition) %>%
  summarise(value = sum(value,na.rm = NA)) %>%
  mutate(variable_label = factor(case_when(variable=="Number_at_Risk" ~ "At risk",
                              variable=="Number_Dead" ~ "Dead",
                              variable=="Number_Sick" ~ "Sick",
                              variable=="Number_Humans_Affected_zoonosis" ~ "Humans affected"),
                              levels=c("Sick","At risk","Dead","Humans affected")))


ggplot(df1,
       aes(x=month,y=value,col=Disease_Condition,group=Disease_Condition))+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x="Month",y="Number",col="Priority Disease",
       title="Trends of priority diseases in 2024")+
  facet_wrap(~variable_label,nrow=1,scales = "free_y")+
  scale_color_viridis_d(begin=0.1,end=0.9)+
  theme(axis.text.x = element_text(angle=45),
        strip.background = element_rect(fill="white"),
        legend.position = "bottom")

## heatmap of number at risk by disease and county 

### need logic later to take the top 10, now taking them all as only 8
top_10_counties <- df %>% group_by(County) %>%
  summarise(Number_at_Risk = sum(Number_at_Risk),
            Number_Sick = sum(Number_Sick),
            Number_Dead = sum(Number_Dead),
            Number_Humans_Affected_zoonosis = sum(Number_Humans_Affected_zoonosis))

top_10_risk <- (top_10_counties %>% arrange(-Number_at_Risk) %>% select(County) %>% unlist() %>% c())[1:10]

top_10_sick <- (top_10_counties %>% arrange(-Number_Sick) %>% select(County) %>% unlist() %>% c())[1:10]

top_10_dead <- (top_10_counties %>% arrange(-Number_Dead) %>% select(County) %>% unlist() %>% c())[1:10]

top_10_human <- (top_10_counties %>% arrange(-Number_Humans_Affected_zoonosis) %>% select(County) %>% unlist() %>% c())[1:10]

#### ACTION: don't drop levels so have NAs
df2 <- pivot_longer(df,cols = c("Number_at_Risk","Number_Sick", 
                         "Number_Dead","Number_Humans_Affected_zoonosis")) %>%
  rename(variable = name) %>%
  group_by(County,Disease_Condition,variable) %>% 
  summarise(value = sum(value,na.rm=TRUE))  %>%
  mutate(variable_label = factor(case_when(variable=="Number_at_Risk" ~ "At risk",
                                           variable=="Number_Dead" ~ "Dead",
                                           variable=="Number_Sick" ~ "Sick",
                                           variable=="Number_Humans_Affected_zoonosis" ~ "Humans affected"),
                                 levels=c("Sick","At risk","Dead","Humans affected")))
 

blues_palette <- rev(brewer.pal(n=9,name="Blues"))
greens_palette <- rev(brewer.pal(n=9,name="YlGn"))
reds_palette <- rev(brewer.pal(n=9,name="Reds"))
purples_palette <- rev(brewer.pal(n=9,name="Purples"))


cowplot::plot_grid(
  ggplot(df2 %>% filter(County %in% top_10_sick,
                        variable == "Number_Sick"),
         aes(x=Disease_Condition,y=County,fill=value))+
    geom_tile()+
    facet_wrap(~variable_label)+
    labs(x="Priority Disease",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"))+
    scale_fill_gradient(low=blues_palette[1],high=blues_palette[length(blues_palette)],na.value = "grey"),
  ggplot(df2 %>% filter(County %in% top_10_risk,
                        variable == "Number_at_Risk"),
         aes(x=Disease_Condition,y=County,fill=value))+
    geom_tile()+
    facet_wrap(~variable_label)+
    labs(x="Priority Disease",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"))+
    scale_fill_gradient(low=greens_palette[1],high=greens_palette[length(blues_palette)],na.value = "grey"),
  ggplot(df2 %>% filter(County %in% top_10_dead,
                        variable == "Number_Dead"),
         aes(x=Disease_Condition,y=County,fill=value))+
    geom_tile()+
    facet_wrap(~variable_label)+
    labs(x="Priority Disease",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"))+
    scale_fill_gradient(low=reds_palette[1],high=reds_palette[length(blues_palette)],na.value = "grey"),
  ggplot(df2 %>% filter(County %in% top_10_human,
                        variable == "Number_Humans_Affected_zoonosis"),
         aes(x=Disease_Condition,y=County,fill=value))+
    geom_tile()+
    facet_wrap(~variable_label)+
    labs(x="Priority Disease",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"))+
    scale_fill_gradient(low=purples_palette[1],high=purples_palette[length(blues_palette)],na.value = "grey"),
  nrow=2
)


## plot of 4 - priority diseases by 1) number at risk; 2) number sick; 3) number deaths; 4) humans affected

df3 <- pivot_longer(df,cols = c("Number_at_Risk","Number_Sick", 
                         "Number_Dead","Number_Humans_Affected_zoonosis")) %>%
  rename(variable = name) %>%
  group_by(Disease_Condition,variable) %>%
  summarise(value = sum(value,na.rm = TRUE)) %>%
  mutate(variable_label = factor(case_when(variable=="Number_at_Risk" ~ "At risk",
                                           variable=="Number_Dead" ~ "Dead",
                                           variable=="Number_Sick" ~ "Sick",
                                           variable=="Number_Humans_Affected_zoonosis" ~ "Humans affected"),
                                 levels=c("Sick","At risk","Dead","Humans affected")))


## background, colours, order the axis 
ggplot(df3 %>% arrange(-value),
       aes(x=value,y=Disease_Condition,fill=variable))+
  geom_col()+
  facet_wrap(~variable)


cowplot::plot_grid(
  ggplot(df3 %>% filter(variable == "Number_Sick"),
         aes(x=value,y=fct_reorder(Disease_Condition,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=blues_palette[5])+
    labs(y="Priority disease",x="Total sick"),
  ggplot(df3 %>% filter(variable == "Number_at_Risk"),
         aes(x=value,y=fct_reorder(Disease_Condition,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values="yellow")+
    labs(y="Priority disease",x="Total at risk"),
  ggplot(df3 %>% filter(variable == "Number_Dead"),
         aes(x=value,y=fct_reorder(Disease_Condition,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=reds_palette[5])+
    labs(y="Priority disease",x="Total dead"),
  ggplot(df3 %>% filter(variable == "Number_Humans_Affected_zoonosis"),
         aes(x=value,y=fct_reorder(Disease_Condition,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=purples_palette[5])+
    labs(y="Priority disease",x="Total humans affected"),
  nrow=2
)


## top counties by number at risk, sick, death, humans affected 
## same plot as above but swapping disease for county
## use the top 10 county logic here 

df4 <- pivot_longer(df,cols = c("Number_at_Risk","Number_Sick", 
                                "Number_Dead","Number_Humans_Affected_zoonosis")) %>%
  rename(variable = name) %>%
  group_by(County,variable) %>%
  summarise(value = sum(value,na.rm = TRUE)) %>%
  mutate(variable_label = factor(case_when(variable=="Number_at_Risk" ~ "At risk",
                                           variable=="Number_Dead" ~ "Dead",
                                           variable=="Number_Sick" ~ "Sick",
                                           variable=="Number_Humans_Affected_zoonosis" ~ "Humans affected"),
                                 levels=c("Sick","At risk","Dead","Humans affected")))


ggplot(df4 %>% arrange(-value),
       aes(x=value,y=County,fill=variable))+
  geom_col()+
  facet_wrap(~variable)


cowplot::plot_grid(
  ggplot(df4 %>% filter(variable == "Number_Sick",
                        County %in% top_10_sick),
         aes(x=value,y=fct_reorder(County,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=blues_palette[5])+
    labs(y="County",x="Total sick"),
  ggplot(df4 %>% filter(variable == "Number_at_Risk",
                        County %in% top_10_risk),
         aes(x=value,y=fct_reorder(County,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values="yellow")+
    labs(y="County",x="Total at risk"),
  ggplot(df4 %>% filter(variable == "Number_Dead",
                        County %in% top_10_dead),
         aes(x=value,y=fct_reorder(County,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=reds_palette[5])+
    labs(y="County",x="Total dead"),
  ggplot(df4 %>% filter(variable == "Number_Humans_Affected_zoonosis",
                        County %in% top_10_human),
         aes(x=value,y=fct_reorder(County,.x=value),fill=variable))+
    geom_col()+
    facet_wrap(~variable_label)+
    #labs(x="",fill="")+
    theme_bw()+
    theme(strip.background = element_rect(fill="white"),
          legend.position = "none")+
    scale_fill_manual(values=purples_palette[5])+
    labs(y="County",x="Total humans affected"),
  nrow=2
)



## priority diseases by county - filter by priority disease, for each of the 4 indicators 

df5 <- pivot_longer(df,cols = c("Number_at_Risk","Number_Sick", 
                                "Number_Dead","Number_Humans_Affected_zoonosis")) %>%
  rename(variable = name) %>%
  group_by(County,Disease_Condition,variable) %>%
  summarise(value = sum(value,na.rm = TRUE)) %>%
  mutate(variable_label = factor(case_when(variable=="Number_at_Risk" ~ "At risk",variable=="Number_Dead" ~ "Dead",
                                                                                        variable=="Number_Sick" ~ "Sick",
                                                                                        variable=="Number_Humans_Affected_zoonosis" ~ "Humans affected"),
                                                                              levels=c("Sick","At risk","Dead","Humans affected")))

## lots of tidying
ggplot(df5,aes(x=County,y=value,fill=Disease_Condition))+
  geom_col(position="dodge")+
  facet_wrap(~variable)


ggplot(df5,aes(x=County,y=value,fill=Disease_Condition))+
  geom_col(position="dodge")+
  facet_wrap(~variable_label,nrow=2,scales="free_y")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle=90))+
  scale_fill_viridis_d(begin=0.1,end=0.9)+
  labs(fill="Priority disease",y="Total")
  

## map of Kenya showing the reporting - SI unit 



