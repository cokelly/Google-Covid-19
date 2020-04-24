### Google Data Belfast, Dublin compared
### Ciarán O'Kelly
### 21 April 2020

# libraries and data

library(tidyverse)
library(cowplot)
library(scales)
library(zoo)

# Get data

if(file.exists("Google_data.csv")){
    google_data <- read_csv("Google_data.csv")
} else {
    url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
    google_data <- read_csv(url)
    write_csv(google_data, "Google_data.csv")
}


#############################

# Get for Ireland v Northern Ireland

NI <- google_data %>% 
    filter(str_detect(country_region, "United Kingdom")) %>%
    filter(sub_region_1 == "Antrim And Newtownabbey" | sub_region_1 == "Ards And North Down" | sub_region_1 == "Armagh City, Banbridge And Craigavon" | sub_region_1 == "Belfast" | sub_region_1 == "Causeway Coast and Glens" | sub_region_1 == "Derry And Strabane" | sub_region_1 == "Fermanagh And Omagh" | sub_region_1 == "Lisburn and Castlereagh" | sub_region_1 == "Mid And East Antrim" | sub_region_1 == "Mid Ulster")

ROI <- google_data %>% 
    filter(str_detect(country_region, "Ireland")) 

NIROI <- bind_rows(NI, ROI) %>% 
    rename(Date = date,
           `Workplaces \n (% from baseline)` = workplaces_percent_change_from_baseline,
           Region = sub_region_1,
           Country = country_region) %>%
    mutate(Country = str_replace_all(Country, "United Kingdom", "Northern Ireland")) 

# %>% 
#     group_by(Country, Date) %>%
#     summarise(Workplaces = mean(`Workplaces \n (% from baseline)`, na.rm = TRUE)) %>% 
#     mutate(`Rolling Average` = zoo::rollmean(Workplaces, 3, fill = NA, align = "center"))

NIROI %>% 
    ggplot(aes(Date, `Rolling Average`, colour = Country)) +
    geom_line(size = 1.2) +
    geom_line(aes(Date, Workplaces, colour = Country), alpha = 0.5) +
    ylab("3 day rolling average")
