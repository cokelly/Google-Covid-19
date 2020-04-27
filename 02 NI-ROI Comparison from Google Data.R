### Google Data Belfast, Dublin compared
### Ciarán O'Kelly
### 21 April 2020

# libraries and data
library(pxR)
library(tidyverse)
library(httr)
library(cowplot)
library(scales)
library(zoo)

# Get data

# Get google mobility data from https://www.google.com/covid19/mobility/

if(file.exists("Google_data.csv")){
    google_data <- read_csv("Google_data.csv")
} else {
    url1 <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
    google_data <- read_csv(url1)
    write_csv(google_data, "Google_data.csv")
}

# Get Ireland population by county from https://statbank.cso.ie/px/pxeirestat/Database/eirestat/Profile%202%20-%20Population%20Distribution%20and%20Movements/Profile%202%20-%20Population%20Distribution%20and%20Movements_statbank.asp?sp=Profile%202%20-%20Population%20Distribution%20and%20Movements&Planguage=0&ProductID=DB_E2

if(file.exists("Ireland_Population_2016.csv")){
    ireland_population <- read_csv("Ireland_Population_2016.csv")
} else {
    url2 <- "https://statbank.cso.ie/px/pxeirestat/Database/eirestat/Profile%202%20-%20Population%20Distribution%20and%20Movements/E2001.px"
    GET(url2, write_disk(tf1 <- tempfile(fileext = ".px")))
    ireland_population <- read.px(tf1) %>% 
        as_tibble %>% 
        filter(CensusYear == "2016",
               Sex == "Both sexes") %>% 
        select(County, value) %>% 
        rename(region = County,
               population = value)
    write_csv(ireland_population, "Ireland_Population_2016.csv")
}

# Get Northern Ireland population by county from  https://www.nisra.gov.uk/publications/2018-mid-year-population-estimates-northern-ireland

if(file.exists("Northern_Ireland_Population_2018.csv")){
    ni_population_2018 <- read_csv("Northern_Ireland_Population_2018.csv")
} else {
    url3 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/MYE18_CoC.xlsx"
    GET(url3, write_disk(tf2 <- tempfile(fileext = ".xlsx")))
    ni_population_2018 <- readxl::read_xlsx(tf2, sheet = "Flat") %>%
        filter(area == "2. Local Government Districts (LGD2014)",
               category == "End population",
               area_name != "NORTHERN IRELAND",
               year == "2016/2017") %>% 
        select(area_name, MYE) %>% 
        rename(region = area_name,
               population = MYE) %>% 
        mutate(region = str_squish(region),
               region = tolower(region),
               region = str_replace_all(region, "derry city and strabane", "derry and strabane"))
    write_csv(ni_population_2018, "Northern_Ireland_Population_2018.csv")
}
#############################

# Get for Ireland v Northern Ireland

NI_google <- google_data %>% 
    filter(str_detect(country_region, "United Kingdom")) %>%
    filter(sub_region_1 == "Antrim And Newtownabbey" | sub_region_1 == "Ards And North Down" | sub_region_1 == "Armagh City, Banbridge And Craigavon" | sub_region_1 == "Belfast" | sub_region_1 == "Causeway Coast and Glens" | sub_region_1 == "Derry And Strabane" | sub_region_1 == "Fermanagh And Omagh" | sub_region_1 == "Lisburn and Castlereagh" | sub_region_1 == "Mid And East Antrim" | sub_region_1 == "Mid Ulster") %>% 
    rename(region = sub_region_1) %>% 
    mutate(region = str_squish(region),
           region = tolower(region)) %>% 
    left_join(., ni_population_2018, by = "region")

ROI_google <- google_data %>% 
    filter(str_detect(country_region, "Ireland")) %>% 
    rename(region = sub_region_1) %>% 
    mutate(region = str_remove_all(region, "County ")) %>% 
    left_join(., ireland_population, by = "region")

NIROI_google <- bind_rows(NI_google, ROI_google) %>% 
    rename(Date = date,
           `Workplaces \n (% from baseline)` = workplaces_percent_change_from_baseline,
           Region = region,
           Jurisdiction = country_region) %>%
    mutate(Jurisdiction = str_replace_all(Jurisdiction, "United Kingdom", "Northern Ireland")) 

# %>% 
#     group_by(Country, Date) %>%
#     summarise(Workplaces = mean(`Workplaces \n (% from baseline)`, na.rm = TRUE)) %>% 
#     mutate(`Rolling Average` = zoo::rollmean(Workplaces, 3, fill = NA, align = "center"))

NIROI %>% 
    ggplot(aes(Date, `Rolling Average`, colour = Country)) +
    geom_line(size = 1.2) +
    geom_line(aes(Date, Workplaces, colour = Country), alpha = 0.5) +
    ylab("3 day rolling average")
