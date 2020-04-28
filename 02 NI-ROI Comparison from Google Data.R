### Google Data NI, ROI compared
### Ciarán O'Kelly
### 21 April 2020

# libraries and data
library(tidyverse)
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
  library(httr)
  # Dplyr has to be loaded after pxR because pxR uses plyr. Not great but just to allow the script to run if necessary
  unloadNamespace(dplyr)
  library(pxR)
  library(dplyr)
  url2 <- "https://statbank.cso.ie/px/pxeirestat/Database/eirestat/Profile%202%20-%20Population%20Distribution%20and%20Movements/E2001.px"
  GET(url2, write_disk(tf1 <- tempfile(fileext = ".px")))
  ireland_population <- read.px(tf1) %>% 
    as_tibble %>%
    # Get year and tidy
    filter(CensusYear == "2016",
           Sex == "Both sexes") %>%
    select(County, value) %>%
    dplyr::rename(region = County,
           population = value)
  write_csv(ireland_population, "Ireland_Population_2016.csv")
}

# Get Northern Ireland population by county from  https://www.nisra.gov.uk/publications/2018-mid-year-population-estimates-northern-ireland

if(file.exists("Northern_Ireland_Population_2018.csv")){
  ni_population_2018 <- read_csv("Northern_Ireland_Population_2018.csv")
} else {
  library(httr)
  url3 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/MYE18_CoC.xlsx"
  GET(url3, write_disk(tf2 <- tempfile(fileext = ".xlsx")))
  ni_population_2018 <- readxl::read_xlsx(tf2, sheet = "Flat") %>%
    # Isolate LGAs and year
    filter(area == "2. Local Government Districts (LGD2014)",
           category == "End population",
           area_name != "NORTHERN IRELAND",
           year == "2016/2017") %>% 
    # Tidy
    select(area_name, MYE) %>% 
    rename(region = area_name,
           population = MYE) %>% 
    mutate(region = str_squish(region),
           region = tolower(region),
           # Match across to google data
           region = str_replace_all(region, "derry city and strabane", "derry and strabane"))
  write_csv(ni_population_2018, "Northern_Ireland_Population_2018.csv")
}
#############################

# Get for Ireland v Northern Ireland

# NI data
NI_google <- google_data %>% 
  # Filter by country then for LGAs
    filter(str_detect(country_region, "United Kingdom")) %>%
    filter(sub_region_1 == "Antrim And Newtownabbey" | sub_region_1 == "Ards And North Down" | sub_region_1 == "Armagh City, Banbridge And Craigavon" | sub_region_1 == "Belfast" | sub_region_1 == "Causeway Coast and Glens" | sub_region_1 == "Derry And Strabane" | sub_region_1 == "Fermanagh And Omagh" | sub_region_1 == "Lisburn and Castlereagh" | sub_region_1 == "Mid And East Antrim" | sub_region_1 == "Mid Ulster") %>% 
  # Tidy
    rename(region = sub_region_1) %>% 
    mutate(region = str_squish(region),
           region = tolower(region)) %>% 
  # Bring in population data
    left_join(., ni_population_2018, by = "region")

# Glasgow data

glasgow_google <- google_data %>%
  filter(str_detect(country_region, "United Kingdom")) %>%
  filter(str_detect(sub_region_1, "Glasgow")) %>%
  rename(region = sub_region_1) %>%
  mutate(region = str_squish(region),
         region = tolower(region),
         region = str_replace_all(region, "glasgow city", "Glasgow"))

# Manchester data

manchester_google <- google_data %>%
  filter(str_detect(country_region, "United Kingdom")) %>%
  filter(str_detect(sub_region_1, "Manchester")) %>%
  rename(region = sub_region_1) %>%
  mutate(region = str_squish(region),
         region = tolower(region),
         region = str_replace_all(region, "greater manchester", "Manchester"))

# ROI data

ROI_google <- google_data %>% 
  # Filter by country
    filter(str_detect(country_region, "Ireland")) %>% 
    rename(region = sub_region_1) %>% 
    mutate(region = str_remove_all(region, "County ")) %>% 
  # Bring in population data
    left_join(., ireland_population, by = "region") %>% 
    filter(!is.na(region))

# Get workplace data for both, using county population to create weighted means
ROI_workplaces <- ROI_google %>% 
    select(region, date, workplaces_percent_change_from_baseline, population) %>% 
    group_by(date) %>% 
  # Get all-ROI data using a mean by LGA weighted by population
    summarise(workplaces_change_from_baseline = weighted.mean(x = workplaces_percent_change_from_baseline, w = population, na.rm = TRUE)) %>% 
    add_column(Region = "Republic of Ireland", .before = 1)

NI_workplaces <- NI_google %>% 
    select(region, date, workplaces_percent_change_from_baseline, population) %>% 
    group_by(date) %>% 
  # Get all-NI data using a mean by LGA weighted by population
    summarise(workplaces_change_from_baseline = weighted.mean(x = workplaces_percent_change_from_baseline, w = population, na.rm = TRUE)) %>% 
    add_column(Region = "Northern Ireland", .before = 1)

 Glasgow_workplaces <- glasgow_google %>%
   select(Region = region, date, workplaces_change_from_baseline = workplaces_percent_change_from_baseline)  

Manchester_workplaces <- manchester_google %>%
   select(Region = region, date, workplaces_change_from_baseline = workplaces_percent_change_from_baseline)   

workplaces_data <- ROI_workplaces %>%
  bind_rows(., NI_workplaces) %>%
  bind_rows(., Glasgow_workplaces) %>%
  bind_rows(., Manchester_workplaces) %>% 
  group_by(Region) %>%
  # Get three date rolling average
  mutate(workplaces_change_from_baseline_rolling_mean = zoo::rollmean(workplaces_change_from_baseline, 5, fill = NA, na.pad = TRUE, align = "center")) %>%
  ungroup %>% 
  mutate(Region = as.factor(Region)) %>% 
  mutate(Region = fct_relevel(Region, "Northern Ireland", "Republic of Ireland", "Glasgow", "Manchester")) %>% 
  mutate(PatricksDay = ifelse(date == "2020-03-17" & Region == "Northern Ireland" |date == "2020-03-17" &  Region == "Republic of Ireland", workplaces_change_from_baseline_rolling_mean, NA))

# Create a separate tibble for the ribbon in the graph
for_ribbon <- workplaces_data %>% 
    mutate(for_ribbon = ifelse(date >= "2020-03-10" & date <= "2020-03-24", workplaces_change_from_baseline_rolling_mean, NA)) %>% 
    pivot_wider(id_cols = c(Region, date), values_from = for_ribbon, names_from = Region)

paddys_day <- workplaces_data %>% 
  filter(!(is.na(PatricksDay))) %>% 
  select(Region, date, workplaces_change_from_baseline_rolling_mean)

cols = c("#018571", "#a6611a", "#80cdc1", "#dfc27d")

workplaces_data %>%
  ggplot() +
  # Line chart
  geom_line(aes(date, workplaces_change_from_baseline_rolling_mean, colour = Region), data = workplaces_data, size = 1.2, na.rm = TRUE) +
  scale_colour_manual(values = cols) +
  geom_line(aes(x = date, y = workplaces_change_from_baseline_rolling_mean), data = paddys_day, colour = "darkgreen", size = 1.2)
  # Ribbon chart
    geom_ribbon(aes(x = date, ymin = `Republic of Ireland`, ymax = `Northern Ireland`), data = for_ribbon, colour = "grey70", alpha = 0.2, na.rm = TRUE) +
  # Horizontal line for baseline
  geom_hline(yintercept = 0, alpha = 0.8, colour = "darkgrey", size = 1) +
  # Theme and titles
    cowplot::theme_minimal_grid() +
    labs(title = "The Republic of Ireland marked a more rabid decline in workplace attendance than Northern Ireland",
         subtitle = "Google mobility data, downloaded 27 April 2020",
         caption = "Datapoints aggregated from mean county-level changes, weighted by population \n baseline = median value for the corresponding day of the week, 2020-01-03 to 2020-02-06") +
    ylab("Change from Baseline (5 day rolling average)") +
    xlab("Date")
