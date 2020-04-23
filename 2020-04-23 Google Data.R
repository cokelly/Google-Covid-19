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

# filter_data

dublin_belfast_london <- google_data %>% 
    filter(str_detect(country_region, "Ireland|United Kingdom")) %>%
    filter(str_detect(sub_region_1, "Dublin|Belfast|London")) %>% 
    rename(Date = date,
           `Workplaces \n (% from baseline)` = workplaces_percent_change_from_baseline,
           City = sub_region_1) %>% 
    mutate(City = str_remove_all(City, "County |Greater "))

# Plot of workplace data

plot1 <- dublin_belfast_london %>% 
    ggplot(., aes(Date, `Workplaces \n (% from baseline)`, colour = City)) +
    geom_line(size = 1) +
    xlab(element_blank()) +
    ylab("% from baseline") +
    geom_hline(yintercept = 0, alpha = 0.7, colour = "darkgrey", size = 1) +
    labs(title = "Dublin workplace activity started declining before Belfast and London",
         subtitle = "Google Mobility Data: https://www.google.com/covid19/mobility/",
         caption = "The baseline is the median value for the corresponding day of the week, 2020-01-03 to 2020-02-06") +
    background_grid() +
    scale_x_date(breaks = pretty_breaks(15)) +
    theme_minimal_grid() +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) +
     panel_border(color = "darkgrey") 

#save_plot(plot = plot1, filename = "Google Mobility Workplaces Dublin-Belfast.png", base_asp = 3)


# Pivot the table and then plot retail and grocery data

dublin_belfast_london2 <- dublin_belfast_london %>%  
    select(Date, City, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline) %>% 
    rename(`Retail and Recreation` = retail_and_recreation_percent_change_from_baseline,
           `Groceries and Pharmacies` = grocery_and_pharmacy_percent_change_from_baseline) %>% 
    pivot_longer(cols = c(`Retail and Recreation`, `Groceries and Pharmacies`), names_to = "Type", values_to = "change") 


#plot2 <- 
    
dublin_belfast_london_rolling <- dublin_belfast_london %>% 
    select(Date, City, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline) %>% 
    mutate(rolling_retail_and_recreation = zoo::rollmean(retail_and_recreation_percent_change_from_baseline, k = 3, fill = NA, na.pad = TRUE, align = "center"),
           rolling_grocery_and_pharmacy = zoo::rollmean(grocery_and_pharmacy_percent_change_from_baseline, k = 3, fill = NA, na.pad = TRUE, align = "right")) %>%
    rename(`Retail and Recreation` = rolling_retail_and_recreation,
           `Groceries and Pharmacies` = rolling_grocery_and_pharmacy) %>% 
    pivot_longer(cols = c(`Retail and Recreation`, `Groceries and Pharmacies`), names_to = "Type", values_to = "rolling_average") %>% 
    select(-retail_and_recreation_percent_change_from_baseline, -grocery_and_pharmacy_percent_change_from_baseline) 

dublin_belfast_london3 <- full_join(dublin_belfast_london2, dublin_belfast_london_rolling, by = c("Date", "City", "Type")) 
plot2 <- dublin_belfast_london3 %>%  
    ggplot(aes(Date, change, colour = City)) +
    geom_line(size = 1, alpha = 0.3) +
    geom_line(aes(Date, rolling_average), size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-21")), colour = "green", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-09")), colour = "green", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-12")), colour = "green", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-24")), colour = "green", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-12")), colour = "red", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-16")), colour = "red", size = 0.7) +
    geom_vline(xintercept = as.numeric(as.Date("2020-03-20")), colour = "red", size = 0.7) +
    facet_wrap(~ Type, ncol = 2) +
    background_grid() +
    #scale_x_date(breaks = pretty_breaks(15)) +
    #scale_y_continuous(breaks = pretty_breaks()) +
    theme_minimal_grid() +
    ylab("% change from baseline \n(3 day rolling average)") +
    xlab(element_blank()) +
    geom_hline(yintercept = 0, alpha = 0.7, colour = "darkgrey", size = 1) +
    labs(title = "Dublin's retail spike came sooner and its decline in recreation was sharper",
         subtitle = "Google Mobility Data: https://www.google.com/covid19/mobility/",
         caption = "The baseline is the median value for the corresponding day of the week, 2020-01-03 to 2020-02-06") +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) +
    panel_border(color = "darkgrey") 


save_plot(plot = plot2, filename = "Google Mobility Retail Recreation Dublin-Belfast.png", base_asp = 3)


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
    mutate(Country = str_replace_all(Country, "United Kingdom", "Northern Ireland")) %>% 
    group_by(Country, Date) %>%
    summarise(Workplaces = mean(`Workplaces \n (% from baseline)`, na.rm = TRUE)) %>% 
    mutate(`Rolling Average` = zoo::rollmean(Workplaces, 3, fill = NA, align = "center"))

NIROI %>% 
    ggplot(aes(Date, `Rolling Average`, colour = Country)) +
    geom_line(size = 1.2) +
    geom_line(aes(Date, Workplaces, colour = Country), alpha = 0.5) +
    ylab("3 day rolling average")
