### Google Data Belfast, Dublin compared
### Ciarán O'Kelly
### 21 April 2020

# libraries and data

library(tidyverse)
library(cowplot)
library(scales)

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
    geom_line() +
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
    pivot_longer(cols = c(`Retail and Recreation`, `Groceries and Pharmacies`), names_to = "Type") 


plot2 <- dublin_belfast_london2 %>% 
    ggplot(aes(Date, value, colour = City)) +
    geom_line() +
    facet_wrap(~ Type, ncol = 2) +
    background_grid() +
    scale_x_date(breaks = pretty_breaks(15)) +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme_minimal_grid() +
    ylab("% change from baseline") +
    xlab(element_blank()) +
    geom_hline(yintercept = 0, alpha = 0.7, colour = "darkgrey", size = 1) +
    labs(title = "Dublin's retail spike came sooner and its decline in recreation was sharper",
         subtitle = "Google Mobility Data: https://www.google.com/covid19/mobility/",
         caption = "The baseline is the median value for the corresponding day of the week, 2020-01-03 to 2020-02-06") +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) +
     panel_border(color = "darkgrey") 

# save_plot(plot = plot2, filename = "Google Mobility Retail Recreation Dublin-Belfast.png", base_asp = 3)