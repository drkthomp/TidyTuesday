# Bring your own data 
library(tidyverse)
library(janitor) # for cleaning input data
library(lubridate) # for handling dates 

# Collected from StoryGraph's export function 
books <- read.delim("~/TidyTuesday/2022-1.tsv")
# clean, remove untagged data, and categorize
books <- books %>% clean_names() %>% 
  # remove untagged data 
  filter(author_gender != "") %>% 
  # correct USA name for mapping 
  mutate(author_country_of_origin = 
               case_when(author_country_of_origin == "USA" ~ "US", 
                         TRUE ~ author_country_of_origin)) %>% 
  # convert factorial columns 
  mutate(across(c("author_gender", "ethnicity",  
                  "author_country_of_origin",
                  "language", "read_status"), factor)) %>% 
  # convert date columns 
  mutate(across(contains("date"),  as.Date, format = "%m/%d/%Y")) %>%
  # and add the month rounding 
  mutate(month=floor_date(most_recent_date, "month"))


library(googleVis)
books %>% group_by(author_country_of_origin) %>% summarize(count=n())

#books_origin <- books %>% group_by(month, author_country_of_origin) %>% summarize(count=n())
months <- unique(books_origin$month)
plots <- lapply(sort(unique(books$month)), function(x){
  data <- books %>% filter(month <= x) %>% group_by(author_country_of_origin) %>% summarize(count=n())
  print(x)
  print(data)
  chart <- gvisGeoChart(data,locationvar='author_country_of_origin',
                   colorvar='count',chartid=month(x, label=TRUE))
  plot(chart)
  return(chart)
})
names(plots) <- months
