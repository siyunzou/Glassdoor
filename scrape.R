# web scrapping Glassdoor's top best places to work from 2009-2022

# load libraries
library(tidyverse)
library(rvest)
library(foreach)

# loop for each year 
gd_data <- foreach(year= 2009:2022, .combine = 'rbind') %do% {
  url <- paste0("https://www.glassdoor.com/Award/Best-Places-to-Work-",year,"-LST_KQ0,24.htm")
  pg <- read_html(url)
  company <- html_nodes(pg,
            xpath = '//p[contains(@class,"h2 m-0 strong")]')
  company <- html_text(company, trim=T)
  rank <- html_nodes(pg,
            xpath = '//span[contains(@class,"h2 listRank my-0 mr-xsm strong")]')
  rank <- html_text(rank, trim = T)
  top_companies <- tibble('company' = company, 'rank'= rank, 'year' = year)
}

# total 931 rows
table(gd_data$year) # only 2021 and 2022 has the full 100 list 

# best 20 companies to work at 
top_20 <- gd_data %>% count(company) %>%
  arrange(desc(n)) %>%
  slice(1:20)

ggplot(data=top_20, aes(x=n, y=reorder(company,n))) + 
  geom_bar(stat="identity") + ylab("") + 
  xlab("Number of Times on Glassdoor's Top List") + xlim(0,15)



