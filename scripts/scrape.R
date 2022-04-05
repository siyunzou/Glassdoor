# web scrapping Glassdoor's top best places to work from 2009-2022

# load libraries
library(tidyverse)
library(rvest)
library(foreach)

light.theme <- theme(legend.position = "none",
                     panel.grid.minor.y = element_line(color = NA),
                     panel.grid.major.y = element_line(color = "gray95"),
                     panel.grid.minor.x = element_line(color = NA),
                     panel.grid.major.x = element_line(color = NA),
                     panel.background = element_rect(fill = NA),
                     plot.background = element_rect(fill = NA,
                                                    color = "gray95",
                                                    size = 10),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_text(color = "gray30"),
                     axis.ticks = element_line(color = NA),
                     strip.background = element_rect(fill = "gray95"),
                     strip.text = element_text(color = "gray30",
                                               size = 11,
                                               face = "bold"),
                     plot.title = element_text(color = "gray30",
                                               face = "bold"),
                     plot.subtitle = element_text(size = 10,
                                                  color = "gray30"),
                     text = element_text(family = "Helvetica"))

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

# best 20 companies to work at/which 20 companies have been on the top 
# best place to work?
top_20 <- gd_data %>% count(company) %>%
  arrange(desc(n)) %>%
  slice(1:20)

p <- ggplot(data=top_20, aes(x=n, y=reorder(company,n))) + 
  geom_col(fill='#2b7551') + 
  xlim(0,15) + 
  labs(title= "Top 20 on Glassdoor's List the most", 
       y = "",
       x= "") + light.theme

ggsave(plot=p, file= '../figures/top20.png', height=7, width=7)

# Which company has been top 10 the most?
top_10 <- gd_data %>% 
  filter(rank %in% c("#1","#2","#3","#4","#5","#6",
                     "#7","#8","#9","#10")) %>%
  count(company) %>%
  arrange(desc(n)) %>%
  slice(1:10)

p2 <- ggplot(data=top_10, aes(x=n, y=reorder(company,n))) + 
  geom_col(fill='#2b7551') + 
  xlim(0,15) + 
  labs(title= "Glassdoor's Most Frequent Top 10 From 2009-2022", 
       y = "",
       x= "") + light.theme

ggsave(plot=p2, file= '../figures/top10.png', height=7, width=7)

