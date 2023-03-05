library(tidyverse)
library(rvest)
library(jsonlite)
library(httr)

url <- "https://en.wikipedia.org/wiki/Income_tax_in_the_United_States"

histrt <- url %>% GET(config = config(ssl_verifypeer = FALSE)) %>% read_html()

# Extract the table using its CSS selector
histrtdf <- histrt %>% 
 html_nodes("table:nth-child(259)") %>% 
  '[['(1) %>%
  html_table(fill=TRUE)

options(HTTPUserAgent = "rebecca.l.crouse@gmail.com")
ALX = 
  fromJSON("https://data.sec.gov/api/xbrl/companyfacts/CIK0000003499.json") %>%
  as_tibble()

ALX12UTB<-ALX[[3]][[3]][["UnrecognizedTaxBenefits"]]

ALX12UTB


