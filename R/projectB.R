# R studio API
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(httr)
library(rvest)
library(stringi)
library(stringr)
library(ggplot2)

# Data import and cleaning
url0 <- "https://scholar.google.com/scholar?start=20&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"
url <- "https://scholar.google.com/scholar?start=10&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"
url2 <- "https://scholar.google.com/scholar?start=40&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"
url3 <- "https://scholar.google.com/scholar?start=220&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"
url4 <- "https://scholar.google.com/scholar?start=240&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24"


scholar_url <- read_html(url0)
scholar_title <- scholar_url %>%
  html_nodes(".gs_rt") %>%
  html_text()

scholar_author <- scholar_url %>%
  html_nodes(".gs_a") %>%
  html_text()

scholar_author2 <- scholar_url2 %>%
  html_nodes(".gs_a") %>%
  html_text()

scholar_author3 <- scholar_url3 %>%
  html_nodes(".gs_a") %>%
  html_text()


str_match(scholar_author, "^.+?(?=\\s\\-)")
str_match(scholar_author, "(?<=\\s\\-[\\s]).+?(?=[\\,][\\s]\\d{4}[\\s]\\-)")
str_match(scholar_author2, "(?<=\\s\\-[\\s]).+?(?=[\\,][\\s]\\d{4}[\\s]\\-)")
str_match(scholar_author3, "(?<=[\\- | \\,][\\s])(\\d{4})")[,2]

# function to get Google scholar search results
scholar_search <- function(x){
  url <- paste("https://scholar.google.com/scholar?start=", 
               x, 
               "&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,24", 
               sep = "")
  scholar_url <- read_html(url)
  # title
  scholar_title <- scholar_url %>%
    html_nodes(".gs_rt") %>%
    html_text
  # author, year, journal
  scholar_part1 <- scholar_url %>%
    html_nodes(".gs_a") %>%
    html_text()
  # create a tibble of title, author, year, journal
  scholar_tbl <- tibble(
    title  = scholar_title,
    part   = scholar_part1
  )
  scholar_tbl <- scholar_tbl %>%
    mutate(author   = str_match(part, "^.+?(?=\\s\\-)"),
           year     = str_match(part, "(?<=[\\- | \\,][\\s])(\\d{4})")[,2],
           journal  = str_match(part, "(?<=\\s\\-[\\s]).+?(?=[\\,][\\s]\\d{4}[\\s]\\-)")) %>%
    select(-part)
  return(scholar_tbl)
}

# scrape all pages
scholar_list <- list()
string <- as.character(seq(0, 240, 10))
for (i in string) {
  tbl <- scholar_search(i)
  tbl$i <- i  
  scholar_list[[i]] <- tbl 
}

# final data
scholar_full <- do.call(rbind, scholar_list)

# data <- tibble(
#   journal = c("a", "b", "s", "a", "a", "b", "s", "a", "a", "b", 
#               "s", "a", "n", "n", "w", "g", "q", "y", "p", "l"),
#   author = c("adf", "fgj", "adf", "fgj", "adf", "fgj", "adf", "fgj", "adf", "fgj", 
#              "adf", "fgj", "adf", "fgj", "adf", "fgj", "adf", "fgj", "adf", "fgj")
# )

# Visualization
scholar_full %>%
  group_by(journal) %>%
  summarise(publication_count = n()) %>%
  top_n(10) %>%
  ggplot(aes(x = journal, y = publication_count)) +
  geom_bar(stat = "identity")
