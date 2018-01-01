library(rvest)
library(tidyverse)
library(stringr)
library(httr)

resultspages <- 1:23  ##Change this to 23 for the complete set
url_base <- "https://www.pornhub.com/video?c=27&o=mv&page=%d"


link_check <- map(resultspages, function(i){
  
  cat(".")
  
  links <- read_html(sprintf(url_base, i)) %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "title", " " ))]//a') %>%
    html_attr('href') %>%
    str_replace_all("/(view.+)", "https://www.pornhub.com/\\1") %>%#add website domain
    as.tibble() %>%
    filter(grepl("^https", value))  #turns variable in tibble into vector
  
  #return(links)
  
  links2 <- unlist(links)
  
  write_delim(links, "links_les.csv", append = TRUE)
  
  return(links2)
  
})

#link_check <- link_check[1:5]

links_list <- read_csv("links_les.csv")
#links_list
link_check <- links_list %>% distinct() %>% pull()