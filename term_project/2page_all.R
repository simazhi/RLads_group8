library(rvest)
library(tidyverse)
library(stringr)
library(httr)


##links
links_list <- read_csv("links_all.csv")
#links_list
link_check <- links_list %>% distinct() %>% pull()

## Video pages (1 page -> 1 kind of element)

### function statement 
video_num <- function(url){
  Sys.sleep(2)
  
  cat(paste("Doing", url, "\n"))
  
  page <- url %>%
    read_html()
  
  #title
  title <- page %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "inlineFree", " " ))]') %>%
    html_text()
  
  #numerical data
  ##views
  views <- page %>% 
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "count", " " ))]') %>%
    html_text() %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  ##percentage
  percentage <- page %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "percent", " " ))]') %>%
    html_text() %>%
    str_replace_all("%", "") %>%
    as.numeric()
  
  ##upvotes
  upvotes <- page %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "votesUp", " " ))]') %>%
    html_text() %>%
    as.numeric()
  
  ##downvotes
  downvotes <- page %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "votesDown", " " ))]') %>%
    html_text() %>%
    as.numeric()
  
  #nominal data
  ##categories
  categories <- page %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "categoriesWrapper", " " ))]') %>%
    html_text() %>%
    str_replace_all("\n|\t|\\+ Suggest", "") %>%
    str_replace_all("^Categories:", "")  %>%
    str_replace_all("^\\s", "")
  
  
  ##data frame
  df <- data.frame(
    url = unlist(url),
    title = unlist(title),
    views = views,
    upvotes = upvotes,
    downvotes = downvotes,
    percentage = percentage,
    categories = categories
  )
  
  write_csv(df, "videopages_all.csv", append = TRUE)
  
}


### function in possibly wrapper

video_num_pos <- possibly(video_num, otherwise = NA_real_)


### execution

videopages <- map_df(link_check, video_num_pos)
videopages