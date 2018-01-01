library(rvest)
library(tidyverse)
library(stringr)
library(httr)


##links
links_list <- read_csv("links_all.csv")
#links_list
link_check <- links_list %>% distinct() %>% pull()

## Comments (1 page -> many comments)

### function definition

video_comm <- function(url){
  cat(paste("Doing", url, "\n"))                  
  
  page <- url %>%
    read_html()
  
  #title
  title <- page %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "inlineFree", " " ))]') %>%
    html_text()  
  
  #comments
  comments <- page %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "commentMessage", " " ))]') %>%
    html_text() %>%
    str_replace_all("\t|\n| Reply|•", "") %>%
    str_replace_all("(\\d+)$", " \\1") %>%
    str_replace_all("\\[\\[commentMessage\\]\\] 0", "")
  
  comments <- comments[comments != ""]
  
  ##commenters
  commenters <- page %>%
    html_nodes(xpath = '//*[(@id = "cmtContent")]//*[contains(concat( " ", @class, " " ), concat( " ", "usernameLink", " " ))]') %>%
    html_text()                
  
  l <- length(commenters)
  
  origin <- rep(url, l)
  
  #Data frame
  df <- data.frame(
    origin = unlist(origin),
    title = unlist(title),
    commenters = commenters,
    comments = comments
  )
  
  write_csv(df, "usercomments_all.csv", append = TRUE)
}


### possibly wrapper

video_comm_pos <- possibly(video_comm, otherwise = NA_real_)


### execution

usercomments <- map_df(link_check, video_comm_pos)
usercomments
