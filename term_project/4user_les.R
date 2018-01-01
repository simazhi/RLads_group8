library(rvest)
library(tidyverse)
library(stringr)
library(httr)

## Get user data

### Links to user profiles
usercomms <- read.csv("usercomments_les.csv")

usercomments <- as.data.frame(usercomms)
colnames(usercomments) <- c("v1", "v2", "commenters", "v4")


userlist <- usercomments %>%
  select(commenters) %>%
  filter(commenters != "Unknown") %>%
  distinct() %>%
  pull() %>%
  str_replace_all("\\s", "%20")

userlist

url_base <- "https://www.pornhub.com/users/%s"


userprofile <- map(userlist, function(i){
  links <- sprintf(url_base, i) 
  return(links)
})

userprofile <- unlist(userprofile)
#head(userprofile)


### Link validity (some profiles are deleted)

#variable: userprofile


### Function definition user_scrape()
user_scrape <- function(url){
  
  Sys.sleep(2)
  
  cat(paste("Doing", url, "\n"))
  
  page <- url %>%
    read_html()
  
  user <- url %>%
    str_replace_all("^https://www.pornhub.com/users/", "")
  
  status <- GET(url) %>%
    status_code()
  
  if(status==200){
  
  #variables
  vars <- page %>%
    html_nodes(xpath = '//*[@id="profileInformation"]/div/div[2]/dl') %>%
    html_text() %>% 
    str_replace_all("\t+", "") %>%
    str_replace_all("\n", " ")
  
  ##Gender
  gender <- vars %>%
    str_extract("Gender:.+Age:") %>%
    str_replace_all("Gender:|Age:", "") %>%
    str_replace_all("\\s", "")
  
  ##Age
  age <- vars %>%
    str_extract("Age: \\d+") %>%
    str_replace_all("Age:", "") %>%
    str_replace_all("\\s", "")
  
  ##Last Login
  lastlogin <- vars %>%
    str_extract("Last Login:.+ago\\s[A-Z]") %>%
    str_replace_all("Last Login:\\s|\\s[A-Z]", "")
  
  ##Relationship Status
  relstatus <- vars %>%
    str_extract("Status:.+Interested") %>%
    str_replace_all("^.+:|Interested", "") %>%
    str_replace_all("\\s", "")
  
  ##Interested in
  interest <- vars %>%
    str_extract("Interested\\sIn:\\s[A-Z][a-z]+\\s") %>%
    str_replace_all("Interested\\sIn:\\s", "") %>%
    str_replace_all("\\s", "")
  
  ##Country
  country <- vars %>%
    str_extract("Country:\\s[A-Za-z]+(\\s[A-Za-z]+)?") %>%
    str_replace_all("Country:\\s", "") %>%
    str_replace_all("\\s", "_")
  
  ##City
  city <- vars %>%
    str_extract("City:.+Country:") %>%
    str_replace_all("City:\\s|\\sCountry:", "") %>%
    str_replace_all("\\s", "_")
  
  #data frame
  df <- data.frame(
    user = unlist(user),
    gender = unlist(gender),
    interest = unlist(interest),
    age = unlist(age),
    lastlogin = unlist(lastlogin),
    relstatus = unlist(relstatus),
    country = unlist(country),
    city = unlist(city)
  )
  
  write_csv(df, "userdata_les.csv", append = TRUE)
  } else {
    print("This went wrong.")
  }
  
  
}

#possible function
user_scrape_pos <- possibly(user_scrape, otherwise = NA_real_)



### Execution

userdata <- map_df(userprofile, user_scrape_pos)

#userdata

