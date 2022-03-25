library(tidyverse)
library(rvest)


## Performing search
MAX_PAGES <- 10    

locations <- c(
  "San Francisco, CA",
  "New York, NY",
  "Seattle, WA",
  "Boston, MA",
  "Chicago, IL",
  "Austin, TX",
  "Los Angeles, CA",
  "Cambridge, MA",
  "San Diego, CA",
  "Atlanta, GA"
)

base_search_url <- "https://www.theladders.com/jobs/searchresults-jobs?keywords=data%20scientist&locationsFilter="
distance <- "&distance=40"
# https://www.theladders.com/jobs/searchresults-jobs?keywords=data%20scientist&locationsFilter=San%20Francisco,%20CA&distance=40

buildSearch <- function(base, location, page) {
  location <- gsub("[ ]", "%20", location)
  page <- paste0("&page=",page)
  url <- paste0(base, location, distance, page)
  return(url)
}

##### The below functions are for getting the info in the job post:
base_job_link <- "https://www.theladders.com"
buildJobLink <- function(base, link) {
  job_link <- paste0(base, link)
  return(job_link)
}


get_title <- function(joblink) {
  title <- read_html(joblink, encoding="UTF-8") %>%
    html_elements(xpath= '//h1[(@class="job-view-title")]') %>% 
    html_text2()
  return(title)
}

get_description <- function(joblink) {
  description <- read_html(joblink, encoding="UTF-8") %>%
    html_elements(xpath= '//div[(@class="job-description job-description-text ")]') %>% 
    html_text2()
  return(description)
}


# salary, industry, etc in a little box
get_details_box <- function(job_link) {
  details <- read_html(job_link, encoding="UTF-8") %>% 
    html_elements(xpath= '//div[(@class="job-view-details")]') %>% 
    html_text2()
  return(details)
}


# Get job links

# Main loop gets the job links for up to 10 pages in each location.
# Store the all the links for each location in a dataframe.


my_func <- function(df,location) {   # call for each location
  all_links <- c() # all links for one location
  for (p in 1:MAX_PAGES) {
    url <- buildSearch(base_search_url, location, p)
    links <- read_html(url, encoding="UTF-8") %>% 
      html_elements(xpath='//a[@class="job-card-title"]') %>% 
      html_attr("href")
    
    all_links <- c(all_links, links)
    
  }
  
  ## get jobs at this location
  num_jobs <- length(all_links)
  print(c("Number of jobs for this city: ", num_jobs))
  count <-0
  for (joblinkpiece in all_links) {
    a <- buildJobLink(base_job_link, joblinkpiece)
    title <- get_title(a)
    description <- get_description(a)
    description <- gsub("\\n", " ", description)  # remove newlines or else this info will be ommitted in the csv file.
    details_box <- get_details_box(a)
    details_box <- gsub("\\n", " ", details_box)
    
    tryCatch(df[nrow(df)+1, ] <- c(title=title, location=location, details=details_box, description=description),
             error = function(e) {
               df[nrow(df)+1, ] <- c(title=NA, location=NA, details=NA, description=NA)
             }
    ) 
    
    
    
    count <- count + 1
    print(c("Job#", count))
  }
  
  return(df)
}


# Initialize empty data frame
pre_df <- data.frame(title=character(), location=character(), details=character(), description=character())
for (location in locations) {
  pre_df <- my_func(pre_df, location)
}



View(pre_df)





















