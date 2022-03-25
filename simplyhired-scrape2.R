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

base_search_url <- "https://www.simplyhired.com/search?q=data+scientist&l="
# https://www.simplyhired.com/search?q=data+scientist&l=San+Francisco%2C+CA&pn=1&job=VagwOEPZb_46y3f5ta3L4nBS2WSlN3wMcz2s9zDT6jbaeXpNJyraYA

buildSearch <- function(base, location, page) {
  location <-gsub("[,]", "%2C", location)   
  location <- gsub("[ ]", "+", location)
  page <- paste0("&pn=",page)
  url <- paste0(base, location, page)
  return(url)
}

##### The below functions are for getting the info in the job post:
base_job_link <- "https://www.simplyhired.com" 
buildJobLink <- function(base, link) {
  job_link <- paste0(base, link)
  return(job_link)
}

get_title <- function(html_page) {
  title <- html_page %>%
    html_elements(xpath= "//div[@class='viewjob-jobTitle h2']") %>%
    html_text2()
  
  if (identical(character(0), title) == TRUE) {
    return(NA)
  }
  return(title)
}

get_qualifications <- function(html_page) {
  quals <- html_page %>%
    html_elements(xpath= '//li[(@class="viewjob-qualification")]') %>%
    html_text2()
  
  if (identical(character(0), quals) == TRUE) {
    return(NA)
  }
  return(quals)
}

get_salary <- function(html_page) {  # estimated salary. Not always available. 
  salary <- html_page %>%
    #html_element(xpath= '//span[(@class="viewJob-labelWithIcon viewjob-salary")]') %>%
    html_element("span.viewjob-labelWithIcon:nth-child(1)") %>%
    html_text2()
  
  if (identical(character(0), salary) == TRUE) {
    return(NA)
  }
  return(salary)
}


# Get job links

# Main loop gets the job links for up to 10 pages in each location.
# Store the all the links for each location in a dataframe.

my_func <- function(df,location) {   # call for each location
  all_links <- c() # all links for one location
  for (p in 1:MAX_PAGES) {
    url <- buildSearch(base_search_url, location, p)
    links <- read_html(url, encoding="UTF-8") %>% 
      html_elements(xpath='//a[@class="SerpJob-link card-link"]') %>% 
      html_attr("href")
    
    Sys.sleep(3)   # To prevent Error 429 - too many requests
    
    all_links <- c(all_links, links)
    
  }
  
  ## get jobs at this location
  num_jobs <- length(all_links)
  print(paste0("Number of jobs for ", location, ": ", num_jobs))
  count <-1
  for (joblinkpiece in all_links) {
    print(paste0("Job#", count))
    
    a <- buildJobLink(base_job_link, joblinkpiece)
    
    job_html <- read_html(a)  # the whole html page for 1 job
    
    title <- get_title(job_html)
    
    qualifications <- paste0(c(get_qualifications(job_html)), collapse='%')
    
    salary <- get_salary(job_html)
    
    tryCatch(df[nrow(df)+1, ] <- c(title=title, location=location, salary=salary, qualifications=qualifications),
             error = function(e) {
               df[nrow(df)+1, ] <- c(title=NA, location=NA, salary=NA, qualifications=NA)
             }
    ) 
    
    count <- count + 1
    
    Sys.sleep(3)   # To prevent Error 429 - too many requests
  }
  
  return(df)
}


# Initialize empty data frame
pre_df <- data.frame(title=character(), location=character(), salary=character(), qualifications=character())
for (location in locations) {
  pre_df <- my_func(pre_df, location)
}

View(pre_df)










