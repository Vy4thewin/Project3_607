library(tidyverse)
library(tidytext)
library(stopwords)
library(tm)
library(wordcloud2)
library(dplyr)
library(stringr)
library(forcats)

#Data frame #2, Data from Simply Hired

df2<-read.csv("https://raw.githubusercontent.com/djunga/project3-most-valued-data-skills/main/scraped2.csv",header = TRUE)
df2$qualifications<-str_replace_all(df2$qualifications," ","")
df2$qualifications<-str_replace_all(df2$qualifications,"%"," ")
glimpse(df2)
#To keep terms with two words and more, we combine them to keep items like [bachelor's degree] together and not [degree,bachelor's]

df2_words<-df2%>%unnest_tokens(words,qualifications)%>% count(words, sort = TRUE)
ed<-c("of","a","b","e","or","ci","cd")
df2_words<-df2_words%>% filter(!(words %in% ed))
print(df2_words[1:10,])

#We got a final product of simply hired's qualification list for data scientists/analysts
# Cannot do analysis on title(too many unique titles) there is no unique identifier in simply hired site like top ladder


#Analysis| Difference in skills based on the salary
df2_s<-df2%>%unnest_tokens(words,qualifications)%>% count(salary,words, sort = TRUE)
ed<-c("of","a","b","e","or")
df2_s<-df2_s%>% filter(!(words %in% ed))
salary<-df2_s%>% filter(grepl("Estimated",df2_s$salary))

salary<-salary%>%separate(salary,sep = "-",c("min","max"))
salary$min<-str_replace(salary$min,"Estimated:"," ")
salary$max<-str_replace(salary$max," a year"," ")

salary <- salary %>% group_by(max) %>% count(words, sort = TRUE)
by_salary<- salary %>% bind_tf_idf(words,max,n)

l1<-c("$65,000"," $75,000 " ," $89,000 "," $83,000 " ," $93,000 "," $98,000 "," $96,000 " ," $100,000 " ," $110,000 ")
l2<-c(" $120,000 "," $130,000 "," $140,000 "," $150,000 "," $160,000 "," $170,000 "," $180,000 "," $190,000 "," $200,000 " )
by_salary_1<-by_salary%>%filter(max %in% l1)
by_salary_2<-by_salary%>%filter(max %in% l2)

by_salary_1%>%
  group_by(max) %>%
  slice_max(tf_idf, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = max)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~max, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

by_salary_2%>%
  group_by(max) %>%
  slice_max(tf_idf, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = max)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~max, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

##########################################################################################################################

# Word cloud without degrees
top.oneH <- df2_words[1:100,] 
top.fifty <- df2_words[1:50,] %>% 
filter(!(words == "bachelor'sdegree")) %>%
filter(!(words == "bachelorofscience")) %>%
filter(!(words == "masterofscience")) %>%
filter(!(words == "master'sdegree")) %>%
filter(!(words == "doctoraldegree")) %>%
filter(!(words == "doctorofphilosophy"))

wordcloud2(top.fifty)

# Top 5 Words facted by location
location_words <- df2 %>% 
  unnest_tokens(words,qualifications)%>% count(location,words, sort = TRUE)

colnames(location_words)[3] <- "count"

location_words %>%
  group_by(location) %>%
  slice_max(count, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(count, fct_reorder(words, count), fill = location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~location, ncol = 2, scales = "free") +
  labs(x = "count", y = NULL)

location_words_no_degree <- location_words %>% 
  filter(!(words == "bachelor'sdegree")) %>% 
  filter(!(words == "bachelorofscience")) %>% 
  filter(!(words == "master'sdegree")) %>% 
  filter(!(words == "doctoraldegree")) %>% 
  filter(!(words == "doctorofphilosophy")) %>% 
  arrange(location)

location_words_no_degree %>%
  group_by(location) %>%
  slice_max(count, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(count, fct_reorder(words, count), fill = location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~location, ncol = 2, scales = "free") +
  labs(x = "count", y = NULL)



