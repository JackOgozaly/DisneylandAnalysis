#Disney Analysis
#By Jack Ogozaly

#This script is an analysis of disneyland reviews, seeking to undersand how 
#Disneyland is being perceived via reviews on tripadvisor

#The main data used in this script is available on kaggle here: 
#https://www.kaggle.com/arushchillar/disneyland-reviews

#The only other data is an excel sheet with the list of ride names available on my 
#github



#______________________________Environment______________________________________

#Built on 
#R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19042)

#Using the packages
library(tidyverse) #used for everything
library(tm)        #Used for NLP 
library(tidytext)  #used for NLP 
library(openxlsx)  #Used to read in data
library(ggthemes)  #Used to make graphs look good

#Package versions
# ggthemes_4.2.4  openxlsx_4.2.3  tidytext_0.3.0  tm_0.7-8        NLP_0.2-1       forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2    
# purrr_0.3.4     readr_1.4.0     tidyr_1.1.2     tibble_3.0.4    ggplot2_3.3.3   tidyverse_1.3.0


#_______________________________Data Setup______________________________________

#Read in our data
setwd("C:/Users/jogoz/OneDrive/Desktop/ISOM Spring 2021/Data Visualization/New Final Project")
disney_data <- read_csv("DisneyLandReviews.csv")

#Some names are not correctly formatted, so first we change that
disney_data$Reviewer_Location <-  iconv(disney_data$Reviewer_Location, 
                                        from = 'UTF-8', to = 'ASCII//TRANSLIT')
#Remove any NAs in our data
disney_data <- na.omit(disney_data)

#________________________________Exploratory Analysis___________________________

#What is each branch's average rating?
disney_data %>%
  group_by(Branch) %>% 
  summarise(avg_rating = mean(Rating))

#Check our distribution of Ratings
ggplot(data = disney_data, aes(as.numeric(Rating)))+
  geom_histogram(fill="navyblue", bins = 5) + xlab("\nRating") + 
  ggtitle("Distribution of Disneyland Review Ratings")+ theme_minimal()

#Average Yearly Rating by Park
disney_data %>% 
  filter(Year_Month != "missing") %>% 
  mutate(year= substr(Year_Month, 1, 4)) %>%
  group_by(year, Branch) %>% 
  summarise(avg_score=mean(Rating)) %>% 
  ggplot(aes(x=year, y=avg_score))+geom_col(aes(fill=Branch)) +
  facet_wrap(~Branch, scales = "free") + theme_minimal()+ 
  theme(legend.position = "null", plot.title = element_text(hjust=.5), text = element_text(size=20)) + 
  xlab("\nYear") + ylab("Averge Rating\n") + 
  ggtitle("Average Yearly Rating By Park\n") + 
  facet_wrap(~Branch, scales = "free")

#Average Monthly Rating By Park
disney_data %>% 
  filter(Year_Month != "missing") %>% 
  mutate(month= substr(Year_Month, 6, 7)) %>%
  group_by(month, Branch) %>% 
  summarise(avg_score=mean(Rating)) %>% 
  ggplot(aes(x=as.numeric(month), y=avg_score))+geom_col(aes(fill=Branch)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  theme_minimal()+ theme(legend.position = "null", 
                         plot.title = element_text(hjust=.5), 
                         text = element_text(size=20)) + 
  xlab("\nMonth") + ylab("Averge Rating\n") + 
  ggtitle("Average Monthly Rating By Park\n") + 
  facet_wrap(~Branch, scales = "free")

#__________________________________Complex Analysis 1____________________________

#What we want to know is how often are certain rides being talked about in reviews.

#To do this we use fuzzy matching with a dataset I made containing every ride at 
#each Disneyland Branch

ride_data <- read.xlsx("Rides_Data.xlsx")

#Filter for Cali
cali_ride_data <- filter(ride_data, Park=="Disneyland California")
cali_data <- filter(disney_data, Branch=="Disneyland_California")
dataset <- cali_data$Review_Text

#Count cali ride popularity
rides <- cali_ride_data$Attraction
i=1
result <- vector("list", length(rides))
while (i <= length(rides)) { 
  ride_to_search <- rides[i]
  result[[i]] <- length(test <- agrep(ride_to_search,dataset,value=T))
  i = i+1
}
#Collect results
df <- data.frame(matrix(unlist(result), nrow=length(result), byrow=TRUE))
cali_ride_data$occurence_count <- df$matrix.unlist.result...nrow...length.result...byrow...TRUE.


#Filter for Paris
paris_ride_data <- filter(ride_data, Park=="Disneyland Paris")
paris_data <- filter(disney_data, Branch=="Disneyland_Paris")
dataset <- paris_data$Review_Text

#Count Paris ride popularity
rides <- paris_ride_data$Attraction
i=1
result <- vector("list", length(rides))
while (i <= length(rides)) { 
  ride_to_search <- rides[i]
  result[[i]] <- length(test <- agrep(ride_to_search,dataset,value=T))
  i = i+1
}
#Collect Results
df <- data.frame(matrix(unlist(result), nrow=length(result), byrow=TRUE))
paris_ride_data$occurence_count <- df$matrix.unlist.result...nrow...length.result...byrow...TRUE.


#Filter for Hong Kong
hong_kong_ride_data <- filter(ride_data, Park=="Hong Kong Disneyland")
hk_data <- filter(disney_data, Branch=="Disneyland_HongKong")
dataset <- hk_data$Review_Text

#Count paris ride popularity
rides <- hong_kong_ride_data$Attraction
i=1
result <- vector("list", length(rides))
while (i <= length(rides)) { 
  ride_to_search <- rides[i]
  result[[i]] <- length(test <- agrep(ride_to_search,dataset,value=T))
  i = i+1
}
#Collect Results
df <- data.frame(matrix(unlist(result), nrow=length(result), byrow=TRUE))
hong_kong_ride_data$occurence_count <- df$matrix.unlist.result...nrow...length.result...byrow...TRUE.

#Bring it all together
all_ride_data <- rbind(hong_kong_ride_data, paris_ride_data, cali_ride_data)
all_ride_data <- 
  all_ride_data[order(all_ride_data$occurence_count, decreasing = TRUE),]

#Visualize most common rides at each branch 
all_ride_data %>% 
  group_by(Park) %>% 
  top_n(n=12)%>%
  ggplot(mapping=aes(reorder(Attraction, occurence_count), y=occurence_count)) + 
  geom_col(aes(fill=Park))+ coord_flip() + xlab("Ride") + ylab("\nCount") + 
  ggtitle("Most Talked About Rides") + theme_fivethirtyeight()+ 
  theme(plot.title = element_text(hjust = .5), legend.position = "null")+ facet_wrap(~Park, scales="free")


#__________________________________Complex Analysis 2____________________________

#Most common two words by location and rating

#Create new object ngram_data that has our review data
ngram_data <- disney_data
ngram_data$Rating <- as.factor(ngram_data$Rating)

#Clean our text field 
ngram_data$clean_text <- ngram_data$Review_Text %>% 
  removeNumbers() %>% 
  tolower() %>% 
  removePunctuation() %>% 
  removeWords(stop_words$word)

#Make bigrams by branch and rating
bigram_by_branch_rating <- ngram_data %>%
  group_by(Branch, Rating) %>%
  unnest_tokens(word, clean_text,token = "ngrams", n = 2) %>% 
  dplyr::count(word, sort = TRUE)%>%
  filter(row_number() <= 10)

#Plot of common words per rating for Cali
bigram_by_branch_rating %>% 
  filter(Branch=="Disneyland_California")%>% 
  group_by(Rating) %>% 
  ggplot(mapping = aes(reorder_within(word, n, Rating), y=n))+ 
  geom_col(aes(fill=Rating)) + 
  coord_flip()+ scale_x_reordered()+
  facet_wrap(~Rating, scales = "free")+ theme_minimal()+ xlab("Bigrams\n")+
  ylab("\nCount") + 
  ggtitle("Most Common 2 Words Used in Disneyland California Reviews by Rating")+
  theme(legend.position = "null", plot.title = element_text(hjust = .5)) 


#__________________________________Complex Analysis 3____________________________

#sentiment Analysis: Find the most common negative words for all parks
nrc_sent <- tibble(get_sentiments("nrc"))
text <- ngram_data$clean_text
text_df <- tibble(text)
text_df$row_num <- seq(1:length(text))
text_df <- text_df %>%
  group_by(row_num) %>%
  unnest_tokens(word, text)

negative_words <- text_df %>%
  left_join(nrc_sent) %>% 
  filter(sentiment=="anger"|sentiment=="negative") %>% 
  group_by(word) %>% 
  count(word, sort = T) %>% 
  filter(word !="parade"& word !="haunted" & word !="buffet"&
           word !="treat" & word!="blast")

#Plot the most common negative words about Disneyland
negative_words %>% 
  head(20) %>% 
  ggplot(mapping = aes(reorder(word, n),n)) + 
  geom_col(fill="darkred")+ coord_flip() + xlab("Negative Word") + ylab("\nCount")+ 
  ggtitle("Most Common Negative Words in Disneyland Reviews") + theme_minimal()+ 
  theme(plot.title = element_text(hjust=.5))


#__________________________________Complex Analysis 4____________________________

#What are the positive things people are saying about the parks?

#Find which rows are are for Cali
cali_index <- ngram_data %>% 
  mutate(ID= seq(1:nrow(ngram_data))) %>% 
  filter(Branch=="Disneyland_California")
cali_index <- cali_index$ID

#Find which rows are for Paris
paris_index <- ngram_data %>% 
  mutate(ID= seq(1:nrow(ngram_data))) %>% 
  filter(Branch=="Disneyland_Paris")
paris_index <- paris_index$ID

#Filter only for positive words
positive_words <- text_df %>%
  left_join(nrc_sent)%>% 
  filter(sentiment=="positive"|sentiment=="joy")

#Create a new park column
positive_words$park <- ifelse(positive_words$row_num %in% cali_index, "Disneyland California", 0)
positive_words$park <- ifelse(positive_words$row_num %in% paris_index, "Disneyland Paris", positive_words$park)
positive_words$park <- ifelse(positive_words$park == "Disneyland Paris"| positive_words$park== "Disneyland California", 
                              positive_words$park, "Hong Kong Disneyland")

#Count positive words by park and word
positive_words <- positive_words %>% 
  group_by(park, word) %>% 
  count(word, sort = T)

#Plot the 20 most common positive words per park
positive_words %>% 
  group_by(park) %>% 
  top_n(20)%>% 
  ggplot(mapping = aes(reorder_within(word, n, park), y=n)) + geom_col(aes(fill=park))+ coord_flip()+ 
  scale_x_reordered()+ facet_wrap(~park, scales="free") +
  xlab("Positive Word\n")+ylab("\nCount") + ggtitle("Most Common Positive Words Per Park\n")+
  theme(legend.position = "null", plot.title = element_text(hjust = .5))


#__________________________________Complex Analysis 5____________________________

#What are the negative things people are saying about the parks? 

#Only select negative words
negative_words <- text_df %>%
  left_join(nrc_sent)%>% 
  filter(sentiment=="anger"|sentiment=="negative") %>% 
  filter(word !="parade"& word !="haunted" & word !="buffet"&
           word !="treat" & word!="blast")
#Create a park column again
negative_words$park <- ifelse(negative_words$row_num %in% cali_index, "Disneyland California", 0)
negative_words$park <- ifelse(negative_words$row_num %in% paris_index, "Disneyland Paris", negative_words$park)
negative_words$park <- ifelse(negative_words$park == "Disneyland Paris"| negative_words$park== "Disneyland California", 
                              negative_words$park, "Hong Kong Disneyland")

#Count negative words by park and word
negative_words <- negative_words %>% 
  group_by(park, word) %>% 
  count(word, sort = T)

#Plot the 20 most common negative words per park
negative_words %>%
  group_by(park) %>% 
  top_n(20)%>% 
  ggplot(mapping = aes(reorder_within(word, n, park), y=n)) + geom_col(aes(fill=park))+ coord_flip()+ 
  scale_x_reordered()+ facet_wrap(~park, scales="free") +
  xlab("Positive Word\n")+ylab("\nCount") + ggtitle("Most Common Negative Words Per Park\n")+
  theme(legend.position = "null", plot.title = element_text(hjust = .5))