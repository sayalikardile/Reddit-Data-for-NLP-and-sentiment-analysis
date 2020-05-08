
#install.packages("RedditExtractoR")

install.packages("textdata")



rm(list = ls())


library(RedditExtractoR)
library(tidyverse) 
library(plyr)
library(dplyr) 
library(wordcloud)
library(tidytext) 
library(ggplot2)
library(stringr) 
library(dplyr)
library(plyr)



library(randomForest) 
library(neuralnet)
library(tidyverse)
library(forcats)
library(kernlab)
library(ggplot2) 
library(xgboost)
library(rpart)
library(tidyr)
library(dplyr) 
library(psych) 
library(caret)
library(plyr)
library(nnet)

?count
#because there is a return limit at each search querry, use loop to run a different search by the subgroup from reddit

subred<-c("movies","whatisthisthing
","AskReddit
","SelfAwarewolves
","YangForPresidentHQ
","modernwarfare
","legaladvice
","IdiotsInCars
","Futurology
","WatchPeopleDieInside
","TheMonkeysPaw
","gatekeeping
","AskMen
","quityourbullshit
","news
","personalfinance
","PublicFreakout
","Instagramreality
","StrangerThings
","PresidentialRaceMemes
","changemyview
","trashy
","MMA
","YouShouldKnow
","aww
","holdmycosmo
","europe
","soccer
","PoliticalHumor
","SubredditDrama
","pics
","nevertellmetheodds
","southcarolina
","dontputyourdickinthat
","Wolcen
","Whatcouldgowrong
","rareinsults
","ActualPublicFreakouts
","Damnthatsinteresting
","FloridaMan
","RoastMe
","AbandonedPorn","Tinder","OurPresident","technology","BlackPeopleTwitter","TooAfraidToAsk","holdmyredbull","thedivision","Music")



total<- {}
for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "election",
    subreddit = x ,
    page_threshold = 20,)
    total <- rbind(total,reddit_links_1)
}

for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "AI",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}


for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "machine learning",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}


for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "coronavirus",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}

for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "Greta Thunberg",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}

for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "enviroment",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}

for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "flu",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}

for (x in subred) {
  
  reddit_links_1 <- reddit_urls(   
    search_terms   = "movie",
    subreddit = x ,
    page_threshold = 20,)
  total <- rbind(total,reddit_links_1)
}







getwd()




save(total,file="total.Rda")
load("total.Rda")



#only keep title words and num of comments as target value
df <- total[,c("num_comments","title")]
####################################etst

#find sentiment analysis from title first


#Data Cleaning

df <- df %>% mutate(title = as.character(title))


#count words in each title for future use
df <- df %>% mutate(n= sapply(strsplit(df$title, " "), length)  )

tokens <- df %>%  
  unnest_tokens(output = word, input = title) 

tokens %>%  dplyr::count(word, sort = TRUE)
length(unique(tokens$word))

sw = get_stopwords()

cleaned_tokens <- tokens %>%  
  filter(!word %in% sw$word)

nums <- cleaned_tokens %>%   
  filter(str_detect(word, "^[0-9]")) %>%   
  select(word) %>% unique()


cleaned_tokens <- cleaned_tokens %>%   
  filter(!word %in% nums$word)

cleaned_tokens <- cleaned_tokens %>%   
  filter(!grepl("[^\u0001-\u007F]+",cleaned_tokens$word))

cleaned_tokens %>% dplyr::count(word, sort = TRUE)


length(unique(cleaned_tokens$word))


#Word Frequency Map


cleaned_tokens %>%   
  dplyr::count(word, sort = TRUE) %>%  
  dplyr::rename(word_freq = n) %>%  
  ggplot(aes(x=word_freq)) +  
  geom_histogram(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_x_continuous(breaks=c(0:5,10,100,500,10e3), 
                     trans="log1p", expand=c(0,0)) +  
  scale_y_continuous(breaks=c(0,100,1000,5e3,10e3,5e4,10e4,4e4), 
                     expand=c(0,0)) +  theme_bw()

rare <- cleaned_tokens %>%   
  dplyr::count(word) %>%  
  filter(n<10) %>%  
  select(word) %>% unique() 
rare

cleaned_tokens <- cleaned_tokens %>%   
  filter(!word %in% rare$word) 

length(unique(cleaned_tokens$word))


#Word Frequency Map


pal <- brewer.pal(8,"Dark2") 
cleaned_tokens %>%   
  dplyr::count(word) %>%  
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))



#Sentiment Analysis

sent_reviews = cleaned_tokens %>%   
  left_join(get_sentiments("nrc")) %>%  
  rename(replace=c("sentiment"="nrc")) %>%
  left_join(get_sentiments("bing")) %>%  
  rename(replace=c("sentiment"="bing")) %>%
  left_join(get_sentiments("afinn")) %>%  
  rename(replace=c("value"="affin"))


?rename
bing_word_counts <- sent_reviews %>%  
  filter(!is.na(bing)) %>%  
  dplyr::count(word,bing, sort = TRUE) 

bing_word_counts %>%  
  filter(n > 5) %>%  
  mutate(n = ifelse(bing == "negative", -n, n)) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n, fill = bing)) +  
  geom_col() +  coord_flip() +  
  labs(y = "Contribution to sentiment")

sent_reviews %>%  
  filter(!is.na(bing)) %>%  
  dplyr::count(bing, sort = TRUE) 



#Sentiment Analysis on words related to the number of views



high_hit_word_list<- cleaned_tokens %>%
  ddply("word",summarize,mean=round(mean(num_comments))) %>%
  arrange(desc(mean))


sent_reviews = high_hit_word_list %>%   
  left_join(get_sentiments("nrc")) %>%  
  rename(replace=c("sentiment"="nrc")) %>%
  left_join(get_sentiments("bing")) %>%  
  rename(replace=c("sentiment"="bing")) %>%
  left_join(get_sentiments("afinn")) %>%  
  rename(replace=c("value"="affin"))


sent_reviews

bing_word_counts <- sent_reviews %>%  
  filter(!is.na(bing))

#average hits > #
bing_word_counts %>%  
  filter(mean > 20) %>%  
  mutate(mean = ifelse(bing == "negative", -mean, mean)) %>%  
  mutate(word = reorder(word, mean)) %>%  
  ggplot(aes(word, mean, fill = bing)) +  
  geom_col() +  coord_flip() +  
  labs(y = "number of hits")





#try to find how to improve number of comments

#combine same words in same title, count how many times it appers in this title


df4<-cleaned_tokens %>%
  mutate(count=1)

df5<-ddply(df4,c("word"),numcolwise(sum))


df6<-df5 %>%    
  spread(key = word, value = count)


df6[is.na(df6)] <- 0

df6




################cant get result, try scale num
df6$num_comments<-df6$num_comments/1000



df6$num_comments<-ifelse(df6$num_comments>10,10,df6$num_comments)
  
df6$num_comments<-floor(df6$num_comments)

df6$num_comments




############################################################################

#recc_data_train= read.csv("recc_data_train.csv", stringsAsFactors = FALSE)
#recc_data_valid= read.csv("recc_data_valid.csv", stringsAsFactors = FALSE)



bank.df <- df6
bank.df$SalePrice<-df6$num_comments


# Select variables
selected.var <- c(1:994)

count(valid.df$wrong)

str(valid.df)

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, selected.var]
valid.df <- bank.df[-train.index, selected.var]




# use lm() to run a linear regression of Price on all 11 predictors in the training set. 
car.lm <- lm(num_comments ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
table1<-summary(car.lm)   # Get model summary


#get something but not good enough, change another way















#######################################################################

#try H2O with scaling

df6$num_comments<-floor(df6$num_comments)

df6$num_comments<-ifelse(df6$num_comments>4,1,0)

#split dataset
which_train <- sample(x = c(TRUE, FALSE), size = nrow(df6),
                      replace = TRUE, prob = c(0.8, 0.2))

recc_data_train <- df6[which_train, ]
recc_data_valid <- df6[!which_train, ]

length(recc_data_train[[1]])
length(recc_data_valid[[1]])


x_train_processed_tbl <- recc_data_train %>% select(-num_comments)
y_train_processed_tbl <- recc_data_train %>% select(num_comments) 

x_test_processed_tbl  <- recc_data_valid




####################################################################################
library(h2o)
library(tidyverse)

h2o.init(nthreads = -1)

h2o.clusterInfo()



data_h2o <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl),
  destination_frame= "train.hex" #destination_frame is optional
)
new_data_h2o <- as.h2o(
  x_test_processed_tbl,
  destination_frame= "test.hex" #destination_frame is optional
)


h2o.ls()


# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o,
                         ratios = c(0.7, 0.15), # 70/15/15 split
                         seed = 1234)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o <- splits[[3]] # from training data




#deep learning in H2o

y <- "num_comments" # column name for outcome
x <- setdiff(names(train_h2o), y) # column names for predictors
m1 <- h2o.deeplearning(
  model_id = "dl_model_first",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o, ## validation dataset: used for scoring and
  ## early stopping
  #activation="Rectifier", ## default
  #hidden=c(200,200), ## default: 2 hidden layers, 200 neurons each
  epochs = 1 ## one pass over the training data
)


summary(m1)





################################################################################
#try H2O without scaling

df6<-df5 %>%    
  spread(key = word, value = count)


df6[is.na(df6)] <- 0


#split dataset
which_train <- sample(x = c(TRUE, FALSE), size = nrow(df6),
                      replace = TRUE, prob = c(0.8, 0.2))

recc_data_train <- df6[which_train, ]
recc_data_valid <- df6[!which_train, ]

length(recc_data_train[[1]])
length(recc_data_valid[[1]])


x_train_processed_tbl <- recc_data_train %>% select(-num_comments)
y_train_processed_tbl <- recc_data_train %>% select(num_comments) 

x_test_processed_tbl  <- recc_data_valid




h2o.init(nthreads = -1)

h2o.clusterInfo()



data_h2o <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl),
  destination_frame= "train.hex" #destination_frame is optional
)
new_data_h2o <- as.h2o(
  x_test_processed_tbl,
  destination_frame= "test.hex" #destination_frame is optional
)


h2o.ls()


# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o,
                         ratios = c(0.7, 0.15), # 70/15/15 split
                         seed = 1234)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o <- splits[[3]] # from training data




#deep learning in H2o

y <- "num_comments" # column name for outcome
x <- setdiff(names(train_h2o), y) # column names for predictors
m1 <- h2o.deeplearning(
  model_id = "dl_model_first",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o, ## validation dataset: used for scoring and
  ## early stopping
  #activation="Rectifier", ## default
  #hidden=c(200,200), ## default: 2 hidden layers, 200 neurons each
  epochs = 1 ## one pass over the training data
)


summary(m1)




######################################################################test test data

prediction_h2o_dl <- h2o.predict(m1,
                                 newdata = new_data_h2o)
prediction_dl_tbl <- tibble(
  SK_ID_CURR = x_test_processed_tbl$num_comments,
  TARGET = as.vector(prediction_h2o_dl$predict)
)



h2o.shutdown(prompt = F)

