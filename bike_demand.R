library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(readr)
library(gridExtra)
library(xgboost)
library(Metrics)
# install.packages("Metrics")

# train_set 및 test_set 할당
train_set <-read.csv("train.csv")
test_set <- read.csv("test.csv")
SampleSubmission <- read.csv("sampleSubmission.csv")

# 목적
# 주변환경(온도,시간대)에 따른 자전거 대여에 대한 수요 예측

# 탐색적 데이터 분석 

# 12개 열 / datetime(factor) - season(int) - holiday(int) -  workingday(int) - temp(num) - 
# atemp(num) - humidity(int) - windspeed(num) - casual(int) - registered(int) - count(int)
str(train_set) 
summary(train_set)
sum(is.na(train_set))


str(test_set) # 9개 열 

# remove casual & registered 

train_set <- train_set %>% 
  select(-casual, -registered) %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    hour = hour(datetime),
    minute = minute(datetime)) %>% 
  mutate(group = sample(
    c("train", "vailid"),
    size = nrow(train_set),
    replace = TRUE,
    prob = c(0.7, 0.3)
  ))


train_set_2 <- train_set

train_set$season <- as.factor(train_set$season)
levels(train_set$season) <- c("Spring", "Summer", "Fall", "Winter")
train_set$season


test_set <- test_set %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    hour = hour(datetime),
    minute = minute(datetime))

# vailied_set <- train_set %>% 
#   filter(group == "valid")
# 
# train_set <- train_set %>% 
#   filter(group == "train")

dim(train_set)


# # remove casual registered
# train_set <- train_set %>% 
#   select(-casual, -registered) %>%  
#   mutate(
#     year = year(datetime),
#     month = month(datetime),
#     hour = hour(datetime),
#     minute = minute(datetime)) %>% 
#   mutate(group = sample(
#     c("train", "valid"),
#     size = nrow(train_set),
#     replace = TRUE,
#     prob = c(0.7, 0.3) # Set weights for each group here
#   ))
# 
# train_set_2 <- train_set
# 
# train_set$season <- as.factor(train_set$season)
# levels(train_set$season) <- c('Spring','Summer','Fall','Winter')

# test_set <- test_set %>% 
#   mutate(
#     year = year(datetime),
#     month = month(datetime),
#     hour = hour(datetime),
#     minute = minute(datetime))
# 
# valid_set <- train_set %>% 
#   filter(group == "valid")
# 
# 
# train_set <- train_set %>% 
#   filter(group == "train")

str(train_set)

colnames(train_set)


head(train_set)


aa <- ggplot(data = train_set, aes(temp,count)) +
  geom_point(alpha=.2,aes(color=temp)) +
  ggtitle("Count vs Temperature") + xlab("Temp (Celsius)") +
  ylab("Rental Count") + labs(color='Temp(C)') +
  theme_bw() +  
  theme(legend.position = "bottom")


# Scatter Plot to show the relationship between count (number of total rentals) and date time.
bb <- ggplot(data = train_set, aes(datetime,count)) +
  geom_point(alpha = .2,aes(color=temp)) +
  scale_colour_continuous(low = "yellow", high = 'red') + theme_bw() +
  ggtitle("Count vs Datetime") + 
  xlab("Date") + 
  ylab("Rental Count") +
  labs(color='Temp(C)') + 
  theme(legend.position = "bottom")

grid.arrange(aa, bb, ncol=2)

ggplot(data=train_set,aes(season,count,color = season)) +
  geom_boxplot( alpha = .2) + 
  ggtitle("Rental count by season") + 
  xlab("Season") +
  ylab("Rental Count") +
  labs(color='Season', labels=c("Spring","Summer","Fall","Winter")) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(filter(train_set,workingday == 1), aes(hour,count)) + 
  geom_point()


aa <- train_set %>% 
  filter(workingday == 1) %>%
  ggplot(aes(hour,count)) +
  geom_point( alpha = .5,position = position_jitter(w=1,h=0),aes(color=temp)) +
  scale_color_gradientn(colors = c('blue','green','yellow','orangered','red')) +
  ggtitle("workingday Rental Count") + xlab("Hour") + ylab("Rental Count") +
  labs(color='Temp(C)') +  
  theme(legend.position = "bottom")

bb <- train_set %>% 
  filter(workingday == 0) %>%
  ggplot(aes(hour,count)) +
  geom_point( alpha = .5,position = position_jitter(w=1,h=0),aes(color=temp)) +
  scale_color_gradientn(colors = c('blue','green','yellow','orangered','red')) +
  ggtitle("Weekday Rental Count") + xlab("Hour") + ylab("Rental Count") + 
  labs(color='Temp(C)') +  
  theme(legend.position = "bottom")


grid.arrange(aa,bb, ncol=2)
