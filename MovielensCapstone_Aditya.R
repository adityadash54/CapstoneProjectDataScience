##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "./ml-10M100K"
if(!file.exists(dl))
  print("yes")
  #download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "/Users/aditya/Documents/Coursework/Online_Courses/Machine_Learning_and_Deep_Learning/Data_Science_Edx_Harvard_R/Final_Project/Movielens/ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
      unzip(dl, ratings_file)


movies_file <- "/Users/aditya/Documents/Coursework/Online_Courses/Machine_Learning_and_Deep_Learning/Data_Science_Edx_Harvard_R/Final_Project/Movielens/ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

?num

#-------------------Mycode-------------------------#
if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)
if(!require(lubridate)) install.packages("lubridate")
library(lubridate)
if(!require(stringr)) install.packages("stringr")
library(stringr)
if(!require(ggcorrplot)) install.packages("ggcorrplot")
library(ggcorrplot)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)
if(!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)


#Data Preprocessing
str(edx)
head(edx,5)
length(unique(edx$userId))
length(unique(edx$movieId))
edx%>%group_by(movieId)%>%summarise(title=title,n=n())%>%arrange(n)
edx%>%group_by(rating)%>%summarise(n=n())%>%arrange(n)

#removing all rows with NA in userId, movieId, genres & rating 
edx<-edx%>%filter(!is.na(userId) & !is.na(movieId) & !is.na(rating) & !is.na(title) & !is.na(genres))
str(edx)

str_match("Boomerang (1992)", "(\\d{4})(\\))$")
edx<-edx%>%mutate(movie_year=as.numeric(str_match(title, "(\\d{4})(\\))$")[,2]))
min(edx$movie_year)
max(edx$movie_year)
edx%>% qplot(movie_year, geom ="histogram", bins = 30, data = ., color = I("black"))

#Creating one hot encoding for genres into different columns
#https://stackoverflow.com/questions/74706268/how-to-create-dummy-variables-in-r-based-on-multiple-values-within-each-cell-in
strsplit("Comedy|Romance", "\\|")
edx<-edx%>%mutate(rating_year=year(as_datetime(timestamp)))
edx<-edx%>%mutate(rating_month=month(as_datetime(timestamp)))
min(edx$rating_year)
max(edx$rating_year)
edx%>%group_by(rating_year)%>%summarise(n=n())%>%arrange(n)
edx%>% qplot(rating_year, geom ="histogram", bins = 30, data = ., color = I("black"))

edx%>%group_by(rating_month)%>%summarise(n=n())%>%arrange(-n)
edx%>% qplot(rating_month, geom ="histogram", bins = 30, data = ., color = I("black"))

edx<-edx %>% mutate(genreslist = strsplit(genres,"\\|")) %>% unnest(genreslist) %>% 
  pivot_wider(names_from = genreslist, values_from = genreslist, values_fn = length,values_fill = 0)

head(edx)
#year(as_datetime(edx$timestamp))
edx<-subset(edx,select=-c(genres))
edx%>%ggplot(aes(x=as.factor(Drama),y=rating))+geom_boxplot()
colSums(edx%>%select(-c(1,2,3,4,5,6,7)))%>%sort()
barplot(colSums(edx%>%select(-c(1,2,3,4,5,6,7)))%>%sort())

head(edx)

#Visualizing data
colnames(edx)
head(edx)




#ggcorr(training_set%>%select(is.numeric))

#edx%>%select(is.numeric) %>% ggplot(aes(movieId, rating, col = year)) + geom_point() + facet_grid(. ~ Drama)

#cor.table <- cor(training_set%>%select(is.numeric))
#print(cor.table)
#corrplot(cor.table)
head(edx)
colnames(edx)

#https://stackoverflow.com/questions/45898505/barplot-from-sums-of-columns-in-data-frame

#abs(cor(edx$rating,edx%>%select_if(is.numeric)))
#corrplot(edx%>%select_if(is.numeric))
#corrplot(abs(cor(edx$rating,edx%>%select_if(is.numeric))))

#Training machine learning model
#Dont use linear models as this consumes a lot of space
#train_glm <- train(rating ~ movieId, method = "glm", data = final_holdout_test)
#train_knn <- train(rating ~ movieId, method = "knn", data = edx)
#library(gam)
#grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
#train_loess <- train(rating ~ movieId, 
#                     method = "gamLoess",
#                     tuneGrid=grid,
#                     data = edx)

RMSE <- function(true_values, predicted_values){
  sqrt(mean((true_values - predicted_values)^2))
}
str(edx)


### Recommendation System
val_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.1, list = FALSE)
training_set <- edx[-val_index,]
#val_set <- edx[test_index_val,]

#We should not include users and movies in the val set that do not appear in the training set, we can remove it using the semi join function
val_set <- edx[val_index,] %>% 
  semi_join(training_set, by = "movieId") %>%
  semi_join(training_set, by = "userId")

removed <- anti_join(edx[val_index,], val_set)
training_set <- rbind(training_set, removed)
rm(removed)

# Model=mean
mu_hat <- mean(training_set$rating)
mu_hat
naive_rmse <- RMSE(val_set$rating, mu_hat)
naive_rmse
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#Movie effects
movie_avs <- training_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu_hat))

movie_avs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- mu_hat + val_set %>% 
  left_join(movie_avs, by='movieId') %>%
  pull(b_i)


#sum(is.na(predicted_ratings))

model_1_rmse <- RMSE(predicted_ratings, val_set$rating)
model_1_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results
rmse_results %>% knitr::kable()

#User Effects
user_avs <- training_set %>% 
  left_join(movie_avs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))
str(user_avs)
user_avs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- val_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, val_set$rating)
model_2_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

#Movie Year Effects
movie_year_avs <- training_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  group_by(movie_year) %>%
  summarise(b_my = mean(rating -mu_hat-b_i-b_u))
movie_year_avs %>% qplot(b_my, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- val_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  left_join(movie_year_avs, by='movie_year') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, val_set$rating)
model_3_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie+User+Movie Year Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results
rmse_results %>% knitr::kable()
head(edx)

#Rating Year Effects
rating_year_avs <- training_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  left_join(movie_year_avs, by='movie_year') %>%
  group_by(rating_year) %>%
  summarise(b_ry = mean(rating -mu_hat-b_i-b_u-b_my))
rating_year_avs %>% qplot(b_ry, geom ="histogram", bins = 70, data = ., color = I("black"))

predicted_ratings <- val_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  left_join(movie_year_avs, by='movie_year') %>%
  left_join(rating_year_avs, by='rating_year') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my+b_ry) %>%
  pull(pred)
model_4_rmse <- RMSE(predicted_ratings, val_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie+User+Movie Year+Rating Year Effects Model",  
                                     RMSE = model_4_rmse ))
rmse_results
rmse_results %>% knitr::kable()

cor(training_set$rating,training_set%>%select_if(is.numeric))
#Drama Effects
drama_avs <- training_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  left_join(movie_year_avs, by='movie_year') %>%
  left_join(rating_year_avs, by='rating_year') %>%
  group_by(Drama) %>%
  summarise(b_d = mean(rating - mu_hat - b_i - b_u -b_my-b_ry))
drama_avs
predicted_ratings <- val_set %>% 
  left_join(movie_avs, by='movieId') %>%
  left_join(user_avs, by='userId') %>%
  left_join(movie_year_avs, by='movie_year') %>%
  left_join(rating_year_avs, by='rating_year') %>%
  left_join(drama_avs, by='Drama') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my+b_ry+b_d) %>%
  pull(pred)
#print(predicted_ratings)
model_5_rmse <- RMSE(predicted_ratings, val_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + MovieYear+RatingYear+Drama Effects Model",  
                                     RMSE = model_5_rmse ))
rmse_results
rmse_results %>% knitr::kable()


#Regularization
#https://rpubs.com/christianakiramckinnon/MovieLens

lambda_regs<-seq(1,10,1)

RMSE_regs<-sapply(lambda_regs,function(lambda_reg){
  # Model=mean
  mu_hat <- mean(training_set$rating)
  
  #Movie effects
  movie_avs_reg <- training_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = sum(rating - mu_hat)/(n()+lambda_reg))

  predicted_ratings <- mu_hat + val_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  pull(b_i)

  #User Effects
  user_avs_reg <- training_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_hat - b_i)/(n()+lambda_reg))

  predicted_ratings <- val_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
  
  #Movie Year Effects
  movie_year_avs_reg <- training_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  group_by(movie_year) %>%
  summarise(b_my = sum(rating -mu_hat-b_i-b_u)/(n()+lambda_reg))

  predicted_ratings <- val_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my) %>%
  pull(pred)

  #Rating Year Effects
  rating_year_avs_reg <- training_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  group_by(rating_year) %>%
  summarise(b_ry = sum(rating -mu_hat-b_i-b_u-b_my)/(n()+lambda_reg))

  predicted_ratings <- val_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  left_join(rating_year_avs_reg, by='rating_year') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my+b_ry) %>%
  pull(pred)
  
  #Drama Effects
  drama_avs_reg <- training_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  left_join(rating_year_avs_reg, by='rating_year') %>%
  group_by(Drama) %>%
  summarise(b_d = sum(rating - mu_hat - b_i - b_u -b_my-b_ry)/(n()+lambda_reg))

  predicted_ratings <- val_set %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  left_join(rating_year_avs_reg, by='rating_year') %>%
  left_join(drama_avs_reg, by='Drama') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my+b_ry+b_d) %>%
  pull(pred)

  model_reg_rmse <- RMSE(predicted_ratings, val_set$rating)
  
  return(model_reg_rmse)

})

qplot(lambda_regs,RMSE_regs)
names(RMSE_regs)<-lambda_regs
RMSE_regs%>%sort()

#Training on full set
lambda_reg=5.0 #value fo which RMSE is least on the validation set adter training on the training set
# Model=mean
mu_hat <- mean(edx$rating)

#Movie effects
movie_avs_reg <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = sum(rating - mu_hat)/(n()+lambda_reg))

#User Effects
user_avs_reg <- edx %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_hat - b_i)/(n()+lambda_reg))


#Movie Year Effects
movie_year_avs_reg <- edx %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  group_by(movie_year) %>%
  summarise(b_my = sum(rating -mu_hat-b_i-b_u)/(n()+lambda_reg))


#Rating Year Effects
rating_year_avs_reg <- edx %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  group_by(rating_year) %>%
  summarise(b_ry = sum(rating -mu_hat-b_i-b_u-b_my)/(n()+lambda_reg))

#Drama Effects
drama_avs_reg <- edx %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  left_join(rating_year_avs_reg, by='rating_year') %>%
  group_by(Drama) %>%
  summarise(b_d = sum(rating - mu_hat - b_i - b_u -b_my-b_ry)/(n()+lambda_reg))

#val on final holdout set
#Performing same data preprocessing on final holdout set
final_holdout_test<-final_holdout_test%>%mutate(movie_year=as.numeric(str_match(title, "(\\d{4})(\\))$")[,2]))
final_holdout_test<-final_holdout_test%>%mutate(rating_year=year(as_datetime(timestamp)))
final_holdout_test<-final_holdout_test%>%mutate(rating_month=month(as_datetime(timestamp)))
final_holdout_test<-final_holdout_test %>% mutate(genreslist = strsplit(genres,"\\|")) %>% unnest(genreslist) %>% 
  pivot_wider(names_from = genreslist, values_from = genreslist, values_fn = length,values_fill = 0)
final_holdout_test<-subset(final_holdout_test,select=-c(genres,timestamp))

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avs_reg, by='movieId') %>%
  left_join(user_avs_reg, by='userId') %>%
  left_join(movie_year_avs_reg, by='movie_year') %>%
  left_join(rating_year_avs_reg, by='rating_year') %>%
  left_join(drama_avs_reg, by='Drama') %>%
  mutate(pred = mu_hat+b_i+b_u+b_my+b_ry+b_d) %>%
  pull(pred)

holdoutset_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
holdoutset_rmse



