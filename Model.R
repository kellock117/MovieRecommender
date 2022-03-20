# INFO411 Group P02: Lim Gyun Hyun, Lim Wei Han, Guok Mee Han, Chua Ki Min Clement, Phua Zhon, Choi Jae Won

library(dplyr)
library(tidyr)
library(randomForest)
library(caret)
library(plyr)
movie_wrating= read.csv("movie_wratings.csv")

#sample of full dataset
movie_wrating_10_per <- slice_sample(movie_wrating,n=0.001*nrow(movie_wrating))
dim(movie_wrating_10_per)
samp3 <- sample(nrow(movie_wrating_10_per), 0.8 * nrow(movie_wrating_10_per))
xtrain <- movie_wrating_10_per[samp3, ]
xtest <- movie_wrating_10_per[-samp3, ]

#trivial model
new_train <- xtrain %>%
select(userId,movieId,rating) %>%
group_by(userId,movieId) %>%
summarise_at(vars(rating),list(rating_average=mean))%>%
arrange(userId)
train_avg_rating<-round_any(mean(new_train$rating_average), 0.5)
sumx <- 0
for(i in 1:nrow(xtest)){
  y <- xtest[i,]["rating"]
  if (xtest[i,]["userId"] %in% c(new_train["userId"])){
    x <-new_train %>% filter(userId == xtest[i,]["userId"]) %>%  select(rating_average)
  }
  else if(xtest[i,]["movieId"] %in% c(new_train["movieId"])){
    x <-new_train %>% filter(userId == xtest[i,]["movieId"]) %>%  select(rating_average)
  }
  else{
    x <- train_avg_rating
    sumx = sumx + (x - max(as.numeric(y$rating)))^2
    next
  }
  sumx = sumx + (round_any(mean(x["rating_average"]),0.5) - max(as.numeric(y$rating)))^2
}
rmse_trivial<-sqrt(sumx/i)

#linear model
lm_model <- lm(rating~ ., data = xtrain)
summary(lm_model)
predictions <- lm_model %>% predict(xtest)
rmse_lr<-RMSE(predictions, xtest$rating)
r2_lr<-R2(predictions, xtest$rating)
plot(lm_model)

#random forest model
rf_model <- randomForest(rating~ ., data = xtrain)
summary(rf_model)
predictions <- rf_model %>% predict(xtest)
rmse_rf<-RMSE(predictions, xtest$rating)
r2_rf<-R2(predictions, xtest$rating)
plot(rf_model)


