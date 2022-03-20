# INFO411 Group P02: Lim Gyun Hyun, Lim Wei Han, Guok Mee Han, Chua Ki Min Clement, Phua Zhon, Choi Jae Won

ratings = read.csv("ml-20m/ratings.csv")
tags = read.csv("ml-20m/tags.csv")
genome_scores = read.csv("ml-20m/genome-scores.csv")
genome_tags = read.csv("ml-20m/genome-tags.csv")
links = read.csv("ml-20m/links.csv")
movies = read.csv("ml-20m/movies.csv")

library(dplyr)
library(tidyr)
library(Hmisc)



# in tag there are  38643 tags and 16 missing, 46554 rows and 4 columns
describe(tags)
dim(tags)
# 4000 different relevance and mean of 0.1165, total 1128 distinct tagid, 11709768 rows 3 cols
describe(genome_scores)
dim(genome_scores)
# 1128 distinct tag and tagid 1128 row 2 columns
describe(genome_tags)
dim(genome_tags)
# missing values in tmdbId, display imbd and tmdb for each movieId, 27278 unique move id, 27278 rows and 3 columns
describe(links)
dim(links)
# 27278 different movieid and 1342 distinct genres, movies and links have same size observation, 27278 rows and 3 columns
describe(movies)
dim(movies)
# 10 distinct rating, 20000263 rows and 4 columns
describe(ratings)
dim(ratings)

# Get only the latest user rating for each movie if there is multiple timestamp in a movieId for a user
new_ratings<-ratings %>% group_by(userId,movieId) %>% filter(timestamp==max(timestamp))
new_ratings

genome_s = merge(x=genome_scores, y=genome_tags, by="tagId")
describe(genome_s)
genome_max <- genome_s %>%
  group_by(movieId) %>%      # group your data on movieId
  arrange(desc(relevance)) %>% 
  filter(relevance>0.9)%>%     # order in descending
  slice(1)  #get first value


movie_gg = merge(x=movies, y=genome_max, by="movieId")

rm(genome_scores)
rm(genome_tags)
rm(genome_s)
rm(genome_max)


library(stringr)
genres = c()

#repeat the number of rows times in movies
for(i in 1:length(movie_gg$genres)){
  #split the string in genres by '|' and unlist
  gen = unlist(strsplit(movie_gg$genres[i], fixed = TRUE, split = '|'))
  #repeat the number of genres times. x is the name of genre
  for(x in gen){
    #if x is not in genres vector
    if(!x %in% genres){
      #put x in the vector
      genres = c(genres, x)
      #then append the column to movies dataframe which is initialized by 0
      movie_gg = cbind(movie_gg, rep(0, times = nrow(movie_gg)))
      #change the name of column to x
      colnames(movie_gg)[match(x, genres) + 6] = x
    }
    
    #replace the value to 1 where the row has the genre
    movie_gg[i, which(colnames(movie_gg) == x)] = 1
  }
}

movie_gg_year <- movie_gg %>%
  # trim whitespaces
  mutate(title = str_trim(title)) %>%
  # split title to title, year
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  # drop title_tmp column
  select(-title_tmp)


movie_gg_year %>% filter_all(any_vars(is.na(.))) 
# remove NA values
movie_gg_year<-na.omit(movie_gg_year)

movie_wratingsPP = merge(x=new_ratings, y=movie_gg_year, by="movieId")
movie_wratingsPP<-within(movie_wratingsPP,rm(relevance,tag,timestamp,genres,title))
sapply(movie_wratingsPP,class)
movie_wratingsPP<-movie_wratingsPP%>%select(-rating,everything())

for (i in 1:24){
  print(cor(movie_wratingsPP[25], movie_wratingsPP[i] ))
}

movie_wratingsPP <- movie_wratingsPP %>%
  group_by(userId) %>%
  filter(length(rating)>5)

movie_wratingsPP<-within(movie_wratingsPP,rm('(no genres listed)'))

#------------------------------------------------Start of Visualizations Segment

ratings_20 <- new_ratings %>%
  group_by(movieId) %>%
  filter(length(rating)>20)
ratings_20avg <- ratings_20 %>%
  select(movieId, rating) %>%
  group_by(movieId) %>%
  summarise_at(vars(rating), list(avg=mean)) %>%
  arrange(movieId)

#average rating by movieId from rating dataset
ratings_20avg
hist(ratings_20avg$avg, xlab="Rating", main="Movie Average")

uratings_20 <- new_ratings %>%
  group_by(userId) %>%
  filter(length(rating)>20) 
uratings_20avg <- uratings_20 %>%
  select(userId, rating) %>%
  group_by(userId) %>%
  summarise_at(vars(rating), list(avg=mean)) %>%
  arrange(userId)

#average rating by userId from rating dataset
uratings_20avg
hist(uratings_20avg$avg, xlab="Rating", main="User Average")

#sum of movie reviews per year
ratings_per_years<-aggregate(movie_wratingsPP$rating, by=list(movie_wratingsPP$year), FUN=length)
ratings_per_years
barplot(ratings_per_years$x, names.arg=ratings_per_years$Group.1, ylab="No. of ratings", xlab="Year", main="Ratings per year")
ratings_per_years %>%
  ggplot(aes(x = Group.1, y = x)) +
  geom_line(color="red")

#average rating for movies released by year
avg_rating_per_year<-aggregate(movie_wratingsPP$rating, by=list(movie_wratingsPP$year), FUN=mean)
avg_rating_per_year %>%
  ggplot(aes(x = Group.1, y = x)) +
  geom_line(color="red") +
  xlab("Year")+
  ylab("Avg Rating")+
  ggtitle("Avg Rating per year") +
  theme(plot.title = element_text(hjust = 0.5))



#---------------------------------------------------genre rating average barplot
#creating list for genre columns
genre_L <- list()
for(i in 5:23) {             # Using for-loop to add columns to list
  genre_L[[i]] <- colnames(movie_wratingsPP[i])
}
genre_L <- genre_L[5:23]
genre_k <- list()

#creating list for genre rating averages
for(i in genre_L){
  assign("genre", i)
  mm <-
    movie_wratingsPP %>%
    group_by(get(genre)) %>%
    filter(get(genre) == 1) %>%
    summarise_at(vars(rating),list(rating_average=mean)) %>%
    select(rating_average)
  genre_k[[i]] <-  mm
}

#bind lists into dataframe
avg_movies <- do.call(rbind, Map(data.frame, genre=genre_L, B=genre_k))

#barplot for average rating for each genre
barplot(avg_movies$rating,names.arg=avg_movies$genre, las=2, ylim=range(pretty(c(0, 5))),ylab="Avg Rating",main="Avg Rating by genre")

#----------------------------------------------barplot for no. ratings by movies
genre_r <- list()

#creating list for genre rating averages
for(i in genre_L){
  assign("genre", i)
  tr<-movie_wratingsPP %>%
    group_by(get(genre)) %>%
    filter(get(genre) == 1) %>%
    summarise(length(rating))
  genre_r[[i]] <- tr
}
genre_r
#bind lists into dataframe
count_ratings <- do.call(rbind, Map(data.frame, genre=genre_L, B=genre_r))
options(scipen=999)

#barplot for total ratings by movies
barplot(count_ratings$B.length.rating,names.arg=count_ratings$genre, las=2, main="Total Rating by genre")
#---------------------------------------------------End of Visualization Segment

#export to csv for model assembly and prediction
write.csv(movie_wratingsPP, "movie_wrating.csv", row.names=FALSE)