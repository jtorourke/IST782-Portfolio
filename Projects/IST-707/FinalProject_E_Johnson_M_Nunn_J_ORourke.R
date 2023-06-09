#Libraries ----
library(arules)
library(arulesViz)
library(grid)
library(datasets)
library(lmtest)
library(tidyverse)
library(dplyr)
library(caret)
library(kernlab)
library(e1071)
library(rpart)
library(rpart.plot)
library(imputeTS)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(gridExtra)
library(corrplot)
library(cowplot)
library(scales)
library(cluster)
library(fpc)
library(NbClust)
library(mclust)
library(factoextra)
library(broom)
library(ggpubr)

#Data ----
##Loading ====
#This will need to be changed for your local machine!
dataFile <- "F:/Syracuse/IST-707/Assignments/Project/songs_normalize.csv"

hits <- read.csv(dataFile)

##Cleaning ====
#Checking for missing values
sapply(hits, function(x) sum(is.na(x)))

#Updating a miscategorized genre
levels(hits$genre)[levels(hits$genre) == "set()"] <- "Uncategorized"

#Exploratory Data Analysis ----
##Initial Viewing ====
summary(hits)
glimpse(hits)

##Value Addition ====


##Plotting ====
#Basic Genre Breakdown (TOP 15)
genreCounts <- hits %>%
  group_by(genre) %>% 
  tally(sort=TRUE) %>%
  slice(1:15)

ggplot(data = genreCounts, aes(x=reorder(genre,(-n)), y=n)) +
  geom_bar(stat="identity", aes(fill = genre)) +
  theme(axis.text.x = element_text(angle = 90, vjust= 0.25,hjust = 1)) +
  labs(x= "Genre", y="Frequency", title="Top 15 Genres by Frequency")

#Popular Song Breakdown - Most
mostpop <- subset(hits, popularity >= 80)
mostpop <- mostpop[order(-mostpop$popularity),] %>%
  slice(1:40) %>%
  distinct(song, .keep_all = TRUE) %>%
  slice(1:10) %>%
  group_by(popularity)

head(mostpop)

#Popular Song Plot - Most
ggplot(aes(x=reorder(song, (-popularity)), y=popularity), data=mostpop) + 
  geom_bar(aes(fill = popularity), stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, vjust= 0.25,hjust = 1)) +
  labs(x= "Song", y="Popularity", title="Top 10 Songs by Popularity")

hist(hits$popularity)

#Popular Song Breakdown - Least
leastpop <- subset(hits, popularity <= 20)
leastpop <- leastpop[order(-leastpop$popularity),] %>%
  slice(1:40) %>%
  distinct(song, .keep_all = TRUE) %>%
  slice(1:10) %>%
  group_by(popularity)

ggplot(aes(x=reorder(song, (-popularity)), y=popularity), data=leastpop) + 
  geom_bar(aes(fill = popularity), stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, vjust= 0.25,hjust = 1)) +
  labs(x= "Song", y="Popularity", title="Top 10 Least Popular Songs")


#Modeling ----
## Popularity Classification ====
### Linear Regression ====
#Correlation Plots
plot(popularity ~ danceability, data = df_hits)
plot(popularity ~ loudness, data = df_hits)
plot(popularity ~ duration_ms, data = df_hits)
plot(popularity ~ energy, data = df_hits)
plot(popularity ~ key, data = df_hits)

#Correlation Outputs
sprintf("%f is Correlation between popularity and danceability", cor(df_hits$popularity, df_hits$danceability))
sprintf("%f is Correlation between popularity and loudness", cor(df_hits$popularity, df_hits$loudness))
sprintf("%f is Correlation between popularity and duration_ms", cor(df_hits$popularity, df_hits$duration_ms))
sprintf("%f is Correlation between popularity and energy", cor(df_hits$popularity, df_hits$energy))
sprintf("%f is Correlation between popularity and key", cor(df_hits$popularity, df_hits$key))

#Model
popularity <-lm(popularity ~ danceability + loudness + duration_ms + energy + key, data = df_hits)

summary(popularity)
plot(popularity)
par(mfrow=c(2,2))
plot(popularity)
par(mfrow=c(1,1))

#Danceability
popularity_graph<-ggplot(popularity, aes(x=danceability, y=popularity))+
  geom_point()

popularity_graph <- popularity_graph + geom_smooth(method="lm", col="red")

popularity_graph

#Loudness
popularity_graph<-ggplot(popularity, aes(x=loudness, y=popularity))+
  geom_point()
popularity_graph

#Duration
popularity_graph<-ggplot(popularity, aes(x=duration_ms, y=popularity))+
  geom_point()
popularity_graph

#Energy
popularity_graph<-ggplot(popularity, aes(x=energy, y=popularity))+
  geom_point()
popularity_graph

#Key
popularity_graph<-ggplot(popularity, aes(x=key, y=popularity))+
  geom_point()
popularity_graph

### Naive Bayes ====
df_NB <- hits

#splitting data
df_NB$popularity <- cut(df_NB$popularity, breaks = c(0,24,49,74,Inf), labels=c("0-24","25-49","50-74","74+"))


every6_indexes<-seq(1,nrow(df_NB),6)

nb_test<-df_NB[every6_indexes, ]
nb_train<-df_NB[-every6_indexes, ]

head(nb_test)
head(head(nb_train))

#Factors and cleaning
nb_test$popularity<-as.factor(nb_test$popularity)
nb_train$popularity<-as.factor(nb_train$popularity)

str(nb_test)
str(nb_train)

sapply(nb_train, function(x) sum(is.na(x)))
sapply(nb_test, function(x) sum(is.na(x)))

#Test data
(Test_Popularity <- nb_test[,6])
str(Test_Popularity)

(DF_Test_NO_POPULARITY <- nb_test[,-c(6)])

#Building the model
NB_e1071_2<-naiveBayes(nb_train, nb_train$popularity, laplace = 1)
NB_e1071_Pred <- predict(NB_e1071_2, DF_Test_NO_POPULARITY)

conf <- table(NB_e1071_Pred,Test_Popularity)

#predicting values
NB_e1071_Pred <- predict(NB_e1071_2, DF_Test_NO_POPULARITY)

conf <- table(NB_e1071_Pred,Test_Popularity)

acc <- sum(diag(conf))/sum(conf)
sprintf("Accuracy of Naive Bayes: %g", acc)

#Plotting
plot(NB_e1071_Pred, ylab="Total Popularity", xlab="Popularity", col=NB_e1071_Pred)


### Decision Tree ====
#Decision Tree
dtMod <- rpart(popularity ~ duration_ms, data = nb_train, method = "class",
               control = rpart.control(cp = 0), minsplit = 100, maxdepth = 4)
summary(dtMod)

#Printing rpart model
rpart.plot(dtMod)

#Predicting values
dtPred <- predict(dtMod, DF_Test_NO_POPULARITY, type="class")

#Building confusion matrix and testing for accuracy
dtConf <- table(dtPred, Test_Popularity)
dtAcc <- sum(diag(dtConf))/sum(dtConf)
print(dtAcc)
sprintf("Accuracy of Decision Tree Model: %g", dtAcc)

### SVM ====
#Linear 
tuned <- tune(svm, popularity ~ danceability + loudness + duration_ms + energy + key, data = nb_train, kernel = "linear", ranges = list(cost=c(0.001, 0.01,.1,1,10,100,200)))
summary(tuned)

#Radial
tuned <- tune(svm, popularity ~ danceability + loudness + duration_ms + energy + key, data = nb_train, kernel = "radial", ranges = list(cost=c(0.001, 0.01,.1,1,10,100)))
summary(tuned)

#Polynomial
tuned <- tune(svm, popularity ~ danceability + loudness + duration_ms + energy + key, data = nb_train, kernel = "polynomial", ranges = list(cost=c(0.001, 0.01,.1,1,10,100)))
summary(tuned)

#Training - Linear
SVM_Popularity_fit_L <- svm(popularity ~ danceability + loudness + duration_ms + energy + key, data=nb_train,
                            kernel="linear", cost=.001,
                            scale=FALSE)
print(SVM_Popularity_fit_L)

#Training - Radial
SVM_Popularity_fit_R <- svm(popularity ~ danceability + loudness + duration_ms + energy + key, data=nb_train,
                            kernel="radial", cost=.001)

print(SVM_Popularity_fit_R)

#Training - Polynomial
SVM_Popularity_fit_P <- svm(popularity ~ danceability + loudness + duration_ms + energy + key, data=nb_train,
                            kernel="polynomial", cost=100,
                            scale=FALSE)
print(SVM_Popularity_fit_P)

#Predictions
p <- predict(SVM_Popularity_fit_P, DF_Test_NO_POPULARITY, type="class")
plot(p, ylab="Total Popularity", xlab="Popularity", col=p)

#Accuracy
t <- table(p,Test_Popularity)

a <- sum(diag(t))/sum(t)

print(t)
sprintf("Accuracy of SVM Model: %g", a)

#Plotting - Radial
plot(SVM_Popularity_fit_R, nb_train, as.numeric(popularity) ~ danceability, ylab="popularity")
plot(SVM_Popularity_fit_R, nb_train, as.numeric(popularity) ~ loudness, ylab="popularity" )
plot(SVM_Popularity_fit_R, nb_train, as.numeric(popularity) ~ duration_ms, ylab="popularity")
plot(SVM_Popularity_fit_R, nb_train, as.numeric(popularity) ~ energy, ylab="popularity")
plot(SVM_Popularity_fit_R, nb_train, as.numeric(popularity) ~ key, ylab="popularity")

#Plotting - Linear
plot(SVM_Popularity_fit_L, nb_train, as.numeric(popularity) ~ danceability, ylab="popularity")
plot(SVM_Popularity_fit_L, nb_train, as.numeric(popularity) ~ loudness, ylab="popularity" )
plot(SVM_Popularity_fit_L, nb_train, as.numeric(popularity) ~ duration_ms, ylab="popularity")
plot(SVM_Popularity_fit_L, nb_train, as.numeric(popularity) ~ energy, ylab="popularity")
plot(SVM_Popularity_fit_L, nb_train, as.numeric(popularity) ~ key, ylab="popularity")

#Plotting - Polynomial
plot(SVM_Popularity_fit_P, nb_train, as.numeric(popularity) ~ danceability, ylab="popularity")
plot(SVM_Popularity_fit_P, nb_train, as.numeric(popularity) ~ loudness, ylab="popularity" )
plot(SVM_Popularity_fit_P, nb_train, as.numeric(popularity) ~ duration_ms, ylab="popularity")
plot(SVM_Popularity_fit_P, nb_train, as.numeric(popularity) ~ energy, ylab="popularity")
plot(SVM_Popularity_fit_P, nb_train, as.numeric(popularity) ~ key, ylab="popularity")

#
plot(x=df_hits$popularity, y=df_hits$danceability, col = df_hits$popularity, xlab="popularity", ylab= "danceability", pch = 19)

(conf.mat <- confusionMatrix(NB_e1071_Pred, Test_Popularity))

plot(SVM_Popularity_fit_L, df_hits,popularity ~ danceability, ylab="popularity")