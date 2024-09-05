library(rpart)
library(rpart.plot)
devtools::install_github("devanshagr/CrossValidation")
sub <- submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')

movies <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Tr.csv')

summary(movies)
head(movies)

t <- table(movies$Genre, movies$RATING)
t <- table(movies$Content, movies$RATING)
barplot(t, beside = FALSE, col = c("blue", "red", 'green', 'purple'), legend.text = TRUE, args.legend = list(title = "Legend Title"))
boxplot(Audience ~ RATING, data = movies, col = c('blue', 'red'))
boxplot(Income ~ RATING, data = movies, col = c('blue', 'red'))

sG <- movies[movies$RATING == 'Great',]
plot(sG$Income, sG$Audience)

movies$Delta <- 1.5*movies$Audience - 3*movies$Income
boxplot(Delta ~ RATING, data = movies, col = c('blue', 'red'))

movies$Audience_Income_Ratio <- movies$Audience / movies$Income

boxplot(Audience ~ RATING, data = movies)
boxplot(Income ~ RATING, data = movies)
boxplot(Income ~ RATING, data = movies)
table(movies$RATING, movies$Genre)
table(movies$RATING, movies$Genre)
movies2 = movies[movies$RATING == 'Great',]

plot(movies2$Income, movies2$Audience)

movies$Delta <- (3*movies$Audience) - (1.5*movies$Income)
head(movies$Delta)

boxplot(movies$Delta ~ movies$RATING)

tree <- rpart(RATING ~ Genre + Content + Audience + Income + Delta, 
              data = movies)
rpart.plot(tree)
tree
pred <- predict(tree, data = movies, type="class")
head(pred)
mean(movies$RATING==pred)
CrossValidation::cross_validate(movies, tree, 10, 0.8)

test <- movies<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')
summary(test)
head(test)

test$Audience_Income_Ratio <- test$Audience / test$Income

sub$RATING <- predict(tree, newdata = test, type="class")
head(test)

CrossValidation::cross_validate(test, tree, 10, 0.8)
sub$RATING <- test$RATING
write.csv(sub, file = 'mysubmission.csv', row.names = FALSE)

sub1 <- read.csv("/Users/ericlin/Desktop/R/HW9 - Prediction Challenge 1/mysubmission.csv")
all(sub1$RATING == sub$RATING)
