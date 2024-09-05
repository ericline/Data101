library(rpart)
library(rpart.plot)
library(Metrics)
devtools::install_github("devanshagr/CrossValidation")
train <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')
test <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')
colors = c('red','orange','yellow','green','blue','purple')

summary(train)
head(train)
head(test)

# Plotting variable relationships
plot(train$LinkedIN, train$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(train$DOB, train$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(train$GPA, train$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(train$Tuition, train$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
boxplot(Salary ~ Major, data = train, col = colors)
boxplot(Salary ~ College_location, data = train, col = colors)
unique(train$Major)

step_model <- step(lm(Salary ~ ., data = train), direction = "both")
summary(step_model)

# Creating new features
train$Age <- 2023 - train$DOB
head(train)

  # Sub-setting
humanities <- train[train$Major == 'Humanities',]
other <- train[train$Major == 'Other',]
stem <- train[train$Major == 'STEM',]
vocational <- train[train$Major == 'Vocational',]
professional <- train[train$Major == 'Professional',]
business <- train[train$Major == 'Buisness',]

# Humanities Model
plot(humanities$LinkedIN, humanities$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(humanities$DOB, humanities$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(humanities$GPA, humanities$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(humanities$Tuition, humanities$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(humanities$Age, humanities$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = humanities, col = colors)
humanities$GPA2 <- (humanities$GPA)^2
humanities$GPAS <- humanities$GPA^2 * log(humanities$Tuition)
humanities$GT <- log(humanities$GPA / humanities$Tuition)
head(humanities)

cor_matrix <- cor(humanities[, c('Salary', 'GPA', 'GPAS')])
cor_matrix

model1 <- lm(Salary ~ GPA + GPA2 + GPAS + GT, data = humanities)
pred1 <- predict(model1, newdata = humanities)
mse(humanities$Salary, pred1)

# Other Model
plot(other$LinkedIN, other$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(other$DOB, other$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(other$GPA, other$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(other$Tuition, other$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(other$Age, other$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = other, col = colors)

other$LinkedIN2 <- other$LinkedIN^2
plot(other$LinkedIN2, other$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")

step_model <- step(lm(Salary ~ LinkedIN+DOB+GPA+Tuition+Age+College_location, data = other), direction = "both")
summary(step_model)
model2 <- lm(Salary ~ LinkedIN2 + LinkedIN, data = other)
pred2 <- predict(model2, newdata = other)
mse(other$Salary, pred2)

# STEM Model
plot(stem$LinkedIN, stem$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(stem$DOB, stem$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(stem$GPA, stem$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(stem$Tuition, stem$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(stem$Age, stem$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = stem, col = colors)

model3 <- lm(Salary ~ DOB + GPA, data = stem)
pred3 <- predict(model3, newdata = stem)
mse(stem$Salary, pred3)

# Vocational Model
plot(vocational$LinkedIN, vocational$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(vocational$DOB, vocational$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(vocational$GPA, vocational$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(vocational$Tuition, vocational$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(vocational$Age, vocational$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = vocational, col = colors)
vocational$GPA_T <- (vocational$GPA)/vocational$Tuition
vocational$GPA_L <- vocational$GPA * vocational$LinkedIN

model4 <- lm(Salary ~ GPA + GPA_T + GPA_L, data = vocational)
pred4 <- predict(model4, newdata = vocational)
mse(vocational$Salary, pred4)

# Professional Model
plot(professional$LinkedIN, professional$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(professional$DOB, professional$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(professional$GPA, professional$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(professional$Tuition, professional$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(professional$Age, professional$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = professional, col = colors)
professional$GPA2 <- professional$GPA^(1/10)
plot(professional$GPA2, professional$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")

model5 <- lm(Salary ~ GPA + GPA2, data = professional)
pred5 <- predict(model5, newdata = professional)
mse(professional$Salary, pred5)

# Business Model
plot(business$LinkedIN, business$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(business$DOB, business$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(business$GPA, business$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(business$Tuition, business$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(business$Age, business$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = business, col = colors)

business_G <- business[business$Salary > 10000,]
business_S <- business[business$Salary < 10000,]
head(business_G)
head(business_S)

business1 <- business[business$DOB %% 2 == 0,]
business2 <- business[business$DOB %% 2 != 0,]

plot(business1$LinkedIN, business1$Salary, main = "Scatter Plot", xlab = "Linkedin", ylab = "Salary")
plot(business1$DOB, business1$Salary, main = "Scatter Plot", xlab = "DOB", ylab = "Salary")
plot(business1$GPA, business1$Salary, main = "Scatter Plot", xlab = "GPA", ylab = "Salary")
plot(business1$Tuition, business1$Salary, main = "Scatter Plot", xlab = "Tuition", ylab = "Salary")
plot(business1$Age, business1$Salary, main = "Scatter Plot", xlab = "Age", ylab = "Salary")
boxplot(Salary ~ College_location, data = business1, col = colors)

model6 <- lm(Salary ~ GPA, data = business1)
pred6 <- predict(model6, newdata = business1)
mse(business1$Salary, pred6)

model7 <- lm(Salary ~ GPA, data = business2)
pred7 <- predict(model7, newdata = business2)
mse(business2$Salary, pred7)

decision <- rep(0, nrow(business))
decision[business$DOB %% 2 == 0] <- pred6
decision[business$DOB %% 2 != 0] <- pred7
mse(business$Salary, decision)

# Combining Linear Regression Models
prediction <- rep(0, nrow(train))
prediction[train$Major == 'Humanities'] <- pred1
prediction[train$Major == 'Other'] <- pred2
prediction[train$Major == 'STEM'] <- pred3
prediction[train$Major == 'Vocational'] <- pred4
prediction[train$Major == 'Professional'] <- pred5
prediction[train$Major == 'Buisness' & train$DOB %% 2 == 0] <- pred6
prediction[train$Major == 'Buisness' & train$DOB %% 2 != 0] <- pred7
mse(train$Salary, prediction)

prediction2 <- rep(0, nrow(test))
prediction2[test$Major == 'Humanities'] <- pred1
prediction2[test$Major == 'Other'] <- pred2
prediction2[test$Major == 'STEM'] <- pred3
prediction2[test$Major == 'Vocational'] <- pred4
prediction2[test$Major == 'Professional'] <- pred5
prediction2[test$Major == 'Buisness' & test$DOB %% 2 == 0] <- pred6
prediction2[test$Major == 'Buisness' & test$DOB %% 2 != 0] <- pred7

test$Salary <- prediction2

ids<-c(1:nrow(test))
submission$ID<-ids
submission$Salary <- prediction2
head(submission)
all.equal(test$Salary, prediction2)

write.csv(submission, 'mysubmission.csv' , row.names = FALSE)
