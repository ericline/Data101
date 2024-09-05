library(rpart)
library(rpart.plot)
library(Metrics)
devtools::install_github("devanshagr/CrossValidation")

LoanTrain<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/LoanTrain2023b.csv')
LoanTest<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/LoanTest2023b.csv')

head(LoanTrain)
head(LoanTest)

colors = c('red','orange','yellow','green','blue','purple')

plot(LoanTrain$Amount, LoanTrain$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(LoanTrain$Amount, LoanTrain$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
boxplot(Amount ~ Home, data = LoanTrain, col = colors)
boxplot(Amount ~ Car, data = LoanTrain, col = colors)

Debt1 <- LoanTrain[LoanTrain$Amount > 4e+04,]
Debt2 <- LoanTrain[LoanTrain$Amount < 4e+04,]

head(Debt1)
head(Debt2)

S1 <- LoanTrain[LoanTrain$Home == 'No' & LoanTrain$Car == 'No',]
S2 <- LoanTrain[LoanTrain$Home == 'Yes' | LoanTrain$Car == 'Yes',]

head(S1)
head(S2)

# S1 - NO Car and No Home
plot(S1$Amount, S1$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S1$Amount, S1$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")

S1D1 <- S1[S1$Debt == 0,]
S1D2 <- S1[S1$Debt == 1e+05,]

LoanTest$Income2 <- rep(0, nrow(LoanTest))

plot(S1D1$Amount, S1D1$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S1D1$Amount, S1D1$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
S1D1$Income2 <- S1D1$Income^(1/2)
LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 0,]$Income2 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 0,]$Income^(1/2)
plot(S1D1$Amount, S1D1$Income2, main = "Scatter Plot", xlab = "Income", ylab = "Amount")

model1 <- lm(Amount ~ Income + Income2, data = S1D1)
pred1 <- predict(model1, newdata = S1D1)
mse(S1D1$Amount, pred1)

plot(S1D2$Amount, S1D2$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S1D2$Amount, S1D2$Debt2, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S1D2$Amount, S1D2$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
S1D2$Income2 <- S1D2$Income^(1/2)
LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 1e+05,]$Income2 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 1e+05,]$Income^(1/2)
plot(S1D2$Amount, S1D2$Income2, main = "Scatter Plot", xlab = "Income", ylab = "Amount")

model2 <- lm(Amount ~ Income + Income2, data = S1D2)
pred2 <- predict(model2, newdata = S1D2)
mse(S1D2$Amount, pred2)

# S2 - Either Car or Home
plot(S2$Amount, S2$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S2$Amount, S2$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")

S2D1 <- S2[S2$Debt == 0,]
S2D2 <- S2[S2$Debt == 1e+05,]

S21YY <- S2D1[S2D1$Home == 'Yes' & S2D1$Car == 'Yes',]
S21YN <- S2D1[S2D1$Home == 'Yes' & S2D1$Car == 'No',]
S21NY <- S2D1[S2D1$Home == 'No' & S2D1$Car == 'Yes',]

S22YY <- S2D2[S2D2$Home == 'Yes' & S2D2$Car == 'Yes',]
S22YN <- S2D2[S2D2$Home == 'Yes' & S2D2$Car == 'No',]
S22NY <- S2D2[S2D2$Home == 'No' & S2D2$Car == 'Yes',]

# S2 - Debt = 0, Yes Yes
plot(S21YY$Amount, S21YY$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S21YY$Amount, S21YY$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model3 <- lm(Amount ~ Income, data = S21YY)
pred3 <- predict(model3, newdata = S21YY)
mse(S21YY$Amount, pred3)

# S2 - Debt = 0, Yes No
plot(S21YN$Amount, S21YN$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S21YN$Amount, S21YN$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model4 <- lm(Amount ~ Income, data = S21YN)
pred4 <- predict(model4, newdata = S21YN)
mse(S21YN$Amount, pred4)

# S2 - Debt = 0, No Yes
plot(S21NY$Amount, S21NY$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S21NY$Amount, S21NY$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model5 <- lm(Amount ~ Income, data = S21NY)
pred5 <- predict(model5, newdata = S21NY)
mse(S21NY$Amount, pred5)

# S2 - Debt = 1e+05, Yes Yes
plot(S22YY$Amount, S22YY$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S22YY$Amount, S22YY$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model6 <- lm(Amount ~ Income + Debt, data = S22YY)
pred6 <- predict(model6, newdata = S22YY)
mse(S22YY$Amount, pred6)

# S2 - Debt = 1e+05, Yes No
plot(S22YN$Amount, S22YN$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S22YN$Amount, S22YN$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model7 <- lm(Amount ~ Income, data = S22YN)
pred7 <- predict(model7, newdata = S22YN)
mse(S22YN$Amount, pred7)

# S2 - Debt = 1e+05, No Yes
plot(S22NY$Amount, S22NY$Debt, main = "Scatter Plot", xlab = "Debt", ylab = "Amount")
plot(S22NY$Amount, S22NY$Income, main = "Scatter Plot", xlab = "Income", ylab = "Amount")
model8 <- lm(Amount ~ Income, data = S22NY)
pred8 <- predict(model8, newdata = S22NY)
mse(S22NY$Amount, pred8)

t1 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 0,]
t2 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 1e+05,]
t3 <- LoanTest[LoanTest$Home == 'Yes' & LoanTest$Car == 'Yes' & LoanTest$Debt == 0,]
t4 <- LoanTest[LoanTest$Home == 'Yes' & LoanTest$Car == 'No' & LoanTest$Debt == 0,]
t5 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'Yes' & LoanTest$Debt == 0,]
t6 <- LoanTest[LoanTest$Home == 'Yes' & LoanTest$Car == 'Yes' & LoanTest$Debt == 1e+05,]
t7 <- LoanTest[LoanTest$Home == 'Yes' & LoanTest$Car == 'No' & LoanTest$Debt == 1e+05,]
t8 <- LoanTest[LoanTest$Home == 'No' & LoanTest$Car == 'Yes' & LoanTest$Debt == 1e+05,]

prediction1 <- rep(0, nrow(LoanTrain))
prediction1[LoanTrain$Home == 'No' & LoanTrain$Car == 'No' & LoanTrain$Debt == 0] <- pred1
prediction1[LoanTrain$Home == 'No' & LoanTrain$Car == 'No' & LoanTrain$Debt == 1e+05] <- pred2
prediction1[(LoanTrain$Home == 'Yes' & LoanTrain$Car == 'Yes') & LoanTrain$Debt == 0] <- pred3
prediction1[(LoanTrain$Home == 'Yes' & LoanTrain$Car == 'No') & LoanTrain$Debt == 0] <- pred4
prediction1[(LoanTrain$Home == 'No' & LoanTrain$Car == 'Yes') & LoanTrain$Debt == 0] <- pred5
prediction1[(LoanTrain$Home == 'Yes' & LoanTrain$Car == 'Yes') & LoanTrain$Debt == 1e+05] <- pred6
prediction1[(LoanTrain$Home == 'Yes' & LoanTrain$Car == 'No') & LoanTrain$Debt == 1e+05] <- pred7
prediction1[(LoanTrain$Home == 'No' & LoanTrain$Car == 'Yes') & LoanTrain$Debt == 1e+05] <- pred8
mse(LoanTrain$Amount, prediction1)

prediction <- rep(0, nrow(LoanTest))
prediction[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 0] <- predict(model1, newdata = t1)
prediction[LoanTest$Home == 'No' & LoanTest$Car == 'No' & LoanTest$Debt == 1e+05] <- predict(model2, newdata = t2)
prediction[(LoanTest$Home == 'Yes' & LoanTest$Car == 'Yes') & LoanTest$Debt == 0] <- predict(model3, newdata = t3)
prediction[(LoanTest$Home == 'Yes' & LoanTest$Car == 'No') & LoanTest$Debt == 0] <- predict(model4, newdata = t4)
prediction[(LoanTest$Home == 'No' & LoanTest$Car == 'Yes') & LoanTest$Debt == 0] <- predict(model5, newdata = t5)
prediction[(LoanTest$Home == 'Yes' & LoanTest$Car == 'Yes') & LoanTest$Debt == 1e+05] <- predict(model6, newdata = t6)
prediction[(LoanTest$Home == 'Yes' & LoanTest$Car == 'No') & LoanTest$Debt == 1e+05] <- predict(model7, newdata = t7)
prediction[(LoanTest$Home == 'No' & LoanTest$Car == 'Yes') & LoanTest$Debt == 1e+05] <- predict(model8, newdata = t8)
mse(LoanTest$Amount, prediction)
# MSE = 99.88947

# Cross Validation
v <- sample(1:nrow(LoanTrain))
trainScramble <- LoanTrain[v, ]
n <- nrow(LoanTrain) * 0.2
trainSample <- trainScramble[nrow(trainScramble) - n:nrow(trainScramble),]
testSample <- trainScramble[1:n, ]

trainSample$Income2 <- rep(0, nrow(trainSample))
trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 1e+05,]$Income2 <- trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 1e+05,]$Income^(1/2)
trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 0,]$Income2 <- trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 0,]$Income^(1/2)

testSample$Income2 <- rep(0, nrow(testSample))
testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 1e+05,]$Income2 <- testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 1e+05,]$Income^(1/2)
testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 0,]$Income2 <- testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 0,]$Income^(1/2)

tm1 <- lm(Amount ~ Income + Income2, data = trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 0,])
tm2 <- lm(Amount ~ Income + Income2, data = trainSample[trainSample$Home == 'No' & trainSample$Car == 'No' & trainSample$Debt == 1e+05,])
tm3 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'Yes' & trainSample$Car == 'Yes' & trainSample$Debt == 0,])
tm4 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'Yes' & trainSample$Car == 'No' & trainSample$Debt == 0,])
tm5 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'No' & trainSample$Car == 'Yes' & trainSample$Debt == 0,])
tm6 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'Yes' & trainSample$Car == 'Yes' & trainSample$Debt == 1e+05,])
tm7 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'Yes' & trainSample$Car == 'No' & trainSample$Debt == 1e+05,])
tm8 <- lm(Amount ~ Income, data = trainSample[trainSample$Home == 'No' & trainSample$Car == 'Yes' & trainSample$Debt == 1e+05,])

p1 <- predict(tm1, newdata = testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 0,])
p2 <- predict(tm2, newdata = testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 1e+05,])
p3 <- predict(tm3, newdata = testSample[testSample$Home == 'Yes' & testSample$Car == 'Yes' & testSample$Debt == 0,])
p4 <- predict(tm4, newdata = testSample[testSample$Home == 'Yes' & testSample$Car == 'No' & testSample$Debt == 0,])
p5 <- predict(tm5, newdata = testSample[testSample$Home == 'No' & testSample$Car == 'Yes' & testSample$Debt == 0,])
p6 <- predict(tm6, newdata = testSample[testSample$Home == 'Yes' & testSample$Car == 'Yes' & testSample$Debt == 1e+05,])
p7 <- predict(tm7, newdata = testSample[testSample$Home == 'Yes' & testSample$Car == 'No' & testSample$Debt == 1e+05,])
p8 <- predict(tm8, newdata = testSample[testSample$Home == 'No' & testSample$Car == 'Yes' & testSample$Debt == 1e+05,])

# SIDE NOTE: Yes I realized I could have been a good coder and created subsets 
# to avoid typing the subsets over and over again in the above code, but by the 
# time I realized, it was already too late :(

# Cross Validation of each Model
mse(testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 0,]$Amount, p1)
mse(testSample[testSample$Home == 'No' & testSample$Car == 'No' & testSample$Debt == 1e+05,]$Amount, p2)
mse(testSample[testSample$Home == 'Yes' & testSample$Car == 'Yes' & testSample$Debt == 0,]$Amount, p3)
mse(testSample[testSample$Home == 'Yes' & testSample$Car == 'No' & testSample$Debt == 0,]$Amount, p4)
mse(testSample[testSample$Home == 'No' & testSample$Car == 'Yes' & testSample$Debt == 0,]$Amount, p5)
mse(testSample[testSample$Home == 'Yes' & testSample$Car == 'Yes' & testSample$Debt == 1e+05,]$Amount, p6)
mse(testSample[testSample$Home == 'Yes' & testSample$Car == 'No' & testSample$Debt == 1e+05,]$Amount, p7)
mse(testSample[testSample$Home == 'No' & testSample$Car == 'Yes' & testSample$Debt == 1e+05,]$Amount, p8)
# All MSE range from 85 - 112
