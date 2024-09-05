install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)
library(rpart)

## Part A
elect <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/Miderm.csv')

summary(elect)
head(elect, 15)

# Republican Subset
table(elect$NetWorth, elect$Party)
table(elect$Education, elect$Party) 
table(elect$Location, elect$Party) 
repub <- elect[elect$NetWorth == 'High' | elect$Location == 'PrimarySchool',]
table(repub$Party)
nrow(repub)

# Democrat Subset
table(elect$Education[elect$NetWorth != 'High'], elect$Party[elect$NetWorth != 'High'])
tapply(elect$Age[elect$NetWorth != 'High' & elect$Education == 'College'], 
       elect$Party[elect$NetWorth != 'High' & elect$Education == 'College'], mean)

dem <- elect[elect$NetWorth != 'High' & elect$Education == 'College' & elect$Age < 54.24,]
table(dem$Party)
nrow(dem)

# Predictor
decision1 <- rep('Democrat', nrow(elect))
decision1[elect$NetWorth == 'High'] <- 'Republican'
decision1[elect$Education == 'PrimarySchool'] <- 'Republican'
cat('Accuracy of Model 1 is: ', mean(decision1 == elect$Party))
# Accuracy = 0.7785

decision2 <- rep('Republican', nrow(elect))
decision2[elect$NetWorth != 'High' & elect$Education == 'College' & elect$Age < 54.24] <- 'Democrat'
cat('Accuracy of Model 2 is: ', mean(decision2 == elect$Party))
# Accuracy = 0.7665

## Part B
mean_age <- mean(elect$Age)
mean_age

tapply(elect$Age, elect$Party, mean)

tapply(elect$Age[elect$Party == 'Republican'], 
       elect$NetWorth[elect$Party == 'Republican'], mean)
# Republican, Low NetWorth

s1 <- elect[elect$Party == 'Republican' & elect$NetWorth == 'Low',]
mean(s1$Age)
nrow(s1)


tapply(elect$Age[elect$Party == 'Democrat'], 
       elect$Education[elect$Party == 'Democrat'], mean)
tapply(elect$Age[elect$Party == 'Democrat' & elect$Education != 'HighSchool'], 
       elect$NetWorth[elect$Party == 'Democrat' & elect$Education != 'HighSchool'], mean)
# Democrat, Primary School

s2 <- elect[elect$Party == 'Democrat' & elect$Education != 'HighSchool',]
mean(s2$Age)
nrow(s2)

# Hypothesis Testing

combined1 <- data.frame(Data = c(rep('Election', times = nrow(elect)), rep('Subset1', times = nrow(s1))),
                        Value = c(elect$Age, s1$Age))
combined2 <- data.frame(Data = c(rep('Election', times = nrow(elect)), rep('Subset2', times = nrow(s2))),
                        Value = c(elect$Age, s2$Age))

# Bonferroni coefficient can be applied as you are testing multiple hypothesises against one dataset (election).
# However, in this case, it may not be neccessary as the number of tests is only two so it is not significant.
permutation_test(combined1, 'Data', 'Value', 10000, 'Election', 'Subset1')
permutation_test(combined2, 'Data', 'Value', 10000, 'Subset2', 'Election')
# Resulting p-values of 0 (less than 0.05) indicates that the Age means are possibly significantly 
# different (higher for s1 and lower for s2), and we can reject the Null Hypothesis that states 
# that the Age means between the subsets and elect set are the same.

## Part C
set.seed(456)
Net<-unique(elect$NetWorth)
Edu <-unique(elect$Education) 
Loc<-unique(elect$Location)
Party<-unique(elect$Party)

Netseq <- sample(Net, 20000, replace = TRUE)
Eduseq<-sample(Edu, 20000, replace = TRUE)
Locseq<-sample(Loc, 20000, replace = TRUE)
Partseq<-sample(Party, 20000, replace = TRUE)
Ageseq<- runif(20000, 18, 100)
colnames(elect)
electR<-data.frame(NetWorth=Netseq, Age=Ageseq, Education=Eduseq,Location=Locseq, Party=Partseq)
summary(electR)

table(electR$NetWorth, electR$Party)

table(electR$Education[electR$NetWorth != 'Low'], 
      electR$Party[electR$NetWorth != 'Low'])

table(electR$Location[electR$NetWorth != 'Low' & electR$Education == 'PrimarySchool'], 
      electR$Party[electR$NetWorth != 'Low' & electR$Education == 'PrimarySchool'])

tapply(elect$Age[electR$NetWorth != 'Low' & electR$Education == 'PrimarySchool' & electR$Location == 'Central'], 
       elect$Party[electR$NetWorth != 'Low' & electR$Education == 'PrimarySchool' & electR$Location == 'Central'], mean)

# S Subset
S <- electR[electR$NetWorth != 'Low' & 
            electR$Education == 'PrimarySchool' &
            electR$Location == 'Central' &
            electR$Age > 26 &
            electR$Age < 82,]
nrow(S)
t <- table(S$Party)
percentage <- t["Democrat"] / sum(t)
cat('Percentage of Democraat Voters: ', percentage)
# Percentage = 0.5524476

# C.1
# One way to demonstrate that S is random is to see if our subset query still holds true after splitting.

# First Split
S1 <- S[1:500,]
S2 <- S[501:1001,]

t1 <- table(S1$Party)
percentage1 <- t1['Democrat'] /sum(t1)
percentage1

t2 <- table(S2$Party)
percentage2 <- t2['Democrat'] /sum(t2)
percentage2

# Second Split
S1 <- S[1:250,]
S2 <- S[251:500,]
S3 <- S[501:750,]
S4 <- S[751:1001,]

t1 <- table(S1$Party)
percentage1 <- t1['Democrat'] /sum(t1)
percentage1

t2 <- table(S2$Party)
percentage2 <- t2['Democrat'] /sum(t2)
percentage2

t3 <- table(S3$Party)
percentage3 <- t3['Democrat'] /sum(t3)
percentage3

t4 <- table(S4$Party)
percentage4 <- t4['Democrat'] /sum(t4)
percentage4

# From our splitting tests, we can see 0.55 Democrat votes does not hold true throughout, therefore
# our subset discovery is likely random.

## Part D
market <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/HomeworkMarket2022.csv")
summary(market)
head(market)
unique(market$Beer)
unique(market$Location)
unique(market$Snacks)

# D1
chisq.test(table(market$Beer, market$Location))
cat('The p-value is 0.7375, therefore we cannot reject the Null Hypothesis
    that there is no relationship between the two variables, suggesting independence.')

# D2
expected_table <- chisq.test(table(market$Beer, market$Location))$expected
expected_table

# D3
observed_table <- table(market$Beer, market$Location)
observed_table

# D4
chisq.test(table(market$Day, market$Beer))
chisq.test(table(market$SoftDrinks, market$Beer))
chisq.test(table(market$Sweets, market$Beer))
chisq.test(table(market$Wine, market$Beer))
chisq.test(table(market$Snacks, market$Beer))
cat('p-value between Snacks and Beer is 3.31e-11, therefore they are dependent.')
chisq.test(table(market$Snacks, market$Location))
cat('p-value between Snacks and Location is 1.86e-11, therefore they are dependent.')
cat('Therefore, we can reject the Null Hypothesis for Snacks and both Beer and Location.')

# Item = Crackers
p <- chisq.test(table(market$Beer[market$Snacks == 'Crackers'], market$Location[market$Snacks == 'Crackers']))
p
cat('With crackers as a snack, we obtain a p-value of 7.058e-09 between Beer and Location,
    therefore we cannot reject the Null Hypothesis of independence.')

# D5
p$expected


e <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/electMidterm.csv')


head(e)

rpart(Party ~ NetWorth + Age + Education + Location, data = e)

republican <- e[e$Age >= 60.51 & e$Location == 'WestCoast',]
democrat <-e[e$Age >= 59.705 & e$Location != 'WestCoast',]

decision <- rep('Democrat', nrow(e))
decision[e$Age >= '60.51' | e$Location == 'WestCoast'] <- 'Republican'
decision[e$Age >= '59.705' & e$election != 'WestCoast'] <- 'Democrat'
cat('Accuracy of Model 1 is: ', mean(decision == e$Party))
cat('Accuracy of Model 1 is: ', mean(decision1 == e$Party))

