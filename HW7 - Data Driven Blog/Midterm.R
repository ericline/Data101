install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)
elect <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/electMidterm.csv')
head(elect)
summary(elect)
color <- c('blue', 'red')

# a.)
df <- parties
df$Party <- ifelse(df$Party == 'Republican', 1, 2)
head(df)

democrat <- elect[elect$Party == 'Democrat',]
summary(democrat)

republican <- parties[parties$Party == 'Republican',]
summary(republican)

boxplot(Age ~ Party, data = elect, col = color)

tapply(df$Party, df$NetWorth, mean)
tapply(df$Party, df$Location, mean)
tapply(elect$Age, elect$Party, mean)
tapply(df$Party, df$Education, mean)

tapply(df$Party[df$NetWorth == 'Low'], df$Education[df$NetWorth == 'Low'], mean)
tapply(df$Party[df$NetWorth == 'Low' & df$Education == 'HighSchool'], df$Location[df$NetWorth == 'Low' & df$Education == 'HighSchool'], mean)
table(elect[df$NetWorth == 'Low' & df$Location == 'Central',]$Party)

df <- elect[elect$NetWorth == 'Medium' & elect$Location == 'EastCoast',]
table(df$Party)

# b.)

mean(elect$Age)
tapply(elect$Age, elect$Party, mean)
tapply(elect$Age[elect$Education == 'PrimarySchool'], elect$NetWorth[elect$Education == 'PrimarySchool'], mean)
tapply(elect$Age[elect$NetWorth == 'Low' & elect$Education == 'PrimarySchool'], elect$Location[elect$NetWorth == 'Low' & elect$Education == 'PrimarySchool'], mean)
tapply(elect$Age[elect$NetWorth == 'Low' & elect$Education == 'PrimarySchool' & elect$Location == 'WestCoast'], elect$Party[elect$NetWorth == 'Low' & elect$Education == 'PrimarySchool' & elect$Location == 'WestCoast'], mean)

s18 <- elect[elect$Location == 'WestCoast' & elect$Party == 'Republican',]
mean(s18$Age)
s20 <- elect[elect$Location == 'WestCoast' & elect$Party == 'Democrat',]
mean(s20$Age)

# Null Hypothesis: The average Age of WestCoasters is same when Party == 'Republican' vs when Party == 'Democrat'.
# Alternate Hypothesis: The aver Age of WestCoasters is higher when Party == 'Republican' vs when Party == 'Democrat'.
permutation_test(elect[elect$Location == 'WestCoast',], 'Party', 'Age', 10000, 'Democrat', 'Republican')
# Reject Null Hypothesis. 

# Null Hypothesis: The average Age of WestCoasters is higher when Party == 'Republican' vs when Party == 'Democrat'.
# Alternate Hypothesis: The aver Age of WestCoasters is the same when Party == 'Republican' vs when Party == 'Democrat'.
permutation_test(elect[elect$Location == 'WestCoast',], 'Party', 'Age', 10000, 'Republican', 'Democrat')
# Fail to Reject Null Hypothesis.

# c.)
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
electR

party_counts_s <- table(electR$Party)

expected_probs <- table(elect$Party) / sum(table(elect$Party))
expected_counts <- round(expected_probs * nrow(electR))

chi_squared <- chisq.test(party_counts_s, p = expected_probs)

cat("Observed Party Frequencies in 'S':")
party_counts_s

cat("Expected Party Frequencies (Random):")
expected_counts
cat("Chi-Squared Test:")
chi_squared
