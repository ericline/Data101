install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)

party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")
summary(party)
head(party)
unique(party$Music)
unique(party$DJ)
unique(party$Day)

colors <- c('red', 'orange', 'yellow', 'green', 'blue', 'violet', 'purple')

# Part A

# Null Hypothesis: The average "Attendance" value is the same when Music == "Techno" and when Music == "Salsa".
# Alternate Hypothesis: The average "Attendance" value is higher when Music == "Techno" than when Music == "Salsa".
boxplot(party$Attendance ~ party$Music, ylab = "Attendance", xlab = "Music", col = colors)
tapply(party$Attendance, party$Music, mean)
permutation_test(party, 'Music', 'Attendance', 10000, 'Salsa', 'Techno')
# p-value of ~0.016

# Null Hypothesis: The average "Attendance" value of Alex parties is the same when Music == "Techno" and when Music == "Jazz".
# Alternate Hypothesis: The average "Attendance" value of Blue parties is higher when Music == "Techno" than when Music == "Jazz".
boxplot(party$Attendance ~ party$DJ, ylab = 'Attendance', xlab = 'DJ', col = colors)
df <- party[party$DJ == 'Alex',]
tapply(df$Attendance, df$Music, mean)
permutation_test(df, 'Music', 'Attendance', 10000, 'Jazz', 'Techno')
# p-value of ~0.010

# Null Hypothesis: The average "Attendance" value of Friday parties is the same when DJ == "Blue" and when DJ == "Alex".
# Alternate Hypothesis: The average "Attendance" value of Blue parties is higher when DJ == "Blue" than when Music == "Alex".
boxplot(party$Attendance ~ party$Day, ylab = 'Attendance', xlab = 'Day', 
        col = colors)
df <- party[party$Day == 'Friday',]
tapply(df$Attendance, df$DJ, mean)
permutation_test(df, 'DJ', 'Attendance', 10000, 'Alex', 'Blue')
# p-value of ~0.002

# Part B

# Null Hypothesis: The average "Attendance" value of Friday parties is the same when DJ == "Blue" and when DJ == "Alex".
# Alternate Hypothesis: The average "Attendance" value of Blue parties is higher when DJ == "Blue" than when Music == "Alex".
A <- permutation_test(df, 'DJ', 'Attendance', 10000, 'Alex', 'Blue')
A
# p-value of ~0.002

boxplot(party$Attendance ~ party$DJ, ylab = "Attendance", xlab = "DJ", 
        col = colors)
tapply(party$Attendance, party$DJ, mean)
df <- party[party$DJ == 'Ania',]
tapply(df$Attendance, df$Day, mean)

# Null Hypothesis: The average "Attendance" value of DJ "Ania" parties is the same when Day == "Saturday" and when Day == "Thursday".
# Alternate Hypothesis: The average "Attendance" value of DJ "Ania" parties is the higher when Day == "Saturday" than when Day == "Thursday",
B <- permutation_test(df, 'Day', 'Attendance', 10000, 'Thursday', 'Saturday')
B
# p-value = 0.032 < 0.05, which means we can reject our Null Hypothesis.

boxplot(party$Attendance ~ party$DJ, ylab = 'Attendance', xlab = 'DJ', col = colors)
tapply(party$Attendance, party$DJ, mean)
df <- party[party$DJ == 'Carol',]
tapply(df$Attendance, df$Day, mean)
# Null Hypothesis: The average "Attendance" value of DJ "Carol" parties is higher when Day == "Saturday" than when Day == "Thursday".
# Alternate Hypothesis: The average "Attendance" value of DJ "Carol" parties is the same when Day == "Saturday" and when Day == "Thursday",
C <- z_test_from_data(df, 'Day', 'Attendance', 'Friday', 'Saturday')
C
# p-value = 0.2059 > 0.05, which means we fail to reject our Null Hypothesis.



