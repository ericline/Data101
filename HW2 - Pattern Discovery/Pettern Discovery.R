moody <- read.csv("/Users/ericlin/Desktop/R/HW2 - Pattern Discovery/moody2023F1.csv")
head(moody)

pass <- subset(moody, moody$Grade == 'P')
fail <- subset(moody, moody$Grade == 'F')
head(pass)
head(fail)

# Rows: 1 - 8
unique(moody$Row)
# Sections: 1-3, B1-3
unique(moody$Section)
# Attendance: Low, Medium, High
unique(moody$Attendance)
# Score: 1-100
unique(moody$Score)

# Boxplot
summary(pass$Score)
summary(fail$Score)
boxplot(Score ~ Grade, data = moody, col = 'blue')

# To pass, must be above 40 score
min(pass$Score)
max(fail$Score)

# Overlap
overlap <- moody[moody$Score >= 41 & moody$Score <= 49,]
overlap$Grade <- ifelse(overlap$Grade == 'P', 1, 0)
summary(overlap)

tapply(overlap$Grade, overlap$Row, mean)

mosaicplot(overlap$Grade~overlap$Row, xlab = 'Grade', ylab = 'Row', main = 'Grade vs Row', col = c('red', 'blue', 'green'))
mosaicplot(overlap$Grade~overlap$Section, xlab = 'Grade', ylab = 'Section', main = 'Grade vs Section', col = c('red', 'blue', 'green'))
mosaicplot(overlap$Grade~overlap$Attendance, xlab = 'Grade', ylab = 'Attendance', main = 'Grade vs Attendance', col = c('red', 'blue', 'green'))

# Rule 1: Scores must be above 49.
a <- moody[moody$Score > 49,]
summary(a)
a[a$Grade == 'F',]

# Rule 2: Scores in the overlap point must be in even rows.
b <- moody[moody$Score >= 41 & moody$Score <= 49 & (moody$Row == 2 | moody$Row == 4 | moody$Row == 6 | moody$Row == 8),]
summary(b)
b[b$Grade == 'F',]


