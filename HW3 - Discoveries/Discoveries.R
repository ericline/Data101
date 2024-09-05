Cars <- read.csv("/Users/ericlin/Desktop/R/HW3 - Discoveries/Cars2023c-1.csv")

mean <- mean(Cars$Buyer_Income)
mean

# Finding highest and lowest mean combination of Dealership and Car Type
tapply(Cars$Buyer_Income, Cars$Dealership, mean)
tapply(Cars[Cars$Dealership == 'Seattle',]$Buyer_Income, Cars[Cars$Dealership == 'Seattle',]$Car, mean)
tapply(Cars[Cars$Dealership == 'LA',]$Buyer_Income, Cars[Cars$Dealership == 'LA',]$Season, mean)

S1 <- Cars[Cars$Dealership == 'Seattle' & Cars$Car == 'Nissan',]
S2 <- Cars[Cars$Dealership == 'LA' & Cars$Season == 'Fall',]

# Q1 Mean
Q1 <- mean(S1$Buyer_Income)
Q1

# Q1 Difference
Q1_diff <- Q1 - mean
Q1_diff

# Q2 Mean
Q2 <- mean(S2$Buyer_Income)
Q2

# Q2 Difference
Q2_diff <- mean - Q2
Q2_diff
  
## Cross Validation
# Split Data
Cars1 <- Cars[1:25000,]
Cars2 <- Cars[25001:50000,]

# Mean of each subset
mean1 <- mean(Cars1$Buyer_Income)
mean2 <- mean(Cars2$Buyer_Income)
mean
mean1
mean2
# Observation: Means are very similar

# Q1 Mean
Cars1_Q1 <- mean(Cars1[Cars1$Dealership == 'Seattle' & Cars1$Car == 'Nissan',]$Buyer_Income)
cat('Cars1 Q1 Diff: ', Cars1_Q1 - mean1, 'vs Original: ', Q1_diff)
Cars2_Q1 <- mean(Cars2[Cars2$Dealership == 'Seattle' & Cars2$Car == 'Nissan',]$Buyer_Income)
cat('Cars2 Q1 Diff: ', Cars2_Q1 - mean2, 'vs Original: ', Q1_diff)

# Q2 Mean
Cars1_Q2 <- mean(Cars1[Cars1$Dealership == 'LA' & Cars1$Season == 'Fall',]$Buyer_Income)
cat('Cars1 Q2 Diff: ', mean1 - Cars1_Q2, 'vs Original: ', Q2_diff)
Cars2_Q2 <- mean(Cars2[Cars2$Dealership == 'LA' & Cars2$Season == 'Fall',]$Buyer_Income)
cat('Cars2 Q2 Diff: ', mean2 - Cars2_Q2, 'vs Original: ', Q2_diff) 
# Observation: Means are different compared to before splitting data.
#              The mean of Cars2 using Q2 is also no longer over 3000 difference.

# Split Data
Cars1 <- Cars[1:12500,]
Cars2 <- Cars[12501:25000,]
Cars3 <- Cars[25001:37500,]
Cars4 <- Cars[37501:50000,]

# Means of each subset
mean1 <- mean(Cars1$Buyer_Income)
mean2 <- mean(Cars2$Buyer_Income)
mean3 <- mean(Cars3$Buyer_Income)
mean4 <- mean(Cars4$Buyer_Income)
cat('1: ', mean1, ' 2: ', mean2, ' 3: ', mean3, ' 4: ', mean4, ' Original: ', mean)
# Observation: Means are different but close to the original.

# Q1 Means
Cars1_Q1 <- mean(Cars1[Cars1$Dealership == 'Seattle' & Cars1$Car == 'Nissan',]$Buyer_Income)
cat('Cars1 Q1 Diff: ', Cars1_Q1 - mean1, 'vs Original: ', Q1_diff)
Cars2_Q1 <- mean(Cars2[Cars2$Dealership == 'Seattle' & Cars2$Car == 'Nissan',]$Buyer_Income)
cat('Cars2 Q1 Diff: ', Cars2_Q1 - mean2, 'vs Original: ', Q1_diff)
Cars3_Q1 <- mean(Cars3[Cars3$Dealership == 'Seattle' & Cars3$Car == 'Nissan',]$Buyer_Income)
cat('Cars3 Q1 Diff: ', Cars3_Q1 - mean3, 'vs Original: ', Q1_diff)
Cars4_Q1 <- mean(Cars4[Cars4$Dealership == 'Seattle' & Cars4$Car == 'Nissan',]$Buyer_Income)
cat('Cars4 Q1 Diff: ', Cars4_Q1 - mean4, 'vs Original: ', Q1_diff)

# Q2 Means
Cars1_Q2 <- mean(Cars1[Cars1$Dealership == 'LA' & Cars1$Season == 'Fall',]$Buyer_Income)
cat('Cars1 Q2 Diff: ', mean1 - Cars1_Q2, 'vs Original: ', Q2_diff)
Cars2_Q2 <- mean(Cars2[Cars2$Dealership == 'LA' & Cars2$Season == 'Fall',]$Buyer_Income)
cat('Cars2 Q2 Diff: ', mean2 - Cars2_Q2, 'vs Original: ', Q2_diff) 
Cars3_Q2 <- mean(Cars3[Cars3$Dealership == 'LA' & Cars3$Season == 'Fall',]$Buyer_Income)
cat('Cars3 Q2 Diff: ', mean3 - Cars3_Q2, 'vs Original: ', Q2_diff)
Cars4_Q2 <- mean(Cars4[Cars4$Dealership == 'LA' & Cars4$Season == 'Fall',]$Buyer_Income)
cat('Cars4 Q2 Diff: ', mean4 - Cars4_Q2, 'vs Original: ', Q2_diff) 

# Observation: The means are drastically different and getting more different the greater the
#              splitting