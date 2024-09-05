install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest@main")
library(HypothesisTesting)

Cars <- read.csv('https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv')

summary(Cars)
head(Cars)

colors <- c('red', 'orange', 'yellow', 'green', 'blue', 'violet', 'purple')

# Finding car brand with the highest mean age of buyers.
results <- tapply(Cars$Buyer_Age, Cars$Car, mean)
barplot(results, ylab = 'Buyer\'s Age', xlab = 'Car Brand', col = colors)
results
# Ram has the highest mean age of buyers.

# Correcting significance level with Bonferroni Coeffiecent.
m_count <- length(unique(Cars$Car)) - 1
sig_lvl <- 0.05 / m_count
cat('Significance Level = ', sig_lvl * 100, '% or ', sig_lvl)
# The new significance level for the multiple hypothesis tests is 0.5%

permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Chevrolet', 'Ram')
# p-value = 0.0010, Reject H0
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Ford', 'Ram')
# p-value = 0.0021, Reject H0
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'GMC', 'Ram')
# p-value = 0.1759, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Honda', 'Ram')
# p-value = 0.0655, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Hyundai', 'Ram')
# p-value = 0.0134, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Jeep', 'Ram')
# p-value = 0.0151, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Kia', 'Ram')
# p-value = 0.3950, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Nissan', 'Ram')
# p-value = 0.4349, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Subaru', 'Ram')
# p-value = 0.1038, Fail to Reject
permutation_test(Cars, 'Car', 'Buyer_Age', 10000, 'Toyota', 'Ram')
# p-value = 0.3517, Fail to Reject



