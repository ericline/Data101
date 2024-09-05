install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
library(devtools)
library(HypothesisTesting)

songs <- read.csv("/Users/ericlin/Desktop/R/HW7 - Data Driven Blog/Spotify_Youtube.csv")
summary(songs)

# CLeaning data
songs$Views[is.na(songs$Views)] <- 0
songs$Energy[is.na(songs$Energy)] <- 0
mean <- mean(songs$Energy)
songs_yt <- songs

colors <- c('red','blue','cyan','yellow','green')

### Part A
# Categorize Energy Levels
songs_yt$EnergyLvl <- ifelse(songs_yt$Energy >= mean + sd(songs$Energy), "High", 
                      ifelse(songs_yt$Energy >= mean - sd(songs$Energy), "Medium", 
                      "Low"))

# Removing Outliers
Q1 <- quantile(songs$Views, 0.25)
Q3 <- quantile(songs$Views, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

lower_bound
upper_bound

songs_yt <- songs_yt[songs_yt$Views < upper_bound & songs_yt$Views > lower_bound,]

# Boxplot
boxplot(Views ~ EnergyLvl, data = songs_yt, col = colors, main = 'Energy Level vs Views on YouTube Songs', outline = FALSE, las = 0)

# Average Views of Different Energy Levels
tapply(songs_yt$Views, songs_yt$EnergyLvl, mean)

# Correcting significance level with Bonferroni Coeffiecent for 
# Multiple Hypothesis Testing
m_count <- length(unique(songs_yt$EnergyLvl)) - 1
sig_lvl <- 0.05 / m_count
cat('Significance Level = ', sig_lvl * 100, '% or ', sig_lvl)

# Permutation Test
permutation_test(songs_yt, 'EnergyLvl', 'Views', 10000, 'Low', 'Medium')
permutation_test(songs_yt, 'EnergyLvl', 'Views', 10000, 'High', 'Medium')

### Part B
# Bayesian Odds: What are the odds that a song with medium energy and above
# average views is an officially licensed music video.

avg <- mean(songs_yt$Views)
avg

# Clean data
songs_yt <- songs_yt[songs_yt$Licensed == 'True' | songs_yt$Licensed == 'False',]

# Barplot of Average Views
barplot(height = tapply(songs_yt[songs_yt$EnergyLvl == 'Medium',]$Views, songs_yt[songs_yt$EnergyLvl == 'Medium',]$Licensed, mean), col = colors, xlab = 'Licensed', ylab = 'Views', main = 'Mean Views of Licensed Songs')


Prior <- nrow(songs_yt[songs_yt$Licensed == 'True',]) / nrow(songs_yt)
Prior

PriorOdds <- Prior / (1 - Prior)
PriorOdds

TruePositive <- nrow(songs_yt[songs_yt$EnergyLvl == 'Medium' & songs_yt$Views > avg & songs_yt$Licensed == 'True',]) / nrow(songs_yt[songs_yt$Licensed == 'True',])
TruePositive

FalsePositive <- nrow(songs_yt[songs_yt$EnergyLvl == 'Medium' & songs_yt$Views > avg & songs_yt$Licensed != 'True',]) / nrow(songs_yt[songs_yt$Licensed != 'True',])
FalsePositive

LikelihoodRatio <- TruePositive / FalsePositive
LikelihoodRatio

PosteriorOdds <- LikelihoodRatio * PriorOdds
PosteriorOdds

Posterior <- round(PosteriorOdds / ( 1 + PosteriorOdds), 2)
Posterior
