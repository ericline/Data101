songs <- read.csv("/Users/ericlin/Desktop/R/Spotify_Youtube.csv")
colors<- c('red','blue','cyan','yellow','green')

## Test 1
# YouTube Songs: Energy vs Views
songs_yt <- subset(songs, select = c(20, 9, 22), songs$Views > 1)
head(songs_yt)

# Separate them into High vs Medium vs Low Energy Levels
songs_yt$EnergyLvl <- ifelse(songs_yt$Energy > 0.60, "High", ifelse(songs_yt$Energy > 0.30, "Medium", "Low"))

# Remove Outliers
IQR <- quantile(songs_yt$Views, 0.75) - quantile(songs_yt$Views, 0.25)
lb <- quantile(songs_yt$Views, 0.25) - 1.5 * IQR
ub <- quantile(songs_yt$Views, 0.75) + 1.5 * IQR
songs_yt <- songs_yt[songs_yt$Views >= lb & songs_yt$Views <= ub,]
head(songs_yt)

# Average Views of Different Energy Levels
tapply(songs_yt$Views, songs_yt$EnergyLvl, mean)

# Conclusion: It was as expected, the higher the energy
#             the higher the YouTube views.

## Test 2
# Spotify Songs: Valence vs Streams
songs_s <- subset(songs, select = c(20, 16, 28), songs$Stream > 1)
head(songs_s)

# Separate them into High vs Medium vs Low Energy Levels
songs_s$ValenceLvl <- ifelse(songs_s$Valence > 0.66, "High", 
                      ifelse(songs_s$Valence > 0.33, "Medium", "Low"))
head(songs_s)

# Average Streams of Different Energy Levels
streams = tapply(songs_s$Stream, songs_s$ValenceLvl, mean)
streams

# Barplot
summ <- data.frame(ValenceLvl = names(streams), MeanStream = streams)
barplot(summ$MeanStream, names.arg = summ$ValenceLvl, xlab = "Valence Level", ylab = "Average Spotify Stream", main = "Average Streams by Valence Level", col = colors)

# Conclusion: It was slightly unexpected, b/c people tend to listen
#             to sadder songs on Spotify as opposed to happier ones.

## Test 3
# Spotify Songs: Speechiness vs Streams
songs_s <- subset(songs, select = c(20, 12, 28), songs$Stream > 1)
head(songs_s)

# Remove Outliers
IQR <- quantile(songs_s$Stream, 0.75) - quantile(songs_s$Stream, 0.25)
lb <- quantile(songs_s$Stream, 0.25) - 1.5 * IQR
ub <- quantile(songs_s$Stream, 0.75) + 1.5 * IQR
songs_s <- songs_s[songs_s$Stream >= lb & songs_s$Stream <= ub,]
head(songs_s)

plot(songs_s$Speechiness,songs_s$Stream, xlab="Speechiness", ylab = "Streams",main="Speechiness vs Streams",col = "red")

# Conclusion: It was unexpected, b/c people tend to listen songs
#             with fewer lyrics as opposed to more lyrics.

## Test 4
# Spotify Songs: Duration vs Album Type
songs_s <- subset(songs, select = c(20, 18, 6), songs$Duration_ms > 1)
head(songs_s)

# Separate them into 4m vs 3m vs 2m vs 1m Durations
songs_s$Duration <- ifelse(songs_s$Duration_ms > 240000, "4-Min", 
                     ifelse(songs_s$Duration_ms > 180000, "3-Min", 
                     ifelse(songs_s$Duration_ms > 120000, "2-Min",
                     ifelse(songs_s$Duration_ms > 60000, "1-Min",
                     "0-Min"))))
head(songs_s)

table(songs_s$Duration, songs_s$Album_type)

mosaicplot(songs_s$Duration~songs_s$Album_type,xlab = 'Duration',ylab = 'Album Type', main = "Duration vs Album Type",col=colors,border="black")

