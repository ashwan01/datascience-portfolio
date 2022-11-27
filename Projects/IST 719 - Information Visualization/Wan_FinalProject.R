#AUTHOR: ASHRAF WAN
#IST 719 FINAL PROJECT - Olympic Games Performance


#Get Kaggle file from Directory 
#URL: https://www.kaggle.com/datasets/surajjha101/countries-olympics-medals-since-1896
my.dir <- "Y:\\Homeworks\\Grad\\q5\\IST 719\\Final project\\"
df <- read.csv(file=paste0(my.dir, "olympics_medals_country_wise.csv")
               , header = TRUE
               , stringsAsFactors = FALSE)
#Load necessart libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(treemap)
install.packages("rworldmap")
library(rworldmap)

#check the structure of the dataset
str(df)
#Check for any NA
any(is.na(df))

#create df for summer specific-uses
sumDF <- data.frame(country = df$countries
                    , participation = df$summer_participations
                    , gold = df$summer_gold
                    , silver = df$summer_silver
                    , bronze = df$summer_bronze
                    , total_medal = df$summer_total)
str(sumDF)
any(is.na(sumDF))

#Looked at Wikipedia Olympic Chart for NA
#URL: https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table
sumDF[sumDF$country == "United States", "gold"] <- 1060
sumDF[sumDF$country == "United States", "total_medal"] <- 2629
sumDF[sumDF$country == "Soviet Union", "total_medal"] <- 1010

#Set Gold and Total_Medal class to Integer
sumDF$gold <- as.integer(sumDF$gold)
sumDF$total_medal <- as.integer(sumDF$total_medal)

#create df for winter specific-uses
winDF <- data.frame(country = df$countries
                    , participation = df$winter_participations
                    , gold = df$winter_gold
                    , silver = df$winter_silver
                    , bronze = df$winter_bronze
                    , total_medal = df$winter_total)
str(winDF)
any(is.na(winDF))

#create df for total only
totalDF <- data.frame(country = df$countries
                      , participation = df$total_participation
                      , gold = df$total_gold
                      , silver = df$total_silver
                      , bronze = df$total_bronze
                      , total_medal = df$total_total)
str(totalDF)
any(is.na(totalDF))

#Looked at Wikipedia Olympic Chart for NA
#URL: https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table
totalDF[totalDF$country == "United States", "total_medal"] <- 2959
totalDF[totalDF$country == "Soviet Union", "total_medal"] <- 1204
totalDF[totalDF$country == "United States", "gold"] <- 1173

#Set Gold and Total_Medal class to Integer
totalDF$gold <- as.integer(totalDF$gold)
totalDF$total_medal <- as.integer(totalDF$total_medal)


#create Map to show participation count for each countries for both olympics
#import and add iso code to df to make map work [there's some difference in code between IOC and ISO codes]
df2 <- read.csv(file=paste0(my.dir, "ioc2.csv")
                , header = TRUE
                , stringsAsFactors = FALSE)
df$iso_code <- as.factor(df2$iso_code)

str(df)

#Clean IOC Code
df$ioc_code <- gsub("[^[:alnum:] ]", "", df$ioc_code)

df[df$ioc_code == "USA", "total_total"] <- 2959
df[df$ioc_code == "URS", "total_total"] <- 1204
df$total_total <- as.integer(df$total_total)

#Set number of Category
num.cat <- 15

#Create Map data by joining ISO codes
df.map <- joinCountryData2Map(df, joinCode = "ISO_A3"
                              , nameJoinColumn = "iso_code")

#create map
mapCountryData(df.map
               , nameColumnToPlot = "total_participation"
               , numCats = num.cat
               , catMethod = c("pretty", "fixedWidth", "diverging", "quantiles")[1]
               , colourPalette = colorRampPalette(c("antiquewhite", "slategrey", "navajowhite3"))(num.cat)
               , borderCol = "peachpuff4"
               , mapTitle = "Total Participations in Both Olympics")

#create treemap for total number of medals
treemap(totalDF
        , index = c("country")
        , vSize = "total_medal"
        , type = "comp")

#top 10 medal count in summer olympic
#sort data to get the top 10 highest
sortedSum <- sumDF[order(sumDF$total_medal, decreasing = TRUE),]
top10Sum <- sortedSum[0:10,]

str(top10Sum)

#Use Dplyr to make multi bar graph with medal type breakdown
top10Sum %>% select(country, gold, silver, bronze) %>%
  pivot_longer(., cols = c(gold, silver, bronze), names_to = "medal", values_to = "total") %>%
  ggplot(aes(x = country, y = total, fill = medal)) + 
  geom_bar(stat = "identity", position="dodge") + 
  scale_fill_manual(values=c("#A77044", "#D6AF36", "#A7A7AD")) + 
  ggtitle("Top 10 Countries by Total Count of Medals in Summer Olympic") +
  xlab("Country Name") + ylab("Total Medal Count")


#top 10 medal count in winter olympic
#sort data to get the top 10 highest
sortedwin <- winDF[order(winDF$total_medal, decreasing = TRUE),]
top10Win <- sortedwin[0:10,] 

str(top10Win)

#Use Dplyr to make multi bar graph with medal type breakdown
top10Win %>% select(country, gold, silver, bronze) %>%
  pivot_longer(., cols = c(gold, silver, bronze), names_to = "medal", values_to = "total") %>%
  ggplot(aes(x = country, y = total, fill = medal)) + 
  geom_bar(stat = "identity", position="dodge") + 
  scale_fill_manual(values=c("#A77044", "#D6AF36", "#A7A7AD")) + 
  ggtitle("Top 10 Countries by Total Count of Medals in Winter Olympic") +
  xlab("Country Name") + ylab("Total Medal Count")

#top 10 medal count in both olympic
#sort data to get the top 10 highest
sortedboth <- totalDF[order(totalDF$total_medal, decreasing = TRUE),]
top10both <- sortedboth[0:10,] 

str(top10both)

#Use Dplyr to make multi bar graph with medal type breakdown
top10both %>% select(country, gold, silver, bronze) %>%
  pivot_longer(., cols = c(gold, silver, bronze), names_to = "medal", values_to = "total") %>%
  ggplot(aes(x = country, y = total, fill = medal)) + 
  geom_bar(stat = "identity", position="dodge") + 
  scale_fill_manual(values=c("#A77044", "#D6AF36", "#A7A7AD")) + 
  ggtitle("Top 10 Countries by Total Count of Medals in both Olympic") +
  xlab("Country Name") + ylab("Total Medal Count")


#CHECK WHICH IOC CODE IS DIFFERENT FROM ISO
#max_length <- max(c(length(df.map$ISO_A3), length(df$ioc_code)))
#temp2 <- c(df$ioc_code,rep(NA, max_length - length(df$ioc_code)))
#temp <- data.frame(col1 = df.map$ISO_A3, col2 = temp2)
#View(df$ioc_code)
#write.csv(df$ioc_code, file = "ioc2.csv")
#write.csv(df.map$ISO3_, file = "iso3.csv")
