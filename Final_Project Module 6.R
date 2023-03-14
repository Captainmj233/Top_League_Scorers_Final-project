#Prints Name
print("Kevin Appiah")

#Installs packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")

#Imports the libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

#Imports and names the data.csv dataset table Top_League_Scorers
Top_league_scorers <- read.csv('/Users/captainmj/Downloads/data.csv')

#Adds a new column Assist to the dataframe
Top_league_scorers <- mutate(Top_league_scorers, Assist = Shots - Goals )

#Displays Topl League Scorers Table
view(Top_league_scorers)

#Displays the First and last three rows of the dataset
headtail(Top_league_scorers)

#Displays structure of the Dataset
str(Top_league_scorers)

#Displays the summary of the Dataset
summary(Top_league_scorers)

#Creates an object player_names that displays player_names column
player_names <- Top_league_scorers$Player.Names

#Creates an object goals that displays goals  column
goals <- Top_league_scorers$Goals

#Creates an object Matches_played that displays matches played column
matches_played <- Top_league_scorers$Matches_Played

#Creates a variable Assist
Assist <- Top_league_scorers$Assist

#Calculates the sum of the object goals
sum(goals)

#Displays the summary of goals
summary(goals)

#Calculates the sum of the object matches_played
sum(matches_played)

#Displays the summary of matchhes_played
summary(matches_played)

#Calculates the sum of the object Assist
sum(Assist)

#Displays the summary of Assist
summary(Assist)

# Use aggregate to sum up the goals scored by each player
total_goals_by_players <- aggregate(goals~player_names, Top_league_scorers, sum)

# Sort the aggregated data frame in descending order by the number of goals
sorted_players<- total_goals_by_players[order(total_goals_by_players$goals, decreasing = TRUE),]

#Displays the sorted players
view(sorted_players)

# Select the top 10 players with the most goals
Top_10_goal_scorers <- head(sorted_players, n=10)

#Displays the top 10 players with most goals
view(Top_10_goal_scorers)

#Summarise the data of top 10 goal scorers
summary(Top_10_goal_scorers)

#Create a data frame for player and goals
Top_10_goal_scorers <- data.frame(
  player= c("Lionel Messi","Lewandowski","Cristiano Ronaldo","Ciro Immobile","Suarez"," Aubameyang","Timo Werner","Iago Aspas"
,"Mauro Icardi","Andrea Belotti"),
  goals <- c(135,127,111,107,95,88,82,80,76,74)
)

#Create a bar chart  of top 10 goal scorers
barplot(Top_10_goal_scorers$goals, names.arg = Top_10_goal_scorers$player, ylim = c(0, 150), xpd = TRUE, las = 2, main = "Top 10 Goal Scorers from Top Leagues from 2016 to 2020", xlab = "Players", ylab = "Goals Scored",cex.names = 0.6)

#Calculates the percentage of goals scored among top 10 goal scorers
percent_goals <- round(Top_10_goal_scorers$goals / sum(Top_10_goal_scorers$goals) * 100, 2)

#Assign colours to each player
colors <- rainbow(nrow(Top_10_goal_scorers))

#Create a pie chart displayinfg percentage of top 10 goal scorers
pie(percent_goals, labels = paste(Top_10_goal_scorers$player, percent_goals, "%"), col = colors, main = "Percentage of Goals Scored by Among Top 10 Players", percent = TRUE)

#Calculate the total number of goals scored by the top 10 goal scorers in your dataset.
Top10_total_goals <- sum(Top_10_goal_scorers$goals)

#Display summary of top 10 matches played object
summary(Top10_total_goals)

# Use aggregate to sum up the matches played by each player
agg_data <- aggregate(matches_played ~ player_names, Top_league_scorers, sum)

# Sort the aggregated data frame in descending order by the number of matches played
sorted_data <- agg_data[order(agg_data$matches_played, decreasing = TRUE),]

#Displays a table of sorted data
view(sorted_data)

# Select the top 10 players with the most matches played
Top10_matches_played <- head(sorted_data, n=10)

#Displays a table of top 10 players with most matches played
view(Top10_matches_played)

#Display summary of top 10 matches played object
summary(Top10_matches_played)

# Create a scatter plot of the top 10 players with rotated x-axis labels
ggplot(Top10_matches_played, aes(x = player_names, y = matches_played)) + 
  geom_point(size = 3) +
  xlab("Player") +
  ylab("Matches played") +
  ggtitle("Top 10 Players with the Most Matches Played from 2016 to 2020.") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Create an object assisted_ data and Use aggregate to sum up assist by each player
assist_data<- aggregate(Assist ~ player_names, Top_league_scorers, sum)

# Sort the aggregated data frame in descending order by the number of assists
sorted_assist_data <- assist_data[order(assist_data$Assist, decreasing = TRUE),]

# Select the top 10 players with the most assist
top10_assist_leaders <- head(sorted_assist_data, 10)

cat("Top 10 assist leaders:\n")
print(top10_assist_leaders)

#Displays a table oftop 10 assist leaders
view(top10_assist_leaders)

#Display summary of top 10 matches played object
summary(top10_assist_leaders)

#Plots a histogram of top 10 assist leaders with player names on the y-axis and number of assists on x-axis
ggplot(top10_assist_leaders, aes(x = Assist, y = player_names)) + 
  geom_histogram(stat = "identity", color = "black", fill = "blue") +
  labs(x = "Assists", y = "Player Names", title = "Top 10 Assist Leaders")


