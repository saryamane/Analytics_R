# NFL game data starts with importing all the needed libraries upfront.

# Install the packages first

install.packages("XML")
install.packages("RSQLite")
install.packages("stringr")

# Load those installed packages into the 
# R environment using the library function.

library(ggplot2)
library(XML)
library(RSQLite)
library(stringr)

# Fetching the data from sports.yahoo.com

year <- 2014

url <- paste("http://sports.yahoo.com/nfl/stats/byteam?group=Offense&cat=Total&conference=NFL&year=season_",year,"&sort=530&old_category=Total&old_group=Offense")
url

# Now that we have the URL construcuted, we can now pull the data from it.

# We add [[7]] at the end because the table we want to pull 
# data from is the seventh element in the page.

offense <- readHTMLTable(url, encoding = "UTF-8", colClasses = "character")[[7]]

offense <- offense[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)]

offense[,1] <- as.character(offense[,1])
offense[,2:13] <- apply(offense[,2:13],2,as.numeric)
offense[,14] <- as.numeric(substr(offense[,14],1,2))*60 + as.numeric(substr(offense[,14],4,6))
offense

# Now our data is fairly clean for us to start our analysis.

# Let's do the same thing for defense as well.

url <- paste("http://sports.yahoo.com/nfl/stats/byteam?group=Defense&cat=Total&conference=NFL&year=season_",year,"&sort=530&old_category=Total&old_group=Defense")
url

defense <- readHTMLTable(url, encoding="UTF-8", colClasses = "character")[[7]]
defense

defense <- defense[,-c(2,4,6,8,10,12,14,16,18,20,22,24,26)]
defense[,1] <- as.character(defense[,1])
defense[,2:13] <- apply(defense[,2:13],2,as.numeric)
defense

# Now we have cleaned our defense data as well.

# Now let's combine our offense and defense data set into a single combined
# dataset, where we perform an inner join operation on the dataset.

combined <- merge(offense, defense, by.x = "Team", by.y = "Team")
combined

# Let's cleanup the combined data now.

# Since some of the offense and defense columns have 
# the same name, we will rename them to avoid confusion later. 
# We'll also get rid of the column from the defense data frame 
# that shows the number of games because it is 
# redundant now that we have combined data:

colnames(combined)[2] <- "Games"
colnames(combined)[3] <- "OffPPG"
colnames(combined)[4] <- "OffYPG"
colnames(combined)[5] <- "OffPassYPG"
colnames(combined)[6] <- "OffRushYPG"
combined$G.y <- NULL # This is how you delete a column from a dataset, just set it to NULL
colnames(combined)[15] <- "DefPPG"
colnames(combined)[16] <- "DefYPG"
colnames(combined)[17] <- "DefRushYPG"
colnames(combined)[18] <- "DefPassYPG"

colnames(combined)

head(combined)

# Let's plot some basic graphs using the histogram,
# where we plot the number of offenseive pts per team.

# Run some basic stats on the distribution.
mean(combined$OffPPG)
sd(combined$OffPPG)
max(combined$OffPPG)
min(combined$OffPPG)

# Plot the historgram and see that most team score 
# around 18-28 points area, per the graph

hist(combined$OffPPG, breaks = 10, main="Offensive Points Per Game", xlab="Offensive PPG", ylab="Number of teams")

# Let's do a similar exercise for the defense data

hist(combined$DefPPG, breaks=10, main="Defensive Points per Game", xlab="Defensive PPG", ylab="Number of teams")

# Plot number of first downs on the game, an offensive statistic

hist(combined$"1stD/G", breaks=10, main="Offensive 1st Downs Per Game", xlab="1st Downs/Game",ylab="Number of Teams")

# Next we move to creating a bar chart for this graph, which
# allows us to compare the performance against other teams.

head(combined)
ppg <- transform(combined, Team=reorder(Team, combined$OffPPG)) # You are just sorting in this step here.
head(ppg)

ggplot(ppg,aes(x=Team, y=OffPPG)) + geom_bar(stat='identity', color='black',fill='blue') + coord_flip() + labs(x="Team", y="Average Points per Game") + ggtitle("Avg points per game")

# Similarly we do this for the defense YPG metric

ypg <- transform(combined, Team=reorder(Team, -combined$DefYPG)) # This sorts in the asc order.
ggplot(ypg,aes(x=Team, y=DefYPG)) + geom_bar(stat='identity', color='black',fill='blue') + coord_flip() + labs(x="Team", y="Average Yards allowed per Game") + ggtitle("Avg yards allowed per game")

# Now that we have dealt with the bar graph, we look at the scatter plot to see how  
# the offense yards vs offensive points are correlated to each other.

ggplot(combined, aes(x=combined$OffYPG, y=combined$OffPPG)) + geom_point(shape=5, size=2) + geom_smooth() + labs(x="Yards per game", y="Points per Game") + ggtitle("Offense Yards vs. Points per Game") + theme(plot.title = element_text(size=18, face="bold"))

# Looks like it tells us that the more yards you do, the more points you gain. They are very correlated to each other.
# Let's look at the correlation coefficient for those 2 metrics.

cor(combined$OffYPG, combined$OffPPG)

# 0.87 is a high positive correlation.

# Similarly lets look at the correlation for the defense
# relating to the yards allowed with respect to points in the game.

# Theoretically, if defense is able to restrict the number of yards
# then it is limiting the ability for the offense to score points.
# Ideally you would want to see this as a postive correlation.
# Lets try to confirm that hypothesis.

ggplot(combined, aes(x=combined$DefYPG, y=combined$DefPPG)) + geom_point(shape=5, size=2) + geom_smooth() + labs(x="Yards allowed per game", y="Points allowed per Game") + ggtitle("Defense Yards vs. Points per Game") + theme(plot.title = element_text(size=18, face="bold"))

# Looking at the correlation coefficient

cor(combined$DefYPG,combined$DefPPG)

# This correlation of 0.68 is also +ve, but is not as strong as the one fore Offense we saw earlier.

# Now lets try one more correlation with the thought as the longer the team is on offense, the higher the points scored.
# Let's test this hypothesis.

ggplot(combined, aes(x=combined$TOP, y=combined$OffPPG)) + geom_point(shape=5, size=2) + geom_smooth() + labs(x="Time of Posession (in seconds)", y="Points scored per Game") + ggtitle("Time of Possession vs. Points per Game") + theme(plot.title = element_text(size=18, face="bold"))

cor(combined$TOP,combined$OffPPG)

# Output is 0.37, this has a very low correlation, as the data tells us compared to the others we found.

# Lets calculate the teams offensive strength score. the passing strength is PassYds / G, which is average passing 
# yards per game. The higher this number, the stronger the team is.

offense$OPassStrength <- max(offense[,5])-offense[,5]
offense$OPassStrength <- (1-(offense$OPassStrength/max(offense$OPassStrength)))*100
head(offense$OPassStrength)

# We do the same for Rushing Yards.

offense$ORushStrength <- max(offense[,6]) - offense[,6]
offense$ORushStrength <- (1-(offense$ORushStrength/max(offense$ORushStrength)))*100
head(offense$ORushStrength)

# Offensive points per yard game

offense$OPPGStrength <- max(offense[,3]) - offense[,3]
offense$OPPGStrength <- (1-(offense$OPPGStrength/max(offense$OPPGStrength)))*100
head(offense$OPPGStrength)

offense$OYPGStrength <- max(offense[,4]) - offense[,4]
offense$OYPGStrength <- (1-(offense$OYPGStrength/max(offense$OYPGStrength)))*100
head(offense$OYPGStrength)

# Now average out the offense strengh number here.

offense$OffStrength <- (offense$OPassStrength + offense$ORushStrength + offense$OPPGStrength + offense$OYPGStrength) / 4

head(offense$OffStrength)

# Similarly when you pull similar metrics for defense, you will not subtract it by 1
# because in that category lower numbers means higher strength.

defense$DPassStrength <- max(defense[,6])-defense[,6]
defense$DPassStrength <- defense$DPassStrength/max(defense$DPassStrength) * 100
head(defense$DPassStrength)

# Rushing defense strength

defense$DRushStrength <- max(defense[,5])-defense[,5]
defense$DRushStrength <- defense$DRushStrength/max(defense$DRushStrength)*100

# Points and yards allowed per game metric

defense$DPPGStrength <- max(defense[,3])-defense[,3]
defense$DPPGStrength <- defense$DPPGStrength/max(defense$DPPGStrength)*100
head(defense$DPPGStrength)

defense$DYPGStrength <- max(defense[,4])-defense[,4]
defense$DYPGStrength <- defense$DYPGStrength/max(defense$DYPGStrength)*100
head(defense$DYPGStrength)

# Now let's take the average of the strength.

defense$DefStrength <- (defense$DPassStrength + defense$DRushStrength + defense$DPPGStrength + defense$DYPGStrength)/4

# The purpose of indees is to simplify and standardiz the underlying statistics,
# so that they can easily be interpreted and compared. We took multiple metrics
# and converted them to a definitive single value for each team to simplify
# our analysis.

# Now that we have our metrics ready, lets start by simulating a sinple
# game to see how our teams would perform when faced with each other,
# i.e. computing their odds to win.

home_team <- "Chicago Bears"
away_team <- "New Orleans Saints"

# Next we bring their offensive and defensive strengths that we calculated
# for this team earlier.

off_game <- subset(offense, Team==home_team | Team==away_team)[,c(1,15,16,19)]
head(off_game)

def_game <- subset(defense, Team==home_team | Team==away_team)[,c(1,14,15,18)]
head(def_game)

game <- merge(off_game, def_game, by.x="Team", by.y="Team")

game

# Now lets compute the Net_Pass score for the teams.

game$Net_Pass[game$Team==home_team] <- game$OPassStrength[game$Team == home_team] - game$DPassStrength[game$Team == away_team]

game$Net_Pass[game$Team==away_team] <- game$OPassStrength[game$Team == away_team] - game$DPassStrength[game$Team == home_team]

# Let's look at the characteristic of the rushing game of each team.

game$Net_Rush[game$Team==home_team] <- game$ORushStrength[game$Team==home_team] - game$DRushStrength[game$Team==away_team]
game$Net_Rush[game$Team==away_team] <- game$ORushStrength[game$Team==away_team] - game$DRushStrength[game$Team==home_team]

game$Net_Total[game$Team==home_team] <- game$OffStrength[game$Team==home_team] - game$DefStrength[game$Team==away_team]
game$Net_Total[game$Team==away_team] <- game$OffStrength[game$Team==away_team] - game$DefStrength[game$Team==home_team]

game

# Let's make Net_Total the sum of all the three metrics.

game$Net_Total <- game$Net_Pass + game$Net_Rush + game$Net_Total
game

if(game$Net_Total[game$Team==home_team] >= game$Net_Total[game$Team==away_team]) {
  winner <- home_team
  loser <- away_team
}else{
  winner <- away_team
  loser <- home_team
}

print(paste(winner, "-- BEATS -- ", loser))

# Next, we try to simulate multiple games where the outcomes are
# determined by statistics alone.

# Develop the play schedule for the NFL games.

games_per_team <- 50
for (week in 1:games_per_team) {
  home_index <- sample(1:32, 16, replace=F)
  home_teams <- data.frame(HomeTeam=offense[home_index,1])
  away_teams <- data.frame(AwayTeam=offense[-home_index, 1])
  
  if (week==1){
    schedule <- cbind(Week=week, HomeTeam=home_teams, AwayTeam=away_teams)
  } else {
    temp <- cbind(Week=week, HomeTeam=home_teams, AwayTeam=away_teams)
    schedule <- rbind(schedule, temp)
  }
}

schedule

# Awesome way to create an automated schedule of the teams.

# Next we will create a team record tracker that will track the number of wins or loses.

records <- data.frame(Team=offense$Team)
records
records$Wins <- 0
records$Losses <- 0

for (i in 1:nrow(schedule)) {
  home_team <- schedule[i,2]
  away_team <- schedule[i,3]
  week <- schedule[i,1]
  
  off_game <- subset(offense, Team==home_team | Team==away_team)[,c(1,15,16,19)]
  
  def_game <- subset(defense, Team==home_team | Team==away_team)[,c(1,14,15,18)]
  
  game <- merge(off_game, def_game, by.x="Team", by.y="Team")
  
  # Now lets compute the Net_Pass score for the teams.
  
  game$Net_Pass[game$Team==home_team] <- game$OPassStrength[game$Team == home_team] - game$DPassStrength[game$Team == away_team]
  
  game$Net_Pass[game$Team==away_team] <- game$OPassStrength[game$Team == away_team] - game$DPassStrength[game$Team == home_team]
  
  # Let's look at the characteristic of the rushing game of each team.
  
  game$Net_Rush[game$Team==home_team] <- game$ORushStrength[game$Team==home_team] - game$DRushStrength[game$Team==away_team]
  game$Net_Rush[game$Team==away_team] <- game$ORushStrength[game$Team==away_team] - game$DRushStrength[game$Team==home_team]
  
  game$Net_Total[game$Team==home_team] <- game$OffStrength[game$Team==home_team] - game$DefStrength[game$Team==away_team]
  game$Net_Total[game$Team==away_team] <- game$OffStrength[game$Team==away_team] - game$DefStrength[game$Team==home_team]
  
  # Let's make Net_Total the sum of all the three metrics.
  
  game$Net_Total <- game$Net_Pass + game$Net_Rush + game$Net_Total
  game
  
  if(game$Net_Total[game$Team==home_team] >= game$Net_Total[game$Team==away_team]) {
    winner <- home_team
    loser <- away_team
  } else{
    winner <- away_team
    loser <- home_team
  }
  
  if(i==1) {
    winnerdf <- data.frame(Winner=winner)
    losserdf <- data.frame(Loser=loser)
    results <- cbind(winnerdf,losserdf)
  } else {
    winnerdf <- data.frame(Winner=winner)
    losserdf <- data.frame(Loser=loser)
    temp <- cbind(winnerdf, losserdf)
    results <- rbind(results, temp)
  }
  
  records$Wins[records$Team==winner] <- as.numeric(records$Wins[records$Team==winner]) + 1
  records$Losses[records$Team==loser] <- as.numeric(records$Losses[records$Team==loser]) + 1
  
  print(paste("Week", week, ":", winner, " beat ", loser))
}

records

records <- records[order(-records$Wins),]
records

# Throughout this chapter we use R's flexibility to use as a calcultor
# chart generator and progg language to complete a project.

