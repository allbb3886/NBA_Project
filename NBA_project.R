library(dplyr)
library(ggplot2)
library(randomForest)
library(arm)
library(stringr)
library(gridExtra)
library(formattable)
library(corrplot)
#rm(list=ls())

## Import the Data in
all.nba <- read.csv("all_nba_stats.csv", stringsAsFactors = FALSE, header = TRUE,skip=1)
nba.players <- read.csv("Seasons_Stats.csv", stringsAsFactors = FALSE, header = TRUE)


dim(all.nba)
dim(nba.players)
head(all.nba)
head(nba.players)


##cleaning the data.
all.nba$Year <- as.numeric(substr(all.nba$Season, start = 1, stop = 4)) + 1
head(all.nba[c(1:10), c(3, 34)], 10) ## to check that worked. 
nba.players.postMJ <- nba.players %>% filter(Year > 1998)
all.nba.postMJ <- all.nba %>% filter(Year > 1998 & Year < 2018)


## Missing NBA data?

sum(is.na(nba.players.postMJ))


## Need to address the missing data problem.
nba.players.postMJ$blanl <- NULL
nba.players.postMJ$blank2<- NULL

# Summing up all the NA's in each column
colSums(is.na(nba.players.postMJ))
# Notice NA in PER, and USG columns, are these the same individuals?

identical(which(is.na(nba.players.postMJ$PER)), which(is.na(nba.players.postMJ$USG.)))

## notice that these are identical matches, so we want to exmaine where these occur.

which(is.na(nba.players.postMJ$PER))

nba.players.postMJ[c(3819, 4136, 5099, 6069, 7957), ]

#Let us remove these observations as they did not play enough minutes to generate real values.
nba.players.postMJ<-nba.players.postMJ[-c(3819,4136,5099,6069,7957),]

# Remove the "double count", where stats are counted for players which play for multiple teams.
nba.players.postMJ[c(9429:9431),]
nba.players.postMJ <- subset(nba.players.postMJ, !Tm == "TOT")

#Josh Smith example
which(all.nba.postMJ$Tm == "TOT")
all.nba.postMJ[239, 5] <- "ATL"
all.nba.postMJ[180, 5] <- "DEN"

#### Converting to PER GAME statistics Name
#Position: The player's position (PG/SG/SF/PF/C)
#age
#year
#Team
##Games
#Starts: How many of the games the player played in did they start?
#Minutes (note that from minutes through to FGs, all values are converted to per-game)
#Points
#Rebounds
#Assists
#Steals
#Blocks
#Turnovers
#Fouls
#FTs: Number of made free-throws.
#Threes: Number of made three point shots.
#FGs: Number of made field goals.
#Usage: Statistic represented how involved the player was in his team's plays.
#EfficiencyRating: Advanced statistic developed by Hollinger to calculate a player's overall efficiency/ output (known as PER).
#BoxPlusMinus: Another statistical metric of overall player performance.
#ShootingPercentage: Average shot percentage, weighted based on different values of different shots (e.g. three point shot is worth 1.5 times two point shot)





## Creating the " All NBA" Indictaor

nba_pergame_stats <- nba.players.postMJ %>% mutate(Name = Player, Position = Pos, age = Age, year = Year,  Team = Tm, Games = G, Starts = GS, Minutes = MP/G, Points = PTS/G, Rebounds = TRB/G, Assists = AST/G, Steals = STL/G, Blocks = BLK/G, Turnovers = TOV/G, Fouls = PF/G, FTs = FT/G, Threes = X3P/G, FGs = FG/G, Usage = USG., EfficiencyRating = PER, BoxPlusMinus = BPM, ShootingPercentage = eFG.)
nba_pergame_stats <- nba_pergame_stats[ , c(52:73)]
two.digit.round <- function(x) round(x, 2)
nba_pergame_stats[ , c(8:18)] <- sapply(nba_pergame_stats[ , c(8:18)], two.digit.round)
str(nba_pergame_stats)

## Checking for Outliers 
# Outliers?
nba_pergame_stats <- nba_pergame_stats %>% filter(Games > 10 & Minutes > 5)

nba_pergame_stats$ID <- str_c(substr(nba_pergame_stats$Name, start = 1, stop = 3), substr(nba_pergame_stats$age, start = 1, stop = 2), substr(nba_pergame_stats$Team, start = 1, stop = 3), substr(nba_pergame_stats$year, start = 3, stop = 4), sep = "")
all.nba.postMJ$ID <- str_c(substr(all.nba.postMJ$Player, start = 1, stop = 3), substr(all.nba.postMJ$Age, start = 1, stop = 2), substr(all.nba.postMJ$Tm, start = 1, stop = 3), substr(all.nba.postMJ$Year, start = 3, stop = 4), sep = "")
nba_pergame_stats$All.NBA <- ifelse(nba_pergame_stats$ID %in% all.nba.postMJ$ID, 1, 0)
sum(nba_pergame_stats$All.NBA)

## check for deviations
nba_pergame_stats.check<- nba_pergame_stats %>%
  filter(All.NBA==1) %>%
  group_by(year) %>%
  summarise(length(Name))
nba_pergame_stats.check

## check Year 2013 :
nba_pergame_stats[nba_pergame_stats$year == 2013 & nba_pergame_stats$All.NBA == 1, ]
#remove James Anderson?
nba_pergame_stats[6047,24] <- 0



#### Exploratory Analysis

## Looking at density plots

points_density <- ggplot(nba_pergame_stats, aes(Points)) + geom_density(fill="skyblue") + geom_vline(aes(xintercept = mean(Points)), linetype="dashed")
points_density
#Rebound Density
rebounds_density<- ggplot(nba_pergame_stats,aes(Rebounds)) + geom_density(fill="mediumorchid1") + 
  geom_vline(aes(xintercept=mean(Rebounds)) , linetype= "dashed")
rebounds_density
#assist density
assists_density <- ggplot(nba_pergame_stats, aes(Assists)) + geom_density(fill = "tomato") + geom_vline(aes(xintercept = mean(Assists)), linetype = "dashed")
assists_density
### Turnover Density
turnovers_density <- ggplot(nba_pergame_stats, aes(Turnovers)) + geom_density(fill = "mediumaquamarine") + geom_vline(aes(xintercept = mean(Turnovers)), linetype = "dashed")
turnovers_density
grid.arrange(points_density, rebounds_density, assists_density, turnovers_density, ncol = 2)


## Explaining the inverse relationship between age and output?

nba.by.age<- nba_pergame_stats %>% 
  group_by(age) %>%
  summarise(Efficiency=mean(EfficiencyRating),
            Players=length(Name)) %>%
  ggplot(aes(age,Efficiency)) + geom_point(aes(size=Players), colour ="peru") +
  geom_smooth(method="loess",colour="seashell4", se=FALSE, linetype="dashed") + theme_bw()
nba.by.age

## Does a correlation exist between efficiency and other variables, such as usage, shooting percentage, minutes?

PER.usage <- ggplot(nba_pergame_stats,aes(Usage,EfficiencyRating)) + geom_point(colour="lightsteelblue4",alpha=0.5) + geom_smooth(method=lm, colour="navyblue", linetype="dashed")
PER.usage
PER.minutes <- ggplot(nba_pergame_stats,aes(Minutes,EfficiencyRating)) + geom_point(colour="lightsteelblue4",alpha=0.5) + geom_smooth(method=lm, colour="navyblue", linetype="dashed") 
PER.minutes
PER.threes <- ggplot(nba_pergame_stats,aes(Threes,EfficiencyRating)) + geom_point(colour="lightsteelblue4",alpha=0.5) + geom_smooth(method=lm, colour="navyblue", linetype="dashed")
PER.threes
PER.shooting <- ggplot(nba_pergame_stats,aes(ShootingPercentage,EfficiencyRating)) + geom_point(colour="lightsteelblue4",alpha=0.5) + geom_smooth(method=lm, colour="navyblue", linetype="dashed")
PER.shooting
grid.arrange(PER.minutes,PER.shooting,PER.threes,PER.usage)

## as we can see we notice positive and lienar relationships on the four variables and efficiency rating.


### How correlated are our predictors?

nba_variable_corr_matrix<-as.matrix(nba_pergame_stats[,c(6:20)])
corrplot(cor(nba_variable_corr_matrix), is.corr=FALSE, method="circle", type="upper")


## How to estimate relationship between ALL NBA and set of predictors?
#Let us visualise probability of being selected for ALL NBA teams for different values of statistics.
#We will use log regression, or the GLM function with binomial options. This will
# estimate the odds of ALL NBA selection at differenvalues, and ask R to use estimated odds to predict probability of selection within each interval

nba_pergame_stats$All.NBA <- as.factor(nba_pergame_stats$All.NBA)

#Taking the log of points
log.points <- glm(All.NBA~Points,family=binomial, data=nba_pergame_stats) ## performing the logitistic regression
points.probability<- data.frame(Points=seq(0,40,0.1)) #this gives a matrix as  abasis to predict all nba likelihood for each point interval.
points.prediction <- predict(log.points,points.probability,type="response") ## running the prediction.
points.probability<-cbind(points.probability,points.prediction)
names(points.probability)<-c("Points", "Probability")

##plotting the chances for points
points.gg <- ggplot(points.probability, aes(Points, Probability)) + geom_line() + geom_vline(xintercept = mean(nba_pergame_stats$Points), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba_pergame_stats$Points, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 24, y = 0.8, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 7, y = 0.8, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3)
points.gg

#do the same procedure for rebounds
log.rebounds <- glm(All.NBA ~ Rebounds, family = binomial, data = nba_pergame_stats)
rebounds.probability <- data.frame(Rebounds = seq(0, 25, 0.1))
rebounds.prediction <- predict(log.rebounds, rebounds.probability, type = "response") 
rebounds.probability <- cbind(rebounds.probability, rebounds.prediction)
names(rebounds.probability) <- c("Rebounds", "Probability") 
rebounds.gg <- ggplot(rebounds.probability, aes(Rebounds, Probability)) + geom_line() + geom_vline(xintercept = mean(nba_pergame_stats$Rebounds), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba_pergame_stats$Rebounds, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 10.5, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 3, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3)

## Assists
log.assists <- glm(All.NBA ~ Assists, family = binomial, data = nba_pergame_stats)
assists.probability <- data.frame(Assists = seq(0, 20, 0.1))
assists.prediction <- predict(log.assists, assists.probability, type = "response") 
assists.probability <- cbind(assists.probability, assists.prediction)
names(assists.probability) <- c("Assists", "Probability") 
assists.gg <- ggplot(assists.probability, aes(Assists, Probability)) + geom_line() + geom_vline(xintercept = mean(nba_pergame_stats$Assists), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba_pergame_stats$Assists, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 8, y = 0.75, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 1.2, y = 0.75, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## PER
log.PER <- glm(All.NBA ~ EfficiencyRating, family = binomial, data = nba_pergame_stats)
PER.probability <- data.frame(EfficiencyRating = seq(0, 40, 0.1))
PER.prediction <- predict(log.PER, PER.probability, type = "response")
PER.probability <- cbind(PER.probability, PER.prediction)
names(PER.probability) <- c("PER", "Probability")
PER.gg <- ggplot(PER.probability, aes(PER, Probability)) + geom_line() + geom_vline(xintercept = mean(nba_pergame_stats$EfficiencyRating), colour = "deepskyblue", linetype = "dashed") + geom_vline(xintercept = quantile(nba_pergame_stats$EfficiencyRating, 0.99), colour = "sienna2", linetype = "dashed") + annotate("text", x = 24, y = 0.9, label = "99th percentile", angle = 90, colour = "sienna2", size = 3) + annotate("text", x = 11, y = 0.9, label = "Mean average", angle = 90, colour = "deepskyblue", size = 3) 

## Display all the graphs
grid.arrange(points.gg,rebounds.gg,assists.gg,PER.gg, top="The probability of being selected for ALL NBA teams at different levels of output")


#anywhere near the mean average for each statistic, the chances of making ALL NBA is ~ 0.

## Multivariate logistic regression,using R to produce odds ratios of the minimal adequate model factors.

nba_pergame_stats$ShootingPercentage <- nba_pergame_stats$ShootingPercentage * 100
multi.log.mam <- glm(All.NBA ~ Points + Rebounds + Assists + Usage + ShootingPercentage + Steals + Blocks + Turnovers + Fouls + FTs, family = binomial, nba_pergame_stats)
summary(multi.log.mam)
OR.table <- round(exp(cbind(Odds_Ratio = coef(multi.log.mam), confint(multi.log.mam))), 3)
formattable(OR.table)

##
#Overall, as you can see, the minimal adequate logistic model is able to reduce deviance from 2493 points in the null model to 771 in our model, with the loss of just 11 degrees of freedom. This suggests the model is a reasonably good fit for the data, and should give us some optimism about our ability to build a reasonably accurate predictive model with the variables we have.
#The odds ratios show us the relative effect of each factor. Odds ratios are a little different than standard regression coefficients - they describe the change in odds of a given outcome as we move along the x axis. For example, in the above, the odds of being selected to the All NBA team are 3.5 times greater (with a confidence interval of 2.5 - 5.1) for each additional block per game a player records. Odds ratios centre around 1; values greater than 1 indicate a positive relationship, values lower than 1 indicate a negative relationship.


#### Using Random Forest Alogirthm to predict selection to All NBA Teams

## Splitting the data into training and testing frames

nba.train <- nba_pergame_stats %>% filter(year<2012)
nba.test <- nba_pergame_stats %>% filter(year>2011)
dim(nba.train)
dim(nba.test)


## Using random forest alogorithm on 1999-2011

set.seed(123)  ## set a random seed to ensure replicability. 
RFmodel <- randomForest(All.NBA ~ Points + Assists + Rebounds + age + Games + Starts + Minutes + Steals + Blocks + Turnovers + Fouls + FTs + Threes + FGs + Usage + EfficiencyRating + BoxPlusMinus + ShootingPercentage, data = nba.train)
plot(RFmodel) ## check errors
varImpPlot(RFmodel) ## to look at variable importance

#plot of RFmodel tells us the classification errors (Out of bag errors) at different number of decision treeds in our random forest.
# the black line represents the overall OOB error rate, the red line represents the class 0; those not selected to all NBA, and the green represents class 1, those selected for all NBA.
## as we look at the results, the overall OOB falls until it stablises at around 300 trees. This is low, but accuracy is infalted by just how few players make all NBA.

#second RF model shows variable importanace, and tshowing the weight that each variable fed to the random forest algorithm, where it is calcualted by Gini importance. (Not that important , since we are interested in the predictor variable and not the relative effect of each X value (this was the logistic regression model))
## 

## using the model to predict on the test data set, where we see how the model predicts correctly (true positives), true negatives (those who did not make it), false positives (where algorithm predicts selection, but the player did not make it), false negatives (algorithm rpedicts non-selected, but the player was selected)

RFpredictions.binary <- predict(RFmodel, nba.test, type = "response")
nba_test_data_check <- data.frame(cbind(nba.test, RFpredictions.binary))
nba_test_data_check$TruePositive <- ifelse(nba_test_data_check$All.NBA == 1 & nba_test_data_check$RFpredictions.binary == 1, 1, 0)
nba_test_data_check$TrueNegative <- ifelse(nba_test_data_check$All.NBA == 0 & nba_test_data_check$RFpredictions.binary == 0, 1, 0)
nba_test_data_check$FalseNegative <- ifelse(nba_test_data_check$All.NBA == 1 & nba_test_data_check$RFpredictions.binary == 0, 1, 0)
nba_test_data_check$FalsePositive <- ifelse(nba_test_data_check$All.NBA == 0 & nba_test_data_check$RFpredictions.binary == 1, 1, 0)
prediction.results <- c(sum(nba_test_data_check$TruePositive), sum(nba_test_data_check$TrueNegative), sum(nba_test_data_check$FalsePositive), sum(nba_test_data_check$FalseNegative))
predictions <- data.frame(cbind(c("True Positive", "True Negative", "False Positive", "False Negative"), prediction.results))
predictions$prediction.results <- as.numeric(as.character(predictions$prediction.results))
names(predictions) <- c("Type", "Count")
formattable(predictions)

## Model can only predict 54 of the 90 All NBA selections.


## Check to see if algorithim can address season-sensitive
#Let's see how well the top 15 players in PER each season match up against that season's All NBA teams. Again, to do this, I'll use the top_n function to create a new data frame and then ask R to compute the percentage of the top 15 players in each season by PER who were on the All NBA teams.
#Ask the random forest model to find fifteen players with highest probabilities in each seasons.

prob.predict.RF <- predict(RFmodel, nba.test, type = "prob")
nba.test.prob <- cbind(nba.test, prob.predict.RF)
names(nba.test.prob)[names(nba.test.prob) == "1"] <- "Probability"
nba.top15 <- nba.test.prob %>% group_by(year) %>% top_n(n = 15, wt = Probability) %>% arrange(year, desc(Probability))
nba.top15$All.NBA <- as.numeric(as.character(nba.top15$All.NBA))
round((sum(nba.top15$All.NBA)/length(nba.top15$All.NBA)*100), 4)

## a difference of 14%, much larger than 60%.

## 6.5. How does our ML algorithm match up against an advanced player efficiency metric in predicting the All NBA teams?

#PER (Player Efficiency Rating), discussed above, is a summary statistic that assesses a player's overall output.

#Instead let ML algorithm match up against an advanced player efficiency metric in predicting all nba teams.
# compute top 15 in PER rating.
nba.PER.elite <- nba.test %>% group_by(year) %>% top_n(n = 15, wt = EfficiencyRating) %>% arrange(year, desc(EfficiencyRating))
nba.PER.elite$All.NBA <- as.numeric(as.character(nba.PER.elite$All.NBA))
round((sum(nba.PER.elite$All.NBA)/length(nba.PER.elite$All.NBA)*100), 4)


##52.2 % which our original model predicts better than the PER for All NBA.


## Where was the algorithm confident but wrong?

which(nba.top15$All.NBA == 0 & nba.top15$Probability > 0.75)

check <- formattable(nba.top15[c(34,38,65,79,82),])


## How to boost accuracy of the model?

##7.2. How could we boost the accuracy of the model?

#In terms of improving the performance of the model, two things stick out:

# Given the model's tendency to overrate players who are considered defensive liabilities, adding an advanced defensive metric might help.
#Often in All NBA discussions, team wins are cited as a factor that journalists considered when they vote. They don't like to reward players on losing teams. Including team winning percentage as a variable could help at the margins.
