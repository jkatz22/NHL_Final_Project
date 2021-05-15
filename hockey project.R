## Packages
library(tidyverse) 
library(ggrepel)
library(ggimage)
library(stringr)
library(MASS)
library(leaps)
library(car)
library(mosaic)

## Data for each year is two tables copy/pasted from hockey-reference bc I don't know how to data scrape and am too stressed out to learn how to. it's only 5 years of data thank god
data_2021_1 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2021_1.csv")
data_2021_2 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2021_2.csv")
data_2021 <- merge(data_2021_1,data_2021_2, by="Team")
data_2021 <- data_2021 %>%
  mutate(Team = case_when(
    Team == "Montreal Canadiens" ~ "Montreal Canadiens*",
    Team == "Nashville Predators" ~ "Nashville Predators*",
    Team == "St. Louis Blues" ~ "St. Louis Blues*",
    TRUE ~ Team
  )) ## adding asterisk to teams that have clinched playoffs but weren't marked as such when I got the data. last part keeps the ones not being changed from becoming NAs

data_2020_1 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2020_1.csv")
data_2020_2 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2020_2.csv")
data_2020 <- merge(data_2020_1,data_2020_2, by="Team")

data_2019_1 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2019_1.csv")
data_2019_2 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2019_2.csv")
data_2019 <- merge(data_2019_1,data_2019_2, by="Team")

data_2018_1 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2018_1.csv")
data_2018_2 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2018_2.csv")
data_2018 <- merge(data_2018_1,data_2018_2, by="Team")
data_2018 <- data_2018[-c(14),] ## remove team "League Average" bc I accidentally copied that over

data_2017_1 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2017_1.csv")
data_2017_2 <- read.csv("/Users/joshkatz/Desktop/Junior Year/Sports Analytics/Hockey Project/hockey data/data_2017_2.csv")
data_2017 <- merge(data_2017_1,data_2017_2, by="Team")


## cut the data down to just the stuff I want (really what I think is interesting). Note everything that isn't PP% or PK% is 5v5 only
## quick rundown: Team, points%, power play%, penalty kill%, Corsi For% (unblocked shots), Fenwick For% (Corsi + blocked shots, so true shot attempts), xGF, xGA (expected goals for and against, later converted into a percentage), Scoring Chances For%, High Danger Chances For%, High Danger Chance Conversion%
## basically took special teams numbers and then a bunch of 5v5 play-driving metrics
myvars <- c("Team","PTS.","PP.","PK.","CF.","FF.","xGF","xGA","SCF.","HDF.","HDC.")
data_2021 <- data_2021[myvars]
data_2020 <- data_2020[myvars]
data_2019 <- data_2019[myvars]
data_2018 <- data_2018[myvars]
data_2017 <- data_2017[myvars]

data <- rbind(data_2021,data_2020,data_2019,data_2018,data_2017)
data <- data %>%
  mutate(xG. = xGF/(xGF+xGA)) #convert expected goals for and against into a percentage

#########
## here is where I will do some analysis to find out what stat is good. maybe i get multiple stats and I develop some metric to use each of those
## decided I would build a multiple linear regression model to predict points%
## regsubsets gets fed all of my possible predictors and returns the 4 best models for each # of predictors in the model (so 4 models w/ 1 predictor, 4 w/2, etc.)
########
leaps <- regsubsets(PTS.~PP.+PK.+CF.+FF.+xG.+SCF.+HDF.+HDC., data = data,nbest = 4)
## plots help visualize which model would be the best to use
plot(leaps,scale = "r2")
subsets(leaps, statistic="rsq")
## it looks like the best model is going to include PP%, PK%, FF%, HDC%. Not entirely surprising. PP% and PK% measure special teams, FF% and HDC% are 5v5 metrics that (kinda) measure ability to drive play and convert opportunities into goals.
model1 <- lm(PTS.~PP.+PK.+FF.+HDC., data = data)
summary(model1)
plot(model1, 1:2) ## check conditions, its close so im gonna try another model that swaps CF% for FF%
car::vif(model1) ## all of these are close to 1, indicating no multicollinearity

model2 <- lm(PTS.~PP.+PK.+CF.+HDC., data = data)
summary(model2)
plot(model2, 1:2)
car::vif(model2)
## both models are similar in terms of conditions being met, first has > adj R sq so I'll use that one

# apply the team score metric (could've called this something like "true points%") and do some basic analysis of it (below and in the hockey plots file), make sure it makes sense. filtered teams into separate data frames for playoffs and non playoffs
data_2021 <- data_2021 %>%
  mutate(team_score = 0.008922*PP.+0.009288*PK.+0.01577*FF.+0.014329*HDC. - 1.339258) %>%
  mutate(in_playoffs = ifelse(PTS. >= .538, 1, 0))
playoff_teams2021 <- data_2021 %>%
  filter(in_playoffs == 1)
missed_playoffs2021 <- data_2021 %>%
  filter(in_playoffs == 0)

data_2020 <- data_2020 %>%
  mutate(team_score = 0.008922*PP.+0.009288*PK.+0.01577*FF.+0.014329*HDC. - 1.339258) %>%
  mutate(in_playoffs = ifelse(PTS. >= .500, 1, 0))
playoff_teams2020 <- data_2020 %>%
  filter(in_playoffs == 1)
missed_playoffs2020 <- data_2020 %>%
  filter(in_playoffs == 0)

data_2019 <- data_2019 %>%
  mutate(team_score = 0.008922*PP.+0.009288*PK.+0.01577*FF.+0.014329*HDC. - 1.339258) %>%
  mutate(in_playoffs = ifelse(PTS. >= .530 & PTS. != .585, 1, 0)) 
playoff_teams2019 <- data_2019 %>%
  filter(in_playoffs == 1)
missed_playoffs2019 <- data_2019 %>%
  filter(in_playoffs == 0)

data_2018 <- data_2018 %>%
  mutate(team_score = 0.008922*PP.+0.009288*PK.+0.01577*FF.+0.014329*HDC. - 1.339258) %>%
  mutate(in_playoffs = ifelse(PTS. >= .579 & PTS. != .585, 1, 0))
playoff_teams2018 <- data_2018 %>%
  filter(in_playoffs == 1)
missed_playoffs2018 <- data_2018 %>%
  filter(in_playoffs == 0)

data_2017 <- data_2017 %>%
  mutate(team_score = 0.008922*PP.+0.009288*PK.+0.01577*FF.+0.014329*HDC. - 1.339258) %>%
  mutate(in_playoffs = ifelse(PTS. >= .573 & PP. > 15 & xGF != 151.9, 1, 0))
playoff_teams2017 <- data_2017 %>%
  filter(in_playoffs == 1)
missed_playoffs2017 <- data_2017 %>%
  filter(in_playoffs == 0)

favstats(missed_playoffs2021$team_score)
favstats(playoff_teams2021$team_score)

favstats(missed_playoffs2020$team_score) ## Expanded playoffs due to pandemic, so this might get weird
favstats(playoff_teams2020$team_score)

favstats(missed_playoffs2019$team_score)
favstats(playoff_teams2019$team_score)

favstats(missed_playoffs2018$team_score)
favstats(playoff_teams2018$team_score)

favstats(missed_playoffs2017$team_score)
favstats(playoff_teams2017$team_score)

## mean for playoff teams is always ~0.60, except for 2020 bc more playoff teams so mean was 0.56. median similar story
## but for non-playoff teams, mean was ~0.52 until pandemic hit, then dropped significantly. median fluctuates a lot

#just out of curiosity, what's the correlation btwn team_score and this years Points%
cor(data_2021$PTS.,data_2021$team_score) #86% is pretty cool imo

## make a dataframe for every playoff team bc simulation should be easier like this
CAR <- data_2021 %>%
  filter(Team == "Carolina Hurricanes*")

TB <- data_2021 %>%
  filter(Team == "Tampa Bay Lightning*") 

FLO <- data_2021 %>%
  filter(Team == "Florida Panthers*") 

NSH <- data_2021 %>%
  filter(Team == "Nashville Predators*")

PIT <- data_2021 %>%
  filter(Team == "Pittsburgh Penguins*") 

WSH <- data_2021 %>%
  filter(Team == "Washington Capitals*") 

BOS <- data_2021 %>%
  filter(Team == "Boston Bruins*") 

NYI <- data_2021 %>%
  filter(Team == "New York Islanders*")

TOR <- data_2021 %>%
  filter(Team == "Toronto Maple Leafs*") 

EDM <- data_2021 %>%
  filter(Team == "Edmonton Oilers*")

WPG <- data_2021 %>%
  filter(Team == "Winnipeg Jets*") 

MTL <- data_2021 %>%
  filter(Team == "Montreal Canadiens*") 

VGK <- data_2021 %>%
  filter(Team == "Vegas Golden Knights*") 

COL <- data_2021 %>%
  filter(Team == "Colorado Avalanche*") 

MIN <- data_2021 %>%
  filter(Team == "Minnesota Wild*") 

STL <- data_2021 %>%
  filter(Team == "St. Louis Blues*") 

#and a dataframe to count # of cup wins. could have expanded this to track # of finals/semifinals appearances. would probably do so if i improved this in the future (i.e. make a better model, ideally with time series rather than treating every year the same, as I did to make the mlr model. could also maybe try non-linear regressions)
counting_cups <- data.frame(Team = c("Vegas Golden Knights*", "Tampa Bay Lightning*","St. Louis Blues*", "Minnesota Wild*","Colorado Avalanche*", "Montreal Canadiens*","Winnipeg Jets*", "Edmonton Oilers*","Toronto Maple Leafs*", "New York Islanders*","Boston Bruins*", "Washington Capitals*","Pittsburgh Penguins*", "Nashville Predators*","Florida Panthers*", "Carolina Hurricanes*"), wins = 0)

