## My goal here was to make sure that the team score metric wasn't totally useless at predicting points%. Basically plotted team_score against points% and looked for anything that looked weird, then tried to rationalize it in my head by looking more in depth at those teams. Also highlighted every cup winner

## 2021
ggplot(data_2021, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Stanley Cup Favorites 2021") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. >.6 & team_score > .6,as.character(Team),'')),box.padding = 0.1,size = 2.5) + theme(legend.position = "none")

ggplot(data_2021, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Standings vs Team Score, 2021") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. <.7 & team_score > .49 & PTS. > .45 & team_score < .602,as.character(Team),'')),box.padding = 0.25,point.padding = .2,size = 2) + theme(legend.position = "none")

#2020
ggplot(data_2020, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Standings vs Team Score, 2020") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. <.55 & team_score > .45 | team_score < .525&PTS.>.5 | Team == "Tampa Bay Lightning*",as.character(Team),'')),box.padding = 0.1,size = 2)+ theme(legend.position = "none")

#2019
ggplot(data_2019, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Standings vs Team Score, 2019") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. <.595 & PTS. >.545 | team_score < .525 & PTS. > .6 | team_score > 0.55 & PTS. < 0.6 | Team == "St. Louis Blues*",as.character(Team),'')),box.padding = 0.1,size = 2.5)+ theme(legend.position = "none")

#2018
ggplot(data_2018, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Standings vs Team Score, 2018") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. <.6 & PTS. >.5 & team_score <.6 | Team == "Washington Capitals*",as.character(Team),'')),box.padding = 0.1,size = 2.5)+ theme(legend.position = "none")

#2017
ggplot(data_2017, aes(x=team_score, y=PTS., color = in_playoffs, label = Team)) + ylab("Points%") + xlab("Team Score metric") + ggtitle("Standings vs Team Score, 2017") + geom_point() + geom_label_repel(aes(label = ifelse(PTS. <.6 & team_score >.495 & team_score < 0.635| Team == "Washington Capitals*" | Team == "Pittsburgh Penguins*",as.character(Team),'')),box.padding = 0.1,size = 2)+ theme(legend.position = "none")

## Interesting teams are:
##2016-17 - LA Kings (low PDO) and TB Lightning (missed playoffs by a point). Also only season a Barry Trotz team graded well by team_score (and also the last year before his first cup, coincidence?)
##2017-18 - Carolina Hurricanes (low PDO, specifically horrible goaltending)
##2018-19 - Florida Panthers (low PDO, specifically horrible goaltending. signed Bob to massive contract after this season, I wonder why?)
##2019-20 - none really bc expanded playoffs. SJ Sharks a tad interesting, they too had bad goaltending
##2020-21 - Dallas Stars (SCF last year, schedule destroyed by COVID and Texas winter this year) and LA Kings (honestly don't know why)



