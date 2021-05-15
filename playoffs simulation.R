#same data frame from hockey project.R
counting_cups <- data.frame(Team = c("Vegas Golden Knights*", "Tampa Bay Lightning*","St. Louis Blues*", "Minnesota Wild*","Colorado Avalanche*", "Montreal Canadiens*","Winnipeg Jets*", "Edmonton Oilers*","Toronto Maple Leafs*", "New York Islanders*","Boston Bruins*", "Washington Capitals*","Pittsburgh Penguins*", "Nashville Predators*","Florida Panthers*", "Carolina Hurricanes*"), wins = 0)
#This is the code that simulates the playoffs based on the work I did in the hockey project file, change line 4 to run the sim x times. run the line above this if you want to clear the counting_cups data. change the matchups for first round as desired (matchups decided according to NHL standings on 5/11/21, which are guaranteed to be the actual matchups except COL and VGK might swap places. If they do, I would not expect odds for either team to win cup to change much, since STL and MIN are near identical in team_score metric, despite difference in Points%)
for (i in 1:100000) {
  ## Central Division
  hteam1 <- CAR
  vteam1 <- NSH
  odds1 <- hteam1$team_score/(hteam1$team_score+vteam1$team_score)
  hwins1 <- 0
  vwins1 <- 0
  while (hwins1 < 4 & vwins1 < 4) {
    game1 <- runif(1,0,1)
    if (game1 <= odds1) {
      hwins1 <- hwins1 + 1
    } else {
      vwins1 <- vwins1+1
    }
  }
  if (hwins1 == 4) {
    winner1 <- hteam1
  } else {
    winner1 <- vteam1
  }
  
  hteam2 <- FLO
  vteam2 <- TB
  odds2 <- hteam2$team_score/(hteam2$team_score+vteam2$team_score)
  hwins2 <- 0
  vwins2 <- 0
  while (hwins2 < 4 & vwins2 < 4) {
    game2 <- runif(1,0,1)
    if (game2 <= odds2) {
      hwins2 <- hwins2 + 1
    } else {
      vwins2 <- vwins2+1
    }
  }
  if (hwins2 == 4) {
    winner2 <- hteam2
  } else {
    winner2 <- vteam2
  }
  
  ## Second Round
  if (winner1$PTS. >= winner2$PTS.) {
    hteam9 <- winner1
    vteam9 <- winner2
  } else {
    hteam9 <- winner2
    vteam9 <- winner1
  }
  odds9 <- hteam9$team_score/(hteam9$team_score+vteam9$team_score)
  hwins9 <- 0
  vwins9 <- 0
  while (hwins9 < 4 & vwins9 < 4) {
    game9 <- runif(1,0,1)
    if (game9 <= odds9) {
      hwins9 <- hwins9 + 1
    } else {
      vwins9 <- vwins9+1
    }
  }
  if (hwins9 == 4) {
    winner9 <- hteam9
  } else {
    winner9 <- vteam9
  }
  
  ## East Division
  hteam3 <- PIT
  vteam3 <- NYI
  odds3 <- hteam3$team_score/(hteam3$team_score+vteam3$team_score)
  hwins3 <- 0
  vwins3 <- 0
  while (hwins3 < 4 & vwins3 < 4) {
    game3 <- runif(1,0,1)
    if (game3 <= odds3) {
      hwins3 <- hwins3 + 1
    } else {
      vwins3 <- vwins3+1
    }
  }
  if (hwins3 == 4) {
    winner3 <- hteam3
  } else {
    winner3 <- vteam3
  }
  
  hteam4 <- WSH
  vteam4 <- BOS
  odds4 <- hteam4$team_score/(hteam4$team_score+vteam4$team_score)
  hwins4 <- 0
  vwins4 <- 0
  while (hwins4 < 4 & vwins4 < 4) {
    game4 <- runif(1,0,1)
    if (game4 <= odds4) {
      hwins4 <- hwins4 + 1
    } else {
      vwins4 <- vwins4+1
    }
  }
  if (hwins4 == 4) {
    winner4 <- hteam4
  } else {
    winner4 <- vteam4
  }
  
  ## Second Round
  if (winner3$PTS. >= winner4$PTS.) {
    hteam10 <- winner3
    vteam10 <- winner4
  } else {
    hteam10 <- winner4
    vteam10 <- winner3
  }
  odds10 <- hteam10$team_score/(hteam10$team_score+vteam10$team_score)
  hwins10 <- 0
  vwins10 <- 0
  while (hwins10 < 4 & vwins10 < 4) {
    game10 <- runif(1,0,1)
    if (game10 <= odds10) {
      hwins10 <- hwins10 + 1
    } else {
      vwins10 <- vwins10+1
    }
  }
  if (hwins10 == 4) {
    winner10 <- hteam10
  } else {
    winner10 <- vteam10
  }
  
  ## North Division
  hteam5 <- TOR
  vteam5 <- MTL
  odds5 <- hteam5$team_score/(hteam5$team_score+vteam5$team_score)
  hwins5 <- 0
  vwins5 <- 0
  while (hwins5 < 4 & vwins5 < 4) {
    game5 <- runif(1,0,1)
    if (game5 <= odds5) {
      hwins5 <- hwins5 + 1
    } else {
      vwins5 <- vwins5+1
    }
  }
  if (hwins5 == 4) {
    winner5 <- hteam5
  } else {
    winner5 <- vteam5
  }
  
  hteam6 <- EDM
  vteam6 <- WPG
  odds6 <- hteam6$team_score/(hteam6$team_score+vteam6$team_score)
  hwins6 <- 0
  vwins6 <- 0
  while (hwins6 < 4 & vwins6 < 4) {
    game6 <- runif(1,0,1)
    if (game6 <= odds6) {
      hwins6 <- hwins6 + 1
    } else {
      vwins6 <- vwins6+1
    }
  }
  if (hwins6 == 4) {
    winner6 <- hteam6
  } else {
    winner6 <- vteam6
  }
  
  ## Second Round
  if (winner5$PTS. >= winner6$PTS.) {
    hteam11 <- winner5
    vteam11 <- winner6
  } else {
    hteam11 <- winner6
    vteam11 <- winner5
  }
  odds11 <- hteam11$team_score/(hteam11$team_score+vteam11$team_score)
  hwins11 <- 0
  vwins11 <- 0
  while (hwins11 < 4 & vwins11 < 4) {
    game11 <- runif(1,0,1)
    if (game11 <= odds11) {
      hwins11 <- hwins11 + 1
    } else {
      vwins11 <- vwins11+1
    }
  }
  if (hwins11 == 4) {
    winner11 <- hteam11
  } else {
    winner11 <- vteam11
  }
  
  ## West Division
  hteam7 <- VGK
  vteam7 <- STL
  odds7 <- hteam7$team_score/(hteam7$team_score+vteam7$team_score)
  hwins7 <- 0
  vwins7 <- 0
  while (hwins7 < 4 & vwins7 < 4) {
    game7 <- runif(1,0,1)
    if (game7 <= odds7) {
      hwins7 <- hwins7 + 1
    } else {
      vwins7 <- vwins7+1
    }
  }
  if (hwins7 == 4) {
    winner7 <- hteam7
  } else {
    winner7 <- vteam7
  }
  
  hteam8 <- COL
  vteam8 <- MIN
  odds8 <- hteam8$team_score/(hteam8$team_score+vteam8$team_score)
  hwins8 <- 0
  vwins8 <- 0
  while (hwins8 < 4 & vwins8 < 4) {
    game8 <- runif(1,0,1)
    if (game8 <= odds8) {
      hwins8 <- hwins8 + 1
    } else {
      vwins8 <- vwins8+1
    }
  }
  if (hwins8 == 4) {
    winner8 <- hteam8
  } else {
    winner8 <- vteam8
  }
  
  ## Second Round
  if (winner7$PTS. >= winner8$PTS.) {
    hteam12 <- winner7
    vteam12 <- winner8
  } else {
    hteam12 <- winner8
    vteam12 <- winner7
  }
  odds12 <- hteam12$team_score/(hteam12$team_score+vteam12$team_score)
  hwins12 <- 0
  vwins12 <- 0
  while (hwins12 < 4 & vwins12 < 4) {
    game12 <- runif(1,0,1)
    if (game12 <= odds12) {
      hwins12 <- hwins12 + 1
    } else {
      vwins12 <- vwins12+1
    }
  }
  if (hwins12 == 4) {
    winner12 <- hteam12
  } else {
    winner12 <- vteam12
  }
  
  ## Fuck let's figure out how to rank the semifinal teams. To whoever reads this, I am so so sorry that you had to read through all this. I hate it too but I can't think of any better way than just listing all 24 combinations. this is what I get for making the whole thing out of dataframes instead of numbers
  if (winner9$PTS. > winner10$PTS. & winner10$PTS. > winner11$PTS. & winner11$PTS. > winner12$PTS.) {
    seed1 <- winner9
    seed2 <- winner10
    seed3 <- winner11
    seed4 <- winner12 #1
  } else if (winner9$PTS. > winner10$PTS. & winner10$PTS. > winner12$PTS. & winner12$PTS. > winner11$PTS.) {
    seed1 <- winner9
    seed2 <- winner10
    seed3 <- winner12
    seed4 <- winner11 #2
  } else if (winner9$PTS. > winner12$PTS. & winner12$PTS. > winner10$PTS. & winner10$PTS. > winner11$PTS.) {
    seed1 <- winner9
    seed2 <- winner12
    seed3 <- winner10
    seed4 <- winner11 #3
  } else if (winner9$PTS. > winner11$PTS. & winner11$PTS. > winner10$PTS. & winner10$PTS. > winner12$PTS.) {
    seed1 <- winner9
    seed2 <- winner11
    seed3 <- winner10
    seed4 <- winner12 #4
  } else if (winner9$PTS. > winner11$PTS. & winner11$PTS. > winner12$PTS. & winner12$PTS. > winner10$PTS.) {
    seed1 <- winner9
    seed2 <- winner11
    seed3 <- winner12
    seed4 <- winner10 #5
  } else if (winner9$PTS. > winner12$PTS. & winner12$PTS. > winner11$PTS. & winner11$PTS. > winner10$PTS.) {
    seed1 <- winner9
    seed2 <- winner12
    seed3 <- winner11
    seed4 <- winner10 #6
  } else if (winner10$PTS. > winner9$PTS. & winner9$PTS. > winner11$PTS. & winner11$PTS. > winner12$PTS.) {
    seed1 <- winner10
    seed2 <- winner9
    seed3 <- winner11
    seed4 <- winner12 #7
  } else if (winner10$PTS. > winner9$PTS. & winner9$PTS. > winner12$PTS. & winner12$PTS. > winner11$PTS.) {
    seed1 <- winner10
    seed2 <- winner9
    seed3 <- winner12
    seed4 <- winner11 #8
  } else if (winner10$PTS. > winner12$PTS. & winner12$PTS. > winner9$PTS. & winner9$PTS. > winner11$PTS.) {
    seed1 <- winner10
    seed2 <- winner12
    seed3 <- winner9
    seed4 <- winner11 #9
  } else if (winner10$PTS. > winner11$PTS. & winner11$PTS. > winner9$PTS. & winner9$PTS. > winner12$PTS.) {
    seed1 <- winner10
    seed2 <- winner11
    seed3 <- winner9
    seed4 <- winner12 #10
  } else if (winner10$PTS. > winner11$PTS. & winner11$PTS. > winner12$PTS. & winner12$PTS. > winner9$PTS.) {
    seed1 <- winner10
    seed2 <- winner11
    seed3 <- winner12
    seed4 <- winner9 #11
  } else if (winner10$PTS. > winner12$PTS. & winner12$PTS. > winner11$PTS. & winner11$PTS. > winner9$PTS.) {
    seed1 <- winner10
    seed2 <- winner12
    seed3 <- winner11
    seed4 <- winner9 #12
  } else if (winner11$PTS. > winner10$PTS. & winner10$PTS. > winner9$PTS. & winner9$PTS. > winner12$PTS.) {
    seed1 <- winner11
    seed2 <- winner10
    seed3 <- winner9
    seed4 <- winner12 #13
  } else if (winner11$PTS. > winner10$PTS. & winner10$PTS. > winner12$PTS. & winner12$PTS. > winner9$PTS.) {
    seed1 <- winner11
    seed2 <- winner10
    seed3 <- winner12
    seed4 <- winner9 #14
  } else if (winner11$PTS. > winner12$PTS. & winner12$PTS. > winner10$PTS. & winner10$PTS. > winner9$PTS.) {
    seed1 <- winner11
    seed2 <- winner12
    seed3 <- winner10
    seed4 <- winner9 #15
  } else if (winner11$PTS. > winner9$PTS. & winner9$PTS. > winner11$PTS. & winner11$PTS. > winner12$PTS.) {
    seed1 <- winner11
    seed2 <- winner9
    seed3 <- winner10
    seed4 <- winner12 #16
  } else if (winner11$PTS. > winner9$PTS. & winner9$PTS. > winner12$PTS. & winner12$PTS. > winner10$PTS.) {
    seed1 <- winner11
    seed2 <- winner9
    seed3 <- winner12
    seed4 <- winner10 #17
  } else if (winner11$PTS. > winner12$PTS. & winner12$PTS. > winner9$PTS. & winner9$PTS. > winner10$PTS.) {
    seed1 <- winner11
    seed2 <- winner12
    seed3 <- winner9
    seed4 <- winner10 #18
  } else if (winner12$PTS. > winner10$PTS. & winner10$PTS. > winner11$PTS. & winner11$PTS. > winner9$PTS.) {
    seed1 <- winner12
    seed2 <- winner10
    seed3 <- winner11
    seed4 <- winner9 #19
  } else if (winner12$PTS. > winner10$PTS. & winner10$PTS. > winner9$PTS. & winner9$PTS. > winner11$PTS.) {
    seed1 <- winner12
    seed2 <- winner10
    seed3 <- winner9
    seed4 <- winner11 #20
  } else if (winner12$PTS. > winner9$PTS. & winner9$PTS. > winner10$PTS. & winner10$PTS. > winner11$PTS.) {
    seed1 <- winner12
    seed2 <- winner9
    seed3 <- winner10
    seed4 <- winner11 #21
  } else if (winner12$PTS. > winner11$PTS. & winner11$PTS. > winner10$PTS. & winner10$PTS. > winner9$PTS.) {
    seed1 <- winner12
    seed2 <- winner11
    seed3 <- winner10
    seed4 <- winner9 #22
  } else if (winner12$PTS. > winner11$PTS. & winner11$PTS. > winner9$PTS. & winner9$PTS. > winner10$PTS.) {
    seed1 <- winner12
    seed2 <- winner11
    seed3 <- winner9
    seed4 <- winner10 #23
  } else if (winner12$PTS. > winner9$PTS. & winner9$PTS. > winner11$PTS. & winner11$PTS. > winner10$PTS.) {
    seed1 <- winner12
    seed2 <- winner9
    seed3 <- winner11
    seed4 <- winner10 #24
  }
  
  
  ## Time for the semifinals and Stanley Cup Finals
  ## 1v4
  hteam13 <- seed1
  vteam13 <- seed4
  odds13 <- hteam13$team_score/(hteam13$team_score+vteam13$team_score)
  hwins13 <- 0
  vwins13 <- 0
  while (hwins13 < 4 & vwins13 < 4) {
    game13 <- runif(1,0,1)
    if (game13 <= odds13) {
      hwins13 <- hwins13 + 1
    } else {
      vwins13 <- vwins13+1
    }
  }
  if (hwins13 == 4) {
    winner13 <- hteam13
  } else {
    winner13 <- vteam13
  }
  
  #2v3
  hteam14 <- seed2
  vteam14 <- seed3
  odds14 <- hteam14$team_score/(hteam14$team_score+vteam14$team_score)
  hwins14 <- 0
  vwins14 <- 0
  while (hwins14 < 4 & vwins14 < 4) {
    game14 <- runif(1,0,1)
    if (game14 <= odds14) {
      hwins14 <- hwins14 + 1
    } else {
      vwins14 <- vwins14+1
    }
  }
  if (hwins14 == 4) {
    winner14 <- hteam14
  } else {
    winner14 <- vteam14
  }
  
  ## Drumroll please... The (virtual) Stanley Cup Finals!
  if (winner13$PTS. >= winner14$PTS.) {
    hteam15 <- winner13
    vteam15 <- winner14
  } else {
    hteam15 <- winner14
    vteam15 <- winner13
  }
  odds15 <- hteam15$team_score/(hteam15$team_score+vteam15$team_score)
  hwins15 <- 0
  vwins15 <- 0
  while (hwins15 < 4 & vwins15 < 4) {
    game15 <- runif(1,0,1)
    if (game15 <= odds15) {
      hwins15 <- hwins15 + 1
    } else {
      vwins15 <- vwins15+1
    }
  }
  if (hwins15 == 4) {
    winner15 <- hteam15
  } else {
    winner15 <- vteam15
  }
  
  ## Count the number of cup wins for every team
  for (i in 1:16) {
    if (winner15$Team == counting_cups[i,1]) {
      counting_cups[i,2] <- counting_cups[i,2]+1
    }
  }
}

counting_cups$win. <- round(counting_cups$wins/1000, digits = 1)
counting_cups
