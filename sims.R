##### Big 5 Season Simulations #####
## By: Stephan Teodosescu

##### Load libraries #####
library(tidyverse)

##### Building the model #####
# Inspiration: http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html


# Load Premier League Game Data from football-data.com
df <- read.csv("https://www.football-data.co.uk/mmz4281/2425/E0.csv", 
               stringsAsFactors = FALSE)

sMatch <- paste(df$HomeTeam, df$AwayTeam, sep = " - ")
sTeams <- unique(c(df$HomeTeam, df$AwayTeam)) %>% 
    sort

# Create tables of matches played for home and away teams, and an aggregated table with all matches
tmp1 <- df %>% 
    group_by(HomeTeam) %>%
    summarise(P = length(FTR),
              Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTHG),
              GC = sum(FTAG)) %>%
    ungroup()

tmp2 <- df %>% 
    group_by(AwayTeam) %>%
    summarise(P = length(FTR),
              Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTAG),
              GC = sum(FTHG)) %>%
    ungroup()

# Aggregate all matches together in one data frame
df.team.stats <- data.frame(Team = sTeams,
                            Points = tmp1$Pts + tmp2$Pts,
                            GD = (tmp1$GS + tmp2$GS) - (tmp1$GC + tmp2$GC),
                            TGS = (tmp1$GS + tmp2$GS)/(tmp1$P + tmp2$P),
                            TGC = (tmp1$GC + tmp2$GC)/(tmp1$P + tmp2$P), stringsAsFactors = FALSE)

# Create all possible combinations between the league teams, removing 
# the cases where a team plays itself, and looking for combinations 
# which have not already been played. 
df.new <- expand.grid(HomeTeam = sTeams, AwayTeam = sTeams, stringsAsFactors = FALSE) %>%
    filter(HomeTeam != AwayTeam) %>%
    mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
    filter(!(Match %in% sMatch)) %>%
    select(-Match) %>%
    mutate(HG = mean(df$FTHG),
           AG = mean(df$FTAG),
           TG = (mean(df$FTHG) + mean(df$FTAG))/2) %>%
    right_join(subset(df.team.stats, select = -c(Points, GD)),  by = c("HomeTeam" = "Team")) %>%
    right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("AwayTeam" = "Team")) %>%
    setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG", 
               "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG, #Expected goals for home teams based on League average rates so far
           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>% #Expected goals for away teams based on League average rates so far
    ungroup()

##### Simulate the remainder of the season 10,000x #####
iSim <- 10000
n <- length(sTeams)

# Initialize the results table
df.all <- data.frame(Team = rep(sTeams, iSim),
                     SimNo = rep(1:iSim, each = n),
                     Pts = rep(NA, n * iSim),
                     GD = rep(NA, n * iSim),
                     Rank = rep(NA, n * iSim))

# Loop through which each season will be simulated (using Poisson distribution, 
# since scoring in soccer is known to follow this distribution)
set.seed(1234)
for (i in 1:iSim){
    
    tmp <- df.new %>% 
        mutate(x1 = rpois(nrow(df.new), lambda = df.new$ExpHG), 
               x2 = rpois(nrow(df.new), lambda = df.new$ExpAG), 
               HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
               APts = 3 * (x1 < x2) + 1 * (x1 == x2))
    
    res <- df.team.stats %>% select(Points, GD) + 
        tmp %>% 
        group_by(HomeTeam) %>% 
        summarise(Pts = sum(HPts), GD = sum(x1) - sum(x2)) %>% select(Pts, GD) + 
        tmp %>% 
        group_by(AwayTeam) %>% summarise(Pts = sum(APts), GD = sum(x2) - sum(x1)) %>% select(Pts, GD) 
    
    df.all[(n*(i-1) + 1):(n*i), c("Pts", "GD")] <- res
    
    res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / max((res$GD - min(res$GD) + 1) + 1) #calculate the ranks based on points and goal difference
    df.all[(n*(i-1) + 1):(n*i), c("Rank")] <- rank(-res$PGD, ties.method = "random")  
    
}


##### Write to csv file in the working directory to be used in the app script #####
write_csv(df.all, "simulations.csv")
