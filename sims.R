##### Big 5 Season Simulations #####
## By: Stephan Teodosescu

##### Load libraries #####
library(tidyverse)

##### Building the model #####
# Inspiration: http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html


run_league_sims <- function(url, competition_name, iSim = 10000, seed = 1234) {
    
    df <- read.csv(url, stringsAsFactors = FALSE)
    
    # Safety: only treat played matches as "played" + use played matches for means
    df_played <- df %>% filter(!is.na(FTHG), !is.na(FTAG))
    
    sMatch <- paste(df_played$HomeTeam, df_played$AwayTeam, sep = " - ")
    sTeams <- sort(unique(c(df$HomeTeam, df$AwayTeam)))
    n <- length(sTeams)
    
    # Home / away stats
    tmp1 <- df_played %>%
        group_by(HomeTeam) %>%
        summarise(
            P = length(FTR),
            Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTHG),
            GC = sum(FTAG),
            .groups = "drop"
        )
    
    tmp2 <- df_played %>%
        group_by(AwayTeam) %>%
        summarise(
            P = length(FTR),
            Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTAG),
            GC = sum(FTHG),
            .groups = "drop"
        )
    
    # Aggregate team stats
    df.team.stats <- data.frame(
        Team = sTeams,
        Points = tmp1$Pts + tmp2$Pts,
        GD = (tmp1$GS + tmp2$GS) - (tmp1$GC + tmp2$GC),
        TGS = (tmp1$GS + tmp2$GS) / (tmp1$P + tmp2$P),
        TGC = (tmp1$GC + tmp2$GC) / (tmp1$P + tmp2$P),
        stringsAsFactors = FALSE
    )
    
    # Remaining fixtures via schedule reconstruction (assumes each team plays home and away)
    df.new <- expand.grid(HomeTeam = sTeams, AwayTeam = sTeams, stringsAsFactors = FALSE) %>%
        filter(HomeTeam != AwayTeam) %>%
        mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
        filter(!(Match %in% sMatch)) %>%
        select(-Match) %>%
        mutate(
            HG = mean(df_played$FTHG),
            AG = mean(df_played$FTAG),
            TG = (mean(df_played$FTHG) + mean(df_played$FTAG)) / 2
        ) %>%
        right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("HomeTeam" = "Team")) %>%
        right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("AwayTeam" = "Team")) %>%
        setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG",
                   "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
        mutate(
            ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
            ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG
        ) %>%
        ungroup()
    
    # Initialize results (adds Competition_Name)
    df.all <- data.frame(
        Team = rep(sTeams, iSim),
        SimNo = rep(1:iSim, each = n),
        Pts = rep(NA_real_, n * iSim),
        GD = rep(NA_real_, n * iSim),
        Rank = rep(NA_integer_, n * iSim),
        Competition_Name = competition_name,
        stringsAsFactors = FALSE
    )
    
    # Sim loop
    set.seed(seed)
    for (i in 1:iSim) {
        
        tmp <- df.new %>%
            mutate(
                x1 = rpois(nrow(df.new), lambda = df.new$ExpHG),
                x2 = rpois(nrow(df.new), lambda = df.new$ExpAG),
                HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
                APts = 3 * (x1 < x2) + 1 * (x1 == x2)
            )
        
        # IMPORTANT: logic aligns by team order safely
        add_home <- tmp %>%
            group_by(HomeTeam) %>%
            summarise(Pts = sum(HPts), GD = sum(x1) - sum(x2), .groups = "drop") %>%
            rename(Team = HomeTeam)
        
        add_away <- tmp %>%
            group_by(AwayTeam) %>%
            summarise(Pts = sum(APts), GD = sum(x2) - sum(x1), .groups = "drop") %>%
            rename(Team = AwayTeam)
        
        res <- df.team.stats %>%
            select(Team, Points, GD) %>%
            left_join(add_home, by = "Team", suffix = c("", ".addH")) %>%
            mutate(
                Points = Points + replace_na(Pts, 0),
                GD = GD + replace_na(GD.addH, 0)
            ) %>%
            select(Team, Points, GD) %>%
            left_join(add_away, by = "Team", suffix = c("", ".addA")) %>%
            mutate(
                Points = Points + replace_na(Pts, 0),
                GD = GD + replace_na(GD.addA, 0)
            ) %>%
            select(Team, Points, GD)
        
        # Rank (adds random tie-break)
        res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / (max(res$GD - min(res$GD) + 1) + 1)
        res$Rank <- rank(-res$PGD, ties.method = "random")
        
        # write sim i results back in the same team order
        res <- res %>% arrange(match(Team, sTeams))
        
        df.all[(n * (i - 1) + 1):(n * i), c("Pts", "GD", "Rank")] <- res[, c("Points", "GD", "Rank")]
    }
    
    df.all
}

# ---- Big 5 league config ----
leagues <- tibble::tribble(
    ~competition_name,       ~url,
    "Premier League",        "https://www.football-data.co.uk/mmz4281/2526/E0.csv",
    "La Liga",               "https://www.football-data.co.uk/mmz4281/2526/SP1.csv",
    "Fußball-Bundesliga",    "https://www.football-data.co.uk/mmz4281/2526/D1.csv",
    "Ligue 1",               "https://www.football-data.co.uk/mmz4281/2526/F1.csv",
    "Serie A",               "https://www.football-data.co.uk/mmz4281/2526/I1.csv"
)

# ---- Output 1. Run all leagues & write one unified file ----
iSim <- 10000
seed <- 1234

simulations_all <- purrr::map2_dfr(
    leagues$url,
    leagues$competition_name,
    ~ run_league_sims(url = .x, competition_name = .y, iSim = iSim, seed = seed)
)

write_csv(simulations_all, "simulations.csv")


# --- Output 2. Summary metrics for dashboard ---
sim_summary <- simulations_all %>%
    group_by(Competition_Name, Team) %>%
    summarise(
        exp_pts = mean(Pts, na.rm = TRUE),
        exp_gd  = mean(GD, na.rm = TRUE),
        
        p_title = mean(Rank == 1, na.rm = TRUE),
        p_ucl   = mean(Rank <= 4, na.rm = TRUE),
        p_uel   = mean(Rank %in% 5:6, na.rm = TRUE),     # optional heuristic
        p_releg = mean(Rank >= (max(Rank, na.rm = TRUE) - 2), na.rm = TRUE), # bottom 3
        
        .groups = "drop"
    ) %>%
    mutate(
        # round to two decimals
        exp_pts = round(exp_pts, 2),
        exp_gd  = round(exp_gd, 2)
    )

write_csv(sim_summary, "simulations_summary.csv")

# --- Output 3. Team-by-rank distribution (for heatmap) ---
sim_rank_dist <- simulations_all %>%
    group_by(Competition_Name, Team, Rank) %>%
    summarise(prob = n() / iSim, .groups = "drop")

write_csv(sim_rank_dist, "simulations_rank_dist.csv")