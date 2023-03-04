# You can install using the pacman package using the following code

library(tidyverse)
library(baseballr)
ncaa_school_id_lu("Ole Miss") #433

om_schedule <- ncaa_schedule_info(teamid = 433, year = 2021)

#Collect Roster, Separate into B, P
om_roster <- ncaa_baseball_roster(team_id = 433,year= 2022)
om_batters <- om_roster %>% 
  filter(om_roster$position != 'P' & !is.na(om_roster$player_url))
om_pitchers <- om_roster %>% 
  filter(om_roster$position == 'P' & !is.na(om_roster$player_url))


om_sched <- ncaa_schedule_info(team_id = 433, year = 2023)

#Load game logs and string replace slashes. PITA.
om_batting <- data.frame()
for(i in om_batters$player_id){
  temp_player <-  ncaa_game_logs(player_id = i, year = 2022, type = "batting")
  col_names <- c('R','AB','H','2B','3B','TB','HR','RBI','BB','HBP','SF',
                 'SH','K','OPP DP','CS','Picked','SB','IBB','GDP','RBI2out')
  temp_player$player_id <- i
  temp_player$AB <- str_replace(temp_player$AB,'/','')
  temp_player$R <- str_replace(temp_player$R,'/','')
  temp_player$H <- str_replace(temp_player$H,'/','')
  temp_player$`2B` <- str_replace(temp_player$`2B`,'/','')
  temp_player$`3B` <- str_replace(temp_player$`3B`,'/','')
  temp_player$HR <- str_replace(temp_player$HR,'/','')
  temp_player$RBI <- str_replace(temp_player$RBI,'/','')
  temp_player$TB <- str_replace(temp_player$TB,'/','')
  temp_player$BB <- str_replace(temp_player$BB,'/','')
  temp_player$HBP <- str_replace(temp_player$HBP,'/','')
  temp_player$SF <- str_replace(temp_player$SF,'/','')
  temp_player$SH <- str_replace(temp_player$SH,'/','')
  temp_player$K <- str_replace(temp_player$K,'/','')
  temp_player$`OPP DP` <- str_replace(temp_player$`OPP DP`,'/','')
  temp_player$SB <- str_replace(temp_player$SB,'/','')
  temp_player$CS <- str_replace(temp_player$CS,'/','')
  temp_player$Picked <- str_replace(temp_player$Picked,'/','')
  temp_player$IBB <- str_replace(temp_player$IBB,'/','')
  temp_player$GDP <- str_replace(temp_player$GDP,'/','')
  temp_player$`RBI2out` <- str_replace(temp_player$`RBI2out`,'/','')
  temp_player[, 4:24] <- lapply(temp_player[, 4:24, drop = FALSE], function(x) as.numeric(as.character(x)))
  temp_player[is.na(temp_player)] = 0
  
  om_batting <- bind_rows(om_batting,temp_player)
}
#Create new columns to simplify next formulas
om_batting_agg <- om_batting %>% 
  mutate(on_base = (H + BB + HBP),
         plate_appearances = (AB + BB + HBP + SF),
         singles = H - `2B` - `3B` - `HR`,
         uBB = BB - IBB)
#Create offensive metrics for each game.
om_game_batting_data <- om_batting_agg %>% group_by(Date) %>% 
  summarise(Opponent = max(Opponent),
            `OP%` = (sum(on_base)/sum(plate_appearances)),
            Slg = (sum(singles) + sum(`2B`*2) + sum(`3B` *3) + sum(HR *4))/sum(AB),
            OPS = `OP%` + Slg,
            RC = ((sum(H) + sum(BB)) * sum(TB)) / (sum(AB) + sum(BB)),
            wOBA = ((sum(uBB*.69)+sum(HBP*.719)+sum(singles*.883)+sum(`2B`*1.248)+sum(`3B`*1.577)+sum(HR*2.031))/(sum(AB)+sum(BB)-sum(IBB)+sum(SF)+sum(HBP))),
            `K%` = sum(K)/sum(plate_appearances),
            `BB%` = sum(BB)/sum(plate_appearances),
            BABIP = (sum(H) - sum(HR))/(sum(AB) - sum(K) - sum(HR) + sum(SF)))
  

om_pitching <- data.frame()
for(i in om_pitchers$player_id){
  temp_player <-  ncaa_game_logs(player_id = i, year = 2022, type = "pitching")
  #col_names <- c('R','AB','H','2B','3B','TB','HR','RBI','BB','HBP','SF',
  #               'SH','K','OPP DP','CS','Picked','SB','IBB','GDP','RBI2out')
  #temp_player$player_id <- i
  
  temp_player$SO[is.na(temp_player$SO)] = 0
  temp_player$FO[is.na(temp_player$FO)] = 0
  temp_player$GO[is.na(temp_player$GO)] = 0
  temp_player$`Inh Run`[is.na(temp_player$`Inh Run`)] = 0
  temp_player$`Inh Run Score`[is.na(temp_player$`Inh Run Score`)] = 0
  om_pitching <- bind_rows(om_pitching,temp_player)
}

om_pitching_agg <- om_pitching %>% 
  mutate(IP = (SO+GO+FO + (`Inh Run` - `Inh Run Score`))/3)

om_pitching_agg %>% 
  group_by(player_id) %>% 
  summarise(player_name = max(player_name),
            app = sum(App, na.rm=TRUE),
            ER = sum(ER,na.rm=TRUE),
            IP = sum(IP, na.rm=TRUE),
            ERA = sum(ER, na.rm=TRUE)/sum(IP, na.rm=TRUE)*9,
            WHIP = (sum(BB, na.rm=TRUE) + sum(H, na.rm=TRUE))/sum(IP,na.rm=TRUE)) %>% 
  print(n=21)

get_ncaa_baseball_roster(team_id = 433, year = 2023)

year = 2023
subset(baseballr::ncaa_season_id_lu, baseballr::ncaa_season_id_lu$season == year, select = id)
baseballr::