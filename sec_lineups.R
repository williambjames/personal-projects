library(tidyverse)
library(bigballR)
sec <- teamids %>% 
  filter(Season == '2021-22' & Conference == 'SEC')
sched <- data.frame()
for(i in sec$ID){
  temp <- get_team_schedule(team.id = i)
  sched <- bind_rows(sched,temp)
}
sec_names <- c('Alabama','Arkansas','Auburn','Florida','Georgia','Kentucky','LSU','Ole Miss',
         'Mississippi St.','Missouri','South Carolina','Tennessee','Texas A&M','Vanderbilt')
sec_colors <- c('#9E1B32','#9D2235','#03244d','#FA4616','#BA0C2F','#0033A0','#461D7C','#006BA6',
                '#660000','#F1B82D','#73000A','#FF8200','#500000','#866D4B')

sec_info <- tibble(team=sec_names,
                   color=sec_colors)


games_sched <- sched %>% 
  distinct(Box_ID)
pbp_sched <- sched %>% 
  distinct(Game_ID)
boxes <- get_box_scores(game_ids = games_sched)
pbp <- get_play_by_play(game_ids = pbp_sched)
roster <- data.frame()
for(i in sec$ID){
  tempr <- get_team_roster(team.id = i)
  tempr$team_id <- i
  roster <- bind_rows(roster,tempr)
}
roster <- roster  %>% 
  merge(sec[c('ID','Team')],by.x='team_id',by.y='ID')
sec_lineups <- bigballR::get_lineups(pbp,garbage.filter = T)
sec_lineups_ht <- sec_lineups %>% 
  merge(roster[c('Player','HtInches','Yr','Team')],by.x=c('P1','Team'),by.y=c('Player','Team')) %>% 
  merge(roster[c('Player','HtInches','Yr','Team')],by.x=c('P2','Team'),by.y=c('Player','Team')) %>% 
  rename(P1_Ht = `HtInches.x`,
         P2_Ht = `HtInches.y`,
         P1_Yr = `Yr.x`,
         P2_Yr = `Yr.y`) %>% 
  merge(roster[c('Player','HtInches','Yr','Team')],by.x=c('P3','Team'),by.y=c('Player','Team')) %>% 
  merge(roster[c('Player','HtInches','Yr','Team')],by.x=c('P4','Team'),by.y=c('Player','Team')) %>% 
  merge(roster[c('Player','HtInches','Yr','Team')],by.x=c('P5','Team'),by.y=c('Player','Team')) %>% 
  rename(P3_Ht = `HtInches.x`,
         P4_Ht = `HtInches.y`,
         P5_Ht = `HtInches`,
         P3_Yr = `Yr.x`,
         P4_Yr = `Yr.y`,
         P5_Yr = `Yr`) %>% 
  mutate(AvgHt = (P1_Ht + P2_Ht + P3_Ht + P4_Ht + P5_Ht)/5)
  
sec_lineups %>% 
  ggplot() +
  geom_density(mapping = aes(x=Mins))

df <- sec_lineups %>% 
  mutate(on_court = paste(P1,P2,P3,P4,P5, sep = " ")) %>% 
  filter(Mins > 50)

sec_lineups_ht %>% 
  filter(Mins > 40) %>% 
  inner_join(sec_info,by = c('Team' = 'team')) %>% 
  ggplot() +
  geom_smooth(mapping = aes(x = AvgHt,
                           y = DRTG,
                           color = color)) +
  scale_color_identity()
# 
# sec_onoff <- data.frame()
# for(i in roster$Player){
#   temponoff <- on_off_generator()
}