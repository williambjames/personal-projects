library(tidyverse)
df <- readxl::read_xlsx('ncaa basketball 2017-18.xlsx')
df2 <- readxl::read_xlsx('ncaa basketball 2018-19.xlsx')
df3 <- readxl::read_xlsx('ncaa basketball 2019-20.xlsx')
df4 <- readxl::read_xlsx('ncaa basketball 2020-21.xlsx')
df5 <- readxl::read_xlsx('ncaa basketball 2021-22.xlsx')
df$ML <- as.numeric(df$ML)
df2$ML <- as.numeric(df2$ML)
df3$ML <- as.numeric(df3$ML)
df4$ML <- as.numeric(df4$ML)
df5$ML <- as.numeric(df5$ML)
df$Close <- as.numeric(df$Close)
df2$Close <- as.numeric(df2$Close)
df3$Close <- as.numeric(df3$Close)
df4$Close <- as.numeric(df4$Close)
df5$Close <- as.numeric(df5$Close)
df$Year <- 17
df2$Year <- 18
df3$Year <- 19
df4$Year <- 20
df5$Year <- 21
DF <- bind_rows(df,df2,df3,df4,df5)
rows <- nrow(DF)
odd_rows <- seq_len(rows) %% 2
DF <- DF %>% 
  mutate(Close = as.numeric(Close),
         `2H` = as.numeric(`2H`))
h <- DF[odd_rows == 0, ]
a <- DF %>%
  anti_join(h)
h <- h %>%
  mutate(Rot = Rot -1)
h$game <- 1:nrow(h)
a$game <- 1:nrow(a)

df_ <- bind_rows(h,a)
df_$Close[df_$Close == "pk"] <- 0
df_$Close[is.na(df_$Close)] <- 0
df_ <- df_ %>%
  mutate(Type = if_else(Close > 46, 'Total','Spread')) %>% 
  drop_na(Type)


first_half_totals <- df_ %>% 
  filter(Type == 'Total') %>% 
  mutate(`1H` = Close/2) %>% 
  select(game,Year,Date,Rot,`1H`)

second_half_totals <- df_ %>% 
  filter(`2H` > 26) %>%  
  select(Year,Date,Rot,`2H`)

totals <- merge(first_half_totals,second_half_totals,by=c('Year','Date','Rot'))

totals <- merge(totals,h[c('Year','Date','Rot','1st','2nd')],by=c('Year','Date','Rot'))
totals <- merge(totals,a[c('Year','Date','Rot','1st','2nd')],by=c('Year','Date','Rot'))

totals <- totals %>% 
  mutate(`1st` = `1st.x` + `1st.y`,
         `2nd` = `2nd.x` + `2nd.y`,
         `1st_Delta` = `1st` - `1H`,
         `2nd_Delta` = `2nd` - `2H`,
         change = floor(`2H` - `1st`), #diff in total from 1st half (1st half score vs 2H total)
         over = if_else(`2nd` > `2H`,1,0),
         under = if_else(`2nd`<`2H`,1,0),
         push = if_else(`2nd` == `2H`,1,0))%>% 
  select('Date','Rot','1H','2H','1st','2nd',`1st_Delta`,`2nd_Delta`,change,over,under,push,game,Year)



groups <- totals %>% 
  mutate(result = case_when(`1st_Delta` >= 0 & `1st_Delta` <= 5 ~ 'Close Over',
                            `1st_Delta` < 0 & `1st_Delta` >= -5 ~ 'Close Under',
                            `1st_Delta` > 5 & `1st_Delta` <= 10 ~ 'Big Over',
                            `1st_Delta` < -5 & `1st_Delta` >= -10 ~ 'Big Under',
                            `1st_Delta` > 10 ~ 'Huge Over',
                            `1st_Delta` < -10 ~ 'Huge Under')) %>% 
  arrange(game)

results <- groups %>% 
  group_by(result,change) %>% 
  summarise(games = n(),
            over = mean(over),
            under = mean(under),
            push = mean(push),
            nonloss = under+push) %>% 
  arrange(-over)

groups <- groups %>% 
  mutate(units = case_when(result == 'Close Over' ~ 0.07,
                           result == 'Close Under' ~ .074,
                           result == 'Big Under' ~ .074,
                           result == 'Big Over' ~ 0,
                           result == 'Huge Under' ~ 0.045,
                           result == 'Huge Over' ~ 0.0303),
         adj = case_when(change >= 0 & change < 5 ~ '< + 5',
                         change >= 5 & change <= 10 ~ '+ 5-10',
                         change > 10 ~ '10+',
                         change < 0 & change > -5 ~ '< - 5',
                         change <= -5 & change >= -10 ~ '- 5-10',
                         change < -10 ~ '- 10+'))

by_year <- groups %>% 
  filter(Year == 21) %>% 
  group_by(result,adj) %>% 
  summarise(games = n(),
            over = mean(over),
            under = mean(under),
            push = mean(push))

games_to_bet <- groups %>% 
  filter(result %in% c('Close Over','Close Under','Big Under','Huge Under','Huge Over')) %>% 
  distinct(game,.keep_all=TRUE)
bankroll <- 300
tally <- c()
for(i in games_to_bet$game){
  game <- games_to_bet %>% 
    filter(game==i)
  bet <- game$unit * bankroll
  print(i)
  outcome <- if_else(game$under == 1, bet * .90909, -1 *bet)
  outcome <- if_else(game$push == 1, 0, outcome)
  bankroll <- bankroll + outcome
  tally <- c(tally, bankroll)
}

results <- groups %>% 
  group_by(result) %>% 
  summarise(games = n(),
            over = mean(over),
            under = mean(under)) %>% 
  arrange(-over)
games_to_bet$bank <- tally
games_to_bet %>% 
  ggplot() +
  geom_line(mapping=aes(x=game,
                        y=bank))

games_to_bet$bank <- tally




groups %>% 
  ggplot() +
  geom_point(mapping = aes(x = `1st`,
                           y = `2H`,
                           color = under))
groups %>% 
  filter(`2nd` > 0 & abs(change) < 20) %>% 
  ggplot() +
  geom_point(mapping = aes(x = change,
                           y = under)) +
  geom_smooth(mapping = aes(x = change,
                            y = under),
              method = 'glm') +
  facet_wrap(~result,scales = 'free')

basic <- groups %>%
  mutate(basic = if_else(result %in% c('Huge Over'), 'Above','Below'))

basic %>% 
  group_by(basic) %>% 
  summarise(games = n(),
            over = mean(over),
            under = mean(under),
            oush = mean(push))

groups2 <- totals %>% 
  mutate(result = case_when(`1st_Delta` >= 0 & `1st_Delta` <= 5 ~ '0-5 Over',
                            `1st_Delta` < 0 & `1st_Delta` >= -5 ~ '0-5 Under',
                            `1st_Delta` > 5 & `1st_Delta` <= 10 ~ '5-10 Over',
                            `1st_Delta` < -5 & `1st_Delta` >= -10 ~ '5-10 Under',
                            `1st_Delta` > 10 ~ '10+ Over',
                            `1st_Delta` < -10 ~ '10+ Under')) %>% 
  arrange(game)

results <- groups2 %>% 
  group_by(result) %>% 
  summarise(games = n(),
            over = mean(over),
            under = mean(under),
            push = mean(push)) %>% 
  arrange(-over)
