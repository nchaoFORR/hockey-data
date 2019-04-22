source('gamelog-helper.r')
library(tidyverse)
library(broom)
library(tidyr)

# Fetch 2017 Season

sched_2017 <- fetch_schedule('2017')

# Screw it, fetch from 2006-2018 (Post-lockout)
# Right to disk and clear out for each dataset to ensure RAM never runs out

# grab every season schedule first

seasons <- map_dfr(2015:2018, function(season){
  
  #if(season == 2014) return()
  #season = 2015
  
  Sys.sleep(3)
  message(season)
  
 # season = 2014
# skip 2014 for now -- it had cancellations due to snowstorms that mess up the scraper
  
  tmp <- fetch_schedule(season) %>% 
    mutate(season = season)
  
})


# Now nest that by season and map through each season
# assist stats only began in 2015
seasons_nested_df <- 
  seasons %>%
  group_by(season) %>% 
  nest() %>% 
  filter(season >= 2015)

### now walk through every season, fetch game stats, and write to disk

j <- 2015
walk(seasons_nested_df$data, function(season_data){
  
  i <<- 1
  message(paste0('season ', j))
  map_dfr(season_data$game_code, function(game){
    
    Sys.sleep(3)
    
    message(paste0('fetching game ', i, ' out of ', length(season_data$game_code)))
    
    i <<- i + 1
    
    fetch_gameroster(game, season_data)

    
  }) %>% write_csv(., paste0('more-data/season-', j, '.csv'))
  
  j <<- j + 1
  
})


#### Now do that same but grab advanced stats

# Now nest that by season and map through each season
# assist stats only began in 2015
seasons_nested_df <- 
  seasons %>%
  group_by(season) %>% 
  nest() %>% 
  filter(season >= 2015)


### Setup Selenium browser
rD <- rsDriver() # runs a chrome browser, wait for necessary files to download#
#remDr$open()
remDr <- rD$client
# 

### now walk through every season, fetch game stats, and write to disk

j <- 2018
walk(seasons_nested_df$data[4], function(season_data){
  
  i <<- 1
  message(paste0('season ', j))
  
  map_dfr(season_data$game_code[844:length(season_data$game_code)], function(game){
    
    Sys.sleep(1)
    
    message(paste0('fetching game ', i, ' out of ', length(season_data$game_code)))
    
    i <<- i + 1
    
    remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', game, '.html'))
    
    Sys.sleep(6)
    
    # a bit unreliable -- if error occurs, restart the browser
    try_fetch_games <- function(game, season_data) {
      
      tryCatch(fetch_adv_stats(game, season_data),
      
        error = function(e) {
        message("error occured; restarting server")
        # stop current server
        remDr$close()
        rD[["server"]]$stop()
        rm(rD)
        gc()
        
        # restart server
        rD <<- rsDriver() # runs a chrome browser, wait for necessary files to download#
        Sys.sleep(1)
        #remDr$open()
        remDr <<- rD$client
        
        Sys.sleep(1)
        
        remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', game, '.html'))
        
        Sys.sleep(6)
        
        fetch_adv_stats(game, season_data)
        
        }
      )
    }
    
    temp <- try_fetch_games(game, season_data)
    
    write_csv(temp, paste0('ind-games/gamecode-', game, '.csv'))
    
    temp
    
  }) %>% write_csv(., paste0('more-data/season-', j, '-adv.csv'))
  
  j <<- j + 1
  
})

### Close Selenium

remDr$close()
# stop the selenium server
rD$server$stop()
rD[["server"]]$stop()
rm(rD)
gc()

Sys.sleep(1)

### Setup Selenium browser
rD <- rsDriver() # runs a chrome browser, wait for necessary files to download#
#remDr$open()
remDr <- rD$client
# 


j <- 2016
walk(seasons_nested_df$data[2:length(seasons_nested_df$data)], function(season_data){
  
  i <<- 1
  message(paste0('season ', j))
  
  map_dfr(season_data$game_code, function(game){
    
    Sys.sleep(1)
    
    message(paste0('fetching game ', i, ' out of ', length(season_data$game_code)))
    
    i <<- i + 1
    
    remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', game, '.html'))
    
    Sys.sleep(6)
    
    # a bit unreliable -- if error occurs, restart the browser
    try_fetch_games <- function(game, season_data) {
      
      tryCatch(fetch_adv_stats(game, season_data),
               
               error = function(e) {
                 message("error occured; restarting server")
                 # stop current server
                 remDr$close()
                 rD[["server"]]$stop()
                 rm(rD)
                 gc()
                 
                 # restart server
                 rD <<- rsDriver() # runs a chrome browser, wait for necessary files to download#
                 Sys.sleep(1)
                 #remDr$open()
                 remDr <<- rD$client
                 
                 Sys.sleep(1)
                 
                 remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', game, '.html'))
                 
                 Sys.sleep(6)
                 
                 fetch_adv_stats(game, season_data)
                 
               }
      )
    }
    
    temp <- try_fetch_games(game, season_data)
    
    write_csv(temp, paste0('ind-games/gamecode-', game, '.csv'))
    
    temp
    
  }) %>% write_csv(., paste0('more-data/season-', j, '-adv.csv'))
  
  j <<- j + 1
  
})

### Close Selenium

remDr$close()
# stop the selenium server
rD$server$stop()
rD[["server"]]$stop()
rm(rD)
gc()


#write_csv(rost_2017, "all-gamestats-2017.csv")

# Sanity Check:
# rost_2017 %>% 
#   mutate_at(vars(-team, -player, -game_code), funs(as.numeric)) %>% 
#   group_by(player) %>% 
#   summarise(num_goals = sum(goals, na.rm=TRUE)) %>% 
#   arrange(desc(num_goals))
# 
# # looks good!
# 
# df_raw <- read_csv("all-gamestats-2017.csv")
# 
# # Fix the misnamed stats
# names(df_raw)[4:(ncol(df_raw)-1)] <- c('a', 'pts', 'plusminus', 'pim', 'ev_g', 'pp_g', 'sh_g', 'gw_g', 'ev_a', 'pp_a', 'sh_a', 's')

# df_clean <- rost_2017 %>% 
#   mutate_at(vars(-team, -player, -game_code), funs(as.numeric))
# 
# summary(df_clean$shifts)
  