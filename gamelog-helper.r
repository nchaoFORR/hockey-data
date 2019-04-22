library(rvest)
library(tidyverse)
library(lubridate)
library(RSelenium)

fetch_gamelog <- function(game_code) {
  
  team_acronyms <- read_csv('team-acronyms.csv')$team
  
  #game_code <- '201710070FLA'
  
  # full url: 'https://www.hockey-reference.com/boxscores/201710070FLA.html'
  
  selectors <- '#scoring td , #scoring th'
  
  date_selector <- '.scorebox_meta div:nth-child(1)'
  
  html_raw <- read_html(paste0('https://www.hockey-reference.com/boxscores/', game_code, '.html'))
  
  scores <- html_raw %>% 
    html_nodes(selectors)
  
  score_text <- html_text(scores)
  
  date <- html_raw %>% 
    html_node(date_selector)
  
  date_text <- html_text(date)
  
  game_summary_list = list()
  
  ## turn into tibble with rowid so i can enumerate through it like in python
  score_text <- tibble(text = score_text) %>% 
    rowid_to_column('rowid')
  
  game_summary_raw_df <- 
    pmap_dfr(score_text, function(rowid, text){
      if(text %in% team_acronyms) {
        temp <- tibble(
          date = date_text,
          team = text,
          raw_text = score_text$text[rowid+1]
        )
      }
    })
  
}

########
########
fetch_gameroster <- function(gamec, schedule) {
  
  # gamec <- '201310010CHI'
  # schedule <- fetch_schedule(2014)
  #gamec <- sched_2017$game_code[5]
  
  ########
  # RSelenium to navigate js
  # rD <- rsDriver() # runs a chrome browser, wait for necessary files to download
  #remDr$open()
  # remDr <- rD$client
  # 
  # remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', gamec, '.html'))
  # 
  # #icf <- remDr$findElement(using = "xpath", '//*[(@id = "TBL_adv")]//tbody//*[contains(concat( " ", @class, " " ), concat( " ", "right", " " ))] | //*[(@id = "TBL_adv")]//tbody//*[contains(concat( " ", @class, " " ), concat( " ", "left", " " ))]')
  # data <- remDr$findElements(using = 'xpath', value ="//*[@_celltype='data-stats']")
  # data$clickElement()
  # 
  # remDr$hig
  # 
  # 
  # remDr$close()
  # # stop the selenium server
  # rD[["server"]]$stop()
  # 
  # # Navigate to our page
  # 
  # 
  # pageSource<-scrape(paste0('https://www.hockey-reference.com/boxscores/', gamec, '.html'),headers=TRUE,
  #                    parse=FALSE)
  # 
  # if(attributes(pageSource)$headers["statusCode"]==200) {
  #   page<-scrape(object="pageSource")
  #   xpathSApply(page,"//table//td/a",xmlValue)
  # } else {
  #   cat
  # 
  # }
  #no need for remDr$open() browser should already be open
  #remDr$navigate("http://stats.nba.com/game/0041700404/playbyplay/")
  
  
  # html_raw %>% 
  #   html_table()
  
  ### SETUP
  #gamec <- '201610120CHI'
  # get all team acronyms
  
  team_acronyms <- read_csv('team-acronyms.csv')$team
  
  #gamec <- "201710070FLA"
  
  # figure out month and year to know what season to lookup
  themonth <- str_replace(gamec, "0[A-Z]{3}$", "") %>% ymd() %>% month
  theyear <- str_replace(gamec, "0[A-Z]{3}$", "") %>% ymd() %>% year()
  
  if(themonth %in% 10:12){
    theyear = theyear + 1
  }
  
  # fetch season schedule -- this is a lookup table for home and away team, using game_code
  schedule_team_lookup <- schedule
  
  ############
  # Fetch data for Away team
  
  # name of away team
  away_team <- schedule_team_lookup %>% 
    filter(game_code == gamec) %>% 
   .$visitor
  
  #LAK_skaters a
  ### selector for names
  away_selector <- paste0('#', away_team, ', #', away_team, '_skaters a')
  
  ### basic stats
  
  stats <- c('assists', 'points', 'plusminus', 'pim', 'ev_goals', 'pp_goals',
             'sh_goals', 'gw_goals', 'ev_assists', 'pp_assists', 'sh_assists',
             'shots', 'shot_perc', "shifts", "toi")
  
  away_goals_selector <- paste0('#', away_team, '_skaters .left+ .right')
  

  
  date_selector <- '.scorebox_meta div:nth-child(1)'
  
  html_raw <- read_html(paste0('https://www.hockey-reference.com/boxscores/', gamec, '.html'))
  
  # away_stats_table <- '#NYR_skaters td'
  # html_raw %>% 
  #   html_table(away_stats_table)
  
  ### Vector of goals
  
  away_goals <- html_raw %>% 
    html_nodes(away_goals_selector) %>% 
    html_text() %>% 
    .[-length(.)] # drop total goals
  
  ### Vectors of player names
  
  away_raw <- html_raw %>% 
    html_nodes(away_selector) %>% 
    html_text()  
    #.[-length(.)] # drop goalie
  
  ### Get the basic stats
  away_basic_stats <-
    map_dfc(4:18, function(index){
      message(index)
      index = 18
      
      temp <- tibble(
        var = paste0("#", away_team, '_skaters tbody .right:nth-child(', index, ')') %>% 
          html_nodes(html_raw, .) %>% 
          html_text
      )
      #names(temp) <- stat
      
    })
  
  ### Possesion metrics for away team
  # away_poss <- html_raw %>% 
  #   html_nodes(xpath = '//*[(@id = "TBL_adv")]//*[contains(concat( " ", @class, " " ), concat( " ", "ALLAll", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "right", " " ))]') %>% 
  #   html_text()
  
  names(away_basic_stats) <- stats
  
  ### Final dataframe for away team
  
  away_df <-
    away_basic_stats %>% 
    mutate(team = away_team, player = away_raw, goals = away_goals) %>% 
    select(player, team, everything())
    #tibble(team = away_team, player = away_raw)
  
  
  ####
  # Now handle home team
  
  home_team <- schedule_team_lookup %>% 
    filter(game_code == gamec) %>% 
    .$home
  
  home_selector <- paste0('#', home_team, ', #', home_team, '_skaters a')
  
  home_goals_selector <- paste0('#', home_team, '_skaters .left+ .right')
  
  
  home_raw <- html_raw %>% 
    html_nodes(home_selector) %>% 
    html_text()
  
  ###
  # Vector of goals
  home_goals <- html_raw %>% 
    html_nodes(home_goals_selector) %>% 
    html_text() %>% 
    .[-length(.)] # drop total goals
  
  ### Get the basic stats
  home_basic_stats <-
    map_dfc(4:18, function(index){
      
      temp <- tibble(
        var = paste0("#", home_team, '_skaters tbody .right:nth-child(', index, ')') %>% 
          html_nodes(html_raw, .) %>% 
          html_text
      )
      #names(temp) <- stat
      
    })
  
  names(home_basic_stats) <- stats
  
  # Build home data table
  home_df <-
    tibble(team = home_team,
           player = home_raw,
           goals = home_goals) %>% 
    bind_cols(home_basic_stats)
  
  
  # Get the date
  
  date_raw <- html_raw %>% 
    html_node(date_selector) %>% 
      html_text()
  
  # output
  
  out <- bind_rows(
    away_df, home_df
  ) %>% 
    mutate(game_code = gamec) %>% 
    select(game_code, everything())
  
  return(out)
  
}

#season <- '2018'

fetch_schedule <- function(season) {
  
  #season <- 2014
  
  # 2014 had some cancellations due to snowstorms that need to be handled.
  
  schedule_url <- paste0('https://www.hockey-reference.com/leagues/NHL_', season, '_games.html')
  
  schedule_raw <- read_html(schedule_url)
  
  tmp <- schedule_raw %>% html_table()
  
  # team1_selector <- '#games .left+ .left a'
  # team2_selector <- '#games td~ .left a'
  # date_selector <- '#games th a'
  # 
  # team1 <- html_nodes(schedule_raw, team1_selector) %>% 
  #   html_text()
  # team2 <- html_nodes(schedule_raw, team2_selector) %>% 
  #   html_text()
  # 
  # dates <- html_nodes(schedule_raw, date_selector) %>% 
  #   html_text()
  # 
  # schedule_df <- tibble(
  #   date = dates,
  #   visitor = team1,
  #   home = team2
  # )
  
  if(as.numeric(season) == 2014) {
    
    schedule_df <- tmp[[1]] %>% 
      .[, c("Date", "Visitor", "Home", "Notes")] %>% 
      rename(date = Date, visitor = Visitor, home = Home) %>% 
      mutate(cancelled = ifelse(str_detect(Notes, "([Pp]ostpone)|([Rr]eschedule)"), 1, 0)) %>% 
      filter(cancelled == 0) %>% 
      select(-Notes, -cancelled)
    
  } else {

    schedule_df <- tmp[[1]] %>%
      .[, c("Date", "Visitor", "Home")] %>%
      rename(date = Date, visitor = Visitor, home = Home)

  }
  

  
  team_acronyms <- read_csv('team-acronyms.csv')$team
  
  schedule_df <- schedule_df %>% 
    mutate_at(vars(visitor, home), function(team){
                                case_when(
                                 team == "Atlanta Thrashers" ~ 'ATL',
                                 team == 'Anaheim Ducks' ~ 'ANA',
                                 team == 'Arizona Coyotes' ~ 'ARI',
                                 team == 'Boston Bruins' ~ 'BOS',
                                 team == 'Buffalo Sabres' ~'BUF' ,
                                 team == 'Calgary Flames' ~ 'CGY',
                                 team == 'Carolina Hurricanes' ~ 'CAR',
                                 team == 'Chicago Blackhawks' ~ 'CHI',
                                 team == 'Colorado Avalanche' ~ 'COL',
                                 team == 'Columbus Blue Jackets' ~ 'CBJ',
                                 team == 'Dallas Stars' ~ 'DAL',
                                 team == 'Detroit Red Wings' ~ 'DET',
                                 team == 'Edmonton Oilers' ~ 'EDM',
                                 team == 'Florida Panthers' ~ 'FLA',
                                 team == 'Los Angeles Kings' ~ 'LAK',
                                 team == "Mighty Ducks of Anaheim" ~ "MDA",
                                 team == 'Minnesota Wild' ~ 'MIN',
                                 team == 'Montreal Canadiens' ~ 'MTL',
                                 team == 'Nashville Predators' ~ 'NSH',
                                 team == 'New Jersey Devils' ~ 'NJD',
                                 team == 'New York Islanders' ~ 'NYI',
                                 team == 'New York Rangers' ~ 'NYR',
                                 team == 'Ottawa Senators' ~ 'OTT',
                                 team == 'Philadelphia Flyers' ~ 'PHI',
                                 team == 'Phoenix Coyotes' ~ 'PHX',
                                 team == 'Pittsburgh Penguins' ~ 'PIT',
                                 team == 'San Jose Sharks' ~ 'SJS',
                                 team == 'St. Louis Blues' ~ 'STL',
                                 team == 'Tampa Bay Lightning' ~ 'TBL',
                                 team == 'Toronto Maple Leafs' ~ 'TOR',
                                 team == 'Vancouver Canucks' ~ 'VAN',
                                 team == 'Vegas Golden Knights' ~ 'VEG',
                                 team == 'Washington Capitals' ~ 'WSH',
                                 team == 'Winnipeg Jets' ~ 'WPG')
                  }
           
          ) %>% 
    mutate(
      game_code = paste0(str_replace_all(date, "-", ""), 0, home)
    )
  
}


############
# Fetch advance stats (setup RSelenium browser outside the function)
### This function takes a gamecode and grabs the home and away stats
fetch_adv_stats <- function(gamec, schedule) {
  
  team_acronyms <- read_csv('team-acronyms.csv')$team
  
  schedule_team_lookup <- schedule
  
  
  # name of away team
  away_team <- schedule_team_lookup %>% 
    filter(game_code == gamec) %>% 
    .$visitor
  
  # name of the hometeam
  home_team <- schedule_team_lookup %>% 
    filter(game_code == gamec) %>% 
    .$home
  
  
  # figure out month and year to know what season to lookup
  themonth <- str_replace(gamec, "0[A-Z]{3}$", "") %>% ymd() %>% month()
  theyear <- str_replace(gamec, "0[A-Z]{3}$", "") %>% ymd() %>% year()
  
  if(themonth %in% 10:12){
    theyear = theyear + 1
  }
  
  
  ### Navigate to page
  
  #remDr$navigate(paste0('https://www.hockey-reference.com/boxscores/', gamec, '.html'))
  
  #Sys.sleep(4)
  
  # Away team nav
  
  away_hover_path <- paste0('#all_', away_team, '_adv .hasmore span')
  
  away_button_xpath <- paste0('//*[@id="all_', away_team, '_adv"]/div[1]/div/ul/li[1]/div/ul/li[4]/button')
  
  away_csv <- paste0('#csv_', away_team, '_adv')
  
  # Home team nav
  
  home_hover_path <- paste0('#all_', home_team, '_adv .hasmore span')
  
  home_button_xpath <- paste0('//*[@id="all_', home_team, '_adv"]/div[1]/div/ul/li[1]/div/ul/li[4]/button')
  
  home_csv <- paste0('#csv_', home_team, '_adv')
  
  ## Away
  
  # find element to expand list
  away_hover <- remDr$findElement(using = 'css', away_hover_path)
  
  #Sys.sleep(0.5)
  
  # click
  away_hover$clickElement()
  
  Sys.sleep(1)
  # find button to convert to csv
  away_csv_button <- remDr$findElement(using = "xpath", away_button_xpath)
  
  # click it
  away_csv_button$clickElement()
  
  Sys.sleep(1)
  # grab csv
  away_csv_data <- remDr$findElement(using = 'css', away_csv)
  
  # grab data
  away_data <- 
    away_csv_data$getElementText()[[1]] %>%
    read_csv() %>% 
    .[1:nrow(.)-1, ] %>% 
    mutate(team = away_team)
  
  ## Home
  
  # find element to expand list
  home_hover <- remDr$findElement(using = 'css', home_hover_path)
  
  # click
  home_hover$clickElement()
  
  Sys.sleep(1)
  
  # find button to convert to csv
  home_csv_button <- remDr$findElement(using = "xpath", home_button_xpath)
  
  # click it
  home_csv_button$clickElement()
  
  Sys.sleep(1)
  
  # grab csv
  home_csv_data <- remDr$findElement(using = 'css', home_csv)
  
  # grab data
  home_data <- 
    home_csv_data$getElementText()[[1]] %>% 
    read_csv() %>% 
    .[1:nrow(.)-1, ] %>% 
    mutate(team = home_team)
  
  out <- bind_rows(away_data, home_data) %>% 
    mutate(game_code = gamec)
  
}



message('game log helper loaded.')

#test <- fetch_gamelog('201902020WPG')
