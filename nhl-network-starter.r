source('gamelog-helper.r')
library(tidyverse)
#library(network)
library(tidygraph)
library(ggraph)

# i'll try and work with 2017-2018 season

schedule_df <- fetch_schedule('2018')

game_codes <- schedule_df$game_code

all_games_raw <- 
  map_dfr(game_codes, ~{
    print(.)
    fetch_gamelog(.)
  })

write_rds(all_games_raw, "all-games-2018_raw.rds")

# this is it -- we have all our pairings for every goal in the 2018 season
all_tuples_raw <- all_games_raw %>% 
  mutate(
    raw_text = str_replace_all(raw_text, '\\t|\\n', ''),
    spec = str_extract(raw_text, "PP|EN|SH"),
    goal = str_extract(raw_text, "[A-Z].* \\("),
    goal = str_replace(goal, " \\(", ""),
    goal = str_replace(goal, '(^PP|EN|SH).{3}', ''),
    assists = str_extract(raw_text, paste0("(?<=", goal, ").*")),
    assists = str_replace(assists, "^.{4}", ""),
    assists = ifelse(assists == "", NA, assists)
  ) %>% 
  filter(!is.na(goal)) %>%  # getting rid of shootout attempts
  separate(assists, c("primary_assist", "second_assist"), sep = " and ") %>% 
  mutate(primary_assist = str_replace(primary_assist, "^\\) ", "")) %>% 
  mutate(primary_assist = str_replace(primary_assist, "\\)", "")) %>% 
  mutate(primary_assist = trimws(primary_assist)) %>% 
  mutate_at(vars(goal, primary_assist, second_assist), ~{str_replace(., '^[A-Z] ', '')})


# sanity check -- 
all_tuples_raw %>% 
  group_by(goal) %>% 
  summarise(goals = n()) %>% 
  arrange(desc(goals)) %>% View()

### might be missing a game or 2 -- hopefully negligible error.

bruins <- all_tuples_raw %>% 
  filter(team == "BOS")

### Node List and Edge List --

node_list <- tibble(
  nodes = c(bruins$goal, bruins$primary_assist, bruins$second_assist)
) %>% 
  unique() %>% 
  rowid_to_column('node_id')

edge_list <- 
  bruins %>% 
  select(goal, primary_assist, second_assist) %>%
  gather('var', 'val', -goal) %>%
  # mutate(val = str_replace(val, "^\\) ", "")) %>%
  mutate(val = ifelse(val == "", NA, val)) %>% 
  drop_na() %>% 
  group_by(goal, val) %>% 
  summarise(weight = n()) %>% 
  ungroup()


id_edge_list <-
  edge_list %>% 
  left_join(node_list, by = c('goal' = 'nodes')) %>% 
  rename(to = node_id) %>% 
  left_join(node_list, by = c('val' = 'nodes')) %>% 
  rename(from = node_id) %>% 
  select(from, to, weight)


####################
# Build some networks
player_network <- network(id_edge_list, vertex.attr = node_list, matrix.type = "edgelist", ignore.eval = FALSE)

plot(player_network, vertex.cex = 3)

detach(package:network)
rm(list = c('player_network'))

library(igraph)

player_network <- graph_from_data_frame(d = id_edge_list, directed = TRUE, vertices = node_list)

plot(player_network, edge.arrow.size = 0.2, layout = layout_with_graphopt)

player_graph <- tbl_graph(nodes = node_list, edges = id_edge_list, directed = TRUE)

player_graph %>% 
  ggraph(layout ='graphopt') +
  geom_edge_link(aes(width = weight), alpha = 0.5) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point() +
  geom_node_text(aes(label = nodes), repel = TRUE) +
  theme_graph()
