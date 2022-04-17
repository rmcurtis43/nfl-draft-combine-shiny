library(tidyverse)
library(tictoc)
library(cfbfastR)
library(cfbplotR)
library(combineR)
library(iVoNcaa)



combine_data <- combineR::pull_combine_data()


saveRDS(combine_data, "data/combine_data.RDS")
combine_data <- readRDS("data/combine_data.RDS")
### Create player stats dataframe #################################
player_stats_all <- data.frame()
for (i in 2004:2021) {
  
  player_stats_all <-  player_stats_all %>%
    bind_rows(
      cfbfastR::cfbd_stats_season_player(i) %>%
        mutate(season = i) %>%
        select(season, everything())
    )
}

saveRDS(player_stats_all, "data/player_stats_all.RDS")
player_stats_all <- readRDS("data/player_stats_all.RDS")
###############################################################


###### ~75 min to run #######################################
#### Create CFB player info dataframe ########################
combine_data_distinct <- combine_data %>%
  distinct(player)

tic("player_info_load")
player_info_all <- data.frame()
for (i in combine_data_distinct$player) {
  
  player_info_all <-  player_info_all %>%
    bind_rows(
      cfbfastR::cfbd_player_info(search_term = i)
    )
}
toc()

saveRDS(player_info_all, "data/player_info_all.RDS")
player_info_all <- readRDS("data/player_info_all.RDS")
##############################################################

### Create NFL player info dataframe #########################
# player_info_nfl <- data.frame()
# for (i in 2000:2021) {
#   
#   player_info_nfl <-  player_info_nfl %>%
#     bind_rows(
#       nflreadr::load_rosters(seasons = i)) %>%
#         mutate(season = i) %>%
#         select(season, everything())
# }

player_info_nfl <- nflfastR::fast_scraper_roster(2000:2021)

saveRDS(player_info_nfl, "data/player_info_nfl.RDS")
player_info_nfl <- readRDS("data/player_info_nfl.RDS")
###############################################################

### Team Info #########################

team_info <- cfbfastR::cfbd_team_info() %>%
  select(school, alt_name1, alt_name2, alt_name3, logos) %>%
  mutate(logo_light = map(logos, pluck, 1) %>% reduce(c)) %>%
  mutate(logo_dark = map(logos, pluck, last) %>% reduce(c)) %>%
  mutate(alt_name1 = case_when(
    school == 'Ole Miss' ~ 'Mississippi',
    school == 'Boston College' ~ 'Boston Col.',
    TRUE ~ as.character(alt_name1)
  )) %>%
  mutate(alt_name1 = ifelse(alt_name1 == school, NA, alt_name1)) %>%
  mutate(alt_name2 = ifelse(alt_name2 == school | alt_name2 == alt_name1, NA, alt_name2)) %>%
  mutate(alt_name3 = ifelse(alt_name3 == school | alt_name3 == alt_name2 | alt_name3 == alt_name1, NA, alt_name3))


saveRDS(team_info, "data/team_info.RDS")
team_info <- readRDS("data/team_info.RDS")





###############################################################

### Conference Info #########################

conference_info <- get_ncaa() 

conference_info <- conference_info %>%
  mutate(ncaa_name = str_trim(ncaa_name)) %>%
  mutate(reference_name = str_trim(reference_name)) %>%
  mutate(ncaa_name = case_when(
    name == "Southern California Trojans" ~ paste0("USC"),
    ncaa_name == "Boston College" ~ "Boston Col.",
    ncaa_name == "Southern Miss." ~ "Southern Miss",
    ncaa_name == "Middle Tenn." ~ "Middle Tennessee State",
    ncaa_name == "Louisiana-Monroe" ~ "La-Monroe",
    TRUE ~ as.character(ncaa_name)
  ))


###############################################################

### Draft Info #########################

tic("draft_info_load")
draft_info_all <- data.frame()
for (i in 1936:2021) {
  draft_info_all <- draft_info_all %>%
    bind_rows(
      rvest::read_html(paste0("https://www.drafthistory.com/index.php/years/",i)) %>%
        rvest::html_nodes("table:nth-child(2)") %>%
        rvest::html_table() %>%
        dplyr::first() %>%
        janitor::clean_names() %>%
        slice(-1) %>%
        janitor::row_to_names(1) %>%
        mutate_at(vars(1:3), as.numeric) %>%
        tidyr::fill(Round) %>%
        mutate(Year = i) %>%
        select(Year, everything())
    ) %>%
    arrange(desc(Year), Player)
}
toc()
saveRDS(draft_info_all, "data/draft_info_all.RDS")


######### nfl.com data###############

library(tidyjson)
library(rjson)
result <- fromJSON(file = "data/combine_2022.json")

json_data_frame <- as.data.frame(result)  

combine_json_data <- result$combineProfiles %>%
  spread_all %>%
  as_tibble() %>%
  mutate(combine_year = 2022) %>%
  mutate(headshot = paste0('https://static.www.nfl.com/image/private/f_png,q_85,h_118,w_118,c_fill,g_face:center,f_auto/league/god-combine-headshots/2022/',id )) %>%
  select(player=person.displayName, combine_year, player_image_nfl1=headshot)
  
saveRDS(combine_json_data, "data/combine_json_data.RDS")
