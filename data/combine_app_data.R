library(tidyverse)
library(combineR)
library(janitor)
library(cfbfastR)
library(fuzzyjoin)
library(shiny)
library(shinyBS)
library(bs4Dash)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library(fresh)



combine_data <- readRDS("data/combine_data.RDS") 

conference_info <- readRDS("data/conference_info.RDS")

draft_info_all <- readRDS("data/draft_info_all.RDS")

combine_json_data <- readRDS("data/combine_json_data.RDS")


player_info <- readRDS("data/player_info_all.RDS") %>%
  group_by(athlete_id) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  select(-rownum, -position) %>%
  rename(school = team, player=name) %>%
  mutate(player_image_cfb = ifelse(grepl('-', athlete_id),
                                   paste0("http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/STA573574.png"),
                                   paste0("http://a.espncdn.com/i/headshots/college-football/players/full/",athlete_id,".png")
  ))

suppressWarnings(
player_info_nfl <- readRDS("data/player_info_nfl.RDS") %>%
  drop_na(espn_id) %>%
  mutate(athlete_id = as.character(espn_id)) %>%
  select(athlete_id, player_image_nfl=headshot_url, years_exp) %>%
  group_by(athlete_id) %>%
  summarise(
    player_image_nfl = first(player_image_nfl),
    nfl_years_exp = max(years_exp, na.rm = TRUE)) %>%
  
  ungroup() %>%
  mutate_if(is.numeric, list(~na_if(., -Inf)))
)

player_info_nfl2 <- readRDS("data/player_info_nfl.RDS") %>%
  distinct(player=full_name, school=college, player_image_nfl2=headshot_url) %>%
  group_by(player) %>% 
  slice(1) %>%
  ungroup()



team_info <- readRDS("data/team_info.RDS")

##### combine manip
combine_clean <- combine_data %>%
  mutate(school = case_when(
    school == "Miami" ~ "Miami (FL)",
    school == "Miami (Ohio)" ~ "Miami (OH)",
    school == "Ala-Birmingham" ~ "Alabama-Birmingham",
    school == "Boston Col." ~ "Boston College",
    school == "NW State (LA)" ~ "Northwestern State (LA)",
    TRUE ~ as.character(school)
  )) %>%
  left_join(player_info, by = c("player", "school")) %>%
  left_join(player_info_nfl, by="athlete_id") %>%
  left_join(combine_json_data, c("player", "combine_year")) %>%
  left_join(player_info_nfl2, by=c("player", "school")) %>%
  group_by(player, school, position) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  select(-rownum) %>%
  mutate(player_image = case_when(
    !is.na(player_image_nfl) ~ player_image_nfl,
    is.na(player_image_nfl) & !is.na(player_image_nfl1) ~ player_image_nfl1,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & !is.na(player_image_nfl2) ~ player_image_nfl2,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & is.na(player_image_nfl2) & !is.na(player_image_cfb) ~ player_image_cfb,
    TRUE ~ as.character("http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/STA573574.png")
  )) %>%
  left_join(
    team_info %>%
      distinct(school, logo_light),
    by="school"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name1) %>%
      distinct(school=alt_name1, logo_light_1=logo_light),
    by="school"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name3) %>%
      distinct(school=alt_name3, logo_light_3=logo_light),
    by="school"
  ) %>%
  left_join(
    conference_info %>%
      distinct(school=ncaa_name, conference, url_conference, logo_light_4=logos),
    by="school"
  ) %>%
  ungroup() %>%
  left_join(
    conference_info %>%
      distinct(school=reference_name,conference_1=conference, url_conference_1=url_conference, logo_light_5=logos),
    by="school"
  ) %>%
  mutate(logo_light = case_when(
    !is.na(logo_light) ~ logo_light,
    is.na(logo_light) & !is.na(logo_light_1) ~ logo_light_1,
    is.na(logo_light) &  is.na(logo_light_1) & !is.na(logo_light_3) ~ logo_light_3,
    is.na(logo_light) &  is.na(logo_light_1) & is.na(logo_light_3) & !is.na(logo_light_4) ~ logo_light_4,
    is.na(logo_light) &  is.na(logo_light_1) & is.na(logo_light_3) & is.na(logo_light_4) & !is.na(logo_light_5) ~ logo_light_5,
    TRUE ~ as.character("https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png")
  )) %>%
  mutate(conference = case_when(
    !is.na(conference) ~ conference,
    is.na(conference) & !is.na(conference_1) ~ conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  mutate(url_conference = case_when(
    !is.na(url_conference) ~ url_conference,
    is.na(url_conference) & !is.na(url_conference_1) ~ url_conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  select(-c(logo_light_1, logo_light_3,logo_light_4, logo_light_5,conference_1, url_conference_1)) %>%
  ungroup() %>%
  mutate(logo_light = case_when(
    grepl("Culver", school) ~ paste0("https://d21gd0ap5v1ndt.cloudfront.net/web02/csc/images_web/headerLogo.png"),
    TRUE ~ as.character(logo_light)
  )) %>%
  mutate(team_color = ifelse(is.na(team_color), "#000000", team_color)) %>%
  mutate(position_cfb = case_when(
    position == "OG" ~ "G",
    position %in% c("OLB", "ILB") ~ "LB",
    position == "EDGE" ~ "DE", 
    position == "PK" ~ "K",
    position == "NT" ~ "DL",
    TRUE ~ as.character(position)
  )) %>%
  mutate(designation = case_when(
    position2 %in% c("DL", "OL") ~ "Bigs",
    position2 %in% c("QB", "TE", "RB", "WR", "LB", "DB") ~ "Skills",
    position2 %in% c("LS", "PK") ~ "Special Teams",
    TRUE ~ as.character(position2)
  )) %>%
  left_join(
    combine_data %>%
      mutate(position_cfb = case_when(
        position == "OG" ~ "G",
        position %in% c("OLB", "ILB") ~ "LB",
        position == "EDGE" ~ "DE", 
        position == "PK" ~ "K",
        position == "NT" ~ "DL",
        TRUE ~ as.character(position)
      )) %>%
      group_by(position_cfb) %>%
      summarise_at(vars(height_in, weight_lbs, vertical_in, broad_jump_in, bench:x40yd), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))) %>%
      mutate_all(~replace(., is.nan(.), NA)), 
    by = "position_cfb"
  ) %>%
  bind_cols(
    combine_data %>%
      summarise_at(vars(height_in, weight_lbs, vertical_in, broad_jump_in, bench:x40yd), list(mean_all = ~mean(., na.rm = TRUE), sd_all = ~sd(., na.rm = TRUE))) %>%
      mutate_all(~replace(., is.nan(.), NA))
  ) %>%
  left_join(
    combine_data %>%
      mutate(designation = case_when(
        position2 %in% c("DL", "OL") ~ "Bigs",
        position2 %in% c("QB", "TE", "RB", "WR", "LB", "DB") ~ "Skills",
        position2 %in% c("LS", "PK") ~ "Special Teams",
        TRUE ~ as.character(position2))) %>%
      group_by(designation) %>%
      summarise_at(vars(height_in, weight_lbs, vertical_in, broad_jump_in, bench:x40yd), list(mean_designation = ~mean(., na.rm = TRUE), sd_designation = ~sd(., na.rm = TRUE))) %>%
      mutate_all(~replace(., is.nan(.), NA)), 
    by = "designation"
  ) %>%
  # z scores
  mutate(height_z_position = (height_in - height_in_mean)/height_in_sd) %>%
  mutate(weight_z_position = (weight_lbs - weight_lbs_mean)/weight_lbs_sd) %>%
  
  mutate(vertical_z_position = (vertical_in - vertical_in_mean)/vertical_in_sd) %>%
  mutate(broad_jump_z_position = (broad_jump_in - broad_jump_in_mean)/broad_jump_in_sd) %>%
  mutate(bench_z_position = (bench - bench_mean)/bench_sd) %>%
  mutate(x3cone_z_position = ((x3cone - x3cone_mean)/x3cone_sd)*-1) %>%
  mutate(shuttle_z_position = ((shuttle - shuttle_mean)/shuttle_sd)*-1) %>%
  mutate(x40yd_z_position = ((x40yd - x40yd_mean)/x40yd_sd)*-1) %>%
  
  mutate(height_z_all = (height_in - height_in_mean_all)/height_in_sd_all) %>%
  mutate(weight_z_all = (weight_lbs - weight_lbs_mean_all)/weight_lbs_sd_all) %>%
  
  mutate(vertical_z_all = (vertical_in - vertical_in_mean_all)/vertical_in_sd_all) %>%
  mutate(broad_jump_z_all = (broad_jump_in - broad_jump_in_mean_all)/broad_jump_in_sd_all) %>%
  mutate(bench_z_all = (bench - bench_mean_all)/bench_sd_all) %>%
  mutate(x3cone_z_all = ((x3cone - x3cone_mean_all)/x3cone_sd_all)*-1) %>%
  mutate(shuttle_z_all = ((shuttle - shuttle_mean_all)/shuttle_sd_all)*-1) %>%
  mutate(x40yd_z_all = ((x40yd - x40yd_mean_all)/x40yd_sd_all)*-1) %>%
  
  mutate(height_z_designation = (height_in - height_in_mean_designation)/height_in_sd_designation) %>%
  mutate(weight_z_designation = (weight_lbs - weight_lbs_mean_designation)/weight_lbs_sd_designation) %>%
  
  mutate(vertical_z_designation = (vertical_in - vertical_in_mean_designation)/vertical_in_sd_designation) %>%
  mutate(broad_jump_z_designation = (broad_jump_in - broad_jump_in_mean_designation)/broad_jump_in_sd_designation) %>%
  mutate(bench_z_designation = (bench - bench_mean_designation)/bench_sd_designation) %>%
  mutate(x3cone_z_designation = ((x3cone - x3cone_mean_designation)/x3cone_sd_designation)*-1) %>%
  mutate(shuttle_z_designation = ((shuttle - shuttle_mean_designation)/shuttle_sd_designation)*-1) %>%
  mutate(x40yd_z_designation = ((x40yd - x40yd_mean_designation)/x40yd_sd_designation)*-1) %>%
  
  #percentiles
  mutate(height_percentile_position = round((pnorm(height_in, mean = height_in_mean, sd = height_in_sd) *100), 0)) %>%
  mutate(weight_percentile_position = round((pnorm(weight_lbs, mean = weight_lbs_mean, sd = weight_lbs_sd) *100), 0)) %>%
  
  mutate(vertical_percentile_position = round((pnorm(vertical_in, mean = vertical_in_mean, sd = vertical_in_sd) *100), 0)) %>%
  mutate(broad_jump_percentile_position = round((pnorm(broad_jump_in, mean = broad_jump_in_mean, sd = broad_jump_in_sd) *100), 0)) %>%
  mutate(bench_percentile_position = round((pnorm(bench, mean = bench_mean, sd = bench_sd) *100), 0)) %>%
  mutate(shuttle_percentile_position = 100 - round((pnorm(x3cone, mean = x3cone_mean, sd = x3cone_sd) *100), 0)) %>%
  mutate(x3cone_percentile_position = 100 - round((pnorm(shuttle, mean = shuttle_mean, sd = shuttle_sd) *100), 0)) %>%
  mutate(x40yd_percentile_position = 100 - round((pnorm(x40yd, mean = x40yd_mean, sd = x40yd_sd) *100), 0)) %>%
  
  mutate(height_percentile_all = round((pnorm(height_in, mean = height_in_mean_all, sd = height_in_sd_all) *100), 0)) %>%
  mutate(weight_percentile_all = round((pnorm(weight_lbs, mean = weight_lbs_mean_all, sd = weight_lbs_sd_all) *100), 0)) %>%
  
  mutate(vertical_percentile_all = round((pnorm(vertical_in, mean = vertical_in_mean_all, sd = vertical_in_sd_all) *100), 0)) %>%
  mutate(broad_jump_percentile_all = round((pnorm(broad_jump_in, mean = broad_jump_in_mean_all, sd = broad_jump_in_sd_all) *100), 0)) %>%
  mutate(bench_percentile_all = round((pnorm(bench, mean = bench_mean_all, sd = bench_sd_all) *100), 0)) %>%
  mutate(shuttle_percentile_all = 100 - round((pnorm(x3cone, mean = x3cone_mean_all, sd = x3cone_sd_all) *100), 0)) %>%
  mutate(x3cone_percentile_all = 100 - round((pnorm(shuttle, mean = shuttle_mean_all, sd = shuttle_sd_all) *100), 0)) %>%
  mutate(x40yd_percentile_all = 100 - round((pnorm(x40yd, mean = x40yd_mean_all, sd = x40yd_sd_all) *100), 0)) %>%
  
  mutate(height_percentile_designation = round((pnorm(height_in, mean = height_in_mean_designation, sd = height_in_sd_designation) *100), 0)) %>%
  mutate(weight_percentile_designation = round((pnorm(weight_lbs, mean = weight_lbs_mean_designation, sd = weight_lbs_sd_designation) *100), 0)) %>%
  
  mutate(vertical_percentile_designation = round((pnorm(vertical_in, mean = vertical_in_mean_designation, sd = vertical_in_sd_designation) *100), 0)) %>%
  mutate(broad_jump_percentile_designation = round((pnorm(broad_jump_in, mean = broad_jump_in_mean_designation, sd = broad_jump_in_sd_designation) *100), 0)) %>%
  mutate(bench_percentile_designation = round((pnorm(bench, mean = bench_mean_designation, sd = bench_sd_designation) *100), 0)) %>%
  mutate(shuttle_percentile_designation = 100 - round((pnorm(x3cone, mean = x3cone_mean_designation, sd = x3cone_sd_designation) *100), 0)) %>%
  mutate(x3cone_percentile_designation = 100 - round((pnorm(shuttle, mean = shuttle_mean_designation, sd = shuttle_sd_designation) *100), 0)) %>%
  mutate(x40yd_percentile_designation = 100 - round((pnorm(x40yd, mean = x40yd_mean_designation, sd = x40yd_sd_designation) *100), 0)) %>%
  # STEN by position
  mutate(weight_sten_position = (2*weight_z_position) + 5.5) %>%
  mutate(height_sten_position = (2*height_z_position) + 5.5) %>%
  
  mutate(vertical_sten_position = (2*vertical_z_position) + 5.5) %>%
  mutate(broad_jump_sten_position = (2*broad_jump_z_position) + 5.5) %>%
  mutate(bench_sten_position = (2*bench_z_position) + 5.5) %>%
  mutate(shuttle_sten_position = (2*shuttle_z_position) + 5.5) %>%
  mutate(x3cone_sten_position = (2*x3cone_z_position) + 5.5) %>%
  mutate(x40yd_sten_position = (2*x40yd_z_position) + 5.5) %>%
  # STEN all
  mutate(height_sten_all = (2*height_z_all) + 5.5) %>%
  mutate(weight_sten_all = (2*weight_z_all) + 5.5) %>%
  
  mutate(vertical_sten_all = (2*vertical_z_all) + 5.5) %>%
  mutate(broad_jump_sten_all = (2*broad_jump_z_all) + 5.5) %>%
  mutate(bench_sten_all = (2*bench_z_all) + 5.5) %>%
  mutate(shuttle_sten_all = (2*shuttle_z_all) + 5.5) %>%
  mutate(x3cone_sten_all = (2*x3cone_z_all) + 5.5) %>%
  mutate(x40yd_sten_all = (2*x40yd_z_all) + 5.5) %>%
  
  # STEN designation
  mutate(height_sten_designation = (2*height_z_designation) + 5.5) %>%
  mutate(weight_sten_designation = (2*weight_z_designation) + 5.5) %>%
  
  mutate(vertical_sten_designation = (2*vertical_z_designation) + 5.5) %>%
  mutate(broad_jump_sten_designation = (2*broad_jump_z_designation) + 5.5) %>%
  mutate(bench_sten_designation = (2*bench_z_designation) + 5.5) %>%
  mutate(shuttle_sten_designation = (2*shuttle_z_designation) + 5.5) %>%
  mutate(x3cone_sten_designation = (2*x3cone_z_designation) + 5.5) %>%
  mutate(x40yd_sten_designation = (2*x40yd_z_designation) + 5.5) %>%
  
  # t by position
  mutate(height_t_position = (10*height_z_position) + 50) %>%
  mutate(weight_t_position = (10*weight_z_position) + 50) %>%
  
  mutate(vertical_t_position = (10*vertical_z_position) + 50) %>%
  mutate(broad_jump_t_position = (10*broad_jump_z_position) + 50) %>%
  mutate(bench_t_position = (10*bench_z_position) + 50) %>%
  mutate(shuttle_t_position = (10*shuttle_z_position) + 50) %>%
  mutate(x3cone_t_position = (10*x3cone_z_position) + 50) %>%
  mutate(x40yd_t_position = (10*x40yd_z_position) + 50) %>%
  # t all
  mutate(height_t_all = (10*height_z_all) + 50) %>%
  mutate(weight_t_all = (10*weight_z_all) + 50) %>%
  
  mutate(vertical_t_all = (10*vertical_z_all) + 50) %>%
  mutate(broad_jump_t_all = (10*broad_jump_z_all) + 50) %>%
  mutate(bench_t_all = (10*bench_z_all) + 50) %>%
  mutate(shuttle_t_all = (10*shuttle_z_all) + 50) %>%
  mutate(x3cone_t_all = (10*x3cone_z_all) + 50) %>%
  mutate(x40yd_t_all = (10*x40yd_z_all) + 50) %>%
  
  
  # t designation
  mutate(height_t_designation = (10*height_z_designation) + 50) %>%
  mutate(weight_t_designation = (10*weight_z_designation) + 50) %>%
  
  mutate(vertical_t_designation = (10*vertical_z_designation) + 50) %>%
  mutate(broad_jump_t_designation = (10*broad_jump_z_designation) + 50) %>%
  mutate(bench_t_designation = (10*bench_z_designation) + 50) %>%
  mutate(shuttle_t_designation = (10*shuttle_z_designation) + 50) %>%
  mutate(x3cone_t_designation = (10*x3cone_z_designation) + 50) %>%
  mutate(x40yd_t_designation = (10*x40yd_z_designation) + 50) %>%
  
  mutate(img_player = paste0("<img src='", player_image, "' width=30px></img>")) %>%
  mutate(img_team = paste0("<img src='", logo_light, "' width=30px></img>")) %>%
  mutate(img_conference = paste0("<img src='", url_conference, "' width=30px></img>")) %>%
  
  arrange(desc(combine_year), draft_overall_pick) %>%
  mutate(player_id = paste0(player,"_", school)) %>%
  select(player_id, everything()) %>%
  ungroup() %>%
  # tss and tsa for zscore
  mutate(tss_z_position = rowMeans(cbind(height_z_position, weight_z_position), na.rm = TRUE)) %>%
  mutate(tsa_z_position = rowMeans(cbind(
    vertical_z_position, 
    broad_jump_z_position, 
    bench_z_position,
    x3cone_z_position,
    shuttle_z_position,
    x40yd_z_position), na.rm = TRUE)) %>%
  mutate(tss_z_all = rowMeans(cbind(height_z_all, weight_z_all), na.rm = TRUE)) %>%
  mutate(tsa_z_all = rowMeans(cbind(
    vertical_z_all, 
    broad_jump_z_all, 
    bench_z_all,
    x3cone_z_all,
    shuttle_z_all,
    x40yd_z_all), na.rm = TRUE)) %>%
  
  mutate(tss_z_designation = rowMeans(cbind(height_z_designation, weight_z_designation), na.rm = TRUE)) %>%
  mutate(tsa_z_designation = rowMeans(cbind(
    vertical_z_designation, 
    broad_jump_z_designation, 
    bench_z_designation,
    x3cone_z_designation,
    shuttle_z_designation,
    x40yd_z_designation), na.rm = TRUE)) %>%

# tss and tsa for percentile
  mutate(tss_percentile_position = rowMeans(cbind(height_percentile_position, weight_percentile_position), na.rm = TRUE)) %>%
  mutate(tsa_percentile_position = rowMeans(cbind(
    vertical_percentile_position, 
    broad_jump_percentile_position, 
    bench_percentile_position,
    x3cone_percentile_position,
    shuttle_percentile_position,
    x40yd_percentile_position), na.rm = TRUE)) %>%
  mutate(tss_percentile_all = rowMeans(cbind(height_percentile_all, weight_percentile_all), na.rm = TRUE)) %>%
  mutate(tsa_percentile_all = rowMeans(cbind(
    vertical_percentile_all, 
    broad_jump_percentile_all, 
    bench_percentile_all,
    x3cone_percentile_all,
    shuttle_percentile_all,
    x40yd_percentile_all), na.rm = TRUE)) %>%
  
  mutate(tss_percentile_designation = rowMeans(cbind(height_percentile_designation, weight_percentile_designation), na.rm = TRUE)) %>%
  mutate(tsa_percentile_designation = rowMeans(cbind(
    vertical_percentile_designation, 
    broad_jump_percentile_designation, 
    bench_percentile_designation,
    x3cone_percentile_designation,
    shuttle_percentile_designation,
    x40yd_percentile_designation), na.rm = TRUE)) %>%
  
  
  # tss and tsa for tscore
  mutate(tss_t_position = rowMeans(cbind(height_t_position, weight_t_position), na.rm = TRUE)) %>%
  mutate(tsa_t_position = rowMeans(cbind(
    vertical_t_position, 
    broad_jump_t_position, 
    bench_t_position,
    x3cone_t_position,
    shuttle_t_position,
    x40yd_t_position), na.rm = TRUE)) %>%
  mutate(tss_t_all = rowMeans(cbind(height_t_all, weight_t_all), na.rm = TRUE)) %>%
  mutate(tsa_t_all = rowMeans(cbind(
    vertical_t_all, 
    broad_jump_t_all, 
    bench_t_all,
    x3cone_t_all,
    shuttle_t_all,
    x40yd_t_all), na.rm = TRUE)) %>%
  
  mutate(tss_t_designation = rowMeans(cbind(height_t_designation, weight_t_designation), na.rm = TRUE)) %>%
  mutate(tsa_t_designation = rowMeans(cbind(
    vertical_t_designation, 
    broad_jump_t_designation, 
    bench_t_designation,
    x3cone_t_designation,
    shuttle_t_designation,
    x40yd_t_designation), na.rm = TRUE)) %>%
  
  # tss and tsa for sten
  mutate(tss_sten_position = rowMeans(cbind(height_sten_position, weight_sten_position), na.rm = TRUE)) %>%
  mutate(tsa_sten_position = rowMeans(cbind(
    vertical_sten_position, 
    broad_jump_sten_position, 
    bench_sten_position,
    x3cone_sten_position,
    shuttle_sten_position,
    x40yd_sten_position), na.rm = TRUE)) %>%
  mutate(tss_sten_all = rowMeans(cbind(height_sten_all, weight_sten_all), na.rm = TRUE)) %>%
  mutate(tsa_sten_all = rowMeans(cbind(
    vertical_sten_all, 
    broad_jump_sten_all, 
    bench_sten_all,
    x3cone_sten_all,
    shuttle_sten_all,
    x40yd_sten_all), na.rm = TRUE)) %>%
  
  mutate(tss_sten_designation = rowMeans(cbind(height_sten_designation, weight_sten_designation), na.rm = TRUE)) %>%
  mutate(tsa_sten_designation = rowMeans(cbind(
    vertical_sten_designation, 
    broad_jump_sten_designation, 
    bench_sten_designation,
    x3cone_sten_designation,
    shuttle_sten_designation,
    x40yd_sten_designation), na.rm = TRUE)) %>%
  
  mutate_all(~replace(., is.nan(.), NA)) 

#STEN = (2 Z) + 5.5
#T-score = (Z-score * 10) + 50 

