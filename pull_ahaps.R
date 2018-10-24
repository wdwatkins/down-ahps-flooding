library(tidyverse)
library(xml2)
library(assertthat)

nwis_ahps_crosswalk <- read_csv('USGS_AHPS_station.csv', col_types = "ccccc")
down_nwis_sites <- read_csv('filtered_sites.csv')

down_ahps_sites <- inner_join(down_nwis_sites, nwis_ahps_crosswalk, by = c(siteID = "gage_id")) %>% 
  filter(!duplicated(siteID))
write_csv(x = down_ahps_sites, path = "down_ahps_sites.csv")
fed_priority_sites <- readxl::read_excel('FPSSites20181023.xlsx')

down_ahps_sites_fps <- left_join(down_ahps_sites, fed_priority_sites, by = c("siteID" = "SiteNumber"))
sum(!is.na(down_ahps_sites_fps$Score)) #down fed priority sites

#### now download, whether Fed priority sites or not

source('functions.R')
max_exceeded_df <- tibble()
for(site in down_ahps_sites$lid) {
  site_df <- parse_ahps_site(site)
  max_exceeded_df <- bind_rows(max_exceeded_df, site_df)
}
saveRDS(max_exceeded_df, "ahps_max_exceeded.rds")
with_forecasts <- max_exceeded_df %>% filter(max_stage_exceeded != "no forecast")
write_csv(with_forecasts, path = "down_ahps_with_forecasts.csv")
down_ahps_max_exceeded <- left_join(down_ahps_sites_fps, max_exceeded_df, by = c(lid = "ahps_id"))

final_df <- down_ahps_max_exceeded %>% 
  filter(!max_stage_exceeded %in% c("no forecast", "none", "low")) %>% 
  select(name = V1, max_stage_exceeded, siteID)
write_csv(final_df, path = "AHPS_forecast_action_or_higher_20181023.csv")
final_df_all_forecasts <- down_ahps_max_exceeded %>% 
  filter(!max_stage_exceeded %in% c("no forecast")) %>% 
  select(name = V1, max_stage_exceeded, siteID) 
write_csv(final_df_all_forecasts, path = "AHPS_all_forecasts_20181023.csv")
