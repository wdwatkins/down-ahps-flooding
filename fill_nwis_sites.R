library(googlesheets)
library(dplyr)
token <- gs_auth(cache = FALSE)
title <- gs_title("Copy of USGS_AHPS_Gauge_Outage_List_Ranked")
priority_df <- gs_read(title)

ahps_nwis_crosswalk <- data.table::fread('USGS_AHPS_station.csv') %>% 
  select(gage_id, lid) %>% filter(!(gage_id == "03250322" & lid == "SRKK2"))

priority_df_joined <- left_join(priority_df, ahps_nwis_crosswalk, 
                                by = c(NWSLI = "lid"))
data.table::fwrite(priority_df_joined, file = "priority_sites_joined.csv")
no_fixed_id <- priority_df_joined %>% filter(is.na(fixed_id))
