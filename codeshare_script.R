library(dataRetrieval)
library(data.table)
library(dplyr)
library(stringr)
library(maptools)
library(maps)
library(sp)
library(rgeos)
library(ggplot2)

################################
# Get NWIS Data
################################

sites <- fread("filtered_sites.csv", colClasses = "character")
aws_list <- fread("USGS_AHPS_station.csv")
priority_list <- readxl::read_xlsx("FPSSites20181023.xlsx")
ahps_forecast <- readRDS('ahps_max_exceeded.rds')
aws_list <- left_join(aws_list, ahps_forecast, by = c(lid = "ahps_id"))

peak_flow <- readRDS('max_flows.rds')
siteInfo <- readNWISsite(sites$siteID)
moreSiteInfo <- whatNWISdata(siteNumber = sites$siteID)

siteInfoTots <- select(siteInfo, site_no, dec_lat_va, dec_long_va, state_cd) 

siteInfoTots <- siteInfoTots %>%
  left_join(select(peak_flow, site_no=site, max_flow), by="site_no") %>% 
  left_join(aws_list, by = c(site_no = "gage_id"))

nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
nws_flood_stage_table <- nws_flood_stage_list[["sites"]]

siteInfoTots$NWS <- siteInfoTots$site_no %in% nws_flood_stage_table$site_no
siteInfoTots$priority <- siteInfoTots$site_no %in% priority_list$SiteNumber

siteInfo <- siteInfoTots

################################
# Setup Map
################################\
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

to_sp <- function(...){
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

shift_sp <- function(sp, scale, shift, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}

# thanks to Bob Rudis (hrbrmstr):
# https://github.com/hrbrmstr/rd3albers

# -- if moving any more states, do it here: --
move_variables <- list(
  AK = list(scale=0.33, shift = c(80,-450), rotate=-50),
  HI = list(scale=1, shift=c(520, -110), rotate=-35)#,
  # PR = list(scale=2.5, shift = c(-140, 90), rotate=20)
)

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii")#,
  # PR = to_sp("world", "Puerto Rico")
)

conus <- to_sp('state')
states.out <- conus
wgs84 <- "+init=epsg:4326"
coords = cbind(siteInfo$dec_long_va, siteInfo$dec_lat_va)
sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>%
  spTransform(CRS(proj4string(states.out)))

sites.df <- as.data.frame(sites) %>% bind_cols(siteInfo) 

siteInfo$state <- dataRetrieval::stateCdLookup(siteInfo$state_cd, "postal")

for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]],
                                 move_variables[[i]],
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  shifted.sites <- do.call(shift_sp, c(sp = sites[siteInfo$state == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()
  
  sites.df[siteInfo$state == i, c("coords.x1", "coords.x2")] <- shifted.sites
  
}

sites.df$NWS <- siteInfo$NWS
sites.df$priority <- siteInfo$priority
sites.df$NWS <- ifelse(sites.df$NWS, "AHPS site (343)",
                       "Non-AHPS site (499)")
sites.df$priority <- ifelse(sites.df$priority, "Federal Priority",
                            "Other")
#filter(sites.df, NWS == "AHPS site (343)") #some sites were incorrectly removed here
#not sure why they were not considered AHPS
#does result in two duplicates though
sites.df <-  sites.df %>% filter(!is.na(max_stage_exceeded) & max_stage_exceeded != "no forecast") %>% 
  mutate(max_stage_exceeded = ifelse(max_stage_exceeded %in% c("none", "low"), 
                                     yes = "none/low", no = max_stage_exceeded)) %>% distinct()

################################
# Get NWM Max Flows
################################

# sbtools::authenticate_sb()
# 
# sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", names = "max_flows.rds", 
#                             destinations = "max_flows.rds", overwrite_file = TRUE)

site_nwm_max_flows <- readRDS("max_flows.rds")

################################
# Get QPF data
################################

# sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", names = "qpf.rds", destinations = "qpf.rds", overwrite_file = TRUE)
# 
# qpf <- readRDS("qpf.rds")

################################
# Clip QPF to CONUS
################################

#qpf <- sf::st_intersection(qpf, sf::st_buffer(sf::st_as_sf(conus), 0))

################################
# Plot it up
################################

gsMap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey90",
               alpha = 0.9, color = "grey") +
  #geom_sf(aes(fill = QPF), data = qpf, alpha = 0.5, lwd = 0) +
  #scale_fill_gradient("72-hr QPF [in]", low = "#f2f2f2", high = "#4186f4") +
  #coord_sf(datum=NA) +
  #geom_polygon(aes(x = long, y = lat, group = group),
  #             data = states.out, fill = NA,
  #             alpha = 0.9, color = "grey") +
  geom_point(data = sites.df, size = 2,
             aes(x = coords.x1, y=coords.x2, shape = priority, 
                 color = max_stage_exceeded))  + 
  scale_color_manual(values = c(`no forecast` = "grey", `none/low` = "green", action = "blue", 
                                bankfull = "yellow", flood = "darkorange", major = "red"))  +
  scale_shape_manual(values=c(`Federal Priority` = 17, Other = 16)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggtitle(label = "Streamgage Outage Summary 2018-10-23", subtitle = "AHPS with forecasts as of 10-23 afternoon") +
  guides(shape = guide_legend(title=NULL, order = 2), 
         color = guide_legend(title="Maximum forecast stage", order = 1))

gsMap
ggsave(gsMap, filename = "site_outages4.pdf", width = 11, height = 7)
ggsave(gsMap, filename = "site_outages3.png", width = 11, height = 7)
