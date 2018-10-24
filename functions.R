#pull and parse NWS xml
library(httr)
parse_ahps_site <- function(ahps_id) {
  url.site <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=",ahps_id,"&output=xml")
  print(ahps_id)
  return_list <- GET(url.site)
  returnedDoc <- content(return_list,encoding = "UTF-8")
  nws.site <- xml_root(returnedDoc)
  sigstages <- xml_find_all(nws.site, "sigstages")
  if(length(sigstages) > 0) {
    sigstages_children <- xml_children(sigstages)
    names <- xml_name(sigstages_children)
    units <- xml_attr(sigstages_children, "units")
    values <- xml_text(sigstages_children)
    sigstages_df <- tibble(stage_name = names, 
                           unit = units, 
                           value = as.numeric(values)) %>% 
      arrange(values)
    if(length(units > 0) && !all(is.na(units))){
      assert_that(length(unique(na.omit(units))) == 1)
    }
  }
  
  forecast <- xml_find_all(nws.site, "forecast")
  forecast_children <- xml_children(forecast)
  if(length(forecast_children) == 0) {
    #not all sites have forecasts
    message("No forecast for ", ahps_id)
    return(tibble(ahps_id = ahps_id, max_stage_exceeded = "no forecast"))
  } else {
    assert_that(length(sigstages) > 0, 
                msg = sprintf("There was a forecast, but no sigstages for %s", ahps_id))
    time_nodes <- xml_find_all(forecast_children, "valid")
    value_nodes <- xml_find_all(forecast_children, "primary")
    if(!all(xml_attr(value_nodes, "name") == "Stage")) {
      value_nodes <- xml_find_all(forecast_children, "secondary")
    }
    assert_that(all(xml_attr(value_nodes, "name") == "Stage"))
    
    value_units <- xml_attr(value_nodes, "units")
    assert_that(length(unique(value_units)) == 1)
    forecast_site <- tibble(dateTime = xml_text(time_nodes),
                            tz = xml_attr(time_nodes, "timezone"),
                            forecast_vals = as.numeric(xml_text(value_nodes)),
                            units = value_units
    )
    #check units against sigstages
    assert_that(unique(na.omit(sigstages_df$unit)) == unique(na.omit(forecast_site$units)))
    #where does max forecast val fall
    max_forecast <- max(forecast_site$forecast_vals, na.rm = TRUE)
    #sorted sigstages above, so can assume they are in increasing value
    max_stage_exceeded <- sigstages_df %>% mutate(max_exceeds_this = max_forecast > value) %>% 
      filter(max_exceeds_this) %>% tail(n=1) %>% pull(stage_name)
    max_stage_exceeded <- ifelse(length(max_stage_exceeded) == 0, 
                                 yes = "none", no = max_stage_exceeded)
    return_tibble <- tibble(max_stage_exceeded = max_stage_exceeded,
                            ahps_id = ahps_id)
    print(return_tibble)
    return(return_tibble)
  }
}