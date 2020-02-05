
#*****************************************************************************************************************
# functions used in this study
#*****************************************************************************************************************

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

# find out studies in MGRsD but not in SRDB
mgrsd_notin_srdb <- function (sdata = srdb, sdata2 = mgrsd) {
  sdata %>% select(Study_number) %>% unique() %>% mutate(srdb_ID = paste(Study_number, "srdb", sep = "-")) -> srdb_study
  sdata2 %>% select(Study_number) %>% unique() -> mgrsd_study
  
  results <- mgrsd_study %>% filter(Study_number %!in% srdb_study$Study_number)
  # results <- left_join(mgrsd_study, srdb_study, by = c("Study_number") )
  return (results)
}

# find out studies in SRDB but not in MGRsD
srdb_notin_mgrsd <- function (sdata = srdb, sdata2 = mgrsd) {
  sdata %>% select(Study_number) %>% unique() -> srdb_study
  sdata2 %>% select(Study_number) %>% unique() -> mgrsd_study
  
  results <- srdb_study %>% filter(Study_number %!in% mgrsd_study$Study_number) 
  return (results)
}

# Find out those Site_ID in MGRsD but not in SRDB
site_ID_check <- function (sdata = srdb) {
  sdata %>% select(Study_number, Site_ID) %>% unique() %>% 
    mutate(srdb_ID = paste(Study_number, Site_ID, sep = "-")) -> srdb_SID
  MGRsD %>% select(Study_number, Site_ID) %>% unique() -> mgrsd_SID
  results <- left_join(mgrsd_SID, srdb_SID, by = c("Study_number", "Site_ID"))
  return (results)
}

# check sites by contry (check latitude and longitude)
lot_long_check <- function (sdata, ct) {
  sdata %>% filter(Country == ct) %>% select(Longitude, Latitude) %>% na.omit() %>% unique() -> map
  # coordinates(map) <- ~Longitude + Latitude 
  leaflet(map) %>% 
    addMarkers() %>% 
    addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 4))
}

# function prepare data for representative analysis from srdb-v1 to srdb-v5
representative <- function (srdb_data, igbp_data, mat_map_data) {
  # Change Latitude and Longitude to 0.5 degree resolution for SRDB,
  # so srdb can link to IGBP and climate data through latitude and longitude (in 0.5 degree solution)
  srdb_data %>% mutate(Latitude = round(Latitude*2)/2+0.25, Longitude = round(Longitude*2)/2+0.25, Source = "SRDB") %>% 
    select(Latitude, Longitude, Source) -> 
    sub_srdb
  
  # Change Latitude and Longitude to 0.5 resolution for IGBP
  igbp_data %>% mutate(Latitude = round(Latitude*4)/4, Longitude = round(Longitude*4)/4) %>% 
    select(Latitude, Longitude, Ecosystem) ->
    sub_IGBP
  
  # Get Ecosystem class, MAT and MAP for srdb data
  left_join(sub_srdb, sub_IGBP, by=c("Latitude", "Longitude")) -> sub_srdb
  left_join(sub_srdb, mat_map_data, by=c("Latitude", "Longitude")) -> sub_srdb
  
  # Get IGBP for global climate data
  left_join(mat_map_data, sub_IGBP, by=c("Latitude", "Longitude")) -> mat_map_data
  mat_map_data %>% mutate(Source = "Global") -> mat_map_data
  
  # combine data from different resources into one dataset
  bind_rows(sub_srdb, mat_map_data) -> rep_data
  return(rep_data)
}

