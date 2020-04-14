
#*****************************************************************************************************************
# load file functions 
#*****************************************************************************************************************

# input csv data
read_file <- function(x) read.csv(file.path(DATA_DIR, x), comment.char = "#", stringsAsFactors = FALSE)
# input xlsx data
# don't know why this function not work
# read_xlsx <- function(x) read.xlsx2(file.path(DATA_DIR, x), sheetIndex = 1, colIndex = c(1:4)) 
read_xlsx <- function(x) read_excel(file.path(DATA_DIR, x), sheet = 1)
# write csv 
writ_file <- function(input, output) write.csv(input, file.path(OUT_DIR, output), row.names = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))


#*****************************************************************************************************************
# data processing functions
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

# functions for ESSD-SRDBV5 manuscript
srdb_v_comp <- function(srdb_v_mamt){
  srdb_v_mamt %>% 
    filter(!is.na(Ecosystem) & !is.na(MAT)) %>% 
    ggplot(aes(x = Ecosystem, y = MAT, fill = factor(Source))) +
    theme(legend.position = "none"
          , axis.title.x=element_blank()
          , axis.text.x = element_text(angle = 25, hjust = 1)) +
    # geom_violin(draw_quantiles = c(0.5))
    geom_boxplot(outlier.shape = 1, outlier.color = "gray", outlier.size = 0.5) +
    labs(x=expression(Ecosystem~types), y=expression(MAT~(degree~C)))
}

# functions for SRDB and MGRsD data quality check
# Select sites with more than 10 months' Rs measurement, and get the mean
srdb_vs_mgrsd <- function () {
  MGRsD %>% select(Study_number, Site_ID, Meas_Month, Rs_Norm) %>% 
    group_by(Study_number, Site_ID, Meas_Month) %>% 
    summarise(RS_mean = mean(Rs_Norm)) %>% 
    mutate(n_Month = n()) %>% 
    filter(n_Month >= 10) %>% 
    group_by(Study_number, Site_ID) %>% 
    summarise(RS_annual_mgrsd = mean(RS_mean)) -> 
    mgrsd_sum
  
  # Select sites and get the mean Rs value from srdb data
  srdbv5 %>% 
    select(Study_number, Site_ID, Rs_annual) %>% 
    na.omit() %>% 
    group_by(Study_number, Site_ID) %>% 
    summarise(RS_annual_srdb = mean(Rs_annual)/365) -> 
    srdb_sum
  
  # use inner join to combine Rs from srdb and mgrsd  
  results <- inner_join(mgrsd_sum, srdb_sum, by=c("Study_number", "Site_ID"))
  return(results)
}



