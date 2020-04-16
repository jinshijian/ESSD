
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
# data quality check functions
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

# function for plot word background
word_bkgd <- function (sdata) {
  ggplot(data = sdata) + 
    # geom_polygon(aes(x = long, y = lat , fill = region , group = group, alpha = 0.1), color = "white") + 
    geom_polygon(aes(x = long, y = lat, group = group, alpha = 0.1), color = "white", fill = "gray") + 
    coord_fixed(1.3) +
    theme(axis.text.y   = element_text(size = 12),
          axis.text.x   = element_text(size = 12),
          axis.title.y   = element_text(size = 13, margin = margin(t = 0, r = 12, b = 0, l = 0)),
          axis.title.x   = element_text(size = 13, margin = margin(t = 12, r = 0, b = 0, l = 0)),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1.25))+
    theme(legend.position = "none")+
    scale_x_continuous(name = "Longitude", breaks = seq(-180, 180, 30),
                       labels = seq(-180, 180, 30)) +
    scale_y_continuous(name = "Latitude", limits = c(-60, 90), breaks = seq(-90, 90, 15),
                       labels = seq(-90,90,15))
}

# make global Rh spatial distribution figures
mgrhd_site <- function(sdata){
  worldMap <- map_data(map = "world")
  basemap <- word_bkgd(worldMap)
  
  # filter and get those sites with rh measurements
  sdata %>% 
    select(Latitude, Longitude, Partition_method, Rh_annual, Ra_annual) %>% 
    filter(!is.na(Rh_annual) | !is.na(Ra_annual)) %>% 
    filter(Partition_method != "") ->
    srdbv5_rh
  
  sdata %>% 
    select(Study_number, Latitude, Longitude, Partition_method, Rh_annual) %>% 
    filter(Partition_method != "" & is.na(Rh_annual)) ->
    srdbv5_partition
  
  # create legend
  cc_legend <- tibble(
    x = rep(-177, 2),
    y = c(-20, -41),
    size = c(1, 1)
  )
  
  rhmap <- basemap +
    geom_point(data = srdbv5_rh, aes(x = Longitude, y = Latitude), 
               color = "black", shape = 1, size = 2.5, alpha = 0.5) +
    geom_point(data = srdbv5_partition, aes(x = Longitude, y = Latitude), 
               color = "red", shape = 3, size = 3, alpha = 0.5) +
    # legend
    geom_point(
      data = cc_legend, aes(x, y, size = size), shape = c(1, 3),
      color = c("black", "red"), alpha = c(1, 1)
    ) +
    annotate("text", x = -165, y = -25, label = paste0("Annual coverage = 1\n(n = ", nrow(srdbv5_rh), ")")
             , size = 3.5, hjust = 0) +
    annotate("text", x = -165, y = -45, label = paste0("Annual coverage < 1\n(n = ", nrow(srdbv5_partition),")")
             , size = 3.5, hjust = 0)
  
  print(rhmap)
}

#*****************************************************************************************************************
# data processing functions
#*****************************************************************************************************************
## Filter MGRsD and get MRGhD
get_mgrhd <- function(sdata) {
  sdata %>% 
    select(1:22) %>% 
    select(-Rs_Paper, -Rh_Paper, -Ra_Paper, -Rs_units, -Converter) %>% 
    filter(!is.na(Rh_Norm) | !is.na(Ra_Norm)) ->
    MGRhD
  
  # MGRhD %>% filter(is.na(Rh_Norm)) %>% nrow()
  MGRhD$Rh_Norm <- ifelse(is.na(MGRhD$Rh_Norm), MGRhD$Rs_Norm - MGRhD$Ra_Norm, MGRhD$Rh_Norm)
  
  # MGRhD %>% filter(is.na(Ra_Norm)) %>% nrow()
  MGRhD$Ra_Norm <- ifelse(is.na(MGRhD$Ra_Norm), MGRhD$Rs_Norm - MGRhD$Rh_Norm, MGRhD$Ra_Norm)
  MGRhD$Site_ID <- as.character(MGRhD$Site_ID)
  
  MGRhD %>% 
    mutate(
      Meas_Month2 = case_when(
        Meas_DOY %in% (0:31) ~ 1,
        Meas_DOY %in% (32:59) ~ 2,
        Meas_DOY %in% (60:90) ~ 3,
        Meas_DOY %in% (91:120) ~ 4,
        Meas_DOY %in% (121:151) ~ 5,
        Meas_DOY %in% (152:181) ~ 6,
        Meas_DOY %in% (182:212) ~ 7,
        Meas_DOY %in% (213:243) ~ 8,
        Meas_DOY %in% (244:273) ~ 9,
        Meas_DOY %in% (274:304) ~ 10,
        Meas_DOY %in% (305:334) ~ 11,
        Meas_DOY %in% (335:365) ~ 12
      )
    ) ->
    MGRhD
  return(MGRhD)
}

## Filter srdbv5 and get sub_srdbv5
# colnames(srdbv5)
get_subsrdbv5 <- function(sdata) {
  sdata %>% 
    select(Study_number, Site_ID, Study_midyear, Latitude, Longitude, Elevation, Manipulation, Biome, Ecosystem_type,Leaf_habit,
           Soil_type, Soil_BD, Soil_CN, Soil_sand, Soil_silt, Soil_clay, MAT, MAP, PET, Study_temp, Study_precip, Meas_method,
           Collar_height, Collar_depth, Chamber_area, Time_of_day, Meas_interval, Annual_coverage, Partition_method, Rs_annual, Ra_annual,
           Rh_annual, RC_annual,C_soilmineral ) %>% 
    mutate(Meas_Year = floor(Study_midyear)) -> sub_srdbv5
  
  sub_srdbv5$Site_ID <- as.character(sub_srdbv5$Site_ID)
  return(sub_srdbv5)
}


