
library(drake)  # 6.1.0
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(tidyr)
library(lubridate)
library(kableExtra)
library(piecewiseSEM)
source('functions.R')
library(dplyr)   # needs to come after MASS above so select() isn't masked
library(raster)

OUTPUT_DIR		<- "outputs"
DATA_DIR <- 'data'

#*****************************************************************************************************************
# functions 
#*****************************************************************************************************************
load_barren <- function(f){
  MODIS_barren <- raster(f)
  MODIS_barren <- rasterToPoints(MODIS_barren)
  MODIS_barren <- as.data.frame(MODIS_barren)
  colnames(MODIS_barren) <- c("lon", "lat", "barren_yn")
  return(MODIS_barren)
}

get_veg_barren_koppen <- function(){
  # srdbv5$Site_ID = as.factor(srdbv5$Site_ID),
  igbp = read_file('IGBP.txt') %>% 
    left_join(read_file("igbp_mapping.csv"), by = "IGBP2001Pr")
  # get landuse data and remove barren region in dasert and arctic
  modis_barren = load_barren('data/MODIS_Barren_2010.tif')
  # load climate koppen
  koppen = read_xlsx('KoeppenGeigerASCII.xlsx')
  # join and get barren, climate koppen information
  IGBP = left_join(igbp, modis_barren, by = c("Latitude" = "lat", "Longitude" = "lon"))
  IGBP = left_join(IGBP, koppen, by = c("Latitude", "Longitude"))
  # seperate forest into tropical forest and non-tropical forest
  IGBP %>% 
    mutate(Ecosystem2 = case_when(
      Ecosystem == "Agriculture" ~ "Agriculture",
      Ecosystem == "Arctic" ~ "Arctic",
      Ecosystem == "Desert" ~ "Desert",
      Ecosystem == "Forest" & ClimateTypes %in% c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk") ~ "Tropic FOR",
      Ecosystem == "Forest" & ClimateTypes %!in% c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk") ~ "T&B FOR",
      Ecosystem == "Grassland" ~ "Grassland",
      Ecosystem == "Savanna" ~ "Savanna",
      Ecosystem == "Shrubland" ~ "Shrubland",
      Ecosystem == "Urban" ~ "Urban",
      Ecosystem == "Wetland" ~ "Wetland",
      TRUE ~ "Other")) ->
    IGBP
}

## Filter MGRsD and get MRGhD
get_mgrhd <- function(sdata) {
  sdata %>% 
    dplyr::select(1:22) %>% 
    dplyr::select(-Rs_Paper, -Rh_Paper, -Ra_Paper, -Rs_units, -Converter) %>% 
    filter(!is.na(Rh_Norm) | !is.na(Ra_Norm)) ->
    MGRhD
  # MGRhD %>% filter(is.na(Rh_Norm)) %>% nrow()
  MGRhD$Rh_Norm <- ifelse(is.na(MGRhD$Rh_Norm), MGRhD$Rs_Norm - MGRhD$Ra_Norm, MGRhD$Rh_Norm)
  # MGRhD %>% filter(is.na(Ra_Norm)) %>% nrow()
  MGRhD$Ra_Norm <- ifelse(is.na(MGRhD$Ra_Norm), MGRhD$Rs_Norm - MGRhD$Rh_Norm, MGRhD$Ra_Norm)
  MGRhD$Site_ID <- as.character(MGRhD$Site_ID)
  # get Meas_Month based on DOY
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
    dplyr::select(Study_number, Site_ID, Study_midyear, Latitude, Longitude, Elevation, Manipulation, Biome, Ecosystem_type,Leaf_habit,
           Soil_type, Soil_BD, Soil_CN, Soil_sand, Soil_silt, Soil_clay, MAT, MAP, PET, Study_temp, Study_precip, Meas_method,
           Collar_height, Collar_depth, Chamber_area, Time_of_day, Meas_interval, Annual_coverage, Partition_method, Rs_annual, Ra_annual,
           Rh_annual, RC_annual,C_soilmineral ) %>% 
    mutate(Meas_Year = floor(Study_midyear)) -> sub_srdbv5
  # change site_ID to character
  sub_srdbv5$Site_ID <- as.character(sub_srdbv5$Site_ID)
  return(sub_srdbv5)
}

#*****************************************************************************************************************
# make a drake plan 
#*****************************************************************************************************************
plan = drake_plan(
  # load data
  srdbv1 = read_file('srdb-data-v1.csv'), 
  srdbv2 = read_file('srdb-data-v2.csv'), 
  srdbv3 = read_file('srdb-data-v3.csv'), 
  srdbv4 = read_file('srdb-data-v4.csv'), 
  # srdbv5 = read_file('srdb-data-v5.csv'),
  srdbv5 = read.csv(file_in('../srdb/srdb-data.csv')),
  # load global climate data (University of Delaware)
  GlobalMATMAP = read_file('summarized_climate.csv'),
  srdb_citation = read_xlsx('SRDB_cited.xlsx'),
  MGRsD = read.csv(file_in(!!file.path(DATA_DIR,'MGRsD.csv'))),
  srdb_study = read_file('srdb-studies.csv'),
  # some study_year_siteID not in srdb, which are included in sub_srdbv5_meas_year_na
  sub_srdbv5_meas_year_na = read_file ('sub_srdbv5_meas_year_na.csv'),
  rhchecked = read.csv('outputs/rhchecked.csv'),
  meas_time_mapping = read.csv('outputs/meas_time_mapping.csv'),
  
  # get MGRhD
  MGRhD_raw = get_mgrhd(MGRsD),
  sub_srdbv5_raw = get_subsrdbv5(srdbv5),
  # some study_year_siteID not in srdb, which are included in sub_srdbv5_meas_year_na
  sub_srdbv5 = bind_rows(sub_srdbv5_raw, sub_srdbv5_meas_year_na), 
  ## Joint to SRDB-V5 and get meta data
  MGRhD = left_join(MGRhD_raw, sub_srdbv5, by = c("Study_number", "Site_ID", "Meas_Year")),
  
  # get IGBP, MODIS barren, and climate koppen
  IGBP = get_veg_barren_koppen()
)

make(plan)
