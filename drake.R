
library(drake)  # 6.1.0
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(tidyr)
library(lubridate)
library(kableExtra)
library(piecewiseSEM)
source('functions.R')
library(dplyr)   # needs to come after MASS above so select() isn't masked

OUTPUT_DIR		<- "outputs"
DATA_DIR <- 'data'

# make a drake plan
plan = drake_plan(
  # load data
  srdbv1 = read_file('srdb-data-v1.csv'), 
  srdbv2 = read_file('srdb-data-v2.csv'), 
  srdbv3 = read_file('srdb-data-v3.csv'), 
  srdbv4 = read_file('srdb-data-v4.csv'), 
  # srdbv5 = read_file('srdb-data-v5.csv'),
  srdbv5 = read.csv(file_in('../srdb/srdb-data.csv')),
  # srdbv5$Site_ID = as.factor(srdbv5$Site_ID),
  IGBP = read_file('IGBP.txt') %>% 
    left_join(read_file("igbp_mapping.csv"), by = "IGBP2001Pr"),
  # load global climate data (University of Delaware)
  GlobalMATMAP = read_file('summarized_climate.csv'),
  srdb_citation = read_xlsx('SRDB_cited.xlsx'),
  MGRsD = read.csv(file_in(!!file.path(DATA_DIR,'MGRsD.csv'))),
  srdb_study = read_file('srdb-studies.csv'),
  sub_srdbv5_meas_year_na = read_file ('sub_srdbv5_meas_year_na.csv'),
  rhchecked = read.csv('outputs/rhchecked.csv'),
  meas_time_mapping = read.csv('outputs/meas_time_mapping.csv'),
  
  # get MGRhD
  MGRhD_raw = get_mgrhd(MGRsD),
  sub_srdbv5_raw = get_subsrdbv5(srdbv5),
  sub_srdbv5 = bind_rows(sub_srdbv5_raw, sub_srdbv5_meas_year_na),
  ## Joint to SRDB-V5 and get meta data
  MGRhD = left_join(MGRhD_raw, sub_srdbv5, by = c("Study_number", "Site_ID", "Meas_Year")) 
)

make(plan)
