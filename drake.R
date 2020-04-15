
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
  
)