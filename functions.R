
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
# two axises
scaleFUN <- function(x) sprintf("%.0f", x)

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
    select(Latitude, Longitude, Ecosystem, Ecosystem2) ->
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
  # Studies with Rh and Ra partition method not null
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
  # plot map
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
  # print map
  print(rhmap)
}

#*****************************************************************************************************************
# Collar depth and measure time test functions
#*****************************************************************************************************************
# pass column name to a function
# test_function <- function(sdata, colName, value){
#   # another way is using
#   # myenc <- enquo(colName)
#   sdata %>%
#     select(Rs_annual, {{colName}}) %>%
#     filter(Rs_annual < 5000 & {{colName}} < value) %>%
#     group_by({{colName}}) %>%
#     summarise(Rs_annual = mean(Rs_annual),
#               obs = n()) %>%
#     mutate(obs2 = ifelse(obs <= 100, obs, 110)) ->
#     sdata2
#   # return(sdata2)
#   return(colnames(sdata2))
# }
# test_function(srdbv5, Collar_height, 50)

time_depth_test <- function(sdata, colName, threshold){
  x_axis_id <- c("Collar_height", "Chamber_area", "Meas_interval", "hour_cover")
  # prepare data
  sdata %>% 
    select(Rs_annual, {{colName}}) %>% 
    filter(Rs_annual < 5000 & {{colName}} < threshold) %>% 
    group_by({{colName}}) %>% 
    summarise(Rs_annual = mean(Rs_annual),
              obs = n()) %>% 
    mutate(obs2 = ifelse(obs <= 250, obs, 250)) ->
    sdata2
  # density 1 panel
  sdata2 %>% 
    ggplot(aes({{colName}})) +
    geom_histogram(bins = 50, fill = "white", col = "black") +
    theme_void() -> 
    dens1
  # density 2 panel
  sdata2 %>% 
    ggplot(aes(Rs_annual)) +
    geom_histogram(bins = 30, fill = "white", col = "black") +
    theme_void() +
    coord_flip() -> 
    dens2
  # main plot
  sdata2 %>%
    ggplot(aes({{colName}}, Rs_annual)) +
    geom_point(aes(size  = obs2), shape = 16, alpha = 0.5) +
    geom_smooth(method = "lm",
                fill = "skyblue") +
    labs(x = if(colnames(sdata2)[1]==x_axis_id[1]) {
      expression(Collar~height~(cm)) } 
      else if (colnames(sdata2)[1]==x_axis_id[2]) {
        expression(Collar~area~(cm^{-2})) }
      else if (colnames(sdata2)[1]==x_axis_id[3]) {
        expression(Measure~interval~(day)) }
      else {
        expression(Hours~covered~(h)) }, 
      
         y = expression(R[S]~(g~C~m^{-2}~yr^{-1}))) +
    labs(size="obs (n)") +
    theme(legend.position = c(0.7, 0.65),
          legend.background = element_rect(colour = NA, fill = NA)) ->
    plot1
  # output
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(
      ncol = 2, 
      nrow = 2, 
      widths = c(4, 1),
      heights = c(1, 4) ) ->
    final_plot
  print(final_plot)
}

# function for meas_interval_test plot
meas_interval_test <- function(sdata, colName, threshold){
  x_axis_id <- c("Collar_height", "Chamber_area", "Meas_interval", "hour_cover")
  # prepare data
  sdata %>% 
    select(Rs_annual, {{colName}}) %>% 
    filter(Rs_annual < 5000 & {{colName}} < threshold) %>% 
    group_by({{colName}}) %>% 
    summarise(Rs_annual = mean(Rs_annual),
              obs = n()) %>% 
    mutate(obs2 = ifelse(obs <= 250, obs, 250)) ->
    sdata2
  # density 1 panel
  sdata2 %>% 
    ggplot(aes({{colName}})) +
    geom_histogram(bins = 50, fill = "white", col = "black") +
    theme_void() -> 
    dens1
  # density 2 panel
  sdata2 %>% 
    ggplot(aes(Rs_annual)) +
    geom_histogram(bins = 30, fill = "white", col = "black") +
    theme_void() +
    coord_flip() -> 
    dens2
  # main plot
  sdata2 %>%
    ggplot(aes({{colName}}, Rs_annual)) +
    geom_point(aes(size  = obs2), shape = 16, alpha = 0.5) +
    # geom_smooth(method = "lm",
    #             fill = "skyblue") +
    labs(x = if(colnames(sdata2)[1]==x_axis_id[1]) {
      expression(Collar~height~(cm)) } 
      else if (colnames(sdata2)[1]==x_axis_id[2]) {
        expression(Collar~area~(cm^{-2})) }
      else if (colnames(sdata2)[1]==x_axis_id[3]) {
        expression(Measure~interval~(day)) }
      else {
        expression(Hours~covered~(h)) }, 
      
      y = expression(R[S]~(g~C~m^{-2}~yr^{-1}))) +
    labs(size="obs (n)") +
    theme(legend.position = c(0.7, 0.65),
          legend.background = element_rect(colour = NA, fill = NA)) ->
    plot1
  # output
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(
      ncol = 2, 
      nrow = 2, 
      widths = c(4, 1),
      heights = c(1, 4) ) ->
    final_plot
  print(final_plot)
}

hour_cover_test <- function(sdata, colName){
  x_axis_id <- c("Collar_height", "Chamber_area", "Meas_interval", "hour_cover")
  # prepare data
  sdata %>% 
    select(Rs_annual, {{colName}}) %>% 
    filter(Rs_annual < 5000 & !is.na({{colName}})) %>% 
    group_by({{colName}}) %>% 
    summarise(Rs_annual = mean(Rs_annual),
              obs = n()) %>% 
    mutate(obs2 = ifelse(obs <= 250, obs, 250)) ->
    sdata2
  # density 1 panel
  sdata %>% 
    filter(!is.na({{colName}})) %>% 
    ggplot(aes({{colName}})) +
    geom_histogram(bins = 50, fill = "white", col = "black") +
    theme_void() -> 
    dens1
  # density 2 panel
  sdata2 %>% 
    ggplot(aes(Rs_annual)) +
    geom_histogram(bins = 30, fill = "white", col = "black") +
    theme_void() +
    coord_flip() -> 
    dens2
  # main plot
  sdata2 %>%
    ggplot(aes({{colName}}, Rs_annual)) +
    geom_point(aes(size  = obs2), shape = 16, alpha = 0.5) +
    # geom_smooth(method = "lm",
    #             fill = "skyblue") +
    labs(x = if(colnames(sdata2)[1]==x_axis_id[1]) {
      expression(Collar~length~(cm)) } 
      else if (colnames(sdata2)[1]==x_axis_id[2]) {
        expression(Chamber~area~(cm^{-2})) }
      else if (colnames(sdata2)[1]==x_axis_id[3]) {
        expression(Measure~interval~(day)) }
      else {
        expression(Diurnal~coverage~(h)) }, 
      
      y = expression(R[S]~(g~C~m^{-2}~yr^{-1}))) +
    labs(size="obs (n)") +
    theme(legend.position = c(0.7, 0.65),
          legend.background = element_rect(colour = NA, fill = NA)) ->
    plot1
  # output
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(
      ncol = 2, 
      nrow = 2, 
      widths = c(4, 1),
      heights = c(1, 4) ) ->
    final_plot
  print(final_plot)
}

#*****************************************************************************************************************
# SLR test for collar and measure time
SLR_sum <- function(sdata){
  sdata %>% 
    select(Rs_annual, Collar_height) %>%
    filter(Rs_annual < 5000 & Collar_height < 50) %>% 
    group_by(Collar_height) %>% 
    summarise(Rs_annual = mean(Rs_annual), obs = n()) ->
    agg_srdb
  
  lm(agg_srdb$Rs_annual ~ agg_srdb$Collar_height, weights = agg_srdb$obs) ->
    lm_height
  
  tibble(Model = "Collar_height",
         Intercept = summary(lm_height)$coefficients[1,1],
         Slope = summary(lm_height)$coefficients[2,1],
         p_slope = summary(lm_height)$coefficients[2,4]) ->
    model_height_out
  
  # test collar area
  sdata %>% 
    select(Rs_annual, Chamber_area) %>%
    filter(Rs_annual < 5000 & Chamber_area < 4000) %>% 
    group_by(Chamber_area) %>% 
    summarise(Rs_annual = mean(Rs_annual),  obs = n()) ->
    agg_srdb
  
  lm(agg_srdb$Rs_annual ~ agg_srdb$Chamber_area, weights = agg_srdb$obs) ->
    lm_area
  
  tibble(Model = "Collar_area",
         Intercept = summary(lm_area)$coefficients[1,1],
         Slope = summary(lm_area)$coefficients[2,1],
         p_slope = summary(lm_area)$coefficients[2,4]) ->
    model_area_out
  
  # test collar depth
  sdata %>% 
    select(Rs_annual, Collar_depth) %>%
    filter(Rs_annual < 5000 & Collar_depth < 20) %>% 
    group_by(Collar_depth) %>% 
    summarise(Rs_annual = mean(Rs_annual), obs = n()) ->
    agg_srdb
  
  lm(agg_srdb$Rs_annual ~ agg_srdb$Collar_depth, weights = agg_srdb$obs) ->
    lm_depth
  
  tibble(Model = "Collar_depth",
         Intercept = summary(lm_depth)$coefficients[1,1],
         Slope = summary(lm_depth)$coefficients[2,1],
         p_slope = summary(lm_depth)$coefficients[2,4]) ->
    model_depth_out
  
  # test hours covered
  sdata %>% 
    select(Rs_annual, hour_cover) %>%
    filter(Rs_annual < 5000 & !is.na(hour_cover)) %>% 
    group_by(hour_cover) %>% 
    summarise(Rs_annual = mean(Rs_annual), obs = n()) ->
    agg_srdb
  
  lm(agg_srdb$Rs_annual ~ agg_srdb$hour_cover, weights = agg_srdb$obs) ->
    lm_hour
  
  tibble(Model = "Hours_coverage",
         Intercept = summary(lm_hour)$coefficients[1,1],
         Slope = summary(lm_hour)$coefficients[2,1],
         p_slope = summary(lm_hour)$coefficients[2,4]) ->
    model_hour_out
  
  # test measure interval
  sdata %>% 
    select(Rs_annual, Meas_interval) %>%
    filter(Rs_annual < 5000 & Meas_interval < 150) %>% 
    group_by(Meas_interval) %>% 
    summarise(Rs_annual = mean(Rs_annual), obs = n()) ->
    agg_srdb
  
  lm(agg_srdb$Rs_annual ~ agg_srdb$Meas_interval, weights = agg_srdb$obs) ->
    lm_interval
  
  tibble(Model = "Meas_interval",
         Intercept = summary(lm_interval)$coefficients[1,1],
         Slope = summary(lm_interval)$coefficients[2,1],
         p_slope = summary(lm_interval)$coefficients[2,4]) ->
    model_interval_out
  
  # combine all results and prepare table 1
  bind_rows(
    model_height_out,
    model_area_out,
    model_depth_out,
    model_hour_out,
    model_interval_out) ->
    slr_summary
  
  return(slr_summary)
}

#*****************************************************************************************************************
# data processing functions
#*****************************************************************************************************************



