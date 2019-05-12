

#LOAD RDS
require(tidyverse)
require(data.table)
require(tictoc)
memory.limit(size=1e9)

## load the csv and add coordinates based on locationid
coord.df <- read.csv("P:\\is-wel\\old_root_folder\\Data\\renewables_ninja\\INDUS_ZAMBEZI\\IIASA_META_DATA.CSV") %>% 
  filter(name %like% "Indus") %>% 
  dplyr::select(lon,lat,id)

## remove years we don't want
year_to_exclude <- seq(1980,1989,1)
yte_string <- paste(year_to_exclude,collapse = "|")

pv_file_names <- c("Solar_Indus0p05_AF.rds","Solar_Indus0p05_PK_3.rds","Solar_Indus0p05_CN.rds","Solar_Indus0p05_PK_4.rds","Solar_Indus0p05_IN_1.rds","Solar_Indus0p05_PK_5.rds","Solar_Indus0p05_IN_2.rds","Solar_Indus0p05_PK_1.rds","Solar_Indus0p05_PK_2.rds")
wind_file_names <- c("Wind_Indus0p05_AF.rds","Wind_Indus0p05_PK_3.rds","Wind_Indus0p05_CN.rds","Wind_Indus0p05_PK_4.rds","Wind_Indus0p05_IN_1.rds","Wind_Indus0p05_PK_5.rds","Wind_Indus0p05_IN_2.rds","Wind_Indus0p05_PK_1.rds","Wind_Indus0p05_PK_2.rds")

calculate_monthly_LF <- function(file_name,solar = TRUE) {
  ##FUNCTION
  if (solar) {folder = "SOLAR_CF\\"} else {folder = "WIND_CF\\"}
  sample.mt <- readRDS(paste0("P:\\is-wel\\old_root_folder\\Data\\renewables_ninja\\INDUS_ZAMBEZI\\",folder,file_name))
  
  locations <- names(sample.mt)[-1]
  
  sample.mt <- sample.mt %>% 
    filter(!Timestamp %like% yte_string)
  
  sample.df <- sample.mt %>% 
    
    ## decompose timestamp in month (leave previous part inc ase we want hours or year)
    
  #  separate(Timestamp,c("year_month","day_hour"), sep = 7) %>% 
  #  mutate(day_hour = gsub("-","", day_hour)) %>% 
  #  mutate(day_hour = gsub(" ","_", day_hour)) %>% 
    
    mutate(Timestamp = gsub(".*[-]([^.]+)[-].*", "\\1", Timestamp)) %>% 
    group_by(Timestamp) %>% 
    summarise_all(mean) %>% 
    rename(month = Timestamp) %>% 

  ## reshape as dataframe, no multiple columns, just Timestamp,location id,value
  
    gather(key = "id" , value = "value", locations) %>% 
    mutate(id = gsub(".*_","",id))
  
  # load the csv and add coordinates based on locationid >> spdf
  
  monthly_avg_lf <- sample.df %>% 
    left_join(coord.df %>% mutate(id = as.character(id))) %>% 
    dplyr::select(lon,lat,everything(),value)
  
  return(monthly_avg_lf)
}

## solar load factor
tic()
solar_monthly_LF <- bind_rows(lapply(pv_file_names, calculate_monthly_LF,solar = TRUE))
toc()

write_csv(solar_monthly_LF,"P:/is-wel/indus/message_indus/input/LF/solar_monthly_LF.csv")

## wind load factor
tic()
wind_monthly_LF <- bind_rows(lapply(wind_file_names, calculate_monthly_LF,solar = FALSE))
toc()

write_csv(wind_monthly_LF,"P:/is-wel/indus/message_indus/input/LF/wind_monthly_LF.csv")