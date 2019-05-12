# Dams storage capacity
# Source: Grand Database V1.1
# Unit million cubic meters

# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

library(rgdal)
library(ncdf4)
library(chron)
library(raster)
library(tidyverse)
library(sp)

year = c( seq( 1990, 2010, by = 10  ), 2015, seq( 2020, 2060, by = 10 ) )

# Indus delineatiion
indus_basin.spdf = readOGR("P:/is-wel/indus/message_indus/input",'Indus_bcu')
proj4string(indus_basin.spdf)

## Grand Database V1.1
# million cubic meters
dam_capacity.spdf = readOGR("P:/is-wel/shared_data_sources/raw_data/grand_v1_1",'GRanD_dams_v1_1')
dam_capacity.spdf@proj4string
dam_capacity.spdf = spTransform( dam_capacity.spdf, crs(indus_basin.spdf) ) 
indus_dam_cam.spdf <- dam_capacity.spdf[indus_basin.spdf , ]
indus_dam_cam.spdf = indus_dam_cam.spdf[,c("DAM_NAME",'YEAR','CAP_REP','AREA_REP')]
names(indus_dam_cam.spdf) = c("DAM_NAME",'YEAR','Mm3','AREA_km2')
dams_no_cap <- indus_dam_cam.spdf@data %>% 
  filter(Mm3 <= 0)
indus_dam_cam.spdf <- indus_dam_cam.spdf[indus_dam_cam.spdf@data$Mm3 >= 0,]

vol_area.spdf <- indus_dam_cam.spdf[indus_dam_cam.spdf@data$AREA_km2 >= 0,]
vol_area.spdf$AV_ratio = vol_area.spdf$AREA_km2 * 10^3 / (vol_area.spdf$Mm3 * 10^6) # result unit: 1/m

# planned plants with storage capacity
planned_projects.df = data.frame( read.csv( "P:/is-wel/indus/message_indus/input/powerplants/indus_future_hydro_projects.csv", stringsAsFactors = FALSE ) )
planned_projects.df = planned_projects.df %>% 
  select("project.name",'opening','storage_km3','x','y') %>% 
  mutate(storage_km3 = storage_km3 * 1000) %>% 
  filter(storage_km3 > 0) %>% 
  mutate()

planned_projects.spdf = planned_projects.df
names(planned_projects.spdf) = c("DAM_NAME", 'YEAR', 'Mm3','LONG_DD','LAT_DD' )
coordinates(planned_projects.spdf) = ~ LONG_DD + LAT_DD
proj4string(planned_projects.spdf) = proj4string(indus_basin.spdf)
planned_projects.spdf = spTransform( planned_projects.spdf, crs(indus_basin.spdf) )

library(tmap)
indus_dam_cam.spdf$DAM_NAME[indus_dam_cam.spdf$Mm3 <= 7000] = NA
tm_shape(indus_basin.spdf)+
  tm_borders(col = NA, lwd = 1.5, lty = "solid", alpha = 0.3)+
  tm_polygons(col = NA, lwd = 1, lty = "solid", alpha = 0.2)+
  tm_text('PID',size = 0.8,auto.placement = T)+
  tm_shape(indus_dam_cam.spdf)+
  tm_bubbles(size = 1, col = "Mm3", 
             breaks = c(0,2000,4000,8000,12000,16000),
             border.col = "black", border.alpha = .5,  
             title.col="Dam Storage Capacity [M m^3]")+
  tm_text('DAM_NAME',size = 1.2,col = 'chocolate',auto.placement = T)+
  tm_layout(legend.position = c("right","bottom"))

tm_shape(indus_basin.spdf)+
  tm_borders(col = NA, lwd = 1.5, lty = "solid", alpha = 0.3)+
  tm_polygons(col = NA, lwd = 1, lty = "solid", alpha = 0.2)+
  tm_text('PID',size = 0.8,auto.placement = T)+
  tm_shape(planned_projects.spdf)+
  tm_bubbles(size = 1, col = "Mm3", 
             breaks = c(0,2000,4000,8000,12000,16000),
             border.col = "black", border.alpha = .5,  
             title.col="Dam Storage Capacity [M m^3]")+
  tm_text('DAM_NAME',size = 1,col = 'chocolate',auto.placement = T)+
  tm_layout(legend.position = c("right","bottom"))

indus_dam_cam.spdf = indus_dam_cam.spdf + planned_projects.spdf
# gets pids
indus_dam_cam.spdf@data$PID <- as.character(over(indus_dam_cam.spdf,indus_basin.spdf[,"PID"])$PID)
a = indus_dam_cam.spdf@data
river.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu_river', sep = '_' ), verbose = FALSE )

map_storage_capacity <- tm_shape(indus_basin.spdf)+
  tm_borders(col = NA, lwd = 1.5, lty = "solid", alpha = 0.3)+
  tm_polygons(col = NA, lwd = 1, lty = "solid", alpha = 0.2)+
  tm_shape(river.spdf)+
    tm_lines(col = 'cyan3', lwd = 1.5, lty = "solid", alpha = 0.8)+
  tm_shape(indus_dam_cam.spdf)+
    tm_bubbles(size = 1, col = "Mm3", 
               breaks = c(0,2000,4000,8000,12000,16000),
               border.col = "black", border.alpha = .5,  
               title.col="Dam Storage Capacity [M m^3]")+
    tm_layout(legend.position = c("right","bottom"))

pdf("P:/is-wel/indus/message_indus/input/dam_storage_capacity_GRANDv1.1_M_m3.pdf")
map_storage_capacity
dev.off()

to_model <- indus_dam_cam.spdf@data %>% 
  mutate(YEAR = as.numeric(if_else(YEAR == 'proposed', '2030',
                      if_else(as.numeric(YEAR) < 2020,'2020','2030')))) %>% 
  group_by(PID,YEAR) %>% 
  summarise(value = sum(Mm3)) %>% ungroup() %>% 
  select(PID,YEAR,value) %>% 
  group_by(PID) %>% 
  mutate(value = cumsum(value))
#  mutate(value = if_else(YEAR == 2030,cumsum(value),value ))
  
  

#add all he years
to_model.out = bind_rows( lapply( year, function( yy ){
  bind_rows( lapply( unique(to_model$PID), function( nn ){
          data.frame( year = yy, node = nn, value = if (yy < 2030){
                      to_model$value[ to_model$PID == nn & to_model$YEAR == 2020 ]  } else {
                        max(to_model$value[ to_model$PID == nn & to_model$YEAR == 2020 ],
                            to_model$value[ to_model$PID == nn & to_model$YEAR == 2030 ]) 
                      } )
  } ) )
} ) ) 

write_csv(to_model.out, "P:/is-wel/indus/message_indus/input/storage/dam_storage_capacity_GRANDv1.1_M_m3_PID.csv")

if (basin == 'Indus'){
level_dams = read_csv("P:/is-wel/indus/message_indus/input/storage/wapda_indus_reservoir_levels.csv")
level_dams_avg = level_dams %>% 
  summarise_all(mean)

# Volumnes and Areas from GRanD_Technical_Documentation_v1_1, V[Mm3] = 0.678 * ( A[km2] * h[m] )^0.9229
# water levels from WAPDA, those are NOT Dam heigt, so should not be used in the above formula
# the following approach works as we deal with a differences of h_max - h_avg
# to account for Volumes the h_dam to be used are: 470 ft Tarbela, 490 ft Mangla
avg_storage_calculation = data.frame(DAM_NAME = c('Tarbela','Mangla'),
                                     h_max = c(max(level_dams$TARBELA_LEVELS_FEET),max(level_dams$MANGLA_LEVELS_FEET)),
                                     h_avg = c(level_dams_avg$TARBELA_LEVELS_FEET,level_dams_avg$MANGLA_LEVELS_FEET),
                                     A_avg = c((dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(AREA_REP))[,1],
                                                (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(AREA_REP))[,1]),
                                     V_max = c((dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(CAP_REP))[,1],
                                               (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(CAP_REP))[,1]),
                                     stringsAsFactors = F) %>% 
  mutate(h_diff = (h_max-h_avg)*0.3048) %>% 
  mutate(V_diff = 0.678 * ( A_avg * h_diff )^0.9229) %>% 
  mutate(avg_strg = 1 -(V_diff/V_max)) %>% 
  left_join(a %>% select(DAM_NAME,PID)) %>% rename(node = PID)
other_pid_val = mean(avg_storage_calculation$avg_strg)
other_pid = indus_basin.spdf$PID[!(indus_basin.spdf$PID %in% avg_storage_calculation$node)]

#check/compare with SDIP Indus River report
b_check <- data.frame(DAM_NAME = c('Tarbela','Mangla'),
                      h_max = c(max(level_dams$TARBELA_LEVELS_FEET),max(level_dams$MANGLA_LEVELS_FEET))*0.3048,
                      h_avg = c(level_dams_avg$TARBELA_LEVELS_FEET,level_dams_avg$MANGLA_LEVELS_FEET)*0.3048,
                      disc_max = c(max(level_dams$`INDUS AT TARBELA OUTLFOW_1000cusecs`),
                                   max(level_dams$JHELUM_AT_MANGLA_OUTFLOW_1000cusecs))*0.3048^3*1000,
                      disc_min = c(min(level_dams$`INDUS AT TARBELA OUTLFOW_1000cusecs`),
                                   min(level_dams$JHELUM_AT_MANGLA_OUTFLOW_1000cusecs))*0.3048^3*1000
                      )
# numbers are consistent

multiplier = data.frame(node = other_pid, avg_strg = other_pid_val,stringsAsFactors = F) %>% 
  bind_rows(avg_storage_calculation %>% select(node,avg_strg))

write_csv(multiplier, "P:/is-wel/indus/message_indus/input/storage/average_storage_multiplier.csv")

}

# evap 

cwatm_wd = 'P:/watxene/CWATM_Indus'
# Get the climate model and scenario names
fls = list.files( cwatm_wd, pattern = '5min' )
#fls = fls[ which( !( grepl( 'historical', fls ) ) ) ]
climate_models = unique( unlist( strsplit( fls, '_' ) )[seq(2,by=4,4*length( fls ) ) ] )
climate_scenarios = unique( unlist( strsplit( fls, '_' ) )[seq(3,by=4,4*length( fls ) ) ] )

if( !file.exists( paste0( getwd(), '/input/daily_evaporation.csv') ) ){
  
  evap.df = bind_rows( lapply( climate_scenarios, function( climate_scenario ){ 
    
    bind_rows( lapply( climate_models, function( climate_model ){ 
      
      if( file.exists( paste0( 'input/hydroclimate_input/evap_metersperday_',climate_model,'_', climate_scenario, '.tif' ) ) )
      {
        
        EVref.stack = stack( paste0( 'input/hydroclimate_input/evap_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
        names(EVref.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
        
        # Resample to match GAEZ data 
        crop_folder = list.dirs(path = yeld_path)
        crop_folder = crop_folder[ grep( paste0('irr_', crop_names[1]), crop_folder)]
        crop_file = list.files(path = crop_folder,pattern = '.tif')
        tmp = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
        EVref.stack = resample( EVref.stack, tmp, method = 'bilinear' )
        
        # FAO equation for irrigation water requirement
        EVref.stack[ EVref.stack < 0 ] = 0
        st = EVref.stack
        # Get values for each basin PID
        lst = raster::extract( st, basin.spdf, byid=TRUE )
        yr = ( unlist( strsplit( names( st ), '[.]' ) )[seq(1,2*length(names(st)),by=2)] )
        yr = unlist( strsplit( yr, 'X' ) )[seq(2,2*length(yr),by=2)]
        mn = ( unlist( strsplit( names( st ), '[.]' ) )[seq(2,2*length(names(st)),by=2)] )
        df = bind_rows( lapply( 1:length( lst ), function( x ){
          data.frame( scenario = climate_scenario,
                      model = climate_model,
                      node = basin.spdf@data$PID[ x ],
                      year = yr,
                      time = mn,
                      unit = 'MCM_per_day',
                      value = 1e4 * sapply( 1:ncol( lst[[ x ]] ), function( jjj ){ mean( lst[[ x ]][ , jjj ], na.rm = TRUE ) } ) ) # 1e4 is conversion from meters per day
        } ) )
        df$value[ is.nan( df$value ) ] = 0
        
        return( df )
        
      }
    } ) )
    
  } ) )
  
  # Write to csv
  write.csv( evap.df, paste0( getwd(), '/input/daily_evaporation.csv'), row.names = FALSE )

} else{
  evap.df = read.csv(paste0( getwd(), '/input/daily_evaporation.csv'),stringsAsFactors=FALSE )
  
}

# gets pids
vol_area.spdf@data$PID <- as.character(over(vol_area.spdf,indus_basin.spdf[,"PID"])$PID)
vol_area.df = vol_area.spdf@data %>% 
  rename(node = PID) %>% 
  select(node,AV_ratio) %>% 
  group_by(node) %>% 
  summarise(AV_ratio = mean(AV_ratio))

avg_AV_ratio = mean(vol_area.df$AV_ratio)

# According to Liu et al. 2018, per unit volume evaporation factor = 0.5 * ev * AV_ratio
evap_factor.df = evap.df %>% left_join(vol_area.df) %>% 
  mutate(AV_ratio = if_else(is.na(AV_ratio),avg_AV_ratio,AV_ratio)) %>% 
  rename(ev = value) %>% 
  mutate(value = 0.5*ev*AV_ratio) %>% 
  mutate(unit = 'unitless')

write.csv( evap_factor.df, paste0( getwd(), '/input/storage/evapo_loss_reservoir_per_unit_volume.csv'), row.names = FALSE )


## rule curves

level_dams = read_csv("P:/is-wel/indus/message_indus/input/storage/wapda_indus_reservoir_levels.csv")

# from GRanD_Technical_Documentation_v1_1, V[Mm3] = 0.678 * ( A[km2] * h[m] )^0.9229
# maximum volumes from WAPDA
monthly_level = rbind(data.frame(month = level_dams$`PERIOD DATE 2017-18`,
                                 DAM_NAME = c('Tarbela'),
                                 node = 'PAK_8',
                                 h_max = c(max(level_dams$TARBELA_LEVELS_FEET)),
                                 h_avg = c(level_dams$TARBELA_LEVELS_FEET)/mean(c(level_dams$TARBELA_LEVELS_FEET))*470,
                                 A_avg = (dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(AREA_REP))[,1],
                                 V_max = (dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(CAP_REP))[,1],
                                 stringsAsFactors = F),
                      data.frame(month = level_dams$`PERIOD DATE 2017-18`,
                                 DAM_NAME = c('Mangla'),
                                 node = 'PAK_1',
                                 h_max = c(max(level_dams$MANGLA_LEVELS_FEET)),
                                 h_avg = c(level_dams$MANGLA_LEVELS_FEET)/mean(c(level_dams$MANGLA_LEVELS_FEET))*490,
                                 A_avg = (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(AREA_REP))[,1],
                                 V_max = (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(CAP_REP))[,1],
                                 stringsAsFactors = F) ) %>% 
  mutate(h_avg2 = h_avg *0.3048) %>% 
  mutate(V_avg = 0.678 * ( A_avg * h_avg2 )^0.9229) %>% 
  mutate(V_avg2 = 30.684 * ( A_avg)^0.9578) %>%
  mutate(month = gsub('.*-','',month))

map_months = data.frame(month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                        time = seq(1,12,1) ,stringsAsFactors = F)

monthly_lvl_agg = monthly_level %>% 
  left_join(map_months) %>% ungroup() %>% 
  group_by(time,DAM_NAME,node) %>% 
  summarise(h_avg2 = mean(h_avg2),V_max = mean(V_max),V_avg = mean(V_avg)) %>% ungroup() %>% 
  group_by(DAM_NAME) %>% mutate(V_avg = V_avg - (max(V_avg) - V_max)) %>% ungroup()


ggplot(data = monthly_lvl_agg)+
  geom_line(aes(time,V_avg))+
  geom_line(aes(time,V_max),color = 'red',linetype="dashed")+
  facet_wrap(~DAM_NAME)+ylab('Mm^3')+xlab('month')+
  scale_x_continuous(expand = c(0,0),breaks=seq(1,12,1))+
  theme_bw()

# probably take the higher series to set upper constraint
monthly_variations = monthly_lvl_agg %>% 
  mutate(ratio_avg_max = V_avg/V_max) %>% 
  mutate(max_min = if_else(node == 'PAK_8','max','min')) %>% 
  group_by(max_min) %>% 
  arrange(time) %>%
  mutate(diff_on_max = (V_avg - lag(V_avg, default = first(V_avg)))/V_max ) %>% 
  select(-node,-h_avg2)


ggplot()+
  geom_line(data = monthly_variations,aes(time, ratio_avg_max,color = max_min))+theme_bw()

write.csv( monthly_variations, paste0( getwd(), '/input/storage/rule_curves_values.csv'), row.names = FALSE )
