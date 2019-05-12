
library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
memory.limit(size=1e9)

exclusion_zones.rs <- raster("P:/is-wel/indus/message_indus/input/percent_grid_cell_available_wind_solar.asc")
plot(exclusion_zones.rs)
#just to have a reference coordinate system, otherwise area function gives wanrning
indus_basin.spdf = readOGR("P:/is-wel/indus/message_indus/input",'Indus_bcu')
proj4string(exclusion_zones.rs) <- proj4string(indus_basin.spdf)
area.rs <- area(exclusion_zones.rs)

turbine_density = 5 # max MW installed per sqkm from IRENA; this is consistent with Eurek et al.
panel_density = 30 # Pietzcker et al claims 100 MW/km2 and provides values by country, but this seems very optimistic
#   Ong et al (2013) claims roughly 30 MW/km2 for large (>20 MW) PV plants; However, to be consistent with Pietzcker,
#   which was used for the global potentials, the Pietzcker value for a representative country in each basin 
#   is used (e.g. Pakistan for Indus).

# potential in MW  <- km^2    * [0-1]             * MW/km^2  
max_wind_potential <- area.rs * exclusion_zones.rs * turbine_density
plot(max_wind_potential)
max_solar_potential <- area.rs * exclusion_zones.rs * panel_density
plot(max_solar_potential)

# load capacity factors
# make exclusion zones values 0 or 1
bin_exclusion_zones.rs <- exclusion_zones.rs
bin_exclusion_zones.rs[bin_exclusion_zones.rs > 0 ] = 1

solar.df <- read.csv("P:/is-wel/indus/message_indus/input/LF/solar_monthly_LF.csv")
months = unique(solar.df$month)
# make spdf
solar.df <- solar.df %>% select(-id) %>% 
  spread(month,value)
solar.spdf <- solar.df  
coordinates(solar.spdf) = ~ lon + lat

gridded(solar.spdf) = TRUE 
plot(solar.spdf)

solar.st = do.call(stack, lapply( months, function(mm){ return( raster( solar.spdf[,mm] ) ) } ) )
                     
solar.st = resample(solar.st, bin_exclusion_zones.rs, method="bilinear")
# pdf("P:/is-wel/indus/message_indus/input/LF/solar_lf_no_exclusion.pdf")
# plot(solar.st)      
# dev.off()
solar.st = solar.st * (bin_exclusion_zones.rs )
# pdf("P:/is-wel/indus/message_indus/input/LF/solar_lf_after_exclusion.pdf")
# plot(solar.st)   
# dev.off()

wind.df <- read.csv("P:/is-wel/indus/message_indus/input/LF/wind_monthly_LF.csv")
wind.df <- wind.df %>% select(-id) %>% 
  spread(month,value)
wind.spdf <- wind.df  
coordinates(wind.spdf) = ~ lon + lat

gridded(wind.spdf) = TRUE 
plot(wind.spdf)

wind.st = do.call(stack, lapply( months, function(mm){ return( raster( wind.spdf[,mm] ) ) } ) )

wind.st = resample(wind.st, bin_exclusion_zones.rs, method="bilinear")
pdf("P:/is-wel/indus/message_indus/input/LF/wind_lf_no_exclusion.pdf")
plot(wind.st)
dev.off()
wind.st = wind.st * (bin_exclusion_zones.rs )
pdf("P:/is-wel/indus/message_indus/input/LF/wind_lf_after_exclusion.pdf")
plot(wind.st)
dev.off()

## gather and fit for gams

make_spatial_with_PID <- function(raster_in,mapping.spdf){
coords <- as.data.frame(coordinates(raster_in),StringAsFactor = F)
out.df <- as.data.frame(raster_in,StringAsFactor = F)
out.spdf <- out.df
coordinates(out.spdf) <- coords
proj4string(out.spdf) <- proj4string(mapping.spdf)
out.spdf = spTransform( out.spdf, crs(mapping.spdf) ) 

out.spdf <- out.spdf[!is.na(out.spdf@data[,1]),]
out.spdf <- out.spdf[out.spdf@data[,1] != 0,]

out.spdf@data$PID <- as.character(over(out.spdf,mapping.spdf[,"PID"])$PID)
#out.spdf@data$PID <- as.numeric(out.spdf@data$PID[,1])
names(out.spdf)[length(names(out.spdf))] = 'PID'
return(out.spdf)
}

PIDs <- over(out.spdf,mapping.spdf[,"PID"])[1]
solar_out.spdf <- make_spatial_with_PID(solar.st,indus_basin.spdf)
b = solar_out.spdf@data
max_solar.spdf <- make_spatial_with_PID(max_solar_potential,indus_basin.spdf)
#@####### check from here!!
solar_out.df <- bind_cols(as.data.frame(coordinates(solar_out.spdf),row.names = NULL), solar_out.spdf@data) %>% 
  filter(!is.na(PID)) %>%
  select(x,y,PID,everything()) %>% 
  gather(key = month, value = value,4:15) %>% 
  group_by(x,y) %>% 
  mutate(annual_lf = mean(value)) %>% 
  ungroup()

#hist(solar_out.df$annual_lf)
q_solar <-solar_out.df %>% select(PID,annual_lf) %>% 
  group_by(PID) %>% 
  mutate(q1 = as.numeric(quantile(annual_lf,probs = c(0.33,0.66,1))[1])) %>% 
  mutate(q2 = as.numeric(quantile(annual_lf,probs = c(0.33,0.66,1))[2])) %>% 
  select(-annual_lf) %>% 
  ungroup() %>% distinct()

solar_out.df<- solar_out.df %>% 
  left_join(q_solar) %>% 
  group_by(PID) %>% 
  mutate(tec = if_else(annual_lf <= q1,"solar_1",
                       if_else(annual_lf > q1 & annual_lf <= q2,"solar_2",
                               if_else(annual_lf > q2,"solar_3","NO")) ) ) %>% 
  select(-q1,-q2) %>% 
  ungroup()

solar_agg.df <- solar_out.df %>% 
  mutate(month = gsub(".*layer.","",month)) %>% 
  group_by(PID,tec,month) %>% 
  summarize(LF = mean(value)) %>% 
  ungroup()

## assign to area file the technologies 1,2,3
max_solar.df <- bind_cols(as.data.frame(coordinates(max_solar.spdf),row.names = NULL), max_solar.spdf@data) %>% 
  filter(!is.na(PID)) %>% 
  left_join(solar_out.df %>% select(x,y,tec) %>% distinct(),by = c("x","y")) %>% 
  filter(!is.na(tec)) %>% 
  group_by(PID,tec) %>% 
  summarise(MW = sum(layer))

## wind
wind_out.spdf <- make_spatial_with_PID(wind.st,indus_basin.spdf)
max_wind.spdf <- make_spatial_with_PID(max_wind_potential,indus_basin.spdf)
wind_out.df <- bind_cols(as.data.frame(coordinates(wind_out.spdf),row.names = NULL), wind_out.spdf@data) %>% 
  filter(!is.na(PID)) %>%
  select(x,y,PID,everything()) %>% 
  gather(key = month, value = value,4:15) %>% 
  group_by(x,y) %>% 
  mutate(annual_lf = mean(value)) %>% 
  ungroup()

#hist(wind_out.df$annual_lf)
q_wind <-wind_out.df %>% select(PID,annual_lf) %>% 
  group_by(PID) %>% 
  mutate(q1 = as.numeric(quantile(annual_lf,probs = c(0.33,0.66,1))[1])) %>% 
  mutate(q2 = as.numeric(quantile(annual_lf,probs = c(0.33,0.66,1))[2])) %>% 
  select(-annual_lf) %>% 
  ungroup() %>% distinct()

wind_out.df<- wind_out.df %>% 
  left_join(q_wind) %>% 
  group_by(PID) %>% 
  mutate(tec = if_else(annual_lf <= q1,"wind_1",
                       if_else(annual_lf > q1 & annual_lf <= q2,"wind_2",
                               if_else(annual_lf > q2,"wind_3","NO")) ) ) %>% 
  select(-q1,-q2) %>% 
  ungroup()

wind_agg.df <- wind_out.df %>% 
  mutate(month = gsub(".*layer.","",month)) %>% 
  group_by(PID,tec,month) %>% 
  summarize(LF = mean(value)) %>% 
  ungroup()

## assign to area file the technologies 1,2,3
max_wind.df <- bind_cols(as.data.frame(coordinates(max_wind.spdf),row.names = NULL), max_wind.spdf@data) %>% 
  filter(!is.na(PID)) %>% 
  left_join(wind_out.df %>% select(x,y,tec) %>% distinct(),by = c("x","y")) %>% 
  filter(!is.na(tec)) %>% 
  group_by(PID,tec) %>% 
  summarise(MW = sum(layer))

## save csv
max_cap_sw <- bind_rows(max_wind.df,max_solar.df)
names(max_cap_sw) = c("node",'tec',"value")
write.csv(max_cap_sw, file = "P:/is-wel/indus/message_indus/input/LF/max_capacity_sw.csv",
          row.names = F) 

year<- c(1990	,2000	,2010	,2015	,2020	,2030	,2040	,2050	,2060	)
vintage <- year

year_nd_vint = expand.grid(year,vintage)

load_factor_sw <- bind_rows(wind_agg.df,solar_agg.df)
names(load_factor_sw) = c("node","tec","time","value")
write.csv(load_factor_sw, file = "P:/is-wel/indus/message_indus/input/LF/load_factor_sw.csv",
          row.names = F) 

