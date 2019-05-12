

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)


# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')


# Existing power generation capacity
# Read in Raptis et al and Carma data and merge

# Mapping between WEPP names and Raptis cooling and the technology naming convention for modeling
raptis_types_map_cooling.df = data.frame(read.csv('input/powerplants/raptis_types_map_cooling_type.csv',stringsAsFactors=FALSE)) %>% 
  mutate(msgssp_type = if_else(msgssp_type == "air","ac",
                               if_else(msgssp_type == "ot_saline","sw",
                                       gsub("_.*","",msgssp_type)))) 
map_unit.df = data.frame(read.csv('input/powerplants/map_unit_type.csv',stringsAsFactors=FALSE))
map_unit.df$tps2[map_unit.df$tps2 == "nuclear_st"] = "nuclear"
fuel_map.df = data.frame(	wepp = c('WAT','UR','GAS','CSGAS','BAG','BL','Oil','TGAS','BFG','PEAT','AGAS','OGAS','CKGAS','CXGAS','BITUMEN','COAL','COG','ETHANOL','KERO','GEO','COKE','LNG','LPG','DGAS','RGAS','CGAS','REFGAS','WSTGAS','SHALE','MGAS','LGAS','BIOMASS','WOOD','WOODGAS','OIL','NG','SUN','FLIQ','KER','WIND','BGAS','BLIQ','FGAS','BSOL','JET','WSTH'), 
                          final = c('Hydro','Nuclear','Gas','Gas','Biomass','Biomass','Biomass','Oil','Gas','Gas','Gas','Gas','Gas','Gas','Oil','Coal','Coal','Biomass','Gas','Geothermal','Oil','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Biomass','Biomass','Biomass','Oil','Gas','Solar','Oil','Oil','Wind','Biogas','Biomass','Gas','Biomass','Oil','Wsth') )

# Load the geocoded WEPP database and rename some of the categories
ppl.df = data.frame(read.csv('input/IIASAPP_v3.6/geocoded/IIASAPP_geo2018-03-08.csv',stringsAsFactors=FALSE))
ppl.df = ppl.df[, c("latM", "lonM", "UNIT", "PLANT", "STATUS",  "YEAR" , "MW_x" , "UTYPE_x" , "FUEL_x", "COOL_complete")] 
names( ppl.df ) = c("LAT" , "LON", "UNIT", "PLANT", "STATUS",  "YEAR" , "MW" , "UTYPE" , "FUEL", "COOL")

# Rename cooling technologies based on defined mapping 
ppl.df <- ppl.df %>% 
  left_join(raptis_types_map_cooling.df %>% dplyr::select(raptis_type,msgssp_type),by = c("COOL" = "raptis_type")) %>% 
  dplyr::select(-COOL) %>% 
  rename(COOL = msgssp_type)

# Rename fuel types and remove NA types
ppl.df$FUEL = fuel_map.df$final[ match( ppl.df$FUEL, as.character(fuel_map.df$wepp) ) ]
ppl.df = ppl.df[ which(!is.na(ppl.df$FUEL)) , ]

# Remove retired plants
ppl.df = ppl.df[ -1 * which( ppl.df$STATUS == 'RET' ) , ]

# carma.df = data.frame(read.csv('P:/ene.general/Water/global_basin_modeling/powerplants/carma.csv',stringsAsFactors=FALSE))
# carma_red.df = carma.df[-1*c(which(as.character(carma.df$locationid) %in% as.character(ppl.df$LOCATIONID))),]
# carma_red.df = carma_red.df[-1*c(which(is.na(carma_red.df$latitude))),]
# carma_red.df = carma_red.df[-1*c(which(as.character(carma_red.df$pmover) %in% c('HY','ST','CC'))),]
# carma_red.df = carma_red.df[-1*c(which(is.na(carma_red.df$pmover))),]
# carma_red.df = carma_red.df[-1*c(which(is.na(carma_red.df$dfuel))),]
# 
# mp1 = data.frame(carma = as.character(c('NG','SUN','FLIQ','KER','WIND','BGAS','BLIQ','FGAS','BSOL','JET','WSTH')), raptis =as.character( c('Gas','Solar','Oil','Oil','Wind','Biogas','Biomass','Gas','Biomass','Oil','Wsth')),stringsAsFactors=FALSE)
# carma_red.df <- carma_red.df %>% 
#   left_join(mp1, by = c("dfuel" = "carma")) %>% 
#   dplyr::select(-dfuel) %>% 
#   rename(dfuel = raptis)
# # carma_red.df$dfuel = unlist( lapply( 1:length(carma_red.df$dfuel), function(x){ if(length(which(as.character(carma_red.df$dfuel[x]) %in% mp1$carma))>0){return( as.character( mp1$raptis[which(mp1$carma == as.character(carma_red.df$dfuel[x]))]) ) }else{return(NA)} } ) )
# carma_red.df = carma_red.df[-1*c(which(is.na(carma_red.df$dfuel))),]
# carma_red.df = carma_red.df[-1*c(which(carma_red.df$pmover == 'OT')),]
# ppl.df = ppl.df[-1*c(which(as.character( ppl.df$FUEL ) == 'Unknown')),]
# ppl.df$FUEL[which(ppl.df$FUEL == 'Biofuel')] = 'Biomass'
# ppl.df$COOL[which(as.character( ppl.df$COOL ) == 'N/A')] = NA

# carma_red.df2 <- carma_red.df %>% 
#   mutate(UNIT = NA) %>% 
#   mutate(YEAR = 2010 - round(as.numeric(age.max))) %>% 
#   mutate(COOL = NA) %>% 
#   dplyr::select(locationid,latitude,longitude,UNIT,plant,YEAR,mw_2009,dfuel,pmover,COOL)
# names(carma_red.df2) = names(ppl.df)
# pplt.df <- ppl.df %>% bind_rows(carma_red.df2)

# pplt.df = pplt.df[-1*c(which( as.character(pplt.df$FUEL) == 'Wsth')),]																					
# pplt.df$UTYPE = unlist( lapply( 1:nrow(pplt.df), function(x){ unlist(strsplit(pplt.df$UTYPE[x],'/'))[1] } ) )
# pplt.df$UTYPE[pplt.df$UTYPE=='CCSS'] = 'CC'																					
# pplt.df$YEAR[which(is.na(pplt.df$YEAR))] = 2000
# Combined unit and fuel types
ppl.df$TPS = paste( ppl.df$FUEL, ppl.df$UTYPE, sep= '.' )
ppl.df$TPS = map_unit.df$tps2[ match( ppl.df$TPS, as.character(map_unit.df$tps1) ) ]


ppl.df <- ppl.df %>% 
  mutate(TPS = paste(TPS,COOL,sep = '_')) %>% 
  mutate(TPS = gsub("_NA.*","",TPS))


#pplt.df$tps_utype = unlist( lapply( 1:nrow(pplt.df), function(x){ paste( pplt.df$FUEL[x], pplt.df$UTYPE[x], sep = '_' ) } ) )																					

# Mapping to message ssp2 technology names
# map2ssp = data.frame( 	ppl_name = c('Oil_ST', 'Coal_ST', 'Biofuel_ST', 'Gas_CC', 'Nuclear_ST', 'Geo_ST', 'Solar_ST', 'Waste_ST', 'Gas_IC', 'Oil_IC', 'Gas_GT', 'Solar_PV', 'Oil_GT', 'Biogas_IC', 'Wind_WT', 'Biomass_IC'),
# 						msg_name = c('loil_ppl', 'coal_ppl', 'bio_ppl', 'gas_cc', 'nuc_lc', 'geo_ppl', 'solar_th_ppl', 'mw_ppl', 'gas_ct', 'foil_ppl', 'gas_ct', 'solar_pv_ppl', 'foil_ppl', 'bio_ppl', 'wind_ppl', 'bio_ppl') )
# 
# pplt.df$msg_utype = rep(NA,length(pplt.df$tps_utype))
# for(i in 1:nrow(map2ssp))
# 	{
# 	pplt.df$msg_utype[ as.character( pplt.df$tps_utype ) == as.character( map2ssp$ppl_name[i] ) ] = as.character( map2ssp$msg_name[i] ) 
# 	}						
# pplt.df$TPS = unlist( lapply( 1:nrow(pplt.df), function(x){ paste( pplt.df$FUEL[x], pplt.df$UTYPE[x], pplt.df$COOL[x], sep = '_' ) } ) )
				
# add hydropower
global_hydropower.spdf = readOGR("input/powerplants",'global_hydropower_plants')
global_hydropower.df = data.frame(global_hydropower.spdf)
pplt.df = bind( ppl.df, data.frame( 	LOCATIONID = rep(NA,length(global_hydropower.spdf)),
										LAT = global_hydropower.df$coords.x1,
										LON = global_hydropower.df$coords.x2,
										UNIT = rep(NA,length(global_hydropower.spdf)),
										PLANT = rep(NA,length(global_hydropower.spdf)),
										YEAR = global_hydropower.df$YEAR,
										MW = global_hydropower.df$MW,
										FUEL = rep('Hydro',length(global_hydropower.spdf)),
										UTYPE = rep('HY',length(global_hydropower.spdf)),
										COOL = rep(NA,length(global_hydropower.spdf)),
										#tps_utype = rep(paste('Wat_HY'),length(global_hydropower.spdf)),
										#msg_utype = 'hydro_lc',
										TPS = rep(paste('hydro'),length(global_hydropower.spdf)) ) )

# some changes to make names consistend with message		
pplt.df <- pplt.df %>% 
  mutate(TPS = if_else(TPS %in% c( "solar_th","solar_th_ac","solar_th_cl" ),"solar_csp",TPS)) %>% 
  mutate(MW = as.numeric(MW)) %>% 
  filter(!is.na(LAT))

unique(sort( pplt.df$TPS))
# many plants with no locations, the should all be out of the indus basin
pplt.spdf = pplt.df
coordinates(pplt.spdf) = ~ LON + LAT

# Indus delineatiion
indus_basin.spdf = readOGR("input",'Indus_bcu')
indus_basin.spdf@proj4string
ind_ppl.df = data.frame(crop(pplt.spdf,extent(indus_basin.spdf)))

unique(sort( ind_ppl.df$TPS))
#sum(ind_ppl.df$MW[ind_ppl.df$TPS %in% c("oil_st_cl","oil_st_ot" ,"oil_st_sw"  ) ])
#sum(ind_ppl.df$MW[ind_ppl.df$TPS == "oil_gt"])
# technologies with no cooling, dont want to spread into cooling tech
ind_ppl.df<- ind_ppl.df %>% 
  mutate(TPS = as.character(TPS)) %>% 
  mutate(TPS = gsub("biogas_","biomass_",TPS)) %>% 
  mutate(TPS = if_else(TPS == "gas_ic","gas_gt",
                       if_else(TPS == "biomass_gt","biomass_st",
                  if_else(TPS == "oil_ic","oil_gt",TPS) )  ) )

unique(sort( ind_ppl.df$TPS))
tech_no_cool <- c("gas_gt","oil_gt","wind","hydro","solar_pv")

# calculate share of cooling technologies
share_cool <- ind_ppl.df %>% 
  dplyr::select(MW,FUEL,COOL) %>% 
  filter(!is.na(COOL)) %>% 
  group_by(FUEL,COOL) %>% 
  summarize(MW = sum(MW)) %>% ungroup() %>% 
  group_by(FUEL) %>% 
  mutate(share = MW/sum(MW))


new_tech <- bind_rows(ind_ppl.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "ac"),
                      ind_ppl.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "cl"),
                      ind_ppl.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "ot"),
                      ind_ppl.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "sw") ) %>% 
  left_join(share_cool %>% dplyr::select(-MW)) %>% 
  filter(!is.na(share)) %>% 
  mutate(MW = MW*share) %>% 
  mutate(TPS = paste(TPS,COOL,sep = "_")) %>% 
  dplyr::select(-share)

#sort(unique(new_tech$TPS))
#sum(new_tech$MW[new_tech$TPS %in% c("coal_st_ac"   , "coal_st_cl"   , "coal_st_ot"   , "coal_st_sw") ])

ind_ppl.df <- bind_rows(ind_ppl.df %>% filter(!is.na(COOL) | (TPS %in% tech_no_cool) ) %>% mutate(COOL = as.character(COOL)),
                         new_tech)

unique(sort( ind_ppl.df$TPS))
#sum(ind_ppl.df$MW[ind_ppl.df$TPS %in% c("oil_st_cl","oil_st_ot" ,"oil_st_sw"  ) ])
#sum(ind_ppl.df$MW[ind_ppl.df$TPS == "oil_gt"])

#ind_ppl.df$COOL[which( is.na(ind_ppl.df$COOL) & ind_ppl.df$UTYPE %in% c('ST','CC') )] = 'cl_fresh'
#ind_ppl.df$TPS = unlist( lapply( 1:nrow(ind_ppl.df), function(x){ if(!is.na(ind_ppl.df$COOL[x])){paste( ind_ppl.df$FUEL[x], ind_ppl.df$UTYPE[x], ind_ppl.df$COOL[x], sep = '_' )}else{paste( ind_ppl.df$FUEL[x], ind_ppl.df$UTYPE[x], sep = '_' )} } ) )
ind_ppl.spdf = ind_ppl.df
coordinates(ind_ppl.spdf) = ~ LON + LAT
proj4string(ind_ppl.spdf) = proj4string(indus_basin.spdf)
ind_ppl.df = cbind(data.frame(ind_ppl.spdf), over(ind_ppl.spdf,indus_basin.spdf[,which(names(indus_basin.spdf) == 'PID')]))
ind_ppl.df = ind_ppl.df[-1*c(which(is.na(ind_ppl.df$PID))),]
ind_ppl.spdf = ind_ppl.df
coordinates(ind_ppl.spdf) = ~ LON + LAT

plot(ind_ppl.spdf)

# historical_new_capacity(node,tec,year_all)

hist_new_cap.spdf <- ind_ppl.spdf
hist_new_cap.spdf@data <- hist_new_cap.spdf@data %>% 
  dplyr::select(PID,TPS,YEAR,MW)


year_all <- c(1990	,2000	,2010	,2015	,2020	,2030	,2040	,2050	,2060	)
range_year <- seq(-4,5,1)
map_year <- data.frame()
for (i in seq_along(year_all)) {
  for (j in seq_along(range_year)) {
    tmp = data.frame(YEAR = c(year_all[i]+range_year[j]),year_all = year_all[i])
    map_year<- rbind(map_year,tmp)
  }
  
}

to_remove<-data.frame(YEAR =     as.numeric(c(2013,2014,2015,2011,2012,2018,2019,2020,2016,2017)),
                      year_all = as.numeric(c(2010,2010,2010,2015,2015,2015,2015,2015,2020,2020)))

map_year <- map_year %>% anti_join(to_remove)

too_old <- 1990
hist_new_cap.df <- hist_new_cap.spdf@data %>% 
  mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
  left_join(map_year) %>% 
  mutate(year_all = if_else ((YEAR <= 1986 & YEAR >= too_old), 1990 , year_all)) %>% 
  filter(year_all >= 1990) %>% 
  dplyr::select(-YEAR) %>% 
  group_by(PID,TPS,year_all) %>% 
  summarise(MW = sum(MW))

names(hist_new_cap.df) <- c("node","tec","year_all","value")

write.csv(hist_new_cap.df,file = "input/historical_new_cap.csv",
          row.names = F)  

hist_new_cap.df = read.csv( "input/historical_new_cap.csv", stringsAsFactors = FALSE )
	  
hist_new_cap.df$units = 'MW'

write.csv(hist_new_cap.df,file = "input/historical_new_cap.csv",
          row.names = F) 		  
		  
