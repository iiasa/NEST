
# Existing ground and surface water pumping capacity

require(rgeos)
require(rgdal)
require(raster)
require(dplyr)
require(rasterVis)
require(maptools)
require(ncdf4)
require(reshape)
require(ggmap)
memory.limit(size=1e9)

# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

# basins to check
basin = c('Indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))

# Create buffered basin polygon for gridded data along the border
buff.sp = gBuffer( basin.sp, width=0.1 ) 
buff2.sp = gBuffer( basin.sp, width=10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)

# Irrigation canal withdrawals in mm from Cheema et al. 2014
IRR_cw.raster = raster( 'input/IRR_cw.img' )
IRR_cw.raster = projectRaster( setValues(raster(IRR_cw.raster),IRR_cw.raster[]), crs = crs( basin.sp ) )
IRR_gw.raster = raster( 'input/IRR_gw.img' )
IRR_gw.raster = projectRaster( setValues(raster(IRR_gw.raster),IRR_gw.raster[]), crs = crs( basin.sp ) )
IRR_gw.raster = mask( 	x = crop( IRR_gw.raster, extent( basin.sp ), snap = "out" ), 
						mask = rasterize( basin.sp, crop( IRR_gw.raster, extent( basin.sp ), snap="out" ) ) )

# Groundwater abstraction fro Wada et al
nc = nc_open('input/Wada_groundwater_abstraction/waterdemand_30min_groundwaterabstraction_million_m3_month.nc', verbose=FALSE)
gwabstract.stack = stack( 'input/Wada_groundwater_abstraction/waterdemand_30min_groundwaterabstraction_million_m3_month.nc' )
extent(gwabstract.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( gwabstract.stack ) = proj4string( basin.spdf )
gwabstract.stack = crop( gwabstract.stack, extent(buff.sp) )
names(gwabstract.stack) = c( sapply( 1:(nlayers(gwabstract.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
gwabstract.stack = gwabstract.stack[[ c( which(grepl( 'X2010',names(gwabstract.stack) )) ) ]] # keep 2010

# Irrigation
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4', verbose=FALSE)
irrigation.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4' )
extent(irrigation.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( irrigation.stack ) = proj4string( basin.spdf )
irrigation.stack = crop( irrigation.stack, extent(buff.sp) )
names(irrigation.stack) = c( sapply( 1:(nlayers(irrigation.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
irrigation.stack = irrigation.stack[[ c( which(grepl( 'X2010',names(irrigation.stack) )) ) ]] # keep 2010

# Industrial
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIndWW_monthly_1960_2010.nc4', verbose=FALSE)
industrial.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIndWW_monthly_1960_2010.nc4' )
extent(industrial.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( industrial.stack ) = proj4string( basin.spdf )
industrial.stack = crop( industrial.stack, extent(buff.sp) )
names(industrial.stack) = c( sapply( 1:(nlayers(industrial.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
industrial.stack = industrial.stack[[ c( which(grepl( 'X2010',names(industrial.stack) )) ) ]] # keep 2010

# Domestic
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PDomWW_monthly_1960_2010.nc4', verbose=FALSE)
domestic.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PDomWW_monthly_1960_2010.nc4' )
extent(domestic.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( domestic.stack ) = proj4string( basin.spdf )
domestic.stack = crop( domestic.stack, extent(buff.sp) )
names(domestic.stack) = c( sapply( 1:(nlayers(domestic.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
domestic.stack = domestic.stack[[ c( which(grepl( 'X2010',names(domestic.stack) )) ) ]] # keep 2010

# Existing groundwater capacity from initial extraction levels
total.stack = domestic.stack + industrial.stack + irrigation.stack
total.stack  =  sum(stack(total.stack))
total_gwabstract.stack = sum(stack(gwabstract.stack))
gwfraction.stack = ( total_gwabstract.stack / total.stack )
gwfraction.stack[gwfraction.stack[]>1]=1
gwfraction.stack[is.na(gwfraction.stack)] = 0


frac = c( unlist( lapply( 1:length(basin.spdf), function(x){ 
	
	gw = sum( data.frame( extract( total_gwabstract.stack, as(basin.spdf[x,],'SpatialPolygons'), na.rm=TRUE, cellnumbers = TRUE )[[1]])['value'] )
	
	ww = sum( data.frame( extract( total.stack, as(basin.spdf[x,],'SpatialPolygons'), na.rm=TRUE, cellnumbers = TRUE )[[1]])['value'] )
	
	return( round( min( 1, max( 0, gw/ww, na.rm=TRUE ) ), digits = 3 ) ) 
	
	} ) ) )

# Data frame output by PID
groundwater_fraction.df = data.frame( node = basin.spdf@data$PID, fraction = frac )

# Adriano approach
# keep only PID variable of spatialpolygon
# basin_pid.spdf <- basin.spdf[,1]
# # make a raster with gwfraction locations and pid information
# pid_gwfrac.rs = rasterize(basin_pid.spdf,gwfraction.stack)
# # merge value and pid raster
# gwfrac_pid.stack = stack(gwfraction.stack,total.stack,pid_gwfrac.rs)

# #same as groundwater_fraction.df
# gw_frac.df = as.data.frame(gwfrac_pid.stack,stringsAsFactor = F) %>%
  # dplyr::rename(node = layer.3_PID) %>% 
  # filter(!is.na(node)) %>% 
  # group_by(node) %>% 
  # summarise(frac = sum(layer.1*layer.2)/sum(layer.2) ) 

# pids_original <- basin.spdf@data$PID
# basin.spdf@data$PID = as.character( paste( 'PID', as.character( 1:length(basin.spdf) ), sep = '_' ) )
# pids_after <- basin.spdf@data$PID
# 
# map_PID <- data.frame(pids_original,pids_after)
# 
# pid_mapping = function(db){
#   db <- db %>% 
#     left_join(map_PID %>% dplyr::rename(node = pids_original)) %>% 
#     select(-node) %>% 
#     dplyr::rename(node = pids_after) %>% 
#     select(node,everything())
#   return(db)
# }
# 
# gw_frac.df<- pid_mapping(gw_frac.df) %>% 
#   mutate(node = as.character(node))

demand.df = read.csv( "input/indus_demands.csv", stringsAsFactors=FALSE )
# Just keep the current SSP and add level and commodity to match core GAMS model
demand.df = demand.df[ which( demand.df$scenario == 'SSP2' ), c( which( names( demand.df ) != 'scenario' ) ) ]
demand.df$commodity = NA
demand.df$commodity[ which( demand.df$type %in% c('withdrawal','return') ) ] = 'freshwater'
demand.df$commodity[ which( demand.df$type %in% c('electricity') ) ] = 'electricity'
demand.df$level = NA
demand.df$level[ which(  demand.df$type %in% c('withdrawal') & demand.df$sector == 'irrigation' ) ] = 'irrigation_field'
demand.df$level[ which(  demand.df$type %in% c('electricity') & demand.df$sector == 'irrigation' ) ] = 'irrigation_final'
demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == c('urban') ) ] = 'urban_final'
demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == c('industry') ) ] = 'industry_final'
demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == 'rural' )  ] = 'rural_final'
demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'urban' ) ] = 'urban_waste'
demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'industry' ) ] = 'industry_waste'
demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'rural' ) ] = 'rural_waste'
demand.df = demand.df[ , c( 'pid', 'level', 'commodity', 'year', 'month', 'value' ) ]
names(demand.df) = c('node','level','commodity','year_all','time', 'value0' ) 

# Multiply the fraction by the historical demands to calibrate historical gw demands
gw_demand.df <- demand.df %>% 
  filter(year_all == 2015) %>% 
  filter(commodity == 'freshwater') %>% 
  filter(level %in% c('industry_final','rural_final','urban_final','irrigation_field')) %>% 
  left_join(groundwater_fraction.df,by = c("node")) %>% 
  mutate(value = value0 * frac) %>% 
  mutate(tec = if_else(level == 'irrigation_field','irrigation_gw_diversion',
                       if_else(level == 'urban_final','urban_gw_diversion',
                               if_else(level == 'rural_final','rural_gw_diversion','CHECK_MISSING')))) %>% 
  select(node,tec,year_all,time,value)
 
# Surface water determined using inverse of groundwater fractions 
sw_demand.df <- demand.df %>% 
  filter(year_all == 2015) %>% 
  filter(commodity == 'freshwater') %>% 
  filter(level %in% c('industry_final','rural_final','urban_final','irrigation_field')) %>% 
  left_join(groundwater_fraction.df,by = c("node")) %>% 
  mutate(value = value0 * (1-frac)) %>% 
  mutate(tec = if_else(level == 'irrigation_field','irrigation_sw_diversion',
                       if_else(level == 'urban_final','urban_sw_diversion',
                               if_else(level == 'rural_final','rural_sw_diversion','CHECK_MISSING')))) %>% 
  select(node,tec,year_all,time,value) 
    
# Estimate capacity of groundwater diversions - units in MCM per day
# Lacking historical data on vintaging, historical capacities
# Are distributed uniformly across 2000, 2010 and 2015 
gw_capacity.df = do.call( rbind, lapply( unique(gw_demand.df$node), function( nn ){ 

	do.call( rbind, lapply( unique(gw_demand.df$tec), function( tt ){
	
		res = max( gw_demand.df$value[ which( gw_demand.df$tec == tt & gw_demand.df$node == nn ) ] ) * ( 1 + 0.1 ) # 10% reserve margin
		
		df = data.frame( 	node = rep( nn, 2 ), 
							tec = rep( tt, 2 ), 
							year_all = c( 2010, 2015 ), 
							value = round( rep( res / 2, 2 ) / 1e6, digits = 2 ),
							units = rep( 'mcm_per_day', 2 )	)
		
		return(df)
		
		} ) )
		
	} ) )
	
# Repeat for surface water	
sw_capacity.df = do.call( rbind, lapply( unique(sw_demand.df$node), function( nn ){ 

	do.call( rbind, lapply( unique(sw_demand.df$tec), function( tt ){
	
		res = max( sw_demand.df$value[ which( sw_demand.df$tec == tt & sw_demand.df$node == nn ) ] ) * ( 1 + 0.1 ) # 10% reserve margin
		
		df = data.frame( 	node = rep( nn, 2 ), 
							tec = rep( tt, 2 ), 
							year_all = c( 2010, 2015 ), 
							value = round( rep( res / 2, 2 ) / 1e6, digits = 2 ),
							units = rep( 'mcm_per_day', 2 )	)
		
		return(df)
		
		} ) )
		
	} ) )


# Extraction technologies that aggregate the diversions (enables constraining the total extraction)	

gw_capacity_extract.df = do.call( rbind, lapply( unique(gw_demand.df$node), function( nn ){ 

	do.call( rbind, lapply( unique(gw_demand.df$year_all), function( yy ){

		res = sum( gw_capacity.df$value[ which( gw_capacity.df$node == nn & gw_capacity.df$year_all == yy ) ] )
		
		df = data.frame( 	node = nn, 
							tec = 'gw_extract', 
							year_all = yy, 
							value = res,
							units = 'mcm_per_day'	)
		
		return(df)
	
		} ) )
	
	} ) )
		
sw_capacity_extract.df = do.call( rbind, lapply( unique(sw_demand.df$node), function( nn ){ 

	do.call( rbind, lapply( unique(sw_demand.df$year_all), function( yy ){

		res = sum( sw_capacity.df$value[ which( sw_capacity.df$node == nn & sw_capacity.df$year_all == yy ) ] )
		
		df = data.frame( 	node = nn, 
							tec = 'sw_extract', 
							year_all = yy, 
							value = res,
							units = 'mcm_per_day'	)
		
		return(df)
	
		} ) )
	
	} ) )
				
### Append historical capacity csv to include freshwater extration capacities

# historical capacity csv
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
ind = which( historical_capacity.df1$tec %in% sw_capacity.df$tec | historical_capacity.df1$tec %in% gw_capacity.df$tec ) 
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] }
historical_capacity.df1 = rbind( 	historical_capacity.df1, 
									sw_capacity.df, 
									sw_capacity_extract.df,
									gw_capacity.df,
									gw_capacity_extract.df )
write.csv( 	historical_capacity.df1, 
			"input/historical_new_cap.csv", 
			row.names = FALSE )

# irr_gw.df <- as.data.frame(stack(irrigation.stack,pid_gwfrac.rs),stringsAsFactor = F )  %>% 
#   dplyr::rename(PID = layer_PID) %>% 
#   filter(!is.na(PID)) %>% 
#   group_by(PID) %>% 
#   summarise(tot = sum(PID) ) %>% 
#   left_join(gw_frac.df) %>% 
#   mutate(irr_gw = tot*frac)
# 
# ind_gw.df <- as.data.frame(stack(industrial.stack,pid_gwfrac.rs),stringsAsFactor = F ) %>% 
#   dplyr::rename(PID = layer_PID) %>% 
#   filter(!is.na(PID)) %>% 
#   group_by(PID) %>% 
#   summarise(tot = sum(PID) ) %>% 
#   left_join(gw_frac.df) %>% 
#   mutate(ind_gw = tot*frac)
# 
# dom_gw.df <- as.data.frame(stack(domestic.stack,pid_gwfrac.rs),stringsAsFactor = F ) %>% 
#   dplyr::rename(PID = layer_PID) %>% 
#   filter(!is.na(PID)) %>% 
#   group_by(PID) %>% 
#   summarise(tot = sum(PID) ) %>% 
#   left_join(gw_frac.df) %>% 
#   mutate(dom_gw = tot*frac)
# 
# out.df <- irr_gw.df %>% 
#   left_join(ind_gw.df) %>% left_join(dom_gw.df) %>% 
#   select(PID,irr_gw,ind_gw,dom_gw)

write.csv(gw_demand.df, file.path(getwd(),'input/Wada_groundwater_abstraction/gw_punping_urb_rur_irr.csv') ,row.names = F)
