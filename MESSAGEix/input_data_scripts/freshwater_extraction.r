
# Existing ground and surface water pumping capacity for urban, rural and industry sectors - note the irrigation capacity is sized in crop_yields.r

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

## MANUAL UPDATE BASED ON KNOWN FRACTIONS

# IND_4 - uses 72% groundwater, see https://www.youtube.com/watch?v=RjsThobgq7Q&t=306s about 5 minutes in 
groundwater_fraction.df$fraction[ groundwater_fraction.df$node == 'IND_4' ] = 0.72

# AFG uses 2.8/15.8=18%, see 'Irrigation Outreach in Afghanistan: Exposure to Afghan Water Security Challenges'
groundwater_fraction.df$fraction[ grepl( 'AFG', groundwater_fraction.df$node ) ] = 0.18


# Now use the groundwater fraction to set the historical capacities based on the historical demand level
demand.df = read.csv( "input/indus_demands_new.csv", stringsAsFactors=FALSE ) %>%
	filter( scenario == 'SSP2' ) %>%
	filter( year == 2015, sector != 'crop', units != 'MW' ) %>%
	mutate( commodity = 'freshwater' ) %>%
	dplyr::select( commodity, sector, type, pid, year, month, value )
demand.df = rbind( 
	demand.df %>% filter( type == 'withdrawal' ) %>% mutate( level = ifelse( sector == 'urban', 'urban_final', ifelse( sector == 'rural', 'rural_final', 'industry_final' ) ) ),
	demand.df %>% filter( type == 'return' ) %>% mutate( level = ifelse( sector == 'urban', 'urban_waste', ifelse( sector == 'rural', 'rural_waste', 'industry_waste' ) ) )
	) %>% dplyr::rename( node = pid, year_all = year, time = month ) %>% dplyr::select( node, level, commodity, year_all, time, value )
	
	
# Multiply the fraction by the historical demands to calibrate historical gw demands 
gw_demand.df = demand.df %>% # demand.df is already in mcm_per_day
  filter(year_all == 2015) %>% 
  filter(commodity == 'freshwater') %>% 
  filter(level %in% c('industry_final','rural_final','urban_final')) %>% 
  left_join(groundwater_fraction.df,by = c("node")) %>% 
  mutate(value = value * fraction ) %>%
  dplyr::select( node, level, commodity, year_all, time, value )
    
# Surface water determined using inverse of groundwater fractions 
sw_demand.df = demand.df %>% 
  filter(year_all == 2015) %>% 
  filter(commodity == 'freshwater') %>% 
  filter(level %in% c('industry_final','rural_final','urban_final')) %>% 
  left_join(groundwater_fraction.df,by = c("node")) %>% 
  mutate(value = value * (1-fraction)) %>%
  dplyr::select( node, level, commodity, year_all, time, value ) 
   
# Estimate capacity of groundwater diversions - units in MCM per day
# Lacking historical data on vintaging, historical capacities
gw_capacity.df = gw_demand.df %>% 
	group_by( node, level ) %>%
	summarise( value = round( max( value ) * 1.1, digits = 3 ) ) %>%
	as.data.frame( ) %>%
	mutate( tec = paste( unlist( strsplit( level, '_' ) )[seq(1,2*length(level),by=2)], 'gw_diversion', sep = '_' ), year_all = 2015, units = 'mcm_per_day' ) %>%
	select( node, tec, year_all, value, units )
sw_capacity.df = sw_demand.df %>% 
	group_by( node, level ) %>%
	summarise( value = round( max( value ) * 1.1, digits = 3 ) ) %>%
	as.data.frame( ) %>%
	mutate( tec = paste( unlist( strsplit( level, '_' ) )[seq(1,2*length(level),by=2)], 'sw_diversion', sep = '_' ), year_all = 2015, units = 'mcm_per_day' ) %>%
	select( node, tec, year_all, value, units )	
	
### Append historical capacity csv to include freshwater extration capacities

# historical capacity csv
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
ind = which( historical_capacity.df1$tec %in% sw_capacity.df$tec | historical_capacity.df1$tec %in% gw_capacity.df$tec ) 
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] } # remove any existing entries 
historical_capacity.df1 = rbind( 	historical_capacity.df1,
									sw_capacity.df, 
									gw_capacity.df  )
write.csv( 	historical_capacity.df1, 
			"input/historical_new_cap.csv", 
			row.names = FALSE )
