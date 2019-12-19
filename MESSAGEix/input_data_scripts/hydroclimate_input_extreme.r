
#source('C:/Users/parkinso/indus_ix/input_data_scripts/basin_hydroclimate_input.r')

require(rgeos)
require(rgdal)
require(raster)
require(rasterVis)
require(dplyr)
require(ncdf4)
require(tictoc)
memory.limit(size=1e9)

# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

cwatm_wd = 'P:/watxene/CWATM_Indus'

# basins to check
basin = c('Indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste0( basin, '_bcu' ), verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(basin.sp) = proj4string(basin.spdf)

# Get the climate model and scenario names
fls = list.files( cwatm_wd, pattern = '5min' )
#fls = fls[ which( !( grepl( 'historical', fls ) ) ) ]
climate_models = unique( unlist( strsplit( fls, '_' ) )[seq(2,by=4,4*length( fls ) ) ] )
climate_scenarios = unique( unlist( strsplit( fls, '_' ) )[seq(3,by=4,4*length( fls ) ) ] )


####################################
# Parameters 1: Runoff & GW Recharge
####################################

print('Working on: Runoff')

tic()
	
vr = 'runoff_monthavg'

runoff.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = c( sapply( 1:(nlayers(nc.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					
					return( calc( nc.stack[[ c( paste( 'X', seq( ystart,yend,by=1 ),'.', mmm, sep = '' ) ) ]], fun = function(x){ quantile( x, probs = 0.1, na.rm =TRUE ) } ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ 
					do.call( stack, lapply( 1:12, function( mmm ){ 
						calc( nc.stack[[ c( paste( 'X', seq( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ),as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + max( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ),by=1 ),'.', mmm, sep = '' ) ) ]], fun = function(x){ quantile( x, probs = 0.1, na.rm =TRUE ) } ) 
						} ) ) 
					} ) )  
				
				}
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( runoff.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( runoff.list[[1]][[1]] )
	runoff.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( runoff.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( runoff.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}
	
# Output harmonized data to file for use in crop model
for( i in names( runoff.list ) ){ for( j in names( runoff.list[[ 1 ]] ) ){ if( !is.null( runoff.list[[ i ]][[ j ]] ) ){ writeRaster( runoff.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/extremerunoff_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		
	
toc()	

print('')
	

print('Working on: Recharge')

tic()
	
vr = 'sum_gwRecharge_monthavg'

recharge.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = c( sapply( 1:(nlayers(nc.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					return( mean( nc.stack[[ c( paste( 'X', seq( ystart,yend,by=1 ),'.', mmm, sep = '' ) ) ]] ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ mean( nc.stack[[ c( paste( 'X', seq( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ),as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + max( ncvar_get(nc, "time") )/12*365.25 ), '-' ) )[1] ),by=1 ),'.', mmm, sep = '' ) ) ]] ) } ) ) } ) )  
				
				}
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( recharge.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( recharge.list[[1]][[1]] )
	recharge.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( recharge.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( recharge.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}
	
# Output harmonized data to file for use in crop model
for( i in names( recharge.list ) ){ for( j in names( recharge.list[[ 1 ]] ) ){ if( !is.null( recharge.list[[ i ]][[ j ]] ) ){ writeRaster( recharge.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/recharge_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		
	
toc()	

print('')
		
	
############################
# Parameter 2: Precipitation
############################

print('Working on: Precipitation')

tic()
	
vr = 'Precipitation_daily'

precip.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1)
			
			# Create data frame for mapping months and years to days
			nc_map.df = data.frame( id = 1:nlayers(nc.stack),
									year = as.numeric( unlist( strsplit( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ],'X' ) )[ seq(2,2*length( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ] ),by=2 ) ] ),
									month =  as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(2,3*length(names(nc.stack)),by=3) ] ),
									day = as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(3,3*length(names(nc.stack)),by=3) ] ) )
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					return( mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm, year %in% seq( ystart,yend,by=1) ) %>% dplyr::select( id ) %>% unlist() ) ]] ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ 
					
					mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm ) %>% dplyr::select( id ) %>% unlist() ) ]] )
					
					} ) )
					
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function(yy){ nc_decadal.stack } ) )
					
				}	
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( precip.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( precip.list[[1]][[1]] )
	precip.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( precip.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( precip.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}

# Output harmonized data to file for use in crop model
for( i in names( precip.list ) ){ for( j in names( precip.list[[ 1 ]] ) ){ if( !is.null( precip.list[[ i ]][[ j ]] ) ){ writeRaster( precip.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/precip_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		
	
###########################################
# Parameter 3: Reference evapotranspiration
###########################################
	
print('Working on: Reference evapotranspiration')

tic()
		
vr = 'ETRef_daily'

etref.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1)
			
			# Create data frame for mapping months and years to days
			nc_map.df = data.frame( id = 1:nlayers(nc.stack),
									year = as.numeric( unlist( strsplit( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ],'X' ) )[ seq(2,2*length( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ] ),by=2 ) ] ),
									month =  as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(2,3*length(names(nc.stack)),by=3) ] ),
									day = as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(3,3*length(names(nc.stack)),by=3) ] ) )
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					return( mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm, year %in% seq( ystart,yend,by=1) ) %>% dplyr::select( id ) %>% unlist() ) ]] ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ 
					
					mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm ) %>% dplyr::select( id ) %>% unlist() ) ]] )
					
					} ) )
					
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function(yy){ nc_decadal.stack } ) )
					
				}
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( etref.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( etref.list[[1]][[1]] )
	etref.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( etref.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( etref.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}

# Output harmonized data to file for use in crop model
for( i in names( etref.list ) ){ for( j in names( etref.list[[ 1 ]] ) ){ if( !is.null( etref.list[[ i ]][[ j ]] ) ){ writeRaster( etref.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/etref_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		
	

###########################################
# Parameter 4: Evaporation from open water
###########################################

print('Working on: Evaporation')

tic()
		
vr = 'EWRef_daily'

evap.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1)
			
			# Create data frame for mapping months and years to days
			nc_map.df = data.frame( id = 1:nlayers(nc.stack),
									year = as.numeric( unlist( strsplit( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ],'X' ) )[ seq(2,2*length( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ] ),by=2 ) ] ),
									month =  as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(2,3*length(names(nc.stack)),by=3) ] ),
									day = as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(3,3*length(names(nc.stack)),by=3) ] ) )
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					return( mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm, year %in% seq( ystart,yend,by=1) ) %>% dplyr::select( id ) %>% unlist() ) ]] ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ 
					
					mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm ) %>% dplyr::select( id ) %>% unlist() ) ]] )
					
					} ) )
					
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function(yy){ nc_decadal.stack } ) )
					
				}
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( evap.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( evap.list[[1]][[1]] )
	evap.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( evap.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( evap.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}

# Output harmonized data to file for use in crop model
for( i in names( evap.list ) ){ for( j in names( evap.list[[ 1 ]] ) ){ if( !is.null( evap.list[[ i ]][[ j ]] ) ){ writeRaster( evap.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/evap_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		

###################################################
# Parameter 5: Effective precipitation - non paddy
#####################################################
		
print('Working on: Effective precipitation - non-paddy')

tic()		
		
vr = 'Precipitation_daily'

eprecip.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
			
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			extent_nc = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
					
			# Get the time steps
			tm = as.character( seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1) )
			yr = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(1,3*length(tm),by=3) ] )
			mn = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(2,3*length(tm),by=3) ] )
			dy = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(3,3*length(tm),by=3) ] )
			
			# Chunk import processing using each year - signficantly improves processing time
			nc.stack = do.call( stack, lapply( unique( yr ), function( yyy ){
				
				# Import data as array
				ind = which( yr == yyy )
				start = rep(1, nc$var[[1]]$ndims)
				start[ nc$var[[1]]$ndims ] = ind[1]
				count = nc$var[[1]]$varsize
				count[ nc$var[[1]]$ndims ] = length( ind )
				dat = ncvar_get( nc, nc$var[[1]]$name, start=start, count=count )
				
				# Convert to moving average - 10 day average for non-paddy
				dat = aperm( apply( dat, c(1,2), function(x){ stats::filter( x, rep(1/10,10), sides = 2, circular = TRUE  ) } ), c(3,2,1) )
				
				# Convert to effective precipitation using the approach described in Doell and Siebert 2002.
				ind1 = which( dat[] >= 8.3/1000 )
				ind2 = which( dat[] < 8.3/1000 )
				dat[ ind1 ] = 1e-3 * ( 4.17 + 0.1 * dat[ ind1 ] )
				dat[ ind2 ] = 1e-3 * ( dat[ ind2 ] * ( 4.17 - 0.2 * dat[ ind2 ] ) )
					
				# Convert to monthly average and raster stack
				st = do.call( stack, lapply( unique( mn ), function( mmm ){ 
					r = raster( apply( dat[ , , which( mn[ ind ] == mmm ) ], c(1,2), mean ) )
					extent( r ) = extent_nc
					proj4string( r ) = proj4string( basin.sp )
					return( r ) 
					} ) )
				
				names( st ) = paste( yyy, unique( mn ), sep= '.' )
				
				gc()
				
				return( st )
				
				} ) )
				
			nc_close(nc)
			
			# New time mapping
			yr2 = unlist( strsplit( names( nc.stack ), '[.]' ) )[ seq( 1, 2 * length( names(nc.stack) ), by = 2 ) ]
			yr2 = unlist( strsplit( yr2, 'X' ) )[ seq( 2, length( yr2 ), by = 2 ) ]
			mm2 = unlist( strsplit( names( nc.stack ), '[.]' ) )[ seq( 2, 2 * length( names(nc.stack) ), by = 2 ) ]
			
			# Now average into decadal decision making intervals using specific time-averaging windows
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){  
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					# Monthly averages over the annual time horizon
					st = do.call( stack, lapply( 1:12, function( mmm ){ mean( nc.stack[[ which( yr2 %in% seq( ystart, yend, by = 1 ) & mm2 == mmm ) ]] ) } ) )
							
					return( st )
					
					} ) )   
				
				}else # Historical years
				{
				
				# Monthly averages over the annual time horizon
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ mean( nc.stack[[ which( mm2 == mmm ) ]] ) } ) )
				
				# Add to raster stack for each future decade	
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ nc_decadal.stack } ) )
	
				}	
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
		
		gc()	
		
		} )
	
	names( res1 ) = climate_models	
	
	gc()
	
	return(res1)
	
	} )

names( eprecip.list ) = climate_scenarios		

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( eprecip.list[[1]][[1]] )
	eprecip.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( eprecip.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( eprecip.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}
	
nonpaddy_precip.list = eprecip.list
rm(eprecip.list)

# Output harmonized data to file for use in crop model
for( i in names( nonpaddy_precip.list ) ){ for( j in names( nonpaddy_precip.list[[ 1 ]] ) ){ if( !is.null( nonpaddy_precip.list[[ i ]][[ j ]] ) ){ writeRaster( nonpaddy_precip.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/effprecip10day_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		

###################################################
# Parameter 6: Effective precipitation - paddy
#####################################################
	
print('Working on: Effective precipitation - paddy')

tic()		
			
vr = 'Precipitation_daily'

eprecip.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
			
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			extent_nc = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
					
			# Get the time steps
			tm = as.character( seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1) )
			yr = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(1,3*length(tm),by=3) ] )
			mn = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(2,3*length(tm),by=3) ] )
			dy = as.numeric( unlist( strsplit( tm, '-' ) )[ seq(3,3*length(tm),by=3) ] )
			
			# Chunk import processing using each year - signficantly improves processing time
			nc.stack = do.call( stack, lapply( unique( yr ), function( yyy ){
				
				# Import data as array
				ind = which( yr == yyy )
				start = rep(1, nc$var[[1]]$ndims)
				start[ nc$var[[1]]$ndims ] = ind[1]
				count = nc$var[[1]]$varsize
				count[ nc$var[[1]]$ndims ] = length( ind )
				dat = ncvar_get( nc, nc$var[[1]]$name, start=start, count=count )
				
				# Convert to moving average - 3 day average for paddy crops
				dat = aperm( apply( dat, c(1,2), function(x){ stats::filter( x, rep(1/3,3), sides = 2, circular = TRUE  ) } ), c(3,2,1) )
				
				# Convert to effective precipitation using the approach described in Doell and Siebert 2002.
				ind1 = which( dat[] >= 8.3/1000 )
				ind2 = which( dat[] < 8.3/1000 )
				dat[ ind1 ] = 1e-3 * ( 4.17 + 0.1 * dat[ ind1 ] )
				dat[ ind2 ] = 1e-3 * ( dat[ ind2 ] * ( 4.17 - 0.2 * dat[ ind2 ] ) )
					
				# Convert to monthly average and raster stack
				st = do.call( stack, lapply( unique( mn ), function( mmm ){ 
					r = raster( apply( dat[ , , which( mn[ ind ] == mmm ) ], c(1,2), mean ) )
					extent( r ) = extent_nc
					proj4string( r ) = proj4string( basin.sp )
					return( r ) 
					} ) )
				
				names( st ) = paste( yyy, unique( mn ), sep= '.' )
				
				gc()
				
				return( st )
				
				} ) )
				
			nc_close(nc)
			
			# New time mapping
			yr2 = unlist( strsplit( names( nc.stack ), '[.]' ) )[ seq( 1, 2 * length( names(nc.stack) ), by = 2 ) ]
			yr2 = unlist( strsplit( yr2, 'X' ) )[ seq( 2, length( yr2 ), by = 2 ) ]
			mm2 = unlist( strsplit( names( nc.stack ), '[.]' ) )[ seq( 2, 2 * length( names(nc.stack) ), by = 2 ) ]
			
			# Now average into decadal decision making intervals using specific time-averaging windows
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){  
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					# Monthly averages over the annual time horizon
					st = do.call( stack, lapply( 1:12, function( mmm ){ mean( nc.stack[[ which( yr2 %in% seq( ystart, yend, by = 1 ) & mm2 == mmm ) ]] ) } ) )
							
					return( st )
					
					} ) )   
				
				}else # Historical years
				{
				
				# Monthly averages over the annual time horizon
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ mean( nc.stack[[ which( mm2 == mmm ) ]] ) } ) )
				
				# Add to raster stack for each future decade	
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ nc_decadal.stack } ) )
	
				}	
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
		
		gc()	
		
		} )
	
	names( res1 ) = climate_models	
	
	gc()
	
	return(res1)
	
	} )

names( eprecip.list ) = climate_scenarios		

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( eprecip.list[[1]][[1]] )
	eprecip.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( eprecip.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( eprecip.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}
	
paddy_precip.list = eprecip.list
rm(eprecip.list)

# Output harmonized data to file for use in crop model
for( i in names( paddy_precip.list ) ){ for( j in names( paddy_precip.list[[ 1 ]] ) ){ if( !is.null( paddy_precip.list[[ i ]][[ j ]] ) ){ writeRaster( paddy_precip.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/effprecip3day_metersperday_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		

###################################################
# Parameter 7: Air temperature
#####################################################
	
print('Working on: Air temperature')

tic()		
			
vr = 'Tavg_daily'

airtemp.list = lapply( climate_scenarios, function( rcp ){
	
	# Assemble the basin rasters for each climate model 
	
	res1 = lapply( climate_models, function(gcm){ 
		
		fl = paste0( 'indus5min_', gcm, '_', rcp, '_naturalized' )
		
		if( fl %in% fls ){
		
			# Data from ncdf
			nc = nc_open( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ), verbose = FALSE ) # in m per day
			nc.stack = stack( paste0( cwatm_wd, '/', fl, '/', vr, '.nc' ) )
			extent(nc.stack ) = extent( min( ncvar_get(nc, "lon") ), max( ncvar_get(nc, "lon") ), min( ncvar_get(nc, "lat") ), max( ncvar_get(nc, "lat") ) )
			proj4string( nc.stack ) = proj4string( basin.sp )
			
			# Stack came without names - use date from netcdf and index to implement
			names(nc.stack) = seq(as.Date("1901-01-01") + min( ncvar_get(nc, "time") ),as.Date("1901-01-01") + max( ncvar_get(nc, "time") ),by=1)
			
			# Create data frame for mapping months and years to days
			nc_map.df = data.frame( id = 1:nlayers(nc.stack),
									year = as.numeric( unlist( strsplit( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ],'X' ) )[ seq(2,2*length( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ] ),by=2 ) ] ),
									month =  as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(2,3*length(names(nc.stack)),by=3) ] ),
									day = as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(3,3*length(names(nc.stack)),by=3) ] ) )
			
			if( rcp != 'historical' )
				{
				
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function( iii ){ do.call( stack, lapply( 1:12, function( mmm ){ 
					
					if( iii == 2010 ){ 
						
						ystart = 2006
						yend = 2020
						
						}else if( iii == 2060 ){
						
						ystart = 2050
						yend = 2055
						
						}else{
						
						ystart = iii - 10
						yend = iii + 5
						
						}
					
					return( mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm, year %in% seq( ystart,yend,by=1) ) %>% dplyr::select( id ) %>% unlist() ) ]] ) )
					
					} ) ) } ) )  
				
				}else
				{
				
				nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ 
					
					mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm ) %>% dplyr::select( id ) %>% unlist() ) ]] )
					
					} ) )
					
				nc_decadal.stack = do.call( stack, lapply( c(2010,2020,2030,2040,2050,2060), function(yy){ nc_decadal.stack } ) )
					
				}	
			
			names(nc_decadal.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			
			return( nc_decadal.stack )
			
			}else{
			
			return( NULL )
			
			}
			
		} )
	
	names( res1 ) = climate_models	
	
	return(res1)
	
	} )

names( airtemp.list ) = climate_scenarios		

gc()

# Ensemble mean
for( rcp in climate_scenarios )
	{
	tms = names( airtemp.list[[1]][[1]] )
	airtemp.list[[ rcp ]][[ 'ensemble' ]] = do.call( stack, lapply( tms, function(tm){ return( mean( do.call(stack, lapply( climate_models, function( cc ){ return( airtemp.list[[ rcp ]][[ cc ]][[ tm ]] ) } ) ) ) ) } ) )
	names( airtemp.list[[ rcp ]][[ 'ensemble' ]] ) = tms
	}

# Output harmonized data to file for use in crop model
for( i in names( airtemp.list ) ){ for( j in names( airtemp.list[[ 1 ]] ) ){ if( !is.null( airtemp.list[[ i ]][[ j ]] ) ){ writeRaster( airtemp.list[[ i ]][[ j ]], paste0( 'input/hydroclimate_input/airtemp_celsius_', j, '_', i, '.tif' ), format = 'GTiff', overwrite = TRUE ) } } }		

toc()
print('')		
	
rm( runoff.list, recharge.list, precip.list, etref.list, evap.list, nonpaddy_precip.list, paddy_precip.list, airtemp.list )		