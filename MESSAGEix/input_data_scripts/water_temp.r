

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

# location of water temp data
wd = 'P:/watxene/Wat-Data/wattemp/waterTemp_real'

# model names
md = unlist( strsplit( list.dirs(wd,recursive=FALSE) , paste0(wd,'/') ) )[seq(2,length( unlist( strsplit( list.dirs(wd,recursive=FALSE) , paste0(wd,'/') ) ) ),by=2)]

# basins to check
basin = c('Indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste0( basin, '_bcu' ), verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(basin.sp) = proj4string(basin.spdf)

water_temp.df = bind_rows( lapply( md, function( mmm ){
	
	# Get data from ncdfs across following variables
	vars = c( 'waterTemp_dailyTot', 'discharge_dailyTot_output' )
	vars2 = data.frame( waterTemp_dailyTot = 'waterTemp', discharge_dailyTot_output = 'discharge' )
	res = lapply( vars, function( vvv ){
		
		# Open ncdf without importing data
		nc = nc_open(paste0( wd, '/', mmm,  "/Mask17/", vvv, ".nc", sep='' ) )	
		
		# Spatial extent matched to basin boundary
		lon = data.frame( lon = ncvar_get(nc, "lon") ) %>% filter( lon >= extent(basin.sp)[1] & lon <= extent(basin.sp)[2] )
		lon_start = min( which( ncvar_get(nc, "lon") == min( lon ) ), which( ncvar_get(nc, "lon") == max( lon ) ) )
		lon_count = max( which( ncvar_get(nc, "lon") == min( lon ) ), which( ncvar_get(nc, "lon") == max( lon ) ) ) - lon_start
		lat = data.frame( lat = ncvar_get(nc, "lat") ) %>% filter( lat >= extent(basin.sp)[3] & lat <= extent(basin.sp)[4] )
		lat_start = min( which( ncvar_get(nc, "lat") == min( lat ) ), which( ncvar_get(nc, "lat") == max( lat ) ) )
		lat_count = max( which( ncvar_get(nc, "lat") == min( lat ) ), which( ncvar_get(nc, "lat") == max( lat ) ) ) - lat_start
		basin_extent = extent( 	min( lon ), max( lon ),  min( lat ), max( lat ) )		
	
		# Dates
		dates = sapply( min( ncvar_get(nc, "time") ):max( ncvar_get(nc, "time") ), function( iii ){ as.character( as.Date("1901-01-01") + iii ) } )
		dates = dates[ grepl( paste(2000:2020,collapse='|'), dates ) ] # limit to 2050
		
		# Import specific data from ncdf
		dat.df = bind_rows( lapply( 1:length(dates), function( ttt ){
			
			print( paste0( ttt, ' of ', length(dates) ) )
			
			r = raster( t( as.matrix( ncvar_get(  nc, 
							as.character( unlist( vars2[ vvv ] ) ), 
							start = c( lon_start, lat_start, ttt ), 
							count = c( lon_count, lat_count, 1 ) ) ) ) )
							
			extent( r ) = basin_extent			

			df = data.frame( 	lon = xFromCell( r, 1:length(r) ), 
								lat = yFromCell( r, 1:length(r) ), 
								time = dates[ ttt ],
								value = r[] ) %>% 
				filter( !is.na( value ) )
				
			names( df )[ names( df ) == 'value' ] = vvv	
							
			return( df )					
			
			} ) )
		
		return( dat.df )
		
		nc_close(nc)
		
		} ) 
	
	# Merge the water temp and discharge data.frames
	res = left_join( res[[1]], res[[2]] )	
	
	# Get the PID for each grid cell
	cells = unique( paste( res$lon, res$lat, sep = '-' ) )
	pts = data.frame( 	lon = as.numeric( unlist( strsplit( cells, '-' ) )[seq(1,2*length(cells),by=2)] ), 
						lat = as.numeric( unlist( strsplit( cells, '-' ) )[seq(2,2*length(cells),by=2)] ),
						id = 1:length(cells) )
	coordinates(pts) = ~lon+lat					
						
	pid2cell = over( pts, as(basin.spdf,'SpatialPolygons') )
	names(pid2cell) = basin.spdf@data$PID
	pid2cell = bind_rows( lapply( 1:length(pid2cell), function(jjj){ data.frame( PID = names(pid2cell)[jjj], id = unlist( pid2cell[[jjj]]] ) ) } ) )
	
	
	res = left_join( res, pid2cell )
	
	# Aggregate into PIDs using discharge to weight the mean temperature

	df = bind_rows( lapply( 1:length(basin.spdf), function( ppp ){
		
		extract( res
		
		df = data.frame( 	model = mmm,
							PID = ,
							month = ,
							value = )
		
		} ) )
	
	return( df )
	
	} ) )


	



availability.df = bind_rows( lapply( 1:length( basin.spdf ), function( ppp ){
	
	ncp.stack = crop( nc.stack, extent( basin.spdf[ ppp, ] ) )
	
	# get percent days below temp threshold
	availability.stack = do.call( stack, lapply( 1:12, function( mmm ){
		
		# get all days in a specific month
		stck = ncp.stack[[ which( as.numeric(unlist( strsplit( dts, '-' ) )[seq(2,3*length(dts),by=3)]) == mmm ) ]]
		
		# calulate percent days above threshold
		stck = calc( stck, fun = function(x){ which( x <= threshhold ) / length( x ) } )
		
		return( stck )
		
		} ) )

	return( data.frame( PID = basin.spdf@data$PID[ ppp ], month = 1:12, value = val ) )
	
	} ) )


# Create data frame for mapping months and years to days
nc_map.df = data.frame( id = 1:nlayers(nc.stack),
						year = as.numeric( unlist( strsplit( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ],'X' ) )[ seq(2,2*length( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(1,3*length(names(nc.stack)),by=3) ] ),by=2 ) ] ),
						month =  as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(2,3*length(names(nc.stack)),by=3) ] ),
						day = as.numeric( unlist( strsplit( names(nc.stack), '[.]' ) )[ seq(3,3*length(names(nc.stack)),by=3) ] ) )

nc_decadal.stack = do.call( stack, lapply( 1:12, function( mmm ){ 
					
					mean( nc.stack[[ c( nc_map.df %>% filter( month == mmm ) %>% dplyr::select( id ) %>% unlist() ) ]] )
					
					} ) )
						
						

#vname<-nc$name  #--- CHOOSE VARIABLE NAME IF MULTIPLE
vname="waterTemp"


