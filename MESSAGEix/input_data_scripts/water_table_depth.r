

require( dplyr )
require( raster )
require( rgdal ) 
require( rgeos )
require( ncdf4 )

basin = c('Indus')
setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(basin.sp) = proj4string(basin.spdf)

# Import the groundwater table depth from the p drive
gtd.raster = raster('input/groundwater_table_depth/Eurasia_model_wtd_v2.nc') 
gtd.raster = mask( x = crop( gtd.raster, extent( basin.sp ), snap = "out" ), mask = rasterize( spTransform( basin.sp, crs( gtd.raster ) ), crop( gtd.raster, extent( basin.sp ), snap="out") ) )

# Average depth by district
bsn = spTransform( basin.spdf[,'PID'], crs(gtd.raster) )
gtd.df = data.frame( PID = basin.spdf@data$PID,
					 avg_depth_m = sapply( 1:length( bsn ), function( bbb ){ mean( unlist( raster::extract( gtd.raster, bsn[bbb,] ) )  , na.rm=TRUE ) } ) )

### For the Indus will alternatively use raw well data					 
					 
# Raw well depth data from csv
res = data.frame( read.csv('input/Water level data _2000_2014.csv', stringsAsFactors = FALSE ) ) %>%
		dplyr::select( Lat_dec , Lon_dec , SITE_ID , SITE_NAME , TOPOSHEET_ , SITE_TYPE , STATE_NAME , DISTRICT_N , TAHSIL_NAM , BLOCK_NAME , VILLAGE_NA , WLS_DATE , Date , Water.Level..m.bgl. ) %>%
		mutate( year = sapply( Date , function(x){ unlist( strsplit( x, '-' ) )[3] } ) )

# Add the year
res$year = sapply( res$Date , function(x){ unlist( strsplit( x, '-' ) )[3] } )

# Get the unique site ids
site_ids = unique(res$SITE_ID) 

# Aseemble list of measurements for each station
water_table_sites.list = lapply( site_ids, function(x){ res[ res$SITE_ID == x , ] } )
names( water_table_sites.list ) = site_ids

# Get the years
yrs = unique( res$year )[ order( unique( res$year ) ) ]

# Return a data frame that contains the measuresments at each site in each year
water_table_sites.df = data.frame( do.call( rbind, lapply( site_ids, function(s){ sapply( yrs, function(yy){ if( yy %in% water_table_sites.list[[ s ]]$year ){ return( mean( water_table_sites.list[[ s ]][ water_table_sites.list[[ s ]]$year == yy, 'Water.Level..m.bgl.' ] ) ) }else{ return( NA ) } } ) } ) ) )
names( water_table_sites.df ) = yrs
row.names( water_table_sites.df ) = site_ids

# Add the id and lat and lon and re-organize
water_table_sites.df$id = row.names( water_table_sites.df )
water_table_sites.df$lat = sapply( 1:length(water_table_sites.list), function(x){ water_table_sites.list[[ x ]]$Lat_dec[1] } )
water_table_sites.df$lon = sapply( 1:length(water_table_sites.list), function(x){ water_table_sites.list[[ x ]]$Lon_dec[1] } )
water_table_sites.df = water_table_sites.df[,c('id','lat','lon',yrs)]

# Convert to spatial data frame
water_table_sites.spdf = water_table_sites.df
coordinates(water_table_sites.spdf) = ~lon+lat

# Harmonize the geographical projections
proj4string( water_table_sites.spdf ) = proj4string( gtd.raster ) 
water_table_sites.spdf = spTransform( water_table_sites.spdf, crs( gtd.raster ) )

# Just keep the parts in the Indus basin
basin.spdf = spTransform( basin.spdf, crs( gtd.raster ) )
basin.sp = spTransform( basin.spdf, crs( gtd.raster ) )

# Add the global groundwater depth data from Fan et al 2013
pts = as( water_table_sites.spdf, 'SpatialPoints' )
water_table_sites.spdf@data$Fan_2010 = unlist( extract( gtd.raster, pts ) )

x1 = water_table_sites.spdf@data$Fan_2010
x2 = unlist( data.frame(water_table_sites.spdf)[,'X2010'] )
nn = which( is.na(x1) | is.na(x2) | x1 <= 0 | x2 <= 0 )
x1 = x1[-1*nn]
x2 = x2[-1*nn]
plot(x1,x2,xlab='Fan et al 2013',ylab='Well data')
x3 = x1/x2-1
x3 = x3[-1*which(is.na(x3))]

# Rasterize @ 0.25 degrees (~25km), 
# the data is irregularly spaced data and is rasterized using the mean of points found in each 0.25 degree cell
# Create empty raster @ 0.25 degrees - this will be used to sample the irregularly spaced data
rb = raster() # 
res(rb) = 0.25 # Set the resolution
rb[] = -9999 # Some values for now
rb = crop(rb,extent(pts)) # crop to the area containing the sites
clls = cellFromXY(rb,pts) # Get the 0.25 degree grid cell associated with each site
clls = clls[!is.na(clls)] # Remove NA
df = data.frame(water_table_sites.spdf) # Get the data frame containing all the site data

# Create a raster stack containing each year in the site data 
stk = do.call( stack, lapply( yrs, function(yy){
	r1 = rb
	# The raster cell value is equal to the mean of all sites found to lie within that raster cell
	r1[ unique( clls ) ]  = sapply( unique( clls ), function( cc ){ return( mean( unlist( df[ which( clls == cc ) , paste('X',yy,sep='') ] ), na.rm = TRUE ) ) } )
	# Set non-sensical values to NA
	r1[ r1[] <= 1 ] = NA
	return(r1)
	} ) )
names(stk) = yrs

# Keep specific years and take mean
gtd2.raster = mean( do.call( stack, lapply( c('X2010','X2011','X2012','X2013','X2014'), function( ttt ){ stk[[ttt]] } ) ) )

gtd2.df = data.frame( 	PID = basin.spdf@data$PID,
						avg_depth_m = sapply( 1:length( bsn ), function( bbb ){ 
							
							x  = unlist( raster::extract( gtd2.raster, bsn[bbb,] ) )
							
							if( length(x) > 1 ){
							
								return( quantile( x, 0.5, na.rm=TRUE ) ) 
							
								}else{ return( NA ) }
							
							} ) 
						
						)

# Temporary - Use mean where no data exists - will update 					
gtd2.df[ which( is.na(gtd2.df$avg_depth_m) ), 'avg_depth_m' ] = mean( gtd2.df$avg_depth_m, na.rm = TRUE ) 						

# Energy use in GW / MCM/day
gtd2.df$GW_per_MCM_per_day = round( 0.85 * 9.81 / 86400 * gtd2.df$avg_depth_m , digits = 5 )

write.csv( gtd2.df, 'input/gw_energy_intensity.csv', row.names = FALSE)		



