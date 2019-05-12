
require(raster)
require(dplyr)
require(rgeos)
require(rgdal) 
require(countrycode)
memory.limit(size=1e9)

basin = c('Indus')

setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))

# Tubewell data from Mekonnen, Siddiqi et al. (2016) and Rao et al. (forthcoming)
elec_share.df = data.frame( read.csv( 'input/electric_share_of_irrigation.csv', header = TRUE, stringsAsFactors = FALSE ) )		

# Get GADM boundaries 
admin.spdf = do.call( rbind, lapply( unique( elec_share.df$country ), function( ccc ){ 
	res = getData( 'GADM', country = as.character( ccc ), level = elec_share.df$gadm_level[ which( elec_share.df$country == ccc )[1] ] )[,c( 'ISO',paste0( 'NAME_', elec_share.df$gadm_level[ which( elec_share.df$country == ccc )[1] ] ))]
	names(res) = c('ISO','NAME')
	res = res[ which( res$NAME %in% elec_share.df$district[ which( elec_share.df$country == ccc ) ] ), ]
	res@data$frac = elec_share.df$electric_share[ match(  res$NAME, elec_share.df$district ) ]
	row.names(res) = paste( res$NAME, 1:length(res$NAME), sep = '_' )
	return(res)
	} ) )
	
# Keep regions in basin	
admin.spdf = admin.spdf[ which( unlist( gIntersects( basin.sp, admin.spdf, byid = TRUE ) ) ), ]	
r = raster()
res(r) = 0.05
r = crop( r, extent( admin.spdf ) )
frac.raster = rasterize( admin.spdf, r, field = 'frac' )
proj4string( frac.raster ) = proj4string( basin.spdf )	
	
electricity_fraction.df = do.call( rbind, lapply( 1:length(basin.spdf), function( iii ){
		
	frc = mean( unlist( raster::extract( frac.raster, basin.spdf[iii,] ) ), na.rm = TRUE )	
	
	if( is.nan(frc) | is.na(frc) )
		{
		
		if( unlist( strsplit( as.character( basin.spdf@data$PID[iii] ), '_' ) )[1] %in% unique( admin.spdf@data$ISO ) )
			{
			
			frc = mean( admin.spdf@data$frac[ which( admin.spdf@data$ISO == unlist( strsplit( as.character( basin.spdf@data$PID[iii] ), '_' ) )[1] ) ] )
			
			}else{ # Use pakistan as default
			
			frc = mean( admin.spdf@data$frac[ which( admin.spdf@data$ISO == 'PAK' ) ] )	
			
			}
			
		}
		
	return( data.frame( PID = as.character( basin.spdf@data$PID[iii] ), fraction = round( frc, digits = 2 ) ) )	
		
	} ) )

write.csv( electricity_fraction.df, 'input/irrigation_electricity_fraction.csv', row.names = FALSE )

