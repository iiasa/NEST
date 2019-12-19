
require(raster)
require(dplyr)
require(rgeos)
require(rgdal) 
require(countrycode)
require(RColorBrewer)
require(latticeExtra)
memory.limit(size=1e9)

basin = c('Indus')

setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin


# For the irrigated areas, we need to account for the massive diversions to outside the basin 
# Irrigation command areas are identified from the gridded irrigation withdrawals harmonized  in Cheema et al. 2014
IRR_cw.raster = raster( 'input/IRR_cw.img' )
IRR_cw.raster = projectRaster( setValues(raster(IRR_cw.raster),IRR_cw.raster[]), crs = crs( basin.spdf ) )

# Get withdrawals within the hydrological basin boundary
IRR_cw0.raster = mask( 	x = crop( IRR_cw.raster, extent( basin.spdf ), snap = "out" ), 
						mask = rasterize( basin.spdf, crop( IRR_cw.raster, extent( basin.spdf ), snap="out" ) ) )
IRR_cw0.raster = IRR_cw0.raster / 1e6 * area( IRR_cw0.raster ) * 1e3 / 365 # convert to MCM per day
IRR_cw0.df = data.frame( 	PID = basin.spdf@data$PID, 
							val = sapply( raster::extract( IRR_cw0.raster, basin.spdf ), sum, na.rm=TRUE ) )

# Create raster and remove grid cells included in the basin already
IRR_cw1.raster = IRR_cw.raster
IRR_cw1.raster = IRR_cw1.raster / 1e6 * area( IRR_cw1.raster ) * 1e3 / 365 # convert to MCM per day
basin_cells = do.call( rbind, raster::extract( IRR_cw.raster, basin.spdf, na.rm=TRUE, cellnumbers = TRUE ) )[,'cell']
IRR_cw1.raster[ basin_cells ] = NA

# Convert to spatial points
ccc = which( !is.na( IRR_cw1.raster[] ) )
IRR_cw1.pts = data.frame( 	x = xFromCell( IRR_cw1.raster, cell = ccc ),
							y = yFromCell( IRR_cw1.raster, cell = ccc ),
							val = IRR_cw1.raster[ ccc ] )
coordinates(IRR_cw1.pts) = ~x+y						
proj4string(IRR_cw1.pts) = proj4string(basin.spdf)					
					
# Get nearest basin polygons
basinr.spdf = basin.spdf[ c( 9, 13, 19, 20 ), ] # Only including basins along boundary - IDENTIFIED MANUALLY
IRR_cw1.pts$PID = sapply( 1:nrow(IRR_cw1.pts), function( iii ){ basinr.spdf@data$PID[ which.min( gDistance( IRR_cw1.pts[iii,], basinr.spdf, byid=TRUE ) ) ] } )

# Ensure all grid cells in India allocated to indian sub-basin
ind = spTransform( getData( 'GADM', country = 'IND', level = 0 )[,'GID_0'], crs( IRR_cw1.pts ) )
IRR_cw1.pts$PID[ which( over( IRR_cw1.pts, ind )[,'GID_0'] == 'IND' ) ] =  'IND_4'

# Convert to polygons
IRR_cw1.pts = merge( IRR_cw1.pts, data.frame( PID = unique( IRR_cw1.pts$PID ), ID = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.raster = rasterize( IRR_cw1.pts, IRR_cw1.raster, field = 'ID' )
IRR_pid.spdf = rasterToPolygons( IRR_pid.raster, dissolve = TRUE )
IRR_pid.spdf@data = left_join( IRR_pid.spdf@data, data.frame( PID = unique( IRR_cw1.pts$PID ), layer = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.spdf = IRR_pid.spdf[,'PID']

# Bind and dissolve with original basin polygons
basin_irr.spdf = aggregate( rbind( basin.spdf[,'PID'], IRR_pid.spdf ) , 'PID' )
basin_irr.spdf@data = left_join( basin_irr.spdf@data, basin.spdf@data ) 

# Use version with irrigated areas to calibrate the crop implementation
basin0.spdf = basin.spdf
basin.spdf = basin_irr.spdf
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))

# Tubewell data from Mekonnen, Siddiqi et al. (2016) and Rao et al. (forthcoming)
elec_share.df = data.frame( read.csv( 'input/electric_share_of_irrigation.csv', header = TRUE, stringsAsFactors = FALSE ) )		

# Get GADM boundaries 
admin.spdf = do.call( rbind, lapply( unique( elec_share.df$country ), function( ccc ){ 
	lev = elec_share.df$gadm_level[ which( elec_share.df$country == ccc )[1] ]
	res = getData( 'GADM', country = as.character( ccc ), level = lev )[,c( 'GID_0',paste0( 'NAME_', lev ) )]
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


# Create figure
pal = rev(brewer.pal(7,"RdYlGn"))#[2:8]
fillc = "grey75"
row.names(admin.spdf) = admin.spdf@data$NAME
plt = gIntersection(admin.spdf[,'NAME'],basin.sp,byid=TRUE)
df = left_join( data.frame( district = unlist( strsplit( names( plt ), ' 1' ) ) ), elec_share.df[,c('district','electric_share')]) 
row.names(df) = row.names( plt ) 
plt = SpatialPolygonsDataFrame( plt, df )
plt@data$electric_share = plt@data$electric_share*100
brks.eq = c(0,0.15,0.30,0.45,0.60,0.75,0.90)*100
loc = data.frame(x = 72, y = 28)
coordinates(loc) = ~x+y
pdf( 'input/indus_electric_share_of_irrigation.pdf', width=6, height=6 )
p1 = layout( matrix( c(1,2), 2,1, byrow=TRUE ), widths=c(1), heights=c(0.9,0.1) )
par( mar=c(3,3,3,3), oma = c(2,2,2,2) )
spplot( plt, 'electric_share', 
		at=brks.eq,col.regions=pal, 
		scales = list(draw=TRUE),
		#sp.layout = list('sp.text',loc,labels='India-Pakistan Border', col = 'blue', cex = 1.2 ), 
		colorkey = list(labels=list(at=c(brks.eq),labels=c(brks.eq))), 
		main = expression('Share of tubewells powered by electricity [ % ]'), 
		col="transparent") + 
		layer_(sp.polygons(getData('GADM',country='IND',level=0)[,1],fill="grey75",col=NA)) +
		layer_(sp.polygons(getData('GADM',country='PAK',level=3)[,1],fill="grey85",col=NA)) +
		layer_(sp.polygons(getData('GADM',country='AFG',level=0)[,1],fill="grey95",col=NA)) +
		layer(sp.polygons(getData('GADM',country='IND',level=1)[,1],fill=NA,col='white',lwd=1)) +
		layer(sp.polygons(getData('GADM',country='PAK',level=3)[,1],fill=NA,col='white',lwd=1)) +
		layer(sp.polygons(getData('GADM',country='IND',level=0)[,1],fill=NA,col='blue',lwd=4))
dev.off()