
require( rgeos )
require( raster )
require( dplyr )
require( rgdal )
require( ggmap )
require( cartography )

# Increase memory size
memory.limit( size = 1e9 )

# set working drive
setwd( 'P:/is-wel/indus/message_indus' )

# Grab the basin boundaries
basin = 'Indus'
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(basin.sp) = proj4string(basin.spdf)
basin.sp = spTransform( basin.sp, crs( basin.spdf ) )
		
# CSV containing the canal data
canals.df = data.frame( read.csv('input/indus_inter_river_canals.csv', stringsAsFactors=FALSE) )

# Test plot the data
starts = canals.df[,c('in_x','in_y')]
row.names(starts) = as.character( canals.df$name )
names(starts) = c( 'x', 'y' )
ends = canals.df[,c('out_x','out_y')]
row.names(ends) = row.names(starts)
names(ends) = names(starts)
l = vector("list", nrow(starts))
names(l) = row.names(starts)
for (ll in seq_along(l)) {l[[ll]] = Lines( list(Line(rbind(starts[ll,  ], ends[ll,]))), row.names(starts)[ll] )}
net.sldf = SpatialLinesDataFrame( SpatialLines(l), data.frame( capacity = canals.df$capacity_m3_per_sec, row.names = row.names(SpatialLines(l)) ) )
windows()
plot(basin.sp)
lwdt = 10 * ( (net.sldf@data$capacity / max(net.sldf@data$capacity)) ) + 1
plot(net.sldf, add=TRUE, lwd = lwdt, pch = NA, lty = 1 )

# Get PID for in and out
canals.df = cbind( canals.df , do.call( rbind, lapply( 1:nrow(canals.df), function( i ){ data.frame( 
	PID_i = over( spTransform( SpatialPoints( canals.df[i,c('in_x','in_y')], proj4string =crs( basin.sp ) ), crs(  basin.spdf ) ), basin.spdf, byid = TRUE )[,'PID'] ,
	PID_o = over( spTransform( SpatialPoints( canals.df[i,c('out_x','out_y')], proj4string =crs( basin.sp ) ), crs(  basin.spdf ) ), basin.spdf, byid = TRUE )[,'PID']
	) } ) ) ) 

# Remove transfers within the same PID 
ind = which( canals.df$PID_i == canals.df$PID_o )
if( length( ind ) > 0 ){ canals.df = canals.df[ -1 * ind, ] }

write.csv( canals.df, 'input/indus_bcu_canals.csv', row.names=FALSE )

