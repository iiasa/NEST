
require(reshape)	
require(maptools)
require(countrycode)
require(raster)
require(sf)
require(dplyr)
require(rgeos)
require(rgdal) 
require(rasterVis)
require(ncdf4)
require(overpass)
memory.limit(size=1e9)

basin = c('Indus')

setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
buff.sp = gBuffer( basin.sp, width=2 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)

### Irrigation in each PID
n_days = c(31,28,31,30,31,30,31,31,30,31,30,31)
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4', verbose=FALSE)
irrigation.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4' )
extent(irrigation.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( irrigation.stack ) = proj4string( basin.spdf )
irrigation.stack = crop( irrigation.stack, extent(buff.sp) )
names(irrigation.stack) = c( sapply( 1:(nlayers(irrigation.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
irrigation.stack = irrigation.stack[[ c( which(grepl( 'X2010',names(irrigation.stack) )) ) ]] # keep 2010
nm = names(irrigation.stack)
irrigation.stack = ( 1e6 * irrigation.stack ) / (1e6 * area(irrigation.stack[[1]]) ) # convert to meters
nc_close(nc)
irrigation.stack[irrigation.stack[]<0]=0
names(irrigation.stack) = nm
irrigation.stack = round( 1e6 * irrigation.stack * area( irrigation.stack[[1]] ) / n_days ) # in m3 per day
irrm.raster = mean(irrigation.stack)
irrw.df = data.frame( do.call( cbind, lapply(1:12, function(m){ unlist( lapply( 1:length(basin.spdf), function(x){ sum( unlist( extract( irrigation.stack[[m]], as(basin.spdf[x,],'SpatialPolygons'), na.rm=TRUE ) ) ) } ) ) })) )
names(irrw.df) = nm
row.names(irrw.df) = basin.spdf@data$PID

#### Get irrigation outside basin boundaries

# Canals
canal.sl = spTransform( as( readOGR( paste( getwd(), 'input/indusbasinorg', sep = '/' ), 'canals', verbose = FALSE ), 'SpatialLines' ), crs( basin.spdf ) )

# Remove grid cells included in the basin already
basin_cells = extract( irrm.raster, basin.sp, na.rm=TRUE, cellnumbers = TRUE )[[1]][,'cell']
irrm.raster[ basin_cells ] = NA
irrm.raster[ irrm.raster[] < 0.1 ] = NA
irrm.raster[ !is.na(irrm.raster[]) ] = 1
irrm.sp = as( rasterToPolygons(irrm.raster), 'SpatialPolygons' )

# Intersect to identify irrigated grid cells outside basin
canal.sp = irrm.sp[ c( unique( unlist( apply( gDistance( irrm.sp, canal.sl , byid=TRUE ), 1, function(x){ which( x < 0.3 ) } ) ) ) ) ]

# Get adjacent basin unit in pakistan
basinr.spdf = basin.spdf[ which( as.character( basin.spdf@data$REGION ) == 'pakistan' ) , ] 
canal.df = data.frame( adj_pid = unlist( lapply( 1:length(canal.sp), function(i){ return( as.character( basinr.spdf@data$PID[ which.min( gDistance( canal.sp[i], basinr.spdf, byid = TRUE ) ) ] ) ) } ) ), row.names = row.names(canal.sp) )
canal.sp = SpatialPolygonsDataFrame( canal.sp, canal.df )
canal.sp = gUnaryUnion( canal.sp, id = canal.sp@data$adj_pid )	
ids = seq(1,length(canal.sp),by=1)
inds = which( gTouches(canal.sp,byid=TRUE)[1:ceiling(length(canal.sp)/2),], arr.ind = TRUE )
if( nrow(inds) > 0 ){ for( j in 1:nrow(inds) ){ ids[ inds[j,2] ] = ids[ inds[j,1] ]  } }
canal.sp = spTransform( gUnaryUnion( canal.sp, id = ids ), crs(basin.spdf) )
canal.sp = disaggregate( canal.sp )
row.names( canal.sp ) = as.character( basin.spdf@data$PID[ apply( gDistance( gCentroid(canal.sp,byid=TRUE), basin.spdf, byid = TRUE ), 2, which.min ) ] )

# Convert to demands
irrw_out.df = 1e-6 * data.frame( do.call( cbind, lapply(1:12, function(m){ unlist( lapply( 1:length(canal.sp), function(x){ sum( unlist( extract( irrigation.stack[[m]], as(canal.sp[x,],'SpatialPolygons'), na.rm=TRUE ) ) ) } ) ) })) )
names(irrw_out.df) = nm
row.names(irrw_out.df) = row.names( canal.sp )
	
# Indira Ghandi Canal
ig_loc = SpatialPoints( data.frame( x = 74.946981, y = 31.144931 ) ) 
proj4string( ig_loc ) = proj4string( basin.sp )
ig_loc = spTransform( ig_loc, crs( basin.sp ) )
ig_pid = as.character( over( ig_loc, basin.spdf )$PID )
ig_average_flow = 138 * 60 * 60 * 24 / 1e6  # m3 per second converted to MCM per day

# Sutlej-Yamuna link canal - based on allocation of 3.5 MAF to Haryana
syl_loc = SpatialPoints( data.frame( x = 76.366436, y = 31.380952 ) ) 
proj4string( syl_loc ) = proj4string( basin.sp )
syl_loc = spTransform( syl_loc, crs( basin.sp ) )
syl_pid = as.character( over( syl_loc, basin.spdf )$PID )
syl_average_flow = 136 * 60 * 60 * 24 / 1e6  # m3 per second converted to MCM per day

# Estimate average seasonality of irrigation demands and apply to canals transfers
ssn = ( colSums( irrw_out.df ) / mean( colSums( irrw_out.df ) ) )

# Output to csv
irrigation_transfers.df = rbind( irrw_out.df, ssn * ig_average_flow, ssn * syl_average_flow )
row.names( irrigation_transfers.df ) = 1:nrow( irrigation_transfers.df )
irrigation_transfers.df$PID = c( row.names( irrw_out.df ), ig_pid, syl_pid	)
irrigation_transfers.df$units = 'MCM/day'
irrigation_transfers.df = irrigation_transfers.df[,c(13,14,1:12)]							
irrigation_transfers.df[,3:(3+11)] = round( irrigation_transfers.df[,3:(3+11)], digits = 1 )
irrigation_transfers.df = irrigation_transfers.df[-1*which(rowSums(irrigation_transfers.df[,3:(3+11)]) < 1 ), ]
		
write.csv( irrigation_transfers.df, 'input/basin_irrigation_transfers.csv', row.names = FALSE)		
		
		