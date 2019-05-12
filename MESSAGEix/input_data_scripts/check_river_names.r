# source( paste( Sys.getenv("INDUS_IX_PATH"), 'basin_msggdx.r', sep = '/' ) )
 

 
##### INITIALIZE #####
 

 
print( 'Initializing functions and data' )
 

 
rm( list = ls( ) )
 
graphics.off( )
 
require( raster )
 
require( dplyr )
 
require( tidyr )
 
require( rgdal )
 
require( maptools )
 
require( rgeos )
 

 

 
# Location of input data
 
setwd( 'P:/is-wel/indus/message_indus' )
 

 
# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
 
indus_ix_path = Sys.getenv("INDUS_IX_PATH")
 

 
# Basin analyzed
 
basin = 'Indus'
 

 
# SSP being analyzed
 
SSP = 'SSP2'
 

 
# Time
 
year = c( seq( 1990, 2010, by = 10  ), 2015, seq( 2020, 2060, by = 10 ) ) 
 
time =  c( seq(1, 12, by = 1 ) ) # monthly time steps
 
year_all = year
 
baseyear = 2015 # last historical year
 
#duration_time = round( c( 31,28,31,30,31,30,31,31,30,31,30,31 )/365, digits =3 )
 
duration_time = rep( 1, length( time ) )
 

 

 
# Import the basin delineation as a spatialpolygonsdataframe (.spdf) 
 
# each polygon has a unique ID (PID) and represents the intersection between countries and catchments (i.e., basin country units - bcus)
 
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )
 
basin.spdf$BASIN = basin
 
bcus = as.character( basin.spdf@data$PID )
 
node = c( bcus, unique( as.character( basin.spdf@data$BASIN ) ) ) 
 

 
basin_red.sp = gUnaryUnion( basin.spdf )
 
basin_red.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin_red.sp@polygons[[1]]@Polygons),ID=1)))
 
proj4string( basin_red.sp ) = proj4string( basin.spdf )
 
basin_red.sp = spTransform( basin_red.sp, crs( basin.spdf ) )
 

 
as_riv_15s.spdf = spTransform( crop( readOGR( 'input/hydrosheds/river_network/as_riv_15s', 'as_riv_15s', verbose = FALSE ), extent( basin_red.sp ) ), crs(basin_red.sp) )    
 
as_riv_15s.spdf = raster::intersect( as_riv_15s.spdf[ which( !is.na( over( as_riv_15s.spdf, basin_red.sp ) ) ), ]  , basin_red.sp )
 
as_riv_15s.spdf@data$UP_CELLS = as.numeric( as.character( as_riv_15s.spdf@data$UP_CELLS ) )
 
as_riv_15s_red.spdf = as_riv_15s.spdf[ which( as_riv_15s.spdf@data$UP_CELLS > quantile( as_riv_15s.spdf@data$UP_CELLS, 0.9 ) ), ]
 

 
# Get river names and groupings from csv
 
map_rivers_to_pids.df = data.frame( read.csv( 'input/indus_map_rivers_to_pids.csv', stringsAsFactors = FALSE ) )
 
basin.spdf@data = merge( basin.spdf@data, map_rivers_to_pids.df, by = 'PID' )
 

 
# Thickness of river line according to upstream cells
 
qntls = quantile( as_riv_15s_red.spdf@data$UP_CELLS, c(0.25,0.5,0.75,1) )
 
as_riv_15s_red.spdf@data$lwd = unlist( lapply( as_riv_15s_red.spdf@data$UP_CELLS, function( i ){
 
  
 
  if( i < qntls[1] ){ lwd = 1 }
 
    
 
  if( i >= qntls[1] & i < qntls[2] ){ lwd = 1.5 }  
 
    
 
  if( i >= qntls[2] & i < qntls[3] ){ lwd = 2 }  
 
    
 
  if( i >= qntls[3] ){ lwd = 2.5 }    
 
  
 
  return(lwd)  
 
  
 
  } ) )
 

 
windows()  
 
plot(basin.spdf)
 
plot(as_riv_15s_red.spdf, add=TRUE, col = 'deepskyblue', lwd=as_riv_15s_red.spdf@data$lwd)
 
text(SpatialPoints(gCentroid(basin.spdf,byid=TRUE)),basin.spdf@data$type_1, cex=0.6)  
 