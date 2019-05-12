
# source('C:/Users/parkinso/indus_ix/input_data_scripts/renewable_exclusion_zones.r')
  
require(rgeos)
require(rgdal)
require(raster)
require(ggmap)
require(dplyr)
require(rasterVis)
require(maptools)
memory.limit(size=1e9)

# ISWEL folder for data
setwd( 'P:/is-wel/indus/message_indus' )

# basins to check
basin = c('Indus')

# Define continental tiles from hydroBASINS
cnt_list = c('af', 'as', 'au', 'ca', 'eu', 'na', 'sa')
wkd_dir1 = paste( getwd(), "input/hydrosheds/elevation/", sep = '/' )
wkd_dir2 = paste( getwd(), "input/hydrosheds/flow_accumulation/", sep = '/' )
wkd_dir3 = paste( getwd(), "input/hydrosheds/river_network/", sep = '/' )
wkd_dir4 = paste( getwd(), "input/hydrosheds/flow_direction/", sep = '/' )
cnt_ext = lapply( cnt_list, function(cnt){ extent( raster( paste ( paste( wkd_dir1, c(paste(cnt,'_dem_15s_grid/',paste(cnt,'_dem_15s/',sep=''),paste(cnt, '_dem_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) ) ) } )
names(cnt_ext) = cnt_list

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste0(basin,'_bcu'), verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(basin.sp)=proj4string(basin.spdf)
basin.sp = spTransform( basin.sp, crs( basin.spdf ) )
buff.sp = gBuffer( basin.sp, width=0.1 ) 
buff2.sp = gBuffer( basin.sp, width=10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)


# Check which hydrobasins tiles to include for this basin
tiles_incl = sapply( names(cnt_ext), function(cnt){
	polyg = as( cnt_ext[[cnt]] , 'SpatialPolygons' )
	proj4string(polyg) = proj4string(basin.sp)
	if( unlist( gIntersects( polyg, basin.sp ) ) ){ return( cnt ) } 
	} )
cnt = unlist( tiles_incl[!is.null(tiles_incl)] )

# Elevation raster used to harmonize
elevation.raster = raster( paste( paste( wkd_dir1, c(paste(cnt,'_dem_15s_grid/',paste(cnt,'_dem_15s/',sep=''),paste(cnt, '_dem_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) )
elevation.raster = mask( x = crop( elevation.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( elevation.raster, extent( basin.sp ), snap="out") ) )

## Exclusion 0: Don't install above max elevation and where slope above max slope

slope_max = 11.3 # Eurek et al. (2017) assumes a max slope of 20% or 11.3 degrees for wind
elevation_max = 3500 # Eurek et al. and Pietzcker et al. assume a max elevation of 2500 meters for wind and PV- moved to 3500 to account for high elevation settlements in the Indus

slope.raster = terrain( elevation.raster, opt='slope', unit='degrees', neighbors=8 ) 
exclusion_topo.raster = slope.raster
exclusion_topo.raster[ slope.raster[] < slope_max ] = NA
exclusion_topo.raster[ elevation.raster[] < elevation_max ] = NA
exclusion_topo.raster[ !is.na( exclusion_topo.raster[] ) ] = 1 


## Exclusion 1: Don't install where population density greater than 250 people per square km

# Urban population - using SSP2 in 2060
urbmax.raster = raster(paste('input/NCAR_population/raw/SSP Population Projections/SSP2/Urban/ASCII/SSP2urb',2060,'.txt', sep = ''))
urbmax.raster = mask( x = crop( urbmax.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( urbmax.raster, extent( basin.sp ), snap="out") ) )

# Rural population - using SSP2 in 2060
rurmax.raster = raster(paste('input/NCAR_population/raw/SSP Population Projections/SSP2/Rural/ASCII/SSP2rur',2060,'.txt', sep = ''))
rurmax.raster = mask( x = crop( rurmax.raster, extent( basin.sp ), snap = "out" ), mask = rasterize( basin.sp, crop( rurmax.raster, extent( basin.sp ), snap="out") ) )

# total population and density
totmax.raster = rurmax.raster + urbmax.raster
totdens.raster = totmax.raster / area( totmax.raster )
exclusion_popdens.raster = totdens.raster
exclusion_popdens.raster[ exclusion_popdens.raster[] < 250 ] = NA
exclusion_popdens.raster[ exclusion_popdens.raster[] > 0 ] = 1


## Exclusion 2: Don't install more than 250 km away from the nearest cell with more than 100 people

exclusion_distpop.raster = totdens.raster
exclusion_distpop.raster[ exclusion_distpop.raster[] < 100 ] = NA
exclusion_distpop.raster[ exclusion_distpop.raster[] > 0 ] = 1
exclusion_distpop.raster = distance( exclusion_distpop.raster ) / 1000
exclusion_distpop.raster[ exclusion_distpop.raster[] <= 250 ] = NA
exclusion_distpop.raster[ exclusion_distpop.raster[] > 250 ] = 1 # flag cells with pop greater than 100km away


## Exclusion 3: Don't install in protecte areas

# protected areas from input data file
exclusion_protected.raster = raster('input/protected_area_data/Data/WDPA_July2016-shapefile/highres_wdpa.tiff')
exclusion_protected.raster = mask( x = crop( exclusion_protected.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( exclusion_protected.raster, extent( basin.sp ), snap="out") ) )
exclusion_protected.raster[ exclusion_protected.raster < 1 ] = NA
exclusion_protected.raster[ !is.na(exclusion_protected.raster) ] = 1

## Exclusion 4: Don't install on lakes

# water bodies - lakes
print( 'Working on lakes - takes about 5 mins to import the data!')
lakes = readOGR('input/hydrosheds/hydrolakes/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp','HydroLAKES_polys_v10',verbose=FALSE)
if( basin == 'Zambezi' ){ flt = 'Africa' }
if( basin == 'Indus' ){ flt = 'Asia' }
lakes = lakes[ which( lakes@data$Continent == flt ), ]
lakes = gIntersection(lakes,spTransform(basin.sp,crs(lakes)))
lakes.raster = rasterize(lakes, elevation.raster)
exclusion_lakes.raster = lakes.raster
rm(lakes)
gc()

## Exclusion 5: Don't install over forested area

# Forest cover
urls = unlist(read.table('input/treecover2000.txt',stringsAsFactors=FALSE)[,1])
cntr_urls = do.call( rbind, lapply( urls, function(uu){ 
	y = unlist(strsplit(uu,'_'))[4]
	if(grepl('S',y)){y=-1*as.numeric(unlist(strsplit(y,'S'))[1])}else{y=as.numeric(unlist(strsplit(y,'N'))[1])}
	x = unlist(strsplit(unlist(strsplit(uu,'_'))[5],'[.]'))[1]
	if(grepl('W',x)){x=-1*as.numeric(unlist(strsplit(x,'W'))[1])}else{x=as.numeric(unlist(strsplit(x,'E'))[1])}
	return( data.frame( x = x, y =y ) )  
	} ) )
cntr_urls$id = seq(1,nrow(cntr_urls))
coordinates(cntr_urls) = ~ x + y
gridded(cntr_urls) = TRUE
cntr_urls = raster(cntr_urls)
print( 'Working on forest cover maps')
forest_cover.raster = do.call(merge, lapply( urls[ unlist(extract(cntr_urls,buff2.sp)) ], function(uu){ 
	print( paste( '-----', which( urls[ unlist(extract(cntr_urls,basin.sp)) ] == uu ), 'of', length(urls[ unlist(extract(cntr_urls,basin.sp)) ]), sep = ' ' ) )
	if( !file.exists(paste('input/treecover',unlist(strsplit(uu,'/'))[6],sep='/')) ){ download.file( uu, paste('input/treecover',unlist(strsplit(uu,'/'))[6],sep='/'), mode='wb' ) } 
	return( aggregate( raster( paste('input/treecover',unlist(strsplit(uu,'/'))[6],sep='/') ), fact=20, fun="mean" ) )
	} ) ) 
forest_cover.raster = mask( x = crop( forest_cover.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( forest_cover.raster, extent( basin.sp ), snap="out") ) )	
gc()
exclusion_forest_cover.raster = forest_cover.raster / 100
exclusion_forest_cover.raster[ exclusion_forest_cover.raster <= 0 ] = NA

## Create map showing the percent of grid cell area available for wind / solar
exclusion.list = list( 	exclusion_topo.raster,
						exclusion_popdens.raster,
						exclusion_distpop.raster,
						exclusion_protected.raster,
						exclusion_lakes.raster,
						exclusion_forest_cover.raster )

# harmonize to elevation raster scale using bilinear interpolation
exclusion.stack = stack( lapply( 1:length(exclusion.list), function( iii ){
	r = resample( exclusion.list[[iii]], elevation.raster, method = 'bilinear' ) 
	r[ is.na( r ) ] = 0
	r = mask( x = crop( r, extent( basin.sp ), snap = "out" ), mask = rasterize( basin.sp, crop( r, extent( basin.sp ), snap="out") ) )	
	return(r)	
	} ) )

# Combine the exclusion zone maps
exclusion.raster = sum( exclusion.stack ) 
exclusion.raster[ exclusion.raster[] > 1 ] = 1

# Avaiable area is the inverse of the exclusion zone map
available_area_fraction.raster = 1 - exclusion.raster

# Test plot
pdf( 'input/exclusion_zones.pdf', width = 7, height = 7 )
print( 	levelplot( 	stack( exclusion.stack, exclusion.raster ),
					main = 'Fraction of area unavailable for wind/solar',
					names.attr = c('Population Density','Population Distance','Protected Areas','Lakes','Forest Cover', 'Combined'),
					par.settings = 'BuRdTheme' ) )
dev.off()
			
# Output available area to .asc 
writeRaster( exclusion.raster, filename = 'input/fraction_grid_cell_available_wind_solar.asc', format = 'ascii', overwrite = TRUE )			
			
