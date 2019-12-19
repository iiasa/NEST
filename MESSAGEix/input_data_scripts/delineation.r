
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(countrycode)
library(ggmap)

# Increase memory size
memory.limit(size = 1e9)

# set working drive
setwd( 'P:/is-wel/indus/message_indus' )

# Set basin name
basin = 'Indus'

######################################
# Get basin units from Hydrobasins DB
######################################

# Mapping of names of the basins to be included (from FAO basin naming convention)
hydrobasins_names_mapping.df = data.frame(read.csv('input/hydrosheds/hydrobasins/hydroBASINS_names.csv', header = TRUE, stringsAsFactors = FALSE))
hydrobasins_1.spdf = readOGR('input/hydrosheds/hydrobasins/delineated_basins', 'hydrobasins_level_1', verbose=FALSE, stringsAsFactors = FALSE)
hydrobasins_2.spdf = readOGR('input/hydrosheds/hydrobasins/delineated_basins', 'hydrobasins_level_2', verbose=FALSE, stringsAsFactors = FALSE)
hydrobasins_3.spdf = readOGR('input/hydrosheds/hydrobasins/delineated_basins', 'hydrobasins_level_3', verbose=FALSE, stringsAsFactors = FALSE)
hydrobasins_4.spdf = readOGR('input/hydrosheds/hydrobasins/delineated_basins', 'hydrobasins_level_4', verbose=FALSE, stringsAsFactors = FALSE)

# Get the basin spdf using the name mapping
hybas_ids = hydrobasins_names_mapping.df$HYBAS_ID[which(hydrobasins_names_mapping.df$NAME == basin)]
pfaf_ids = hydrobasins_1.spdf@data$PFAF_ID[hydrobasins_1.spdf@data$HYBAS_ID == hybas_ids] 
basin.spdf = hydrobasins_1.spdf[which(floor(as.numeric(hydrobasins_1.spdf@data$PFAF_ID)) %in% pfaf_ids),]
basin.sp = spTransform( as( basin.spdf, 'SpatialPolygons' ), crs( basin.spdf ) )

# Get lower level catchments
basin2.spdf = hydrobasins_2.spdf[ which( !is.na( over( gCentroid( hydrobasins_2.spdf, byid=TRUE ), basin.sp ) ) ), ]
basin3.spdf = hydrobasins_3.spdf[ which( !is.na( over( gCentroid( hydrobasins_3.spdf, byid=TRUE ), basin.sp ) ) ), ]
basin4.spdf = hydrobasins_4.spdf[ which( !is.na( over( gCentroid( hydrobasins_4.spdf, byid=TRUE ), basin.sp ) ) ), ]

# Indus specifics
if( basin == 'Indus' )
	{
	
	# Manually checked
	bsn_lo.spdf = basin2.spdf[ which( as.numeric( basin2.spdf@data$HYBAS_ID ) == 4040794730 ) , ]
	bsn_lo.sp = spTransform( as( bsn_lo.spdf, 'SpatialPolygons' ), crs( bsn_lo.spdf ) )
	
	# Remaining units
	bsn_lo2.spdf = basin2.spdf[ which( as.numeric( basin2.spdf@data$HYBAS_ID ) != 4040794730 ) , ]
	bsn_lo2.sp = spTransform( as( bsn_lo2.spdf, 'SpatialPolygons' ), crs( bsn_lo2.spdf ) )
	
	# Manually checked
	bsn_md.spdf = basin3.spdf[ which( as.numeric( basin3.spdf@data$HYBAS_ID ) == 4050786190 ) , ]
	bsn_md.sp = spTransform( as( bsn_md.spdf, 'SpatialPolygons' ), crs( bsn_md.spdf ) )
	
	# Join other units
	bsn_md2.spdf = basin3.spdf[ which( 	as.numeric( basin3.spdf@data$HYBAS_ID ) != c( 4050786190 ) &
										!is.na( over( gCentroid( basin3.spdf, byid=TRUE ), bsn_lo.sp ) ) ), ]
	bsn_md2.sp = spTransform( gUnaryUnion( as( bsn_md2.spdf, 'SpatialPolygons' ) ), crs( bsn_md2.spdf ) )
	
	# Manually checked units to include
	bsn_hi.spdf = basin4.spdf[ which( as.numeric( basin4.spdf@data$HYBAS_ID ) %in% c( 4060746720,4060729320, 4060729330 ) ), ]
	bsn_hi.sp = spTransform( as( bsn_hi.spdf, 'SpatialPolygons' ), crs( bsn_hi.spdf ) )
	
	# Join other units
	bsn_hi2.spdf = basin4.spdf[ which( 	!( as.numeric( basin4.spdf@data$HYBAS_ID ) %in% c( 4060746720,4060729320, 4060729330 ) ) &
										!is.na( over( gCentroid( basin4.spdf, byid=TRUE ), bsn_md.sp ) ) ), ]
	bsn_hi2.sp = spTransform( gUnaryUnion( as( bsn_hi2.spdf, 'SpatialPolygons' ) ), crs( bsn_hi2.spdf ) )
	
	# windows()
	# plot(bsn_lo2.sp)
	# plot(bsn_md2.sp,add=TRUE,col='green')
	# plot(bsn_hi.sp,add=TRUE,col='red')
	# plot(bsn_hi2.sp,add=TRUE,col='blue')
	
	# Combine into basin units
	basin_units.sp = rbind( bsn_hi.sp, bsn_hi2.sp, bsn_md2.sp, bsn_lo2.sp, makeUniqueIDs=TRUE )
	
	
	}else{
	
	# Use level 2
	basin_units.sp = spTransform( as( basin2.spdf, 'SpatialPolygons' ), crs( basin2.spdf ) )
	
	}

###########################	
# Get the riparian nations
############################

gadm.spdf = readOGR('input/country_delineation/gadm/output_data', 'gadm_country_boundaries', verbose=FALSE)

# First filter using extents
riparian_nations.spdf = gadm.spdf[ 	which( !is.na( unlist( lapply( 1:length( gadm.spdf ), function( iii ){ if( !is.null( intersect( extent( gadm.spdf[iii,] ), extent( basin.sp ) ) ) ){ return( iii ) }else{ return(NA) } } ) ) ) ), ]	

# Second filter using gIntersects - much faster than intersecting the whole global map
riparian_nations.spdf = riparian_nations.spdf[ which( gIntersects( riparian_nations.spdf, basin.sp, byid = TRUE ) ), ]

# Indus specifics - remove nepal
if( basin == 'Indus' ){ riparian_nations.spdf = riparian_nations.spdf[ -1*which( riparian_nations.spdf@data$ISO == 'NPL' ), ] }

row.names( riparian_nations.spdf ) = as.character( riparian_nations.spdf@data$ISO )

######################################################################################	
# Create new polygons representing the intersection between countries and catchments
######################################################################################

# Harmonize and intersect
riparian_nations.sp = spTransform( as( riparian_nations.spdf, 'SpatialPolygons' ), crs(basin_units.sp) )
bcus.sp = raster::intersect( riparian_nations.sp, gBuffer( basin_units.sp , byid=TRUE, width=0 ) )
row.names(bcus.sp) = c( as.matrix( sapply( names(bcus.sp), function( nnn ){ paste( names( riparian_nations.sp )[ as.numeric( unlist( strsplit( nnn, '[ ]' ) ) )[1] ], unlist( strsplit( nnn, '[ ]' ) )[2], sep='_' ) } ) ) )

# Dissaggregate non-touching polygons
temp = disaggregate( SpatialPolygonsDataFrame(  bcus.sp, data.frame( ID = row.names( bcus.sp ), row.names = row.names( bcus.sp ) ) ), byid=TRUE )
newid = unlist( strsplit( as.character( temp@data$ID ), '_' ) )[seq(1,2*length(temp),by=2)]
for( j in unique(newid) ){ newid[ which( newid == j ) ] = paste( j, seq(1,length(which( newid == j )),by=1), sep='_' ) }
temp@data$ID = newid
row.names(temp) = newid
bcus.sp = spTransform( as(temp,'SpatialPolygons'), crs(basin_units.sp) )

# Dissolve smaller
pcheck = names( bcus.sp )[ which( gArea(bcus.sp,byid=TRUE) < 0.1 ) ]
temp = bcus.sp
if( length( pcheck ) > 0 )
	{
	for( j in pcheck )
		{
		
		# Delete small polygon from spdf
		temp = temp[ -1*which(names(temp)==j) ]
		
		# Check if small polygon touching another polygon, if touching dissolve, else remove
		if( length( which( gTouches( bcus.sp[ j ], temp, byid = TRUE ) ) ) > 0 )
			{
			
			# Find  and isolate largest neighbor
			big = temp[ which( gTouches( bcus.sp[ j ], temp, byid = TRUE ) ) ]
			
			# Keep neighbors from same country if possible
			if( length( which( grepl( strsplit( j, '_')[[1]][1], names(big) ) ) ) > 1 ){ big = big[ which( grepl( strsplit( j, '_')[[1]][1], names(big) ) ) ] }
			
			# Largest neighbour
			big = big[ which.max( gArea( big ) ) ]
			
			# Isolate small polygon
			sml = bcus.sp[ j ]
			
			# Combine small and large polygon maintaining the large polygon attributes
			cmb.sp = gUnaryUnion(rbind(big,sml,makeUniqueIDs=TRUE))
			row.names(cmb.sp) = row.names(big)
			
			# Remove big and add in the dissolved polygon back into the psdf
			temp = rbind( temp[ -1*which( names(temp) == names(big) ) ], cmb.sp )
			
			}
		}
	}	
	
# Reset	and rename
newid = unlist( strsplit( names(temp), '_' ) )[seq(1,2*length(temp),by=2)]
for( j in unique(newid) ){ newid[ which( newid == j ) ] = paste( j, seq(1,length(which( newid == j )),by=1), sep='_' ) }
row.names(temp) = newid
df = data.frame( 	t1 = unlist( strsplit( names(temp), '_' ) )[seq(1,2*length(temp),by=2)], 
					t2 = unlist( strsplit( names(temp), '_' ) )[seq(2,2*length(temp),by=2)] )
temp = temp[ order(df$t1,df$t2 ) ]
bcus.sp = temp

######################################
# Reduced form flow network 
######################################

# Flow accumulation tiles from hydrobasins
wkd_dir2 = c("input/hydrosheds/flow_accumulation/")
tiles = c('af','as','au','ca','eu','na','sa')
cnts = tiles[ which(  sapply( tiles, function(cnt) {
	
	ext = as( extent( stack( paste( paste( wkd_dir2, c(paste(cnt,'_acc_15s_grid/',paste(cnt,'_acc_15s/',sep=''),paste(cnt, '_acc_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) ) ), 'SpatialPolygons' )
	proj4string(ext) = proj4string( basin.sp )
	
	if( gIntersects( spTransform( ext, crs(basin.sp) ), basin.sp ) ){ return( TRUE ) }else{ return( FALSE ) }  
	
	} ) ) ]
	
# Get flow accumulation for tiles and merge (if multiple)
flow_accumulation.raster = do.call( merge, lapply( cnts, function(cnt){ 

	res = stack( paste( paste( wkd_dir2, c(paste(cnt,'_acc_15s_grid/',paste(cnt,'_acc_15s/',sep=''),paste(cnt, '_acc_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) ) 
	
	res = mask( x = crop( res, extent( basin.sp ), snap = "out" ), mask = rasterize( spTransform( basin.sp, crs( res ) ), crop( res, extent( basin.sp ), snap="out") ) )

	return( res )
	
	} ) )
	
# Find downstream polygon using flow accumulation
downstream.df = do.call( rbind, lapply( names( bcus.sp ), function( ppp ){
	
	# Mask for bcu
	bcu.raster = rasterize( spTransform( bcus.sp[ ppp ], crs( flow_accumulation.raster ) ), flow_accumulation.raster )
	bcu.raster[ bcu.raster > 0 ] = 1
	bcu.raster[ is.na(bcu.raster) ] = 0
	
	# Leftover flow accumulation
	mskd = flow_accumulation.raster * bcu.raster
	
	# Get adjacent cell to max flow accumulation within BCU
	around = matrix( c(	1, 1, 1, 
						1, 0, 1, 
						1, 1, 1 ), ncol = 3, byrow=TRUE )
	adj = data.frame( adjacent( flow_accumulation.raster, cell = which.max( mskd[] ), directions=around, sorted=TRUE, include = TRUE ) )
	
	# outlet location
	outlet = data.frame( xyFromCell( flow_accumulation.raster, which.max( mskd[] ) ) )
	
	# Downstream has mos flow accumulation
	to = SpatialPoints( xyFromCell( flow_accumulation.raster, adj$to[which.max( c( flow_accumulation.raster[ adj$to ] - max( mskd[], na.rm=TRUE ) ) )] ) )
	proj4string(to) = proj4string(bcus.sp)
	to = spTransform( to, crs( bcus.sp ) )
	down = names(bcus.sp)[ over( to, bcus.sp )[ which( over( to, bcus.sp ) != which( names( bcus.sp ) == ppp ) ) ] ]
	if( length(down) == 0 ){ down = NA }
	
	# Test plot for tracking up and downstream
	# if( !is.na( down ) )
		# {
		# windows()
		# plot( basin.sp )
		# plot( bcus.sp[ppp],add=TRUE,col='red')
		# plot( bcus.sp[down],add=TRUE,col='blue')
		# legend( 'bottomright', c('upstream','downstream'), fill = c('red','blue'), bty = 'n', cex = 0.9 )
		# }else{
		# windows()
		# plot( basin.sp )
		# plot( bcus.sp[ppp],add=TRUE,col='purple')
		# legend( 'bottomright', c('outlet'), fill = c('purple'), bty = 'n', cex = 0.9 )
		# }
		
	return(data.frame( down = down, outx = outlet$x, outy = outlet$y ))
	
	} ) )
row.names( downstream.df ) = names( bcus.sp )	

####################################################
# Specific regional aggregation (used for calibration)	
#######################################################

# Default to basin name
region = rep( basin, length(bcus.sp) )

# Indus specific - for implementation of Indus Water Treaty Allocations
if( basin == 'Indus' )
	{
	
	# Grab the basin boundaries identified manually previously
	basint.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'indus_basin_units', verbose = FALSE )
	basint.spdf@data$river = 'pakistan'
	basint.spdf@data$river[which(basint.spdf@data$PID%in%c(2215, 2242, 2244, 2245, 2247))] = 'china'
	basint.spdf@data$river[which(basint.spdf@data$PID%in%c(2216, 2217, 2218, 2219, 2221, 2222, 2223, 2243, 2246))] = 'india_east' 
	basint.spdf@data$river[which(basint.spdf@data$PID%in%c(2249, 2254, 2255))] = 'india_west' 
	basint.spdf@data$river[which(basint.spdf@data$PID%in%c(2213, 2251))] = 'afghan_south' 
	basint.spdf@data$river[which(basint.spdf@data$PID%in%c(2252))] = 'afghan_north'

	region = over( gCentroid( bcus.sp, byid=TRUE ), spTransform( basint.spdf, crs(bcus.sp) ) )$river

	}
	
##########################################
# Output as spatial polygons data frame	
##########################################

bcus.df = data.frame( 	PID = names( bcus.sp ),
						OUTX = downstream.df$outx,
						OUTY = downstream.df$outy,
						DOWN = downstream.df$down, 
						REGION = region,
						row.names = names( bcus.sp ) )
bcus.spdf = SpatialPolygonsDataFrame( bcus.sp, bcus.df )
							
# Write to file	
writeOGR(
		bcus.spdf, 
		'input', 
		paste( basin, 'bcu', sep = '_' ),
		driver="ESRI Shapefile", 
		overwrite_layer=TRUE
		)

################
# Test plots
################

bcus.spdf = readOGR( 'input', paste( basin, 'bcu', sep = '_' ) )
bcus.sp = as( bcus.spdf, 'SpatialPolygons')

## Plot 1 - Map of indus region with river, elevation and key cities

# Get river network
basin_red.sp = gUnaryUnion( bcus.sp )
basin_red.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin_red.sp@polygons[[1]]@Polygons),ID=1)))
proj4string( basin_red.sp ) = proj4string( bcus.sp )
basin_red.sp = spTransform( basin_red.sp, crs( bcus.sp ) )
as_riv_15s.spdf = spTransform( crop( readOGR( 'input/hydrosheds/river_network/as_riv_15s', 'as_riv_15s', verbose = FALSE ), extent( basin_red.sp ) ), crs(basin_red.sp) )		
as_riv_15s.spdf = raster::intersect( as_riv_15s.spdf[ which( !is.na( over( as_riv_15s.spdf, basin_red.sp ) ) ), ]	, basin_red.sp )
as_riv_15s.spdf@data$UP_CELLS = as.numeric( as.character( as_riv_15s.spdf@data$UP_CELLS ) )
as_riv_15s_red.spdf = as_riv_15s.spdf[ which( as_riv_15s.spdf@data$UP_CELLS > quantile( as_riv_15s.spdf@data$UP_CELLS, 0.9 ) ), ]

# Thickness of river line according to upstream cells
qntls = quantile( as_riv_15s_red.spdf@data$UP_CELLS, c(0.25,0.5,0.75,1) )
as_riv_15s_red.spdf@data$lwd = unlist( lapply( as_riv_15s_red.spdf@data$UP_CELLS, function( i ){
	
	if( i < qntls[1] ){ lwd = 1 }
		
	if( i >= qntls[1] & i < qntls[2] ){ lwd = 1.5 }	
		
	if( i >= qntls[2] & i < qntls[3] ){ lwd = 2 }	
		
	if( i >= qntls[3] ){ lwd = 2.5 }		
	
	return(lwd)	
	
	} ) )

## Elevation
elevation.raster = raster('input\\hydrosheds\\elevation\\as_dem_15s_grid\\as_dem_15s\\as_dem_15s\\w001001.adf')
elevation.raster = mask( x = crop( elevation.raster, extent( basin_red.sp ), snap = "out" ), mask = rasterize(	basin_red.sp, crop( elevation.raster, extent( basin_red.sp ), snap="out") ) )
proj4string(elevation.raster) = proj4string( basin_red.sp )

## Cities
cities = c(	#'Jammu', 
			#'Srinagar', 
			#'Chandigarh',
			#'Karachi', 
			#'Lahore', 
			#'Faisalabad',
			#'Hyderabad Pakistan',
			#'Rawalpindi',
			'Islamabad', 
			#'Gujranwala', 
			'Kabul' )
cities2 = c(#'Jammu', 
			#'Srinagar', 
			#'Chandigarh',
			#'Karachi', 
			#'Lahore', 
			#'Faisalabad',
			#'Hyderabad',
			#'Rawalpindi',
			'Islamabad', 
			#'Gujranwala', 
			'Kabul' )

dlab = data.frame( dlat = c(-0.4,0.4,-0.4), dlon = c(0.75,-1.2,-1) )			
labloc =  c(#1, 
			#1, 
			#1,
			#3, 
			#1, 
			#2,
			#2,
			#1,
			1, 
			#2, 
			1 )
			
cities.spdf = do.call( rbind, lapply( cities, function(ccc){
	j = 1
	while( j == 1 )
		{
		res = geocode( ccc )
		if( !is.na(unlist(res)[1]) ){ j = 0 }else{ Sys.sleep(0.5) }
		}
	row.names(res) = cities2[ which( cities == ccc ) ]
	return(res)
	} ) )	
cities.spdf$city = cities2
row.names(cities.spdf) = cities2
coordinates( cities.spdf ) = ~lon+lat
proj4string(cities.spdf) = proj4string(basin_red.sp)
cities.spdf = spTransform( cities.spdf, crs(basin_red.sp) )
cities.spdf@data$num = 1
cities.df = data.frame( cities.spdf )
cities.df$dlat = dlab$dlat[labloc]
cities.df$dlon = dlab$dlon[labloc]
cities_lab.df = cities.df %>%
					mutate( lat = lat + dlat ) %>%
					mutate( lon = lon + dlon )
cities_lab.spdf = cities_lab.df 
coordinates( cities_lab.spdf ) = ~lon+lat
proj4string(cities_lab.spdf) = proj4string(basin_red.sp)
cities_lab.spdf = spTransform( cities_lab.spdf, crs(basin_red.sp) )		
			

islamabad = data.frame(x=73.0479,y=33.6844)
coordinates( islamabad ) = ~x+y
			
## Plot
colbreaks = c(0,200,500,1000,2000,4000,max(elevation.raster[],na.rm=TRUE))
colregions = alpha( c('darkolivegreen3','darkolivegreen1','khaki1','tan1','tan2','tan3'), alpha = 0.6 )

# Country borders
cnt.spdf = do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) )
cnt.spdf = gSimplify( cnt.spdf, tol = 0.1 )
cnt2.sp = gUnaryUnion(cnt.spdf)
cnt2.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},cnt2.sp@polygons[[1]]@Polygons),ID=1)))
proj4string(cnt2.sp) = proj4string(basin_red.sp)
cnt2.sp = spTransform( cnt2.sp, crs( basin_red.sp ) )

# Buffer
buff.sp = buffer( basin_red.sp, width = 0.4 )
proj4string(buff.sp) = proj4string(bcus.spdf)
buff.sp = spTransform(buff.sp, crs(bcus.spdf))

## Label for arabian sea
sea_label.spdf = data.frame( coords.x1 = 66.1, coords.x2 = 23.5, sea = 'Arabian Sea' )
coordinates( sea_label.spdf ) = ~coords.x1+coords.x2
proj4string(sea_label.spdf) = proj4string(basin.sp)
sea_label.spdf = spTransform( sea_label.spdf, crs(basin.sp) )	

# Canals
canal.spdf = spTransform( crop( readOGR( 'input/indusbasinorg', 'canals', verbose = FALSE ), extent( bcus.spdf ) ), crs(bcus.spdf) )		

# Add Indira Gandhi
ind_gan.sl = SpatialLines( list( Lines( list( Line( data.frame( read.csv( 'input/indira_gandhi.csv' )  ) ) ), ID = 1 ) ) )
proj4string(ind_gan.sl) =proj4string(basin.sp)
ind_gan.sl = spTransform( ind_gan.sl, crs(basin.sp))

# River labels
rivers = c('Indus', 'Chenab', 'Jhelum', 'Ravi', 'Beas', 'Sutlej','Kabul','Gomal')

#colregions = alpha( colorRampPalette( c('darkolivegreen3', 'tan1') )(length(colbreaks)), alpha = 1 )
pdf( 'input/indus_map_base_2.pdf', width=6, height=6 )
plot( 	buff.sp, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 0.8,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		xlab = expression("Longitude ["~degree~"East ]"), 
		ylab = expression("Latitude ["~degree~"North ]"), 
		mgp = c(2.5,0.5,0)
		)
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)	
#plot(admin.spdf, border  = "white", col="gray76", add=TRUE)		
plot(	elevation.raster,
		breaks=colbreaks,
		col=colregions,
		add=TRUE, legend = FALSE )
plot(basin_red.sp,add=TRUE, col = NA, border = 'red', lwd=2)	
plot( 	do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) ), 
		col = alpha( c('khaki','lightgreen','lightblue','lightcoral','grey75','grey75','grey75','grey75','grey75'), alpha = 0.2 ),
		border = 'gray25', add = TRUE )	
plot(as_riv_15s_red.spdf, add=TRUE, col = 'deepskyblue', lwd=as_riv_15s_red.spdf@data$lwd)	
text( 65.75, 26.5, 'PAK', col = 'forestgreen',cex=1.5)
text( 76, 29, 'IND', col = 'brown',cex=1.5)
text( 82.5, 34, 'CHN', col = 'darkred',cex=1.5)
text( 67, 36, 'AFG', col = 'navy',cex=1.5)
#plot(canal.spdf,add=TRUE,col=alpha('red',alpha=0.8),lwd=0.5)
#plot(ind_gan.sl,add=TRUE,col=alpha('red',alpha=0.8),lwd=0.5)
text( sea_label.spdf, label = sea_label.spdf@data$sea, col = 'navy', cex = 0.65, font = 3 )
#text( admin_lab.sp, label = row.names(admin_lab.sp), col = alpha('gray25',alpha=0.7), cex = 0.7, font = 2 )
points( islamabad, pch = 21, bg = 'white', col = 'black', cex=1.2 )
text( data.frame(islamabad) %>% mutate( x = x-0.75, y = y-0.4 ) %>% 'coordinates<-'(~x+y), label = 'Islamabad', col = 'black', cex = 0.8, font = 2 )
#points( cities.spdf, pch = 21, bg = 'white', col = 'black' )
#text( cities_lab.spdf, label = cities_lab.spdf@data$city, col = 'black', cex = 0.6, font = 2 )
box()		 
axis(side=1, cex.axis = 0.8)
axis(side=2, cex.axis = 0.8)
legend( 'bottomright', 
		legend = c('0-200','200-500','500-1000','1000-2000','2000-4000', '> 4000'), 
		fill = colregions, 
		ncol = 1,  
		title = 'Elevation [ m ]', 
		bty = 'n',
		cex = 0.75 )
legend( 'topright', 
		legend = c('Basin boundary', 'River channel', 'Country border'),
		lty = 1,
		lwd = c(2,3,1.5),		
		col = c('red','deepskyblue','gray25'), 
		seg.len = 3,
		ncol = 1,  
		bty = 'n',
		cex = 0.8 )		
scalebar(500, type = 'bar', xy = c(72, 24), divs = 5, cex = 0.75, lonlat = TRUE )
text( 74.75, 23.65, label = 'Distance [ km ]', cex = 0.75 ) 
# text( 73.75, 23.65, label = 'Distance [ km ]', cex = 0.75 ) 
# arrows( 65 , 35, 67, 35, length = 0.1, angle = 30 )
# text( 67.35, 35, label = 'E', cex = 0.7, font = 1 ) 
# arrows( 65 , 35, 65, 37, length = 0.1, angle = 30 )  
# text( 65, 37.35, label = 'N', cex = 0.7, font = 1 ) 
dev.off()

# Irrigation command areas are identified from the gridded irrigation withdrawals harmonized  in Cheema et al. 2014
IRR_cw.raster = raster( 'input/IRR_cw.img' )
IRR_cw.raster = projectRaster( setValues(raster(IRR_cw.raster),IRR_cw.raster[]), crs = crs( basin.spdf ) )
IRR_cw.raster[ ! IRR_cw.raster[] > 0 ] = NA
IRR_gw.raster = raster( 'input/IRR_gw.img' )
IRR_gw.raster = projectRaster( setValues(raster(IRR_gw.raster),IRR_gw.raster[]), crs = crs( basin.spdf ) )
IRR_gw.raster[ ! IRR_gw.raster[] > 0 ] = NA
IRR_gw.raster = resample( IRR_gw.raster, IRR_cw.raster, method = 'bilinear' )
	
## Plot
colbreaks = c(0,100,200,400,600,800,1000,max(IRR_cw.raster[],na.rm=TRUE))
colregions = c( 'navy','cyan','chartreuse','yellow','orange','red','brown')

pdf( 'input/indus_irrig_base_2.pdf', width=6, height=6 )	
plot( 	IRR_cw.raster, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 1,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		legend = FALSE,
		main = 'Total Irrigation Withdrawal',
		xlab = expression("Longitude ["~degree~"East ]"), 
		ylab = expression("Latitude ["~degree~"North ]"), 
		mgp = c(2.5,0.5,0)
		)	
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)			
plot(	IRR_cw.raster,
		breaks=colbreaks,
		col=colregions,
		add=TRUE, legend = FALSE )
plot( 	do.call( rbind, lapply( c('IND','PAK','AFG'), function(cnt){ crop( getData( 'GADM', country = cnt, level = 0 ), extent(par('usr')) ) } ) ), 
		col = alpha( c('khaki','lightgreen','lightblue'), alpha = 0.1 ),
		border = 'gray25', add = TRUE )	
plot(crop(as_riv_15s_red.spdf,extent(par('usr'))), add=TRUE, col = 'deepskyblue', lwd=as_riv_15s_red.spdf@data$lwd)			
plot(crop(basin_red.sp,extent(par('usr'))),add=TRUE, col = NA, border = 'red', lwd=2)			
legend( 'bottomright', 
		legend = c('0-100','100-200','200-400','400-600','600-800','800-1000', '> 1000'), 
		fill = colregions, 
		ncol = 1,  
		title = expression( '[ ' ~ m^3 ~ ' ] ' ), 
		bty = 'n',
		cex = 0.75 )
abline(v=c(par('usr')[1:2],lwd=2))
abline(h=c(par('usr')[3:4],lwd=2))		
dev.off()			
pdf( 'input/indus_irrig_base_3.pdf', width=6, height=6 )	
plot( 	IRR_gw.raster, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 1,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		legend = FALSE,
		main = 'Groundwater Irrigation Withdrawal',
		xlab = expression("Longitude ["~degree~"East ]"), 
		ylab = expression("Latitude ["~degree~"North ]"), 
		mgp = c(2.5,0.5,0)
		)	
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)	
plot(	IRR_gw.raster,
		breaks=colbreaks,
		col=colregions,
		add=TRUE, legend = FALSE )
plot( 	do.call( rbind, lapply( c('IND','PAK','AFG'), function(cnt){ crop( getData( 'GADM', country = cnt, level = 0 ), extent(par('usr')) ) } ) ), 
		col = alpha( c('khaki','lightgreen','lightblue'), alpha = 0.1 ),
		border = 'gray25', add = TRUE )	
plot(crop(as_riv_15s_red.spdf,extent(par('usr'))), add=TRUE, col = 'deepskyblue', lwd=as_riv_15s_red.spdf@data$lwd)			
plot(crop(basin_red.sp,extent(par('usr'))),add=TRUE, col = NA, border = 'red', lwd=2)			
legend( 'bottomright', 
		legend = c('0-100','100-200','200-400','400-600','600-800','800-1000', '> 1000'), 
		fill = colregions, 
		ncol = 1,  
		title = expression( '[ ' ~ m^3 ~ ' ] ' ), 
		bty = 'n',
		cex = 0.75 )
abline(v=c(par('usr')[1:2],lwd=2))
abline(h=c(par('usr')[3:4],lwd=2))		
dev.off()		



## Plot 2 - BCUs broken into 

test.spdf = readOGR( 'input', paste( basin, 'bcu', sep = '_' ), verbose = FALSE )		
test.spdf@data$CNTRY = unlist( strsplit( as.character( test.spdf@data$PID ), '_' ) )[ seq( 1, 2*length(test.spdf), by = 2 ) ]
cntry = data.frame( PAK = 'Pakistan', IND = 'India', CHN = 'China', AFG = 'Afghanistan' )
test.spdf@data$CNTRY = as.character( unlist( data.frame( lapply( test.spdf@data$CNTRY, function(iii){ as.character( unlist( cntry[ iii ] ) ) } ) ) ) )

# Network
cnts = data.frame( test.spdf[,c('OUTX','OUTY')] )
starts = cnts[ which( !is.na( test.spdf@data$DOWN ) ), ]
ends = do.call( rbind, lapply( match( test.spdf@data$DOWN[ which( !is.na( test.spdf@data$DOWN ) ) ], test.spdf@data$PID ), function(iii){ cnts[ iii,  ] } ) )
l = vector("list", nrow(starts))
for (ll in seq_along(l)) {l[[ll]] = Lines(list(Line(rbind(starts[ll,  ], ends[ll,]))), as.character(ll))}
net.sldf = SpatialLinesDataFrame( SpatialLines(l), data.frame( max_flow = rep( 1, length(l)), row.names = row.names(SpatialLines(l)) ) )

# plot
regs = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( rr ){
	res = gUnaryUnion( test.spdf[ which(test.spdf@data$CNTRY == rr),] ) 
	row.names(res) = as.character(rr)
	return(res)
	} ) )
	
pdf( 'input/indus_map_bcu.pdf', width=6, height=6 )
plot( 	buff.sp, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 0.8,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		#xlab = expression("Longitude ["~degrees~"East ]"), 
		#ylab = expression("Latitude ["~degrees~"North ]"), 
		mgp = c(2.5,0.5,0)
		)
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)
plot( regs, border = NA, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), add=TRUE )
plot(bcus.sp,add=TRUE,col=NA,border='white' )
plot(net.sldf, add=TRUE, lwd = 1.5, pch = NA, lty = 1 )
plot(SpatialPoints( data.frame( test.spdf[,c('OUTX','OUTY')] ) ),pch =21, bg = 'white', col = 'black', cex= 1.2 , add=TRUE)
text( sea_label.spdf, label = sea_label.spdf@data$sea, col = 'navy', cex = 0.65, font = 3 )
legend( 'bottomright', 
		legend = names(regs), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1,  
		title = 'Country Border', 
		bty = 'n',
		cex = 0.75 )
legend( 'topright', 
		legend = c('Sub-basin\nOutlet','River\nNetwork'),
		lty = c(NA,1),
		pch = c(21, NA),
		lwd = c(NA,1.5),		
		col = c('black'),
		pt.bg = c('white',NA),	
		seg.len = 2,
		ncol = 1,  
		y.intersp = 2,
		bty = 'n',
		pt.cex=1,
		cex = 0.75 )	
scalebar(500, type = 'bar', xy = c(71, 24), divs = 5, cex = 0.75, lonlat = TRUE )
box()
text( 73.75, 23.65, label = 'Distance [ km ]', cex = 0.75 ) 
arrows( 65 , 35, 67, 35, length = 0.1, angle = 30 )
text( 67.35, 35, label = 'E', cex = 0.7, font = 1 ) 
arrows( 65 , 35, 65, 37, length = 0.1, angle = 30 )  
text( 65, 37.35, label = 'N', cex = 0.7, font = 1 ) 
dev.off()


# Plot together
pdf( 'input/indus_map_cmb.pdf', width=13, height=6 )
p1 = layout( matrix( c(1,2), 1,2, byrow=TRUE ), widths=c(0.5,0.5), heights=c(0.9) )
par(mar=c(1,1,1,1), oma = c(1,1,1,1))
plot( 	buff.sp, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 1,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		#xlab = expression("Longitude ["~degrees~"East ]"), 
		#ylab = expression("Latitude ["~degrees~"North ]"), 
		#main = c('a. Indus river basin'),
		mgp = c(2.5,0.5,0)
		)
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)	
#plot(admin.spdf, border  = "white", col="gray76", add=TRUE)		
plot(	elevation.raster,
		breaks=colbreaks,
		col=colregions,
		add=TRUE, legend = FALSE )
plot(basin_red.sp,add=TRUE, col = NA, border = 'purple', lwd=1.5)	
plot(as_riv_15s_red.spdf, add=TRUE, col = 'deepskyblue', lwd=as_riv_15s_red.spdf@data$lwd)	
plot(canal.spdf,add=TRUE,col=alpha('red',alpha=0.8),lwd=0.5)
plot(ind_gan.sl,add=TRUE,col=alpha('red',alpha=0.8),lwd=0.5)
text( sea_label.spdf, label = sea_label.spdf@data$sea, col = 'navy', cex = 0.9, font = 3 )
#text( admin_lab.sp, label = row.names(admin_lab.sp), col = alpha('gray25',alpha=0.7), cex = 0.7, font = 2 )
points( cities.spdf, pch = 21, bg = 'white', col = 'black' )
text( cities_lab.spdf, label = cities_lab.spdf@data$city, col = 'black', cex = 0.9, font = 2 )
box()		 
#axis(side=1, cex.axis = 0.8)
#axis(side=2, cex.axis = 0.8)
legend( 'bottomright', 
		legend = c('0-200','200-500','500-1000','1000-2000','2000-4000', '> 4000'), 
		fill = colregions, 
		ncol = 1,  
		title = 'Elevation [ m ]', 
		bty = 'n',
		cex = 1 )
legend( 'topright', 
		legend = c('Basin boundary', 'Main river', 'Main canal'),
		lty = 1,
		lwd = c(1.5,1.5,1),		
		col = c('purple','deepskyblue',alpha('red',alpha=0.8)), 
		seg.len = 3,
		ncol = 1,  
		bty = 'n',
		cex = 1 )		
scalebar(500, type = 'bar', xy = c(71, 24), divs = 5, cex = 0.85, lonlat = TRUE )
text( 73.75, 23.55, label = 'Distance [ km ]', cex = 0.85 ) 
arrows( 65 , 35, 67, 35, length = 0.1, angle = 30 )
text( 67.35, 35, label = 'E', cex = 0.8, font = 1 ) 
arrows( 65 , 35, 65, 37, length = 0.1, angle = 30 )  
text( 65, 37.35, label = 'N', cex = 0.8, font = 1 )							
plot( 	buff.sp, 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 1,
		cex.lab = 0.8,
		bg = "#A6CAE0", 
		#xlab = expression("Longitude ["~degrees~"East ]"), 
		#ylab = expression("Latitude ["~degrees~"North ]"), 
		#main = c('b. BCU delineation'),
		mgp = c(2.5,0.5,0)
		)
plot(cnt2.sp, border  = "navy", col="gray96", add=TRUE)
plot( regs, border = NA, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), add=TRUE )
plot(bcus.sp,add=TRUE,col=NA,border='white' )
plot(net.sldf, add=TRUE, lwd = 1.5, pch = NA, lty = 1 )
plot(SpatialPoints( data.frame( test.spdf[,c('OUTX','OUTY')] ) ),pch =21, bg = 'white', col = 'black', cex= 1.2 , add=TRUE)
text( sea_label.spdf, label = sea_label.spdf@data$sea, col = 'navy', cex = 0.9, font = 3 )
legend( 'bottomright', 
		legend = names(regs), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1,  
		title = 'Country Border', 
		bty = 'n',
		cex = 1 )
legend( 'topright', 
		legend = c('Sub-basin\nOutlet','River\nNetwork'),
		lty = c(NA,1),
		pch = c(21, NA),
		lwd = c(NA,1.5),		
		col = c('black'),
		pt.bg = c('white',NA),	
		seg.len = 2,
		ncol = 1,  
		y.intersp = 2,
		bty = 'n',
		pt.cex=1,
		cex = 1 )	
scalebar(500, type = 'bar', xy = c(71, 24), divs = 5, cex = 0.75, lonlat = TRUE )
box()
text( 73.75, 23.55, label = 'Distance [ km ]', cex = 0.85 ) 
arrows( 65 , 35, 67, 35, length = 0.1, angle = 30 )
text( 67.35, 35, label = 'E', cex = 0.8, font = 1 ) 
arrows( 65 , 35, 65, 37, length = 0.1, angle = 30 )  
text( 65, 37.35, label = 'N', cex = 0.8, font = 1 ) 
dev.off()							
		


## Plot 3 - SSP demographics 

test.spdf = readOGR( 'input', paste( basin, 'bcu', sep = '_' ), verbose = FALSE )		
test.spdf@data$CNTRY = unlist( strsplit( as.character( test.spdf@data$PID ), '_' ) )[ seq( 1, 2*length(test.spdf), by = 2 ) ]
cntry = data.frame( PAK = 'Pakistan', IND = 'India', CHN = 'China', AFG = 'Afghanistan' )
test.spdf@data$CNTRY = as.character( unlist( data.frame( lapply( test.spdf@data$CNTRY, function(iii){ as.character( unlist( cntry[ iii ] ) ) } ) ) ) )

res.list = lapply( c(1,2,5), function(ss){ 
	res2 = bind_cols( lapply( c(2010,2050), function(yy2){
		bsn.df=test.spdf
		if(ss==1){rr=1}
		if(ss==2){rr=2}
		if(ss==5){rr=4}
		dat.df = data.frame( readRDS( paste('input/harmonized_rcp_ssp_data/water_use_ssp',ss,'_rcp',rr,'_',yy2,'_data.Rda',sep='') ) )
		names(dat.df)[which(names(dat.df) == paste('xloc',yy2,sep='.'))] = 'xloc'
		names(dat.df)[which(names(dat.df) == paste('yloc',yy2,sep='.'))] = 'yloc'
		names(dat.df)[which(names(dat.df) == paste('urban_pop',yy2,sep='.'))] = 'urban_pop'
		names(dat.df)[which(names(dat.df) == paste('rural_pop',yy2,sep='.'))] = 'rural_pop'
		names(dat.df)[which(names(dat.df) == paste('urban_gdp',yy2,sep='.'))] = 'urban_gdp'
		names(dat.df)[which(names(dat.df) == paste('rural_gdp',yy2,sep='.'))] = 'rural_gdp'
		coordinates(dat.df) = ~ xloc + yloc
		gridded(dat.df) = TRUE
		dat.spdf = dat.df
		proj4string(dat.df) = proj4string(bsn.df)
		dat.df = cbind(data.frame(dat.df), over(dat.df,bsn.df[,which(names(bsn.df) == 'PID')]))
		dat.df = dat.df[-1*which(is.na(dat.df$PID)),]
		assign(paste('URBAN_POP',yy2,sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$urban_pop[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('RURAL_POP',yy2,sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$rural_pop[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('URBAN_GDP',yy2,sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$urban_gdp[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('RURAL_GDP',yy2,sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$rural_gdp[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		URBAN_INC = get(paste('URBAN_GDP',yy2,sep='.')) / get(paste('URBAN_POP',yy2,sep='.')); URBAN_INC[is.na(URBAN_INC)] = 0
		RURAL_INC = get(paste('RURAL_GDP',yy2,sep='.')) / get(paste('RURAL_POP',yy2,sep='.')); RURAL_INC[is.na(RURAL_INC)] = 0
		assign(paste('URBAN_INC',yy2,sep='.'), URBAN_INC )
		assign(paste('RURAL_INC',yy2,sep='.'), RURAL_INC )
		df = data.frame( 	urbpop = get(paste('URBAN_POP',yy2,sep='.')),
							rurpop = get(paste('RURAL_POP',yy2,sep='.')),
							urbinc = get(paste('URBAN_INC',yy2,sep='.')),
							rurinc = get(paste('RURAL_INC',yy2,sep='.')) )
		names(df) = c( paste( 'urbpop',yy2,sep='.' ), paste( 'rurpop',yy2,sep='.' ), paste( 'urbinc',yy2,sep='.' ), paste( 'rurinc',yy2,sep='.' ) )
		return(df)
		} ) )
	} )	
	
urbpop.df = data.frame( base.2010 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[1]]$urbpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp1.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[1]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp2.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[2]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp5.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[3]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
           				row.names = unique(test.spdf@data$CNTRY) )
rurpop.df = data.frame( base.2010 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[1]]$rurpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp1.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[1]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp2.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[2]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
						ssp5.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = sum( res.list[[3]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) } ) ),
           				row.names = unique(test.spdf@data$CNTRY) )
urbinc.df = data.frame( base.2010 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[1]]$urbinc.2010[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[1]]$urbpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[1]]$urbpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp1.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[1]]$urbinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[1]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[1]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp2.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[2]]$urbinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[2]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[2]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp5.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[3]]$urbinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[3]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[3]]$urbpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
           				row.names = unique(test.spdf@data$CNTRY) )
rurinc.df = data.frame( base.2010 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[1]]$rurinc.2010[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[1]]$rurpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[1]]$rurpop.2010[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp1.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[1]]$rurinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[1]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[1]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp2.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[2]]$rurinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[2]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[2]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
						ssp5.2050 = do.call( rbind, lapply( unique(test.spdf@data$CNTRY), function( reg ){ res = max( 0, sum( res.list[[3]]$rurinc.2050[ which( test.spdf@data$CNTRY == reg ) ] * res.list[[3]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ) / sum( res.list[[3]]$rurpop.2050[ which( test.spdf@data$CNTRY == reg ) ] ), na.rm=TRUE ) } ) ),
           				row.names = unique(test.spdf@data$CNTRY) )					
						

# Plot together
pdf( 'input/indus_barplot_ssp.pdf', width=13, height=12 )
p1 = layout( matrix( c(1,2,3,4), 2,2, byrow=TRUE ), widths=c(0.5,0.5), heights=c(0.5,0.5) )
par(mar=c(3,5,3,3), oma = c(2,2,2,2))						
barplot( 1e-6*as.matrix(urbpop.df), beside = TRUE, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), ylab = c('Population [ million ]'), main = 'Urban Population - Indus Basin', cex.axis = 1.5, cex.names = 1.5, cex.main = 1.8, cex.lab = 1.5, ylim = c(0,150) )
legend( 'topleft', 
		legend = unique(test.spdf@data$CNTRY), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1, 
		bty = 'n',
		cex = 1.5 )
box()
barplot( 1e-6*as.matrix(rurpop.df), beside = TRUE, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), ylab = c('Population [ million ]'), main = 'Rural Population - Indus Basin', cex.axis = 1.5, cex.names = 1.5, cex.main = 1.8, cex.lab = 1.5, ylim = c(0,150) )
legend( 'topleft', 
		legend = unique(test.spdf@data$CNTRY), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1, 
		bty = 'n',
		cex = 1.5 )
box()
barplot( 1e-3*as.matrix(urbinc.df), beside = TRUE, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), ylab = c('Per Capita Income [ thousand USD2010 ]'), main = 'Urban Income - Indus Basin', cex.axis = 1.5, cex.names = 1.5, cex.main = 1.8, cex.lab = 1.5, ylim = c(0,50) )
legend( 'topleft', 
		legend = unique(test.spdf@data$CNTRY), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1, 
		bty = 'n',
		cex = 1.5 )
box()
barplot( 1e-3*as.matrix(rurinc.df), beside = TRUE, col = alpha( c('blue','red','orange','green'), alpha=0.4 ), ylab = c('Per Capita Income [ thousand USD2010 ]'), main = 'Rural Income - Indus Basin', cex.axis = 1.5, cex.names = 1.5, cex.main = 1.8, cex.lab = 1.5, ylim = c(0,50) )
legend( 'topleft', 
		legend = unique(test.spdf@data$CNTRY), 
		fill = alpha( c('blue','red','orange','green'), alpha=0.4 ), 
		ncol = 1, 
		bty = 'n',
		cex = 1.5 )
box()
dev.off()
		
