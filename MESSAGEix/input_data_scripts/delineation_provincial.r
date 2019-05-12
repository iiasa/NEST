

library(raster)
library(sp)
library(dplyr)
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

# Get the basin spdf using the name mapping.
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
	row.names(basin_units.sp) = paste( basin, seq(1,length(basin_units.sp),by=1), sep = '|' )
	
	}else{
	
	# Use level 2
	basin_units.sp = spTransform( as( basin2.spdf, 'SpatialPolygons' ), crs( basin2.spdf ) )
	row.names(basin_units.sp) = paste( basin, seq(1,length(basin_units.sp),by=1), sep = '|' )
	
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

row.names( riparian_nations.spdf ) = paste( as.character( riparian_nations.spdf@data$ISO ), '1', sep = '_' )

# Get provinces for each country
keep_country = NULL
if( basin == 'Indus' ){ keep_country = c('AFG','CHN') }
country2split = unique( as.character( riparian_nations.spdf@data$ISO ) )[ which( !( unique( as.character( riparian_nations.spdf@data$ISO ) ) %in% keep_country ) ) ]
province.spdf = do.call( rbind, lapply( country2split, function( cnt ){
	cwd = getwd()
	setwd( 'P:/is-wel/indus/message_indus/input' ) 
	res = getData( 'GADM', country = cnt, level = 1 )
	row.names(res) = paste( res@data$ISO[1], seq( 1, length( res ) ), sep = '_' )
	setwd(cwd)
	return( res )
	} ) ) 
province.spdf@data = province.spdf@data %>%
						select( ISO, NAME_1 ) %>%
						rename( NAME = NAME_1 ) %>%
						mutate( REGION = riparian_nations.spdf@data$REGION[ match( ISO, riparian_nations.spdf@data$ISO ) ] ) %>%
						mutate( STATUS = riparian_nations.spdf@data$STATUS[ match( ISO, riparian_nations.spdf@data$ISO ) ] ) %>%
						mutate( CONTINENT = riparian_nations.spdf@data$CONTINENT[ match( ISO, riparian_nations.spdf@data$ISO ) ] ) %>%
						select( ISO, NAME, REGION, STATUS, CONTINENT ) %>%
						'rownames<-'( row.names( province.spdf@data ) )
						
# Combine provinces with countries remaining
admin.spdf = rbind( province.spdf, riparian_nations.spdf[ match( keep_country, as.character( riparian_nations.spdf$ISO ) ), ] ) 
admin.spdf@data =  admin.spdf@data %>% 
					mutate( NAME = unlist( sapply( NAME, function( nnn ){ paste( unlist( strsplit( nnn, '[ ]' ) ), collapse = '_' ) } ) ) ) %>%
					mutate( NAME = unlist( sapply( NAME, function( nnn ){ paste( unlist( strsplit( nnn, '[.]' ) ), collapse = '' ) } ) ) ) %>%
					mutate( PID =  unlist( sapply( 1:length(NAME), function( nnn ){ paste( ISO[nnn], NAME[nnn], sep = '|' ) } ) ) ) %>%
					mutate( PID = sapply( 1:length(PID), function( nnn ){ if( duplicated(PID)[nnn] ){ return( paste(PID[nnn],'b',sep='_') ) }else{ return(PID[nnn]) } } ) ) %>%
					select( PID, ISO, NAME, REGION, STATUS, CONTINENT ) 
row.names( admin.spdf )	= as.character( admin.spdf@data$PID )			
					
######################################################################################	
# Create new polygons representing the intersection between countries and catchments
######################################################################################

# Harmonize and intersect
admin.sp = spTransform( as( admin.spdf, 'SpatialPolygons' ), crs(basin_units.sp) )
bcus.sp = raster::intersect( admin.sp, gBuffer( basin_units.sp , byid=TRUE, width=0 ) )
row.names(bcus.sp) = c( as.matrix( sapply( names(bcus.sp), function( nnn ){ paste( names( admin.sp )[ as.numeric( unlist( strsplit( nnn, '[ ]' ) ) )[1] ], names( basin_units.sp )[ as.numeric( unlist( strsplit( nnn, '[ ]' ) )[2] ) ], sep='__' ) } ) ) )

# Dissaggregate non-touching polygons
temp = disaggregate( SpatialPolygonsDataFrame(  bcus.sp, data.frame( ID = row.names( bcus.sp ), row.names = row.names( bcus.sp ) ) ), byid=TRUE )
newid1 = unlist( strsplit( as.character( temp@data$ID ), '__' ) )[seq(1,2*length(temp),by=2)]
newid2 = unlist( strsplit( as.character( temp@data$ID ), '__' ) )[seq(2,2*length(temp),by=2)]
for( j in unique(newid2) ){ newid2[ which( newid2 == j ) ] = paste( j, seq(1,length(which( newid2 == j )),by=1), sep='.' ) }						
newid = paste( newid1, newid2, sep = '__' )
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
			if( length( which( grepl( strsplit( strsplit( j, '__')[[1]][1], '[|]' )[[1]][1], names(big) ) ) ) > 1 ){ big = big[ which( grepl( strsplit( strsplit( j, '__')[[1]][1], '[|]' )[[1]][1], names(big) ) ) ] }
			
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
newid = unlist( strsplit( names( temp ), '[.]' ) )[seq(1,2*length(temp),by=2)]
for( j in unique(newid) ){ newid[ which( newid == j ) ] = paste( j, seq(1,length(which( newid == j )),by=1), sep='.' ) }		
row.names(temp) = newid
df = data.frame( 	t1 = unlist( strsplit( names(temp), '__' ) )[seq(1,2*length(temp),by=2)], 
					t2 = unlist( strsplit( names(temp), '__' ) )[seq(2,2*length(temp),by=2)] )
temp = temp[ order(df$t1,df$t2 ) ]
bcus.sp = temp

# Correct any remaining holes from intersection
tmp = gUnaryUnion( bcus.sp )
holes = SpatialPolygons( lapply( 1:length( tmp@polygons[[1]]@Polygons ), function( iii ){ Polygons( list( tmp@polygons[[1]]@Polygons[[iii]] ),ID=iii) } ) )
proj4string(holes) = proj4string( bcus.sp )
holes = spTransform( holes, crs( bcus.sp ))
holes = holes[ which( gArea(holes,byid=TRUE) < 0.5 * gArea( basin.sp ) ) ]

temp = bcus.sp
if( length( holes ) > 0 )
	{
	
	for( j in 1:length(holes) )
		{
		
		# Check if small polygon touching another polygon, if touching dissolve, else remove
		if( length( which( gTouches( holes[ j ], temp, byid = TRUE ) ) ) > 0 )
			{
			
			# Find  and isolate largest neighbor
			big = temp[ which( gTouches( holes[ j ], temp, byid = TRUE ) ) ]
			
			# Largest neighbour
			big = big[ which.max( gArea( big ) ) ]
			
			# Isolate small polygon
			sml = holes[ j ]
			
			# Combine small and large polygon maintaining the large polygon attributes
			cmb.sp = gUnaryUnion(rbind(big,sml,makeUniqueIDs=TRUE))
			row.names(cmb.sp) = row.names(big)
			
			# Remove big and add in the dissolved polygon back into the psdf
			temp = rbind( temp[ -1*which( names(temp) == names(big) ) ], cmb.sp )
			
			}
			
		}
	}	
	
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
	
	if( !is.na( down ) )
		{
		windows()
		plot( basin.sp )
		plot( bcus.sp[ppp],add=TRUE,col='red')
		plot( bcus.sp[down],add=TRUE,col='blue')
		legend( 'bottomright', c('upstream','downstream'), fill = c('red','blue'), bty = 'n', cex = 0.9 )
		}else{
		windows()
		plot( basin.sp )
		plot( bcus.sp[ppp],add=TRUE,col='purple')
		legend( 'bottomright', c('outlet'), fill = c('purple'), bty = 'n', cex = 0.9 )
		}
		
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
						ADMIN = unlist( strsplit( names(bcus.sp), '__' ) )[seq(1,2*length(bcus.sp),by=2)],
						SUBASIN = unlist( strsplit( unlist( strsplit( names(bcus.sp), '__' ) )[seq(2,2*length(bcus.sp),by=2)], '[.]' ) )[ seq(1,2*length(bcus.sp),by=2) ],
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
		paste( basin, 'prov_bcu', sep = '_' ),
		driver="ESRI Shapefile", 
		overwrite_layer=TRUE
		)

###########
# Test plot
###########

test.spdf = readOGR( 'input', paste( basin, 'prov_bcu', sep = '_' ), verbose = FALSE )		

# Network
cnts = data.frame( test.spdf[,c('OUTX','OUTY')] )
starts = cnts[ which( !is.na( test.spdf@data$DOWN ) ), ]
ends = do.call( rbind, lapply( match( test.spdf@data$DOWN[ which( !is.na( test.spdf@data$DOWN ) ) ], test.spdf@data$PID ), function(iii){ cnts[ iii,  ] } ) )
l = vector("list", nrow(starts))
for (ll in seq_along(l)) {l[[ll]] = Lines(list(Line(rbind(starts[ll,  ], ends[ll,]))), as.character(ll))}
net.sldf = SpatialLinesDataFrame( SpatialLines(l), data.frame( max_flow = rep( 1, length(l)), row.names = row.names(SpatialLines(l)) ) )

# plot
regs = do.call( rbind, lapply( unique(test.spdf@data$REGION), function( rr ){
	res = gUnaryUnion( test.spdf[ which(test.spdf@data$REGION == rr),] ) 
	row.names(res) = as.character(rr)
	return(res)
	} ) )
#pdf(paste('input/',basin,'_delineation.pdf',sep=''))		
windows()
par(mar=c(2,2,2,2), oma = c(2,2,2,2))
plot( basin.sp, border = alpha( c('black'), alpha=0.8 ))
plot( regs, border = NA, col = alpha( c('red','blue','chartreuse','orange','gold','purple'), alpha=0.2 ))
plot(bcus.sp,add=TRUE,col=NA,border='white' )
plot(net.sldf, add=TRUE, lwd = 2, pch = NA, lty = 1 )
plot(SpatialPoints( data.frame( test.spdf[,c('OUTX','OUTY')] ) ),pch =21, bg = 'white', col = 'black', cex= 1.2 , add=TRUE)

admin_basin.sp = gUnaryUnion( test.spdf, test.spdf@data$ADMIN )
require( RColorBrewer )
windows()
par(mar=c(2,2,2,2), oma = c(2,2,2,2))
plot( basin.sp, border = alpha( c('black'), alpha=0.8 ))
plot( admin_basin.sp, border = 'white', col = alpha( rev( brewer.pal(12, 'Set3' ) ), alpha=0.8 ) )
legend( 'bottomright', legend = c(names(admin_basin.sp)), fill = alpha( rev( brewer.pal(12, 'Set3' ) ), alpha=0.8 ), ncol = 1, cex=0.55, bty='n' )

#legend('topleft',legend = c(nms, 'Main outlet'), pch = c(rep( 21, length(nms)), 15), col=c(rep('black',length(nms)),'red'), pt.bg = c(cols,'red'), bty='n', ncol = 2, cex=1.1)	
#dev.off()

							
							
		

