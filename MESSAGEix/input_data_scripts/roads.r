
require(rgeos)
require(overpass)
require(rgdal)
require(raster)

basins = c('Indus')

# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

for( basin in basins )
	{

	# Grab the basin boundaries
	basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
	basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
	basin.sp = gUnaryUnion( basin.spdf )
	basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
	buff.sp = gBuffer( basin.sp, width=0.1 ) 
	proj4string(basin.sp) = proj4string(basin.spdf)
	proj4string(buff.sp) = proj4string(basin.spdf)
	
	# Grab electricity transmission data corresponding to extent from basin polygon
	rtypes = c('motorway','trunk','primary','secondary')#,'tertiary','unclassified')
	roads = lapply( rtypes, function(rtype){
		ext = paste( '(',paste(bbox(basin.spdf)[2,1] , bbox(basin.spdf)[1,1], bbox(basin.spdf)[2,2], bbox(basin.spdf)[1,2],sep=','),')',sep='')
		pth = paste( '[out:xml][timeout:100];(node["highway"="',rtype,'"]',ext,';way["highway"="',rtype,'"]',ext,';relation["highway"="',rtype,'"]',ext,';);out body;>;out skel qt;', sep = '' ) 
		frb = overpass_query(pth)
		if( !is.null(frb) )
			{
			proj4string(frb) = proj4string(basin.spdf)
			frb = frb[ which( unlist( row.names(frb) ) %in% unlist( sapply( unlist( row.names( gIntersection( frb, buff.sp , byid=TRUE ) ) ), function(xxx){ unlist( strsplit( xxx, '[ ]') )[1] } ) ) ), 'highway' ]
			}
		return(frb)	
		} )
	names(roads) = rtypes
	roads[ sapply( roads, is.null ) ] = NULL
	roads = do.call( rbind, roads )
	
	# Output to shapefile
	writeOGR( roads, 'input/basin_roads', paste(basin,'osm_roads',sep='_'),  driver="ESRI Shapefile",  overwrite_layer=TRUE )
	
	}

