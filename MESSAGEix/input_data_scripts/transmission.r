

require(rgeos)
require(overpass)
require(rgdal)
require(raster)
library(tidyverse)

basin = c('Indus')

setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
pids_original <- as.character(basin.spdf@data$PID )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
buff.sp = gBuffer( basin.sp, width=0.1 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)


# include the riparian countries outside the basin, to have external routes
extra_basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'indus_extra_basin_rip_countries', verbose = FALSE )
extra_basin.spdf <- extra_basin.spdf[,(3)]
names(extra_basin.spdf) = "PID"
proj4string(extra_basin.spdf) = proj4string(basin.spdf)
extra_basin.spdf = spTransform( extra_basin.spdf, CRS("+proj=longlat") )

basin_pid.spdf <- basin.spdf[,(1)]
mrg.spdf <- rbind(basin_pid.spdf,extra_basin.spdf)
plot(mrg.spdf)
subbasins.sp = gUnaryUnion(mrg.spdf, id = mrg.spdf@data$PID)
subbasins.sp = SpatialPolygons(lapply(1:length(subbasins.sp),function(x){Polygons(Filter(function(f){f@ringDir==1},subbasins.sp@polygons[[x]]@Polygons),ID=x)}))
proj4string( subbasins.sp ) = proj4string( basin.spdf )


if( !file.exists( paste( 'input/basin_transmission', paste(basin, 'osm_power_lines.shp', sep='_' ), sep = '/' ) ) )
	{
	
	# Grab electricity transmission data corresponding to extent from basin polygon
	ltypes = c('line','cable','minor_line')
	power_lines = lapply( ltypes, function(ltype){
		ext = paste( '(',paste(bbox(basin.spdf)[2,1] , bbox(basin.spdf)[1,1], bbox(basin.spdf)[2,2], bbox(basin.spdf)[1,2],sep=','),')',sep='')
		pth = paste( '[out:xml][timeout:100];(node["power"="',ltype,'"]',ext,';way["power"="',ltype,'"]',ext,';relation["power"="',ltype,'"]',ext,';);out body;>;out skel qt;', sep = '' ) 
		frb = overpass_query(pth)
		if( !is.null(frb) )
			{
			proj4string(frb) = proj4string(basin.spdf)
			frb = frb[ which( unlist( row.names(frb) ) %in% unlist( sapply( unlist( row.names( gIntersection( frb, buff.sp , byid=TRUE ) ) ), function(xxx){ unlist( strsplit( xxx, '[ ]') )[1] } ) ) ), 'voltage' ]
			}
		return(frb)	
		} )
	names(power_lines) = ltypes
	power_lines[ sapply( power_lines, is.null ) ] = NULL
	power_lines = do.call( rbind, power_lines )

	# Output to shapefile
	writeOGR( power_lines, 'input/basin_transmission', paste(basin,'osm_power_lines',sep='_'),  driver="ESRI Shapefile",  overwrite_layer=TRUE )
	}


# Electricity transmission
x1 = c(69,138,230,345,500,765)
y1 = 0.8 * c(12,50,140,375,900,2200)
mod1 = nls(y1 ~ a1 + a2*x1 + a3*x1^2, start = list(a1 = 0, a2 = 0, a3 = 1)) # model for converting kV to MW

# Clean up transmission data using an minimum zie of 66kV and then fit to MW using model
## HOW HAS THE OSM_POWER_LINES.SHP BEEN CREATED? should have the script, for transferrability
basin_pline.sldf = readOGR( 'input/basin_transmission', paste(basin,'osm_power_lines',sep='_') )	
basin_pline.sldf = basin_pline.sldf[ -1 * which( is.na(basin_pline.sldf$voltage) ), ]
basin_pline.sldf$voltage = as.numeric( as.character( basin_pline.sldf$voltage ) ) / 1000
basin_pline.sldf = basin_pline.sldf[ which( basin_pline.sldf@data$voltage > 60 ), ] 
basin_pline.sldf$MW = unlist( lapply( 1:length(basin_pline.sldf), function(zz){ 
	return( round( 	coef(mod1)[1] + 
					coef(mod1)[2] * basin_pline.sldf@data$voltage[zz] + 
					coef(mod1)[3] * basin_pline.sldf@data$voltage[zz]^2 ) ) 
	} ) )

# Get intersections between spatial lines and basin polygons - t	
subbasins.sp = spTransform( subbasins.sp, crs(basin_pline.sldf) )
inters = lapply( 1:length(basin_pline.sldf), function(x){ which( gIntersects(basin_pline.sldf[x,], subbasins.sp,byid=TRUE)) } ) 

# Initialize matrix tracking connections and installed capacity
electricity_transmission_MW = matrix(data = 0, ncol = length(subbasins.sp), nrow = length(subbasins.sp))
ln = rep(NA,length(inters))
for(x in 1:length(inters))
	{	
	res = inters[[x]]
	ln[x] = length(res)
	if( length(res) > 1 )
		{
		if( length(res) == 2 )
			{
			electricity_transmission_MW[ res[1], res[2] ] = electricity_transmission_MW[ res[1], res[2] ] + basin_pline.sldf$MW[x] 
			electricity_transmission_MW[ res[2], res[1] ] = electricity_transmission_MW[ res[2], res[1] ] + basin_pline.sldf$MW[x] 
			}else
			{
			# Manual check
			# cols = c('red','green','blue','orange')
			# windows()
			# plot(subbasins.sp[c(res)], main = paste('x','=',x,basin_pline.sldf$voltage[x], 'kV',sep=' '), border = 'white',col = cols)
			# plot(basin_pline.sldf,col='gray',add=TRUE)
			# plot(basin_pline.sldf[x,],add=TRUE, col = 'black', lwd = 4)
			# legend('topleft',legend = unlist(lapply(1:length(res),function(ww){paste(res[ww], as.character(basin.spdf@data$BCU[c(res[ww])]), sep = ' - ')})), fill = cols )
			temp = combn(length(res),2)
			for(kkk in 1:ncol(temp))
				{
				line_features = disaggregate(crop( as(basin_pline.sldf[x,],'SpatialLines'), gUnaryUnion( subbasins.sp[ c( res[ temp[1,kkk] ],res[ temp[2,kkk] ] ) ] ) ),byid=TRUE)
				for(lll in 1:length(line_features))
					{
					if( ( length( crop( line_features[lll], subbasins.sp[ c( res[ temp[1,kkk] ]) ] ) ) > 0 ) & ( length( crop( line_features[lll], subbasins.sp[ c( res[ temp[2,kkk] ]) ] ) ) > 0 ) )
						{
						print(temp[,kkk])
						electricity_transmission_MW[ res[ temp[1,kkk] ], res[ temp[2,kkk] ] ] = electricity_transmission_MW[ res[ temp[1,kkk] ], res[ temp[2,kkk] ] ] + basin_pline.sldf@data$MW[x] 
						electricity_transmission_MW[ res[ temp[2,kkk] ], res[ temp[1,kkk] ] ] = electricity_transmission_MW[ res[ temp[2,kkk] ], res[ temp[1,kkk] ] ] + basin_pline.sldf@data$MW[x] 
						}
					}	
				}
			}
		}
}
# this name assignments are wrong, PID from 1 to 17 are not corrisponding in subbasin.spdf
electricity_transmission_MW = data.frame( electricity_transmission_MW )
names_et <- c( as.character( basin.spdf$PID), as.character( extra_basin.spdf$PID)) 
# names(electricity_transmission_MW) = names_et
# row.names(electricity_transmission_MW) = names_et

# instead
pid_mrg<- data.frame( round(coordinates(mrg.spdf),2) ) %>% 
  mutate(coords = paste(X1,X2,sep = '.')) %>% 
  mutate(pid_4subb = names_et) %>% 
  select(coords,pid_4subb)

pid_correspondence<- data.frame( round(coordinates(subbasins.sp),2) ) %>% 
  mutate(coords = paste(X1,X2,sep = '.')) %>% 
  left_join(pid_mrg) %>% 
  select(-X1,-X2)

names(electricity_transmission_MW) = pid_correspondence$pid_4subb
row.names(electricity_transmission_MW) = pid_correspondence$pid_4subb

starts = NULL
ends = NULL
mw = NULL
trns.df2 = NULL
for(ii in 1:ncol(electricity_transmission_MW))
	{
	for(jj in ii:ncol(electricity_transmission_MW))
		{
		if(electricity_transmission_MW[ii,jj]>0)
			{
			starts = c(starts,ii)
			ends = c(ends,jj)
			mw = c(mw,electricity_transmission_MW[ii,jj])
			trns.df2 = rbind(trns.df2,data.frame(value = electricity_transmission_MW[ii,jj], tec = paste0(names(electricity_transmission_MW)[ii],'|',row.names(electricity_transmission_MW)[jj]),starts = names(electricity_transmission_MW)[ii], ends = row.names(electricity_transmission_MW)[jj],stringsAsFactors = FALSE ) )
			}
		}
	}	
# starts = data.frame( xc = unlist(lapply(1:length(starts),function(ww){gCentroid(subbasins.sp[starts[ww]],byid=TRUE)$x})),    
# 					 yc = unlist(lapply(1:length(starts),function(ww){gCentroid(subbasins.sp[starts[ww]],byid=TRUE)$y})),	
# 					 mw = mw )
# ends = data.frame( 	 xc = unlist(lapply(1:length(ends),function(ww){gCentroid(subbasins.sp[ends[ww]],byid=TRUE)$x})),    
# 					 yc = unlist(lapply(1:length(ends),function(ww){gCentroid(subbasins.sp[ends[ww]],byid=TRUE)$y})),	
# 					 mw = mw)	
# 
# coords_start = cbind(starts$xc, starts$yc)
# starts.spdf <- SpatialPointsDataFrame(coords_start,as.data.frame( starts$mw) )
# coords_ends = cbind(ends$xc, ends$yc)
# ends.spdf <- SpatialPointsDataFrame(coords_ends,as.data.frame( ends$mw) )
# 
# proj4string(starts.spdf) = proj4string(basin.spdf)
# starts.spdf$PID <- over(starts.spdf,mrg.spdf)$PID
# proj4string(ends.spdf) = proj4string(basin.spdf)
# ends.spdf$PID <- over(ends.spdf,mrg.spdf)$PID

# basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'indus_basin_units', verbose = FALSE )
# basin.spdf@data$PID = c( 1:length(basin.spdf) )
# pids_after <- as.character( basin.spdf@data$PID )
# map_PID <- data.frame(pids_original = as.character(c(pids_original, as.character(extra_basin.spdf$PID))),pids_after = as.character(c(pids_after, as.character(extra_basin.spdf$PID))),stringsAsFactors = F)

# trns.df <- data.frame(start = starts.spdf$PID ,  end = ends.spdf$PID, value = starts.spdf$`starts$mw` ,stringsAsFactors = F) %>% 
#   left_join(map_PID, by = c("start" = "pids_original") )%>% rename("starts" = pids_after) %>% 
#   left_join(map_PID, by = c("end" = "pids_original") )%>% rename("ends" = pids_after) %>% 
#   rowwise() %>% 
#   mutate(routes0 = paste0(min(starts,ends),'|',max(starts,ends) ) ) %>% 
#   mutate(tec = if_else(max(starts,ends) %in% extra_basin.spdf$PID, paste0('PID_',min(starts,ends),'|',max(starts,ends) ),
#                        paste0('PID_',min(starts,ends),'|','PID_',max(starts,ends) ) ) )

# Get adjacent and downstream bcus
gt = gTouches( basin.spdf, byid=TRUE )
adjacent.list = lapply( 1:length(basin.spdf), function( iii ){ return( basin.spdf@data$PID[ which( gt[,iii] ) ] ) } )
downstream.list = lapply( 1:length(basin.spdf), function( iii ){ if( !is.na( basin.spdf@data$DOWN[iii] ) ){ return( basin.spdf@data$PID[ which( as.character( basin.spdf@data$ID ) == as.character( basin.spdf@data$DOWN[iii] ) ) ] ) }else{ return( 'SINK' ) } } )
names(adjacent.list) = basin.spdf@data$PID
names(downstream.list) = basin.spdf@data$PID
flow_routes = sapply( 1:length( downstream.list ), function( iii ){ paste( names( downstream.list )[[ iii ]], downstream.list[[ iii ]], sep = '|' ) } ) 
adj1 = unlist( sapply( 1:length( adjacent.list ), function( iii ){ paste( names( adjacent.list )[[ iii ]], adjacent.list[[ iii ]], sep = '|' ) } ) )
adj2 = unlist( sapply( 1:length( adjacent.list ), function( iii ){ paste( adjacent.list[[ iii ]], names( adjacent.list )[[ iii ]], sep = '|' ) } ) )
inds = NULL
for(iii in 1:length(adj2)){ jjj = which( adj1 == adj2[iii] ) ; if( jjj > iii ){ inds = c(inds,jjj) } }
adj3 = unlist( sapply( 1:length( adjacent.list ), function( iii ){ paste( names( adjacent.list )[[ iii ]],'|',adjacent.list[[ iii ]], sep = '' ) } ) )
adjacent_routes = adj3[ -1 * inds ]
export_routes = c( 	"PAK_2|PAK",
                                "PAK_4|PAK",
                                "PAK_5|PAK",
                                "PAK_6|PAK",
                                "PAK_10|PAK",
                                "PAK_12|PAK",
                                "PAK_13|CHN",
                                "CHN_2|CHN",
                                "CHN_1|CHN",
                                "CHN_3|CHN",
                                "AFG_2|AFG",
                                "AFG_1|AFG",
                                "PAK_7|AFG",
                                "PAK_8|AFG",
                                "PAK_10|IND",
                                "IND_4|IND")

library(tmap)
tm_shape(basin.spdf)+
  tm_borders(col = NA, lwd = 1.5, lty = "solid", alpha = NA)+
  tm_polygons(col = 'REGION', lty = "solid", alpha = 0.2)+
  tm_text("PID", size="AREA", root=5)+
  tm_shape(extra_basin.spdf)+
  tm_borders(col = NA, lwd = 2.5, lty = "solid", alpha = NA)+
  tm_text("PID", color = 'red')

all_routes <- c(adjacent_routes,export_routes)
trns_out.df <- data.frame(tec = all_routes, value = 0,stringsAsFactors = F) %>% 
  filter(!tec %in% trns.df2$tec) %>% 
  bind_rows(trns.df2 %>%   select(tec,value) ) %>% 
  mutate(node = gsub('\\|.*','',tec)) %>% 
  mutate(year_all = 2015) %>% 
  select(node,tec,year_all,value)

write.csv(trns_out.df,file.path(getwd(),'input/basin_transmission/existing_routes.csv'),row.names = F)


trueCentroids = gCentroid(basin.spdf,byid=TRUE)
#proj4string(trueCentroids) = "+proj=longlat +ellps=WGS84 +units=m"
trueCentroids <- spTransform(trueCentroids,CRS("+proj=lcc +lat_1=26 +lat_0=26 +lon_0=74 +k_0=0.99878641 +x_0=2743196.4 +y_0=914398.8 +a=6377301.243 +b=6356100.230165384 +towgs84=283,682,231,0,0,0,0 +units=m +no_defs" ))
distp = gDistance(trueCentroids,byid = T)
dist = as.data.frame(distp*gt)

names(dist) = as.character(pid_correspondence$pid_4subb)[1:24]
row.names(dist) = as.character(pid_correspondence$pid_4subb)[1:24]

starts = NULL
ends = NULL
df = NULL
for(ii in 1:ncol(dist))
{
  for(jj in ii:ncol(dist))
  {
    if(dist[ii,jj]>0)
    {
      starts = c(starts,ii)
      ends = c(ends,jj)
      df = rbind(df,data.frame(dist = dist[ii,jj], route = paste0(names(dist)[ii],'|',row.names(dist)[jj]), country = gsub('_.*','',names(dist)[ii])) )
    }
  }
}

#need to add externa routes, but can set an average number
library(data.table)
dist.df <- df %>% 
  mutate(dist = dist/1000) %>% 
  mutate(mean = mean(dist)) %>% 
  group_by(country) %>% 
  mutate(cnt_mean = mean(dist)) %>% 
  ungroup()

dist.out <- dist.df %>% 
  select(dist,route) %>% 
  rename(tec = route)

write.csv(dist.out,'P:/is-wel/indus/message_indus/input/PID_distances_km.csv',row.names = F)

#sp = SpatialPoints(coords)
# make spatial data frame
#spdf = SpatialPointsDataFrame(coords, data)
## raw list to store Lines objects
# l = vector("list", nrow(starts))
# for (ll in seq_along(l)) {l[[ll]] = Lines(list(Line(rbind(starts[ll, c(1,2) ], ends[ll,c(1,2)]))), as.character(ll))}
# net.sldf = SpatialLinesDataFrame( SpatialLines(l), data.frame( mw = starts[,c('mw')], row.names = row.names(SpatialLines(l)) ) )
# lwdt = 10 * ( (net.sldf@data$mw / max(net.sldf@data$mw)) ) + 1
# #pdf(paste("reduced_electricity_network.pdf",sep=""))
# windows()
# plot(subbasins.sp, border = 'yellow')
# plot(basin_pline.sldf,add=TRUE,col='gray' )
# plot(net.sldf, add=TRUE, lwd = sqrt(lwdt), pch = NA, lty = 1 )
# #dev.off()

# # Candidate transmission lines
# ret = lapply( 1:length(africa_basin_units.sp), function(p){ which( gTouches(africa_basin_units.sp[p],africa_basin_units.sp,byid=TRUE) ) } )
# connections = NULL
# for(i in 1:length(ret))
	# {
	# if( length(ret[[i]]) > 0 )
		# {
		# for(j in 1:length(ret[[i]]))
			# {
			# pair = c(i, ret[[i]][j])
			# if( length(which(is.na(pair))) == 0 )
				# {
				# if( pair[1] != pair[2] )
					# {	
					# if(!is.null(connections))
						# {
						# if( length( which( pair[2] == connections[,1] & pair[1] == connections[,2] ) ) == 0 )
							# {
							# connections = rbind(connections, pair)
							# }
						# }else
						# {
						# connections = t(as.matrix(pair))		
						# }
					# }	
				# }
			# }
		# }	
	# }	
# PID_connections.df = data.frame( ID = unlist( lapply(  1:nrow(connections), function(x){ paste( 'PID_', africa_basin_units.spdf@data$PID[connections[x,1]], '__', 'PID_', africa_basin_units.spdf@data$PID[connections[x,2]], sep='' ) } ) ),
								 # NODE1_PID = africa_basin_units.spdf@data$PID[connections[,1]],
								 # NODE2_PID = africa_basin_units.spdf@data$PID[connections[,2]],
								 # NODE1_COUNTRY = as.character( africa_basin_units.spdf@data$COUNTRY_ID[connections[,1]] ),
								 # NODE2_COUNTRY = as.character( africa_basin_units.spdf@data$COUNTRY_ID[connections[,2]] ),		
								 # NODE1_BASIN = as.character( africa_basin_units.spdf@data$BASIN[connections[,1]] ),
								 # NODE2_BASIN = as.character( africa_basin_units.spdf@data$BASIN[connections[,2]] ) )
# PID_connections.df$LENGTH = round(unlist( lapply(  1:nrow(connections), function(x){  100 * gDistance( gCentroid(africa_basin_units.sp[connections[x,1]]), gCentroid(africa_basin_units.sp[connections[x,2]]) )} ) ))
# PID_connections.df$TRANSFER_FRACTION_HVAC = 1 - round(PID_connections.df$LENGTH * 0.07/1000,digits=4)
# PID_connections.df$TRANSFER_FRACTION_HVDC = 1 - round(PID_connections.df$LENGTH * 0.03/1000,digits=4)
# PID_connections.df$INV_COST_HVAC = round(PID_connections.df$LENGTH * 1130) 
# PID_connections.df$INV_COST_HVDC = round(PID_connections.df$LENGTH * 1130 * 1.4) 
# PID_connections.df$FIX_COST_HVAC = round(0.002 * PID_connections.df$INV_COST_HVAC)
# PID_connections.df$FIX_COST_HVDC = round(0.002 * PID_connections.df$INV_COST_HVDC)
# PID_connections.df$VAR_COST_HVAC = rep(0,length(PID_connections.df$LENGTH))
# PID_connections.df$VAR_COST_HVDC = rep(0,length(PID_connections.df$LENGTH))
# PID_connections.df$LIFETIME_HVAC = 50
# PID_connections.df$LIFETIME_HVDC = 50
# PID_connections.df$HISTORICAL_CAPACITY_HVAC_MW = unlist( lapply( 1:nrow(connections), function(x){ electricity_transmission_MW[ connections[x,1],connections[x,2] ] } ) )
# PID_connections.df$HISTORICAL_CAPACITY_HVDC_MW = rep(0,length(PID_connections.df$LENGTH))
# PID_connections.df$HURDLE_RATE_INV = unlist( lapply( 1:length(PID_connections.df$LENGTH), function(x){ if(PID_connections.df$HISTORICAL_CAPACITY_HVAC_MW[x]>0){return(1)}else{return(2)} } ) )
# PID_connections.df$HURDLE_RATE_VAR = rep(0,length(PID_connections.df$LENGTH))

# Calibrate to known HVDC lines
# existing_hvdc_lines = data.frame( name = c('Cahora Bassa','Inga-Shaba','Caprivi Link'), mw = c(1920,560,300), x1 = c(-15.611389,-10.6575,-20.314722), y1 = c(32.749722, 25.452222, 16.4525), x2 = c(-25.919722,-5.524167,-17.501667), y2 = c( 28.276111, 13.610833, 24.222222) )
# for(i in 1:nrow(existing_hvdc_lines))
	# {
	# p1 = existing_hvdc_lines[i,c('x1','y1','name','mw')]
	# coordinates(p1) = ~ y1 + x1
	# proj4string(p1) = "+proj=longlat"
	# p1_in = unlist(over(p1,africa_basin_units.sp,byid=TRUE))
	# p2 = existing_hvdc_lines[i,c('x2','y2','name','mw')]
	# coordinates(p2) = ~ y2 + x2
	# proj4string(p2) = "+proj=longlat"
	# p2_in = unlist(over(p2,africa_basin_units.sp,byid=TRUE))
# }
	