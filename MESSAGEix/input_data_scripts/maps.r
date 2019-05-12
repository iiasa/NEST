
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

basin = 'Indus'

# Get the basin country units with provicial detail for india and pakistan
test.spdf = readOGR( 'input', paste( basin, 'prov_bcu', sep = '_' ), verbose = FALSE )		
admin_basin.sp = gUnaryUnion( test.spdf, test.spdf@data$ADMIN )
proj4string( admin_basin.sp ) = proj4string( test.spdf )
admin_basin.sp = spTransform( admin_basin.sp, crs( test.spdf ) )

require( RColorBrewer )
windows()
par(mar=c(2,2,2,2), oma = c(2,2,2,2))
plot( basin.sp, border = alpha( c('black'), alpha=0.8 ))
plot( admin_basin.sp, border = 'white', col = alpha( rev( brewer.pal(12, 'Set3' ) ), alpha=0.8 ) )
legend( 'bottomright', legend = c(names(admin_basin.sp)), fill = alpha( rev( brewer.pal(12, 'Set3' ) ), alpha=0.8 ), border = alpha( rev( brewer.pal(12, 'Set3' ) ), alpha=0.8 ), ncol = 1, cex=0.55, bty='n' )

## Cities for plotting
cities = c('Lahore', 'Islamabad', 'Hyderabad Pakistan', 'Gilgit', 'Kabul')
cities2 = c('Lahore', 'Islamabad', 'Hyderabad', 'Gilgit', 'Kabul')
cities.spdf = data.frame( geocode( cities ) )
cities.spdf$city = cities2
row.names(cities.spdf) = cities2
coordinates( cities.spdf ) = ~lon+lat
proj4string(cities.spdf) = proj4string(admin_basin.sp)
cities.spdf = spTransform( cities.spdf, crs(admin_basin.sp) )
cities.spdf@data$num = 1
cities.df = data.frame( cities.spdf )
cities_lab.df = cities.df %>%
					mutate( lat = lat - 0.5 ) %>%
					mutate( lon = lon - 0 )
cities_lab.spdf = cities_lab.df 
coordinates( cities_lab.spdf ) = ~lon+lat
proj4string(cities_lab.spdf) = proj4string(admin_basin.sp)
cities_lab.spdf = spTransform( cities_lab.spdf, crs(admin_basin.sp) )		

## Major reservoirs for plotting

reservoirs = c('Tarbela','Mangla')
reservoirs.spdf = spTransform(readOGR("P:/ene.general/Water/global_basin_modeling/grand_v1_1",'GRanD_dams_v1_1',verbose=FALSE), crs(admin_basin.sp))[ which( grepl( paste( reservoirs, collapse='|'), global_reservoirs.spdf@data$RES_NAME ) ) , 'RES_NAME' ]
reservoirs.spdf@data$num=1
reservoirs.df = data.frame( reservoirs.spdf )
reservoirs_lab.df = reservoirs.df %>%
					mutate( coords.x2 = coords.x2 + 1 ) %>%
					mutate( coords.x1 = coords.x1 + 1.3 )
reservoirs_lab.spdf = reservoirs_lab.df 
coordinates( reservoirs_lab.spdf ) = ~coords.x1+coords.x2
proj4string(reservoirs_lab.spdf) = proj4string(admin_basin.sp)
reservoirs_lab.spdf = spTransform( reservoirs_lab.spdf, crs(admin_basin.sp) )	

## Label for arabian sea
sea_label.spdf = data.frame( coords.x1 = 65.5, coords.x2 = 23.5, sea = 'Arabian Sea' )
coordinates( sea_label.spdf ) = ~coords.x1+coords.x2
proj4string(sea_label.spdf) = proj4string(admin_basin.sp)
sea_label.spdf = spTransform( sea_label.spdf, crs(admin_basin.sp) )	

## Map 1 - Existing and Planned Hydropower

# Get hydropower data and aggregate to provinces
global_hydropower.spdf = readOGR("P:/ene.general/Water/global_basin_modeling/global_hydropower_plants",'global_hydropower_plants')
global_hydropower.df = data.frame(global_hydropower.spdf)
planned_projects.df = data.frame( read.csv( "P:/is-wel/indus/datagis/raw/indus_future_hydro_projects.csv", stringsAsFactors = FALSE ) )
planned_projects.df = planned_projects.df[, c('x','y','opening','capacity_MW') ]
names(planned_projects.df) = c( 'coords.x1', 'coords.x2', 'YEAR', 'MW' )
planned_projects.df$STATUS = 'PLN'
planned_projects.df = planned_projects.df[,c( 'MW','STATUS','YEAR','coords.x1','coords.x2')]
global_hydropower.df = global_hydropower.df[,c( 'MW','STATUS','YEAR','coords.x1','coords.x2')]
global_hydropower.df = rbind( global_hydropower.df, planned_projects.df)
coordinates(global_hydropower.df) = ~ coords.x1 + coords.x2
proj4string(global_hydropower.df) = proj4string(admin_basin.sp)
global_hydropower.df = spTransform( global_hydropower.df, crs(admin_basin.sp) )
basin_hydro.df = do.call( rbind, lapply( 1:length(admin_basin.sp), function( iii ){ 
	
	data.frame( Installed = sum( c( 0, global_hydropower.df@data[ which(	!is.na( over(global_hydropower.df,admin_basin.sp[iii]) ) &
																			as.character( global_hydropower.df$STATUS ) == 'OPR'), 'MW' ] ), na.rm=TRUE ), 
				Planned =  sum( c( 0, global_hydropower.df@data[ which(		!is.na( over(global_hydropower.df,admin_basin.sp[iii]) ) &
																			as.character( global_hydropower.df$STATUS ) == 'PLN'), 'MW' ] ), na.rm=TRUE ) )

	} ) )
row.names( basin_hydro.df ) = row.names( admin_basin.sp )
basin_hydro.spdf = SpatialPolygonsDataFrame( admin_basin.sp, basin_hydro.df )

# plot using cartography packages
plt.spdf = basin_hydro.spdf
plt.df=data.frame(plt.spdf)
# Set a custom color palette
bb = 4
cols2 = carto.pal(pal1="green.pal", n1 = bb-1, pal2 = "orange.pal", n2 = bb, middle = FALSE,transparency = FALSE) 
#cols2 = carto.pal(pal1 = "red.pal" ,n1 = 7)

# Country borders
cnt.spdf = do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) )
cnt.spdf = gSimplify( cnt.spdf, tol = 0.1 )

windows()
plot( 	buffer( plt.spdf, width = 1.4 ), 
		border = NA, 
		col = NA, 
		cex.axis = 0.8,
		cex.main = 0.9,
		cex.lab = 0.8,
		main="Installed Hydropower Capacity", 
		bg = "#A6CAE0", 
		xlab = expression("Longitude ["~degree*"E ]"), 
		ylab = expression("Latitude ["~degree*"N ]"), 
		mgp = c(2.5,0.5,0) )
#plot(spTransform( world.spdf, crs(plt.spdf ) ), col  = "grey95", border=NA, add=TRUE)
plot(spTransform( cnt.spdf, crs(plt.spdf ) ), border  = "grey40", col="grey95", add=TRUE)
choroLayer(	spdf = plt.spdf,
			df = plt.df, 
			var = "Installed", 
			#breaks = round( unlist( c( quantile(plt.df$Installed,c(0,0.3,0.4,0.6,0.8,0.9,0.95,1)) ) ), digits=-1 ),
			breaks = c(0,50,100,1000,2000,2500,4000,5700),
			col = cols2,
			border = 'white',
			lwd = 0.1, 
			legend.pos = "topright",
			legend.title.txt = "Megawatts", 
			legend.title.cex = 0.6,
			legend.values.rnd = 0, 
			add = TRUE )	
propSymbolsLayer( 	spdf = cities.spdf,
					var = "num",
					inches = 0.035,
					col = 'black',
					legend.pos = 'n',
					border = 'white' )
labelLayer(spdf = cities_lab.spdf, # SpatialPolygonsDataFrame used to plot he labels
           txt = "city", # label field in df
           col = "black", # color of the labels
           cex = 0.6, # size of the labels
           font = 2) # label font	
labelLayer(spdf = sea_label.spdf, # SpatialPolygonsDataFrame used to plot he labels
           txt = "sea", # label field in df
           col = "royalblue4", # color of the labels
           cex = 0.6, # size of the labels
           font = 3) # label font			   
# propSymbolsLayer( 	spdf = reservoirs.spdf,
					# var = "num",
					# inches = 0.035,
					# col = 'mediumorchid1',
					# legend.pos = 'n',
					# border = 'white' )		
# labelLayer(spdf = reservoirs_lab.spdf, # SpatialPolygonsDataFrame used to plot he labels
           # txt = "RES_NAME", # label field in df
           # col = "mediumorchid1", # color of the labels
           # cex = 0.7, # size of the labels
           # font = 2) # label font					
box()		 
axis(side=1, cex.axis = 0.8)
axis(side=2, cex.axis = 0.8)




