

# source( paste( Sys.getenv("INDUS_IX_PATH"), 'input_data_scripts/powerplants_historical_capacity.r', sep = '/' ) )

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)


# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

basin = 'Indus'

# Indus delineatiion
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
proj4string( basin.sp ) = proj4string( basin.spdf )	
	
# Existing power generation capacity
# Read in Raptis et al and Carma data and merge

# Mapping between WEPP names and Raptis cooling and the technology naming convention for modeling
raptis_types_map_cooling.df = data.frame(read.csv('input/powerplants/raptis_types_map_cooling_type.csv',stringsAsFactors=FALSE)) %>% 
  mutate(msgssp_type = if_else(msgssp_type == "air","ac",
                               if_else(msgssp_type == "ot_saline","sw",
                                       gsub("_.*","",msgssp_type)))) 
map_unit.df = data.frame(read.csv('input/powerplants/map_unit_type.csv',stringsAsFactors=FALSE))
map_unit.df$tps2[map_unit.df$tps2 == "nuclear_st"] = "nuclear"
fuel_map.df = data.frame(	wepp = c('WAT','UR','GAS','CSGAS','BAG','BL','Oil','TGAS','BFG','PEAT','AGAS','OGAS','CKGAS','CXGAS','BITUMEN','COAL','COG','ETHANOL','KERO','GEO','COKE','LNG','LPG','DGAS','RGAS','CGAS','REFGAS','WSTGAS','SHALE','MGAS','LGAS','BIOMASS','WOOD','WOODGAS','OIL','NG','SUN','FLIQ','KER','WIND','BGAS','BLIQ','FGAS','BSOL','JET','WSTH'), 
                          final = c('Hydro','Nuclear','Gas','Gas','Biomass','Biomass','Biomass','Oil','Gas','Gas','Gas','Gas','Gas','Gas','Oil','Coal','Coal','Biomass','Gas','Geothermal','Oil','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Gas','Biomass','Biomass','Biomass','Oil','Gas','Solar','Oil','Oil','Wind','Biogas','Biomass','Gas','Biomass','Oil','Wsth') )

# Load the geocoded WEPP database and rename some of the categories
ppl.df = data.frame(read.csv('input/powerplants/IIASAPP_v3.6/geocoded/IIASAPP_geo2018-03-08.csv',stringsAsFactors=FALSE))
ppl.df = ppl.df[, c("latM", "lonM", "UNIT", "PLANT", "STATUS",  "YEAR" , "MW_x" , "UTYPE_x" , "FUEL_x", "COOL_complete")] 
names( ppl.df ) = c("LAT" , "LON", "UNIT", "PLANT", "STATUS",  "YEAR" , "MW" , "UTYPE" , "FUEL", "COOL")

# Rename cooling technologies based on defined mapping 
ppl.df <- ppl.df %>% 
  left_join(raptis_types_map_cooling.df %>% dplyr::select(raptis_type,msgssp_type),by = c("COOL" = "raptis_type")) %>% 
  dplyr::select(-COOL) %>% 
  rename(COOL = msgssp_type)

# Rename fuel types and remove NA types
ppl.df$FUEL = fuel_map.df$final[ match( ppl.df$FUEL, as.character(fuel_map.df$wepp) ) ]

# Remove retired plants and plants without location or fuel types
ppl.df = ppl.df %>% filter( STATUS %in% c('OPR','PLN','CON','DEC','DEL','UNK'), !is.na(LAT), !is.na(FUEL) ) %>%
	mutate( MW = as.numeric( as.character( MW ) ) )

# Combined unit and fuel types
ppl.df$TPS = paste( ppl.df$FUEL, ppl.df$UTYPE, sep= '.' )
ppl.df$TPS = map_unit.df$tps2[ match( ppl.df$TPS, as.character(map_unit.df$tps1) ) ]
ppl.df = ppl.df %>% 
  mutate(TPS = paste(TPS,COOL,sep = '_')) %>% 
  mutate(TPS = gsub("_NA.*","",TPS))

# Pakistan shapefile
pakistan.spdf<-getData("GADM", country="PAK", level=0)
# Make spatial and crop to basin extent	
ppl.spdf = ppl.df %>% rename( x = LON, y = LAT )
coordinates(ppl.spdf) = ~x+y 

# crop with pakistan
ppl.spdf2 = ppl.spdf
proj4string(ppl.spdf2) = proj4string( pakistan.spdf )
ppl.spdf2 = crop( ppl.spdf2, extent( pakistan.spdf ) )
ppl.spdf2 = crop( ppl.spdf2, extent( basin.spdf ) ,snap = 'out')
ppl.spdf2 = cbind( data.frame( ppl.spdf2 ), over( ppl.spdf2, pakistan.spdf[ ,'ISO'])) %>% 
  filter( !is.na( ISO ) ) %>% 
  mutate( TPS = as.character( TPS ) ) %>%
  mutate( TPS = if_else(TPS %in% factor( c( "solar_th","solar_th_ac","solar_th_cl" ) ),'solar_csp',TPS) ) %>%
  mutate( TPS = as.factor( TPS )  )%>%
  as.data.frame() %>% 'coordinates<-'(~x+y) %>% 
  'proj4string<-'(proj4string(basin.spdf)) 	

plot(ppl.spdf2)
plot(pakistan.spdf,add = T)
plot(basin.spdf,add = T)

proj4string(ppl.spdf) = proj4string( basin.spdf )
ppl.spdf = crop( ppl.spdf, extent( basin.spdf ) )
ppl.spdf = cbind( data.frame( ppl.spdf ), over( ppl.spdf, basin.spdf[ ,'PID'])) %>% 
	filter( !is.na( PID ) ) %>% 
	mutate( TPS = as.character( TPS ) ) %>%
	mutate( TPS = if_else(TPS %in% factor( c( "solar_th","solar_th_ac","solar_th_cl" ) ),'solar_csp',TPS) ) %>%
	mutate( TPS = as.factor( TPS )  )%>%
	as.data.frame() %>% 'coordinates<-'(~x+y) %>% 
	'proj4string<-'(proj4string(basin.spdf)) 	

ppl_pak.spdf = ppl.spdf[grep('PAK',ppl.spdf$PID),]
ppl_pak.spdf@data %>% filter(STATUS == 'OPR') %>% summarise(MW = sum(MW))
# 24.8 GW
ppl.spdf2 = ppl.spdf2[!ppl.spdf2$UNIT %in% ppl_pak.spdf$UNIT, ]
ppl.df2 = ppl.spdf2@data
ppl.df2 %>% filter(STATUS == 'OPR') %>% summarise(MW = sum(MW))
# 5.7 GW mainly in the Karachi area, no large hydropower

# add hydropower from van vliet et al data base and planned projects collected from literature / planning documents
hydropower.spdf = rbind( 
	crop( readOGR("input/powerplants",'global_hydropower_plants'), extent( basin.spdf ) ) %>%
		as.data.frame() %>% rename( x = coords.x1, y = coords.x2 ) %>%
		mutate( UNIT  = NA, PLANT = NA, UTYPE = factor('HY'), FUEL = factor('HYDRO'), COOL = NA, TPS = factor('hydro'), optional = TRUE ) %>%
		select( x, y, UNIT, PLANT, STATUS, YEAR,  MW, UTYPE, FUEL, COOL, TPS ) %>% 
		mutate( MW = as.numeric( as.character( MW ) ) ) %>%
		'coordinates<-'(~x+y) %>% 'proj4string<-'(proj4string(basin.spdf)),
	data.frame( read.csv( "input/powerplants/indus_future_hydro_projects.csv", stringsAsFactors = FALSE ) ) %>%
		rename( YEAR = opening, MW = capacity_MW, UNIT = project.name ) %>%
		mutate( YEAR = replace( YEAR, YEAR == 'proposed', 2030 ) ) %>%
		mutate( MW = as.numeric( as.character( MW ) ) ) %>%
		mutate( PLANT = NA, UTYPE = factor('HY'), STATUS = factor('PLN'), FUEL = factor('HYDRO'), COOL = NA, TPS = factor('hydro'), optional = TRUE ) %>%
		select( x, y, UNIT, PLANT, STATUS, YEAR,  MW, UTYPE, FUEL, COOL, TPS ) %>% 
		as.data.frame() %>% 'coordinates<-'(~x+y) %>% 
		'proj4string<-'(proj4string(basin.spdf)) 
	)		
hydropower.spdf = cbind( data.frame( hydropower.spdf ), over( hydropower.spdf, basin.spdf[ ,'PID'])) %>%
	filter( !is.na( PID ) ) %>% 
	as.data.frame() %>% 'coordinates<-'(~x+y) %>% 
	'proj4string<-'(proj4string(basin.spdf)) 
# 50 GW

# Combine power plant data sources
pplt.spdf = rbind( 	ppl.spdf %>% # remove hydropower
						as.data.frame() %>% 
						mutate( MW = as.numeric( as.character( MW ) ) ) %>%
						filter( UTYPE != 'HY' ) %>% 
						as.data.frame() %>% 'coordinates<-'(~x+y) %>% 
						'proj4string<-'(proj4string(basin.spdf)),
					hydropower.spdf )	
					
# technologies with no cooling, dont want to spread into cooling tech
pplt.spdf@data <- pplt.spdf@data %>% 
	mutate(TPS = as.character(TPS)) %>% 
	mutate(TPS = gsub("biogas_","biomass_",TPS)) %>% 
	mutate(TPS = if_else(TPS == "gas_ic","gas_gt", if_else(TPS == "biomass_gt","biomass_st", if_else(TPS == "oil_ic","oil_gt",TPS) ) ) )

tech_no_cool <- c("gas_gt","oil_gt","wind","hydro","solar_pv")

# calculate share of cooling technologies
share_cool <- pplt.spdf@data %>% 
	dplyr::select(MW,FUEL,COOL) %>% 
	filter(!is.na(COOL)) %>% 
	group_by(FUEL,COOL) %>% 
	summarize(MW = sum(MW)) %>% ungroup() %>% 
	group_by(FUEL) %>% 
	mutate(share = MW/sum(MW)) %>% as.data.frame()

pplt.df = data.frame(pplt.spdf)	
new_tech <- bind_rows(pplt.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "ac"),
                      pplt.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "cl"),
                      pplt.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "ot"),
                      pplt.df %>% filter(is.na(COOL) & !(TPS %in% tech_no_cool) ) %>% mutate(COOL = "sw") ) %>% 
  left_join(share_cool %>% dplyr::select(-MW)) %>% 
  filter(!is.na(share)) %>% 
  mutate(MW = MW*share) %>% 
  mutate(TPS = paste(TPS,COOL,sep = "_")) %>% 
  dplyr::select(-share)

pplt.df <- bind_rows( pplt.df %>% 
						filter( !is.na(COOL) | (TPS %in% tech_no_cool) ) %>% 
						mutate(COOL = as.character(COOL)),
					new_tech )
row.names(pplt.df) = 1:nrow(pplt.df)							
pplt.spdf = pplt.df %>% 
	'coordinates<-'(~x+y) %>% 
	'proj4string<-'(proj4string(basin.spdf))

# Output plant level database
writeOGR(  pplt.spdf, 'input', paste(basin,'power_plants',sep='_'),  driver="ESRI Shapefile",  overwrite_layer=TRUE )	
	
# historical_new_capacity(node,tec,year_all)
years = c(1990	,2000	,2010	,2015	,2020	,2030	,2040	,2050	,2060	) # !! must match model horizon !!
hist_new_cap.df = data.frame( pplt.spdf ) %>% 
	rename( node = PID, tec = TPS, year_all = YEAR, value = MW ) %>%
	mutate( year_all = as.numeric( as.character( year_all ) ) ) %>%
	mutate( year_all = if_else(!is.na(year_all),year_all,
	                           if_else(STATUS == 'PLN',2020,2015)) ) %>% 
	mutate( units = 'MW' ) %>%
	mutate( value = round( value, digits = 3 ) ) %>%
	filter( value > 0 ) %>% 
	mutate( year_all = years[ sapply( as.numeric( as.character( year_all ) ), function( yyy ){ which.min( ( years - yyy )^2 ) } ) ] ) %>% 
	dplyr::select( node, tec, year_all, value, units ) %>%
	group_by( node, tec, year_all ) %>% summarise( value = sum( value ) ) %>% as.data.frame()

		  
# Plot the power plants							
ppls = 	unique( hist_new_cap.df$tec )	
hist_new_cap.df = hist_new_cap.df %>% mutate( tec = as.character( tec ), node = as.character( node ) )
cap = hist_new_cap.df	%>% 
	filter( tec %in% ppls ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type, year_all ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()		
cap_2017 = hist_new_cap.df	%>% 
	filter( tec %in% ppls, year_all < 2020 ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()  	
cap_2020 = hist_new_cap.df	%>% 
	filter( tec %in% ppls, year_all >= 2020 ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()  	

# existing	
pdf( 'input/check/indus_electricity_capacity_existing.pdf', width=5, height=6 )
p1 = layout( matrix( c(2,1),2,1, byrow=TRUE ), widths=c(1), heights=c(0.12,0.9) )
cap2plot = reshape(cap_2017, direction='wide', idvar = 'type', timevar = 'country' )
row.names(cap2plot) = cap2plot$type
ordr = row.names(cap2plot)
cap2plot = cap2plot[,c(2:ncol(cap2plot))]
names(cap2plot) = unlist(strsplit(names(cap2plot),'[.]'))[seq(2,2*length(names(cap2plot)),by=2)]
ordr2 = names(cap2plot)
cap2plot_tot = colSums( cap2plot, na.rm=TRUE )
cap2plot_fr = do.call( cbind, lapply( 1:ncol( cap2plot ), function( iii ){ ( 100 * cap2plot[,iii] /  colSums( cap2plot, na.rm=TRUE )[iii] ) } ) )
cap2plot_fr[is.na(cap2plot_fr)]=0
par(mar=c(2,4,0.5,3),oma=c(2,2,2,2))
cols = c( 'red','deepskyblue', 'brown', 'blue', 'green', 'black', 'goldenrod2', 'purple' )
cntrs = barplot( as.matrix( cap2plot_fr ), names.arg = names( cap2plot ), col = cols, ylab = 'Percent of Total Installed Capacity [ % ]', ylim = c(-2,100) )
abline(h=0)
abline(h=100)
abline(v=par()$usr[1])
abline(v=par()$usr[2],col='slategray')
xlim0=par()$usr[1:2]
par(new=TRUE)
plot.new()
plot.window(xlim=xlim0,ylim=c(500,round(max(cap2plot_tot)*1.05, digits=-3)),xaxs="i")
points( as.vector(cap2plot_tot) ~ cntrs, bg = 'slategray', col = 'white', pch = 21, cex = 2, lwd = 2 )
axis(side = 4, col = 'slategray', col.axis = 'slategray' )
mtext('Total Installed Capacity [ MW ]', side = 4, line = 2.75, col = 'slategray' )
par(mar=c(0.5,4,0,3))
plot.new()
legend( 'center', legend = ordr, fill = cols, bty = 'n', ncol = 3, cex = 0.9 )
dev.off()		 

pdf( 'input/check/indus_electricity_capacity_existing_map.pdf', width=6, height=6 )
plot( basin.spdf, col = NA, border = NA, xlab = 'Longitude', ylab = 'Latitude'  )
plot( 	do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) ), 
		col = alpha( c('khaki','lightgreen','lightblue','lightcoral','grey75','grey75','grey75','grey75','grey75'), alpha = 0.2 ),
		border = 'gray55', add = TRUE )	
plot( basin.sp, col = NA, border = 'grey25', lwd = 1.5, add=TRUE )
text( 69, 30, 'PAK', col = 'forestgreen',cex=1.5)
text( 76, 29, 'IND', col = 'brown',cex=1.5)
text( 80, 36, 'CHN', col = 'darkred',cex=1.5)
text( 67, 36, 'AFG', col = 'navy',cex=1.5)
pt1 = data.frame( pplt.spdf ) %>% mutate( YEAR = as.numeric( as.character( YEAR ) ) ) %>% 
	filter( YEAR < 2018 ) %>% 
	mutate( type = sapply( as.character( TPS ), function( ttt ){ unlist( strsplit( ttt, '_' ) )[1] } ) ) %>% 
	rename( x = coords.x1, y = coords.x2 ) %>%
	select( x, y, type, MW ) %>% 
	group_by( x, y, type ) %>% summarise( MW = sum (MW ) ) %>% as.data.frame() %>%
	left_join( ., data.frame( type = ordr, cols = cols ) ) %>%
	mutate( cex = ( sqrt( MW ) - min( sqrt( pplt.spdf@data$MW ) ) ) / ( max( sqrt( pplt.spdf@data$MW ) ) - min( sqrt( pplt.spdf@data$MW ) ) ) * 4 + 1 ) %>%
	'coordinates<-'(~x+y) %>% 'proj4string<-'(proj4string(basin.spdf))		
points( pt1, pch = 21, cex = pt1@data$cex, col = as.character( pt1@data$cols ), bg = alpha( 'white', alpha= 0.6 ) )
sizes = c(1, 100, 500, 1000, 5000 )
sgap = c(0,0.5,1.1,1.85,2.85)
for( iii in 1:length( sizes ) ){ 
	points( 74.2, 24+sgap[iii], cex = pt1@data$cex[ which.min( ( pt1@data$MW - sizes[iii] )^2 ) ], pch = 21, col = 'gray35', bg = 'gray35' ) 
	text( 74, 24+sgap[iii], paste0( sizes[ iii ], ' MW ' ), col = 'gray35', cex = 0.9, pos = 2 )
	}
axis(side=1,at=c())
axis(side=2,at=c())
box()
legend( 'bottomright', legend = ordr, pch = 21, pt.bg = 'white', col = cols, lwd = 2, lty = NA, pt.cex = 1.8, ncol = 2, bty = 'n', cex = 0.95 )
dev.off()


# planned
pdf( 'input/check/indus_electricity_capacity_planned.pdf', width=5, height=6 )
p1 = layout( matrix( c(2,1),2,1, byrow=TRUE ), widths=c(1), heights=c(0.12,0.9) )
cap2plot = reshape(cap_2020, direction='wide', idvar = 'type', timevar = 'country' )
row.names(cap2plot) = cap2plot$type
cap2plot = cap2plot[,c(2:ncol(cap2plot))]
names(cap2plot) = unlist(strsplit(names(cap2plot),'[.]'))[seq(2,2*length(names(cap2plot)),by=2)]
if( !('CHN' %in% names(cap2plot) ) ){ cap2plot = cbind( data.frame( CHN = rep( NA, nrow( cap2plot ) ) ), data.frame( cap2plot ) ) }
cap2plot = cap2plot[ ,ordr2]
cap2plot = cap2plot[ordr,]
cap2plot_tot = colSums( cap2plot, na.rm=TRUE )
cap2plot_fr = do.call( cbind, lapply( 1:ncol( cap2plot ), function( iii ){ ( 100 * cap2plot[,iii] /  colSums( cap2plot, na.rm=TRUE )[iii] ) } ) )
cap2plot_fr[is.na(cap2plot_fr)]=0
par(mar=c(2,4,0.5,3),oma=c(2,2,2,2))
cntrs = barplot( as.matrix( cap2plot_fr ), names.arg = names( cap2plot ), col = cols, ylab = 'Percent of Total Planned Capacity [ % ]', ylim = c(-2,100) )
abline(h=0)
abline(h=100)
abline(v=par()$usr[1])
abline(v=par()$usr[2],col='slategray')
xlim0=par()$usr[1:2]
par(new=TRUE)
plot.new()
plot.window(xlim=xlim0,ylim=c(800,round(max(cap2plot_tot)*1.05, digits=-3)),xaxs="i")
points( as.vector(cap2plot_tot) ~ cntrs, bg = 'slategray', col = 'white', pch = 21, cex = 2, lwd = 2 )
axis(side = 4, col = 'slategray', col.axis = 'slategray' )
mtext('Total Planned Capacity [ MW ]', side = 4, line = 2.75, col = 'slategray' )
par(mar=c(0.5,4,0,3))
plot.new()
legend( 'center', legend = ordr, fill = cols, bty = 'n', ncol = 3, cex = 0.9 )
dev.off()		  


pdf( 'input/check/indus_electricity_capacity_planned_map.pdf', width=6, height=6 )
plot( basin.spdf, col = NA, border = NA, xlab = 'Longitude', ylab = 'Latitude' )
plot( 	do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) ), 
		col = alpha( c('khaki','lightgreen','lightblue','lightcoral','grey75','grey75','grey75','grey75','grey75'), alpha = 0.2 ),
		border = 'gray55', add = TRUE )	
plot( basin.sp, col = NA, border = 'grey25', lwd = 1.5, add=TRUE )
text( 69, 30, 'PAK', col = 'forestgreen',cex=1.5)
text( 76, 29, 'IND', col = 'brown',cex=1.5)
text( 80, 36, 'CHN', col = 'darkred',cex=1.5)
text( 67, 36, 'AFG', col = 'navy',cex=1.5)
pt1 = data.frame( pplt.spdf ) %>% mutate( YEAR = as.numeric( as.character( YEAR ) ) ) %>% 
	filter( YEAR >= 2018 ) %>% 
	mutate( type = sapply( as.character( TPS ), function( ttt ){ unlist( strsplit( ttt, '_' ) )[1] } ) ) %>% 
	select( x, y, type, MW ) %>% 
	group_by( x, y, type ) %>% summarise( MW = sum (MW ) ) %>% as.data.frame() %>%
	left_join( ., data.frame( type = ordr, cols = cols ) ) %>%
	mutate( cex = ( sqrt( MW ) - min( sqrt( pplt.spdf@data$MW ) ) ) / ( max( sqrt( pplt.spdf@data$MW ) ) - min( sqrt( pplt.spdf@data$MW ) ) ) * 4 + 1 ) %>%
	'coordinates<-'(~x+y) %>% 'proj4string<-'(proj4string(basin.spdf))		
points( pt1, pch = 21, cex = pt1@data$cex, col = as.character( pt1@data$cols ), bg = alpha( 'white', alpha= 0.6 ) )
for( iii in 1:length( sizes ) ){ 
	points( 74.2, 24+sgap[iii], cex = pt1@data$cex[ which.min( ( pt1@data$MW - sizes[iii] )^2 ) ], pch = 21, col = 'gray35', bg = 'gray35' ) 
	text( 74, 24+sgap[iii], paste0( sizes[ iii ], ' MW ' ), col = 'gray35', cex = 0.9, pos = 2 )
	}
axis(side=1,at=c())
axis(side=2,at=c())
box()
legend( 'bottomright', legend = ordr, pch = 21, pt.bg = 'white', col = cols, lwd = 2, lty = NA, pt.cex = 1.8, ncol = 2, bty = 'n', cex = 0.95 )
dev.off()

# Output to CSV for reading as input to MESSAGE 

hist_new_cap.df$units = 'MW'

if( !file.exists( "input/historical_new_cap.csv" ) ){ fl = hist_new_cap.df }else{
	
	fl = bind_rows( read.csv( "input/historical_new_cap.csv" ) %>% filter( !( units == 'MW' ) ), hist_new_cap.df )
						
	}
	
write.csv( fl, file = "input/historical_new_cap.csv", row.names = F ) 		  
		  
