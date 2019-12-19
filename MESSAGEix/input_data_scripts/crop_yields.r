
# aggregating crop yields in the basin catchments
require(rgeos)
require(ncdf4)
require(rgdal)
require(raster)
require(dplyr)
require(tidyr)

# Location of input data
setwd( 'P:/is-wel/indus/message_indus' )

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")

# Basin analyzed
basin = 'Indus'

#basin shape file
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))

# Create buffered basin polygon for gridded data along the border
buff.sp = gBuffer( basin.sp, width=0.1 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)


# For the irrigated areas, we need to account for the massive diversions to outside the basin 
# Irrigation command areas are identified from the gridded irrigation withdrawals harmonized  in Cheema et al. 2014
IRR_cw.raster = raster( 'input/IRR_cw.img' )
IRR_cw.raster = projectRaster( setValues(raster(IRR_cw.raster),IRR_cw.raster[]), crs = crs( basin.spdf ) )

# Get withdrawals within the hydrological basin boundary
IRR_cw0.raster = mask( 	x = crop( IRR_cw.raster, extent( basin.spdf ), snap = "out" ), 
						mask = rasterize( basin.spdf, crop( IRR_cw.raster, extent( basin.spdf ), snap="out" ) ) )
IRR_cw0.raster = IRR_cw0.raster / 1e6 * area( IRR_cw0.raster ) * 1e3 / 365 # convert to MCM per day
IRR_cw0.df = data.frame( 	PID = basin.spdf@data$PID, 
							val = sapply( raster::extract( IRR_cw0.raster, basin.spdf ), sum, na.rm=TRUE ) )

# Create raster and remove grid cells included in the basin already
IRR_cw1.raster = IRR_cw.raster
IRR_cw1.raster = IRR_cw1.raster / 1e6 * area( IRR_cw1.raster ) * 1e3 / 365 # convert to MCM per day
basin_cells = do.call( rbind, raster::extract( IRR_cw.raster, basin.spdf, na.rm=TRUE, cellnumbers = TRUE ) )[,'cell']
IRR_cw1.raster[ basin_cells ] = NA

# Convert to spatial points
ccc = which( !is.na( IRR_cw1.raster[] ) )
IRR_cw1.pts = data.frame( 	x = xFromCell( IRR_cw1.raster, cell = ccc ),
							y = yFromCell( IRR_cw1.raster, cell = ccc ),
							val = IRR_cw1.raster[ ccc ] )
coordinates(IRR_cw1.pts) = ~x+y						
proj4string(IRR_cw1.pts) = proj4string(basin.spdf)					
					
# Get nearest basin polygons
basinr.spdf = basin.spdf[ c( 9, 13, 19, 20 ), ] # Only including basins along boundary - IDENTIFIED MANUALLY
IRR_cw1.pts$PID = sapply( 1:nrow(IRR_cw1.pts), function( iii ){ basinr.spdf@data$PID[ which.min( gDistance( IRR_cw1.pts[iii,], basinr.spdf, byid=TRUE ) ) ] } )

# Ensure all grid cells in India allocated to indian sub-basin
ind = getData( 'GADM', country = 'IND', level = 0 )[,'ID_0']
IRR_cw1.pts$PID[ which( over( IRR_cw1.pts, ind )[,'ID_0'] == 'IND' ) ] =  'IND_4'

# Convert to polygons
IRR_cw1.pts = merge( IRR_cw1.pts, data.frame( PID = unique( IRR_cw1.pts$PID ), ID = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.raster = rasterize( IRR_cw1.pts, IRR_cw1.raster, field = 'ID' )
IRR_pid.spdf = rasterToPolygons( IRR_pid.raster, dissolve = TRUE )
IRR_pid.spdf@data = left_join( IRR_pid.spdf@data, data.frame( PID = unique( IRR_cw1.pts$PID ), layer = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.spdf = IRR_pid.spdf[,'PID']

# Bind and dissolve with original basin polygons
basin_irr.spdf = aggregate( rbind( basin.spdf[,'PID'], IRR_pid.spdf ) , 'PID' )
basin_irr.spdf@data = left_join( basin_irr.spdf@data, basin.spdf@data ) 

# Output the irrigation area polygons
writeOGR( basin_irr.spdf, 'input', paste(basin,'irrigated_bcu',sep='_'),  driver="ESRI Shapefile",  overwrite_layer=TRUE )

# Use version with irrigated areas to calibrate the crop implementation
basin0.spdf = basin.spdf
basin.spdf = basin_irr.spdf

# Get data from GAEZ
# path with yield data, from GAEZ
yeld_path = paste0(getwd(),'/input/land_maps_crop_yields/YIELD')

# Crops to inlcude - must match the technology file
crop_names = c('wheat','rice','cotton','fodder','sugarcane','pulses','maize','fruit','vegetables')

# Get data for each crop - these are potential yields with irrigation
irr.df = bind_rows( lapply( seq_along(crop_names), function( ii ){
	
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')

	irr_crop.rs = raster(paste0(crop_folder,'/',crop_file) )

	# crop with basin extent
	irr_crop_basin.rs = crop( irr_crop.rs, extent(basin.spdf) )

	irr_crop_basin.sp = rasterToPoints(irr_crop_basin.rs, spatial = T)

	irr_crop_basin.sp = spTransform(irr_crop_basin.sp, crs(  basin.spdf ) )
	names(irr_crop_basin.sp) = 'value'
	irr_crop_basin.sp$node = as.character(over(irr_crop_basin.sp , basin.spdf)[,'PID'] )
	irr_crop_basin.sp = irr_crop_basin.sp[!is.na(irr_crop_basin.sp$node),]

	df_i =irr_crop_basin.sp@data %>% 
	  mutate(crop = crop_names[ii])

	return( df_i ) 

	} ) )

# exclude point in which yield is 0 because of no crop availability
irr_yield_out = irr.df %>% 
  filter(value != 0 ) %>% 
  group_by(node,crop) %>% 
  summarise(value = mean(value)) %>%  #convert from kg DW/ ha = ton/Mha
  mutate(value = round(value, 4)) %>% 
  mutate(unit = 'kton DW/ Mha') %>% 
  mutate(par = 'irrigation_yield') %>% 
  mutate(time = 'year') %>% 
  select(crop,par,node,time,unit,value)

# same thing for rain-fed crops - this is the yield potential
rain.df = bind_rows( lapply( seq_along(crop_names), function( ii ){
	
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')

	rain_crop.rs = raster(paste0(crop_folder,'/',crop_file) )

	# crop with basin extent
	rain_crop_basin.rs = crop( rain_crop.rs, extent(basin.spdf) )

	rain_crop_basin.sp = rasterToPoints(rain_crop_basin.rs, spatial = T)

	rain_crop_basin.sp = spTransform(rain_crop_basin.sp, crs(  basin.spdf ) )
	names(rain_crop_basin.sp) = 'value'
	rain_crop_basin.sp$node = as.character(over(rain_crop_basin.sp , basin.spdf)[,'PID'] )
	rain_crop_basin.sp = rain_crop_basin.sp[!is.na(rain_crop_basin.sp$node),]

	df_i =rain_crop_basin.sp@data %>% 
	mutate(crop = crop_names[ii])

	return( df_i )
  
	} ) )

# exclude point in which yield is 0 because of no crop availability
rain_yield_out = rain.df %>% 
  filter(value != 0 ) %>% 
  group_by(node,crop) %>% 
  summarise(value = round( mean(value)) , digits = 4) %>% # convert from kg DW/ ha = kton/Mha
  mutate(value = round(value, 4)) %>% 
  mutate(unit = 'kton DW/ Mha') %>% 
  mutate(par = 'rain-fed_yield') %>% 
  mutate(time = 'year') %>% 
  select(crop,par,node,time,unit,value)

existing_csv = read.csv(paste0(getwd(), '/input/crop_input_data.csv')) %>% 
  filter(unit != 'kton DW/ Mha')
to_csv = bind_rows(existing_csv,irr_yield_out, rain_yield_out)
write.csv(to_csv, paste0(getwd(), '/input/crop_input_data.csv'), row.names = F)

# Crop production data in 2000, summing irrigated and rainfed crops 
# data from GAEZ, original unit: 1000t -  this is the historical production capacity
prod_path = paste0(getwd(),'/input/land_maps_crop_yields/PROD')

prod.df = bind_rows( lapply( seq_along(crop_names), function( ii ){
	
	crop_folder = list.dirs(path = prod_path)
	crop_folder = crop_folder[ grep( paste0( crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')

	prod_crop.rs = raster(paste0(crop_folder,'/',crop_file) )
	
	# crop with basin extent
	prod_crop_basin.rs = crop( prod_crop.rs, extent(basin.spdf) )
	
	prod_crop_basin.sp = rasterToPoints(prod_crop_basin.rs, spatial = T)

	prod_crop_basin.sp = spTransform(prod_crop_basin.sp, crs(  basin.spdf ) )
	names(prod_crop_basin.sp) = 'value'
	prod_crop_basin.sp$node = as.character(over(prod_crop_basin.sp , basin.spdf)[,'PID'] )
	prod_crop_basin.sp = prod_crop_basin.sp[!is.na(prod_crop_basin.sp$node),]

	df_i =prod_crop_basin.sp@data %>% 
	mutate(crop = crop_names[ii])

	return( df_i )
  
	} ) )

# exclude point in which production is 0 because of no crop availability
prod_out = prod.df %>% 
  filter(value != 0 ) %>% 
  group_by(node,crop) %>% 
  summarise(value = sum(value)) %>%  # sum of production in the area
  mutate(value = round(value, 4)) %>% 
  mutate(unit = 'kton') %>% 
  mutate(par = 'production_2000') %>% 
  mutate(time = 'year') %>% 
  select(crop,par,node,time,unit,value)

#calculate the multiplication factor for years after 2000, China data just remain unchanged as in 2000
# from FAO national harvested area 1990-2015 trends, used as multiplication factors for the spatial harvested area in 2000
# this gives the historical capacity of crop technologies in 2015

maps_path = paste0(getwd(),'/input/land_maps_crop_yields')
national_trend.path = path.expand(paste0(maps_path,'/FAO_hist_national_prod'))
nat_trend.files = list.files(path = national_trend.path,pattern = '.csv')

inc_rate2000 = NULL
for (jj in seq_along(nat_trend.files)){
  
	tmp_national = read.csv(paste0(national_trend.path,'/',nat_trend.files[jj])) %>% 
		filter(Unit == 'tonnes', Year <= 2015 ) %>% 
		mutate(Value = Value * 1E-3, Unit = 'kton') %>% # Mha
		group_by(Area,Item) %>% 
		mutate(mult = (lm(Value ~ Year)$coefficients[[2]]) ) %>%
		filter(Year == 2000) %>% 
		mutate(mult = mult / Value) %>% 
		# mutate(Year = paste0('v',Year)) %>% 
		# select(Area,Item,Year,Unit,Value) %>% 
		# spread(Year,Value) %>% 
		# mutate(perc_inc = (v2015-v2000)/v2000) %>% 
		dplyr::select(Area,Item,Unit,mult)
	# we want only mult at the end
	inc_rate2000 = bind_rows(inc_rate2000,tmp_national)
	}

inc_rate2000[ inc_rate2000$Area == 'Pakistan' & inc_rate2000$Item == 'Oranges',]$Item = 'Fruit, citrus nes'
veg_to_aggregate = inc_rate2000 %>% 
  filter(Item %in% c('Onions, dry', 'Potatoes')) %>% 
  group_by(Area,Unit) %>% summarise(mult = mean(mult)) %>% ungroup() %>% 
  mutate(Item = 'vegetables') %>% select(Area,Item,Unit,mult)

inc_rate2000 = inc_rate2000 %>% 
  filter(!Item %in% c('Onions, dry', 'Potatoes')) %>% 
  bind_rows(veg_to_aggregate)

crop_item_map = data.frame(Item = unique(inc_rate2000$Item), crop = c('cotton', 'pulses', 'rice','sugarcane','wheat','fodder','maize','fruit','vegetables' ) )
country_map = data.frame(Area = unique(inc_rate2000$Area), node2 = c('AFG', 'IND', 'PAK') )

inc_rate2000 = inc_rate2000 %>% left_join(crop_item_map) %>% 
	left_join(country_map) %>% as.data.frame() %>% 
	select(node2,crop,mult) %>% unique()

year_all = c(2015,2020,2030,2040,2050,2060)

prod_out = prod_out %>% 
	tidyr::expand( prod_out, year_all) %>% unique() %>% 
	rename(v2000 = value) %>% 
	mutate(node2 = gsub('_.*','',node) ) %>% 
	left_join(inc_rate2000) %>% 
	mutate(value = if_else(v2000 == 0,0 ,v2000 + (v2000* mult * (year_all - 2000))) ) %>% 
	mutate(value = if_else(value <= 0,0,value)) %>% 
	mutate(scenario = 'SSP2',sector = 'crop',month = 1) %>% 
	rename(type = crop, pid = node, units = unit, year = year_all) %>% 
	select(scenario,	sector,	type,	pid,	year,	month,	value,	units)

pid_pop_gdp = read.csv( "input/PID_pop_gdp.csv", stringsAsFactors=FALSE )

#consider SSP2 as reference, million people
country_pop = read.csv('input/OECD_SSP_POP.csv', stringsAsFactors = F) %>% 
	filter(Country_Name %in% c('Afghanistan', 'India','Pakistan')) %>% 
	filter(grepl('SSP2|SSP1|SSP5',Scenario)) %>% 
	gather('year','value',10:22) %>% 
	dplyr::select(Scenario,Region,year,value) %>% rename(node = Region,scenario = Scenario) %>% 
	mutate( year = gsub('X','',year), scenario = gsub('_.*','',scenario))

rel_to_ssp2 = country_pop %>% 
	group_by(scenario,node,year) %>% 
	left_join(country_pop %>% filter(scenario == 'SSP2') %>% rename(SSP2 = scenario,valssp2 = value)) %>% 
	as.data.frame( ) %>% 
	mutate(increase = ((value - valssp2)/value) ) %>% 
	mutate(year = as.numeric(year)) %>% 
	rename(country = node) %>% 
	select(scenario,country,year,increase)

prod_out2 = prod_out %>% 
  bind_rows( prod_out %>% as.data.frame() %>% 
              mutate(country = gsub('_.*','',pid), scenario = 'SSP1') %>% 
              left_join(rel_to_ssp2 %>% filter(year %in% unique(prod_out$year))) %>% 
              filter(!is.na(value)) %>% 
              mutate(value = value * (1 + increase) )%>% 
              select(scenario,	sector,	type,	pid,	year,	month,	value,	units)) %>% 
  bind_rows(prod_out %>% as.data.frame() %>% 
              mutate(country = gsub('_.*','',pid), scenario = 'SSP5') %>% 
              left_join(rel_to_ssp2 %>% filter(year %in% unique(prod_out$year))) %>% 
              filter(!is.na(value)) %>% 
              mutate(value = value * (1 + increase) ) %>% 
              select(scenario,	sector,	type,	pid,	year,	month,	value,	units))

existing_csv = read.csv(paste0(getwd(), '/input/indus_demands_new.csv'))
#remove existing crop demand, if any
to_csv = bind_rows(existing_csv %>% filter(sector != 'crop'), prod_out2)
write.csv(to_csv, paste0(getwd(), '/input/indus_demands_new.csv'), row.names = F )

## Define irrigation water input per unit area using data from CWaTM

# Set climate scenarios and models to use
climate_models = c( "ensemble", "gfdl-esm2m", "hadgem2-es", "ipsl-cm5a-lr", "miroc5" ) 
climate_scenarios = c("historical","rcp26","rcp60")

# Create data frame with the irrigation water input data for each model-scenario combination
irrwat.df = bind_rows( lapply( climate_scenarios, function( climate_scenario ){ 
	
	bind_rows( lapply( climate_models, function( climate_model ){ 
		
		print( 'Working on:' )
		print( climate_scenario )
		print( climate_model )
		
		if( file.exists( paste0( 'input/hydroclimate_input/etref_metersperday_',climate_model,'_', climate_scenario, '.tif' ) ) )
			{
			
			ETref.stack = stack( paste0( 'input/hydroclimate_input/etref_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(ETref.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			Peff10.stack = stack( paste0( 'input/hydroclimate_input/effprecip10day_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(Peff10.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			Peff3.stack = stack( paste0( 'input/hydroclimate_input/effprecip3day_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(Peff3.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			Precip.stack = stack( paste0( 'input/hydroclimate_input/precip_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(Precip.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 

			# Resample to match GAEZ data 
			crop_folder = list.dirs(path = yeld_path)
			crop_folder = crop_folder[ grep( paste0('irr_', crop_names[1]), crop_folder)]
			crop_file = list.files(path = crop_folder,pattern = '.tif')
			tmp = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
			ETref.stack = resample( ETref.stack, tmp, method = 'bilinear' )
			Peff10.stack = resample( Peff10.stack, tmp, method = 'bilinear')
			Peff3.stack = resample( Peff3.stack, tmp, method = 'bilinear' )

			# Import crop coefficients
			crop_coefficients.df = read.csv( 'input/crop_tech_data.csv' )
			
			# Average across PIDs 
			df1 = bind_rows( lapply( 1:length(crop_names), function( ii ){ 
				
				# Get the irrigated and rainfed yield for this specific crop
				crop_folder = list.dirs(path = yeld_path)
				crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
				crop_file = list.files(path = crop_folder,pattern = '.tif')
				irr_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
				crop_folder = list.dirs(path = yeld_path)
				crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
				crop_file = list.files(path = crop_folder,pattern = '.tif')
				rain_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
				
				# Use irrigated yield for mask to eliminate cells where no yield occurs
				msk = irr_yield
				msk[ msk[] > 0 ] = 1
				msk[ msk[] != 1 ] = NA
				msk = resample( msk, ETref.stack, method = 'ngb')
				
				# Use correct effective precipoitation based on whether paddy or non-paddy
				if( crop_names[ ii ] == 'rice' ){ Peff.stack = Peff3.stack * msk }else{ Peff.stack = Peff10.stack * msk }
				
				
				# FAO equation for irrigation water requirement
				kc = rep( (crop_coefficients.df %>% filter(crop == crop_names[ii] & par == 'crop_coef'))$value, nlayers( ETref.stack ) / 12 )
				st =  kc * ETref.stack - Peff.stack # in meters per day
				st[ st < 0 ] = 0
				names( st ) = names(ETref.stack)
				
				# Get values for each basin PID
				lst = raster::extract( st, basin.spdf, byid=TRUE )
				yr = ( unlist( strsplit( names( st ), '[.]' ) )[seq(1,2*length(names(st)),by=2)] )
				yr = unlist( strsplit( yr, 'X' ) )[seq(2,2*length(yr),by=2)]
				mn = ( unlist( strsplit( names( st ), '[.]' ) )[seq(2,2*length(names(st)),by=2)] )
				df = bind_rows( lapply( 1:length( lst ), function( x ){
					data.frame( scenario = climate_scenario,
								model = climate_model,
								crop = crop_names[ ii ],
								node = basin.spdf@data$PID[ x ],
								year = yr,
								time = mn,
								unit = 'MCM_per_day_per_Mha',
								value = 1e4 * sapply( 1:ncol( lst[[ x ]] ), function( jjj ){ mean( lst[[ x ]][ , jjj ], na.rm = TRUE ) } ) ) # 1e4 is conversion from meters per day to MCM_per_day_per_Mha
					} ) )
				df$value[ is.nan( df$value ) ] = 0
				
				return( df )
				
				} ) )
				
			return(df1)
			
			}		
			
		} ) )
		
	} ) )

# Compare the uncalibrated irrigation intensities
windows()
p1 = layout( matrix( c(1,2), 1,2, byrow=TRUE ), widths=c(0.5,0.5), heights=c(0.5) )
plot( irrwat.df %>% filter( scenario == 'historical' ) %>% dplyr::select( value ) %>% unlist(), irrwat.df %>% filter( scenario == 'rcp26' ) %>% dplyr::select( value ) %>% unlist(), xlab = 'historical', ylab = 'rcp26', main = 'MCM_per_day_per_Mha' )
abline(0,1, col='red')
plot( irrwat.df %>% filter( scenario == 'historical' ) %>% dplyr::select( value ) %>% unlist(), irrwat.df %>% filter( scenario == 'rcp60' ) %>% dplyr::select( value ) %>% unlist(), xlab = 'historical', ylab = 'rcp60', main = 'MCM_per_day_per_Mha' )
abline(0,1, col='red')

irrwat.df %>% filter(value > 0) %>% 
  ggplot()+
  geom_boxplot(aes(x = crop, y = value))+
  theme_bw()+ylab('MCM per day per Mha')+
  ggtitle('Crop water withdraw factor before calibration (RCP 6.0)')
  
	
# Write to csv
write.csv( irrwat.df, paste0( getwd(), '/input/crop_irrigation_water.csv'), row.names = FALSE )
	
## Calibrate the irrigation intensities based on historical canal and aquifer diversions reported in Cheema et al. 2014.
	
# Read uncalibrated data from the csv
irrwat.df = read.csv( paste0( getwd(), '/input/crop_irrigation_water.csv'), stringsAsFactors = FALSE )
			
# Aggregate canal withdrawals outside basin to sub basin level
IRR_cw1.df = data.frame( IRR_cw1.pts ) %>%
	dplyr::select( PID, val ) %>%
	group_by( PID ) %>%
	summarise( val = sum( val, na.rm = TRUE ) ) %>%
	as.data.frame( ) %>%
	data.frame( )
	
# Combine withdrawals occurring within and outside basin	
IRR_cw.df = rbind( IRR_cw0.df, IRR_cw1.df ) %>% group_by( PID ) %>% summarise( val = sum( val ) ) %>% as.data.frame() %>% data.frame()	

# Irrigation groundwater withdrawal
IRR_gw.raster = raster( 'input/IRR_gw.img' )
IRR_gw.raster = projectRaster( setValues(raster(IRR_gw.raster),IRR_gw.raster[]), crs = crs( basin.spdf ) )
IRR_gw.raster = mask( 	x = crop( IRR_gw.raster, extent( basin.spdf ), snap = "out" ), 
						mask = rasterize( basin.spdf, crop( IRR_gw.raster, extent( basin.spdf ), snap="out" ) ) )
IRR_gw.raster = IRR_gw.raster / 1e6 * area( IRR_gw.raster ) * 1e3 / 365 # convert to MCM per day
IRR_gw.df = data.frame( PID = basin.spdf@data$PID,
						val = sapply( raster::extract( IRR_gw.raster, basin.spdf ), sum, na.rm=TRUE ) )
			
# Combined df - amount of canal and groundwater diversions at the farm gate (still need to account for field efficiency)
# Note that the canal diversions also need to account for conveyance losses to obtain the actual amount taken from rivers
irr_cheema.df = left_join( 	IRR_cw.df %>% mutate( cw = val ) %>% select( PID, cw ) , 
							IRR_gw.df %>% mutate( gw = val ) %>% select( PID, gw ) )

# From Cheema et al.: The total irrigation supplies in the irrigated areas of the Indus Basin were estimated at 181 km3, 
# an amount of 68 km3 originated from groundwater, while the surface water contribution was 113 km3. 
# This diagnosis suggests that groundwater supplies 68/181 or 38% of the total water applied at the farm gate. 
# The results are in agreement with the 40% to 50% groundwater contribution reported by Sarwar and Eggers (2006).

irr_cheema.km3 = irr_cheema.df %>% mutate(cw = cw * 365 / 1e3,
                                          gw = gw * 365 / 1e3)
# Surface total
sum(irr_cheema.km3$cw) # 122 km3

# ground total
sum(irr_cheema.km3$gw) # 67 km3
#total 189 km3

# But the irrigation data from Cheema et al. does not consider irrigation withdrawals in Afghanistan
# Check Yoshi's data to see if any irrigation withdrawals in Afghanistan

afg = gUnaryUnion( basin.spdf[ grepl( 'AFG', basin.spdf@data$PID ), ] )

# Groundwater abstraction fro Wada et al
nc = nc_open('input/Wada_groundwater_abstraction/waterdemand_30min_groundwaterabstraction_million_m3_month.nc', verbose=FALSE)
gwabstract.stack = stack( 'input/Wada_groundwater_abstraction/waterdemand_30min_groundwaterabstraction_million_m3_month.nc' )
extent(gwabstract.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( gwabstract.stack ) = proj4string( basin.spdf )
gwabstract.stack = crop( gwabstract.stack, extent(afg) )
names(gwabstract.stack) = c( sapply( 1:(nlayers(gwabstract.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
gwabstract.stack = gwabstract.stack[[ c( which(grepl( 'X2010',names(gwabstract.stack) )) ) ]] # keep 2010

# create mask with the afg grid cells
afg.raster = mask( x = crop( gwabstract.stack[[1]], extent( afg ), snap = "out" ), mask = rasterize( afg, crop( gwabstract.stack[[1]], extent( afg ), snap="out" ) ) )
gwabstract.stack = gwabstract.stack * afg.raster

# Irrigation
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4', verbose=FALSE)
irrigation.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4' )
extent(irrigation.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( irrigation.stack ) = proj4string( basin.spdf )
irrigation.stack = crop( irrigation.stack, extent(afg) )
names(irrigation.stack) = c( sapply( 1:(nlayers(irrigation.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
irrigation.stack = irrigation.stack[[ c( which(grepl( 'X2010',names(irrigation.stack) )) ) ]] # keep 2010
irrigation.stack = irrigation.stack * afg.raster

# Existing groundwater capacity from initial extraction levels
total.stack = irrigation.stack
total.stack  =  sum(stack(total.stack))
total_gwabstract.stack = sum(stack(gwabstract.stack))
gwfraction.stack = ( total_gwabstract.stack / total.stack )
gwfraction.stack[gwfraction.stack[]>1]=1
gwfraction.stack[is.na(gwfraction.stack)] = 0

sum( irrigation.stack[], na.rm = TRUE ) # it's zero! WTF!

# But we know from the literature that there are irrigation withdrawals in the Kabul river basin
# From: Scoping Strategic Options for Development of the Kabul River Basin A MulTISecTORAl DecISION SuppORT SySTeM AppROAcH (World Bank)
# Table 3.1 indicates 3,389 Mm3 per year of irrigation - Allocate to AFG_2 which includes Kabul River
# AFG uses 2.8/15.8=18%, see 'Irrigation Outreach in Afghanistan: Exposure to Afghan Water Security Challenges'
#historical_demands.df$value[ historical_demands.df$node == 'AFG_2' & historical_demands.df$sector == 'irrigation' ] = round( 3389 / 365, digits = 2 )
irr_cheema.df[ irr_cheema.df$PID == 'AFG_2', c( 'cw', 'gw'  ) ] = round( c( 0.82, 0.18 )*3389 / 365, digits = 2 )

# Add historical capacity of pumping systems in irrigation
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
ind = which( historical_capacity.df1$tec %in% c( 'irrigation_sw_diversion', 'irrigation_gw_diversion' ) ) 
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] } # remove any existing entries 
historical_capacity.df1 = historical_capacity.df1 %>%
	filter( ! tec %in% c( 'irrigation_sw_diversion', 'irrigation_gw_diversion' ) ) %>%
	rbind( ., irr_cheema.df %>% 
				dplyr::select( PID, cw ) %>% 
				dplyr::rename( node = PID, value = cw ) %>% 
				mutate( tec = 'irrigation_sw_diversion', year_all = 2015, units = 'mcm_per_day', value = round( value, digits = 3 ) ) %>%
				dplyr::select( names( historical_capacity.df1 ) ) ) %>%
	rbind( ., irr_cheema.df %>% 
				dplyr::select( PID, gw ) %>% 
				dplyr::rename( node = PID, value = gw ) %>% 
				mutate( tec = 'irrigation_gw_diversion', year_all = 2015, units = 'mcm_per_day', value = round( value, digits = 3 ) ) %>%
				dplyr::select( names( historical_capacity.df1 ) ) )

# Write to csv for importing into messageix later	
write.csv( 	historical_capacity.df1, 
			"input/historical_new_cap.csv", 
			row.names = FALSE )

# We have already calculated irrigated and rainfed area for each crop 2015
# calculations in land_use_maps.r data in crop_input_data.csv
# GAEZ data are for 2000, those in crop_input_data.csv have been scaled for 2015

# Historical area of each crop	
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
hist_area.df = read.csv( "input/crop_input_data.csv", stringsAsFactors=FALSE ) %>% 
  filter(par == 'crop_irr_land_2015') %>% dplyr::rename(irr_Mha = value) %>% 
  select(crop,node,irr_Mha)

# load historical area in 2000 to check
hist_area_2000 = bind_rows( lapply( 1:length(crop_names), function( ii ){ 
  
  # Get the irrigated and rainfed yield for this specific crop
  crop_folder = list.dirs( path = 'P:\\is-wel\\indus\\message_indus\\input\\land_maps_crop_yields' )
  crop_folder = crop_folder[ grepl( crop_names[ii], crop_folder ) & grepl( 'act2000', crop_folder ) & grepl( '2000har', crop_folder ) & !grepl( '_r_', crop_folder ) ]
  crop_file = list.files(path = crop_folder,pattern = '.tif')
  irr_area = raster( paste0(crop_folder,'/',crop_file) ) # in Mha
  irr_area = mask( x = crop( irr_area, extent( basin.sp ), snap = "out" ), mask = rasterize( basin.sp, crop( irr_area, extent( basin.sp ), snap="out" ) ) )
  
  # Output crop area maps in 1000 hectares
  writeRaster( irr_area, paste0( 'input/crop_area_map_', crop_names[ ii ], '.asc' ), format = 'ascii', overwrite = TRUE )
  
  # Get the area in each PID
  lst1 = raster::extract( irr_area, basin.spdf, byid = TRUE )
  
  df = bind_rows( lapply( 1:length( basin.spdf ), function( jjj ){
    
    data.frame( crop = crop_names[ ii ],
                node = basin.spdf@data$PID[ jjj ],
                irr_Mha = round( 1e-3 * sum( lst1[[ jjj ]], na.rm = TRUE ), digits = 4 ) ) # convert from 1000 ha to Mha
    
  } ) )
  
  return( df )
  
} ) )	
# Stack maps for plotting if you want to check
hist_area.raster = do.call( stack, lapply( crop_names, function( iii ){ raster( paste0( 'input/crop_area_map_', iii, '.asc' ) ) } ) )	
	
# Define average efficiency for water once it reaches the farm-gate 
# Assuming all irrigation is basically flood irrigation based on disucssions with stakeholders
# Also aligns closely with asssumptions in Wu et al. (2013, World Bank) figure 5.4 
field_efficiency = 0.85
water_course_efficiency = 0.55
irrigation_canal_efficiency = 0.75
irrigation_gw_efficiency = 0.95
flood_efficiency = 0.4

# files for external calibration in GAMS
crop_water_use.gms = irrwat.df %>% 
  select(scenario,model,crop,node,year,time,value) %>% 
  mutate(value = value / (field_efficiency * flood_efficiency * water_course_efficiency) ) %>% 
  dplyr::rename(scen = scenario,mod = model, values = value)

hist_land.gms = hist_area.df %>% dplyr::rename(values = irr_Mha)

ch.gms = irr_cheema.df %>% # in km3 per year
  mutate( tw = ( cw * irrigation_canal_efficiency + gw *irrigation_gw_efficiency)  * 365 / 1e3) %>% 
  select(PID,tw) %>% 
  dplyr::rename(node = PID, values = tw)

sum((ch.gms %>% mutate(values = values ))$values)

time.set = unique(crop_water_use.gms$time)

# write gdx
gams_path = 'C:/GAMS/win64/24.9'
par_list = lapply( ls(pattern='\\.gms'), get )
names(par_list) = unlist( strsplit( ls(pattern='\\.gms'), '[.]' ) )[seq(1,2*length(ls(pattern='\\.gms')),by=2)]
par_list = lapply( seq_along(par_list), function( iii ){
  df = data.frame( par_list[[ iii ]] )
  df[names(df)[1:(ncol(df)-1)]]=lapply( df[names(df)[1:(ncol(df)-1)]], factor )
  df[names(df)[ncol(df)]]=lapply( df[names(df)[ncol(df)]], as.numeric )
  if( 'time' %in% names( df ) ){ df$time = factor( df$time, levels = time.set ) } # make sure ordered correctly
  attr( df, 'symName' ) = names( par_list )[ iii ]
  attr( df, 'type' ) = 'parameter'
  return(df)
} )

input_folder = 'P:/is-wel/indus/message_indus' 
setwd( 'P:/is-wel/indus/message_indus/input/irrigation_water_calib' )
fl = paste( getwd(), '/water_calib_in.gdx',sep='') 
require( gdxrrw )
# Write to gdx 
wgdx.lst( fl, c( par_list ), squeeze = FALSE )

#run gams calibration
cmd = paste0( "gams water_calib.gms" )
res = system(cmd)
if( res == 0 ) { print( 'all good!' ) } else { print( 'Error in GAMS solution' ) }

# read gdx after calibration
igdx( gams_path )
upath = getwd()
res.list = lapply( c('/water_calib_out.gdx'), function(fpath){ 
  vars = c( 'new_crop_water_use','new_hist_land')
  gdx_res = lapply( vars, function( vv ){
    tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
    names(tmp$uels) = tmp$domains
    rs = data.frame( tmp$val )
    names(rs) = c( unlist( tmp$domains ), 'value' )
    rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
    return(rs)
  } )
  names(gdx_res) = vars	
  return(gdx_res)
} )

irrwatc_new.df = res.list[[1]]$new_crop_water_use %>% as.data.frame() %>% 
  rename(scenario = scen, model = mod)  %>% 
  mutate(value = value * (field_efficiency * flood_efficiency * water_course_efficiency) )

ymax = max(irrwatc_new.df$value,irrwat.df$value)*1.01
ggplot()+
  geom_boxplot(data = irrwat.df %>% filter(value > 10),
               aes(x = crop, y = value))+
  geom_boxplot(data = irrwatc_new.df %>% filter(value > 10),
  aes(x = crop, y = value),color = 'red',alpha = 0.5)+
  theme_bw()+ylab('MCM per day per Mha')+ylim(c(0,ymax))+
  ggtitle('Crop water withdraw factor before/after calibration')

hist_land_new.df = res.list[[1]]$new_hist_land %>% as.data.frame() %>% 
  rename(irr_Mha = value)

setwd(input_folder)
# tentative, cutting off peaks above 75 percentile

#cut below 75 percentile
irrwatc_new.df95 = irrwatc_new.df %>% group_by(scenario,model,crop,node,year) %>%
  summarise(max_value = max(value)) %>% ungroup() %>% group_by(scenario,model,crop,year) %>% 
  mutate(perc75 = as.numeric(quantile(max_value,.97)) ) %>%
  ungroup() %>%
  mutate(value75 = if_else(max_value > perc75, perc75, max_value)) %>%
  select(-perc75) %>% right_join(irrwatc_new.df) %>% 
  group_by(scenario,model,crop,node,year) %>%
  mutate(new_val = value / max_value * value75) %>% 
  select(-max_value,-value75,-value) %>% rename(value = new_val)

# Check that calibrated irrigation withdrawals make sense
before_calib.df = irrwat.df %>% mutate(time = as.character(time)) %>% # calibration coefficient
  filter( year == '2010' ) %>% # base year data
  left_join( ., hist_area.df ) %>% # historical area
  left_join( ., data.frame( time = as.character( 1:12 ), days = c( 31,28.25,31,30,31,30,31,31,30,31,30,31 ) ) ) %>%
  mutate( est = value * irr_Mha * days / 1e3 / (field_efficiency * flood_efficiency * water_course_efficiency)) %>%
  group_by( scenario, model,node ) %>% summarise( before =  sum(est) ) %>% 
  left_join( ., irr_cheema.df %>% dplyr::rename( node = PID ) %>% 
               mutate( tw = ( cw  * irrigation_canal_efficiency + gw *irrigation_gw_efficiency) * 
                               365 / 1e3 ) %>% select( node, tw ) ) %>% 
  as.data.frame()

post_calib.df = irrwatc_new.df %>% mutate(time = as.character(time)) %>% # calibration coefficient
  filter( year == '2010' ) %>% 
  left_join( ., hist_land_new.df ) %>% # historical area
  left_join( ., data.frame( time = as.character( 1:12 ), days = c( 31,28.25,31,30,31,30,31,31,30,31,30,31 ) ) ) %>%
  mutate(irr_Mha = if_else(is.na(irr_Mha), 0, irr_Mha)) %>% 
  mutate( est = value * irr_Mha * days / 1e3 / (field_efficiency * flood_efficiency * water_course_efficiency)) %>%
  group_by( scenario, model,node ) %>% summarise( post =  sum(est) ) %>% 
  left_join( ., irr_cheema.df %>% dplyr::rename( node = PID ) %>% 
               mutate( tw = ( cw * irrigation_canal_efficiency + gw *irrigation_gw_efficiency) * 
                               365 / 1e3 ) %>% select( node, tw ) ) %>% 
  as.data.frame()
		
before_post.df = before_calib.df %>% left_join(post_calib.df) %>% 
  mutate(post = if_else(is.na(post) , 0 , post)) %>% ungroup()

max = max(before_post.df$tw,before_post.df$before)
pdf( 'input/check/irrigation_calibration.pdf', width = 7, height = 6 ) 
ggplot(before_post.df # %>% gather(case,value,before:post), 
       )+
  geom_point(aes(x = tw, y = before ),size = 2,shape = 1)+
  geom_point(aes(x = tw, y = post ),color = 'red', size = 2)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(c(0,max))+ylim(c(0,max))+
  xlab('data')+ylab('model')+
  ggtitle('data vs model before/after calibration')+
  theme_bw()
dev.off()

summary_diff = before_post.df %>% 
  mutate(diff_be = (tw - before)/tw,
         diff_post = (tw - post)/tw ) %>% 
  filter(tw != 0) %>% 
  group_by(model,scenario) %>% 
  summarise(diff_be = mean(diff_be), diff_post = mean(diff_post) )

# Write to csv
write.csv( irrwatc_new.df, paste0( getwd(), '/input/crop_irrigation_water_calibrated.csv'), row.names = FALSE )
# write new historical area modified
hist_land_out.df = hist_land.gms %>% left_join(hist_land_new.df) %>% 
  mutate(irr_Mha = if_else(is.na(irr_Mha), 0 , irr_Mha)) %>% 
  mutate(value = irr_Mha) %>% ungroup() %>% 
  mutate(par = 'crop_irr_land_2015',
         unit = 'Mha',
         time = 'year') %>% 
  select(crop,par,node,time,unit,value)

existing_csv = read.csv( "input/crop_input_data.csv", stringsAsFactors=FALSE ) %>% 
  filter(par != 'crop_irr_land_2015')
out.df = rbind(existing_csv,hist_land_out.df)
write.csv( out.df, paste0( getwd(), '/input/crop_input_data.csv'), row.names = FALSE )

# for last time overwrite total available land, taking maximum between
# previous one and the total land per crop after the calibration
land_av_agriculture.df =read.csv(paste0(getwd(), '/input/land_availability_map.csv'))

crop_area2015 = read.csv(paste0(getwd(), '/input/crop_input_data.csv')) %>% 
  filter(unit == 'Mha')

# check after 2015 upscaling
check_area2 = crop_area2015 %>%  
  group_by(node,par) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  tidyr::spread(par,value) %>% 
  left_join(land_av_agriculture.df %>% select(-units) %>% 
              rename( tot_land_av = value)) %>% 
  mutate(diff = (tot_land_av - (crop_rainfed_land_2015 + crop_irr_land_2015)  )/tot_land_av *100 ) %>% 
  mutate(tot_crop_area = crop_rainfed_land_2015 + crop_irr_land_2015) %>% 
  select(node,diff,everything())

#write available land for agriculture, use tot available land 2000 when it is greater than the sum of rainfed+irr land in 2015
# otherwise use rainfed + irr land 2015
land_av_agriculture.df = check_area2 %>% 
  group_by(node) %>% 
  mutate(value = max(tot_land_av,tot_crop_area),
         units = 'Mha') %>% ungroup() %>% 
  select(node,value,units)

write.csv(land_av_agriculture.df, paste0(getwd(),'/input/land_availability_map.csv'), row.names = F  )

# Combine historical area with crop coefficients to estimate historical withdrawal
nds = as.character(unique(irrwatc_new.df$node))
irr.df = left_join( irrwatc_new.df %>% filter(year == 2010), hist_land_out.df %>% rename(irr_Mha = value) %>% select(crop,node,irr_Mha) ) %>% 
  mutate(time = as.character(time)) %>% 
	left_join( ., data.frame( time = as.character( 1:12 ), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
	left_join( ., data.frame( node = nds, country = unlist( strsplit( nds, '_' ) )[ seq( 1, 2*length(nds), by = 2 ) ] ) ) %>%
	mutate( irr_km3 = value * irr_Mha * days / 1e3 / (field_efficiency * flood_efficiency * water_course_efficiency) ) %>% # add other efficiencies? yes
	group_by( scenario, model, country, time, crop ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	as.data.frame() %>% data.frame() %>% filter(country != 'CHN') 
irr.df = rbind( irr.df, irr.df %>% 	
	group_by( scenario, model, time, country ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	as.data.frame() %>% data.frame() %>%
	mutate( crop = 'all' ) %>%
	select( scenario, model, country, time, crop, irr_km3 ) )	
irr.df = rbind( irr.df, irr.df %>% 	
	group_by( scenario, model, time, crop ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	as.data.frame() %>% data.frame() %>%
	mutate( country = 'Indus') %>%
	select( scenario, model, country, time, crop, irr_km3 ) ) %>% 
  mutate( time = as.numeric(time))
cols = data.frame( cotton = 'green', fodder = 'brown', pulses = 'blue', rice = 'red', sugarcane = 'gold', wheat = 'purple', maize = 'pink', fruit = 'orange', vegetables ='grey', all = 'black' )	
pdf( 'input/check/irrigation_withdrawal.pdf', width = 7, height = 6 ) 
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( irr.df$country ) ){
	df = irr.df %>% filter( country == ccc, model == 'ensemble', scenario == 'historical' ) %>%
		dplyr::select( crop, time, irr_km3 ) %>%
	  arrange(time) %>% 
		reshape( ., idvar = 'time', timevar = 'crop', direction = 'wide' ) %>%
		select( -time ) %>% as.matrix()
	matplot( 	df, type = 'l', 
				ylab = 'cubic kilometers', 
				xlab = 'month of year', 
				main = paste0( ccc, ' - Withdrawal' ), 
				lty = 1, 
				col = as.character( unlist( cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ] ) ) )
	}
plot.new() 
legend( 'left', legend = unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ], title = 'Crop', bty = 'n', cex = 1.2, seg.len = 3, col = as.character(unlist(cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ])), lty = 1 )	
dev.off()
pdf( 'input/check/irrigation_withdrawal_2.pdf', width = 7.5, height = 4 ) 
p1 = layout( matrix( c(1,2,3,4), 1,4, byrow=TRUE ), widths=c(0.3,0.3,0.3,0.2), heights=c(0.8) )
par(mar=c(4,4,2,2), oma = c(1,1,1,1))
for( ccc in unique( irr.df$country )[c(2,3,4)] ){
	df = irr.df %>% filter( country == ccc, model == 'ensemble', scenario == 'historical' ) %>%
		select( crop, time, irr_km3 ) %>%
		reshape( ., idvar = 'time', timevar = 'crop', direction = 'wide' ) %>%
		select( -time ) %>% as.matrix()
	matplot( 	df, type = 'l', 
				ylab = 'cubic kilometers', 
				xlab = 'month of year', 
				main = paste0( ccc, '' ), 
				lty = 1, 
				col = as.character(unlist(cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ])) )
	}
par(mar=c(4,0,3,0))
plot.new() 
legend( 'left', legend = unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ], title = '', bty = 'n', cex = 1, y.intersp = 1.5, seg.len = 3, col = as.character(unlist(cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ])), lty = 1 )	
dev.off()		

# create historical activity from irrigation withdrawals, and sw/gw shares from cheema
sw_gw_ratio = irr_cheema.df %>% mutate(cw = cw * irrigation_canal_efficiency ,
                                       gw = gw *irrigation_gw_efficiency,
                                       tw = cw + gw,
                                       ratio_sw = cw/tw)
m =mean(sw_gw_ratio$ratio_sw[!is.na(sw_gw_ratio$ratio_sw)])
sw_gw_ratio = sw_gw_ratio %>% mutate(ratio_sw = if_else(is.na(ratio_sw), m,ratio_sw )) %>% 
  rename(node= PID) %>% 
  select(node,ratio_sw)

irr_by_node.df1 = left_join( irrwatc_new.df %>% filter(year == 2010), hist_land_out.df %>% rename(irr_Mha = value) %>% select(crop,node,irr_Mha) ) %>% 
  mutate(time = as.character(time)) %>% 
  left_join( ., data.frame( time = as.character( 1:12 ), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
  left_join( ., data.frame( node = nds, country = unlist( strsplit( nds, '_' ) )[ seq( 1, 2*length(nds), by = 2 ) ] ) ) %>%
  mutate( tw_out1 = value * irr_Mha  / (field_efficiency * flood_efficiency * water_course_efficiency) ) %>% # add other efficiencies? yes
  left_join(sw_gw_ratio) %>% 
  mutate(cw_out = tw_out1 * ratio_sw /  irrigation_canal_efficiency ,
         gw_out = tw_out1 * ( 1- ratio_sw) / irrigation_gw_efficiency )

irr_act_by_node.df = irr_by_node.df1 %>% #sum over crops
  group_by(scenario,model,node,time) %>% summarise(irrigation_sw_diversion = sum(cw_out),
                                              irrigation_gw_diversion = sum(gw_out)) %>% ungroup() %>% 
  gather(tec,value,irrigation_sw_diversion:irrigation_gw_diversion)

# write to gdx, can be used to set historical activity of irrigation diversion  
write.csv( irr_act_by_node.df, 'input/historical_irrigation_withdrawals_act.csv', row.names = FALSE )

irr_cap_by_node.df = irr_by_node.df1 %>% filter(year == 2010) %>% 
  group_by(scenario,model,node) %>% 
  summarise(cw_out = (sum(cw_out * days) / 365), # in MCM_day_Mha
            gw_out = (sum(gw_out *days) / 365) ) %>% ungroup()
# very similar to cheema, this should overwrite historical capcity for irrigation_sw_diversiona nd irrigation_gw_diversion
# Add historical capacity of pumping systems in irrigation
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
ind = which( historical_capacity.df1$tec %in% c( 'irrigation_sw_diversion', 'irrigation_gw_diversion' ) ) 
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] } # remove any existing entries 
historical_capacity.df1 = historical_capacity.df1 %>%
  filter( ! tec %in% c( 'irrigation_sw_diversion', 'irrigation_gw_diversion' ) ) %>%
  rbind( ., irr_cap_by_node.df %>% filter(model == 'ensemble',scenario == 'historical') %>% 
           dplyr::select( node, cw_out ) %>% 
           dplyr::rename(value = cw_out ) %>% 
           mutate( tec = 'irrigation_sw_diversion', year_all = 2015, units = 'mcm_per_day', value = round( value, digits = 3 ) ) %>%
           dplyr::select( names( historical_capacity.df1 ) ) ) %>%
  rbind( ., irr_cap_by_node.df %>% filter(model == 'ensemble',scenario == 'historical') %>%
           dplyr::select( node, gw_out ) %>% 
           dplyr::rename(value = gw_out ) %>% 
           mutate( tec = 'irrigation_gw_diversion', year_all = 2015, units = 'mcm_per_day', value = round( value, digits = 3 ) ) %>%
           dplyr::select( names( historical_capacity.df1 ) ) )

# Write to csv for importing into messageix later	
write.csv( 	historical_capacity.df1, 
            "input/historical_new_cap.csv", 
            row.names = FALSE )

# Output historical withdrawals (annual) to csv for plotting/accounting
historical_withdrawals.df = irr_cap_by_node.df %>% filter(model == 'ensemble',scenario == 'historical' ) %>% 
  mutate(value = cw_out + gw_out) %>% select(node,value) %>% as.data.frame() %>% 
  mutate( sector = 'irrigation', units = 'mcm_per_day', year = 2015, value = round( value, digits = 2 ) ) %>%
  dplyr::select( node, sector, year, value, units ) %>%
  rbind( read.csv( "input/indus_demands_new.csv", stringsAsFactors = FALSE ) %>% filter( sector != 'irrigation' ) %>%
           filter( type == 'withdrawal', scenario == 'SSP2', year == 2015 ) %>%
           left_join( ., data.frame( month = 1:12, days = c( 31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>% 
           group_by( sector, pid ) %>%
           summarise( value = sum( value * days ) / 365 ) %>%
           as.data.frame( ) %>%
           dplyr::rename( node = pid ) %>%
           mutate( units = 'mcm_per_day', year = 2015 ) %>%
           dplyr::select( node, sector, year, value, units ), 
         . ) %>%
  mutate( value = round( value, digits = 3 ) )

write.csv( historical_withdrawals.df, 'input/historical_withdrawals.csv', row.names = FALSE )

## Maximum area for each crop	
# 
# # Average across PIDs 
# max_area.df = bind_rows( lapply( 1:length(crop_names), function( ii ){ 
# 	
# 	# Get the irrigated and rainfed yield for this specific crop
# 	crop_folder = list.dirs(path = yeld_path)
# 	crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
# 	crop_file = list.files(path = crop_folder,pattern = '.tif')
# 	irr_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
# 	crop_folder = list.dirs(path = yeld_path)
# 	crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
# 	crop_file = list.files(path = crop_folder,pattern = '.tif')
# 	rain_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
# 	
# 	# Calculate area where yield exists
# 	msk1 = irr_yield
# 	msk1[ msk1[] > 0 ] = 1
# 	msk1[ msk1[] != 1 ] = NA
# 	msk1 = area( msk1 ) * msk1
# 	
# 	msk2 = rain_yield
# 	msk2[ msk2[] > 0 ] = 1
# 	msk2[ msk2[] != 1 ] = NA
# 	msk2 = area( msk2 ) * msk2
# 	
# 	# Get the area in each PID
# 	lst1 = raster::extract( msk1, basin.spdf, byid = TRUE )
# 	lst2 = raster::extract( msk2, basin.spdf, byid = TRUE )
# 	
# 	df = bind_rows( lapply( 1:length( basin.spdf ), function( jjj ){
# 		
# 		data.frame( crop = crop_names[ ii ],
# 					node = basin.spdf@data$PID[ jjj ],
# 					rain_max_Mha = round( 1e-4 * sum( lst2[[ jjj ]], na.rm = TRUE ), digits = 4 ),
# 					irr_max_Mha = round( 1e-4 * sum( lst1[[ jjj ]], na.rm = TRUE ), digits = 4 ) )
# 	
# 		} ) )
# 	
# 	return( df )
# 	
# 	} ) )
# 
# # Max area in each PID across all crops
# 
# pot_area.raster = sum( do.call( stack, lapply( 1:length(crop_names), function( ii ){ 
# 	
# 	# Get the irrigated and rainfed yield for this specific crop
# 	crop_folder = list.dirs(path = yeld_path)
# 	crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
# 	crop_file = list.files(path = crop_folder,pattern = '.tif')
# 	irr_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
# 	crop_folder = list.dirs(path = yeld_path)
# 	crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
# 	crop_file = list.files(path = crop_folder,pattern = '.tif')
# 	rain_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
# 	
# 	# Calculate area where yield exists
# 	msk1 = irr_yield
# 	msk1[ msk1[] > 0 ] = 1
# 	msk1[ msk1[] != 1 ] = 0
# 	
# 	
# 	msk2 = rain_yield
# 	msk2[ msk2[] > 0 ] = 1
# 	msk2[ msk2[] != 1 ] = 0
# 	
# 	return( msk1 + msk2 )
# 	
# 	} ) ) )
# 	
# pot_area.raster[ pot_area.raster[] > 0 ] = 1
# pot_area.raster[ pot_area.raster[] != 1 ] = NA	
# pot_area.raster = area( pot_area.raster ) * pot_area.raster
# 
# lst = raster::extract( pot_area.raster, basin.spdf, byid = TRUE )
# 	
# max_tot.df = bind_rows( lapply( 1:length( basin.spdf ), function( jjj ){
# 		
# 	data.frame( crop = 'all',
# 				node = basin.spdf@data$PID[ jjj ],
# 				rain_max_Mha = round( 1e-4 * sum( lst[[ jjj ]], na.rm = TRUE ), digits = 4 ),
# 				irr_max_Mha = round( 1e-4 * sum( lst[[ jjj ]], na.rm = TRUE ), digits = 4 ) )
# 
# 	} ) )
# 
# max_area.df = rbind( max_area.df, max_tot.df )
# 
# write.csv( max_area.df, paste0( getwd(), '/input/max_crop_area.csv'), row.names = F)
# 
# after calibrating water and land use, demand trends and crop yields
# also need to be consistent to the same land use projections

# crop related data: costs, yields, initial capacity source GAEZ, some is in Mha
crop_input_data.df = read.csv( 'input/crop_input_data.csv', stringsAsFactors=FALSE ) 
crop_names = unique(crop_input_data.df$crop)

crop_coef = read.csv( 'input/crop_tech_data.csv' ) %>% 
  filter(par == 'crop_coef') %>% 
  rename(kc = value) %>% 
  select(crop,time,kc)

## irrigated Crop Yields per node, month kton/Mha
monthly_yields = crop_input_data.df %>% 
  filter(par %in% c('irrigation_yield','rain-fed_yield')) %>% 
  select(-time) %>% 
  crossing(time = as.character(seq(1,12,1)) ) %>% 
  left_join(crop_coef) %>% 
  mutate(value = value * kc,
         par = gsub('_yield','',par)) %>% 
  select(crop,par,node,time,value)

# actuall irrigated land Mha

total_agri_land = crop_input_data.df %>%
  filter(par %in% c('crop_irr_land_2015','crop_rainfed_land_2015') ) %>% 
  mutate(par = if_else(grepl('irr',par), 'irrigation', 'rain-fed')) %>% 
  rename(area = value) %>% 
  select(-time,-unit)

# area by yield, make this match annual demand. in kton year
calculated_prod = 
  total_agri_land %>% left_join(monthly_yields %>% rename(yield = value)) %>% 
  mutate(value = area * yield) %>% 
  filter(!is.na(value)) %>% 
  group_by(crop,node) %>% 
  summarise(calc_val = sum(value)) %>% ungroup()

# food  demand, assumption as multiple of historical yield
# we have historical demand of 2000 (GAEZ) projected with FAO national production levels and SSP population growth
### MANUAL TWICKING OF DEMAND AFTER COMPARING WITH FAO NATIONAL PRODUCTION ###
SSP = 'SSP2'
yield_demand.df_match = read.csv( "input/indus_demands_new.csv", stringsAsFactors=FALSE ) %>% 
  filter( scenario == SSP , sector == 'crop') %>%
  mutate(D_dem = if_else(grepl('AFG',pid) & type == c('wheat'), 0.1, 
                         if_else( grepl('PAK',pid) & type %in% c('wheat','rice'), 0.1 , 
                                  if_else( grepl('IND',pid) & type %in% c('wheat','rice'), 0.2 , 0 ) ) ) ) %>% 
  mutate(value = value * (1 + D_dem)) %>% 
  select(scenario,sector,type,pid,year,month,value,units )

existing_csv = read.csv( "input/indus_demands_new.csv", stringsAsFactors=FALSE ) %>% 
  filter( scenario == SSP , sector != 'crop') 
to_csv = rbind(existing_csv,yield_demand.df_match)
write.csv( to_csv, paste0( getwd(), '/input/indus_demands_new.csv'), row.names = F)

# kton
node_food_demand.df = yield_demand.df_match %>% 
  filter(year == 2020) %>% 
  rename(crop = type, node = pid) %>% 
  rename(Fdemand = value) %>% 
  select(crop,node,Fdemand)

# at the end need to check that we have 0 land where there is not food demand
prod_comparison = calculated_prod %>% left_join(node_food_demand.df) %>% 
  mutate(delta = (Fdemand - calc_val)/Fdemand ) %>% 
  filter(!is.na(Fdemand))
  # mutate(to_remove =if_else(is.na(delta),T, F)) #for checking
ggplot(prod_comparison)+
  geom_point(aes(x = node,y = Fdemand))+
  geom_point(aes(x = node,y = calc_val),color = 'red')+
  facet_wrap(~crop)+ggtitle('before calibration')

# steps> take perc factor 
# keep only factor < + - 50%
perc_change = prod_comparison %>% select(crop,node,delta) %>% 
  filter(!is.na(delta)) %>% 
  mutate(delta = if_else(delta > 0.5, 0.5,
         if_else(delta < -0.5, -0.5, delta/2)) )

# and apply it to rainfed land and irr all yields
new_area_yield = 
  total_agri_land %>% left_join(monthly_yields %>% rename(yield = value)) %>% 
  left_join(perc_change) %>% 
  mutate(area = if_else(par == 'irrigation', area, area * (1 + delta) )) %>% 
  mutate(yield = if_else(par == 'irrigation',yield * (1+delta/2), yield * (1+delta)) ) %>% 
  mutate(value = area * yield) %>% 
  filter(!is.na(value))
  
calculated_prod2 = new_area_yield %>%  
  group_by(crop,node) %>% 
  summarise(calc_val2 = sum(value)) %>% ungroup()

# re- assess the cal_val of production
prod_comparison2= prod_comparison %>% 
 left_join(calculated_prod2) %>% 
  mutate(delta2 = (Fdemand - calc_val2)/Fdemand )

ggplot(prod_comparison2)+
  geom_point(aes(x = node,y = Fdemand))+
  geom_point(aes(x = node,y = calc_val2),color = 'red')+
  facet_wrap(~crop)+ ggtitle('first iteration')

# make sure it-s better and perc has not changed sign
perc_change2 = perc_change %>% left_join(prod_comparison2 %>% select(crop,node,delta2)) %>% 
  mutate(mult = delta * delta2,
         diff = round(delta-delta2,3)) %>% 
  mutate(delta2 = if_else(delta2 > 0.5, 0.5,
                         if_else(delta2 < -0.5, -0.5, delta2/2)) )

# repeat many times
##### LOOP ####
for (i in c(1,2,3,4,5)) {
new_area_yield = new_area_yield %>% select(crop,par,node,area,time,yield,value) %>% # remove delta or delta2
  left_join(perc_change2 %>% select(crop,node,delta2)) %>% 
  mutate(area = if_else(par == 'irrigation', area, area * (1 + delta2) )) %>% 
  mutate(yield = if_else(par == 'irrigation',yield * (1+delta2/2), yield * (1+delta2)) ) %>% 
  mutate(value = area * yield) %>% 
  filter(!is.na(value))

calculated_prod2 = new_area_yield %>%   
  group_by(crop,node) %>% 
  summarise(calc_val2 = sum(value)) %>% ungroup()

# re- assess the cal_val of production
prod_comparison2= prod_comparison %>% 
  left_join(calculated_prod2) %>% 
  mutate(delta2 = (Fdemand - calc_val2)/Fdemand )

ggplot(prod_comparison2)+
  geom_point(aes(x = node,y = Fdemand))+
  geom_point(aes(x = node,y = calc_val2),color = 'red')+
  facet_wrap(~crop)+ggtitle(paste0('iteration n ',i+1))

# make sure it-s better and perc has not changed sign
perc_change2 = perc_change2 %>% 
  select(crop,node,delta2) %>% rename(delta_old = delta2) %>% 
  left_join(prod_comparison2 %>% select(crop,node,delta2)) %>% 
  mutate(mult = delta_old * delta2,
         diff = round(delta_old-delta2,3)) %>% 
  mutate(delta2 = if_else(delta2 > 0.5, 0.5,
                          if_else(delta2 < -0.5, -0.5, delta2/2)) )
}
#### END LOOP ###
# significant improvement
# now check and save back irr and rainfed land use and yield

new_yield.df = new_area_yield %>% select(crop,par,node,time,yield) %>% 
  group_by(crop,par,node) %>% 
  filter(yield == max(yield)) %>% 
  filter(time == max(time)) %>% ungroup() %>%  
  left_join(crop_coef %>% group_by(crop) %>% filter(kc == max(kc)) %>% ungroup()) %>% 
  mutate(new_yield = yield / kc) %>% select(crop,par,node,new_yield) %>% 
  mutate(par = paste0(par,'_yield')) %>% 
  right_join(crop_input_data.df %>% 
  filter(par %in% c('irrigation_yield','rain-fed_yield')) )

# reduction ratio
rr = new_yield.df %>% filter(!is.na(new_yield)) %>%
  mutate(ratio = new_yield / value) %>% 
  group_by(par) %>% 
  summarise(avg = mean(ratio)) %>% ungroup()

new_yield.df = new_yield.df %>% left_join(rr) %>% 
  mutate(new_yield = if_else(is.na(new_yield),value * avg, new_yield))

new_yield.df %>% filter(value > 0) %>%
  ggplot()+
  geom_boxplot(aes(x = crop, y = value))+
  geom_boxplot(aes(x = crop, y = new_yield),color = 'red')+
  facet_wrap(~par)+
  theme_bw()+ylab('kton/Mha')+
  ggtitle('Crop yields')

new_yield.out = new_yield.df %>% select(-value) %>% rename(value = new_yield) %>% 
  mutate(time = 'year') %>% 
  select(crop,par,node,time,unit,value)

existing_csv = read.csv(paste0(getwd(), '/input/crop_input_data.csv')) %>% 
  filter(unit != 'kton DW/ Mha')
to_csv = bind_rows(existing_csv,new_yield.out)
write.csv(to_csv, paste0(getwd(), '/input/crop_input_data.csv'), row.names = F)

# Similar for land
new_land.df = new_area_yield %>% select(crop,par,node,area) %>% 
  distinct() %>% 
  mutate(par = if_else(grepl('irr',par), 'crop_irr_land_2015', 'crop_rainfed_land_2015')) %>%
  right_join(crop_input_data.df %>% 
  filter(par %in% c('crop_irr_land_2015','crop_rainfed_land_2015') ) )

  
new_land.df %>% filter(value > 0) %>%
  ggplot()+
  geom_boxplot(aes(x = crop, y = value))+
  geom_boxplot(aes(x = crop, y = area),color = 'red')+
  facet_wrap(~par)+
  theme_bw()+ylab('Mha')+
  ggtitle('Crop land')

new_land.out = new_land.df %>% 
  select(-value) %>% mutate(value = if_else(is.na(area),0 , area)) %>% 
  select(crop,par,node,time,unit,value)

existing_csv = read.csv( "input/crop_input_data.csv", stringsAsFactors=FALSE ) %>% 
  filter(unit != 'Mha')
out.df = rbind(existing_csv,new_land.out)
write.csv( out.df, paste0( getwd(), '/input/crop_input_data.csv'), row.names = FALSE )

# # remove yield peaks, take avg
# # the model tends to go to the points of max yields, which overestimate the national yield capacity
# 
# yield_data = crop_input_data.df %>% 
#   filter(par %in% c('irrigation_yield','rain-fed_yield'))
# 
# yield_data %>% filter(value > 0) %>% 
#   ggplot()+
#   geom_boxplot(aes(x = crop, y = value))+
#   facet_wrap(~par)+
#   theme_bw()+ylab('MCM per day per Mha')+
#   ggtitle('Crop yields')
# 
# #cut below 60 percentile
# yield_data_cut = yield_data %>% group_by(par,crop) %>% 
#   mutate(perc6 = as.numeric(quantile(value,.6)) ) %>% 
#   ungroup() %>% 
#   mutate(value = if_else(value > perc6, perc6, value)) %>% 
#   select(-perc6)
# 
# yield_data_cut %>% filter(value > 0) %>% 
#   ggplot()+
#   geom_boxplot(aes(x = crop, y = value))+
#   facet_wrap(~par)+
#   theme_bw()+ylab('MCM per day per Mha')+
#   ggtitle('Crop yields')
# 
# # possible yield variation to match with land data
# # we filter only cases that are consistend with unmatch historical data
# 
# delta_yield = min_land_use %>% 
#   mutate(D_yield = (Fdemand / (land2015 * yield ) ) - 1) %>% 
#   mutate(D_yield = if_else(D_yield > 0, 0, 
#                            if_else(D_yield < -0.3, -0.3, D_yield))) %>% 
#   select(country,crop,D_yield)
# 
# crop_yields_calib.df_cut = yield_data_cut %>% 
#   mutate(country = gsub('_.*','',node) ) %>% 
#   left_join(delta_yield) %>% 
#   mutate(D_yield = if_else(is.na(D_yield), 0, D_yield  )) %>% 
#   mutate(value = value * ( 1 + D_yield)) %>% 
#   select(names(crop_input_data.df))
# 
# existing_csv = read.csv( 'input/crop_input_data.csv', stringsAsFactors=FALSE ) %>% 
#   filter(!par %in% c('irrigation_yield','rain-fed_yield'))
# 
# to_csv = rbind(existing_csv,crop_yields_calib.df_cut)
# write.csv( to_csv, paste0( getwd(), '/input/crop_input_data.csv'), row.names = F)
