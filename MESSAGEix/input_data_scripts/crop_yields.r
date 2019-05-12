
# source('C:/Users/parkinso/indus_ix/input_data_scripts/crop_yields.r')

# aggregating crop yelds value in basin catchments
require(rgeos)
require(rgdal)
require(raster)
require(dplyr)

# Location of input data
setwd( 'P:/is-wel/indus/message_indus' )

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")

# Basin analyzed
basin = 'Indus'

#basin shape file
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )

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
ind = getData( 'GADM', country = 'IND', level = 0 )[,'ISO']
IRR_cw1.pts$PID[ which( over( IRR_cw1.pts, ind )[,'ISO'] == 'IND' ) ] =  'IND_4'

# Convert to polygons
IRR_cw1.pts = merge( IRR_cw1.pts, data.frame( PID = unique( IRR_cw1.pts$PID ), ID = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.raster = rasterize( IRR_cw1.pts, IRR_cw1.raster, field = 'ID' )
IRR_pid.spdf = rasterToPolygons( IRR_pid.raster, dissolve = TRUE )
IRR_pid.spdf@data = left_join( IRR_pid.spdf@data, data.frame( PID = unique( IRR_cw1.pts$PID ), layer = 1:length( unique( IRR_cw1.pts$PID ) ) ) )
IRR_pid.spdf = IRR_pid.spdf[,'PID']

# Bind and dissolve with original basin polygons
basin_irr.spdf = aggregate( rbind( basin.spdf[,'PID'], IRR_pid.spdf ) , 'PID' )
basin_irr.spdf@data = left_join( basin_irr.spdf@data, basin.spdf@data ) 

# Use version with irrigated areas to calibrate the crop implementation
basin0.spdf = basin.spdf
basin.spdf = basin_irr.spdf

# Get data from GAEZ
# path with yield data, from GAEZ
yeld_path = paste0(getwd(),'/input/land_maps_crop_yields/YIELD')

# Crops to inlcude - must match the technology file
crop_names = c('wheat','rice','cotton','fodder','sugarcane','pulses')

# Get data for each crop
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

# same thing for rain-fed crops

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

to_csv = bind_rows(irr_yield_out, rain_yield_out)
write.csv(to_csv, paste0(getwd(), '/input/crop_input_data.csv'), row.names = F)

# Crop production data in 2000, summing irrigated and rainfed crops 
# data from GAEZ, original unit: 1000t
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
    select(Area,Item,Unit,mult)
  # we want only mult at the end
  inc_rate2000 = bind_rows(inc_rate2000,tmp_national)
}

crop_item_map = data.frame(Item = unique(inc_rate2000$Item),
                           crop = c('cotton', 'pulses', 'rice','sugarcane','wheat','fodder' ) )
country_map = data.frame(Area = unique(inc_rate2000$Area),
                         node2 = c('AFG', 'IND', 'PAK') )

inc_rate2000 = inc_rate2000 %>% left_join(crop_item_map) %>% 
  left_join(country_map) %>% ungroup() %>% 
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

existing_csv = read.csv(paste0(getwd(), '/input/indus_demands.csv'))
#remove existing crop demand, if any
to_csv = bind_rows(existing_csv %>% filter(sector != 'crop'), prod_out)
write.csv(to_csv, paste0(getwd(), '/input/indus_demands.csv'), row.names = F )

## Define irrigation water input per unit area using data from CWaTM

# Get scenario and model names
fls = list.files( 'input/hydroclimate_input', pattern = '.tif' )
climate_models = unique( unlist( strsplit( fls, '_' ) )[ seq( 3, 4*length(fls), by=4 ) ] )
climate_scenarios = unique( unlist( strsplit( fls, '_' ) )[ seq( 4, 4*length(fls), by=4 ) ] )
climate_scenarios = unlist( strsplit( climate_scenarios, '[.]' ) )[ seq( 1, 2*length( climate_scenarios ), by = 2 ) ]

# Create data frame with the irrigation water input data for each model-scenario combination
irrwat.df = bind_rows( lapply( climate_scenarios, function( climate_scenario ){ 
	
	bind_rows( lapply( climate_models, function( climate_model ){ 
		
		if( file.exists( paste0( 'input/hydroclimate_input/etref_metersperday_',climate_model,'_', climate_scenario, '.tif' ) ) )
			{
			
			ETref.stack = stack( paste0( 'input/hydroclimate_input/etref_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(ETref.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			Peff10.stack = stack( paste0( 'input/hydroclimate_input/effprecip10day_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(Peff10.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			Peff3.stack = stack( paste0( 'input/hydroclimate_input/effprecip3day_metersperday_',climate_model,'_', climate_scenario, '.tif' ) )
			names(Peff10.stack) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
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
								value = 1e4 * sapply( 1:ncol( lst[[ x ]] ), function( jjj ){ mean( lst[[ x ]][ , jjj ], na.rm = TRUE ) } ) ) # 1e4 is conversion from meters per day
					} ) )
				df$value[ is.nan( df$value ) ] = 0
				
				return( df )
				
				} ) )
			
			}		
			
		} ) )
		
	} ) )

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
	ungroup( ) %>%
	data.frame( )
	
# Combine withdrawals occurring within and outside basin	
IRR_cw.df = rbind( IRR_cw0.df, IRR_cw1.df ) %>% 
				group_by( PID ) %>% summarise( val = sum( val ) ) %>% ungroup() %>% data.frame()	

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
	
# Historical area of each crop	
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
hist_area.df = bind_rows( lapply( 1:length(crop_names), function( ii ){ 
	
	# Get the irrigated and rainfed yield for this specific crop
	crop_folder = list.dirs( path = 'P:\\is-wel\\indus\\message_indus\\input\\land_maps_crop_yields' )
	crop_folder = crop_folder[ grepl( crop_names[ii], crop_folder ) & grepl( 'act2000', crop_folder ) & grepl( '2000har', crop_folder ) ]
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

# Add historical area to historical_new_capacity 
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 

# remove previous if existing
nms = paste0( 'crop_', crop_names )
ind = which( as.character( historical_capacity.df1$tec ) %in% nms & as.character( historical_capacity.df1$year_all ) == '2015' )
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] }

# Add the historical area of each crop
historical_capacity.df1 = rbind( historical_capacity.df1, 
	hist_area.df %>% 
		mutate( crop = paste0( 'crop_', crop ) ) %>% 
		rename( tec = crop, value = irr_Mha ) %>%
		mutate( year_all = 2015, units = 'Mha' ) %>%
		select( node, tec, year_all, value, units )
	)

# Write to csv for importing into messageix later	
write.csv( 	historical_capacity.df1, 
			"input/historical_new_cap.csv", 
			row.names = FALSE )	
	
# Stack maps for plotting if you want to check
hist_area.raster = do.call( stack, lapply( crop_names, function( iii ){ raster( paste0( 'input/crop_area_map_', iii, '.asc' ) ) } ) )	
	
# Define average efficiency for water once it reaches the farm-gate 
# Assuming all irrigation is basically flood irrigation based on disucssions with stakeholders
# Also aligns closely with asssumptions in Wu et al. (2013, World Bank) figure 5.4 
field_efficiency = 0.85
water_course_efficiency = 0.55
irrigation_canal_efficiency = 0.75
irrigation_gw_efficiency = 0.95
	
# Calibrated parameters for each climate / model scenario	
# Scale irrigation coefficients to match reported irrigation withdrawal
irrwatc.df = irrwat.df %>% # calibration coefficient
	filter( year == '2010' ) %>% # base year data
	left_join( ., hist_area.df ) %>% # historical area
	left_join( ., data.frame( time = as.character(seq(1,12)), days = c( 31,28,31,30,31,30,31,31,30,31,30,31 ) ) ) %>%
	left_join( ., irr_cheema.df %>% rename( node = PID ) ) %>% # historical withdrawals
	filter( cw > 0 | gw > 0 ) %>% # just areas with reported data
	mutate( est = value * irr_Mha * days / 1e3 ) %>% # total estimated km3  in each month
	filter( est > 0 ) %>% # only keep positive values
	group_by( scenario, model, node ) %>% # group by scenario and model and node
	summarise( est = sum( est ) ) %>% ungroup() %>% data.frame() %>% # sum the monthly volumes
	left_join( ., irr_cheema.df %>% rename( node = PID ) %>% mutate( tw = ( cw * water_course_efficiency + gw ) * field_efficiency * 365 / 1e3 ) %>% select( node, tw ) ) %>% # historical withdrawal applied to field
	mutate( delta = tw / est - 1 ) %>% # Calculate difference
	mutate( final = est * ( 1 + delta ) ) %>% # check final
	rename( estimated = est, value = final ) %>%
	filter( abs(delta) <= 1 )
		
# Summary plot for calibration
pdf( 'input/check/irrigation_calibration.pdf', width = 12, height = 6 )
p1 = layout( matrix( c(1,2), 1,2, byrow=TRUE ), widths=c(0.5,0.5), heights=c(0.9) )
par(mar=c(4,4,4,4), oma = c(2,2,2,2))
cols = irrwatc.df %>% select( scenario ) %>% left_join( ., data.frame( scenario = unique( irrwatc.df$scenario ), cols = c( 'red', 'blue', 'green' ) ) ) %>% select( cols ) %>% unlist()	
pchs = irrwatc.df %>% select( scenario ) %>% left_join( ., data.frame( scenario = unique( irrwatc.df$scenario ), cols = c( 21,22,23 ) ) ) %>% select( cols ) %>% unlist()	
plot( c(0, max( c( irrwatc.df$est, irrwatc.df$tw ) ) ), c(0, max( c( irrwatc.df$est, irrwatc.df$tw ) ) ), xlim = c(0, ceiling( max( c( irrwatc.df$est, irrwatc.df$tw ) ) ) ), ylim = c(0, ceiling( max( c( irrwatc.df$est, irrwatc.df$tw ) ) ) ), type = 'l', xlab = 'data - cubic kilometers', ylab = 'model - cubic kilometers', , main = 'a. Model-data comparison' )
points( irrwatc.df$tw, irrwatc.df$est, col = as.character( cols ), pch = pchs )
legend( 'topleft', legend = unique( irrwatc.df$scenario ), pch = c(21,22,23), col = c('red','blue','green'), bty='n' )
plot( irrwatc.df$tw, round( abs( 100*irrwatc.df$delta ), digits = 1 ), xlim = c(0, ceiling( max( c( irrwatc.df$est, irrwatc.df$tw ) ) ) ), col = as.character( cols ), pch = pchs, log = 'y', xlab = 'data - cubic kilometers', ylab = 'percent - logscale', main = 'b. Model-data absolute difference', yaxt = 'n' )
axis(2,c(0.01,0.01,0.1,1,10,100))
dev.off()		
	
# calibrate the coefficients	
mean_delta = weighted.mean(irrwatc.df$delta,irrwatc.df$tw,na.rm=TRUE) # for locations without observed data
irrwatc.df = left_join( irrwat.df , irrwatc.df %>% select( scenario, model, node, delta ) ) %>%
	mutate( delta = ifelse( is.na( delta ), mean_delta, delta ) ) %>% 
	rename( estimated = value ) %>%
	mutate( value = estimated * ( 1 + delta ) )
	
# Write to csv
write.csv( irrwatc.df, paste0( getwd(), '/input/crop_irrigation_water_calibrated.csv'), row.names = FALSE )

# Combine historical area with crop coefficients to estimate historical withdrawal
nds = as.character(unique(irrwatc.df$node))
irr.df = left_join( irrwatc.df %>% filter( year == 2010), hist_area.df ) %>% 
	left_join( ., data.frame( time = as.character(seq(1:12)), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
	left_join( ., data.frame( node = nds, country = unlist( strsplit( nds, '_' ) )[ seq( 1, 2*length(nds), by = 2 ) ] ) ) %>%
	mutate( irr_km3 = value * irr_Mha * days / 1e3 / field_efficiency ) %>% 
	group_by( scenario, model, country, time, crop ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	ungroup() %>% data.frame() 
irr.df = rbind( irr.df, irr.df %>% 	
	group_by( scenario, model, time, country ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	ungroup() %>% data.frame() %>%
	mutate( crop = 'all' ) %>%
	select( scenario, model, country, time, crop, irr_km3 ) )	
irr.df = rbind( irr.df, irr.df %>% 	
	group_by( scenario, model, time, crop ) %>% summarise( irr_km3 = sum( irr_km3 ) ) %>%
	ungroup() %>% data.frame() %>%
	mutate( country = 'Indus' ) %>%
	select( scenario, model, country, time, crop, irr_km3 ) )
	
irr.df  %>% filter( crop =='all' ) %>% group_by( scenario, model, country ) %>% summarise( irr_km3 = sum(irr_km3) ) %>% ungroup() %>% data.frame()	
	
cols = data.frame( cotton = 'green', fodder = 'brown', pulses = 'blue', rice = 'red', sugarcane = 'gold', wheat = 'purple', all = 'black' )	
pdf( 'input/check/irrigation_withdrawal.pdf', width = 7, height = 6 ) 
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( irr.df$country ) ){
	df = irr.df %>% filter( country == ccc, model == 'ensemble', scenario == 'historical' ) %>%
		select( crop, time, irr_km3 ) %>%
		reshape( ., idvar = 'time', timevar = 'crop', direction = 'wide' ) %>%
		select( -time ) %>% as.matrix()
	matplot( df, type = 'l', ylab = 'cubic kilometers', xlab = 'month of year', main = paste0( ccc, ' - Withdrawal' ), lty = 1, col = as.character(unlist(cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ])) )
	}
plot.new() 
legend( 'left', legend = unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ], title = 'Crop', bty = 'n', cex = 1.2, seg.len = 3, col = as.character(unlist(cols[ unlist( strsplit( colnames(df), '[.]' ) )[ seq(2,2*ncol(df),by=2) ] ])), lty = 1 )	
dev.off()		
	
## Maximum area for each crop	

# Average across PIDs 
max_area.df = bind_rows( lapply( 1:length(crop_names), function( ii ){ 
	
	# Get the irrigated and rainfed yield for this specific crop
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')
	irr_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')
	rain_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
	
	# Calculate area where yield exists
	msk1 = irr_yield
	msk1[ msk1[] > 0 ] = 1
	msk1[ msk1[] != 1 ] = NA
	msk1 = area( msk1 ) * msk1
	
	msk2 = rain_yield
	msk2[ msk2[] > 0 ] = 1
	msk2[ msk2[] != 1 ] = NA
	msk2 = area( msk2 ) * msk2
	
	# Get the area in each PID
	lst1 = raster::extract( msk1, basin.spdf, byid = TRUE )
	lst2 = raster::extract( msk2, basin.spdf, byid = TRUE )
	
	df = bind_rows( lapply( 1:length( basin.spdf ), function( jjj ){
		
		data.frame( crop = crop_names[ ii ],
					node = basin.spdf@data$PID[ jjj ],
					rain_max_Mha = round( 1e-4 * sum( lst2[[ jjj ]], na.rm = TRUE ), digits = 4 ),
					irr_max_Mha = round( 1e-4 * sum( lst1[[ jjj ]], na.rm = TRUE ), digits = 4 ) )
	
		} ) )
	
	return( df )
	
	} ) )

# Max area in each PID across all crops

pot_area.raster = sum( do.call( stack, lapply( 1:length(crop_names), function( ii ){ 
	
	# Get the irrigated and rainfed yield for this specific crop
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('irr_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')
	irr_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
	crop_folder = list.dirs(path = yeld_path)
	crop_folder = crop_folder[ grep( paste0('rain_', crop_names[ii]), crop_folder)]
	crop_file = list.files(path = crop_folder,pattern = '.tif')
	rain_yield = crop( raster( paste0(crop_folder,'/',crop_file) ), extent(basin.spdf) )
	
	# Calculate area where yield exists
	msk1 = irr_yield
	msk1[ msk1[] > 0 ] = 1
	msk1[ msk1[] != 1 ] = 0
	
	
	msk2 = rain_yield
	msk2[ msk2[] > 0 ] = 1
	msk2[ msk2[] != 1 ] = 0
	
	return( msk1 + msk2 )
	
	} ) ) )
	
pot_area.raster[ pot_area.raster[] > 0 ] = 1
pot_area.raster[ pot_area.raster[] != 1 ] = NA	
pot_area.raster = area( pot_area.raster ) * pot_area.raster

lst = raster::extract( pot_area.raster, basin.spdf, byid = TRUE )
	
max_tot.df = bind_rows( lapply( 1:length( basin.spdf ), function( jjj ){
		
	data.frame( crop = 'all',
				node = basin.spdf@data$PID[ jjj ],
				rain_max_Mha = round( 1e-4 * sum( lst[[ jjj ]], na.rm = TRUE ), digits = 4 ),
				irr_max_Mha = round( 1e-4 * sum( lst[[ jjj ]], na.rm = TRUE ), digits = 4 ) )

	} ) )

max_area.df = rbind( max_area.df, max_tot.df )

write.csv( max_area.df, paste0( getwd(), '/input/max_crop_area.csv'), row.names = F)
	
	