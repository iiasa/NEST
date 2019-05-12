
# Import the basin delineation as a spatialpolygonsdataframe (.spdf) 
# each polygon has a unique ID (PID) and represents the intersection between countries and catchments (i.e., basin country units - bcus)
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )
basin.spdf$BASIN = basin
bcus = as.character( basin.spdf@data$PID )
node = c( 	bcus, 
			unique( as.character( basin.spdf@data$BASIN ) ) ) 

# Get river names and groupings from csv
map_rivers_to_pids.df = data.frame( read.csv( 'input/indus_map_rivers_to_pids.csv', stringsAsFactors = FALSE ) )
basin.spdf@data = merge( basin.spdf@data, map_rivers_to_pids.df, by = 'PID' )
 
# Fixed demands in each regions				
demand_fixed.df = read.csv( "input/indus_demands.csv", stringsAsFactors=FALSE )

# Historical capacity of technologies
hist_new_cap.df = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) %>%
					dplyr::select( -units )

# Capacity factor of renewable technologies
capacity_factor_sw.df = read.csv("input/LF/load_factor_sw.csv", stringsAsFactors=FALSE) %>%
							mutate( time = as.character( time ) )

# Max potential of renewable technologies
max_potential_sw.df = read.csv("input/LF/max_capacity_sw.csv", stringsAsFactors=FALSE)

# Fuel costs
fossil_fuel_cost_var0 = read.csv("input/fuel_cost/cost_variation_gams.csv", stringsAsFactors=FALSE) %>% 
							mutate(year_all = as.numeric(year_all)) %>% rename(value = variation)

# Dam storage capacity	
dam_storage_capacity.df = read.csv("input/storage/dam_storage_capacity_GRANDv1.1_M_m3_PID.csv", stringsAsFactors=FALSE) %>% 
	rename(value = value, node = node)
# avg_storage_multiplier for setting avg level of storage at thebeginning and end of timesteps
avg_storage_multiplier = read.csv('input/storage/average_storage_multiplier.csv', stringsAsFactors=FALSE)

# storage evaporation losses

evap_losses.df = read.csv('input/storage/evapo_loss_reservoir_per_unit_volume.csv', stringsAsFactors = F) %>% 
  filter(scenario == climate_scenario & model == climate_model)

# to sefine rule curves
rule_curves.df = read.csv('input/storage/rule_curves_values.csv', stringsAsFactors = F)

# Transmission capacity and routes	
transmission_routes.df = read.csv("input/basin_transmission/existing_routes.csv", stringsAsFactors=FALSE) %>% 
							mutate(tec = paste0('trs_',tec))
 
# Air pollution emision factors from MESSAGE IAM - convert from Mt per GW per year to ton per MW per day
air_pollution_factors.df = read.csv('input/MESSAGE_SSP2_emission_factor.csv', stringsAsFactors=FALSE) %>%
	mutate( val = round( val * 1e3 / 365, digits = 5 ) )
 
# GWPs for converting pollutants to CO2eq - 100-year time frame
gwp.df = data.frame( 	BCA = 460, # IPCC AR5 
						CH4 = 28,
						CO = 0, 
						N2O = 265,
						NH3 = 0,
						NOx = 0,
						OCA = -69,
						SO2 = 0,
						VOC = 0,
						CO2 = 1 )
gwp.df = data.frame( emission = names( gwp.df ), gwp = unlist( gwp.df )  )						

# Distance between nodes 
distance.df = read.csv('input/PID_distances_km.csv', stringsAsFactors=FALSE)

# Water resource data 
water_resources.df = read.csv( paste0( 'input/basin_water_resources/basin_water_resources_',climate_model,'_',climate_scenario,'.csv' ), stringsAsFactors=FALSE ) %>%
	dplyr::rename( node = PID )
 
environmental_flow.df = read.csv( paste0('input/basin_water_resources/basin_environmental_flow_',climate_model,'_',climate_scenario,'.csv'), stringsAsFactors=FALSE ) %>%
	dplyr::rename( node = PID )
 
# Water canal linkages 
canals.df = read.csv( 'input/indus_bcu_canals.csv', stringsAsFactors=FALSE )
  
# Groundwater energy intensity
gw_ei.df = read.csv( 'input/gw_energy_intensity.csv', stringsAsFactors=FALSE )

# basin_irrigation_transfers    
basin_irrigation_transfers.df = read.csv( 'input/basin_irrigation_transfers.csv', stringsAsFactors=FALSE ) 

# piped water access and wastewater treatment fractions
basin_water_connections.df = read.csv( 'input/indus_water_connections_sdg.csv', stringsAsFactors=FALSE ) %>%
								filter( SSP == 'SSP2' )
								
# fractions of groundwater pumps that are electric (assuming remaining fraction is diesel powered)
electric_share_of_irrigation.df = read.csv( 'input/irrigation_electricity_fraction.csv', stringsAsFactors=FALSE ) %>%
									rename( node = PID )

# land availability [km2] source GAEZ to be multiplyby 100 if we want hectare
land_availability.df = read.csv( 'input/land_availability_map.csv', stringsAsFactors=FALSE )

# crop related data: costs, yields, initial capacity source GAEZ, some is in Mha
crop_input_data.df = read.csv( 'input/crop_input_data.csv', stringsAsFactors=FALSE ) 
crop_names = unique(crop_input_data.df$crop) 

# crop related data:  costs, CF, lifetime. Sources: Ansir
crop_tech_data.df = read.csv( 'input/crop_tech_data.csv', stringsAsFactors=FALSE ) 

# irrigation technologies related data: costs, CF, lifetime. Sources: Ansir
irr_tech_data.df = read.csv( 'input/irr_tech_data.csv', stringsAsFactors=FALSE ) 

# crop water requirements: Source iteraction with CWATM: historical, rcp2.6 and rcp6.0 scenarios - in MCM_per_day_per_Mha
crop_water.df = read.csv('input/crop_irrigation_water_calibrated.csv', stringsAsFactors=FALSE) %>% 
  filter(scenario == climate_scenario, model == climate_model) %>% dplyr::select( crop, node, year, time, value) 

# crop import and export in 2020 for portion of countries in the basin
imp_exp.df = read.csv('input/land_maps_crop_yields/FAO_prices/basin_crop_import_export.csv',stringsAsFactors = F)
  
# import fertilizer use by crop type and region data
fertilizer_by_crop.df = read.csv('input/indus_fertilizer_dat.csv', stringsAsFactors=FALSE) 

# match to PID using provincial level fertilizer data
prov.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'prov_bcu', sep = '_' ), verbose = FALSE )
prov.sp = gUnaryUnion(prov.spdf,prov.spdf@data$ADMIN)
r=raster()
res(r)=0.25
prov.raster = rasterize( prov.sp, crop(r,extent(prov.spdf)) )
mapr = raster::extract(prov.raster,as(basin.spdf,'SpatialPolygons'),byid=TRUE)
mapr = bind_rows( lapply( 1:length(mapr), function(iii){ data.frame( PID = basin.spdf@data$PID[iii], region = names(prov.sp)[as.numeric(names(which.max(table(unlist( mapr[[iii]] )))))] ) } ) )
mapr$region[ which( grepl( 'IND', mapr$PID ) ) ] = 'IND'
mapr$region[ which( grepl( 'CHN|AFG|Northern_Areas', mapr$region ) ) ] = 'PAK|NWFP'
fertilizer_by_crop.df = left_join( mapr, fertilizer_by_crop.df ) %>% select( crop, irrigation, PID, fertilizer, unit, value, min, max )
	
# import fertlizer emissions data and merge with crop data 
fertilizer_emissions.df = left_join( fertilizer_by_crop.df, 
	read.csv('input/fertilizer_emissions.csv', stringsAsFactors=FALSE) %>% select( fertilizer, out, value, unit ) %>%
		rename( value2 = value, unit2 = unit ) ) %>% 
		mutate( value3 = round( value * value2, digits = 5 ), unit3 = 'kg/ha', emission = out ) %>%
		select( PID, crop, fertilizer, irrigation, emission, value3, unit3 ) %>% rename( value = value3, unit = unit3 ) 
  
# Get adjacent and downstream bcus
gt = gTouches( basin.spdf, byid=TRUE )
adjacent.list = lapply( 1:length(basin.spdf), function( iii ){ return( basin.spdf@data$PID[ which( gt[,iii] ) ] ) } )
downstream.list = lapply( 1:length(basin.spdf), function( iii ){ if( !is.na( basin.spdf@data$DOWN[iii] ) ){ return( basin.spdf@data$PID[ which( as.character( basin.spdf@data$PID ) == as.character( basin.spdf@data$DOWN[iii] ) ) ] ) }else{ return( 'SINK' ) } } )
names(adjacent.list) = basin.spdf@data$PID
names(downstream.list) = basin.spdf@data$PID
flow_routes = sapply( 1:length( downstream.list ), function( iii ){ paste( names( downstream.list )[[ iii ]], downstream.list[[ iii ]], sep = '|' ) } ) 
adj1 = unlist( sapply( 1:length( adjacent.list ), function( iii ){ paste( names( adjacent.list )[[ iii ]], adjacent.list[[ iii ]], sep = '|' ) } ) )
adj2 = unlist( sapply( 1:length( adjacent.list ), function( iii ){ paste( adjacent.list[[ iii ]], names( adjacent.list )[[ iii ]], sep = '|' ) } ) )
inds = NULL
for(iii in 1:length(adj2)){ jjj = which( adj1 == adj2[iii] ) ; if( jjj > iii ){ inds = c(inds,jjj) } }
adjacent_routes = adj1[ -1 * inds ]

# Identify basin units along coast - limit to approx. 1.5 degrees from coastline (about 150km near equator)
coast = crop( spTransform( readShapeLines('input/ne_10m_coastline.shp', proj4string = crs(basin.spdf)), crs(basin.spdf) ), extent( buffer( basin.spdf, 0.1 ) ) ) 
cdist = gDistance(coast, basin.spdf, byid = TRUE)
coast_pid = unlist( lapply( 1:nrow(cdist), function(b){ if( length( which( cdist[b,] < 1.5 ) ) > 0 ){ return( as.character( basin.spdf@data$PID[b] ) ) }else{ return( NULL ) } } ) )

# Electricity trade routes for regions outside the basin were identified from basin map 
# electricity_export_routes = c( 	"PAK_2|PAK",
#                                 "PAK_4|PAK",
#                                 "PAK_5|PAK",
#                                 "PAK_6|PAK",
#                                 "PAK_10|PAK",
#                                 "PAK_12|PAK",
#                                 "PAK_13|CHN",
#                                 "CHN_2|CHN",
#                                 "CHN_1|CHN",
#                                 "CHN_3|CHN",
#                                 "AFG_2|AFG",
#                                 "AFG_1|AFG",
#                                 "PAK_7|AFG",
#                                 "PAK_8|AFG",
#                                 "PAK_10|IND",
#                                 "IND_4|IND" )

# take only existing lines in 2015 as transmission lines to the rest of the basin

electricity_export_routes = gsub('.*trs_','', (transmission_routes.df %>% 
                               filter(gsub('.*_','',gsub('.*\\|','',tec)) %in% c('AFG', 'CHN','IND','PAK') & value > 0))$tec )
								
all_elec_routes = c(adjacent_routes,electricity_export_routes)

electricity_import_routes = electricity_export_routes
	
# Commodity types for demand 
type_commodity = data.frame( 	withdrawal = 'freshwater',
								return = 'freshwater',
								electricity = 'electricity' )

# Level types for demand								
type_level = data.frame( 	withdrawal = c( 'irrigation_final', 'urban_final', 'industry_final', 'rural_final' ),
							return = c(NA, 'urban_waste', 'industry_waste', 'rural_waste' ),
							electricity = c('irrigation_final', 'urban_final', 'industry_final', 'rural_final' ), 
							row.names =  c('irrigation','urban','industry','rural')  )	

# Exogenous demands							
demand.df = demand_fixed.df %>%
				filter( scenario == SSP , sector != 'crop') %>%
				mutate( commodity = unlist( type_commodity[ type ] ) ) %>%
				mutate( level = unlist( sapply( 1:length(type),function(iii){ type_level[ sector[iii], type[iii] ] } ) ) ) %>%
				mutate( year_all = as.numeric( year ) ) %>%
				mutate( time = as.numeric( month ) ) %>%
				rename( node = pid ) %>%
        # remove demand freshwater at the irrigation_final level
        filter(level != 'irrigation_final') %>% 
				dplyr::select( node, level, commodity, year_all, time, value )
				
# Convert water demand units from m3 per day to million m3 per day (mcm_per_day)
demand.df$value[ which( demand.df$commodity == 'freshwater' ) ] = demand.df$value[ which( demand.df$commodity == 'freshwater' ) ] / 1e6

# Convert return flows to negative to represent inflow into the system
demand.df$value[ which( grepl( 'waste', demand.df$level ) ) ] = -1 * demand.df$value[ which( grepl( 'waste', demand.df$level ) ) ]

# Add units
demand.df$units = NA
demand.df$units[ which( demand.df$commodity == 'freshwater' ) ] = 'mcm_per_day'
demand.df$units[ which( demand.df$commodity == 'electricity' ) ] = 'MW'

# Round
demand.df$value = round( demand.df$value, digits = 2 )

# Add runoff into PIDs as an additional demand (inflow) into pids
nms = names( water_resources.df )[ grepl( 'runoff_km3_per_day', names( water_resources.df ) ) ]
inflow.df = do.call( rbind, lapply( 1:nrow(water_resources.df), function( iii ){
	data.frame( node = as.character( water_resources.df$node[ iii ] ),
				level = 'river_in',
				commodity = 'freshwater',
				year_all = as.numeric( unlist( strsplit( unlist( strsplit( nms, '[.]' ) )[ seq(1,2*length(nms),by=2) ], '_' ) )[ seq( 5, 5*length( nms ),by=5) ] ),
				time = as.numeric( unlist( strsplit( nms, '[.]' ) )[ seq(2,2*length(nms),by=2) ] ),
				value = -1*round( 1e3 * unlist( water_resources.df[ iii, nms ] ) , digits = 2 ),
				units = 'mcm_per_day')	} ) )
row.names( inflow.df ) = 1:nrow(inflow.df)	
inflow.df = inflow.df %>% dplyr::select( node, commodity, level, year_all, time, value ,units)
demand.df = rbind( demand.df, inflow.df )


# Add inter-basin irrigation transfers as demand taken straight from the river
# transfer.df = do.call( rbind, lapply( 1:nrow(basin_irrigation_transfers.df), function( iii ){
	# do.call( rbind, lapply( unique(demand.df$year_all), function( yyy ){
		# data.frame( node = as.character( basin_irrigation_transfers.df$PID[ iii ] ),
					# level = 'river_in',
					# commodity = 'freshwater',
					# year_all = yyy,
					# time = time,
					# value = round( unlist( basin_irrigation_transfers.df[ iii, which( grepl( paste(time, collapse='|'), names( basin_irrigation_transfers.df ) ) ) ] ) , digits = 2 ),
					# units = 'mcm_per_day')	
				# } ) )
			# } ) )	
# row.names( transfer.df ) = 1:nrow(transfer.df)	
# transfer.df = transfer.df %>% dplyr::select( node, commodity, level, year_all, time, value ,units)
# demand.df = rbind( demand.df, transfer.df )

# Aggregate demands occurring for same commodity and level
demand.df$level = as.character( demand.df$level )
demand.df$commodity = as.character( demand.df$commodity )
demand.df$commodity[ which( as.character( demand.df$level ) == 'urban_waste' ) ] = 'wastewater'
demand.df$level[ which( as.character( demand.df$level ) == 'urban_waste' ) ] = 'urban_final'
demand.df$commodity[ which( as.character( demand.df$level ) == 'rural_waste' ) ] = 'wastewater'
demand.df$level[ which( as.character( demand.df$level ) == 'rural_waste' ) ] = 'rural_final'
demand.df$commodity[ which( as.character( demand.df$level ) == 'industry_waste' ) ] = 'wastewater'
demand.df$level[ which( as.character( demand.df$level ) == 'industry_waste' ) ] = 'industry_final'
demand.df = demand.df %>% 
	group_by(node,level,commodity,year_all,time,units) %>% 
	summarise( value = round( sum( value ), digits = 2 ) ) %>% 
	ungroup( ) %>% data.frame( ) 

# Land demand
land_demand.df = land_availability.df %>% 
  expand(land_availability.df,year_all) %>% 
  mutate(level = 'area') %>% 
  mutate(time = 'year') %>%
  mutate(commodity = 'crop_land') %>% 
  mutate(value = value) %>% #in Mha
  dplyr::select(node,level,commodity,year_all,time,units,value)
  
# add land availability
demand.df = bind_rows(demand.df %>% mutate( time = as.character( time ) ), land_demand.df)

# yield demand, assumption as multiple of historical yield
# for now we set the demand to be equal to the production in 2000, must check if it is feasible with our data
yield_demand.df = demand_fixed.df %>% filter(sector == 'crop') %>% 
  mutate( level = 'raw' ,year_all = as.numeric(year), time = 'year') %>% 
  mutate( commodity = paste0( type, '_yield' ) ) %>% 
  mutate( value = value ) %>%       # ktons
  rename(node = pid) %>% 
  select( node,level,commodity,year_all,time,units,value ) %>%
  mutate( node = unlist( strsplit( as.character( node ), '_' ) )[seq(1,2*length(node),by=2)] ) %>%
  group_by(node,level,commodity,year_all,time,units) %>% 
  summarise(value = sum(value)) %>% ungroup() %>% data.frame()
  
# add yield demamd
demand.df = demand.df %>% 
  mutate(time = as.character(time)) %>% 
  bind_rows(yield_demand.df)

# from 1975 to 1990 in 1990, MW
hist_new_cap.df = hist_new_cap.df %>%  mutate(year_all = as.numeric(year_all))
hist_new_cap.df$tec[hist_new_cap.df$tec == "solar_pv"] = "solar_pv_1"
hist_new_cap.df$tec[hist_new_cap.df$tec == "wind"] = "wind_1"

# fuel var cost
miss_val = expand.grid(commodity = unique(fossil_fuel_cost_var0$commodity),year_all = year, value = 0,stringsAsFactors = FALSE)
fossil_fuel_cost_var = fossil_fuel_cost_var0 %>% 
	bind_rows(anti_join(miss_val,fossil_fuel_cost_var0, by = c("commodity", "year_all"))) %>% 
	filter(year_all %in% year) %>% arrange(commodity,year_all)

# transmission costs million USD per MW, see file Indus_transmission_cost.xlsx - cost of 1.31 USD / kW per km -  in line with WECC data from SWITCH model
mean_dist_to_border = mean(distance.df$dist)/2
trs_inv_cost.df = distance.df %>% mutate( value = round( 1e-3 * dist*1.31, digits = 3 ) ) %>% dplyr::select(-dist) %>%
	bind_rows( data.frame( tec = electricity_export_routes, value = round( 1e-3 * mean_dist_to_border*1.31, digits = 3 ) ) )

# transmission efficiency assuming losses of 0.006% per km (aligned with Parkinson et al.)	
trs_eff.df = distance.df %>% mutate( value = round( 0.01 * dist*0.006, digits = 3 ) ) %>% dplyr::select(-dist) %>%
	bind_rows( data.frame( tec = electricity_export_routes, value = round( 0.01 * 2 * mean_dist_to_border*0.006, digits = 3 ) ) )

# Hurdle rates for transmission outside of the same country
trs_hurdle.df = do.call( rbind, lapply( trs_inv_cost.df$tec, function( iii ){
	ci = unlist( strsplit( iii, '[|]' ) )[1]
	co = unlist( strsplit( iii, '[|]' ) )[2]
	if( unlist( strsplit( ci, '_' ) )[1] != unlist( strsplit( co, '_' ) )[1] | length( unlist( strsplit( co, '_' ) ) ) == 10 ){ hurdle = 10 }else{ 
	  if( unlist( strsplit( ci, '_' ) )[1] == unlist( strsplit( co, '_' ) )[1] & length( unlist( strsplit( co, '_' ) ) ) == 1 ){ hurdle = 0.05 }else{ hurdle = 0 } }
	return( data.frame( tec = iii, hurdle = hurdle ) )
	} ) ) # %>% 
  # mutate(hurdle = if_else(tec == 'PAK_10|IND',10,hurdle))
	
# Canal costs million USD per MCM per day -  costs taken from Parkinson et al. (2016) - approx. 6.70 USD per MCM per day per km	
can_inv_cost.df = distance.df %>% mutate( value = round( dist*6.70, digits = 0 ) ) %>% dplyr::select(-dist) 
	
# aggregate canals with same in and out regions
canals_agg.df = canals.df %>% 
	mutate(route = paste(PID_i,PID_o,sep = '|')) %>% 
	group_by(route) %>% 
	summarise(capacity_m3_per_sec = sum(capacity_m3_per_sec)) %>% ungroup() %>% data.frame()

##### Check consistency, demand and supply/output of techs ######
# can be done in a more automatic way in the future

yield_dem_comb = unique(yield_demand.df %>% dplyr::select(commodity,node)) %>% 
  mutate(commodity = gsub('_yield','',commodity)) %>% 
  rename(tec = commodity) %>% arrange(tec)

irr_yield_comb = crop_input_data.df %>% filter( par == 'irrigation_yield') %>% rename(tec = crop) %>% 
  dplyr::select(tec,node) %>% unique() %>% arrange(tec) %>% 
  mutate(node = gsub('_.*','',node)) %>% unique()

diff_y_demand = dplyr::setdiff(yield_dem_comb,irr_yield_comb) %>% # they have the same combinations
  mutate(commodity = paste0(tec,'_yield')) %>% dplyr::select(-tec)
#print('exclude these elments from demand, or consider checking the supply side')
#print(diff_y_demand)
demand.df <- demand.df %>% anti_join(diff_y_demand)

# Adjust to year_all definition
demand.df <- demand.df %>% filter(year_all <= lastyear)

## data for biomass 
# kton residues per hectare Lal 2004
residue_data = data.frame(crop = c('cotton','fodder','pulses','rice','sugarcane','wheat'),
                          res_yield = c(6.7e3 , 8.4e3 , 6.48e3 , 6.7e3 , 5.6e3 , 5e3), #kt/Mha
                          mode = c(1:6),
                          liquid = c(0 ,1 , 0, 1, 1, 1),
                          ethanol_ratio = c(NA, 0.67, NA, 0.7, 0.153, 0.67),    # kt ethanol/ kt biomass
                          var_eth_cost = c(NA, 0.316, NA, 0.410, 0.291, 0.621)) #2010 M$/kt straw biomass

							  
						  