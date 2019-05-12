
###### Policies  #######

# ENV_FLOWS = F       # Environmental flows
# IND_TREAT = T       # Indus Treaty
# SDG6 = F            # SDG6
# EMISS = F          # GhG Emission
# SDG7 = F        # SDG7: solar/wind targets + phase out of oc
# SDG2 = F        # SDG2 Food and land
# GROUNDWAT = T       # groundwater extraction bounds
# RELAX_LAND = T      # relax land availability constraint (x100 land)
# END_FOOD_IMPORT = F
# END_FOOD_EXPORT = F

# policy_option = paste0('_EF.',substr(ENV_FLOWS, 1,1),'_IT.',substr(IND_TREAT, 1,1),'_EM.',substr(EMISS, 1,1),
#                        '_S6.',substr(SDG6, 1,1),'_S7.',substr(SDG7, 1,1),'_S2.',substr(SDG2, 1,1))
# Options for sensitivity analysis, default options re not declared here
GROUNDWAT.opt = F #c('low')    # options: 'low', 'high'
YIELD_DEM.opt = F #c('flat')   # options: 'flat', 'low' 
 
## Reduced fresh surface runoff into system by X %
percent_reduction = 5
if (REDUCE_RUNOFF){ demand.par = demand.par %>% 
  mutate( value = if_else(level == 'river_in' & grepl('PAK',node),value * (1 - percent_reduction / 100 ), value ) ) } 
 
 
## Environmental flows
if (!ENV_FLOWS){Delta = 0.9}else{Delta = 0.75}

# Set using sustainability boundary approach from Richter et al.
# A boundary of width 2 X Delta is centered on the naturlaized flow
# The recommended value for Delta to maintain high functioning ecosystems is 0.1. 

bound_activity_lo.par = rbind( bound_activity_lo.par, 

	do.call( rbind, lapply( technology.set[ grepl( 'river', technology.set ) & technology.set != 'hydro_river' ], function(tt){
 
		do.call( rbind, lapply( year_all[ which( year_all > baseyear ) ], function(yy){
	 
			data.frame( node = unlist( strsplit( tt, '[|]' ) )[2],
						tec = tt,
						year_all = yy,
						mode = 1,
						time = as.character( time ),
						value = round( 1e3 * ( 1 - Delta ) * unlist( environmental_flow.df[ match( unlist( strsplit( tt, '[|]' ) )[2] , environmental_flow.df$node  ), grepl( paste( paste( yy, time, sep = '.' ), collapse = '|' ), names(environmental_flow.df) ) ] ), digits = 3 ) )
	 
			} ) )
	 
		} ) ) 
	
	 )
 
# bound_activity_up.par = rbind( do.call( rbind, lapply( technology.set[ grepl( 'river', technology.set ) & technology.set != 'hydro_river' ], function(tt){
 
	# do.call( rbind, lapply( year_all[ which( year_all > baseyear ) ], function(yy){
 
		# data.frame( node = unlist( strsplit( tt, '[|]' ) )[2],
					# tec = tt,
					# year_all = yy,
					# mode = 1,
					# time = time,
					# value = round( 1e3 * ( 1 + Delta ) * unlist( environmental_flow.df[ grepl( unlist( strsplit( tt, '[|]' ) )[2], environmental_flow.df$node  ), grepl( paste( paste( yy, time, sep = '.' ), collapse = '|' ), names(environmental_flow.df) ) ] ), digits = 3 ) )
			 
		# } ) )
 
  # } ) ) )  

## Indus water treaty
if (!IND_TREAT){} else{

# Get upstream nodes in India 
policy_nodes = as.character( basin.spdf@data$PID[ which( basin.spdf@data$type_1 %in% c('Upper_Indus','Shyok','Jhelum','Chenab') &  basin.spdf@data$REGION != 'pakistan' ) ] )
policy_nodes = policy_nodes[ -1*which( grepl( 'CHN', policy_nodes ) ) ]

# Constrain closed loop and once through energy withdrawals to zero in all upstream basins outside Pakistan considered in the treaty
# Also limit new hydro plants
tec2limit = technology.set[ grepl( '_ot|_cl|hydro_river|hydro_canal', technology.set ) ]
bound_total_capacity_up.par = rbind( bound_total_capacity_up.par, do.call( rbind,  lapply( tec2limit, function( tt ){
	
	do.call( rbind, lapply( policy_nodes, function( rr ){ 
		
		data.frame( node = rep( rr, length( year_all[ which( year_all > baseyear ) ] ) ),
					tec = rep( tt,length( year_all[ which( year_all > baseyear ) ] ) ),
					year_all = year_all[ which( year_all > baseyear ) ],
					value = rep(0,length( year_all[ which( year_all > baseyear ) ] ) ) )  
		
			} ) )
	
		} ) ) 
		
	)  
 
# Constrain irrigated area from Annex C in Treaty document
# 70,000 acres max in Indus
# 400,000 acres max in Jhelum
# 225,000 acres max  in Chenab 
land_constraint.df = data.frame( 	IND_5 = 35000, # Indus
								 	IND_6 = 35000, # Indus
									IND_1 = 400000, # Jhelum
									IND_2 = 225000 ) * 0.404 / 1e6 # Chenab - convert to Mha

# Simplified approach: Bound irrigation withdrawals to historical levels in the sub-basins of India impacted by the policy
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE )	
bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par, 
	
	bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
	
		historical_capacity.df1 %>% 
			filter( tec == 'irrigation_sw_diversion', node %in% policy_nodes ) %>% 
			group_by( node, tec ) %>%
			summarise( value = sum( value ) ) %>%
			ungroup() %>% data.frame() %>%
			mutate( year_all = yyy ) %>% 
			select( node, tec, year_all, value )
		
		} ) )
	
	)	
	
} #end if

## Water access and treatment rates for SDG6 targets
if (!SDG6){} else{
  
bound_activity_lo.par = bind_rows( bound_activity_lo.par, 
	
	# Urban piped water access
	do.call( rbind, lapply( nds, function( nnn ){
															
		ww = demand.df %>% filter( node == nnn, level == 'urban_final', commodity == 'freshwater' )%>%
				dplyr::rename( demand = value )
		
		fr = basin_water_connections.df %>% filter( PID == nnn ) %>%
				dplyr::select( year, urban_connection_rate ) %>%
				dplyr::rename( year_all = year )
		
		df = merge( ww, fr, by = 'year_all', all.x=TRUE ) %>%
				mutate( value = abs( round( demand * urban_connection_rate, digits = 2 ) ) ) %>%
				mutate( mode = 1 ) %>%
				dplyr::select( node, year_all, mode, time, value  )
		
		return( df )			
			
		} ) ) %>% mutate(tec = 'urban_piped_distribution') %>% dplyr::select( node, tec, year_all, mode, time, value ),
	
	
	# Rural piped water access
	do.call( rbind, lapply( nds, function( nnn ){
															
				ww = demand.df %>% filter( node == nnn, level == 'rural_final', commodity == 'freshwater' )%>%
						dplyr::rename( demand = value )
				
				fr = basin_water_connections.df %>% filter( PID == nnn ) %>%
						dplyr::select( year, rural_connection_rate ) %>%
						dplyr::rename( year_all = year )
				
				df = merge( ww, fr, by = 'year_all', all.x=TRUE ) %>%
						mutate( value = abs( round( demand * rural_connection_rate, digits = 2 ) ) ) %>%
						mutate( mode = 1 ) %>%
						dplyr::select( node, year_all, mode, time, value  )
				
				return( df )			
					
				} ) ) %>% mutate(tec = 'rural_piped_distribution') %>% dplyr::select( node, tec, year_all, mode, time, value ),
	
	# Urban wastewater treatment rate
	do.call( rbind, lapply( nds, function( nnn ){
															
		ww = demand.df %>% filter( node == nnn, level == 'urban_final', commodity == 'wastewater' )%>%
				dplyr::rename( demand = value )
		
		fr = basin_water_connections.df %>% filter( PID == nnn ) %>%
				dplyr::select( year, urban_treated_rate ) %>%
				dplyr::rename( year_all = year )
		
		df = merge( ww, fr, by = 'year_all', all.x=TRUE ) %>%
				mutate( value = abs( round( demand * urban_treated_rate, digits = 2 ) ) ) %>%
				mutate( mode = 1 ) %>%
				dplyr::select( node, year_all, mode, time, value  )
		
		return( df )			
			
		} ) ) %>% mutate(tec = 'urban_wastewater_collection') %>% dplyr::select( node, tec, year_all, mode, time, value ),
		
	# Industry wastewater treatment rate
	do.call( rbind, lapply( nds, function( nnn ){
															
		ww = demand.df %>% filter( node == nnn, level == 'urban_final', commodity == 'wastewater' )%>%
				dplyr::rename( demand = value )
		
		fr = basin_water_connections.df %>% filter( PID == nnn ) %>%
				dplyr::select( year, urban_treated_rate ) %>%
				dplyr::rename( year_all = year )
		
		df = merge( ww, fr, by = 'year_all', all.x=TRUE ) %>%
				mutate( value = abs( round( demand * urban_treated_rate, digits = 2 ) ) ) %>%
				mutate( mode = 1 ) %>%
				dplyr::select( node, year_all, mode, time, value  )
		
		return( df )			
			
		} ) ) %>% mutate(tec = 'industry_wastewater_collection') %>% dplyr::select( node, tec, year_all, mode, time, value ),	
	
	# Rural wastewater treatment rate
	do.call( rbind, lapply( nds, function( nnn ){
															
		ww = demand.df %>% filter( node == nnn, level == 'rural_final', commodity == 'wastewater' )%>%
				dplyr::rename( demand = value )
		
		fr = basin_water_connections.df %>% filter( PID == nnn ) %>%
				dplyr::select( year, rural_treated_rate ) %>%
				dplyr::rename( year_all = year )
		
		df = merge( ww, fr, by = 'year_all', all.x=TRUE ) %>%
				mutate( value = abs( round( demand * rural_treated_rate, digits = 2 ) ) ) %>%
				mutate( mode = 1 ) %>%
				dplyr::select( node, year_all, mode, time, value  )
		
		return( df )			
			
		} ) ) %>% mutate(tec = 'rural_wastewater_collection') %>% dplyr::select( node, tec, year_all, mode, time, value )
		
	)


## Wastewater recycling
# Constrain wastewater recycling to at least X % urban return flow			
bound_activity_lo.par = bind_rows( bound_activity_lo.par, 
	
	do.call( rbind, lapply( node[ node != basin ], function( nnn ){
	
		do.call( rbind, lapply( year_all[ which( year_all > baseyear ) ], function( yyy ){ 
			
			do.call( rbind, lapply( time, function( mmm ){
			
				data.frame( node = nnn, 
							tec = 'urban_wastewater_recycling',
							year_all = yyy, 
							mode = 1,
							time = as.character( mmm ),
							value = abs( round( 	0.1 * min( 	0, 
														demand.par %>% 
															dplyr::filter( node == nnn, commodity == 'wastewater', level == 'urban_final', year_all == yyy, time == mmm ) %>%
															dplyr::select( value ) %>% unlist() ,
														na.rm = TRUE ), digits = 3 ) ) )
				
				} ) )										
														
			} ) ) 
		
		} ) ),
		
	do.call( rbind, lapply( node[ node != basin ], function( nnn ){
	
		do.call( rbind, lapply( year_all[ which( year_all > baseyear ) ], function( yyy ){ 
			
			do.call( rbind, lapply( time, function( mmm ){
			
				data.frame( node = nnn, 
							tec = 'industry_wastewater_recycling',
							year_all = yyy, 
							mode = 1,
							time = as.character( mmm ),
							value = abs( round( 	0.1 * min( 	0, 
														demand.par %>% 
															dplyr::filter( node == nnn, commodity == 'wastewater', level == 'industry_final', year_all == yyy, time == mmm ) %>%
															dplyr::select( value ) %>% unlist() ,
														na.rm = TRUE ), digits = 3 ) ) )
				
				} ) )										
														
			} ) ) 
		
		} ) )	
		
	)	

} #end if

## GHG Emissions
if (!EMISS){} else{


	# Set emissions trajectory relative to baseline emissions
	require( gdxrrw )
	igdx( gams_path ) ## Set based on local machine	
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
								paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ), 
								list( name = 'EMISS', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	base_ems.df = rs %>% filter( node == basin, emission == 'CO2eq', type_tec == 'all' ) %>% select( year_all, value ) 					
	rm( rs, tmp )

	bound_emission.par = data.frame( 	node = basin,
										type_emission = 'CO2eq',
										type_tec = 'all',
										type_year = base_ems.df$year_all,
										value = c(0.75,0.2,0.1,0.1,0.1) * base_ems.df$value )


	# bound_emission.par = rbind(data.frame( 	node = 'AFG',
									  # type_emission = 'CO2',
									  # type_tec = 'all',
									  # type_year = as.factor(c(2020,2030,2040,2050,2060)),
									  # value = c(12.4,17.1,17.1,17.1,17.1)*10^5 ),
							   # data.frame( 	node = 'IND',
											# type_emission = 'CO2',
											# type_tec = 'all',
											# type_year = as.factor(c(2020,2030,2040,2050,2060) ) ,
											# value = c(61.2,50.5,50.5,50.5,50.5)*10^5 ),
							   # data.frame( 	node = 'PAK',
											# type_emission = 'CO2',
											# type_tec = 'all',
											# type_year = as.factor(c(2020,2030,2040,2050,2060) ),
											# value = c(487.5,896.3,896.3,896.3,896.3)*10^5 )
							  # )

  } #end if	

# Wind / solar target (in produced MW over the entire year)
if (!SDG7){} else{
# bound_emission.par = bind_rows( list( 	bound_emission.par,
# 										data.frame( node = basin,
# 													type_emission = 'wind_credit',
# 													type_tec = 'all',
# 													type_year = c(2020,2030,2040,2050,2060),
# 													value = -12*4*c(500,1000,2000,3000,4000) ),
# 										data.frame( node = basin,
# 													type_emission = 'solar_credit',
# 													type_tec = 'all',
# 													type_year = c(2020,2030,2040,2050,2060),
# 													value = -12*4*c(500,1000,2000,3000,4000) ) ) )

# use share equations
shares.set = 'solar_wind_electricity'

type_tec.set = c(type_tec.set,'share_total','share_solar_wind')

power_tec = (cat_tec.set %>% filter(type_tec == 'power' & !grepl('trs|distribution|ethanol|genset|biom',tec)) )$tec

cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'share_total',
                                               tec = power_tec),
                                        data.frame(type_tec = 'share_solar_wind',
                                                   tec = power_tec[grepl('solar|wind|geothermal',power_tec)]))

node_share.tmp = map_node.set %>% filter(node1 %in% c('AFG','IND','PAK') & !node2 %in% c('AFG','IND','PAK')) %>%
  rename(node_share = node1, node = node2)
map_shares_commodity_total.set = crossing(data.frame(shares = shares.set,
                                          type_tec = 'share_total',
                                          mode = c(1,2,3,4),
                                          commodity = 'electricity',
                                          level = 'energy_secondary'),
                                          node_share.tmp ) %>% 
  select(shares,node_share,node,type_tec,mode,commodity,level)

map_shares_commodity_share.set = crossing(data.frame(shares = shares.set,
                                                     type_tec = 'share_solar_wind',
                                                     mode = c(1,2,3,4),
                                                     commodity = 'electricity',
                                                     level = 'energy_secondary'),
                                          node_share.tmp ) %>% 
  select(shares,node_share,node,type_tec,mode,commodity,level)

share_commodity_lo.par = crossing(data.frame(shares = shares.set,
                                             node_share = node_share.tmp$node_share,
                                             unit = '%'),
                                  crossing(data.frame(year_all = c(2020, 2030, 2040, 2050, 2060),
                                                      value =    c(0.1,  0.2,  0.2,  0.3,  0.3)),
                                           time = time) ) %>% 
  select(shares,node_share,year_all,time,value)

# phase out of coal
bound_activity_up.par = rbind( bound_activity_up.par,
                               crossing( crossing(node = node[ node != basin ],
                                        year_all = c(2030,2040,2050,2060),
                                        time = as.numeric(time)),
                               crossing(tec = technology.set[grepl('_ot',technology.set)],
                                             mode = c(1,2)) %>% 
                                  bind_rows(data.frame(tec = "coal_st_ot",mode = c(3,4),stringsAsFactors = F))
                              ) %>% 
                                select(node,tec,year_all,mode,time) %>% 
                                mutate(value = 0))
										
} #end if	


if (!SDG2){
  
  # no smart irrigation before 2030 in the baseline
  bound_total_capacity_up.par = bound_total_capacity_up.par %>% bind_rows(
    crossing(node = node[ node != basin ], year_all = year_all[year_all<=2030], 
             tec = technology.set[grepl('smart',technology.set)],value = 0	) )
  
  } else{
  
    # no flood irrigation after 2030, with the exception of rice
    bound_total_capacity_up.par = bound_total_capacity_up.par %>% bind_rows(
      crossing(node = node[ node != basin ], year_all = year_all[year_all>=2030], 
               tec = technology.set[grepl('flood',technology.set) & technology.set != 'irr_flood_rice'],value = 0	) )
  
} #end if	
															
## Groundwater extraction
if (!GROUNDWAT){} else{
	
	# Constrain fossil groundwater extraction to pct of baseline
	pct_bsl = c( 0.5, 0.1, 0.01, 0.01, 0.01 ) # 2020 to 2060 
	require( gdxrrw )
	igdx( gams_path ) ## Set based on local machine	
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
								paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ), 
								list( name = 'EMISS', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	base_gw.df = rs %>% filter( node %in% bcus, emission == 'groundwater', type_tec == 'all' ) %>% select( node, emission, type_tec, year_all, value ) 					
	rm( rs, tmp )
	
	tmp = crossing(node = node[-25] ,type_year =as.character( c(2020,2030,2040,2050,2060) ))

	gw_bound = tmp %>% left_join(left_join( base_gw.df, data.frame( year_all = unique( base_gw.df$year_all ), val = pct_bsl ) ) %>%
	                       mutate( value = val * value * Beta_water) %>%
	                       select( node, emission, type_tec, year_all, value ) %>%
	                       rename( type_emission = emission, type_year = year_all ) ) %>% 
	  mutate(type_emission = 'groundwater',type_tec = 'all') %>% 
	  group_by(type_year) %>% 
	  mutate(value = if_else(is.na(value), 0,value)) %>% 
	  mutate(value = if_else(value == 0, max(value),value)) %>% ungroup()
	
	bound_emission.par = bind_rows( bound_emission.par, 
	                                gw_bound		) 
									
  if (!GROUNDWAT.opt) {} else{
    # do something with options eg.
    opts = data.frame(opt = c('low','high') , mult = c(0.75,1.25) ) %>% 
      filter(opt == GROUNDWAT.opt)
    
    bound_emission.par = bound_emission.par %>% 
      mutate(value = if_else((type_emission == 'groundwater' & year_all >= 2020), value * opts$mult, value) )
    
  }
} #end if

## Yiels Demand options
if (!exists('YIELD_DEM.opt')){} else{
  if (YIELD_DEM.opt == 'low'){
    mult = data.frame( year_all = c(2015,2020,2030,2040,2050,2060),
                       mult =     c(1,   0.9, 0.85,0.8, 0.75,0.7 ))
    
    demand.par = demand.par %>% 
      left_join(mult) %>% 
      mutate(value = if_else(level == 'raw', value * mult, value )) %>% 
      select(-mult)
  }
  
  if (YIELD_DEM.opt == 'flat'){
    
    demand.par = demand.par %>%  
      group_by(node,commodity,level,time) %>% 
      mutate(value = if_else(level == 'raw', first(value), value )) %>% 
      ungroup()
    
  }
}
  
	# ) 

if (!RELAX_LAND){} else{
  
## release area constraints
demand.par = demand.par %>%  
  group_by(node,commodity,level,time) %>% 
  mutate(value = if_else(level == 'area', 1.5* value, value )) %>% 
  ungroup()

}

if (!END_FOOD_IMPORT) {} else{
  
  imp_exp_add = imp_exp.df %>% rename(value_add = value) %>% filter(imp_exp == 'import') %>% 
    mutate(commodity = paste0(crop,'_yield')) %>% select(node,commodity,value_add)
  
  demand.par = demand.par %>% 
    left_join(imp_exp_add) %>% 
    mutate(value = if_else(level == 'raw' & !is.na(value_add), value + value_add, value )) %>% 
    select(-value_add)
  
}


if (!END_FOOD_EXPORT) {} else{
  
  imp_exp_add = imp_exp.df %>% rename(value_add = value) %>% filter(imp_exp == 'export') %>% 
    mutate(commodity = paste0(crop,'_yield')) %>% select(node,commodity,value_add)
  
  demand.par = demand.par %>% 
    left_join(imp_exp_add) %>% 
    mutate(value = if_else(level == 'raw' & !is.na(value_add), value - value_add, value )) %>% 
    select(-value_add)
  
}

# import upper bound
# fix electricity import to be lower or equal to the baseline, not to
if (!FIX_ELEC_IMPORT) {} else{

igdx( gams_path ) ## Set based on local machine
tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''),
                   paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ),
            list( name = 'ACT', form = "sparse" ) )
names(tmp$uels) = tmp$domains
rs = data.frame( tmp$val )
names(rs) = c( unlist( tmp$domains ), 'value' )
rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
trs_bound = rs %>% filter(tec %in% exp_routes_names) %>% dplyr::select(node,tec,year_all,mode,time,value) %>%
  group_by(node,tec,year_all,mode,time) %>% summarise(value = sum(value)) %>% ungroup()
bound_activity_up.par = rbind( bound_activity_up.par,
                               trs_bound)

}