
###### Baseline Policies  #######
# default policy options that does not require activation, identify the baseline

## current policies in India on solar energy
# use share equations
shares.set = c( shares.set, 'solar_electricity' )

type_tec.set = c(type_tec.set,'share_total','share_solar')

power_tec = (cat_tec.set %>% filter(type_tec == 'power' & !grepl('trs|distribution|ethanol|genset|biom|wind|geothermal',tec)) )$tec

cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'share_total',
                                               tec = power_tec),
                                        data.frame(type_tec = 'share_solar',
                                                   tec = power_tec[grepl('solar',power_tec)]))

node_share.tmp = map_node.set %>% filter(node1 %in% c('IND') & !node2 %in% c('IND')) %>%
  rename(node_share = node1, node = node2)
map_shares_commodity_total.set = crossing(data.frame(shares = 'solar_electricity',
                                          type_tec = 'share_total',
                                          mode = factor(c(1,2,3,4)),
                                          commodity = 'electricity',
                                          level = 'energy_secondary'),
                                          node_share.tmp ) %>%
  dplyr::select(shares,node_share,node,type_tec,mode,commodity,level)

map_shares_commodity_share.set = crossing(data.frame(shares = 'solar_electricity',
                                                     type_tec = 'share_solar',
                                                     mode = factor(c(1,2,3,4)),
                                                     commodity = 'electricity',
                                                     level = 'energy_secondary'),
                                          node_share.tmp ) %>%
  dplyr::select(shares,node_share,node,type_tec,mode,commodity,level)

share_commodity_lo.par = crossing(data.frame(shares = 'solar_electricity',
                                             node_share = node_share.tmp$node_share,
                                             unit = '%'),
                                  crossing(data.frame(year_all = as.factor(c(2020, 2030, 2040, 2050, 2060)),
                                                      value =    c(0.1,  0.2,  0.2,  0.2,  0.2)),
                                           time = time) ) %>%
  dplyr::select(shares,node_share,year_all,time,value)

# boundaries on surface water matching historical
tmp3 = historical_new_capacity.par %>% 
  filter(grepl('irrigation_sw_diversion',tec)) %>% filter(year_all < 2020) %>% 
  group_by(node,tec,year_all) %>% 
  summarise(value = sum(value)) %>% ungroup() %>% 
  filter( !grepl('AFG|CHN',node)  )

# force water diversion in 2020 to be as calibration ONLY in 2020
bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
                                    crossing(tmp3 %>% select(-year_all),
                                             year_all = '2020') %>%
                                      select(node,tec,year_all,value)  )

bound_total_capacity_up.par = rbind(bound_total_capacity_up.par,
                                    crossing(tmp3 %>% select(-year_all),
                                             year_all = '2020' ) %>%
                                      mutate(value = 1.05 * value) %>%
                                      select(node,tec,year_all,value)  )

###### Baseline Policies  #######
# Policies that require activation

# ## Reduced fresh surface runoff into system from high elevation sources due to climate change
# # Used to simulate more extreme impacts than projected in the hydrological model/output/
# # A fraction of the run off is reduced in the upstream parts of the basin considered to be mountain regions
# # Exponential function that saturates at an assumed percentrage decrease relaive to the hydrological model
fraction_reduction = 0.5
saturation_year = 2050
saturation_rate = -1 * log( 0.1 ) / ( saturation_year - baseyear )
fraction_value = fraction_reduction * ( 1 - exp( -1 * saturation_rate * ( year_all[ year_all > baseyear ] - baseyear ) ) )
fdf = data.frame( year_all = year_all[ year_all >= baseyear ], frc = c( 0, fraction_value ) )
fdf$frc = fdf$frc * fraction_reduction / fdf$frc[ which( fdf$year_all == saturation_year ) ]
mountain_nodes = c( bcus[ grepl( 'IND|CHN|AFG', bcus ) ], 'PAK_7', 'PAK_8', 'PAK_11', 'PAK_13')

frc = c(0.2,0.2,0.2,0.5,0.8, 0.9, 0.9, 0.8, 0.6, 0.5, 0.4, 0.3)
fdf.df = data.frame(time = time,frc = as.numeric(frc), stringsAsFactors = F)
if (!REDUCE_RUNOFF){}else{
	demand.par = demand.par %>% left_join(fdf.df ) %>%
		mutate( value = if_else( level == 'inflow', value * (1-frc), value ) ) %>%
		dplyr::select( -frc )
	}


## Indus water treaty
if (IND_TREAT){

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
						year_all = as.character(year_all[ which( year_all > baseyear ) ]),
						value = rep(1e-6,length( year_all[ which( year_all > baseyear ) ] ) ) )  
			
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
	historical_capacity.df1 = hist_new_cap.df
	bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par,

		bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){

			historical_capacity.df1 %>%
				filter( tec == 'irrigation_sw_diversion', node %in% policy_nodes ) %>%
				group_by( node, tec ) %>%
				summarise( value = sum( value ) + 1e-6 ) %>%
				ungroup() %>% data.frame() %>%
				mutate( year_all = as.character(yyy) ) %>%
				dplyr::select( node, tec, year_all, value )

			} ) )

		)

	# Divert Ravi / Sutlej flows to represent transfers outside the basin through canals such as Indira Ghandhi
	# Note that some diversions are already included in the irrigation representation
	# See Wescoat et al. 2018 figure 3		
	# Get upstream nodes in India 
	policy_nodes = as.character( basin.spdf@data$PID[ which( basin.spdf@data$type_2 == 'eastern_rivers' &  basin.spdf@data$REGION != 'pakistan' ) ] )
	policy_nodes = policy_nodes[ -1*which( grepl( 'CHN', policy_nodes ) ) ]
	bound_activity_up.par = bind_rows( bound_activity_up.par, 
		bind_rows( lapply( policy_nodes, function( nnn ) { 
			expand.grid( 	node = nnn, 
							tec = paste0( 'river|',nnn,'|',basin.spdf@data$DOWN[ which( basin.spdf@data$PID == nnn ) ] ),
							year_all = as.character(year_all), 
							mode = c('1'),
							time = as.character( time ), 
							value = 3.5 ) # approx. 1 MAF  per yearconverted to MCM per day - Note Wescoat et al 2018 show declining flows to ~ 1 MAF per year
			} ) )
	)		
					
	# Remove inconsistent activity bounds from environmental flows	
	bound_activity_lo.par = bound_activity_lo.par %>% filter( !(node %in% policy_nodes & tec == 'environmental_flow') )
		
	# Remove opportunity to expand conveyance out of eastern rivers PIDs to other basin PIDs 
	# Allow transferring between eastern river Pid but disallow transfers out to other PIDs e.g., to PAK and CHN.
	tlim = technology.set[ grepl( c('lined_canal|conv_canal'), technology.set ) ]
	tlim = tlim[ grepl( paste( policy_nodes, collapse = '|' ), tlim ) ]
	tlim = unlist( c( sapply( policy_nodes, function( nnn ){ tlim[ grepl( nnn, unlist( strsplit( tlim, '[|]' ) )[seq(2,3*length(tlim),by=3)] ) & !( grepl( paste( policy_nodes, collapse = '|' ), unlist( strsplit( tlim, '[|]' ) )[seq(3,3*length(tlim),by=3)] ) ) ] } ) ) )
	bound_total_capacity_up.par	= bind_rows( bound_total_capacity_up.par, 
		bind_rows( lapply( tlim, function( ttt ){
			expand.grid(	node = unlist( strsplit( ttt, '[|]' ) )[ 2 ],
							tec = tlim,
							year_all = as.character(year_all[year_all>baseyear]),
							value = 1e-6 ) 
			} ) )
		)						
	
	} #end if
 
## Environmental flows (EF)
# Indus can't be transformed to naturalized conditions so better to focus on improving EFs instead of basis on natural flows
# see: Acreman et al "Environmental flows for natural, hybrid, and novel riverine ecosystems in a changing world"
if (sc == 'environment'){fractional_increment_ef = 2 
} else if (sc == 'economy'){
fractional_increment_ef = 0.5  
} else {
  fractional_increment_ef = 1  
}
if ( ENV_FLOWS & sc != baseline0 ){
	
	# Import baseline0 solution from gdx
	igdx( gams_path )
	upath = paste( indus_ix_path, '/model/output/', sep='')
	fpath = paste( 	'MSGoutput_', baseline0, '.gdx', sep = '' )	
	vv = c('ACT')
	tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ 
		sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) 
		} ) )
	rs = rs %>% filter( tec == 'environmental_flow' ) %>% 
	  mutate( value = value * ( 1 + fractional_increment_ef )) %>% 
	  select(node,tec,year_all,mode,time,value)
	rm( tmp )
	
	value_by_time =  data.frame(val = c(12.23, 12.23, 12.23, 46.21, 46.21 ,46.21, 46.21, 46.21, 46.21, 12.23, 12.23, 12.23),
	                            time = as.character(seq(1,12,1)), stringsAsFactors = F )
	
	pak_4_pol = environmental_flow.df %>% mutate(tec = 'environmental_flow',mode = c('1'),time = as.character(time),year_all = as.character(year_all)) %>%  
	  filter(node %in% c('PAK_4'), year_all >2015) %>% left_join(value_by_time) %>% 
	  mutate(value = val) %>%  ungroup() %>% # already in MCM 
	  dplyr::select(node,tec,year_all,mode,time,value) %>% 
	  left_join(rs %>% rename(base_val = value)) %>% 
	  mutate(base_val = if_else(is.na(base_val), 0, base_val) ) %>% 
	  mutate(value = pmax(value,base_val)) %>% 
	  select(node,tec,year_all,mode,time,value)
	
	bound_activity_lo.par = rbind( bound_activity_lo.par, pak_4_pol   )
		
	}
	
## No planned hydropower
if (!NOT_PLANNED_HYDRO){} else {
	
	bound_total_capacity_lo.par =  rbind( 
		bound_total_capacity_lo.par %>% filter( tec != 'hydro_old' ), 
		bound_total_capacity_lo.par %>% filter(tec == 'hydro_old') %>%
			group_by( node, tec ) %>% 
			mutate( value = min(value) ) %>%
			as.data.frame( ) 
		)
		
	bound_total_capacity_up.par =  rbind( 
		bound_total_capacity_up.par %>% filter( tec != 'hydro_old' ), 
		bound_total_capacity_up.par %>% filter(tec == 'hydro_old') %>%
			group_by( node, tec ) %>% 
			mutate( value = min(value) ) %>%
			as.data.frame( ) 
		)	

	bound_storage_up.par = bound_storage_up.par %>% 
		group_by(node,commodity,level,time) %>% 
		mutate(value = min(value)) %>%
		as.data.frame( )	
	
	bound_storage_lo.par = bound_storage_lo.par %>% 
		group_by(node,commodity,level,time) %>% 
		mutate(value = min(value)) %>%
		as.data.frame( )
  
	} #end if

## Water access and treatment rates for SDG6 targets
if (SDG6 & sc != baseline0){
  
	
	tmp = rbind(
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

			ww = demand.df %>% filter( node == nnn, level == 'industry_final', commodity == 'wastewater' )%>%
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
			
		) %>% mutate(year_all = as.character(year_all), mode = as.character(mode))
	bound_activity_lo.par = bind_rows( bound_activity_lo.par, tmp)
	## Wastewater recycling
	# Constrain wastewater recycling to at least X % urban return flow			
	tmp = rbind(
		
		do.call( rbind, lapply( node[ node != basin ], function( nnn ){
		
			do.call( rbind, lapply( year_all[ which( year_all > baseyear ) ], function( yyy ){ 
				
				do.call( rbind, lapply( time, function( mmm ){
				
					data.frame( node = nnn, 
								tec = 'urban_wastewater_recycling',
								year_all = yyy, 
								mode = 1,
								time = as.character( mmm ),
								value = abs( round( 	0.25 * min( 	0, 
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
								value = abs( round( 	0.25 * min( 	0,
															demand.par %>%
																dplyr::filter( node == nnn, commodity == 'wastewater', level == 'industry_final', year_all == yyy, time == mmm ) %>%
																dplyr::select( value ) %>% unlist() ,
															na.rm = TRUE ), digits = 3 ) ) )

					} ) )

				} ) )

			} ) )
			
		)
	tmp = tmp %>% mutate_if(!sapply(tmp, is.character), as.character) %>% 
	  mutate(value = as.numeric(value)) 
	bound_activity_lo.par = bind_rows( bound_activity_lo.par, tmp)
	
	## WHAT BEFORE WE CALLED SURFACE_ALLOCATION
	# allocation_reduction = 3
	## Maintain surface withdrawal capacity to limit expanded surface allocation (surface water fully allocated in Indus ie.., no room for expanded use) 
	  
	  # Get the historical capacity of surface water diversions and then bound total capacity of sw_extract to prevent further expansion
	  historical_capacity.df1 = hist_new_cap.df
	  sw_extract_cap.df = historical_capacity.df1 %>%
	    filter( grepl( 'sw_diversion', tec ) ) %>%
	    filter(node != 'PAK_13') %>%
	    group_by( node ) %>%
	    summarise( value = sum(value) * 1) %>%
	    as.data.frame( )

	  # gw_extract_agri_cap.df = historical_capacity.df1 %>%
	  #   filter( grepl( 'irrigation_gw_diversion', tec ) ) %>%
	  #   filter(node != 'PAK_13') %>%
	  #   group_by( node ) %>%
	  #   summarise( value = sum(value) * allocation_reduction) %>%
	  #   as.data.frame( ) %>%
	  #   crossing(year_all =  c('2030','2040','2050')) %>%
	  #   mutate( tec = 'irrigation_gw_diversion' ) %>%
	  #   dplyr::select( names( bound_total_capacity_up.par )) %>%
	  #   mutate(value = if_else((node == 'IND_1' & year_all == 2050), value +2 ,
	  #                      if_else((node == 'IND_2' & year_all == 2050), value +3 ,
	  #                          if_else((node %in% c('PAK_5','PAK_12','PAK_9') & year_all == 2050), value + 10 ,
	  #                              if_else((grepl('PAK_',node) & year_all == 2050), value * 1.5 ,
	  #                                 if_else((node %in% c('IND_5','IND_6') & year_all == 2050), value +0.05 , value ) ) ) )) )



	  # bound_total_capacity_up.par = data.frame( node=factor(),tec=factor(),year_all=factor(),value=numeric() )
	  bound_total_capacity_up.par = bind_rows(
	    bound_total_capacity_up.par,
	    bind_rows( lapply( c(2030,2040,2050), function( yyy ){
	      sw_extract_cap.df %>% mutate( tec = 'sw_extract', year_all = as.character(yyy) ) %>% dplyr::select( names( bound_total_capacity_up.par ) )
	    } ) )
	    # bind_rows(
	    #   gw_extract_agri_cap.df   )
	  )
	
	  
	  # demand and food production at basin level, not countrty level
	  demand.par = demand.par %>% filter(level != 'raw') %>%
	    bind_rows(	demand.par %>% filter(level == 'raw') %>%
	                 mutate(node = gsub('_.*','',node) ) %>%
	                 group_by(node,commodity,level,year_all,time) %>%
	                 summarize(value = sum(value)) %>% as.data.frame() %>%
	                 dplyr::select(node,commodity,level,year_all,time,value) )
	  
	  output.par = output.par %>% filter(level != 'raw') %>%
	    bind_rows(	output.par %>% filter(level == 'raw') %>%
	                 mutate(node_out = gsub('_.*','',node) ) %>%
	                 group_by(node,tec,vintage,year_all,mode,node_out,commodity,level,time,time_out) %>%
	                 summarize(value = sum(value)) %>% ungroup() %>%
	                 dplyr::select(node,tec,vintage,year_all,mode,node_out,commodity,level,time,time_out,value) )
}# END if


## GHG Emissions
if( EMISS & sc != baseline0 ){


	# Set emissions trajectory relative to baseline emissions
	require( gdxrrw )
	igdx( gams_path ) ## Set based on local machine	
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
								paste( 	'MSGoutput_no_planned_hydro.gdx', sep = '' ), sep = '' ), 
								list( name = 'EMISS', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	base_ems.df = rs %>% filter( node == basin, emission == 'CO2eq', type_tec == 'all' ) %>% dplyr::select( year_all, value ) 					
	rm( rs, tmp )

	bound_emission.par = data.frame( 	node = basin,
										type_emission = 'CO2eq',
										type_tec = 'all',
										type_year = base_ems.df$year_all,
										value = c(0.5,0.3,0.1,0.1,0.1) * base_ems.df$value )

	} #end if	

# Wind / solar target (in produced MW over the entire year)
if (SDG7 & sc != baseline0){
	
	# # use share equations
	# shares.set = c( shares.set, 'solar_wind_electricity' )
	#
	# type_tec.set = c(type_tec.set,'share_total','share_solar_wind')
	#
	# power_tec = (cat_tec.set %>% filter(type_tec == 'power' & !grepl('trs|distribution|ethanol|genset|biom',tec)) )$tec
	#
	# cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'share_total',
	#                                                tec = power_tec),
	#                                         data.frame(type_tec = 'share_solar_wind',
	#                                                    tec = power_tec[grepl('solar|wind|geothermal',power_tec)]))
	#
	# node_share.tmp = map_node.set %>% filter(node1 %in% c('AFG','IND','PAK') & !node2 %in% c('AFG','IND','PAK')) %>%
	#   rename(node_share = node1, node = node2)
	# map_shares_commodity_total.set = crossing(data.frame(shares = 'solar_wind_electricity',
	#                                           type_tec = 'share_total',
	#                                           mode = factor(c(1,2,3,4)),
	#                                           commodity = 'electricity',
	#                                           level = 'energy_secondary'),
	#                                           node_share.tmp ) %>%
	#   dplyr::select(shares,node_share,node,type_tec,mode,commodity,level)
	#
	# map_shares_commodity_share.set = crossing(data.frame(shares = 'solar_wind_electricity',
	#                                                      type_tec = 'share_solar_wind',
	#                                                      mode = factor(c(1,2,3,4)),
	#                                                      commodity = 'electricity',
	#                                                      level = 'energy_secondary'),
	#                                           node_share.tmp ) %>%
	#   dplyr::select(shares,node_share,node,type_tec,mode,commodity,level)
	#
	# share_commodity_lo.par = crossing(data.frame(shares = 'solar_wind_electricity',
	#                                              node_share = node_share.tmp$node_share,
	#                                              unit = '%'),
	#                                   crossing(data.frame(year_all = as.factor(c(2020, 2030, 2040, 2050, 2060)),
	#                                                       value =    c(0.1,  0.2,  0.2,  0.3,  0.3)),
	#                                            time = time) ) %>%
	#   dplyr::select(shares,node_share,year_all,time,value)

	# phase out of coal and once through tecghnologies
	bound_activity_up.par = rbind( bound_activity_up.par,
								   crossing( crossing(node = node[ node != basin ],
											year_all = c(2030,2040,2050,2060),
											time = as.numeric(time)),
								   crossing(tec = technology.set[grepl('_ot',technology.set)],
												 mode = c(1,2)) %>% 
									  bind_rows(data.frame(tec = "coal_st_ot",mode = c(3,4),stringsAsFactors = F))
								  ) %>% 
									dplyr::select(node,tec,year_all,mode,time) %>% 
									mutate(value = 1e-6))
										
	} #end if	


## Nonrenewable Groundwater extraction
if( GROUNDWAT & sc != baseline0 ){
	
	# Constrain fossil groundwater extraction to pct of baseline0
	pct_bsl =  data.frame( year_all = as.character(c(2020,2030,2040,2050,2060 )), val = c( 0.5, 0.1, 0.1, 0.1, 0 ) ,stringsAsFactors = F) # 2020 to 2060
	
	require( gdxrrw )
	igdx( gams_path ) ## Set based on local machine	
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
								paste( 	'MSGoutput_', baseline0, '.gdx', sep = '' ), sep = '' ), 
								list( name = 'EMISS', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	base_gw.df = rs %>% filter( node %in% bcus, emission == 'groundwater', type_tec == 'all' ) %>% dplyr::select( node, emission, type_tec, year_all, value ) 					
	rm( rs, tmp )
	tmp = crossing(node = node[-25] ,type_year =as.character( c(2020,2030,2040,2050,2060) )) 
	
	gw_bound = tmp %>% left_join(left_join( base_gw.df, pct_bsl ) %>% 
	                               mutate( value = val * value * Beta_water) %>% 
	                               dplyr::select( node, emission, type_tec, year_all, value ) %>% 
	                               rename( type_emission = emission, type_year = year_all ) ) %>%  
	  mutate(type_emission = 'groundwater',type_tec = 'all') %>%  
	  group_by(type_year) %>%  
	  mutate(value = if_else(is.na(value), 0,value)) %>%  
	  mutate(value = if_else(value == 0, max(value),value)) %>% ungroup() 
	
	bound_emission.par = bind_rows( bound_emission.par,  
	                                gw_bound		) 
									
  
} #end if


if(CONSTRAINT_LAND){
  
	## release area constraints
	expand_par = data.frame(year_all = c(1990,2000,2010,2015,2020,2030,2040,2050,2060),
                          mult_ind = c(1   ,1   ,1   ,1   ,0.1 ,0.18 ,0.18 ,0.18  ,0.19 ) ,
                          mult_pak = c(1   ,1   ,1   ,1   ,0.1 ,0.18 ,0.18, 0.18 ,0.18 ))
						  
	demand.par = demand.par %>% filter(level != 'area') %>% 
		bind_rows(demand.par %>% filter(level == 'area') %>%
        left_join(expand_par) %>% 
		group_by(node,commodity,level,time) %>% 
		mutate(value = if_else(grepl('IND',node) , 0.4 * value , value ) ) %>%
		mutate(value = if_else(grepl('PAK',node) , 0.3 * value , value ) ) %>% 
		ungroup() %>% 
		dplyr::select(-mult_ind, -mult_pak))

	}


if(CHANGE_FOOD_DEMAND){

	mult = data.frame( 	year_all = c(2015,2020,2030,2040,2050,2060),
						mult = c(1,   0.9, 0.8,0.8, 0.7,0.7 ) )
  
	demand.par = demand.par %>% 
		left_join(mult) %>% 
		mutate(value = if_else(level == 'raw', value * mult, value )) %>% 
		dplyr::select(-mult)
  
	}

# import upper bound
# fix electricity import to be lower or equal to the baseline0, not to
if (FIX_ELEC_IMPORT){

	igdx( gams_path ) ## Set based on local machine
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''),
					   paste( 	'MSGoutput_', baseline0, '.gdx', sep = '' ), sep = '' ),
				list( name = 'ACT', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	
	trs_bound = rs %>% 
		filter(tec %in% exp_routes_names) %>% filter(mode == 2) %>% 
		dplyr::select(node,tec,year_all,mode,time,value) %>%
		group_by(node,tec,year_all,mode,time) %>% 
		summarise(value = sum(value)) %>% 
		as.data.frame( )
		
	bound_activity_up.par = rbind( bound_activity_up.par, trs_bound )

	}
	
if (SMART_IRR_WATER){	
# smart irrigation share constraints
	
	perc_red = 50
	
	if (perc_red == 0){} else {
    
		percentages_smart = data.frame(	year_all = as.factor(c(2020, 2030, 2040, 2050, 2060)),
										value = c(0.5 * perc_red,  perc_red,  perc_red, perc_red,  perc_red) /100 )
    # use share equations
    # share on the whole use of freshwater.irrigation_final
    
		shares.set = c( shares.set, 'smart_irr_water' )
    
		type_tec.set = c(type_tec.set,'water_irr_total','water_irr_smart')

		cat_tec.set = cat_tec.set %>% 
			bind_rows( 	data.frame(type_tec = 'water_irr_total', tec = irr_tech_names ),
                        data.frame(type_tec = 'water_irr_smart', tec = irr_tech_names[grepl('smart',irr_tech_names)]) )
    
		node_share.tmp = map_node.set %>% 
			filter(node1 %in% c('AFG','IND','PAK') & !node2 %in% c('AFG','IND','PAK')) %>%
			rename(node_share = node1, node = node2)
    
		map_shares_commodity_total.set = map_shares_commodity_total.set %>% 
			bind_rows(
				crossing(	
					data.frame(	shares = 'smart_irr_water',
								type_tec = 'water_irr_total',
								mode = factor(c(1,2,3,4)),
								commodity = 'freshwater',
								level = 'irrigation_final'),
					node_share.tmp ) %>% 
				dplyr::select(shares,node_share,node,type_tec,mode,commodity,level) 
				)
    
		map_shares_commodity_share.set = map_shares_commodity_share.set %>% 
			bind_rows(
				crossing(	
					data.frame( shares = 'smart_irr_water',
								type_tec = 'water_irr_smart',
								mode = factor(c(1,2,3,4)),
								commodity = 'freshwater',
								level = 'irrigation_final'),
					node_share.tmp ) %>% 
				dplyr::select(shares,node_share,node,type_tec,mode,commodity,level) 
				)
    
		share_commodity_lo.par = share_commodity_lo.par %>% 
			bind_rows(
				crossing( 
					data.frame(	shares = 'smart_irr_water',
								node_share = node_share.tmp$node_share,
								unit = '%'),
					crossing( percentages_smart, time = time) 
					) %>% 
				dplyr::select(shares,node_share,year_all,time,value) 
				)
  
		}

	}

# constraint to keep limit the shre of rainfed agriculture to X%	
type_tec.set = c(type_tec.set,'agri_land_total','agri_land_rainfed')
		
		cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'agri_land_total',
														   tec = c(irr_tech_names,rainfed_crop_names) ),
												data.frame(type_tec = 'agri_land_rainfed',
														   tec = rainfed_crop_names) )
if(RAINFED_LAND){	
	# smart irrigation share constraints
  
	perc_red = 20
	
	if (perc_red == 0){} else {
    
		percentages_smart = data.frame( year_all = as.factor(c(2020, 2030, 2040, 2050, 2060)), value =  perc_red /100 )
		# use share equations
		# share on the whole use of freshwater.irrigation_final
		
		shares.set = c( shares.set, 'rainfed_land' )
		
		node_share.tmp = map_node.set %>% 
			filter(grepl(('AFG_|IND_|PAK_'),node1)) %>%
			rename(node_share = node1, node = node2)
		
		map_shares_commodity_total.set = map_shares_commodity_total.set %>% 
			bind_rows(
				crossing( 
					data.frame(	shares = 'rainfed_land',
								type_tec = 'agri_land_total',
								mode = factor(c(1,2,3,4)),
								commodity = 'crop_land',
								level = 'area'),
					node_share.tmp ) %>% 
				dplyr::select(shares,node_share,node,type_tec,mode,commodity,level) 
				)
		
		map_shares_commodity_share.set = map_shares_commodity_share.set %>% 
			bind_rows(
				crossing( 
					data.frame(	shares = 'rainfed_land',
								type_tec = 'agri_land_rainfed',
								mode = factor(c(1,2,3,4)),
								commodity = 'crop_land',
								level = 'area'),
					node_share.tmp ) %>% 
			dplyr::select(shares,node_share,node,type_tec,mode,commodity,level) 
			)
		
		share_commodity_up.par = share_commodity_up.par %>% 
			bind_rows(
				crossing(
					data.frame(	shares = 'rainfed_land',
								node_share = node_share.tmp$node_share,
								unit = '%'),
					crossing( percentages_smart, time = time ) ) %>% 
			dplyr::select(shares,node_share,year_all,time,value) 
			)
		
		}
  
	}	
	
# minimum activity for crops and power plants in 2020, based on historical capacity

tmp1 = historical_new_capacity.par %>% filter(year_all < 2020) %>%
  filter(grepl('crop|irr_flood|rainfed',tec)) %>% filter(!grepl('fruit',tec)) %>%
  group_by(node,tec,year_all) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  filter( !(grepl('AFG',node) & grepl('vegetables',tec)) )

if (!HIST_AGRICULTURE){ year_pol = c(2030,2040,2050)
# in 2020 70% match with historical
bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
                                    crossing(tmp1  %>%
                                               select(-year_all),
                                             year_all = 2020) %>%
                                      mutate(value = 0.7 * value) %>%
                                      select(node,tec,year_all,value)  )

#after 2020 50% match with historical location, but irrigation technology can change
bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
                                    crossing(tmp1 %>% filter( !grepl('irr_flood',tec) ) %>%
                                               select(-year_all),
                                             year_all = year_pol) %>%
                                      mutate(value = 0.5 * value) %>%
                                      select(node,tec,year_all,value)  )



} else { year_pol = c(2020,2030,2040,2050)

bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
                                    crossing(tmp1 %>% select(-year_all),
                                             year_all = year_pol) %>%
                                      select(node,tec,year_all,value)  )}

if(FULL_COOPERATION){
		  
		  # demand and food production at basin level, not countrty level
		  demand.par = demand.par %>% filter(level != 'raw') %>%
		    bind_rows(	demand.par %>% filter(level == 'raw') %>%
		                 group_by(commodity,level,year_all,time) %>%
		                 summarize(value = sum(value)) %>% as.data.frame() %>%
		                 mutate(node = 'Indus') %>%
		                 dplyr::select(node,commodity,level,year_all,time,value) )

		  output.par = output.par %>% filter(level != 'raw') %>%
		    bind_rows(	output.par %>% filter(level == 'raw') %>%
		                 group_by(node,tec,vintage,year_all,mode,commodity,level,time,time_out) %>%
		                 summarize(value = sum(value)) %>% ungroup() %>%
		                 mutate(node_out = 'Indus') %>%
		                 dplyr::select(node,tec,vintage,year_all,mode,node_out,commodity,level,time,time_out,value) )
		  
		  #cooperative electricity - remove the hurdle rates
		  var_cost.par = var_cost.par %>% mutate(value = if_else(grepl('trs_',tec) & !(tec %in% exp_routes_names)
		                                                         , 0 , value ) )
		  
	} #end if
		

## Policies specific for one scenario ####
# in this way we do not add several policy options

if(sc == 'irr_Nexus'){
  
	# demand and food production at basin level, not countrty level
	demand.par = demand.par %>% filter(level != 'raw') %>% 
		bind_rows(	demand.par %>% filter(level == 'raw') %>% 
					group_by(commodity,level,year_all,time) %>% 
					summarize(value = sum(value)) %>% as.data.frame() %>% 
					mutate(node = 'Indus') %>% 
					dplyr::select(node,commodity,level,year_all,time,value) )
  
	output.par = output.par %>% filter(level != 'raw') %>% 
		bind_rows(output.par %>% filter(level == 'raw') %>% 
                group_by(node,tec,vintage,year_all,mode,commodity,level,time,time_out) %>% 
                summarize(value = sum(value)) %>% ungroup() %>% 
                mutate(node_out = 'Indus') %>% 
				dplyr::select(node,tec,vintage,year_all,mode,node_out,commodity,level,time,time_out,value) )
  
	# crop production level remains constant
	demand.par = demand.par %>%  
		group_by(node,commodity,level,time) %>% 
		mutate(value = if_else(level == 'raw', first(value), value )) %>% 
		ungroup()
  
	#cooperative electricity trading - remove the hurdle rates
	var_cost.par = var_cost.par %>% mutate(value = if_else(grepl('trs_',tec), 0 , value ) )
  
	} # end if

if(grepl('extreme|glacier',sc)){
  
  # bound_activity_lo.par = bound_activity_lo.par %>% filter(!grepl('canal|lining|river',tec))
  
  bound_storage_lo.par = bound_storage_lo.par %>% mutate(value = 0)
  
  bound_total_capacity_lo.par = bound_total_capacity_lo.par %>% filter(tec == 'interbasin_canal' | year_all == '2020')
  
  require( gdxrrw )
  igdx( gams_path ) ## Set based on local machine	
  tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
                     paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ), 
              list( name = 'CAP', form = "sparse" ) )
  names(tmp$uels) = tmp$domains
  rs = data.frame( tmp$val )
  names(rs) = c( unlist( tmp$domains ), 'value' )
  rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
  base_gw.df = rs %>% filter( node %in% bcus, tec == 'irrigation_gw_diversion' ) %>% dplyr::select( node, tec, year_all, value ) 					
  rm( rs, tmp )
  
  gw_extract_agri_cap.df = base_gw.df %>%
    filter(node != 'PAK_13') %>% 
    group_by(node,tec,year_all) %>% summarise(value = sum(value) * mean(frc)) %>% ungroup()
  bound_total_capacity_up.par = bound_total_capacity_up.par %>% rbind(
    gw_extract_agri_cap.df
  )
}
