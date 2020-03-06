

### UNITS
# Costs are in million USD2010 per year
# Water flows in million cubic meters per day (MCM/day)
# power flows in MW 
# land in Mha (million hectares)
# yields are in kton
# Note that most parameters are defined as rates as opposed to absolute volumes
# This avoids some of the headache associated with unequal timesteps (i.e., months)

# Power plant cost and performance data from Parkinson et al. (2016) - Impacts of groundwater constraints on Saudi Arabia's low-carbon electricity supply strategy
# Additional power plant technologies not included in Parkinson et al. (2016) are estimated from Black & Veatch (2012) "Cost and performance data for Power generation"
# Emission factors in metric tons per day per MW are default IPCC numbers or matched to MESSAGE IAM
# Fuel usage rates are estimated from the power plant heat rate
# flexibility rates for electricity technologies from Sullivan et al. (2013)

# Gas combined cycle with once through freshwater cooling

msg_tec = 'gas_cc' # tec name in MESSAGE IAM
vtgs = year_all
nds = bcus
lft = 20
dCF = 0.9 # capacity factor drop in flexibility mode, modelled as output decrease and var_cost increase actually (1-(CF0-CF))
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_cc_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0.00096 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round( 0.00096 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00094 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00094 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 1.88 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 2.00 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.0002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),
						
						
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.023	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_cc_ot",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# Gas combined cycle with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_cc_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0.00002 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round( 0.00002 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 1.92 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 2.04 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 2.04 / 1.92 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ), 						
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.064	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.92 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 1.92 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_cc_cl",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
				
# Gas combined cycle with air cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_cc_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 2.06 / 1.94 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 1.94 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 2.06 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 2.06 / 1.94 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),	
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.105	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.017	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.94 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 2.06 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_cc_ac",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
# Gas combined cycle with once-through sea cooling
vtgs = year_all
nds = coast_pid
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_cc_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 1.88 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 1.94 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.023	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.88 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 1.94 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_cc_sw",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
# Gas turbine
vtgs = year_all
msg_tec = 'gas_ct'
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_gt = list( 		nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 2.86 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.03 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( ., gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( ., emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.088 , digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.102 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
											
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_gt",]

					)

# Gas single cycle with once through freshwater cooling
vtgs = year_all
msg_tec = 'gas_ppl'
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_st_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00319 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00319 * 3.37 / 3.18, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00315 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00315 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00004 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00004 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.159	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_st_ot",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
# Gas single cycle with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_st_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00006 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00006 * 3.43 / 3.23, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00001 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00001 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.23 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.43 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00005 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00005 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),								
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.205	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.017	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.43/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_st_cl",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# Gas single cycle with air cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gas_st_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0 * 3.74 / 3.53, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.53 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.74 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.251	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.018	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.53/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.74/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_st_ac",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
# Gas single cycle with once-through sea cooling
vtgs = year_all
nds = coast_pid
gas_st_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0561 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),								
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.159	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.18/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "gas") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.37/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "gas_st_sw",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# oil combined cycle with once through freshwater cooling
vtgs = year_all
msg_tec = 'loil_cc'
nds = bcus
oil_cc_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0.00096 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round( 0.00096 * ( 2.00/1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00094 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00094 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 1.88 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 2.00 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),
						
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.023	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_cc_ot",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)

# oil combined cycle with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
oil_cc_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0.00002 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round( 0.00002 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 1.92 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 2.04 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 2.00 / 1.88 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.064	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.92 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 1.92 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_cc_cl",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)

# oil combined cycle with air cooling
vtgs = year_all
nds = bcus
oil_cc_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 2.06 / 1.94 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 1.94 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 2.06 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),					
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.105	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.017	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.94 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 2.06 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_cc_ac",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)

# oil combined cycle with once-through sea cooling
vtgs = year_all
nds = coast_pid
oil_cc_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 1.88 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.07333 * 1.94 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.023	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.026 * ( 1.88 / 1.88 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.046 * ( 1.94 / 1.88 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_cc_sw",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)

# Oil turbine
vtgs = year_all
msg_tec = 'loil_ppl'
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
oil_gt = list( 		nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )													
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 2.86 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.03 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),
						
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.088 , digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.102 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
											
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_gt",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 10	)

					)

# Oil single cycle with once through freshwater cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
oil_st_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00319 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00319 * 3.37 / 3.18, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00315 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00315 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00004 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00004 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),						
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.159	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_st_ot",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)
					
# Oil single cycle with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
oil_st_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00006 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00006 * 3.43 / 3.23, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00001 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00001 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.23 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.43 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00005 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00005 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.205	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.017	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.43/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_st_cl",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)

# Oil single cycle with air cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
oil_st_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0 * 3.74 / 3.53, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.53 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.74 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.251	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.018	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.53/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.74/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_st_ac",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)
# Oil single cycle with once-through sea cooling
vtgs = year_all
nds = coast_pid
oil_st_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0733 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.159	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.18/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.37/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "oil_st_sw",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 1e-6	) %>% 
					  mutate(value = if_else(!grepl('PAK',node),value,
					                         if_else(year_all %in% c(2020,2030),50,value ) ) )

					)
							
# pulverized coal with once through freshwater cooling
vtgs = year_all
msg_tec = 'coal_ppl'
nds = bcus
coal_st_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2, 3, 4 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00318 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00318 * 3.37 / 3.18, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(3,4), 
																	commodity = 'biomass', 
																	level = 'solid',
																	value = round(1/ 0.31 * 0.2 * 3600 * 15 * 1e-6 , digits = 5) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00316 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00316 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
														
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 3, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 4, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,3), 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(2,4), 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2,3,4),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.60	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.023	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(1,3) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(2,4) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.43/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "coal_st_ot",]
					
					# data.frame(node,inv_tec,year)
					# growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
			
# pulverized coal with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
coal_st_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2, 3, 4 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00006 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00006 * 3.43 / 3.23, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(3,4), 
																	commodity = 'biomass', 
																	level = 'solid',
																	value = round(1/ 0.31 * 0.2 * 3600 * 15 * 1e-6 , digits = 5) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )	
						
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00001 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00001 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.23 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.43 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 3, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.23 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 4, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.43 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),													

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,3), 
																			emission = 'water_consumption', 
																			value = 0.00005 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(2,4), 
																			emission = 'water_consumption', 
																			value = round( 0.00005 * ( 3.43 / 3.23 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2,3,4),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.860	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.024	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(1,3) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(2,4) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.053 * (3.43/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "coal_st_cl",]
					
					# data.frame(node,inv_tec,year)
					# growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# Pilverized coal with air cooling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
coal_st_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2, 3, 4 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0 * 3.74 / 3.53, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(3,4), 
																	commodity = 'biomass', 
																	level = 'solid',
																	value = round(1/ 0.31 * 0.2 * 3600 * 15 * 1e-6 , digits = 5) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.53 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.74 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 3, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.53 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 4, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.74 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,3), 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(2,4), 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.74 / 3.53 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2,3,4),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 7.145	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.025	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(1,3) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.53/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(2,4) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.056 * (3.74/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "coal_st_ac",]
					
					# data.frame(node,inv_tec,year)
					# growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
# Pulveized coal with once-through sea cooling
vtgs = year_all
nds = coast_pid
coal_st_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,3), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(2,4), 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

														left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 3, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.18 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 4, 
																			emission = 'CO2', 
																			value = round( 0.8 * 0.0961 * 3.37 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,3), 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(2,4), 
																			emission = 'water_consumption', 
																			value = round( 0 * ( 3.37 / 3.18 ), digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2,3,4),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.600	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.023	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(1,3) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.035 * ( 3.18/3.18 ), digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = c(2,4) ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.056 * (3.37/3.18 ) / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "coal_st_sw",]
					
					# data.frame(node,inv_tec,year)
					# growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# integrated gasification with once through freshwater cooling
vtgs = year_all
nds = bcus
msg_tec = 'igcc'
igcc_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00136 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00136 * 2.83 / 2.65, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00135 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00135 * 2.83 / 2.65, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.65 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.83 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00001 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00001 * 2.83 / 2.65, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) ,

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 4.010	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.031	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.057, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.077 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "igcc_ot",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)

# integrated gasifcation with closed-loop freshwater cooling
vtgs = year_all
nds = bcus
igcc_cl =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00004 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00004 * 2.89 / 2.71, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00002 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00002 * 2.89 / 2.71, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.71 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.89 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * 2.89 / 2.71, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 4.131	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.032	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.057, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.077 * 2.89 / 2.71 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "igcc_cl",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
						
# integrated gasification with air cooling
vtgs = year_all
nds = bcus
igcc_ac =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.95 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 3.07 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 5.105	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.037	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.057 * 2.95 / 2.71, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.077 * 3.07 / 2.71 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "igcc_ac",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
# integrated gasification combined cycle with once-through sea cooling
vtgs = year_all
nds = coast_pid
igcc_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.65 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0.0961 * 2.83 * 60 * 60 * 24 / 1e3, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),

													# Other air pollutant emissions
													left_join( air_pollution_factors.df %>% 
																filter( tec == msg_tec ) %>% 
																expand( node = nds, 
																		vintage = vtgs,
																		mode = c(1,2),
																		nesting(emission,val) ) %>% 
																ungroup() %>% data.frame() %>% rename( value = val ), 
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )
														
						) %>% # aggregate CO2eq emissions
						left_join( . , gwp.df ) %>% mutate( CO2eq = value * gwp ) %>% dplyr::select( -gwp ) %>%
						bind_rows( ., gather( . , emission, value, -node, -vintage, -year_all, -mode, -emission,   -value, CO2eq) ) %>%
						group_by( node,  vintage, year_all, mode, emission ) %>% summarise( value = sum( value, na.rm=TRUE ) ) %>%
						ungroup( ) %>% data.frame ( ) %>%
						dplyr::select( node,  vintage, year_all, mode, emission, value ),									
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 4.010	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.031	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.057, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) ,
														
											left_join(  left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 2 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "coal") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.077 / dCF, digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ) 
											
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "igcc_sw",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.5	)

					)
					
# nuclear with once through freshwater cooling
vtgs = year_all
nds = bcus[!grepl('AFG',bcus)]
nuclear_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00424 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00433, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00420 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00420, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00004 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00013, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 5.530	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.093	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.018	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = round( 0.036 / dCF, digits = 5 ) ),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),					
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "nuclear_ot",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1e-6	)

					)
					
# nuclear with closed-loop freshwater cooling
vtgs = year_all
nds = bcus[!grepl('AFG',bcus)]
nuclear_cl =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00014 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00020, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00001 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00001, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00013 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00019, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 5.751	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.097	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.021	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = round( 0.039 / dCF, digits = 5 ) ),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),						
														
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "nuclear_cl",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1e-6	)

					)
				
# nuclear with once-through sea cooling
vtgs = year_all
nds = coast_pid[grepl('PAK',coast_pid)]
nuclear_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 5.530	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.093	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.018	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = round( 0.036 / dCF, digits = 5 ) ),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),	
												
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "nuclear_sw",],
					
					# data.frame(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1e-6	)

					)
					
# geothermal with once-through freshwater
vtgs = year_all
nds = bcus
geothermal_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00424 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00433, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00420 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00420, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00004 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00013, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.243	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.132	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.018	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "geothermal_ot",],
					
					# bound_new_capacity_up(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030), value = 5	),
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.05	)


					)

# geothermal with closed loop cooling
vtgs = year_all
nds = bcus
geothermal_cl =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00014 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00023, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.0010 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.0010, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00004 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00013, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.343	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.135	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.018	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "geothermal_cl",],
					
					# bound_new_capacity_up(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030), value = 5	),
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.05	)

					)

# geothermal with once-through sea cooling
vtgs = year_all
nds = coast_pid
geothermal_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.15 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 6.243	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.132	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.018	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "geothermal_sw",],
					
					# bound_new_capacity_up(node,inv_tec,year)
					bound_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030), value = 5	),
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2040,2050,2060), value = 0.05	)


					)

# Biomass steam plant with once-through freshwater cooling
vtgs = year_all
nds = bcus
biomass_st_ot = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00318 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00318 * 3.63 / 3.45, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.00316 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.00316 * 3.63 / 3.45, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.3 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00002 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00002 * 3.63 / 3.45, digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.830	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.095	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.118	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.130	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "biomass_st_ot",]

					)


# Biomass steam plant with closed loop cooling
vtgs = year_all
nds = bcus
biomass_st_cl = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0.00018 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0.00023, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.0005 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0.0005, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.3 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.00013 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0.00018 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.930	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.096	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.120	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.135	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "biomass_st_cl",]

					)					

# Biomass steam plant with air cooling
vtgs = year_all
nds = bcus
biomass_st_ac = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = round(  0, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = round( 0, digits = 5 ) ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),

										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.3 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),	

													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'water_consumption', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 4.130	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.098	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.125	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.140),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "biomass_st_ac",]

					)
					
# Biomass plant with once-through sea cooling
vtgs = year_all
nds = coast_pid
biomass_st_sw = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1, 2 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.3 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 2, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.930	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.096	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.120	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) ,
															
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 2,
																		time = time,
																		value = 0.135	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.5	),
														vtg_year ) %>% 
					  mutate(value = if_else(vintage < 2030 , 1,value) ) %>% 
					  dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "biomass_st_sw",]

					)
					
# solar PV (utility-scale)
vtgs = year_all
nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "solar_1"]))
solar_pv_1 = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = -0.05 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'solar_credit', 
																			value = -1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "solar_1",],
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.873	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		value = 0.003	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value )
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[ hist_new_cap.df$tec == "solar_pv_1", ],

					# bound_total_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = max_potential_sw.df %>% 
												filter( tec == 'solar_1' ) %>% 
												expand( data.frame( node, value  ), year_all ) %>%
												dplyr::select( node, year_all, value )
					
					)

nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "solar_2"]))
solar_pv_2 = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = -0.05 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'solar_credit', 
																			value = -1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "solar_2",],
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.873	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.003	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value )
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "solar_pv_2",],

					# bound_total_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = max_potential_sw.df %>% 
												filter( tec == 'solar_2' ) %>% 
												expand( data.frame( node, value  ), year_all ) %>%
												dplyr::select( node, year_all, value )
					
					)
					
nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "solar_3"]))
solar_pv_3 = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value =  0 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = -0.05 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'solar_credit', 
																			value = -1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "solar_3",],
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.873	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.003	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value )
															
											),							
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "solar_pv_3",],

					# bound_total_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = max_potential_sw.df %>% 
												filter( tec == 'solar_3' ) %>% 
												expand( data.frame( node, value  ), year_all ) %>%
												dplyr::select( node, year_all, value )
					
					)
					
					
# Wind (utility-scale)
vtgs = year_all
nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "wind_1"]))
wind_1 = list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'energy_secondary',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'river_out',
																value = 0 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.08 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'water_consumption', 
																		value = 0 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'wind_credit', 
																		value = -1 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

					),										
																
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "wind_1",],
												vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 5	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 7.000	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.008	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1,
																	time = time,
																	value = 0.000	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value )
														
										),							
													
												
				# data.frame(node,vintage,year_all,value)
				min_utilization_factor = left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0	),
													vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
				
				# data.frame(node,year_all,value)
				historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "wind_1",],

				# bound_total_capacity_up(node,inv_tec,year)
				bound_total_capacity_up = max_potential_sw.df %>% 
												filter( tec == 'wind_1' ) %>% 
												expand( data.frame( node, value  ), year_all ) %>%
												dplyr::select( node, year_all, value )
				
				)

nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "wind_2"]))
wind_2 = list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'energy_secondary',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'river_out',
																value = 0 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.08 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'water_consumption', 
																		value = 0 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'wind_credit', 
																		value = -1 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

					),										
																
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "wind_2",],
												vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 5	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 7.000	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.008	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1,
																	time = time,
																	value = 0.000	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value )
														
										),							
													
												
				# data.frame(node,vintage,year_all,value)
				min_utilization_factor = left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0	),
													vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
				
				# data.frame(node,year_all,value)
				historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "wind_2",],

				# bound_total_capacity_up(node,inv_tec,year)
				bound_total_capacity_up = max_potential_sw.df %>% 
											filter( tec == 'wind_2' ) %>% 
											expand( data.frame( node, value  ), year_all ) %>%
											dplyr::select( node, year_all, value )
				
				)

nds = as.character(unique(capacity_factor_sw.df$node[capacity_factor_sw.df$tec == "wind_3"]))
wind_3 = list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'energy_secondary',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'freshwater', 
																level = 'river_out',
																value = 0 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.08 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'water_consumption', 
																		value = 0 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
												
												left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'wind_credit', 
																		value = -1 ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

					),										
																
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	capacity_factor_sw.df[capacity_factor_sw.df$tec == "wind_3",],
												vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 7.000	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.008	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1,
																	time = time,
																	value = 0.000	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value )
														
										),							
													
												
				# data.frame(node,vintage,year_all,value)
				min_utilization_factor = left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0	),
													vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
				
				# data.frame(node,year_all,value)
				historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "wind_3",],

				# bound_total_capacity_up(node,inv_tec,year)
				bound_total_capacity_up = max_potential_sw.df %>% 
												filter( tec == 'wind_3' ) %>% 
												expand( data.frame( node, value  ), year_all ) %>%
												dplyr::select( node, year_all, value )
				
				)

# Old hydropower 
vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = as.character( water_resources.df$node[ which( water_resources.df$existing_MW > 0 | water_resources.df$planned_MW > 0 ) ] )

bound_total_capacity_lo_hydro = water_resources.df %>% select(node,existing_MW,planned_MW) %>% 
  tidyr::crossing(year_all = year_all[ year_all > baseyear ]) %>% 
  mutate(value = if_else(year_all == 2020, existing_MW, existing_MW + planned_MW)) %>% 
  filter(value != 0) %>% dplyr::select(node,year_all,value)

hydro_old = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,2), 
																	commodity = 'hydro_potential_old', 
																	level = 'energy_secondary',
																	value =  1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
										
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
													
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value = 1 * dCF ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
																
										left_join(   expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'flexibility', 
																	level = 'energy_secondary',
																	value = 0.5 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
						
						),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,2), 
																			emission = 'CO2', 
																			value = round( 0 , digits = 5 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.95 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 5	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.015	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1,
																		time = time,
																		value = 0.000	),
														vtg_year_time ) %>% 
															dplyr::select( node, vintage, year_all, mode, time, value ) 
															
											),							
												
					# Bound total capacity based on existing and planned MW
					# data.frame(node,year_all,value)
					# bound_total_capacity_lo = bound_total_capacity_lo_hydro,
					
					# Upper bound equal to lower bound to prevent building in future years beyond planned - 
					# add additional 0.1 % to allow for some room beteweeen bounds for optimization efficiency
					bound_total_capacity_up = bound_total_capacity_lo_hydro %>% mutate( value = value * 1.001 ),		
					
					# data.frame(node,year_all) 
					historical_new_capacity = hist_new_cap.df %>%
						filter( tec == 'hydro')	
					
					)


# New hydropower - river technologies, highly dependent on upstream flow_routes
nds = as.character( water_resources.df$node[ which( water_resources.df$river_max_mw > 0 ) ] )
vtgs = year_all[ year_all > baseyear ]
hydro_river = list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1,2 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1,2), 
																commodity = 'hydro_potential_river', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 2, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 * dCF ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
															
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 2, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = 0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = c(1,2), 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
																
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.95 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 10	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 50	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 5	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.015	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,2),
																	time = time,
																	value = 0.000	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value )
														
										),							
											
				# Bound total capacity based on existing and planned MW
				# data.frame(node,year_all,value)
				bound_total_capacity_up = do.call( rbind, lapply( nds, function( nn ){ 
						data.frame( node = rep( nn, length( vtgs ) ), 
									year_all = vtgs, 
									value = rep( ( water_resources.df$river_max_mw[ which( water_resources.df$node ==  nn ) ] ), length( vtgs ) ) ) 
						} ) )
				
				)

# New hydropower - canal technologies, independent of upstream flow  				
nds = as.character( water_resources.df$node[ which( water_resources.df$canal_max_mw > 0 ) ] )
vtgs = year_all[ year_all > baseyear ]
hydro_canal = list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1, 2 ),
				lifetime = lft,
				
				input = NULL,
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 2, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 * dCF ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
															
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 2, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = 0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = c(1,2), 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				# Canal plant capacity factor is determined based on internal run off
				capacity_factor = left_join( bind_rows( lapply( nds, function(nn){ do.call( rbind, lapply( vtgs, function( yy ){ 
					
												data.frame( node = rep( nn, length(time) ),
															year_all = rep( yy, length(time) ),
															time = time,
															value = unlist( sapply( unlist( water_resources.df[ which( as.character( water_resources.df$node ) == nn ), grepl( paste0( 'runoff_km3_per_day_', yy ), names(water_resources.df) )  ] ) *
																					c( water_resources.df[ which( as.character( water_resources.df$node ) == nn ), 'canal_mw_per_km3_per_day' ] ) / 
																					c( water_resources.df[ which( as.character( water_resources.df$node ) == nn ), 'canal_max_mw' ] ), function(iii){ min( 1, round( iii, digits = 3) ) } ) ),
																					row.names = 1:length(time)  )
																					
												} ) ) } ) ),
												vtg_year ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 5	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.015	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,2),
																	time = time,
																	value = 0.000	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),							
											
				# Bound total capacity based on existing and planned MW
				# data.frame(node,year_all,value)
				bound_total_capacity_up = do.call( rbind, lapply( nds, function( nn ){ 
						data.frame( node = rep( nn, length( vtgs ) ), 
									year_all = vtgs, 
									value = rep( ( water_resources.df$river_max_mw[ which( water_resources.df$node ==  nn ) ] ), length( vtgs ) ) ) 
						} ) )
				
				)

# electricity_distribution_urban - generation to end-use within the same spatial unit
vtgs = year_all
nds = bcus
tmp = demand.df %>% filter(commodity == 'electricity' & level == 'urban_final' & year_all == 2015) %>% 
		mutate(year_all = as.numeric(year_all)) %>% 
		group_by(node,year_all,units) %>% 
		summarise(value = sum(value)) %>% 
		mutate(tec = 'electricity_distribution_urban') %>% 
		dplyr::select(node,tec,year_all,value) %>% ungroup() %>% 
		mutate(value = 0.5 * value)
electricity_distribution_urban =  
		list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'urban_final',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 1.12	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.036	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	time = time,
																	value = 0.025	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = tmp %>% 
											bind_rows(tmp %>% mutate(year_all = 2010))						
				
				)
				
# electricity_distribution_industry - generation to end-use within the same spatial unit
vtgs = year_all
nds = bcus
tmp = demand.df %>% filter(commodity == 'electricity' & level == 'industry_final' & year_all == 2015) %>% 
		mutate(year_all = as.numeric(year_all)) %>% 
		group_by(node,year_all,units) %>% 
		summarise(value = sum(value)) %>% 
		mutate(tec = 'electricity_distribution_industry') %>% 
		dplyr::select(node,tec,year_all,value) %>% ungroup() %>% 
		mutate(value = 0.5 * value)				
electricity_distribution_industry =  
		list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'industry_final',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 1.12	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.036	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	time = time,
																	value = 0.025	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = tmp %>% 
											bind_rows(tmp %>% mutate(year_all = 2010))						
				
				)				

# electricity_distribution_rural - generation to end-use within the same spatial unit
vtgs = year_all
nds = bcus
tmp = demand.df %>% filter(commodity == 'electricity' & level == 'rural_final' & year_all == 2015) %>% 
		mutate(year_all = as.numeric(year_all)) %>% 
		group_by(node,year_all,units) %>% 
		summarise(value = sum(value)) %>% 
		mutate(tec = 'electricity_distribution_rural') %>% 
		dplyr::select(node,tec,year_all,value) %>% ungroup() %>% 
		mutate(value = 0.5 * value)
electricity_distribution_rural =  
		list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'rural_final',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 1.12	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.036	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	time = time,
																	value = 0.025	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = tmp %>% 
											bind_rows(tmp %>% mutate(year_all = 2010))						
				
				)
				
# electricity_distribution_irrigation - generation to end-use within the same spatial unit
vtgs = year_all
nds = bcus
tmp = demand.df %>% filter(commodity == 'electricity' & level == 'irrigation_final' & year_all == 2015) %>% 
	mutate(year_all = as.numeric(year_all)) %>% 
	group_by(node,year_all,units) %>% 
	summarise(value = sum(value)) %>% 
	mutate(tec = 'electricity_distribution_irrigation') %>% 
	dplyr::select(node,tec,year_all,value) %>% ungroup() %>% 
	mutate(value = 0.5 * value)
electricity_distribution_irrigation = 
		list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = -0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 1.12	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.036	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	time = time,
																	value = 0.025	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value )
														
										),
										
				historical_new_capacity = tmp %>% 
											bind_rows(tmp %>% mutate(year_all = 2010))						
				
				)
				
# electricity_shtort term storage
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
electricity_short_strg = 
		list( 	nodes = nds,
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  0.2 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									
					),
					
				output = bind_rows( left_join(   expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 20	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 3.000	),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.016	),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	time = time,
																	value = 0.015	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = NULL						
				
				)

# Electricity transmission
vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
for( jjj in 1:length(adjacent_routes) ){ # add

	routes_names <- paste0('trs_',adjacent_routes)
	nds = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1]
	assign( routes_names[jjj], 
		
		list( 	nodes = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1,2 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																node_in = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),

									left_join(  expand.grid( 	node = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																node_in = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[2],
																vintage = vtgs,
																mode = c(2), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																node_out = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[2],
																vintage = vtgs,
																mode = 1, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 - trs_eff.df$value[trs_eff.df$tec == adjacent_routes[jjj]] ) ,
												vtg_year_time ) %>% mutate( time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
									left_join(  expand.grid( 	node = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																node_out = unlist( strsplit( adjacent_routes[jjj], '[|]' ) )[1],
																vintage = vtgs,
																mode = 2, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value = 1 - trs_eff.df$value[trs_eff.df$tec == adjacent_routes[jjj]] ) ,
												vtg_year_time ) %>% mutate( time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )			
					
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		emission = 'CO2', 
																		value = round( 0 , digits = 5 ) ) ,
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 5	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = trs_inv_cost.df$value[trs_inv_cost.df$tec == adjacent_routes[jjj]] ),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.05 * trs_inv_cost.df$value[trs_inv_cost.df$tec == adjacent_routes[jjj]] ),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,2),
																	time = time,
																	value = trs_hurdle.df$hurdle[as.character(trs_hurdle.df$tec) == adjacent_routes[jjj]]	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = transmission_routes.df[transmission_routes.df$tec == routes_names[jjj],]					
				
				)
				
				
		) 
  
	}

# externl routes no input or output for countries
#extimate of elec export to Karachi area # 0.791 ratio of population that is not in the basin
# we scale pakistan electricity demand accordingly, then remove 5.7GW * 0.75 (CF) * 30*24 /12
elec_exp =demand.df %>% filter(grepl('PAK',node), commodity == 'electricity') %>% 
  mutate(value = value * (1 - 0.791) / 0.791 ) %>% #in MW month
  group_by(level,year_all,time) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  #subtract external generation capacity to esternal industry demand
  mutate(value = if_else(level == 'industry_final',value - (5700/12), value)) %>% 
  group_by(year_all,time) %>% summarise(value = sum(value))

vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
trs_hurdle.df$hurdle[trs_hurdle.df$tec == 'PAK_4|PAK'] = 0
for( jjj in 1:length(electricity_export_routes) ){ # add
	
	exp_routes_names <- paste0('trs_',electricity_export_routes)
	nds = unlist( strsplit( electricity_export_routes[jjj], '[|]' ) )[1]
	
	if (electricity_export_routes[jjj] == "PAK_4|PAK"){ 
	  tmp_bound_act_lo =  elec_exp %>% 
	    crossing(node = "PAK_4",
	             mode = 1) %>% 
	    select(node,year_all,mode,time,value)
	  
	  tmp_bound_act_up =  crossing(node = "PAK_4",
	             mode = c('2'),
	             year_all = as.character(year_all),
	             time = time,
	             value =  1e-6) %>% 
	    select(node,year_all,mode,time,value)
	  
	} else { 
	  tmp_bound_act_lo =  NULL 
	  tmp_bound_act_up = NULL
	}
	
	assign( exp_routes_names[jjj], 
	
		list( 	nodes = unlist( strsplit( electricity_export_routes[jjj], '[|]' ) )[1],
				years = year_all,
				times = time,
				vintages = vtgs,	
				types = c('power'),
				modes = c( 1,2 ),
				lifetime = lft,
				
				input = bind_rows(  left_join(  expand.grid( 	node = unlist( strsplit( electricity_export_routes[jjj], '[|]' ) )[1],
																vintage = vtgs,
																mode = c(1), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 /  ( 1 - trs_eff.df$value[trs_eff.df$tec == adjacent_routes[jjj]] ) ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
					),
					
				output = bind_rows( left_join(  expand.grid( 	node = unlist( strsplit( electricity_export_routes[jjj], '[|]' ) )[1],
																vintage = vtgs,
																mode = 2, 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  1 - trs_eff.df$value[trs_eff.df$tec == adjacent_routes[jjj]] ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
														
					),	

				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																		vintage = vtgs,
																		mode = 2, 
																		emission = 'CO2', 
																		value = round( 0.0561 * 1.88 * 60 * 60 * 24 / 1e3, digits = 5 ) ) , # test with ccgt ef
													vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

					),										
										
				# data.frame( value ) or data,frame( node, year_all, time, value )
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 5	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = trs_inv_cost.df$value[trs_inv_cost.df$tec == electricity_export_routes[jjj]] ),
				
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0.05 * trs_inv_cost.df$value[trs_inv_cost.df$tec == electricity_export_routes[jjj]] ),
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = bind_rows( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c(1,2),
																	time = time,
																	value = trs_hurdle.df$hurdle[ as.character(trs_hurdle.df$tec) == electricity_export_routes[jjj] ]	),
													vtg_year_time ) %>% 
														dplyr::select( node, vintage, year_all, mode, time, value ) 
														
										),
										
				historical_new_capacity = transmission_routes.df[transmission_routes.df$tec == exp_routes_names[jjj],] %>% mutate( value = 0.1 * value ),
				
				# growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 0.0005	),
				
				bound_activity_lo = tmp_bound_act_lo,
				
				bound_activity_up = tmp_bound_act_up
				
			)
		)
  
	}

#### Water technologies

# sw_extract - generalized surface water extraction technology 	
# Note that for the sw_extract and gw_extract, no costs are included because these technologies are just aggregating the sectoral withdrawals
# which makes it easier to constrain based on historical capacity			
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
sw_extract = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input =  bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_in',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
								),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_div',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)

# gw_extract					
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
gw_extract = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'aquifer',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = c('groundwater','water_consumption'), 
																			value = 1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
					
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
renew_gw_extract = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('water'),
						modes = c( 1 ),
						lifetime = lft,
						
						input = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'renewable_gw', 
																		level = 'aquifer',
																		value = 1 ) ,
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
									),
							
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'freshwater', 
																		level = 'aquifer',
																		value = 1 ) ,
														vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									),	

						# data.frame( node,vintage,year_all,mode,emission) 
						emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1, 
																				emission = 'groundwater', 
																				value = 0 ) ,
															vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 

							),										
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 1 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = 0	),
						
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0	),
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = NULL,							
								
						# data.frame(node,year_all,value)
						historical_new_capacity = NULL

						)					
									
# urban_sw_diversion 
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 15	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 57	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 3	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,	
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.2	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_sw_diversion",]

					)
				
# urban_gw_diversion 
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  13 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 15	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 20	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 8.5	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,				
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_gw_diversion",]

					)
							
# urban_piped_distribution 
vtgs = year_all
nds = bcus
lft = 50
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_piped_distribution = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
										left_join(  expand.grid( node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'urban_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1013	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 252	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_piped_distribution",]

					)
					
# urban_unimproved_distribution 
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_unimproved_distribution = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'urban_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
								
# urban_wastewater_collection
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_wastewater_collection = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'urban_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'wastewater', 
																	level = 'urban_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 785	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 251	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_wastewater_collection",]

					)
					
	
# urban_wastewater_release
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_wastewater_release = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'urban_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
																	vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
					
# urban_wastewater_treatment
vtgs = year_all
nds = bcus
lft = 25
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_wastewater_treatment = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 25,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  12.5 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'urban_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.9 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 431	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 37	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_wastewater_treatment",]

					)
					
# industry_sw_diversion 
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'industry_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 15	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 57	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 3	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,		
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.2	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_sw_diversion",]

					)
				
# industry_gw_diversion 
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  13 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'industry_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 15	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 20	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 8.5	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,		
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_gw_diversion",]

					)
							
# industry_distribution 
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_distribution = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'industry_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'industry_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
								
# industry_wastewater_collection
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_collection = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'wastewater', 
																	level = 'industry_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_wastewater_collection",]

					)
					
	
# industry_wastewater_release
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_release = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
																	vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
					
# industry_wastewater_treatment
vtgs = year_all
nds = bcus
lft = 25
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_treatment = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 25,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  12.5 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.9 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 431	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 37	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_wastewater_treatment",]

					)					
					
# industry_wastewater_collection
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_collection = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'wastewater', 
																	level = 'industry_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_wastewater_collection",]

					)
					
	
# industry_wastewater_release
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_release = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
																	vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
					
# industry_wastewater_treatment
vtgs = year_all
nds = bcus
lft = 25
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_treatment = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 25,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  12.5 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 0.9 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 431	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 37	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_wastewater_treatment",]

					)					
					
# rural_sw_diversion 
vtgs = year_all
lft = 15
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 15,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'rural_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 57	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 3	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,						
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_sw_diversion",]

					)
				
# rural_gw_diversion 
vtgs = year_all
lft = 15
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 15,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  15 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'rural_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 8.5	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,		
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_gw_diversion",]

					)
					
# rural_piped_distribution 
vtgs = year_all
lft = 15
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_piped_distribution = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'rural_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'rural_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 326	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 18	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_piped_distribution",]

					)
					
# rural_unimproved_distribution 
vtgs = year_all
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_unimproved_distribution = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'rural_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'rural_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_unimproved_distribution",]

					)

# rural_wastewater_collection
vtgs = year_all
lft = 15
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_wastewater_collection = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 15,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'rural_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'wastewater', 
																	level = 'rural_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_wastewater_collection",]

					)
							
# rural_wastewater_release
vtgs = year_all
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_wastewater_release = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'rural_final',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
																	vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_wastewater_release",]

					)
					
					
# rural_wastewater_treatment
as.character( basin.spdf@data$DOWN[ iii ] )
vtgs = year_all
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
rural_wastewater_treatment = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'electricity', 
																	level = 'rural_final',
																	value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'rural_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds, 
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 20	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 759	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 77	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_wastewater_treatment",]

					)
					
# irrigation_sw_diversion - conventional
vtgs = year_all
nds = bcus
lft = 50
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
irrigation_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 0.75 ) , # Average losses for canals from Wu et al. World Bank (2013)
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										# Seepage to groundwater
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'renewable_gw', 
																	level = 'aquifer',
																	value = 0.25 ) , # Assumed inverse of the above
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
											
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 57	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 3	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,		
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 1	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "irrigation_sw_diversion",]

					)
					
# irrigation_sw_diversion - smart
vtgs = year_all
nds = bcus
lft = 50
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
smart_irrigation_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = lft,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 0.75 + 0.03 ) , # Average losses for canals from Wu et al. World Bank (2013) + assumed efficiency increase from smart tech
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										
										# Seepage to groundwater
										left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'renewable_gw', 
																	level = 'aquifer',
																	value = 0.25 - 0.03 ) , # Assumed inverse of the above
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								
								# Electricity flexibility			
								left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value =  6 * 0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
								
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 57 * 1.1	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 3 * 1.1	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)					
			
# irrigation_gw_diversion - conv
vtgs = year_all
nds = bcus
irrigation_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value =  16 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 0.95 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 20	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 8.5	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,	
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "irrigation_gw_diversion",]

					)
					
# irrigation_gw_diversion - smart
vtgs = year_all
nds = bcus
smart_irrigation_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value =  16 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
										left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'flexibility', 
																level = 'energy_secondary',
																value =  16 * 0.1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 20	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 8.5 * 1.1	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1 * 1.1	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,			
					
					min_utilization_factor = left_join( expand.grid( 	node = nds,
					                                                  vintage = vtgs,
					                                                  value = 0.8	),
					                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)					
					
# energy_sw_diversion
vtgs = year_all
nds = bcus
energy_sw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  0 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'river_div',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,	
					
					# min_utilization_factor = left_join( expand.grid( 	node = nds,
					#                                                   vintage = vtgs,
					#                                                   value = 0.8	),
					#                                     vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "energy_sw_diversion",]

					)
					
# energy_gw_diversion 
vtgs = year_all
nds = bcus
energy_gw_diversion = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 1,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'energy_secondary',
																value =  6 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
												
												left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'freshwater', 
																level = 'aquifer',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'energy_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,		
					
					# min_utilization_factor = left_join( expand.grid( 	node = nds,
					#                                                   vintage = vtgs,
					#                                                   value = 0.8	),
					#                                     vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "energy_gw_diversion",]

					)
				
# transfer surface to groundwater - backstop option for meeting flow constraints					
vtgs = year_all
nds = bcus
surface2ground = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('water'),
						modes = c( 1 ),
						lifetime = 1,
						
						input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value =  6 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
													left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'freshwater', 
																	level = 'river_div',
																	value =  1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
										
							),
							
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'freshwater', 
																		level = 'aquifer',
																		value = 1 ) ,
														vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									),	

						# data.frame( node,vintage,year_all,mode,emission) 
						emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1, 
																				emission = 'water_consumption', 
																				value = 0 ) ,
															vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
							),								
																								
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 1 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = 0	),
						
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0	),
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = NULL,							
								
						# data.frame(node,year_all,value)
						historical_new_capacity = NULL

						)


# transfer groundwater to suface water - backstop option for meeting flow constraints
vtgs = year_all
nds = bcus
ground2surface = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('water'),
						modes = c( 1 ),
						lifetime = 1,
						
						input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'electricity', 
																	level = 'energy_secondary',
																	value =  6 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
													left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'freshwater', 
																	level = 'aquifer',
																	value =  1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )			
										
							),
							
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'freshwater', 
																		level = 'river_div',
																		value = 1 ) ,
														vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									),	

						# data.frame( node,vintage,year_all,mode,emission) 
						emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1, 
																				emission = 'water_consumption', 
																				value = 0 ) ,
															vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
							),								
																								
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 1 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 1	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = 0	),
						
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0	),
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = NULL,							
								
						# data.frame(node,year_all,value)
						historical_new_capacity = NULL

						)
			
# urban_desal_seawater 
vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = coast_pid
urban_desal_seawater = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  167 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1650	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 165	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_desal_seawater",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2030,2040,2050,2060), value = 0.5	)

					)
					
# industry_desal_seawater 
vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = coast_pid
industry_desal_seawater = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  167 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )		
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'industry_secondary',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1650	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 165	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_desal_seawater",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2030,2040,2050,2060), value = 0.5	)

					)					
			
# urban_wastewater_recycling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_wastewater_recycling = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  42 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),

										left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'urban_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'urban_secondary',
																	value = 0.8 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.2 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1350	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 99	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_wastewater_recycling",]

					)
					
# industry_wastewater_recycling
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
industry_wastewater_recycling = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'industry_final',
																value =  42 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),

										left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'industry_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'industry_secondary',
																	value = 0.8 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.2 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1350	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 99	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "industry_wastewater_recycling",]

					)					
					
# urban_wastewater_irrigation
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
urban_wastewater_irrigation = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'urban_final',
																value =  80 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),

										left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'urban_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 0.8 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.2 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1350	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 99	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "urban_wastewater_irrigation",]

					)					
					
# rural_wastewater_recycling
vtgs = year_all
nds = bcus
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
rural_wastewater_recycling =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'rural_final',
																value =  42 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),

										left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'wastewater', 
																level = 'rural_secondary',
																value =  1 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'rural_secondary',
																	value = 0.8 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.2 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1350	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 99	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "rural_wastewater_recycling",]

					)
					
					
# irrigation_desal_seawater - reverse osmosis
vtgs = year_all
nds = coast_pid
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
irrigation_desal_seawater =  list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 30,
					
					input = bind_rows(  left_join(  expand.grid( 	node = nds,
																vintage = vtgs,
																mode = c( 1 ), 
																commodity = 'electricity', 
																level = 'irrigation_final',
																value =  167 ) ,
												vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
									
						),
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'irrigation_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'water_consumption', 
																			value = 0.1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1650	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 165	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "irrigation_desal_seawater",],
					
					# data.frame(node,inv_tec,year)
					growth_new_capacity_up = expand.grid( 	node = nds, year_all = c(2030,2040,2050,2060), value = 0.5	)

					)
	
# Distributed diesel gensets for water pumping in agriculture sector
vtgs = year_all
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
irri_diesel_genset = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'irrigation_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0741 * 2.86 * 60 * 60 * 24 / 1e3, digits = 3 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = left_join(  left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.088 , digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ),							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "irri_diesel_genset",]

					)
					
# Distributed diesel gensets for machinery in agriculture sector
vtgs = year_all
lft = 20
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
agri_diesel_genset = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'energy', 
																	level = 'agriculture_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0.0741 * 2.86 * 60 * 60 * 24 / 1e3, digits = 3 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = left_join(  left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1 ),
															vtg_year_time ), 
														fossil_fuel_cost_var %>% 
															filter(commodity == "crudeoil") %>% 
															dplyr::select( year_all, value ) %>% 
															mutate( value = round( (1 + value) * 0.088 , digits = 5 ) )
														) %>% dplyr::select( node,  vintage, year_all, mode, time, value ),							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "agri_diesel_genset",]

					)					
					
# # Distributed solar PV for water pumping in agriculture sector - assume pumping flexible to output (i.e., flexible load)
vtgs = year_all
nds = bcus
agri_pv = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = NULL,
						
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'irrigation_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			emission = 'CO2', 
																			value = round( 0 * 2.86 * 60 * 60 * 24 / 1e3, digits = 3 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
						),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.25 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 3.873	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "agri_pv",]

					)
					
# ##### Network technologies

## Technology to represent environmental flows - movement of water from river_in to river_out within the same PID
vtgs = year_all
nds = bcus
environmental_flow = list( 	nodes = nds,
							years = year_all,
							times = time,
							vintages = vtgs,	
							types = c('water'),
							modes = c( 1 ),
							lifetime = 1,
						
							input = bind_rows(  left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = c( 1 ), 
																	commodity = 'freshwater', 
																	level = 'river_in',
																	value =  1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
										
								),
								
							output = bind_rows( left_join(  expand.grid( 	node = nds,
																			vintage = vtgs,
																			mode = 1, 
																			commodity = 'freshwater', 
																			level = 'river_out',
																			value = 1 - ( 0.6 * 0.1 ) ) ,
															vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
															dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
												
												# left_join(  expand.grid( 	node = nds,
												# 							vintage = vtgs,
												# 							mode = 1, 
												# 							commodity = 'renewable_gw', 
												# 							level = 'aquifer',
												# 							value = 0.6 * 0.1 ) , # test value of 0.6 base flow index; 0.1 represents 10% baseflow as indicator for renewable GW availability from Gleeson and Richter 2017
												# 			vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												# 			dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
								),	

							# data.frame( node,vintage,year_all,mode,emission) 
							emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																					vintage = vtgs,
																					mode = 1, 
																					emission = 'env_flow', 
																					value = 1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ) 
								),								
																									
																			
							# data.frame(node,vintage,year_all,time)
							capacity_factor = left_join( 	expand.grid( 	node = nds,
																			vintage = vtgs,
																			value = 1 ) ,
															vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
							
							# data.frame( vintages, value )
							construction_time = expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0	),
							
							# data.frame( vintages, value )
							technical_lifetime = expand.grid( 	node = nds,
																vintage = vtgs,
																value = 1	),
							
							# data.frame( vintages, value )
							inv_cost = expand.grid( node = nds,
													vintage = vtgs,
													value = 0	),
							
							# data.frame( node, vintages, year_all, value )
							fix_cost = NULL, 
							
							# data.frame( node, vintage, year_all, mode, time, value ) 
							var_cost = NULL,							
									
							# data.frame(node,year_all,value)
							historical_new_capacity = NULL

							)

## Technology to represent internal inflows in the node, move water to the river_in level and assign internal hydropower runoff potential
vtgs = year_all
nds = bcus
initial_nodes = as.character(basin.spdf@data$PID[!basin.spdf@data$PID %in% basin.spdf@data$DOWN])
tmp = bind_rows( lapply( initial_nodes, function( ii ){ data.frame(node = ii,
    qnt = quantile( unlist( water_resources.df[ which( water_resources.df$node == ii), 
                                        grepl( '2015', names(water_resources.df) ) ] ) , 0.75 )
	) } ) )
internal_runoff = list( 	nodes = nds,
                            years = year_all,
                            times = time,
                            vintages = vtgs,	
                            types = c('water'),
                            modes = c( 1 ),
                            lifetime = 1,
                            
                            input = bind_rows(  left_join(  expand.grid( 	node = nds,
                                                                          vintage = vtgs,
                                                                          mode = c( 1 ), 
                                                                          commodity = 'freshwater', 
                                                                          level = 'inflow',
                                                                          value =  1 ) ,
                                                            vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
                                                  dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )				
                                                
                            ),
                            
                            output = bind_rows( left_join(  expand.grid( 	node = nds,
                                                                          vintage = vtgs,
                                                                          mode = 1, 
                                                                          commodity = 'freshwater', 
                                                                          level = 'river_in',
                                                                          value = 1 ),
                                                            vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
                                                  dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ) #,	
                                                
                                                # left_join(  expand.grid( 	node = nds,
                                                                          # vintage = vtgs,
                                                                          # mode = 1, 
                                                                          # commodity = 'hydro_potential_river', 
                                                                          # level = 'energy_secondary' ),
                                                            # vtg_year_time ) %>% left_join(
                                                              # water_resources.df %>%
                                                              # mutate( value = river_mw_per_km3_per_day / 1e3 ) %>%
                                                              # dplyr::select( node,value )) %>% 
                                                  # filter(value > 0) %>% 
                                                  # mutate( node_out = node, time_out = time ) %>% 
                                                  # dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
                                                
                                                # left_join(  expand.grid( 	node = initial_nodes,
                                                                          # vintage = vtgs,
                                                                          # mode = 1, 
                                                                          # commodity = 'hydro_potential_old', 
                                                                          # level = 'energy_secondary' ),
                                                            # vtg_year_time ) %>% left_join(
                                                              # water_resources.df %>% filter(node %in% initial_nodes) %>% 
                                                                # left_join(tmp) %>% 
                                                              # mutate( value =  as.numeric(( existing_MW + planned_MW ) / qnt / 1e3)) %>% 
                                                              # dplyr::select(node,value) ) %>% 
                                                  # filter(value > 0) %>% 
                                                  # mutate( node_out = node, time_out = time ) %>% 
                                                  # dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
                                                
                            ),	
                         
                         
                            
                            # data.frame( node,vintage,year_all,mode,emission) 
                            emission_factor = NULL,								
                            
                            
                            # data.frame(node,vintage,year_all,time)
                            capacity_factor = left_join( 	expand.grid( 	node = nds,
                                                                        vintage = vtgs,
                                                                        value = 1 ) ,
                                                          vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
                            
                            # data.frame( vintages, value )
                            construction_time = expand.grid( 	node = nds,
                                                              vintage = vtgs,
                                                              value = 0	),
                            
                            # data.frame( vintages, value )
                            technical_lifetime = expand.grid( 	node = nds,
                                                               vintage = vtgs,
                                                               value = 1	),
                            
                            # data.frame( vintages, value )
                            inv_cost = expand.grid( node = nds,
                                                    vintage = vtgs,
                                                    value = 0	),
                            
                            # data.frame( node, vintages, year_all, value )
                            fix_cost = NULL, 
                            
                            # data.frame( node, vintage, year_all, mode, time, value ) 
                            var_cost = NULL,							
                            
                            # data.frame(node,year_all,value)
                            historical_new_capacity = NULL
                            
)

## River network - technologies that move surface water between PIDs
vtgs = year_all
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
river_names = NULL
for( iii in 1:length( basin.spdf ) )
	{
	
	# Use the network mapping contained with the basin shapefile
	bi = as.character( basin.spdf@data$PID[ iii ] )
	bo = as.character( basin.spdf@data$DOWN[ iii ] )
	
	if( !is.na( bo ) ){ # A downstream basin exists
		
		# Output to downstream basin while producing hydropower potential 
		outx = bind_rows( 	expand.grid( 	node = bi,
											mode = 1,
											vintage = vtgs,
											time = time,
											commodity = 'freshwater',
											level = 'river_in',
											value = 0.75 ) %>%
												mutate( node_out = bo, time_out = time, year_all = vintage ) %>%
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
												
							expand.grid( 	node = bi,
											mode = 1,
											vintage = vtgs,
											time = time,
											commodity = 'hydro_potential_old',
											level = 'energy_secondary',
											value =  water_resources.df %>%
												filter( node == bi ) %>%
												mutate( value =  as.numeric(( existing_MW + planned_MW ) / 
												                              quantile( unlist( water_resources.df[ which( water_resources.df$node == bo), 
												                                                                    grepl( '2015', names(water_resources.df) ) ] ) , 0.75 ) / 1e3 )) %>%
												dplyr::select( value ) %>% unlist() ) %>%	
								mutate( node_out = bi, time_out = time, year_all = vintage ) %>%
								dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
								
							expand.grid( 	node = bi,
											mode = 1,
											vintage = vtgs,
											time = time,
											commodity = 'hydro_potential_river',
											level = 'energy_secondary',
											value =  water_resources.df %>%
												filter( node == bi ) %>%
												mutate( value = river_mw_per_km3_per_day / 1e3 ) %>%
												dplyr::select( value ) %>% unlist() ) %>%	
								mutate( node_out = node, time_out = time, year_all = vintage ) %>%
								dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )	
																
			)
			
		}else{ # No downstream basin - sink or outlet, i.e., no downstream node in the core model
		
		bo = 'SINK'  
		
		outx = bind_rows( 	left_join(  data.frame( commodity = 'hydro_potential_old', 
													level = 'energy_secondary', 
													mode = 1, 
													node = bi,
													vintage = vtgs,
													value = water_resources.df %>%
																filter( node == bi ) %>%
																mutate( value =  ( existing_MW + planned_MW ) / quantile( get( names(water_resources.df)[ grepl( '2015', names(water_resources.df) ) ] ) , 0.75 ) / 1e3 ) %>%
																dplyr::select( value ) %>% unlist() ),
										vtg_year_time ) %>%
									mutate( node_out = node, time_out = time ) %>%							
									dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
							
							left_join(  data.frame( commodity = 'hydro_potential_river', 
													level = 'energy_secondary', 
													mode = 1, 
													node = bi,
													vintage = vtgs,
													value = water_resources.df %>%
																filter( node == bi ) %>%
																mutate( value = river_mw_per_km3_per_day / 1e3 ) %>%
																dplyr::select( value ) %>% unlist() ),
										vtg_year_time ) %>%
									mutate( node_out = node, time_out = time ) %>%							
									dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
						
							)
		
		}
	
	river_name = paste( 'river', bi, bo, sep = '|' ) 
	
	river_names = c( river_names, river_name )
	
	tlst = list( 	nodes = bi,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('water'),
					modes = c( 1 ),
					lifetime = 20,
					
					input = bind_rows( left_join(  expand.grid( 	node = bi,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_out',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )	
								),
						
					output = outx,
					
					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = NULL,								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL

					)
	
	assign( river_name, tlst )
			
	}

river_routes = gsub('river\\|','',river_names)
## Conveyance - technologies that move surface water between PIDs
vtgs = year_all
lft = 50
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
canal_names = NULL

# Need to provide options for lined and unlined canals 
for( hhh in c( 'conv', 'lined' ) )
	{
	
	if( hhh == 'conv' ){ eff = 0; cst = 1; flg = 1 }else{ eff = 0.15; cst = 1.5; flg = 0 } # set adders/mulitpliers for efficiency and costs between lined and unlined systems
	
	# Need to provide option for both directions as invididual investments as opposed to a single investment / bi-directional system - conveyance not typically operated bi-directionally
	adjacent_routes2 = c( adjacent_routes, paste( unlist( strsplit( adjacent_routes, '[|]' ) )[ seq( 2,2*length(adjacent_routes),by=2 ) ], unlist( strsplit( adjacent_routes, '[|]' ) )[ seq( 1,2*length(adjacent_routes),by=2 ) ], sep = '|'  ) )
	
	# # Remove options that are across borders 
	# adjacent_routes2 = adjacent_routes2[ which( unlist( strsplit( unlist( strsplit( adjacent_routes2, '[|]' ) )[ seq( 1,2*length(adjacent_routes2), by=2 ) ], '_' ) )[ seq( 1,2*length(adjacent_routes2), by=2 ) ] ==
	# 											unlist( strsplit( unlist( strsplit( adjacent_routes2, '[|]' ) )[ seq( 2,2*length(adjacent_routes2), by=2 ) ], '_' ) )[ seq( 1,2*length(adjacent_routes2), by=2 ) ] ) ]
	# 
	# Cost data
	adjacent_routes2 = setdiff(adjacent_routes2,river_routes)
	
	if (!FULL_COOPERATION) {
	# # Remove options that are across borders
	adjacent_routes2 = adjacent_routes2[gsub('_.*', '',gsub('.*\\|', '',adjacent_routes2)) == 	gsub('_.*', '',gsub('\\|.*', '',adjacent_routes2))]
	} else{
	} # end if
	can_inv_cost.df = rbind( can_inv_cost.df, can_inv_cost.df %>% mutate( tec = paste( unlist( strsplit( tec, '[|]' ) )[ seq( 2,2*length(tec),by=2 ) ], unlist( strsplit( tec, '[|]' ) )[ seq( 1,2*length(tec),by=2 ) ], sep = '|'  ) ) )
	
	# Go through routes
	for( iii in 1:length(adjacent_routes2) )
		{
		
		bi = unlist( strsplit( as.character( adjacent_routes2[iii] ), '[|]' ) )[1]
		bo = unlist( strsplit( as.character( adjacent_routes2[iii] ), '[|]' ) )[2]
		
		if( adjacent_routes2[iii] %in% canals_agg.df$route ){ hc = round( canals_agg.df$capacity_m3_per_sec[ which( canals_agg.df$route == adjacent_routes2[iii] ) ] * 60 * 60 * 24 / 1e6, digits = 1 ) }else{ hc = 0 }
		
		canal_name = paste0( hhh, '_canal|', adjacent_routes2[iii] )
		
		canal_names = c( canal_names, canal_name )
		
		tlst = list( 	nodes = bi,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('water'),
						modes = c( 1 ),
						lifetime = 20,
						
						input = bind_rows(  left_join(  expand.grid( 	node = bi,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'freshwater', 
																		level = 'river_out',
																		value = 1 ) , 
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),	
											left_join(  expand.grid( 	node = bi,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'electricity', 
																		level = 'urban_final',
																		value = 12 ) , # ! Should be updated to unique value for each route
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
									),
							
						output = bind_rows( left_join(  expand.grid( 	node = bi,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'freshwater', 
																		level = 'river_in',
																		value = 0.75 + eff ) , # assuming 25% losses - should be updated to align with seepage estimates
														vtg_year_time ) %>% mutate( node_out = bo, time_out = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),	
									),
						
						# data.frame( node,vintage,year_all,mode,emission) 
						emission_factor = NULL,								
																								
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.9 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 5	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = lft	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = cst * can_inv_cost.df$value[ which( can_inv_cost.df$tec == adjacent_routes2[iii] | can_inv_cost.df$tec == paste( unlist( strsplit( adjacent_routes2[iii], '[|]' ))[c(2,1)] ,collapse='|') ) ] ),
						
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0.05 * cst * can_inv_cost.df$value[ which( can_inv_cost.df$tec == adjacent_routes2[iii] | can_inv_cost.df$tec == paste( unlist( strsplit( adjacent_routes2[iii], '[|]' ))[c(2,1)] ,collapse='|') ) ]	),
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = NULL,							
						
						# data.frame(node,year_all,value)
						historical_new_capacity = expand.grid( node = bi, year_all = year_all, value = hc * flg ),
						
						# data.frame(node,year_all,value)
						bound_total_capacity_lo = expand.grid(  node = bi, 
																year_all = year_all, 
																value = flg * max( c( 0, round( canals_agg.df %>% filter( route == adjacent_routes2[iii] ) %>% select( capacity_m3_per_sec )%>% unlist( . ) * 60 * 60 * 24 / 1e6, digits = 1 ) ), na.rm = TRUE ) ),
						
						bound_activity_lo = expand.grid( 	node = bi, 
															year_all = year_all, 
															mode = 1,
															time = as.character( time ), 
															value = flg * 0.2 * max( c( 0, round( canals_agg.df %>% filter( route == adjacent_routes2[iii] ) %>% select( capacity_m3_per_sec ) %>% unlist( . ) * 60 * 60 * 24 / 1e6, digits = 1 ) ), na.rm = TRUE ) )
							
						)
				
		assign( canal_name, tlst )
				
		}	
	
	}
	
# interbasin canal for tranfers to India potentially missed in irrigation implementation
# Identified manually as 1) node that transfers water out to India via Indira Ghandhi canal etc.; and 
# 2) node the contains Keenjhar Lake and 583 MGD link to Karachi for potable water supply
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = interbasin_transfer.df$node 
interbasin_canal = list( 	nodes = nds,
							years = year_all,
							times = time,
							vintages = vtgs,	
							types = c('water'),
							modes = c( 1 ),
							lifetime = 1,
					
					input = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'freshwater', 
																	level = 'river_in',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )	
								),
						
					output = NULL, # transferred outside the basin
					
					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = left_join( expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																emission = 'water_consumption', 
																value = 1 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),								
																							
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 1 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 1	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1	), # test w/ small penalty 
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = NULL,							
							
					# data.frame(node,year_all,value)
					historical_new_capacity = NULL,
					
					# data.frame(node,year_all,value)
					bound_total_capacity_lo = bind_rows( lapply( nds, function( nnn ){
						expand.grid( 	node = nnn, 
										year_all = year_all[ year_all>baseyear ], 
										value = unlist( interbasin_transfer.df[nnn,'min_flow'] )	)
						} ) ),
					
					bound_activity_lo = bind_rows( lapply( nds, function( nnn ) { 
						expand.grid( 	node = nnn, 
										year_all = year_all, 
										mode = 1,
										time = as.character( time ), 
										value = unlist( interbasin_transfer.df[nnn,'min_flow'] ) * 0.95 ) 
						} ) )			
						
					)	
	
#------------
# CROPS and irrigation technologies

# the number of crops and names can be decided by the user, it is basin specific 
# to select the main crops, and in this way many crops can be easily added
crp = crop_names
# ittigation technologies are just 3 at the moment, and rain-fed irrigation
# is considered in addition

machinery_ei_kwh_per_kg = 0.13 # energy in per unit of crop production - Average from Rao et al. (2018)
irrigation_tecs = unique( irr_tech_data.df$irr_tech )
# Define average efficiency for water once it reaches the farm-gate 
# Assuming all irrigation is basically flood irrigation based on disucssions with stakeholders
# Also aligns closely with asssumptions in Wu et al. (2013, World Bank) figure 5.4 
field_efficiency = 0.85
water_course_efficiency = 0.55 # these numbers need to match the efficiencies used for calibration in crop_yields.r so perhaps good to explicility link later
field_efficiency_conv = field_efficiency * water_course_efficiency
gwp_ch4 = gwp.df$gwp[gwp.df$emission == 'CH4']
crop_ch4 = data.frame( crop = 'rice',  value = 1300 ) # default IPCC CH4 emission factor converted to metric tons per Mha per day
crop_tech_names = NULL
rainfed_crop_names = NULL
irr_tech_names = NULL
residue_data.d = residue_data %>% bind_rows(
	data.frame(crop = c('fruit','vegetables'), res_yield = c(0,0) , mode = c(8,9), liquid = c(0,0),
    ethanol_ratio = c(NA,NA), var_eth_cost = c(NA,NA)  ,stringsAsFactors = F)
	)
for( ii in seq_along( crop_names ) )
	{
	vtgs = year_all
	nds = bcus
	crop_tech_name = paste0('crop_',crop_names[ii])
	crop_tech_names = c( crop_tech_names, crop_tech_name )
	
	lft = 1
	vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
	vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
	node_mode_vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( node = bcus, mode = 1, vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) )
	growing_season = crop_tech_data.df %>% filter(crop == crop_names[ii] & par == 'crop_coef') %>% filter( value > 0 ) %>% select( time ) %>% unlist()
	vtg_year_time = vtg_year_time %>% filter( time %in% growing_season )
	
	tmp = crop_input_data.df %>% filter(crop == crop_names[ii] & par == 'rain-fed_yield') %>% dplyr::select(node,value)
	tmp = tmp %>% expand(tmp,time) %>% filter( time %in% growing_season ) %>%
    left_join(crop_tech_data.df %>% 
                filter(crop == crop_names[ii] & par == 'crop_coef') %>% 
                rename(ccf = value) %>% 
                dplyr::select(time,ccf) ) %>%     #from yearly yield to monthly and scaled with crop coefficient
    mutate(value = value * ccf ) %>%  #kton/Mha
    filter(!is.na(value)) %>% 
    mutate(commodity = paste0(crop_names[ii],'_yield'))

	tlst = list(nodes = nds,
				years = year_all,
				times = growing_season,
				vintages = vtgs,	
				types = c('land'),
				modes = c( 1 ),
				lifetime = 1,
				
				input =  left_join( tmp %>% expand(tmp, level = c('agriculture_final'), vintage = vtgs ) %>% # on-farm energy requirements for machinery
									mutate( mode = 1, time_in = time, node_in = node, commodity = 'energy' ) %>%
									mutate( value = round( machinery_ei_kwh_per_kg * value * ( 1 / 8760  ) * ( 1 / 1e3 ) * ( 1e6 / 1 ) , digits = 3 ) )	, # convert the intensity to MW per Mha - assumed it is evenly distributed across the year
								vtg_year ) %>% 
									dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
					
				output = bind_rows( 
					
					left_join( expand.grid( node = nds,
											commodity = paste0(crop_names[ii],'_land'), 
											vintage = vtgs,
											level = 'crop', 
											mode = 1, 
											value = 1 ),  # crop area in Mha
						vtg_year_time )	%>%	mutate( node_out = node, time_out = time ) %>% 
						dplyr::select(  node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ) 
					
					),
	
				# data.frame( node,vintage,year_all,mode,emission) 
				emission_factor = 
				  rbind( left_join( fertilizer_emissions.df %>% filter( crop == crop_names[ii] ) %>%
						filter( if( length( which( irrigation == 'rainfed' ) ) == 0 ) irrigation == 'irrigated' else irrigation == 'rainfed' ) %>%
						group_by( PID, crop, emission ) %>% # 0.8 reflects that about 80% farmers using fertilizers at recommended rates
						summarise( value = round( sum ( value ) * 1 / 365 * 1 / 1e3 * 1e6 / 1 * 0.8, digits = 3 ) ) %>% # convert from kg per year per hectare to metric tons per day per Mha
						ungroup() %>% data.frame() %>%
						rename( node = PID ),
					node_mode_vtg_year ) %>%
					select( node, vintage, year_all, mode, emission, value ), # add methane emissions
					left_join( 	node_mode_vtg_year,
								rbind( 	expand.grid( node = bcus, emission = 'CH4', value = max( 0, unlist( crop_ch4 %>% filter(crop==crop_names[ii]) %>% select(value) ), na.rm=TRUE ) ),
										expand.grid( node = bcus, emission = 'CO2eq', value = max( 0, unlist( crop_ch4 %>% filter(crop==crop_names[ii]) %>% select(value) ), na.rm=TRUE ) * gwp_ch4 ) ) ) ) %>%
					group_by( node, vintage, year_all, mode, emission ) %>% summarise( value = round( sum(value), digits = 3 ) ) %>%
					ungroup() %>% data.frame(),
																																		
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 1 ) ,
												vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = crop_tech_data.df %>% 
												filter( crop == crop_names[ii], par == 'inv_cost' ) %>% 
												dplyr::select( value ) %>% 
												unlist() ),
												
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = crop_tech_data.df %>% 
												filter( crop == crop_names[ii], par == 'fix_cost' ) %>% 
												dplyr::select( value ) %>% 
												unlist() ), 
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = left_join( 	expand.grid( 	node = nds,
														mode = 1,
														vintage = vtgs,
														value = crop_tech_data.df %>% 
															filter( crop == crop_names[ii], par == 'var_cost' ) %>% 
															dplyr::select( value ) %>% 
															unlist() ), 
										vtg_year_time ) %>% dplyr::select( node, vintage, year_all, mode, time, value )	,	
				
				min_utilization_factor = left_join( expand.grid( 	node = nds,
				                                                  vintage = vtgs,
				                                                  value = 0	),
				                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
						
				# data.frame(node,year_all,value)
				
				historical_new_capacity = 
				
										expand.grid(node = nds, year_all = c(2015) ) %>%
										left_join(	crop_input_data.df %>%
										filter(crop == crop_names[ii] & par %in% c('crop_irr_land_2015','crop_rainfed_land_2015') ) %>%
										mutate(crop = crop_tech_name, value = value ) %>%
										dplyr::rename( tec = crop ) ) %>%
				            group_by(node,tec,year_all) %>% 
				            summarise(value = sum(value)) %>% ungroup() %>% 
										dplyr::select(node,tec,year_all,value)
						
				)
				
					
	assign( crop_tech_name, tlst )
  
	# rain-fed crops, will have particular parametrization, like yield,
	# for now we keep it separated from other irrigation technologies
	
	rainfed_crop_name = paste0('rainfed_',crop_names[ii])
	rainfed_crop_names = c( rainfed_crop_names, rainfed_crop_name  )

	tmp = crop_input_data.df %>% filter(crop == crop_names[ii] & par == 'rain-fed_yield') %>% dplyr::select(node,value)
	tmp = tmp %>% expand(tmp,time) %>% filter( time %in% growing_season ) %>%
    left_join(crop_tech_data.df %>% 
                filter(crop == crop_names[ii] & par == 'crop_coef') %>% 
                rename(ccf = value) %>% 
                dplyr::select(time,ccf) ) %>%     #from yearly yield to monthly and scaled with crop coefficient
    mutate(value = value * ccf ) %>%  #kton/Mha
    filter(!is.na(value)) %>% 
    mutate(commodity = paste0(crop_names[ii],'_yield'))
	
	nds = unique((tmp %>% filter(value > 0))$node)
		
	tlst = list(nodes = nds,
				years = year_all,
				times = growing_season,
				vintages = vtgs,	
				types = c('land'),
				modes = c( 1 ),
				lifetime = 1,
				
				input = left_join( expand.grid( node = nds,
											commodity = paste0(crop_names[ii],'_land'), 
											vintage = vtgs,
											level = 'crop', 
											mode = 1, 
											value = 1 ), 
						vtg_year_time )	%>%	mutate( node_in = node, time_in = time ) %>% 
						dplyr::select(  node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
					
				output = bind_rows( 
					
					left_join( expand.grid( node = nds,
											commodity = 'crop_land', 
											vintage = vtgs,
											level = 'area', 
											mode = 1, 
											value = 1 ), 
						vtg_year_time )	%>%	mutate( node_out = node, time_out = as.character(time) ) %>% 
						dplyr::select(  node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ) ,

					 # for the next df the time_output is yearly (annual yield demand)
                    left_join( tmp %>% expand(tmp, level = c('raw','residue'), vintage = vtgs ) %>% 
									mutate(mode = 1, time = time) %>% 
									mutate(time_out = if_else(level == 'raw','year',time)) %>% 
									mutate(node_out = node) %>%
									mutate(value = if_else(value == 0 , value, if_else(level == 'residue', ( residue_data.d$res_yield[ residue_data.d$crop == crop_names[ii] ] ) *ccf , value ) ) ),
								vtg_year ) %>% filter( time %in% growing_season ) %>%
									dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )
					
					),
	
				# data.frame( node,vintage,year_all,mode,emission) emissions allocated to crop land area and to irrigation tec
				emission_factor = NULL,
					
				# data.frame(node,vintage,year_all,time)
				capacity_factor = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 1 ) ,
												vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
				
				# data.frame( vintages, value )
				construction_time = expand.grid( 	node = nds,
													vintage = vtgs,
													value = 1	),
				
				# data.frame( vintages, value )
				technical_lifetime = expand.grid( 	node = nds,
													vintage = vtgs,
													value = lft	),
				
				# data.frame( vintages, value )
				inv_cost = expand.grid( node = nds,
										vintage = vtgs,
										value = 0 ),
												
				# data.frame( node, vintages, year_all, value )
				fix_cost = left_join( 	expand.grid( 	node = nds,
														vintage = vtgs,
														value = 0 ), 
										vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
				
				# data.frame( node, vintage, year_all, mode, time, value ) 
				var_cost = left_join( 	expand.grid( 	node = nds,
														mode = 1,
														vintage = vtgs,
														value = 0), 
										vtg_year_time ) %>% dplyr::select( node, vintage, year_all, mode, time, value ),	
				
				min_utilization_factor = left_join( expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0	),
				                                    vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),	
						
				# data.frame(node,year_all,value)
				historical_new_capacity =  expand.grid(node = nds, year_all = c(2015) ) %>%
				  left_join( crop_input_data.df %>%
				               dplyr::filter(crop == crop_names[ii] & par == 'crop_rainfed_land_2015') %>%
				               mutate( crop = rainfed_crop_name, value = round( value, digits = 5 ) ) %>%
				               dplyr::rename( tec = crop )
				  ) %>% dplyr::select(node,tec,year_all,value)
						
				)
				
	assign( rainfed_crop_name, tlst )
  
	for (jj in seq_along(irrigation_tecs)){
		
		irr_tech_name = paste0(irrigation_tecs[jj],'_',crop_names[ii])
		irr_tech_names = c( irr_tech_names, irr_tech_name)
		tmp = crop_input_data.df %>% filter(crop == crop_names[ii]) %>% 
		  spread(par,value) %>% 
		  mutate(irrigation_yield = if_else(is.na(irrigation_yield),0,irrigation_yield),
		         `rain-fed_yield` = if_else(is.na(`rain-fed_yield` ),0,`rain-fed_yield` ) ) %>% 
		  mutate(value = irrigation_yield) %>% 
		  dplyr::select(node,value) %>% 
		  group_by(node) %>% summarise(value = max(value)) %>% ungroup()
		tmp = tmp %>% expand(tmp,time) %>% 
			left_join(	crop_tech_data.df %>% 
						filter(crop == crop_names[ii] & par == 'crop_coef') %>% 
						rename(ccf = value) %>% 
						dplyr::select(time,ccf) ) %>% 
			mutate(value = value * ccf) %>%  #kton/Mha
			dplyr::filter(!is.na(value)) %>% 
			mutate(commodity = paste0(crop_names[ii],'_yield'))
		
		if (grepl('irr_flood_',irr_tech_name)){
		  tmp_hist_new_cap =  expand.grid(node = nds, year_all = c(2015) ) %>%
		    left_join( crop_input_data.df %>%
		                 dplyr::filter(crop == crop_names[ii] & par == 'crop_irr_land_2015') %>%
		                 mutate( crop = irr_tech_name, value = round( value, digits = 5 ) ) %>%
		                 dplyr::rename( tec = crop )
		    ) %>% dplyr::select(node,tec,year_all,value)
		} else {
		  tmp_hist_new_cap =  NULL
		}
		
		nds = unique((tmp %>% filter(value > 0))$node)
		nds = nds[nds %in% unique((crop_water.df %>% filter(crop == crop_names[ii]))$node)]
    
		tlst = list( 	nodes = nds,
						years = year_all,
						times = growing_season,
						vintages = vtgs,	
						types = c('land'),
						modes = 1,
						lifetime = 9,
							
						input = bind_rows(  left_join(  
												expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1, 
																commodity = paste0(crop_names[ii],'_land'), 
																level = 'crop',
																value = 1 ) ,
												vtg_year_time ) %>% 
													mutate( node_in = node, time_in = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
													
											# crop water requirement scaled to represent water taken from irrigation diversions 
											# need to divide by the field efficiency multiplied by the efficiency of the irrigation tech to estimate water requirement at the crop-level
											# Note that additional efficiency losses are accounted for in 'irrigation_sw_diversion', to account for the distribution losses
											left_join( crop_water.df %>% filter(crop == crop_names[ii]) %>% 
													dplyr::rename( year_all = year ) %>%
													mutate( value = ( 1 / unlist( 	irr_tech_data.df %>% 
																						filter( irr_tech == irrigation_tecs[jj], par == 'water_efficiency' ) %>% 
																						select( value ) %>% unlist( . ) * 
																					field_efficiency_conv ) 
																		) * value, time = as.character( time ) ) %>%
													mutate( commodity = 'freshwater', level = 'irrigation_final',  mode = 1, node_in = node, time_in = time ),
												vtg_year ) %>%	
											  filter(year_all <= lastyear) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ) %>%
													mutate( value = round( value, digits = 3 ) ),

											# Operational electricity for the irrigation tech - varies based on water use
											left_join( crop_water.df %>% filter(crop == crop_names[ii]) %>% 
													dplyr::rename( year_all = year ) %>%
													mutate( value = ( 1 / unlist( 	irr_tech_data.df %>% 
																						filter( irr_tech == irrigation_tecs[jj], par == 'water_efficiency' ) %>% 
																						select( value ) %>% unlist( . ) * 
																					field_efficiency_conv ) 
																		) * value, time = as.character( time ) ) %>%
													mutate( commodity = 'electricity', level = 'irrigation_final',  mode = 1, node_in = node, time_in = time ),
												vtg_year ) %>%	
												filter(year_all <= lastyear) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ) %>%
													mutate( value = value * irr_tech_data.df %>% # multiply water use per area by electricity use per water to get electricity per activity
																				filter( irr_tech == irrigation_tecs[jj], par == 'electricity_intensity' ) %>% 
																				select( value ) %>% unlist( . ) ) %>% 
																				mutate( value = round( value, digits = 3 ) )
																				
									) %>% filter(node %in% nds), 
											
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'crop_land', 
																		level = 'area',
																		value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = as.character(time) ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )	,
													
													# for the next df the time_output is yearly (annual yield demand)
													left_join( tmp %>% expand(tmp, level = c('raw','residue'), vintage = vtgs ) %>% 
																	mutate(mode = 1, time = time) %>% 
																	mutate(time_out = if_else(level == 'raw','year',time)) %>% 
																	mutate(node_out = node) %>%
																	mutate(value = if_else(value == 0 , value, if_else(level == 'residue', ( residue_data.d$res_yield[ residue_data.d$crop == crop_names[ii] ] ) *ccf , value ) ) ),
																vtg_year ) %>% filter( time %in% growing_season ) %>% 
																	dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ) ,
													
													# irrigation losses contribute to groundwater recharge		
													left_join( crop_water.df %>% filter(crop == crop_names[ii]) %>% 
														dplyr::rename( year_all = year ) %>%
														mutate( value = (( 1 / unlist( irr_tech_data.df %>% 
																						filter( irr_tech == irrigation_tecs[jj], par == 'water_efficiency' ) %>% 
																						select( value ) %>% unlist( . ) * field_efficiency_conv ) )-1 ) * value, time = as.character( time ) ) %>%
														mutate( commodity = 'renewable_gw', level = 'aquifer',  mode = 1, node_out = node, time_out = time ),
														vtg_year ) %>%	
														filter(year_all <= lastyear) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
														
													# Operational electricity flexibility for the smart irrigation tech - varies based on water use
													left_join( crop_water.df %>% filter(crop == crop_names[ii]) %>% 
															dplyr::rename( year_all = year ) %>%
															mutate( value = ( 1 / unlist( 	irr_tech_data.df %>% 
																								filter( irr_tech == irrigation_tecs[jj], par == 'water_efficiency' ) %>% 
																								select( value ) %>% unlist( . ) * 
																							field_efficiency_conv ) 
																				) * value, time = as.character( time ) ) %>%
															mutate( commodity = 'flexibility', level = 'energy_secondary',  mode = 1, node_in = node, time_in = time ),
														vtg_year ) %>%	
														filter(year_all <= lastyear) %>% 
															dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ) %>%
															mutate( value = value * irr_tech_data.df %>% # multiply water use per area by electricity use per water to get electricity per area 
																						filter( irr_tech == irrigation_tecs[jj], par %in% c( 'electricity_intensity', 'electricity_flexibility' ) ) %>% 
																						select( value ) %>% unlist( . ) %>% min( c( prod( . ), 0 ) ) ) %>% # electricity flexibility impacts depend on electricity demand level - avoid double counting flexibility impacts included in distribution techs									
																						mutate( value = round( value, digits = 3 ) ) %>%
																						rename( node_out = node_in, time_out = time_in )		
																			
													
									
									) %>% filter(node %in% nds),	

						# data.frame( node,vintage,year_all,mode,emission) - difference between the irrigated and rain fed to avoid doible counting
						emission_factor = right_join(
							left_join( fertilizer_emissions.df %>% filter( crop == crop_names[ii] ) %>%
								filter( if( length( which( irrigation == 'rainfed' ) ) == 0 ) irrigation == 'irrigated' else irrigation == 'rainfed' ) %>%
								group_by( PID, crop, emission ) %>%
								summarise( value = sum ( value ) * 1 / 365 * 1 / 1e3 * 1e6 / 1 * 0.8 ) %>% ungroup() %>% data.frame() %>%
								rename( node = PID ), node_mode_vtg_year ) %>%
							select( node, vintage, year_all, mode, emission, value ) %>% rename( value2 = value ),
							left_join( fertilizer_emissions.df %>% filter( crop == crop_names[ii] ) %>%
									filter( irrigation == 'irrigated') %>%
									group_by( PID, crop, emission ) %>%
									summarise( value = sum ( value ) * 1 / 365 * 1 / 1e3 * 1e6 / 1 * 0.8 ) %>% # 0.8 reflects that about 80% farmers using fertilizers at recommended rates
									ungroup() %>% data.frame() %>%
								rename( node = PID ), node_mode_vtg_year ) %>%
							select( node, vintage, year_all, mode, emission, value ) ) %>%
								mutate( value2 = ifelse( is.na(value2), 0, value2 ) ) %>%
								mutate( value = round( value-value2 , digits = 3 ) ) %>%
								select( node, vintage, year_all, mode, emission, value ),
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 1 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = lft	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = irr_tech_data.df %>% 
															filter( irr_tech == irrigation_tecs[ jj ], par == 'inv_cost' ) %>%
															select( value ) %>% unlist() ),
												
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = irr_tech_data.df %>% 
																	filter( irr_tech == irrigation_tecs[ jj ], par == 'fix_cost' ) %>%
																	select( value ) %>% unlist() ), 
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = left_join( 	expand.grid( 	node = nds,
																mode = 1,	
																vintage = vtgs,
																value = irr_tech_data.df %>% 
																	filter( irr_tech == irrigation_tecs[ jj ], par == 'var_cost' ) %>%
																	select( value ) %>% unlist() ),
												vtg_year_time ) %>% dplyr::select( node, vintage, year_all, mode, time, value ),
												
						min_utilization_factor = left_join( expand.grid( 	node = nds,
																			vintage = vtgs,
																			value = 0	),
															vtg_year ) %>% dplyr::select( node, vintage, year_all, value ) ,
						
						# data.frame(node,year_all,value)
						
						historical_new_capacity =  tmp_hist_new_cap
			)
	
		assign( irr_tech_name, tlst )
    
		}
	
	}

# fellow crops, do not consume of produce anything, no costs, just consume land,
# it is required to not necessarily use all the available land and still having
# a full commodity balance
vtgs = year_all
nds = bcus
lft = 1
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
fallow_crop	 = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('land'),
						modes = 1,
						lifetime = lft,
							
						input = NULL, 
											
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = 1, 
																		commodity = 'crop_land', 
																		level = 'area',
																		value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )	
										
									),	

						# data.frame( node,vintage,year_all,mode,emission) 
						emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																				vintage = vtgs,
																				mode = 1, 
																				emission = 'CO2', 
																				value = 0 ) ,
															vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

							),										
																		
						# data.frame(node,vintage,year_all,time)
						capacity_factor = left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 1 ) ,
														vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
						
						# data.frame( vintages, value )
						construction_time = expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
						
						# data.frame( vintages, value )
						technical_lifetime = expand.grid( 	node = nds,
															vintage = vtgs,
															value = lft	),
						
						# data.frame( vintages, value )
						inv_cost = expand.grid( node = nds,
												vintage = vtgs,
												value = 0	),
						
						# data.frame( node, vintages, year_all, value )
						fix_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																value = 0	),
												vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
						
						# data.frame( node, vintage, year_all, mode, time, value ) 
						var_cost = left_join( 	expand.grid( 	node = nds,
																vintage = vtgs,
																mode = 1,
																value =0 ) ,	 # 5.2E-3 M$/kt transport costs	
												vtg_year_time ) %>% dplyr::select( node, vintage, year_all, mode, time, value ),				
												
														
						# data.frame(node,vintage,year_all,value)
						min_utilization_factor = left_join( expand.grid( 	node = nds,
																			vintage = vtgs,
																			value = 0	),
															vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
						
						# data.frame(node,year_all,value)
						historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "fallow_crop",]

						)
					
## biomass converions into ethanol or solid, including transport
vtgs = year_all
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
nds = bcus
solid_biom = list( 	nodes = nds,
					years = year_all,
					times = time,
					vintages = vtgs,	
					types = c('power'),
					modes = residue_data$mode,
					lifetime = lft,
						
					input = left_join( 	left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = residue_data$mode, 
																	level = 'residue',
																	value = 1	),
													data.frame( mode = residue_data$mode, commodity = paste0( residue_data$crop, '_yield' ) ) 
											), vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
										dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ), 
										
					output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = residue_data$mode, 
																	commodity = 'biomass', 
																	level = 'solid',
																	value = 1 ) ,
												vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
												dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )	
									
								),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = residue_data$mode, 
																			emission = 'CO2', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															mode = residue_data$mode,
															value = 5.2E-3 ) ,	 # 5.2E-3 M$/kt transport costs	
											vtg_year_time ) %>% dplyr::select( node, vintage, year_all, mode, time, value ),				
											
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.2	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "solid_biom",]

					)
					
#solid biomass tratment, and trnasport (drying is usually done at the power plant, using waste heat)
vtgs = year_all
nds = bcus
lft = 30
vtg_year = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ] ) } ) ) 
vtg_year_time = bind_rows( lapply( vtgs, function( vv ){ expand.grid( vintage = vv, year_all = year_all[ year_all %in% vv:(vv+lft) ], time = time )  } ) ) 
liq_modes = unlist( ( residue_data %>% filter(liquid > 0))['mode'] )
ethanol_prod = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('power'),
						modes = liq_modes,
						lifetime = lft,
						
						input = bind_rows(  left_join( 	left_join(  expand.grid( 	node = nds,
																					vintage = vtgs,
																					mode = liq_modes, 
																					level = 'residue' ),
																	data.frame( mode = liq_modes, value = round( 1 / ( residue_data %>% filter(liquid > 0) %>% dplyr::select( ethanol_ratio ) %>% unlist() ), digits = 5 ),
																	            commodity = paste0(residue_data$crop[residue_data$liquid > 0],'_yield') ) 
															), vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value ),
											
											left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = liq_modes, 
																		commodity = 'freshwater', 
																		level = 'energy_secondary',
																		value = 3 ),
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
														
							),
						
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = liq_modes, 
																	commodity = 'biomass', 
																	level = 'ethanol',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )	
										
									),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = liq_modes, 
																			emission = 'CO2', 
																			value = 0 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value ),
													
													left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = liq_modes, 
																			emission = 'water_consumption', 
																			value = 3 ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 1.064	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.016	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = left_join( 	left_join( 	expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = liq_modes ),
												data.frame( mode = liq_modes, value = 5.2E-3 +( residue_data %>% filter(liquid > 0) %>% dplyr::select( var_eth_cost ) %>% unlist() ) ) ),	 # 5.2E-3 M$/kt transport costs	
											vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, mode, time, value ),				
											
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.2	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "ethanol_prod",],
					
					# bound_new_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1	)

					)			

# Ethanol generator, rural areas only
ethanol_genset = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('power'),
						modes = c(1,2),
						lifetime = lft,
						
						input = bind_rows(  left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = c(1,2), 
																		commodity = 'biomass', 
																		level = 'ethanol',
																		value = 1/0.33 ),
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
											
							),
						
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'electricity', 
																	level = 'irrigation_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value ),
											left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 2, 
																	commodity = 'electricity', 
																	level = 'rural_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )					
										
							),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1,2), 
																			emission = 'CO2', 
																			value = round( 0.0741 * 2.86 * 60 * 60 * 24 / 1e3, digits = 3 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join( expand.grid( node = nds,
																	vintage = vtgs,
																	mode = c(1,2),
																	value = 0 ),
															vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, mode, time, value )
											),				
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.2	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "ethanol_genset",],
					
					# bound_total_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1	)

					)
					
# Ethanol generator, on-farm machinery
ethanol_agri_genset = list( 	nodes = nds,
						years = year_all,
						times = time,
						vintages = vtgs,	
						types = c('power'),
						modes = c(1,2),
						lifetime = lft,
						
						input = bind_rows(  left_join(  expand.grid( 	node = nds,
																		vintage = vtgs,
																		mode = c(1,2), 
																		commodity = 'biomass', 
																		level = 'ethanol',
																		value = 1/0.33 ),
														vtg_year_time ) %>% mutate( node_in = node, time_in = time ) %>% 
														dplyr::select( node,  vintage, year_all, mode, node_in, commodity, level, time, time_in, value )
											
							),
						
						output = bind_rows( left_join(  expand.grid( 	node = nds,
																	vintage = vtgs,
																	mode = 1, 
																	commodity = 'energy', 
																	level = 'agriculture_final',
																	value = 1 ) ,
													vtg_year_time ) %>% mutate( node_out = node, time_out = time ) %>% 
													dplyr::select( node,  vintage, year_all, mode, node_out, commodity, level, time, time_out, value )				
										
							),	

					# data.frame( node,vintage,year_all,mode,emission) 
					emission_factor = bind_rows( 	left_join( expand.grid( node = nds,
																			vintage = vtgs,
																			mode = c(1), 
																			emission = 'CO2', 
																			value = round( 0.0741 * 2.86 * 60 * 60 * 24 / 1e3, digits = 3 ) ) ,
														vtg_year ) %>% dplyr::select( node,  vintage, year_all, mode, emission, value )

						),										
																	
					# data.frame(node,vintage,year_all,time)
					capacity_factor = left_join( 	expand.grid( 	node = nds,
																	vintage = vtgs,
																	value = 0.9 ) ,
													vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, time, value ),
					
					# data.frame( vintages, value )
					construction_time = expand.grid( 	node = nds,
														vintage = vtgs,
														value = 5	),
					
					# data.frame( vintages, value )
					technical_lifetime = expand.grid( 	node = nds,
														vintage = vtgs,
														value = lft	),
					
					# data.frame( vintages, value )
					inv_cost = expand.grid( node = nds,
											vintage = vtgs,
											value = 0.676	),
					
					# data.frame( node, vintages, year_all, value )
					fix_cost = left_join( 	expand.grid( 	node = nds,
															vintage = vtgs,
															value = 0.007	),
											vtg_year ) %>% dplyr::select( node, vintage, year_all, value ), 
					
					# data.frame( node, vintage, year_all, mode, time, value ) 
					var_cost = bind_rows( 	left_join( expand.grid( node = nds,
																	vintage = vtgs,
																	mode = c(1),
																	value = 0 ),
															vtg_year_time ) %>% dplyr::select( node,  vintage, year_all, mode, time, value )
											),				
														
													
					# data.frame(node,vintage,year_all,value)
					min_utilization_factor = left_join( expand.grid( 	node = nds,
																		vintage = vtgs,
																		value = 0.2	),
														vtg_year ) %>% dplyr::select( node, vintage, year_all, value ),
					
					# data.frame(node,year_all,value)
					historical_new_capacity = hist_new_cap.df[hist_new_cap.df$tec == "ethanol_agri_genset",],
					
					# bound_new_capacity_up(node,inv_tec,year)
					bound_total_capacity_up = expand.grid( 	node = nds, year_all = c(2020,2030,2040,2050,2060), value = 1	)

					)					
				