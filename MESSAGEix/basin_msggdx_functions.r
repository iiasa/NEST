

add_input = function( nodes, vintages, lifetime, years, times, params ){
	
	if( !is.null( params ) )
		{
	
		if( length( names( params ) ) == 4 ) # params = data.frame( commodity, level, mode, value )
			{
			
			df = bind_rows( lapply( nodes, function( nn ){
						
				bind_rows( lapply( vintages, function( vv ){
				
					bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
					
						bind_rows( lapply( times, function(mm){	
				
							data.frame( node = rep( nn, nrow( params ) ),
										vintage = rep( vv, nrow( params ) ),
										year_all = rep( yy, nrow( params ) ),
										mode = params$mode,
										node_in = nn,
										commodity = params$commodity,
										level = params$level,
										time = as.character( mm ),
										time_in = as.character( mm ),
										value = params$value )
												
							} ) )

						} ) ) 

					} ) )			
				
				} ) )
				
			}else{
			
			df = do.call( rbind, lapply( 1:nrow( params ), function( iii ){
				
				if( is.na( params$year[ iii ] ) )
					{
					
					bind_rows( lapply( vintages, function( vv ){
				
						bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
						
							if( is.na( params$time[ iii ] ) )
								{
								
								data.frame( node = params$node[ iii ],
											vintage = vv,
											year_all = yy,
											mode = params$mode[ iii ],
											node_in = params$node_in[ iii ],
											commodity = params$commodity[ iii ],
											level = params$level[ iii ],
											time = as.character( time ),
											time_in = as.character( time ),
											value = params$value[ iii ] )
								
								}else{
								
								data.frame( node = params$node[ iii ],
											vintage = vv,
											year_all = yy,
											mode = params$mode[iii],
											node_in = params$node_in[ iii ],
											commodity = params$commodity[ iii ],
											level = params$level[ iii ],
											time = as.character( params$time[ iii ] ),
											time_in = as.character( params$time_in[ iii ] ),
											value = params$value[ iii ] )
								
								}
						
							} ) )
						
						} ) )	

					}else{
					
					bind_rows( lapply( vintages[ which( vintages %in% seq( params$year[ iii ] - lifetime + 1, params$year[ iii ], by = 1 ) ) ], function( vv ){ 
						
						if( is.na( params$time[ iii ] ) )
							{
							
							data.frame( node = params$node[ iii ],
										vintage = vv,
										year_all = params$year[ iii ],
										mode = params$mode[ iii ],
										node_in = params$node_in[ iii ],
										commodity = params$commodity[ iii ],
										level = params$level[ iii ],
										time = as.character( time ),
										time_in = as.character( time ),
										value = params$value[ iii ] )
									
							}else{
							
							data.frame( node = params$node[ iii ],
										vintage = vv,
										year_all = params$year[ iii ],
										mode = params$mode[ iii ],
										node_in = params$node_in[ iii ],
										commodity = params$commodity[ iii ],
										level = params$level[ iii ],
										time = as.character( params$time[ iii ] ),
										time_in = as.character( params$time_in[ iii ] ),
										value = params$value[ iii ] )
							
							}
								
						} ) )
							
					}
							
				} ) )
				
			}
			
			return( df )
		
		}
		
	}	
	
add_output = function( nodes, vintages, lifetime, years, times, params ){
	
	if( !is.null( params ) )
		{
	
		if( length( names( params ) ) == 4 ) # params = data.frame( commodity, level, mode, value )
			{
			
			df = bind_rows( lapply( nodes, function( nn ){
						
				bind_rows( lapply( vintages, function( vv ){
				
					bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
					
						bind_rows( lapply( times, function(mm){	
				
							data.frame( node = rep( nn, nrow( params ) ),
										vintage = rep( vv, nrow( params ) ),
										year_all = rep( yy, nrow( params ) ),
										mode = params$mode,
										node_out = nn,
										commodity = params$commodity,
										level = params$level,
										time = as.character( mm ),
										time_out = as.character( mm ),
										value = params$value )
												
							} ) )

						} ) ) 

					} ) )			
				
				} ) )
				
			}else{
			
			df = do.call( rbind, lapply( 1:nrow( params ), function( iii ){
				
				if( is.na( params$year[ iii ] ) )
					{
					
					bind_rows( lapply( vintages, function( vv ){
				
						bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
						
							if( is.na( params$time[ iii ] ) )
								{
								
								data.frame( node = params$node[ iii ],
											vintage = vv,
											year_all = yy,
											mode = params$mode[ iii ],
											node_out = params$node_out[ iii ],
											commodity = params$commodity[ iii ],
											level = params$level[ iii ],
											time = as.character( time ),
											time_out = as.character( time ),
											value = params$value[ iii ] )
								
								}else{
								
								data.frame( node = params$node[ iii ],
											vintage = vv,
											year_all = yy,
											mode = params$mode[iii],
											node_out = params$node_out[ iii ],
											commodity = params$commodity[ iii ],
											level = params$level[ iii ],
											time = as.character( params$time[ iii ] ),
											time_out = as.character( params$time_out[ iii ] ),
											value = params$value[ iii ] )
								
								}
						
							} ) )
						
						} ) )	

					}else{
					
					bind_rows( lapply( vintages[ which( vintages %in% seq( params$year[ iii ] - lifetime + 1, params$year[ iii ], by = 1 ) ) ], function( vv ){ 
						
						if( is.na( params$time[ iii ] ) )
							{
							
							data.frame( node = params$node[ iii ],
										vintage = vv,
										year_all = params$year[ iii ],
										mode = params$mode[ iii ],
										node_out = params$node_out[ iii ],
										commodity = params$commodity[ iii ],
										level = params$level[ iii ],
										time = as.character( time ),
										time_out = as.character( time ),
										value = params$value[ iii ] )
									
							}else{
							
							data.frame( node = params$node[ iii ],
										vintage = vv,
										year_all = params$year[ iii ],
										mode = params$mode[ iii ],
										node_out = params$node_out[ iii ],
										commodity = params$commodity[ iii ],
										level = params$level[ iii ],
										time = as.character( params$time[ iii ] ),
										time_out = as.character( params$time_out[ iii ] ),
										value = params$value[ iii ] )
							
							}
								
						} ) )
							
					}
							
				} ) )
				
			}
			
		return( df )
		
		}	

	}
	
add_construction_time = function( nodes, vintages, params ){
		
	# params = data.frame( vintages, value )
	
	df = bind_rows( lapply( vintages, function( vv ){
			
		res = data.frame( 	node = nodes,
							vintage = rep( vv, length(nodes) ),
							value = rep( params$value[which(vintages==vv)], length(nodes) ) )
								
		return(res)

		} ) )
			
	return(df)		
	
	}

	
		
add_technical_lifetime = function( nodes, vintages, params ){
	
	# params = data.frame( vintages, value )
	
	df = bind_rows( lapply( vintages, function( vv ){
			
		res = data.frame( 	node = nodes,
							vintage = rep( vv, length(nodes) ),
							value = rep( params$value[which(vintages==vv)], length(nodes) ) )
								
		return(res)

		} ) )
			
	return(df)		
	
	}
																													
add_inv_cost = function( nodes, vintages, params ){
	
	# params = data.frame( vintages, value )
	
	df = bind_rows( lapply( vintages, function( vv ){
			
		res = data.frame( 	node = nodes,
							year_all = rep( vv, length(nodes) ),
							value = rep( params$value[which(vintages==vv)], length(nodes) ) )
								
		return(res)

		} ) )
			
	return(df)		
	
	}
	
add_fix_cost = function( nodes, years, vintages, lifetime, params ){
	
	# params = data.frame( vintages, value )
	
	df = bind_rows( lapply( nodes, function( nn ){
			
		bind_rows( lapply( vintages, function( vv ){
			
			bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
				
				res = data.frame( 	node = nn,
									vintage = vv,
									year_all = yy,
									value = params$value[ match( vv, vintages ) ]  )
											
				return(res)
				
				} ) )
				
			} ) )	

		} ) )
			
	return(df)		
	
	}	
		
add_var_cost = function( nodes, years, vintages, lifetime, times, params ){
	
	# params = data.frame( mode, value ) 
	
	df = bind_rows( lapply( nodes, function( nn ){
			
		bind_rows( lapply( vintages, function( vv ){
			
			bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
				
				bind_rows( lapply( unique(params$mode), function( oo ){

				  if ("year_all" %in% names(params)){ val =c( params$value[ params$mode == oo & params$year_all == yy ] * duration_time)} else  {val =c( params$value[ params$mode == oo ] * duration_time ) }
				  # careful we use oo as value directly
					res = data.frame( 	node = rep(nn,length(times)),
										vintage = rep(vv,length(times)),
										year_all = rep(yy,length(times)),
										mode = rep(oo,length(times)),
										time = times,
										value = val )
												
					return(res)
					
					} ) )
				
				} ) )
				
			} ) )	

		} ) )
			
	return(df)		
	
	}	
		
add_capacity_factor = function( nodes, years, times, vintages, lifetime, params ){
	
	# params = data.frame( value ) 
	# or
	# params = data,frame( node, year_all, time, value ) 
	
	df = bind_rows( lapply( nodes, function( nn ){
	  			
		bind_rows( lapply( vintages, function( vv ){
			
			bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
			
				if( length(params) == 1 ){ val = params$value }else{ val = params$value[ which( params$node == nn ) ] }
				
				res = data.frame( 	node = rep(nn,length(times)),
									vintage = rep(vv,length(times)),
									year_all = rep(yy,length(times)),
									time = times,
									value = val  )
											
				return(res)
			
				} ) )
				
			} ) )	

		} ) )
		
	return(df)		
	
	}
	
add_emission_factor = function( nodes, years, times, vintages, lifetime, params ){
	
	# params = data.frame( mode, emission, value ) 
	
	df = bind_rows( lapply( nodes, function( nn ){
			
		bind_rows( lapply( vintages, function( vv ){
			
			bind_rows( lapply( years[ which( years %in% c( vv:min( years[length(years)], (vv+lifetime) ) ) ) ], function( yy ){
				
				res = data.frame( 	node = rep( nn, length( params$value ) ),
									year_all = rep( vv, length( params$value ) ),
									year_emission = rep( yy, length( params$value ) ),
									mode = params$mode,
									emission = params$emission,
									value = params$value  )
											
				return(res)
					
				} ) )
				
			} ) )	

		} ) )
			
	return(df)		
	
	}
		
add_bound_new_capacity_up = function( params ){
	
	# params = data.frame( node, year_all )
	
	df = params
			
	return(df)		
	
	}

add_bound_new_capacity_lo = function( params ){
	
	# params = data.frame( node, year_all )
	
	df = params
			
	return(df)		
	
	}

add_bound_total_capacity_up = function( years,params ){
	
	# params = data.frame( node, year_all )
    
    df = bind_rows( lapply( years, function( yy ){
      
      res = data.frame( node = params$node,
                        year_all = rep( yy, length(params$value) ),
                        value = rep( params$value, length(params$value) ),stringsAsFactors = F )
      
      return(res)
      
    } ) )
    
    return(df)		
	
	}

add_bound_total_capacity_lo = function( params ){
	
	# params = data.frame( node, year_all )
	
	df = params
			
	return(df)		
	
	}	
	
add_bound_activity_up = function( params ){

	# params = data.frame(node,year_all,mode,time)
	
	df = params
			
	return(df)		

	}

add_bound_activity_lo = function( params ){

	# params = data.frame(node,year_all,mode,time)
	
	df = params
			
	return(df)		

	}
	
add_min_utilization_factor = function( params ){
	
	# params = data.frame(node,vintage,year_all)
	
	df = params
			
	return(df)
	
	}		

add_historical_new_capacity = function( params ){
	
	
	# params = data.frame(node,year_all)
	
	df = params
			
	return(df)
	
	}	

	
pid_mapping = function(db){
  db <- db %>% 
    left_join(map_PID %>% rename(node = pids_original)) %>% 
    select(-node) %>% 
    rename(node = pids_after) %>% 
    select(node,everything())
  return(db)
}
