
##	node	world - regions - countries - grid cells
node.set = c( 	node,
				unique( unlist( strsplit( as.character( basin.spdf@data$PID ), '_' ) )[ seq( 1, 2*length( basin.spdf ), by = 2 ) ] ) )

## map_node
map_node.set = bind_rows( data.frame( node1 = node, node2 = node, stringsAsFactors = F) %>% 
		bind_rows( data.frame( node1 = c( unique( as.character( basin.spdf@data$BASIN ) ) ), node2 = c( node ), stringsAsFactors = F) ) %>% 
		unique(),
	data.frame( node1 = c(  unlist( strsplit( as.character( basin.spdf@data$PID ), '_' ) )[ seq( 1, 2*length( basin.spdf ), by = 2 ) ],
							unique(unlist( strsplit( as.character( basin.spdf@data$PID ), '_' ) )[ seq( 1, 2*length( basin.spdf ), by = 2 ) ]) ), 
				node2 = c( as.character( basin.spdf@data$PID ), unique(unlist( strsplit( as.character( basin.spdf@data$PID ), '_' ) )[ seq( 1, 2*length( basin.spdf ), by = 2 ) ] ) ) )	)
map_node.set = bind_rows( map_node.set, data.frame( node1 = basin, node2 = unique(unlist( strsplit( as.character( basin.spdf@data$PID ), '_' ) )[ seq( 1, 2*length( basin.spdf ), by = 2 ) ] ) ) )
				
## node categoriea cat_node
cat_node.set = data.frame( type_node = factor(), node = factor() )

## type_node
type_node.set = unique( cat_node.set$type_node )
				
##	year(year_all)		years included in a model instance (for myopic or rolling-horizon optimization)	
year.set = year_all

##	type_year		types of year aggregations 
years.list = lapply( year_all, function(yy){return(yy)} ) 
names(years.list) = year_all
years.list$baseyear = baseyear
years.list$firstmodelyear = 2020
years.list$initializeyear_macro = 2020
years.list$lastmodelyear = 2060
years.list$cumulative = year_all		
type_year.set = names(years.list)	

##  cat_year(type_year,year_all)  	mapping of years to respective categories				
cat_year.set = do.call( rbind, lapply( names(years.list), function(tt){ data.frame( type_year = rep( tt, length( years.list[[tt]] ) ), year_all = years.list[[tt]] ) } ) )

##	time		subannual time periods (seasons - days - hours) 		
time.set = c( time, 'year' ) # Need to add year to allow for output on annual basis

## map_time
map_time.set = data.frame(time1 = c( 'year', rep('year', length(time) ), time ), time2 = c('year', time, time), stringsAsFactors = F) 
							
##	lvl_spatial		hierarchical levels of spatial resolution 
lvl_spatial.set =  c('bcu','national')
					
## map_spatial_hierarchy
map_spatial_hierarchy.set = bind_rows( 	data.frame(lvl_spatial = 'bcu',node = bcus, node_parent = basin, stringsAsFactors = F) ,
										data.frame(lvl_spatial = 'national',node=unique(unlist(strsplit(bcus,'_'))[seq(1,2*length(bcus),by=2)]),node_parent = basin, stringsAsFactors = F) )	

##	lvl_temporal		hierarchical levels of temporal resolution 
lvl_temporal.set = c( 'year', 'sub_1' )

## map_temporal_hierarchy
map_temporal_hierarchy.set = data.frame( lvl_temporal = c( 'year', rep( 'sub_1', length(time) ) ), time = c( 'year' , time ), time2 = 'year', stringsAsFactors = F) 
	
##	rating		identifies the 'quality' of the renewable energy potential (bins acc. to Sullivan)     
rating.set = 1

## sets for share constraints
shares.set = factor()
map_shares_commodity_total.set = data.frame(shares = factor(),
                                            node_share = factor(),
                                            node = factor(),
                                            type_tec = factor(),
                                            mode = factor(),
                                            commodity = factor(),
                                            level = factor() )

map_shares_commodity_share.set = data.frame(shares = factor(),
                                            node_share = factor(),
                                            node = factor(),
                                            type_tec = factor(),
                                            mode = factor(),
                                            commodity = factor(),
                                            level = factor() )


## 	duration_period(year_all)		duration of one multi-year period (in years)
duration_period.par = data.frame(	year = year.set, 
									value = c( diff(year_all), diff(year_all)[ length(diff(year_all)) ] ), 
									stringsAsFactors = F )


## 	duration_time(time)		duration of one time slice (relative to 1)
duration_time.par = data.frame(	time = time.set, 
								value = 1, 
								stringsAsFactors = F )

## 	interestrate(year_all)		interest rate (to compute discount factor)
interestrate.par = data.frame(	year = year.set, 
								value = rep(0.05,length(year_all)), 
								stringsAsFactors = F )

# mapping of stocks to commodities	- no stocks currently							
map_stocks.set = data.frame(	node=factor(),
								commodity=factor(),
								level=factor(),
								year=factor())								
								
# Commodity in stock					
commodity_stock.par = data.frame(	node=factor(),
									commodity=factor(),
									level=factor(),
									year=factor(),
									value = numeric())								
								
								
## 	demand(node,commodity,level,year_all,time) 		exogenous demand levels
demand.par = demand.df %>% dplyr::select( node, commodity, level, year_all, time, value)
							
##	tec		technologies
# Create list of technology parameters				
params.list = lapply( technology.set, function( tt ){ get( tt ) } )
names(params.list) = technology.set
inv_tec.set = technology.set

# get parameter names and remove redundancies
vars = unique( unlist( sapply( technology.set, function( tt ){ names( params.list[[ tt ]] ) } ) ) )
vars = vars[ which( !( vars %in% c( 'nodes', 'modes', 'years', 'times', 'vintages', 'types', 'lifetime' ) ) ) ]

# Initialize optional parameters - these can be written over but need to be initialized and passed to the GDX 
# especially in instances where ixmp isnt used.
historical_activity.par = data.frame(node=factor(),tec=factor(),year_all=factor(),mode=factor(),time=factor(),value=numeric()) #   historical acitivity
historical_new_capacity.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric()) #   historical new capacity
historical_emission.par = data.frame(node=factor(),emission=factor(),type_tec=factor(),year_all=factor(),value=numeric()) #  historical emissions by technology type (including land)
fixed_stock.par = data.frame(node=factor(),commodity=factor(),level=factor(),year_all=factor(),value=numeric()) #          fixed stock level
fixed_new_capacity.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric()) #               fixed new-built capacity
fixed_capacity.par = data.frame(node=factor(),tec=factor(),vintage=factor(),year_all=factor(),value=numeric()) #          fixed maintained capacity
fixed_activity.par = data.frame(node=factor(),tec=factor(),vintage=factor(),year_all=factor(),mode=factor(),time=factor(),value=numeric()) #fixed activity level
initial_new_capacity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric()) #    dynamic upper bound on new capacity (fixed initial term)
initial_new_capacity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric()) #   dynamic lower bound on new capacity (fixed initial term)
initial_activity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),time=factor(),value=numeric()) #  dynamic upper bound on activity (fixed initial term)
initial_activity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),time=factor(),value=numeric()) #  dynamic lower bound on activity (fixed initial term)
growth_new_capacity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric()) #    dynamic lower bound on new capacity (growth rate)
growth_new_capacity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric())    #  dynamic upper bound on new capacity (growth rate)
growth_activity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),time=factor(),value=numeric()) #   dynamic upper bound on activity (growth rate)
growth_activity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),time=factor(),value=numeric()) #  dynamic lower bound on activity (growth rate)
bound_activity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),mode=factor(),time=factor(),value=numeric())
bound_activity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),mode=factor(),time=factor(),value=numeric())
bound_new_capacity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric())
bound_new_capacity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric())
bound_total_capacity_up.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric())
bound_total_capacity_lo.par = data.frame(node=factor(),tec=factor(),year_all=factor(),value=numeric())
bound_storage_lo.par = data.frame(node=factor(),commodity=factor(),level=factor(),year=factor(),time=factor(),value=numeric())
bound_storage_up.par = data.frame(node=factor(),commodity=factor(),level=factor(),year=factor(),time=factor(),value=numeric())
bound_emission.par = data.frame(node=factor(),type_emission=factor(),type_tec=factor(),type_year=factor(),value=numeric())		
operation_factor.par = data.frame(node=factor(),tec=factor(),vintage=factor(),year_all=factor(),value=numeric()) #	yearly total operation factor
emission_factor.par = data.frame(node = factor(),tec=factor(),vintage=factor(),year_all=factor(),mode=factor(),emission=factor(),value=numeric())
emission_scaling.par = data.frame(type_emission=factor(),emission=factor(),value=numeric()) #               scaling factor to harmonize bounds or taxes across tpes
tax_emission.par = data.frame(node=factor(),type_emission=factor(),type_tec=factor(),type_year=factor(),value=numeric()) #   emission tax
peak_load_factor.par = data.frame(node=factor(),commodity=factor(),level=factor(),year_all=factor(),time=factor(),value=numeric()) #  maximum peak load factor for reliability constraint of firm capacity
rating_bin.par = data.frame(node=factor(),tec=factor(),year_all=factor(),commodity=factor(),level=factor(),time=factor(),rating=factor(),value=numeric()) # maximum share of technology in commodity use per rating
reliability_factor.par = data.frame(node=factor(),tec=factor(),year_all=factor(),commodity=factor(),level=factor(),time=factor(),rating=factor(),value=numeric()) #		reliability of a technology (per rating)
share_commodity_lo.par = data.frame(shares=factor(),node_share=factor(),year_all=factor(),time=factor(),value=numeric())

##	tec		technologies
# Create list of technology parameters				
params.list = lapply( technology.set, function( tt ){ get( tt ) } )
names(params.list) = technology.set
inv_tec.set = technology.set

# get parameter names and remove redundancies
vars = unique( unlist( sapply( technology.set, function( tt ){ names( params.list[[ tt ]] ) } ) ) )
vars = vars[ which( !( vars %in% c( 'nodes', 'modes', 'years', 'times', 'vintages', 'types', 'lifetime' ) ) ) ]

# go through parameters and create the data frames covering the entire technological system 
for( i in seq_along(vars)){ 
	
	assign( paste0( vars[i], '.par'), 
			
		bind_rows( lapply( technology.set, function( tt ){ 
			
			df = params.list[[ tt ]][ vars[i] ][[1]] 
			
			if( !is.null(df) ){ if( nrow( df ) > 0 ){ 
			
				df$tec = tt 
				
				df = df[ , c( 'node', 'tec', names(df)[ which( !( names(df) %in% c( 'node', 'tec' ) ) ) ] ) ]
				
				} }
			
			return( df )	
				
			} ) ) 
		
		) 
		
	}

##	commodity		resources - electricity - water - land availability - etc.
commodity.set = unique( c( 	input.par$commodity, 
							output.par$commodity,
							demand.par$commodity ) )

## full_balance(commodity)		commodities to include in full commodity balance
full_balance.set = c('freshwater','wastewater','crop_land',paste0(crop_names,'_land'),paste0(crop_names,'_yield'))

##	level		levels of the reference energy system or supply chain ( primary - secondary - ... - useful )			
level.set = unique( c( 	input.par$level, 
						output.par$level,
						demand.par$level ) )
			
##	type_tec		types of technologies			
type_tec.set = unique( unlist( lapply( technology.set, function( iii ){ c( as.character( params.list[[ iii ]]$type ) ) } ) ) )
	
##	cat_tec(type_tec,tec)		mapping of technologies to respective categories		

type.list = lapply(	type_tec.set,  function( xx ){ 

	unlist( lapply( names( params.list ), function( iii ){  
	
		if( xx %in% as.character( params.list[[ iii ]]$type ) ){ return( iii ) } 
		
		} ) ) 
		
	} )
names(type.list) = type_tec.set

cat_tec.set = do.call( rbind, lapply( type_tec.set, function( ttt ){ data.frame( type_tec = ttt, tec = technology.set[ which( technology.set %in% type.list[[ ttt ]] ) ] ) } ) )
	
##  inv_tec(tec)		technologies that have explicit investment and capacity decision variables

inv_tec.set = technology.set
 
 
##	grade		grades of extraction of raw materials    				

grade.set =  c( 1 )


##	mode		modes of operation

mode.set = unique( unlist( c( input.par$mode, output.par$mode ) ) )

	
##	emission		greenhouse gases - pollutants - etc.	
		
emission.set = unique( unlist( lapply( names( params.list ), function( iii ){ 
	
	return( c( as.character( unlist( params.list[[ iii ]]$emission_factor$emission ) ) ) )
	
	} ) ) )	

##	type_emission		types of emission aggregations

type_emission.set = emission.set


##	cat_emission(type_emission,emission)		mapping of emissions to respective categories	
					
cat_emission.set = data.frame(type_emission = type_emission.set, emission = emission.set, stringsAsFactors = F)							


## map_storage(node,commodity,level,year_all,time)   mapping of commodity-level to node and time
	
stock_commodity = c( "freshwater" )
stock_level = c( "river_in" )
map_storage.set = expand.grid( node = bcus, commodity = stock_commodity, level = stock_level, year_all = year_all, time = time )


## bound_storage_up(node,commodity,level,year,time)
# dam_storage_capacity_2020 = dam_storage_capacity.df %>% filter(year == 2020) %>% select(-year) %>% 
#   rename(`1990` = value) %>% 
#   mutate(`2000` = `1990`, `2010` = `1990`, `2015` = `1990` ,
#          `2020` = `1990`, `2030` = `1990` , `2040` = `1990`, `2050` = `1990`, `2060` = `1990`) %>% 
#   gather(year,value,2:10)

bound_storage_up.par = left_join(	dam_storage_capacity.df,
									crossing( 	year = dam_storage_capacity.df$year, 
													time = time, 
													commodity = stock_commodity, 
													level = stock_level ) ) %>%
										dplyr::select( node, commodity, level, year, time, value )

map_bound_ratio = rule_curves.df %>% select(time,max_min,ratio_avg_max) %>% 
  mutate(time = as.factor(time))

## bound_stock_lo(node,commodity,level,year,time)

bound_storage_lo.par = bound_storage_up.par %>% 
  left_join(map_bound_ratio %>% filter(max_min == 'min')) %>% # take lower rule curve multiplocator
  mutate(value = value * ratio_avg_max) %>% 
  select(-max_min,-ratio_avg_max) %>% 
  mutate(value = if_else( time %in% c(1,12) ,value * 0.8, value *0.8) ) # scale everything down to give some margin

diff_to_smooth2030 = bound_storage_lo.par %>% spread(year,value) %>% 
  select(node,time,`2020`,`2030`) %>% group_by(node) %>% 
  mutate(first_diff = (first(`2030`) - first(`2020`)) / 12 , time = as.numeric(time)) %>% 
  filter(first_diff > 0) %>% arrange(node,-time) %>% 
  mutate(cumsum_diff = cumsum(first_diff)) %>% select(node,time,cumsum_diff)

bound_storage_lo.par = bound_storage_lo.par %>% left_join(diff_to_smooth2030 %>% mutate(time = as.character(time))) %>% 
  mutate(value = if_else(year == 2030 & !is.na(cumsum_diff),value - cumsum_diff,value)) %>% 
  select(-cumsum_diff)

bound_storage_up.par = bound_storage_up.par %>% 
  left_join(map_bound_ratio %>% filter(max_min == 'max')) %>% # take upper rule curve multiplicator
  mutate(value = value * ratio_avg_max) %>% 
  select(-max_min,-ratio_avg_max)

# bound_storage_up.par = bound_storage_up.par %>% 
# 	left_join( avg_storage_multiplier ) %>% 
# 	mutate( value = if_else( time %in% c(1,12) ,value * avg_strg,value ) ) %>% 
#   filter(year <= lastyear) %>% 
# 	dplyr::select( -avg_strg )

# maximum variability of storage!!
# a = rule_curves.df %>% group_by(max_min) %>% 
#   mutate(max_d = max(abs(diff_on_max)))
# mean(a$max_d)

## Evaporative losses 

storage_loss.par = evap_losses.df %>% 
  mutate(commodity = 'freshwater',level = 'river_in') %>% 
  select(node,commodity,level,year,time,value) 
 
# Remove large redundant list of data	
rm( params.list )	

## Additional mca parameters for cumulative emission constraints
emission_1.set = c('CO2eq')
emission_2.set = c('groundwater')
emission_3.set = c('BCA')
emission_4.set = c('N')
emission_5.set = c('wind_credit')
emission_6.set = c('solar_credit')
emission_7.set = c('env_flow')
type_tec_1.set = type_tec_2.set = type_tec_3.set = type_tec_4.set = type_tec_5.set = type_tec_6.set = type_tec_7.set = 'all'



	
	