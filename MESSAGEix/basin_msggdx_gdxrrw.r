
# Launch GAMS connection
require( gdxrrw )
igdx( gams_path ) ## Set based on local machine	

##########################################################################
# Define additional sets and parameters usually generated within ixmp
########################################################################

# Update some parameters that are modified within ixmp when outputting to gdx
type_tec.set = c( 'all', type_tec.set )
cat_tec.set =  rbind( cat_tec.set, data.frame( type_tec = 'all', tec = technology.set ) )	

# Name of parameters to check for flagging constraints
chk = c('fixed_stock' ,
		'fixed_new_capacity' ,
		'fixed_capacity',
		'fixed_activity',
		'bound_new_capacity_up',
		'bound_new_capacity_lo',
		'bound_total_capacity_up',
		'bound_total_capacity_lo',
		'bound_activity_up',
		'bound_activity_lo',
		'bound_emission')

# The flags for the activity, capacity and emission bounds 
chk_lst = lapply( chk, function( bound ){ get( paste0( bound, '.par' ) ) %>% select( -value ) } )
for( iii in seq_along(chk_lst) ){ assign( paste0( 'is_', chk[iii], '.set' ), chk_lst[[ iii ]] ) }	

# Need to do manually for dynamic constraints that don't match naming exactly
is_dynamic_new_capacity_up.set = growth_new_capacity_up.par %>% select( -value )
is_dynamic_new_capacity_lo.set = growth_new_capacity_lo.par %>% select( -value )
is_dynamic_activity_up.set = growth_activity_up.par %>% select( -value )
is_dynamic_activity_lo.set = growth_activity_lo.par %>% select( -value )
		
# Commodity mapping to nodes and time periods
commodity.level = unique( c( paste( input.par$commodity, input.par$level, sep = '.' ), paste( output.par$commodity, output.par$level, sep = '.' ) ) )			

map_commodity.set = bind_rows(	output.par %>% select( node_out, commodity, level, year_all, time_out ) %>% rename( node = node_out, time = time_out ), 
								input.par %>% select( node_in, commodity, level, year_all, time_in ) %>% rename( node = node_in, time= time_in ) ) %>% 
						distinct( . )

# technology mapping to node and year		
params.list = lapply( technology.set, function( tt ){ get( tt ) } )
names(params.list) = technology.set

map_tec.set = bind_rows(	output.par %>% select( node, tec, year_all ), 
							input.par %>% select( node, tec, year_all ) ) %>% 
						distinct( . )

# technology mapping of mode to node, and year 	
map_tec_mode.set = bind_rows(	output.par %>% select( node, tec, year_all, mode ), 
								input.par %>% select( node, tec, year_all, mode ) ) %>% 
						distinct( . )

# technology mapping to time slicing
map_tec_time.set = bind_rows(	output.par %>% select( node, tec, year_all, time ), 
								input.par %>% select( node, tec, year_all, time ) ) %>% 
						distinct( . )
						
# additional druation period parameter
duration_period.par = data.frame( 
	year_all = year_all,
	value = sapply( seq_along(year_all), function(yyy){ if( yyy == 1 ){ 
		year_all[yyy+1] - year_all[yyy] }else{ year_all[yyy] - year_all[yyy-1] } 
		} )
	)	

# Time step duration	
duration_time.par = data.frame( time = time.set, value = rep( 1, length( time.set ) ) )
	
# MESSAGE_ix version
MESSAGE_ix_version.par = data.frame( version = c('major','minor'), value = c(1,0) )
			
# Update node set to include global region
node.set = c('World',node.set)
			
#################################################
# Compile sets and parameters
#################################################

set_list = lapply( ls(pattern='\\.set'), get )
names(set_list) = unlist( strsplit( ls(pattern='\\.set'), '[.]' ) )[seq(1,2*length(ls(pattern='\\.set')),by=2)]

# Process to ensure factors and attribute names align with gdx requirements
set_list = lapply( 1:length(set_list), function( iii ){ 
	df = data.frame( set_list[[ iii ]] )
	if( ncol( df ) == 1 ){ names( df ) = names( set_list )[ iii ] }
	df[] = lapply( df, factor )
	if( 'time' %in% names( df ) ){ df$time = factor( df$time, levels = time.set ) } # make sure ordered correctly
	attr( df, 'symName' ) = names( set_list )[ iii ]
	attr( df, 'type' ) = 'set'
	return(df)
	} )				

par_list = lapply( ls(pattern='\\.par'), get )
names(par_list) = unlist( strsplit( ls(pattern='\\.par'), '[.]' ) )[seq(1,2*length(ls(pattern='\\.par')),by=2)]

# Process to match gdx requirements					
par_list = lapply( seq_along(par_list), function( iii ){
	df = data.frame( par_list[[ iii ]] )
	df[names(df)[1:(ncol(df)-1)]]=lapply( df[names(df)[1:(ncol(df)-1)]], factor )
	df[names(df)[ncol(df)]]=lapply( df[names(df)[ncol(df)]], as.numeric )
	if( 'time' %in% names( df ) ){ df$time = factor( df$time, levels = time.set ) } # make sure ordered correctly
	attr( df, 'symName' ) = names( par_list )[ iii ]
	attr( df, 'type' ) = 'parameter'
	return(df)
	} )
	
#################################################
# Output to GDX
#################################################

print( 'Generating GDX' )

fl = paste( indus_ix_path, '/model/data', '/MSGdata_', data_gdx, '.gdx',sep='') 

# Write to gdx 
wgdx.lst( fl, c( set_list, par_list ), squeeze = FALSE )

