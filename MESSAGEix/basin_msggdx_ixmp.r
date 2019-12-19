options(java.parameters = "-Xmx16g")
# launch the IX modeling platform using the local default database
require( 'retixmp' )
ixmp = import('ixmp')
message_ix <- import('message_ix')
mp = ixmp$Platform(dbtype='HSQLDB')

model = "message_indus"
scen = "configuration_test"
annot = "developing the indus basin model" 

scen = message_ix$Scenario(mp, model, scen, version = 'new', annotation=annot)
#scen = message_ix$Scenario(mp,model, scen)

# list of sets, maps and parameter available
# set_check_list = objects()[grep('.set',objects())]    # single index sets, vectors in R
# par_check_list = objects()[grep('.par',objects())]    # parameters, dataframes in R

# Create lists containing parameters and force to character string
set_list = list(	node = node.set,
					year = year.set,
					commodity = commodity.set,
					level = level.set,
					lvl_spatial = lvl_spatial.set,
					lvl_temporal = lvl_temporal.set,
					time = time.set,
					type_tec = type_tec.set,
					type_year = type_year.set,
					type_emission = type_emission.set,
					technology = technology.set,
					grade = grade.set,
					mode = mode.set,
					emission = emission.set,
					rating = rating.set,
					#in theory only if useing the share eq
					shares = shares.set
					)

map_list = list(	cat_emission = cat_emission.set,
					cat_year = cat_year.set,
					cat_tec = cat_tec.set,
					map_spatial_hierarchy = map_spatial_hierarchy.set,
					map_temporal_hierarchy = map_temporal_hierarchy.set,
					map_time = map_time.set,
					map_node = map_node.set,
					#if share constraint
					map_shares_commodity_share = map_shares_commodity_share.set,
					map_shares_commodity_total = map_shares_commodity_total.set
					)


par_list = list(	bound_activity_lo = bound_activity_lo.par, 
                 bound_activity_up = bound_activity_up.par, 
                 bound_total_capacity_up = bound_total_capacity_up.par,    
                 bound_total_capacity_lo = bound_total_capacity_lo.par,    
                 bound_emission = bound_emission.par,  # might generate error is parameter does not exist, no policy create it
                 capacity_factor = capacity_factor.par, 
                 construction_time = construction_time.par,   
                 demand = demand.par,  
                 duration_time = duration_time.par, 
                 emission_factor = emission_factor.par, 
                 fix_cost = fix_cost.par, 
                 historical_new_capacity = historical_new_capacity.par,  
                 input = input.par, 
                 interestrate = interestrate.par,  
                 inv_cost = inv_cost.par, 
                 min_utilization_factor = min_utilization_factor.par, 
                 output = output.par, 
                 technical_lifetime = technical_lifetime.par,  
                 var_cost = var_cost.par ,
                 # for share of renewables
                 share_commodity_lo = share_commodity_lo.par
				 share_commodity_up = share_commodity_up.par
) 

nms = names( par_list )						
par_list = lapply(seq_along( par_list ), function( iii ){ par_list[[iii]][NROW(par_list[[iii]]) >0] } )
names( par_list ) = nms		
par_list = par_list[lengths(par_list,use.names = T) >0]

## Add SETs
res = lapply( seq_along(set_list), function(set){
	scen$add_set( names(set_list[set]), adapt_to_ret(set_list[[set]] ) )
	} )

## Add MAPPING SETs
res = lapply( seq_along(map_list), function(map){
	tmp_name = scen$idx_names(names(map_list)[map])
	tmp_map = map_list[[map]]
	names(tmp_map) = c(tmp_name)
	scen$add_set(names(map_list[map]),adapt_to_ret(tmp_map))
	} )

## Add PARAMETERs

res = lapply( seq_along(par_list), function(par){
	tmp_name = scen$idx_names(names(par_list)[par])
	tmp_par = par_list[[par]]
	names(tmp_par) = c(tmp_name,'value')
	tmp_par$unit = '-'
	scen$add_par(names(par_list[par]), adapt_to_ret(tmp_par))
	print(paste0( '----- ', names(par_list[par]), ' loaded') )
	} )

##### INITIALIZE NEW SETS, VARIABLES AND EQUATIONS

scen$init_set( "full_balance", c( "commodity" ) )
scen$init_equ( "COMMODITY_BALANCE_FULL", c( "node", "commodity", "level", "year", "time" ) )
	
scen$init_set( "map_storage", c( "node" , "commodity", "level", "year", "time" ) )	
new_map_list = list( map_storage = data.frame( lapply( map_storage.set, as.character), stringsAsFactors=FALSE ),
                     full_balance = data.frame(commodity =  full_balance.set ,stringsAsFactors=FALSE))	
res = lapply( seq_along(new_map_list), function(map){
	tmp_name = scen$idx_names(names(new_map_list)[map])
	tmp_map = new_map_list[[map]]
	names(tmp_map) = c(tmp_name)
	scen$add_set(names(new_map_list[map]),adapt_to_ret(tmp_map))
	} )

scen$init_par( "bound_storage_lo", c( "node" , "commodity", "level", "year", "time" ) )	
scen$init_par( "bound_storage_up", c( "node" , "commodity", "level", "year", "time" ) )		
# NEED to initialize storage loss
scen$init_par('storage_loss',c('node','commodity','level','year','time') )

new_par_list = list(	
	  bound_storage_lo = bound_storage_lo.par, 
    bound_storage_up = bound_storage_up.par,
	  storage_loss = storage_loss.par
	  )	

res = lapply( seq_along(new_par_list), function(par){
	tmp_name = scen$idx_names(names(new_par_list)[par])
	tmp_par = new_par_list[[par]]
	names(tmp_par) = c(tmp_name,'value')
	tmp_par$unit = '-'
	scen$add_par(names(new_par_list[par]), adapt_to_ret(tmp_par))
	print(paste0( '----- ', names(new_par_list[par]), ' loaded') )
	} )

scen$init_equ( "STORAGE_BALANCE", c( "node", "commodity", "level", "year", "time" ) )	
scen$init_equ( "STORAGE_BALANCE_UP", c( "node", "commodity", "level", "year", "time" ) )	
scen$init_equ( "STORAGE_BALANCE_LO", c( "node", "commodity", "level", "year", "time" ) )									

#### COMMIT and output to GDX

print( 'Generating GDX' )
comment = 'initial test commit for Indus model'
scen$commit(comment)
scen$set_as_default()

scen$to_gdx( paste( indus_ix_path, '/model/data', sep=''), paste( 'MSGdata_', scname, '.gdx',sep='') )

