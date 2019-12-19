
###### Policies  #######


# policy_option = paste0('_EF.',substr(ENV_FLOWS, 1,1),'_IT.',substr(IND_TREAT, 1,1),'_EM.',substr(EMISS, 1,1),
#                        '_S6.',substr(SDG6, 1,1),'_S7.',substr(SDG7, 1,1),'_S2.',substr(SDG2, 1,1))
# Options for sensitivity analysis, default options re not declared here
GROUNDWAT.opt = F #c('low')    # options: 'low', 'high'
YIELD_DEM.opt = F #c('flat')   # options: 'flat', 'low' 
 
## Reduced fresh surface runoff into system by X %
fraction_reduction = 0.25
if (!REDUCE_RUNOFF){}else{ 
  demand.par = demand.par %>% 
    mutate( value = if_else(level == 'inflow' & grepl('PAK',node),value * (1 - fraction_reduction ), value ) ) 
  } 
 
## Maintain surface withdrawal capacity to limit expanded surface allocation (surface water fully allocated in Indus ie.., no room for expanded use) 

if (!exists('SURFACE_ALLOCATION')) {} else{	
  # surface and groundwater allocation
  
  type = as.character(SURFACE_ALLOCATION)
  if (!type %in% c('UR','A','ALL')) {} else {
  historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE )
	
	if( SDG6 ){ delt = 1 }else{ delt = 0.5 }
	
			# not sure whether we should cum over historical year or just take values in 2015
		  # sometimes it seems 2010 and 2015 values are the same, as if they were tot capacity, not new
		  if (type == c('ALL')){
				surface_alloc.df = historical_capacity.df1 %>% 
					filter( tec %in% c('urban_sw_diversion','rural_sw_diversion','irrigation_sw_diversion') ) %>% 
					group_by( node ) %>%
		      filter(year_all == 2015) %>% 
					summarise( value = sum( value ) * delt ) %>%
					ungroup() %>% data.frame() %>% 
					mutate( tec = 'sw_extract' ) %>% 
				  mutate(value = if_else(value == 0, 0.01, value))
				
		  } else {
		    if (type == 'UR') surf_tec = c('urban_sw_diversion','rural_sw_diversion')
		    if (type == 'A') surf_tec = c('irrigation_sw_diversion','irrigation_gw_diversion') # I added groundwater here
		    
		    surface_alloc.df = historical_capacity.df1 %>% 
		      filter( tec %in% surf_tec  ) %>%
		      group_by( node,tec ) %>%
		      filter(year_all == 2015) %>% 
		      summarise( value = sum( value ) * delt ) %>%
		      ungroup() %>% data.frame() %>% 
		      mutate(value = if_else(value == 0, 0.01, value))
		      
		  }
  
  bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par,
                                           
          bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
            surface_alloc.df %>% 
              filter(!grepl('CHN',node)) %>% 
              mutate( year_all = yyy ) %>%
              select( node, tec, year_all, value )
				} ) ) 
		)		
	
  }
}
 



## Environmental flows (Policy set#1  distributed case for every node)) 
# Indus can't be transformed to naturalized conditions so better to focus on improving EFs instead of basis on natural flows
# see: Acreman et al "Environmental flows for natural, hybrid, and novel riverine ecosystems in a changing world"

if ( exists('ENV_FLOWS1') & sc != baseline ){
  
  fractional_increment_ef = as.numeric(ENV_FLOWS1)  
  if (fractional_increment_ef == 0){} else { 
    
    # Import baseline solution from gdx
    igdx( gams_path )
    upath = paste( indus_ix_path, '/model/output/', sep='')
    fpath = paste( 	'MSGoutput_', baseline, '.gdx', sep = '' )	
    vv = c('ACT')
    tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
    names(tmp$uels) = tmp$domains
    rs = data.frame( tmp$val )
    names(rs) = c( unlist( tmp$domains ), 'value' )
    rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ 
      sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) 
    } ) )
    rs = rs %>% filter( tec == 'environmental_flow' )
    rm( tmp )
    
    bound_activity_lo.par = rbind( as.data.frame(as.matrix(bound_activity_lo.par)) %>% 
                                     mutate(value = as.numeric(value))
                                            , rs %>%
                                     mutate( value = value * fractional_increment_ef) %>% 
                                     select(node,tec,year_all,mode,time,value))
    
  }
}
## Environmental flows (EF) (Policy Set#2: Centralized/basin=aggregated case just at specific nodes, in this case PAK_4)
# Indus can't be transformed to naturalized conditions so better to focus on improving EFs instead of basis on natural flows
# see: Acreman et al "Environmental flows for natural, hybrid, and novel riverine ecosystems in a changing world"


 
if ( exists('ENV_FLOWS2') & sc != baseline ){
  
fractional_increment_ef = as.numeric(ENV_FLOWS2)  
  if (fractional_increment_ef == 0){} else { 
  
# Import baseline solution from gdx
  igdx( gams_path )
  upath = paste( indus_ix_path, '/model/output/', sep='')
  fpath = paste( 	'MSGoutput_', baseline, '.gdx', sep = '' )	
  vv = c('ACT')
  tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
  names(tmp$uels) = tmp$domains
  rs = data.frame( tmp$val )
  names(rs) = c( unlist( tmp$domains ), 'value' )
  rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ 
    sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) 
  } ) )
  rs = rs %>% filter( tec == 'environmental_flow' )
  rm( tmp )
  
  pak_4_incremented = rs %>%
    filter(node %in% c('PAK_4')) %>%
    mutate( value = value * fractional_increment_ef) %>% 
    select(node,tec,year_all,mode,time,value)
  
  bound_activity_lo.par = rbind( as.data.frame(as.matrix(bound_activity_lo.par)) %>% 
                                   mutate(value = as.numeric(value)), 
                                 pak_4_incremented )
  
}
}

## Environmental flows (baseline (local policy)) 
# Local Policy for releasing flows below Kotri Barrage (Ref: Final Report of IPOE , Gonzalez et al. 2005) 
# 46.2117444 MCM/day monthly during Rabi (Month 10 -3) & 12.2328779 MCM/day monthly during Kharif season (Month 4-9) 
# Total flow to be released annually =  350.675903 MCM/day 

if (!ENV_FLOWS){}else{
  
  pak_4_incremented = bound_activity_lo.par %>% filter((tec == 'environmental_flow' & node == 'PAK_4')) %>% 
    rename(val_inc = value) %>% as.matrix() %>% as.data.frame() %>% 
    mutate(node = as.character(node),
           time = as.character(time),
           tec = as.character(tec),
      year_all = as.numeric(year_all),
           mode = as.numeric(mode),
           val_inc = as.numeric(val_inc))
  
  bound_activity_lo.par =   bound_activity_lo.par %>% filter(!(tec == 'environmental_flow' & node == 'PAK_4') )
  # only for PAK_4, avoid writing twoce
  value_by_time =  data.frame(val = c(12.23, 12.23, 12.23, 46.21, 46.21 ,46.21, 46.21, 46.21, 46.21, 12.23, 12.23, 12.23),
                              time = as.character(seq(1,12,1)), stringsAsFactors = F )
  
  pak_4_pol = environmental_flow.df %>% mutate(tec = 'environmental_flow',mode = 1,time = as.character(time)) %>%  
    filter(node %in% c('PAK_4'), year_all >2015) %>% left_join(value_by_time) %>% 
    mutate(value = val) %>%  ungroup() %>% # already in MCM 
    dplyr::select(node,tec,year_all,mode,time,value)
  
  # pak_4_pol = pak_4_incremented %>% 
  #   left_join( pak_4_pol0) %>% 
  #   group_by(node,tec,year_all,mode,time) %>% 
  #   mutate(value = max(value,val_inc)) %>% ungroup() %>% 
  #   dplyr::select(node,tec,year_all,mode,time,value)
    
  
  bound_activity_lo.par = rbind( as.data.frame(as.matrix(bound_activity_lo.par)) %>% 
                                   mutate(value = as.numeric(value),
                                          year_all = as.numeric(year_all)),  
                                 pak_4_pol ) %>% mutate(mode = as.numeric(mode)
                                 
                                 
  )
  
} 

## Natural Flow (To check natural flows using model) 
if (!NAT_FLOWS){}else{  
  input.par = input.par %>%  
    mutate(value = if_else(tec == 'sw_extract',0,value)) 
  
  bound_storage_up.par = bound_storage_up.par %>%  
    group_by(node,commodity,level,time) %>%  
    mutate(value = 0) %>% 
    as.data.frame( )	 
  
  bound_storage_lo.par = bound_storage_lo.par %>%  
    group_by(node,commodity,level,time) %>%  
    mutate(value = 0) %>% 
    as.data.frame( ) 
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
                  year_all = year_all[ which( year_all > baseyear ) ], 
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
  historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
  # bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par,  
  # 	 
  # 	bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){ 
  # 	 
  # 		historical_capacity.df1 %>%  
  # 			filter( tec == 'irrigation_sw_diversion', node %in% policy_nodes ) %>%  
  # 			group_by( node, tec ) %>% 
  # 			summarise( value = sum( value ) + 1e-6 ) %>% 
  # 			ungroup() %>% data.frame() %>% 
  # 			mutate( year_all = yyy ) %>%  
  # 			dplyr::select( node, tec, year_all, value ) 
  # 		 
  # 		} ) ) 
  # 	 
  # 	) 
  
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
                                                     year_all = factor(year_all),  
                                                     mode = factor(1), 
                                                     time = factor(as.character( time )),  
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
                                                          year_all = year_all[year_all>baseyear], 
                                                          value = 1e-6 )  
                                           } ) ) 
  )						 
  
} #end if 


## Maintain surface withdrawal capacity to limit expanded surface allocation (surface water fully allocated in Indus ie.., no room for expanded use)  
if( SURFACE_ALLOCATION ){	 
  
  # Get the historical capacity of surface water diversions and then bound total capacity of sw_extract to prevent further expansion 
  historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
  sw_extract_cap.df = historical_capacity.df1 %>%  
    filter( grepl( 'sw_diversion', tec ) ) %>%  
    group_by( node ) %>% 
    summarise( value = sum(value) ) %>%  
    as.data.frame( ) 
  
  # bound_total_capacity_up.par = data.frame( node=factor(),tec=factor(),year_all=factor(),value=numeric() ) 
  bound_total_capacity_up.par = bind_rows(  
    bound_total_capacity_up.par,  
    bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){ 
      sw_extract_cap.df %>% mutate( tec = 'sw_extract', year_all = yyy ) %>% dplyr::select( names( bound_total_capacity_up.par ) )  
    } ) )  
  )		 
  
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
						year_all = year_all[ which( year_all > baseyear ) ],
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
	historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE )
	# bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par, 
	# 	
	# 	bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
	# 	
	# 		historical_capacity.df1 %>% 
	# 			filter( tec == 'irrigation_sw_diversion', node %in% policy_nodes ) %>% 
	# 			group_by( node, tec ) %>%
	# 			summarise( value = sum( value ) + 1e-6 ) %>%
	# 			ungroup() %>% data.frame() %>%
	# 			mutate( year_all = yyy ) %>% 
	# 			dplyr::select( node, tec, year_all, value )
	# 		
	# 		} ) )
	# 	
	# 	)

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
							year_all = factor(year_all), 
							mode = factor(1),
							time = factor(as.character( time )), 
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
							year_all = year_all[year_all>baseyear],
							value = 1e-6 ) 
			} ) )
		)						
	
	} #end if
	

## Maintain surface withdrawal capacity to limit expanded surface allocation (surface water fully allocated in Indus ie.., no room for expanded use) 
if( SURFACE_ALLOCATION ){	
	
	# Get the historical capacity of surface water diversions and then bound total capacity of sw_extract to prevent further expansion
	historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE )
	sw_extract_cap.df = historical_capacity.df1 %>% 
		filter( grepl( 'sw_diversion', tec ) ) %>% 
		group_by( node ) %>%
		summarise( value = sum(value) ) %>% 
		as.data.frame( )
	
	# bound_total_capacity_up.par = data.frame( node=factor(),tec=factor(),year_all=factor(),value=numeric() )
	bound_total_capacity_up.par = bind_rows( 
		bound_total_capacity_up.par, 
		bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
			sw_extract_cap.df %>% mutate( tec = 'sw_extract', year_all = yyy ) %>% dplyr::select( names( bound_total_capacity_up.par ) ) 
			} ) ) 
		)		
	
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
if (SDG6 & sc != baseline){
  
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
	
	# Add water efficiency targets for irrigation
# 	eff_target = 0.33
# 	bound_total_capacity_up.par = bind_rows( bound_total_capacity_up.par,
# 	
# 		bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
# 			
# 				historical_capacity.df1 %>% 
# 					filter( tec %in% c('irrigation_sw_diversion','irrigation_gw_diversion') ) %>% 
# 					mutate( value = value * (1 - eff_target) ) %>%
# 					filter( value > 0 ) %>%
#   		    group_by( node, tec ) %>%
#   		    summarise( value = sum( value ) + 1e-6 ) %>%
#   		    ungroup() %>%
# 					mutate( year_all = yyy ) %>% 
# 					dplyr::select( node, tec, year_all, value )
# 				
# 				} ) )
# 		)

	} #end if

## GHG Emissions
if( EMISS & sc != baseline ){


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
	base_ems.df = rs %>% filter( node == basin, emission == 'CO2eq', type_tec == 'all' ) %>% dplyr::select( year_all, value ) 					
	rm( rs, tmp )

	bound_emission.par = data.frame( 	node = basin,
										type_emission = 'CO2eq',
										type_tec = 'all',
										type_year = base_ems.df$year_all,
										value = c(0.75,0.2,0.1,0.05,0.05) * base_ems.df$value )


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
if (SDG7 & sc != baseline){
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

# Limit electricity import 
if( LIMIT_IO ){ 
	
	# # # Add constraint on input / output
	# bound_activity_up.par = bind_rows( bound_activity_up.par, 
		# bind_rows( lapply( exp_routes_names, function( nnn ) { 
			# expand.grid( 	node = unlist( strsplit( unlist( strsplit( nnn, 'trs_' ) ), '[|]' ) )[1], 
							# tec = nnn,
							# year_all = factor(year_all), 
							# mode = factor(2), # limit the imports which occur in mode 2 of operation
							# time = factor(as.character( time )), 
							# value = max( c( 1e-6, historical_new_capacity.par%>%filter(tec==nnn)%>%dplyr::select(value)%>%unlist()*1 ), na.rm=TRUE ) ) # approx. 1 MAF  per yearconverted to MCM per day - Note Wescoat et al 2018 show declining flows to ~ 1 MAF per year
			# } ) )
		# )
	
	
	# use share equations
	shares.set = c( shares.set, 'io_electricity')

	type_tec.set = c(type_tec.set,'share_io')

	power_tec = c( (cat_tec.set %>% filter(type_tec == 'power' & !grepl('trs|distribution|ethanol|genset|biom',tec)) )$tec, exp_routes_names )

	cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'share_io',
													   tec = exp_routes_names))

	node_share.tmp = map_node.set %>% filter(node1 %in% c('AFG','IND','PAK') & !node2 %in% c('AFG','IND','PAK')) %>%
	  rename(node_share = node1, node = node2)
	  
	map_shares_commodity_total.set = bind_rows( map_shares_commodity_total.set,
												crossing(data.frame(shares = 'io_electricity',
											  type_tec = 'share_io',
											  mode = factor(c(1,2,3,4)),
											  commodity = 'electricity',
											  level = 'energy_secondary'),
											  node_share.tmp ) %>% 
		dplyr::select(shares,node_share,node,type_tec,mode,commodity,level) )

	share_commodity_up.par = crossing(data.frame(shares = 'io_electricity',
												 node_share = node_share.tmp$node_share,
												 unit = '%'),
									  crossing(data.frame(year_all = c(2020, 2030, 2040, 2050, 2060),
														  value =    c(0.25,  0.25,  0.25,  0.25,  0.25)),
											   time = time) ) %>% 
		dplyr::select(shares,node_share,year_all,time,value)
	
	}
	  

if(SDG2){
  
    # no flood irrigation after 2030, with the exception of rice
    bound_total_capacity_up.par = bound_total_capacity_up.par %>% bind_rows(
      crossing(node = node[ node != basin ], year_all = year_all[year_all>=2030], 
               tec = technology.set[grepl('flood',technology.set) & technology.set != 'irr_flood_rice'],value = 0	) )
  
	} #end if	
															
## Nonrenewable Groundwater extraction
if( GROUNDWAT & sc != baseline ){
	
	# Constrain fossil groundwater extraction to pct of baseline
	pct_bsl = c( 0.99, 0.5, 0.5, 0.5, 0.5 ) # 2020 to 2060 
	require( gdxrrw )
	igdx( gams_path ) ## Set based on local machine	
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''), 
								paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ), 
								list( name = 'EMISS', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	base_gw.df = rs %>% filter( node %in% bcus, emission == 'groundwater', type_tec == 'all' ) %>% dplyr::select( node, emission, type_tec, year_all, value ) 					
	rm( rs, tmp )
	tmp = crossing(node = node[-25] ,type_year =as.character( c(2020,2030,2040,2050,2060) )) 
	
	gw_bound = tmp %>% left_join(left_join( base_gw.df, data.frame( year_all = unique( base_gw.df$year_all ), val = pct_bsl ) ) %>% 
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

## Yiels Demand options
YIELD_DEM.opt = FALSE
if(exists('YIELD_DEM.opt')){
	if (YIELD_DEM.opt == 'low'){
    
		mult = data.frame(  year_all = c(2015,2020,2030,2040,2050,2060),
							mult = c(1,   0.9, 0.85,0.8, 0.75,0.7 ) )
    
		demand.par = demand.par %>% 
			left_join(mult) %>% 
			mutate(value = if_else(level == 'raw', value * mult, value )) %>% 
			dplyr::select(-mult)
		}
  
	if (YIELD_DEM.opt == 'flat'){
    
		demand.par = demand.par %>%  
			group_by(node,commodity,level,time) %>% 
			mutate(value = if_else(level == 'raw', first(value), value )) %>% 
			as.data.frame()
    
		}
	}
  

if(RELAX_LAND){
  
	## release area constraints
	expand_par = data.frame(year_all = c(1990,2000,2010,2015,2020,2030,2040,2050,2060),
                          mult_ind = c(1   ,1   ,1   ,1   ,1.5 ,1.8 ,2 ,2.3  ,2.5 ) ,
                          mult_pak = c(1   ,1   ,1   ,1   ,1.5 ,1.5 ,1.55,1.65 ,1.65 ))
						  
	demand.par = demand.par %>% filter(level != 'area') %>% 
		bind_rows(demand.par %>% filter(level == 'area') %>%
        left_join(expand_par) %>% 
		group_by(node,commodity,level,time) %>% 
		mutate(value = if_else(grepl('IND',node) , mult_ind * value , value ) ) %>%
		mutate(value = if_else(grepl('PAK',node) , mult_pak * value , value ) ) %>% 
		ungroup() %>% 
		dplyr::select(-mult_ind, -mult_pak))

	}

if(END_FOOD_IMPORT){
  
	imp_exp_add = imp_exp.df %>% rename(value_add = value) %>% filter(imp_exp == 'import') %>% 
		mutate(commodity = paste0(crop,'_yield')) %>% dplyr::select(node,commodity,value_add)
  
	demand.par = demand.par %>% 
		left_join(imp_exp_add) %>% 
		mutate(value = if_else(level == 'raw' & !is.na(value_add), value + value_add, value )) %>% 
		dplyr::select(-value_add)
  
	}


if(END_FOOD_EXPORT){
  
	imp_exp_add = imp_exp.df %>% 
		rename(value_add = value) %>% 
		filter(imp_exp == 'export') %>% 
		mutate(commodity = paste0(crop,'_yield')) %>% dplyr::select(node,commodity,value_add)
  
  # demand.par = demand.par %>% 
  #   left_join(imp_exp_add) %>% 
  #   mutate(value = if_else(level == 'raw' & !is.na(value_add), value - value_add, value )) %>% 
  #   dplyr::select(-value_add)
  
  # reduce of 2% per year
  # demand.par = demand.par %>% filter(level != 'raw') %>% 
  #   bind_rows(demand.par %>% filter(level == 'raw', year_all >= 2020) %>%
  #               group_by(node,commodity,level,time) %>% 
  #               mutate(value = first(value))  %>% # this is still not very precise, for crops that initially have decreasing demand
  #               mutate(value = value * (1 - ((year_all - 2020)/10 * 0.02 ) ) ) %>% 
  #               ungroup()
  #               )
  
	mult = data.frame( 	year_all = c(2015,2020,2030,2040,2050,2060),
						mult = c(1,   0.9, 0.85,0.8, 0.75,0.7 ) )
  
	demand.par = demand.par %>% 
		left_join(mult) %>% 
		mutate(value = if_else(level == 'raw', value * mult, value )) %>% 
		dplyr::select(-mult)
  
	}

# import upper bound
# fix electricity import to be lower or equal to the baseline, not to
if (FIX_ELEC_IMPORT){

	igdx( gams_path ) ## Set based on local machine
	tmp = rgdx( paste( paste( indus_ix_path, '/model/output/', sep=''),
					   paste( 	'MSGoutput_', baseline, '.gdx', sep = '' ), sep = '' ),
				list( name = 'ACT', form = "sparse" ) )
	names(tmp$uels) = tmp$domains
	rs = data.frame( tmp$val )
	names(rs) = c( unlist( tmp$domains ), 'value' )
	rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
	
	trs_bound = rs %>% 
		filter(tec %in% exp_routes_names) %>% 
		dplyr::select(node,tec,year_all,mode,time,value) %>%
		group_by(node,tec,year_all,mode,time) %>% 
		summarise(value = sum(value)) %>% 
		as.data.frame( )
		
	bound_activity_up.par = rbind( bound_activity_up.par, trs_bound )

	}

# if (!CROP_IMPORT) {} else{
#   mult.df = data.frame(year_all = c(2020,2030,2040,2050,2060),
#              mult = c(1,0.98,0.96,0.94,0.92))
#   a =  bind_rows( lapply( year_all[ year_all > baseyear ], function( yyy ){
#      demand.df %>% filter(level == 'raw', year_all == 2020) %>% 
#       mutate(year_all = yyy,
#              mult = mult.df[year_all == yyy,]$mult) %>% 
#       mutate(value = value * mult)
#   }))
# }
	
if (SMART_IRR_WATER){	
  # smart irrigation share constraints
  
  perc_red = 80
  
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
if(RAINFED_LAND){	
	# smart irrigation share constraints
  
	perc_red = 20
	
	if (perc_red == 0){} else {
    
		percentages_smart = data.frame( year_all = as.factor(c(2020, 2030, 2040, 2050, 2060)), value =  perc_red /100 )
		# use share equations
		# share on the whole use of freshwater.irrigation_final
		
		shares.set = c( shares.set, 'rainfed_land' )
		
		type_tec.set = c(type_tec.set,'agri_land_total','agri_land_rainfed')
		
		cat_tec.set = cat_tec.set %>% bind_rows(data.frame(type_tec = 'agri_land_total',
														   tec = c(irr_tech_names,rainfed_crop_names) ),
												data.frame(type_tec = 'agri_land_rainfed',
														   tec = rainfed_crop_names) )
		
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
	
cap_fac = capacity_factor.par %>% 
	dplyr::select(-vintage) %>% 
	distinct() %>% 
	rename( CF = value )

tmp1 = historical_new_capacity.par %>% filter(year_all < 2020) %>% 
  filter(grepl('crop|irr_flood|rainfed',tec)) %>% filter(!grepl('fruit',tec)) %>% 
  group_by(node,tec,year_all) %>% 
  summarise(value = sum(value)) %>% ungroup() %>% 
  filter( !(grepl('AFG',node) & grepl('vegetables',tec)) )

bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
                                    crossing(tmp1 %>% select(-year_all),
                                             year_all = c(2020,2030,2040,2050)) %>%
                                      select(node,tec,year_all,value)  )

# tmp2 = hist_irr_water_act %>% filter(tec == 'irrigation_sw_diversion') %>% 
#   crossing(year_all = c(2020),
#                                        mode = 1) %>%
#   mutate(time = as.character(time)) %>%
#   select(node,tec,year_all,mode,time,value) %>% as.data.frame() %>% 
#   filter(!grepl('AFG|CHN',node))
# 
# bound_activity_lo.par = rbind(bound_activity_lo.par %>% as.matrix %>% as.data.frame(stringsAsFactors = F),
#                               tmp2) 
# 
# bound_activity_up.par = rbind(bound_activity_up.par %>% as.matrix %>% as.data.frame(stringsAsFactors = F),
#                               tmp2 %>% mutate(value = value * 1.05)) 

# tmp3 = historical_new_capacity.par %>% 
#   filter(grepl('irrigation_sw_diversion',tec)) %>% filter(year_all < 2020) %>% 
#   group_by(node,tec,year_all) %>% 
#   summarise(value = sum(value)) %>% ungroup() %>% 
#   filter( !grepl('AFG|CHN',node)  )
# 
# # force water diversion in 2020 to be as calibration ONLY in 2020
# bound_total_capacity_lo.par = rbind(bound_total_capacity_lo.par,
#                                     crossing(tmp3 %>% select(-year_all),
#                                              year_all = c(2020)) %>%
#                                       select(node,tec,year_all,value)  )
# 
# bound_total_capacity_up.par = rbind(bound_total_capacity_up.par,
#                                     crossing(tmp3 %>% select(-year_all),
#                                              year_all = c(2020)) %>%
#                                       mutate(value = 1.05 * value) %>%
#                                       select(node,tec,year_all,value)  )

# bound_total_capacity_up.par = rbind(bound_total_capacity_up.par,
#                                     crossing(tmp1 %>% select(-year_all),
#                                              year_all = c(2020,2030,2040,2050)) %>% 
#                                       select(node,tec,year_all,value)  %>% 
#                                       mutate(value = value *0.01 + 1e-6) )

#not for crops, the historical capacity in 2020 is equal to 2015, actually should be removed
# bound_new_capacity_lo.par = rbind(bound_new_capacity_lo.par,
#                                   historical_new_capacity.par %>% filter(year_all >= 2020) %>% 
#   filter(grepl('solar|wind|gas|coal|oil|nuclear|hydro_old',tec)) %>% 
#   group_by(node,tec,year_all) %>% 
#   summarise(value = sum(value)) %>% ungroup()
#   )
  
# tmp1 = historical_new_capacity.par %>% 
# 	filter(grepl('solar|wind|gas|coal|oil|hydro|nuclear|crop|irr_flood|rainfed',tec)) %>% 
# 	group_by(node,tec,year_all) %>% 
# 	summarise(value = sum(value)) %>% 
# 	as.data.frame() %>% 
# 	left_join(cap_fac) %>% 
# 	mutate(value = value * CF /12) %>% 
# 	crossing(mode = c(1,2)) %>% 
# 	dplyr::select( -CF )
# 
# hist_plant_act = tmp1 %>% 
# 	mutate(year_all = 2020) %>% 
# 	group_by(node,tec,mode,time) %>% summarise(value = max(value)) %>% 
# 	as.data.frame( ) %>%
# 	mutate( year_all = 2020 ) %>% 
# 	dplyr::select(node,tec,year_all,mode,time,value) %>% 
# 	filter( !is.na(value), !grepl('CHN',node), !(mode == 2 & grepl('crop',tec)) ) %>% 
# 	filter( !(grepl('AFG',node) & tec == 'crop_maize') ) %>% # it seems this constaint set production higher than demand for maize in AFG and pulses in PAK
# 	filter( !(grepl('PAK',node) & tec %in% c('crop_pulses','crop_fodder') ) )
# 
# # demand.df %>% filter(level == 'raw', year_all == 2020) %>% 
# #   filter( (node == 'PAK' & commodity == 'pulses_yield') | (node == 'AFG' & commodity == 'maize_yield') )
# 
# bound_activity_lo.par = bound_activity_lo.par %>% bind_rows(hist_plant_act)

