# Multiple run script

################################
######## multiple run ##########
################################

require( raster )
require( dplyr )
require( tidyr )

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")

## data source ##
# I is possible to run the Github version of NEST without access to IIASA databases
# the data for the Indus Basin model SSP2 and RCP6.0 default assumptions 
# is available in .RData format in 'Data_NESTv2.1_Indus_SSP2_RCP6.RData'
# to use this data set default_data=T , 
# to load data from IIASA internal database set default_data=F
default_data = T

# Scenario names
scens = c( 	'baseline0',
			'baseline',
			'no_planned_hydro',
			'baseline_coop', 
			'no_hydro_EMI_res',
			'SDG6') # no flood in HIST_AGRICULTURE

# Data frame containing the various settings for each scenario			
policy_settings.df = data.frame( scen_name = scens , stringsAsFactors = F ) %>% 
    mutate( SSP = rep('SSP2', length(scens) ),
			climate_model = rep('ensemble', length(scens) ),
			climate_scenario = rep('rcp60', length(scens) ),
			REDUCE_RUNOFF = rep( F, length(scens) ),
			IND_TREAT =         c( T, T, T, F, T ,T ),
			ENV_FLOWS =        	c( F, F, F, F, F ,F ),      
			SDG6 =             	c( F, F, F, F, F ,T ),            # SDG6 
			EMISS =            	c( F, F, F, F, T ,F ),          # GhG Emission 
			SDG7 =             	c( F, F, F, F, F ,F ),        # SDG7: solar/wind targets + phase out of oc 
			GROUNDWAT =        	c( F, T, T, T, T ,T ),       # groundwater extraction bounds 
			CONSTRAINT_LAND =  	c( F, F, F, F, F ,F ),      # constraint available land, anyhow
			CHANGE_FOOD_DEMAND =c( F, F, F, F, F ,F ),     # change food demaind, customize the multiplication factor in policy script
			FIX_ELEC_IMPORT =  	c( F, T, T, T, T ,T ),
		  SMART_IRR_WATER =  	c( F, F, F, F, F ,F ),
		  RAINFED_LAND   =   	c( T, T, T, T, T ,T ),
		  NOT_PLANNED_HYDRO =	c( F, F, T, F, T ,F ),
		  FULL_COOPERATION = 	c( F, F, F, T, F ,F ),
			HIST_AGRICULTURE =  c( T, T, T, T, T ,T )
  ) %>% 
  gather(key,value,2:length(.))

## Run single scenario: two ways ##
# uncomment and write the name of the specific scenario
# policy_settings.df = policy_settings.df %>% filter(scen_name %in% c('baseline') )

# uncomment and write the number of the specific scenario in scens
# sc = (unique(policy_settings.df$scen_name ))[1] 

## Run multiple scenarios in a row ##
for( sc in (unique(policy_settings.df$scen_name )) ){ 
	
	rm(list=setdiff(ls(), c("policy_settings.df",'sc','indus_ix_path','default_data') ) )

	# assign 
	for( nnn in ( policy_settings.df$key ) ){ 
    
		if ( !is.na( as.logical( policy_settings.df %>% filter(scen_name == sc, key == nnn) %>% dplyr::select( value ) ) ) ){
	
			assign( nnn, as.logical((policy_settings.df %>% filter(scen_name == sc, key == nnn))$value ) ) 
			
			}else{
			
			assign( nnn, as.character((policy_settings.df %>% filter(scen_name == sc, key == nnn))$value ) ) 
			
			}
		
		} 
  
	# to be saved in the data gdx name
	policy_option = paste0(	'_EF.',substr(ENV_FLOWS, 1,1),'_IT.',substr(IND_TREAT, 1,1),'_EM.',substr(EMISS, 1,1), 
							'_S6.',substr(SDG6, 1,1),'_S7.',substr(SDG7, 1,1)) 
	baseline = 'baseline'
	baseline0 = 'baseline0'
	data_gdx = paste0(sc,policy_option)
	shiny_mode = F
	Beta_water = 1
	source( paste( indus_ix_path, 'basin_msggdx.r', sep = '/' ) ) 

	}

#### Another set of scenario, same procedure as before
# multiple sustainable objective
scens = c( 'multiple_objective',
           'multiple_obj_coop',
           'baseline_glacier',
           'multi_obj_glacier',
           'baseline_extreme',
           'multi_obj_extreme_new',
           'baseline_coop_extreme',
           'multi_obj_coop_extreme')

# Data frame containing the various settings for each scenario			
policy_settings.df = data.frame( scen_name = scens , stringsAsFactors = F ) %>% 
  mutate( SSP = rep('SSP2', length(scens) ),
          climate_model = rep('ensemble', length(scens) ),
          climate_scenario = rep('rcp60', length(scens) ),
          REDUCE_RUNOFF =     c( F, F , T, T, F, F, F, F),
          IND_TREAT =         c( T, F , T, T, T, T, F, F),
          ENV_FLOWS =        	c( T, T , F, T, F, T, F, T),      
          SDG6 =             	c( T, T , F, T, F, T, F, T),            # SDG6 
          EMISS =            	c( T, T , F, T, F, T, F, T),          # GhG Emission 
          SDG7 =             	c( T, T , F, T, F, T, F, T),        # SDG7: solar/wind targets + phase out of oc 
          GROUNDWAT =        	c( T, T , T, T, T, T, T, T),       # groundwater extraction bounds 
          CONSTRAINT_LAND = 	c( F, F , F, F, F, F, F, F),       # constraint available land, anyhow 
          CHANGE_FOOD_DEMAND =c( F, F , F, F, F ,F, F, F ),  # change food demaind, customize the multiplication factor in policy script
          FIX_ELEC_IMPORT =  	c( T, T , T, T, T, T, T, T),
          SMART_IRR_WATER =  	c( T, T , F, T, F, T, F, T),
          RAINFED_LAND   =   	c( T, T , T, T, T, T, T, T),
          NOT_PLANNED_HYDRO =	c( F, F , F, F, F, F, F, F),
          FULL_COOPERATION = 	c( F, T , F, F, F, F, T, T),
          HIST_AGRICULTURE =  c( F, F , T, F, T, F, T, F)
  ) %>% 
  gather(key,value,2:length(.))

## Run multiple scenarios in a row ##
for( sc in (unique(policy_settings.df$scen_name )) ){ 
  
  rm(list=setdiff(ls(), c("policy_settings.df",'sc','indus_ix_path','default_data') ) )
  
  # assign 
  for( nnn in ( policy_settings.df$key ) ){ 
    
    if ( !is.na( as.logical( policy_settings.df %>% filter(scen_name == sc, key == nnn) %>% dplyr::select( value ) ) ) ){
      
      assign( nnn, as.logical((policy_settings.df %>% filter(scen_name == sc, key == nnn))$value ) ) 
      
    }else{
      
      assign( nnn, as.character((policy_settings.df %>% filter(scen_name == sc, key == nnn))$value ) ) 
      
    }
    
  } 
  
  # to be saved in the data gdx name
  policy_option = paste0(	'_EF.',substr(ENV_FLOWS, 1,1),'_IT.',substr(IND_TREAT, 1,1),'_EM.',substr(EMISS, 1,1), 
                          '_S6.',substr(SDG6, 1,1),'_S7.',substr(SDG7, 1,1)) 
  baseline = 'multiple_objective'
  baseline0 = 'baseline0'
  data_gdx = paste0(sc,policy_option)
  shiny_mode = F
  Beta_water = 1
  source( paste( indus_ix_path, 'basin_msggdx.r', sep = '/' ) ) 
  
}
