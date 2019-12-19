
# Run from the R console using:
# source( paste( Sys.getenv("INDUS_IX_PATH"), 'basin_msggdx.r', sep = '/' ) )

##### INITIALIZE #####

options( java.parameters = "-Xmx16g" )

print( 'Initializing functions and data' )

#rm( list = ls( ) )
graphics.off( )
require( raster )
require( dplyr )
require( tidyr )
require( rgdal )
require( tictoc )
require( maptools )
require( rgeos )
require( gdxrrw )


#### Initialization / Model setup #####

tic()

# Location of input data
setwd( 'P:/is-wel/indus/message_indus' )

# Use ixmp? Takes time to upload - can choose option to debug w/o ixmp
use_ixmp = FALSE

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")

# Location of GAMS - need to add to system environemtn variables but can't change from remote desktop :(
gams_path = 'C:/GAMS/win64/24.9'

# Basin analyzed
basin = 'Indus'
# 
# # SSP being analyzed
# SSP = 'SSP2'
# 
# # Climate model analyzed
# climate_model = 'ensemble'
# climate_scenario = 'rcp60'

# Time steps
year = c( seq( 1990, 2010, by = 10  ), 2015, seq( 2020, 2060, by = 10 ) ) 
time =  as.character( ( seq(1, 12, by = 1 ) ) ) # monthly time steps
year_all = year
baseyear = 2015 # last historical year
lastyear = last(year_all)

# load data inputs
source( paste( indus_ix_path, 'basin_msggdx_load_inputs.r', sep = '/' ), verbose = FALSE ) 

# load technology parameters using 'basin_msggdx_technologies.r' and add to formatted list
source( paste( indus_ix_path, 'basin_msggdx_technologies.r', sep = '/' ) ) 

# Set the technologies to add to the GDX - ! needs to match what's defined in 'basin_msggdx_technologies.r' !
technology.set = c( 'gas_cc_ot',
					'gas_cc_cl',
					'gas_cc_ac',
					'gas_cc_sw',
					'gas_st_ot',
					'gas_st_cl',
					'gas_st_ac',
					'gas_st_sw',
					'gas_gt',
					'oil_cc_ot',
					'oil_cc_cl',
					'oil_cc_ac',
					'oil_cc_sw',
					'oil_st_ot',
					'oil_st_cl',
					'oil_st_ac',
					'oil_st_sw',
					'oil_gt',	
					'coal_st_ot',
					'coal_st_cl',
					'coal_st_ac',
					'coal_st_sw',
					'igcc_ot',
					'igcc_cl',
					'igcc_ac',
					'igcc_sw',
					'nuclear_sw',
					'nuclear_ot',
					'nuclear_cl',
					'geothermal_sw',
					'geothermal_ot',
					'geothermal_cl',
					#'biomass_st_ot',
					#'biomass_st_ac',
					#'biomass_st_cl',
					#'biomass_st_sw',
					'solar_pv_1',
					'solar_pv_2',
					'solar_pv_3',
					'wind_1',
					'wind_2',
					'wind_3',
					'hydro_old',
					'hydro_river',
					'hydro_canal',
					'electricity_distribution_urban',
					'electricity_distribution_rural',
					'electricity_distribution_industry',
					'electricity_distribution_irrigation',
					'electricity_short_strg', 
					routes_names,
					exp_routes_names,
					'sw_extract',
					'gw_extract',
					'renew_gw_extract',
					'urban_sw_diversion',
					'urban_gw_diversion',
					'urban_piped_distribution',
					'urban_unimproved_distribution',
					'urban_wastewater_collection',
					'urban_wastewater_release',
					'urban_wastewater_treatment',
					'urban_wastewater_recycling',
					'urban_wastewater_irrigation',
					'urban_desal_seawater', 
					'industry_sw_diversion',
					'industry_gw_diversion',
					'industry_distribution',
					'industry_wastewater_collection',
					'industry_wastewater_release',
					'industry_wastewater_treatment',
					'industry_wastewater_recycling',
					'industry_desal_seawater',
					'rural_sw_diversion',
					'rural_gw_diversion',
					'rural_piped_distribution',
					'rural_unimproved_distribution',
					'rural_wastewater_release',
					'rural_wastewater_treatment',
					'rural_wastewater_recycling',
					'rural_wastewater_collection',
					'irrigation_sw_diversion',
					'irrigation_gw_diversion',
					'smart_irrigation_sw_diversion',
					# 'smart_irrigation_gw_diversion',
					'irrigation_desal_seawater',
					'energy_sw_diversion',
					'energy_gw_diversion',
					'environmental_flow',
					'internal_runoff',
					# 'surface2ground',
					# 'ground2surface',
					'interbasin_canal',
					river_names,
					canal_names,
					crop_tech_names,
					rainfed_crop_names,
					irr_tech_names[!grepl('flood_fruit',irr_tech_names)],
					'fallow_crop',
					'solid_biom',
					'ethanol_prod',
					'ethanol_genset',
					'ethanol_agri_genset',
					'irri_diesel_genset',
					'agri_diesel_genset',
					'agri_pv'
					)				
				
# Generate data frames for the parameters and sets to pass to ixmp			
source( paste( indus_ix_path, 'basin_msggdx_setpardef.r', sep = '/' ) )		
		
# Additional policies
source( paste( indus_ix_path, 'basin_msggdx_policies.r', sep = '/' ) )	
		
toc() # finished loading data		

#### SOLVE  ######

# name of scenario
#scname = 'test'

# if using ixmp, upload data and solve
if( use_ixmp )
	{
	
	print( 'Uploading to DB using ixmp' )
	
	tic()
	source( paste( indus_ix_path, 'basin_msggdx_ixmp.r', sep = '/' ) )
	toc()
	
	tic()
	source( paste( indus_ix_path, 'basin_msggdx_solve.r', sep = '/' ) )
	toc()
	
	}
	
	# Solve locally

	print( 'Output to GDX with gdxrrw' )

tic()
source( paste( indus_ix_path, 'basin_msggdx_gdxrrw.r', sep = '/' ) )
toc()	

tic()
source( paste( indus_ix_path, 'basin_msggdx_solve.r', sep = '/' ) )
toc()
	
	print( paste0( sc, ' ---> scenario finished' ) )
#### RUN DIAGNOSTICS
tic()
print( 'Running diagnostics' )
source( paste( indus_ix_path, 'basin_msggdx_diagnostics.r', sep = '/' ) )
toc()
#### FIN #####

print( 'Scenario finished' )
