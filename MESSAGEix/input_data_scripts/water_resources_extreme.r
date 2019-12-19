
#source('C:/Users/parkinso/indus_ix/input_data_scripts/water_resources_extreme.r')

require(rgeos)
require(rgdal)
require(raster)
require(dplyr)
require(rasterVis)
require(maptools)
require(ncdf4)
require(igraph)
require(RColorBrewer)
require(reshape)
require(ggmap)
memory.limit(size=1e9)

# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

# basins to check
basin = c('Indus')

# Define continental tiles from hydroBASINS
cnt_list = c('af', 'as', 'au', 'ca', 'eu', 'na', 'sa')
wkd_dir1 = paste( getwd(), "input/hydrosheds/elevation/", sep = '/' )
wkd_dir2 = paste( getwd(), "input/hydrosheds/flow_accumulation/", sep = '/' )
wkd_dir3 = paste( getwd(), "input/hydrosheds/river_network/", sep = '/' )
wkd_dir4 = paste( getwd(), "input/hydrosheds/flow_direction/", sep = '/' )
cnt_ext = lapply( cnt_list, function(cnt){ extent( raster( paste ( paste( wkd_dir1, c(paste(cnt,'_dem_15s_grid/',paste(cnt,'_dem_15s/',sep=''),paste(cnt, '_dem_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) ) ) } )
names(cnt_ext) = cnt_list

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))

# Create buffered basin polygon for gridded data along the border
buff.sp = gBuffer( basin.sp, width=0.1 ) 
buff2.sp = gBuffer( basin.sp, width = 10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)

# Check which hydrobasins tiles to include for this basin
tiles_incl = sapply( names(cnt_ext), function(cnt){
	polyg = as( cnt_ext[[cnt]] , 'SpatialPolygons' )
	proj4string(polyg) = proj4string(basin.sp)
	if( unlist( gIntersects( polyg, basin.sp ) ) ){ return( cnt ) } 
	} )
cnt = unlist( tiles_incl[!is.null(tiles_incl)] )

# river
#trv = readOGR('P:/ene.general/Water/global_basin_modeling/basin_delineation','indus_rivers',verbose=FALSE)

# Add river names
if( basin == 'Indus' ){ basin.spdf@data$river = basin.spdf@data$REGION }
	
# Get the climate model and scenario names
fls = list.files( 'input/hydroclimate_input')
climate_models = unique( unlist( strsplit( fls, '_' ) )[seq(3,4*length( fls ),by=4 ) ] )
climate_scenarios = unique( unlist( strsplit( fls, '_' ) )[seq(4,4*length( fls ),by=4 ) ] )
climate_scenarios = unique( unlist( strsplit( climate_scenarios, '[.]' ) )[ seq(1,2*length(climate_scenarios),by=2) ] )	
	
# create stack of climate model -scenario combinations for runoff
ghm.list = lapply( climate_models, function( cc ){ 
	r1 = lapply( climate_scenarios, function( ss ){ 
		if( file.exists( paste0( 'input/hydroclimate_input/extremerunoff_metersperday_',cc,'_', ss, '.tif' ) ) ){
			r = stack( paste0( 'input/hydroclimate_input/extremerunoff_metersperday_',cc,'_', ss, '.tif' ) )
			names(r) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			return(r)	
			}
		} )
	names(r1) = climate_scenarios
	return(r1)	
	} )
names(ghm.list) = climate_models		

# create stack of climate model -scenario combinations - groundwater recharge
gwr.list = lapply( climate_models, function( cc ){ 
	r1 = lapply( climate_scenarios, function( ss ){ 
		if( file.exists( paste0( 'input/hydroclimate_input/recharge_metersperday_',cc,'_', ss, '.tif' ) ) ){
			r = stack( paste0( 'input/hydroclimate_input/recharge_metersperday_',cc,'_', ss, '.tif' ) )
			names(r) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			return(r)	
			}
		} )
	names(r1) = climate_scenarios
	return(r1)	
	} )
names(gwr.list) = climate_models

# import the elevation raster and crop to the basin outline
elevation.raster = raster( paste( paste( wkd_dir1, c(paste(cnt,'_dem_15s_grid/',paste(cnt,'_dem_15s/',sep=''),paste(cnt, '_dem_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) )
elevation.raster = mask( x = crop( elevation.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( elevation.raster, extent( basin.sp ), snap="out") ) )

# Cells above max elevation
elevation_max.raster = elevation.raster
elevation_max.raster[ elevation_max.raster < 3500 ] = NA
elevation_max.raster[ !is.na(elevation_max.raster) ] = 1

# Flow accumulation raster 
flow_accumulation.raster = raster( paste( paste( wkd_dir2, c(paste(cnt,'_acc_15s_grid/',paste(cnt,'_acc_15s/',sep=''),paste(cnt, '_acc_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) )
flow_accumulation.raster = mask( x = crop( flow_accumulation.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( flow_accumulation.raster, extent( basin.sp ), snap="out") ) )

# Flow direction raster 
flow_direction.raster = raster( paste( paste( wkd_dir4, c(paste(cnt,'_dir_15s_grid/',paste(cnt,'_dir_15s/',sep=''),paste(cnt, '_dir_15s',sep=''),sep='')),sep='' ),'/', 'w001001.adf', sep='' ) )
flow_direction.raster = mask( x = crop( flow_direction.raster, extent( basin.sp ), snap = "out" ), mask = rasterize(	basin.sp, crop( flow_direction.raster, extent( basin.sp ), snap="out") ) )

# Harmonize
w1 = flow_accumulation.raster
w1[] = flow_direction.raster[]
flow_direction.raster = w1
w1 = flow_accumulation.raster
w1[] = elevation.raster[]
elevation.raster = w1
rm(w1)

# Get PIDs for raster cells 
tmp = basin.spdf[,'PID']
basin.raster = rasterize( tmp, flow_accumulation.raster, 'PID' )
basin.raster[ which( !( basin.raster[] > 0 ) ) ] = NA

# Compute elevation change and flow direction

# Get the sub-bcu land units and elevation at these unit - 
ae = area( flow_accumulation.raster )
ae[ which( !( 1:length(ae[]) %in% which(!is.na(flow_accumulation.raster[])))) ]=NA

# limit land units to cells with at least 5 km2 of upstream area
lu = which( !is.na( flow_accumulation.raster[] ) & 
			( flow_accumulation.raster[] > 0 ) & 
			( flow_accumulation.raster[] >= round( 5 / mean(ae[],na.rm=TRUE) ) ) )

# harmonize other data to land units
fa = c( flow_accumulation.raster[ lu ] ) 
fd = c( flow_direction.raster[ lu ] )
hi = c( elevation.raster[ lu ] )
ar = c( ae[ lu ] )

# Get adjacent cells

# matrix for defining the adjacent cells to look at in the flow direction raster
around = matrix( c(	1, 1, 1, 
					1, 0, 1, 
					1, 1, 1 ), ncol = 3, byrow=TRUE )

# flow direction matrix					
hmat = c( 32, 64, 128, 
		  16, 0, 1, 
		  8, 4, 2 )

# get the flow direction in the adjacent cells 		  
adj = data.frame( adjacent( flow_direction.raster, cell = lu, directions=around, sorted=TRUE, include = TRUE ) )
adj = adj[ which( adj$from %in% lu ),  ]

# Use flow direction matrix to determine the direction of downstream
adj$hmat = unlist( lapply( 1:length(lu), function(iii){ return(hmat) } ) )
sd = match( lu, adj$from )
se = match( fd, adj$hmat )

# use the direction mapping to create a corresponding raster that contains the downstream cell id based on the flow direction map
tm = unlist(adj$to)
down = sapply( 1:length(lu), function( x ){ tm[ c( sd[x]+se[x] - 1 ) ] } )

# compute the elevation change
dh = hi - hi[ match( down, lu ) ]
dh[ dh < 0 ] = 0 # discard	

# Get the spatial locations of the land units
xloc = xFromCell( flow_accumulation.raster, lu )
yloc = yFromCell( flow_accumulation.raster, lu )
locs = data.frame( x = xloc, y = yloc )

# Get the mapping to cells from the GHM at lower resolution
ghm_cell = ghm.list[[1]][[1]][[1]]
ghm_cell[] = 1:length(ghm_cell[])

# resample to match the land unit grid
ghm_cell = resample( ghm_cell, flow_accumulation.raster, method = 'ngb')
ghm_cell = ghm_cell[ lu ]

# Replace missing locations with nearest neighbour
if( length(which(is.na(ghm_cell))) > 0 )
	{
	
	pts = SpatialPoints( data.frame( xyFromCell( flow_accumulation.raster, lu[ which(is.na(ghm_cell)) ] ) ) )
	proj4string(pts) = proj4string(basin.sp)
	
	ghm_cell[ which(is.na(ghm_cell)) ] = unlist( lapply( 1:length(pts), function(ppp){ 
		
		basin.spdf@data$PID[ which.min(gDistance(pts[ppp],basin.spdf,byid=TRUE)) ] 
		
		} ) )
	
	}
	
# Get the PID associated with each land unit - again use nearest neighbour when na encountered	
basin.raster = resample( basin.raster, flow_accumulation.raster, method = 'ngb' )
basin_cell = basin.raster[ lu ]
if( length(which(is.na(basin_cell))) > 0 )
	{
	pts = SpatialPoints( data.frame( xyFromCell( flow_accumulation.raster, lu[ which(is.na(basin_cell)) ] ) ) )
	proj4string(pts) = proj4string(basin.sp)
	basin_cell[ which(is.na(basin_cell)) ] = unlist( lapply( 1:length(pts), function(ppp){ which.min(gDistance(pts[ppp],basin.spdf,byid=TRUE)) } ) )
	}
basin.raster[ lu ] = basin_cell

# use the downstream mapping to get the basin associated with the downstream cell
down_basin_cell = basin_cell[ match( down, lu ) ]

# re-collect the network data
nt.df = data.frame( xloc = xloc, 
					yloc = yloc, 
					cell = lu, 
					downstream_cell = down, 
					elevation = hi, 
					elevation_change = dh, 
					flow_accumulation = fa, 
					ghm_cell = ghm_cell, 
					pid = basin.spdf@data$PID[basin_cell],
					down_pid = basin.spdf@data$PID[down_basin_cell] )


# Estimate upstream area for converting runoff depth to discharge volume

# Check and calibration of the flow accumulation to reflect missing grid cell areas lost due to 5km2 upstream area simplification

# The estimated flow accumulation is calculated by routing the through the reduced netwok  
# (i.e., cells in nt.df that are remaining after limiting upstream areas to 5km2)

act_acc = unlist( nt.df$flow_accumulation )
est_acc = rep( 1, length( nt.df$flow_accumulation ) ) # initilize
ntm.df = nt.df
while( nrow(ntm.df) > 0  )
	{
	
	# Get the most upstream cells and find immediate downstream cells
	cell_tot = nrow(ntm.df)
	cell_start = as.integer( unlist( ntm.df$cell[ which( !( ntm.df$cell %in% ntm.df$downstream_cell ) ) ] ) )
	ind_start = match( cell_start, nt.df$cell )
	cell_down = as.integer( unlist( nt.df$downstream_cell[ ind_start ] ) )
	ind_down = match( cell_down, nt.df$cell )
	
	# If there are downsteam cells
	if( length(ind_down) > 0 ){ 
		
		for( i in 1:length(cell_down) ){ 
			
			# if the downstream cell is not a sink
			if( !is.na(ind_down[i]) & ind_down[i] != ind_start[i] ){ 
				
				# add the accumulated cell area of the upstream cell to the accumulated downstream cell area
				est_acc[ ind_down[i] ] = est_acc[ ind_down[i] ] + act_acc[ ind_start[i] ] 
				
				} 
				
			} 
			
		}
		
	# remove the most upstream cells	
	ntm.df = ntm.df[ -1*which( ntm.df$cell %in% cell_start ), ]
	
	}

# the partial flow accumulation misses the area lost in 5km2 simplification
# use the actual flow ccumulation from hydrobasins to estimate the missing 
nt.df$partial_flow_accumulation = act_acc - est_acc

# area of all land units
a = area(basin.raster)
a[which(!( 1:length(a[])%in% which(!is.na(basin.raster[]))))]=NA

# Scale the area of each cell to reflect the missng upstream area
nt.df$area = ( 1 + nt.df$partial_flow_accumulation ) * a[lu]
nt.df$area_og =  a[lu]

# exclusion zones from wind and solar analysis
ws_excl = raster('input/fraction_grid_cell_available_wind_solar.asc')

# Additionally - exlusion of areas where existing power plants are in use 
# to avoid double counting hydropower potential already built and included in the model separately

# Asset-level data base compiled for input to MESSAGE in 'input_data_scripts/powerplants_historical_capacity.r'
ppl.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), paste( basin, 'power_plants', sep = '_' ), verbose = FALSE )	
proj4string( ppl.spdf ) = proj4string( basin.raster )

# Distance to power plants of different sizes - currently breaking into group of less than and greater than 100 MW 
basin_hydropower.raster = rasterize( ppl.spdf, field='MW', basin.raster, fun=sum )
basin_hydropower_lo.raster = basin_hydropower.raster
basin_hydropower_lo.raster[ basin_hydropower_lo.raster[] < 100 ] = 1
basin_hydropower_lo.raster[ basin_hydropower_lo.raster[] != 1 ] = NA
basin_hydropower_hi.raster = basin_hydropower.raster
basin_hydropower_hi.raster[ basin_hydropower_hi.raster[] >= 100 ] = 1
basin_hydropower_hi.raster[ basin_hydropower_hi.raster[] != 1 ] = NA
dist_hydro_lo.raster = basin_hydropower_lo.raster
dist_hydro_lo.raster[ dist_hydro_lo.raster > 0 ] = 1
dist_hydro_lo.raster=distance(dist_hydro_lo.raster)/1000
dist_hydro_hi.raster = basin_hydropower_hi.raster
dist_hydro_hi.raster[ dist_hydro_hi.raster > 0 ] = 1
dist_hydro_hi.raster=distance(dist_hydro_hi.raster)/1000

# Exclude grid cells within certain distance to the existing projects
temp1 = dist_hydro_lo.raster
temp1[ which( temp1[] <= 5 )  ] = 1 # don't install less than 5 km away for less than 100 MW
temp1[ temp1[] != 1 ] = 0
temp2 = dist_hydro_hi.raster
temp2[ which( temp2[] <= 5  ) ] = 1 # don't install less than 5 km away for greater than 100 MW
temp2[ temp2[] != 1 ] = 0
dist_hydro.raster = temp1 + temp2
dist_hydro.raster[ dist_hydro.raster[] %in% c(1,2) ] = 1
dist_hydro.raster[ dist_hydro.raster[] != 1 ] = NA

# resample to match flow accumulation
dist_hydro.raster = resample(dist_hydro.raster, flow_accumulation.raster, method="bilinear")

# Go through each GCM-scenario combination and route the runoff through the high resolution network

for( climate_scenario in climate_scenarios )
	{
	
	for( climate_model in climate_models )
		{
		
		print( paste0( 'Working on: ', climate_scenario, ' ', climate_model ) )
		
		# Get the runoff from low resolution hydrological model
		ghm_dynamic.stack = ghm.list[[ climate_model ]][[ climate_scenario ]]
		if( !is.null( ghm_dynamic.stack ) )
			{
			f.stack  = ghm_dynamic.stack / 1000 # convert to km per day
			proj4string(f.stack) = proj4string(flow_accumulation.raster)
			runoff.df = data.frame( do.call( cbind, lapply( 1:nlayers( f.stack ), function( nnn ){ 
			
				unlist( f.stack[[ nnn ]][ c(nt.df$ghm_cell) ] ) * unlist( nt.df$area )  # in km3 per day
				
				} ) ) ) 
			names(runoff.df) = names(f.stack)
			runoff.df[is.na(runoff.df)]=0
			
			# check annual inflow 
			annual_inflow_km3 = sapply( 1:6, function(i){ sum( apply( runoff.df[,(12*(i-1)+1):(12*i)], 1, mean ), na.rm=TRUE ) * 365 } )
			
			# Get the groundwater recharge from the low resolution hydrological model
			gwr_dynamic.stack = gwr.list[[ climate_model ]][[ climate_scenario ]]
			f.stack  = gwr_dynamic.stack / 1000 # convert to km per day
			proj4string(f.stack) = proj4string(flow_accumulation.raster)
			recharge.df = data.frame( do.call( cbind, lapply( 1:nlayers( f.stack ), function( nnn ){ 
			
				unlist( f.stack[[ nnn ]][ c(nt.df$ghm_cell) ] ) * unlist( nt.df$area )   # in km3 per day
				
				} ) ) ) 
			names(recharge.df) = names(f.stack)
			recharge.df[is.na(recharge.df)]=0
			recharge.df[recharge.df<0]=0
			
			# Check annual recharge
			annual_recharge_km3 = sapply( 1:6, function(i){ sum( apply( recharge.df[,(12*(i-1)+1):(12*i)], 1, mean ), na.rm=TRUE ) * 365 } )
			
			# Transmission losses - based on LISFLOOD documentation
			# Limit the transmission losses to cells with a given downstream area (1e10)
			# Estimated that under the standard parameters, 3% loss for 0.5 degree grid cell - scaling to flow accumulation raster
			tmp = nt.df$flow_accumulation * nt.df$area_og * 1e6 / 1e10
			nt.df$trn_loss = nt.df$flow_accumulation * nt.df$area_og * 1e6 / 1e10
			nt.df$trn_loss[ which( nt.df$trn_loss > 1 ) ] = 0 
			nt.df$trn_loss[ which( nt.df$trn_loss > 0 ) ] = 0.03 * res( basin.raster )[1] / 0.5

			# Downscale runoff and route through the network	
			flow.df = runoff.df
			ntm.df = nt.df
			while( nrow(ntm.df) > 0 )
				{
				
				# Get the most upstream cells and immediate downstream neighbours
				cell_tot = nrow(ntm.df)
				cell_start = as.integer( unlist( ntm.df$cell[ which( !( ntm.df$cell %in% ntm.df$downstream_cell ) ) ] ) )
				ind_start = match( cell_start, nt.df$cell )
				
				# Upstream cells to look at are those not included in the downstream list
				cell_down = as.integer( unlist( nt.df$downstream_cell[ ind_start ] ) )
				ind_down = match( cell_down, nt.df$cell )
				
				# transmission loss parameter for estimating the losses GW and evaporation during transport.
				trn_loss = as.numeric( unlist( nt.df$trn_loss[ ind_start ] ) )
				
				# Set a threshold indicating the length of the upstream cell vector to start using direct indexing for 
				# setting the flow values, which becomes faster as ntm.df gets smaller
				thr = 3
				if( length(ind_down) > thr )
					{ 
							
							
					# Route the flow from the upstream cells		
					nm = names(flow.df)
					
					flow.df = data.frame( do.call( cbind, lapply( 1:ncol( flow.df ), function( ttt ){
						
						fl = as.vector( flow.df[ , ttt ] ) 
						
						for( i in which( !is.na( ind_down ) ) ){ 
							
							# if not a sink, include flow at upstream cell in downstream accounting and include transmission loss
							if( ind_down[i] != ind_start[i] ){ 
							
								fl[ ind_down[i] ] = fl[ ind_down[i] ] + fl[ ind_start[i] ] * ( 1 - trn_loss[ i ] ) 
								
								} 
							
							}
						
						return(fl)	
						
						} ) ) )
					
					names(flow.df) = nm		
					
					}else{ # exclude transmission losses and compute all time steps simultaneuosuly - more efficient for few indices
					
					if( length(ind_down) > 0 ){ 
						
						for( i in which( !is.na( ind_down ) ) ){ 
						
							if( ind_down[i] != ind_start[i] ){ 
							
								flow.df[ ind_down[i], ] = as.vector( flow.df[ ind_down[i], ] ) + as.vector( flow.df[ ind_start[i], ] ) 
								
								} 
								
							} 
							
						}
					
					}
				
				# remove the most upstream cells to step through the network
				ntm.df = ntm.df[ -1*which( ntm.df$cell %in% cell_start ), ] # remove routed cells from list
				
				}
		
			# Output routed flow to netcdf	
			# temp = cbind( nt.df[,c('xloc','yloc')], flow.df )
			# coordinates(temp) = ~xloc+yloc
			# gridded(temp) = TRUE
			# temp = do.call( stack, lapply( 1:12, function(iii){ return( raster( temp[ , iii ] ) ) } ) )
			# elev_out.raster = resample( elevation.raster, temp[[1]], method = 'bilinear' )
			# drv = 'I:/hunt/Indus Basin/' 
			# vars = c('qnat')
			# vars_long = c('flow')
			# vars_units = c('km3 per day')
			# vars_prec = rep('double',length(vars))
			# dim_time = ncdim_def( 'time', "months since December 2009", unlist(lapply(1:length(c(2010)), function(y){ (120*(y-1) + 1) + seq(0,11,by=1) })), unlim=T ) 
			# dim_lon  = ncdim_def(	'lon', 
									# 'degrees_east', 
									# seq( bbox(rasterToPoints(temp[[1]], spatial = TRUE))[1,1], bbox(rasterToPoints(temp[[1]], spatial = TRUE))[1,2], by = res(temp[[1]])[1]) ) 
			# dim_lat  = ncdim_def(	'lat', 
									# 'degrees_north', 
									# seq( bbox(rasterToPoints(temp[[1]], spatial = TRUE))[2,1], bbox(rasterToPoints(temp[[1]], spatial = TRUE))[2,2], by = res(temp[[1]])[2]) ) 
			# nc = nc_create( 	paste( drv, 'naturalized_flow_indus.nc', sep = '' ), 
								# ncvar_def( vars, vars_units, list(dim_lon,dim_lat,dim_time), -9999, longname = vars_long ), 
								# force_v4 = TRUE, 
								# verbose = FALSE )
			# ncvar_put( 	nc, 
						# ncvar_def( 	vars, 
									# vars_units, 
									# list(dim_lon,dim_lat,dim_time), 
									# -9999, 
									# longname = vars_long ), 
									# aperm(	as.array(temp), c(2,1,3) ) )	
			# nc_close(nc)

			# # Output elevation
			# vars = c('elevation')
			# vars_long = c('elevation')
			# vars_units = c('meters')
			# vars_prec = rep('double',length(vars))
			# nc = nc_create( 	paste( drv, 'elevation_indus.nc', sep = '' ), 
								# ncvar_def( vars, vars_units, list(dim_lon,dim_lat), -9999, longname = vars_long ), 
								# force_v4 = TRUE, 
								# verbose = FALSE )
			# ncvar_put( 	nc, 
						# ncvar_def( 	vars, 
									# vars_units, 
									# list(dim_lon,dim_lat), 
									# -9999, 
									# longname = vars_long ), 
									# aperm(	as.array(elev_out.raster), c(2,1,3) )[,,1] )					
			# nc_close(nc)
			
			# Design flow for hydropower potential calculated based on 30 % exceedance
			design_flow.raster = flow_accumulation.raster
			design_flow.raster[] = NA
			design_hist_flow.raster = design_flow.raster
			design_flow.raster[lu] = apply( flow.df, 1, quantile, probs = 0.7 )
			design_hist_flow.raster[lu] = apply( flow.df[,1:12], 1, quantile, probs = 0.7 )

			# Design head and length assuming maximum of approx. 2 km penstock length and no transfers to different catchments
			# i.e., move down 3 grid cells and calculate the elevation drop - that's the max drop assumed for the penstock in this case.
			dni = match( nt.df$downstream_cell, nt.df$cell )
			dnii = match( nt.df$downstream_cell[ dni ], nt.df$cell )
			dniii = match( nt.df$downstream_cell[ dnii ], nt.df$cell )
			dh.df = nt.df$elevation_change[ dni ] + nt.df$elevation_change[ dnii ] + nt.df$elevation_change[ dniii ]

			# Gross hydropower potential -> in MW
			hydro.df = bind_cols( lapply( names(flow.df), function(iii){ 
				
				data.frame(  ( 0.925 * 0.1 * 1e9 * unlist( flow.df[,iii] ) / ( 60 * 60 * 24 ) ) * unlist( dh.df ) * 1000 * 9.81 * 0.85 / 1e6 ) 
				
				} ) )
			names(hydro.df) = names(flow.df)
			hydro.df = round( hydro.df, digits = 4 )
			hydro.df[is.na(hydro.df[])]=0

			# Design hydropower - inluding the design flow-rate
			hydro_design.df = data.frame(  hydro_design = ( 0.925 * 0.1 * 1e9 * unlist( design_flow.raster[lu] ) / ( 60 * 60 * 24 ) ) * unlist( dh.df ) * 1000 * 9.81 * 0.85 / 1e6 )
			hydro_design.df = round( hydro_design.df, digits = 4 )
			hydro_design.df[is.na(hydro_design.df[])]=0

			### Exclusion zones

			# Create exclusion zone mask from criteria
			exclusion_zones.list = list( dist_hydro.raster )
			exclusion_zone.raster = sum( do.call( stack, lapply( 1:length( exclusion_zones.list ), function(iii){
				exclusion_zones1.raster = exclusion_zones.list[[iii]] 
				exclusion_zones1.raster[ !is.na( exclusion_zones1.raster ) ] = 1
				exclusion_zones1.raster[ is.na( exclusion_zones1.raster ) ] = 0
				return(exclusion_zones1.raster)
				} ) ) )
			exclusion_zone.raster[ exclusion_zone.raster[] > 0 ] = 1

			# Import exclusion zone map for renewables and add to additional hydropower criteria 
			exclusion_zone.raster = ws_excl + exclusion_zone.raster
			exclusion_zone.raster[ exclusion_zone.raster[] > 0 ] = 1

			# Hydropower sites remaining 
			ht.stack = do.call( stack, lapply( names(hydro.df), function(mm){		
				tts = flow_accumulation.raster
				tts[] = NA
				tts[ lu ] = hydro.df[,mm]
				tts = tts * -1* ( exclusion_zone.raster - 1 )
				return(tts)
				} ) )
			names(ht.stack) = names(hydro.df)
			htm.raster = flow_accumulation.raster
			htm.raster[] = NA
			htm.raster[ lu ] = unlist( hydro_design.df )
			htm.raster = htm.raster * -1* ( exclusion_zone.raster - 1 )
			htm0.raster=htm.raster
				
			# Identify run-of-river sites along 5 km stretches of the rivers 
			# move through each polgyon in the basin for computational efficiency
			sites = do.call( rbind, lapply( 1:length(basin.spdf), function(ii){
				
				# mask for the basin
				msk = rasterize( as(basin.spdf,'SpatialPolygons')[ii], htm.raster ) 
				are = area(basin.spdf[ii,])/1e6
				arec = mean( area( msk )[], na.rm = TRUE )
				hp = msk * htm.raster 
				flc = msk * flow_accumulation.raster
				
				# matrices for gathering cells withing 5 km of the identified sire
				around2 = matrix( rep(1,144), ncol = 12, byrow=TRUE ) # set for an approximate 5 km buffer around projects
				around2[6,6] = 0
				around3 = matrix( rep(1,24*24), ncol = 24, byrow=TRUE ) # set for an approximate 5 km buffer around projects
				around3[12,12] = 0
				
				# get the cell ids
				cell = which( hp[] > 0 ) # all cells with flow data 
				cell = cell[ -1*which( cell %in% c( which.max(flc[]), data.frame( adjacent( flow_accumulation.raster, cell = which.max(flc[]), directions=around2, sorted=TRUE, include = TRUE ) )$to ) ) ]
				
				# initialize tracking variables
				sts = NULL
				tts = NULL
				ppr = NULL
				xxs = NULL
				yys = NULL
				pid = NULL
				
				# Check each potential cell within the 5 km radius to find site with max potential
				if( length( cell ) > 0 ){ 
					
					while( length(cell) > 0 ){
						
						st = cell[which.max( hp[ cell ] )]
						
						fl = flow_accumulation.raster[ st ]
						
						pr = hp[ st ]
						
						xs = xFromCell( flow_accumulation.raster, st )
						ys = yFromCell( flow_accumulation.raster, st )
						
						# if the flow accumulation at the site is greater than the subasin area, than
						# the site is located on the main river branch i.e., river plant - otherwise it is likely 
						# on one of the tributaries that run into the main branch i.e., canal plant
						if( ( fl * arec ) > 0.9 * are ){ tt = 'river' }else{ tt = 'canal' }
						
						
						# store the data
						sts = c( sts, st )
						tts  =c( tts, tt )
						ppr = c( ppr, pr )
						xxs = c( xxs, xs )
						yys = c( yys, ys )
						
						# remove all potential sites within 10 km of the current site
						ad = data.frame( adjacent( flow_direction.raster, cell = st, directions=around3, sorted=TRUE, include = TRUE ) )$to
						cell = cell[ -1*which( cell %in% c( st, ad ) ) ] 
						
						} 
					
					# save the polygon id
					pid = rep( basin.spdf@data$PID[ii], length( sts ) )
					
					}
				
				return( data.frame( cell = sts, x = xxs, y = yys, pid = pid, type = tts, power = ppr ) )
				
				} ) )
			
			# limit sites to more than 1 MW
			sites = sites[ -1*which( sites$power < 1 ), ] 
			
			# check the distance between potential sites
			temp = sites
			coordinates(temp) = ~x+y
			dds = sapply( 1:nrow(temp), function(ttt){ return( min( gDistance( temp[ttt,],temp[-1*ttt,], byid=TRUE ) ) ) } )
			
			# create raster with the sites
			temp = htm.raster
			temp[ ] = NA
			temp[ sites$cell ] = round( htm.raster[ sites$cell ], digits=1 )
			htm.raster = temp
			rm(temp)
			nmd=names(ht.stack)
			ht.stack = do.call( stack, lapply( nmd, function(mm){ 
				temp = ht.stack[[ mm ]]
				temp[ ] = NA
				temp[ sites$cell ] = round( ht.stack[[ mm ]][ sites$cell ],digits=1 )
				return(temp)
				} ) )
			names(ht.stack) = nmd

			# Get power output in each month
			sites = cbind( sites, data.frame( do.call( rbind, lapply( 1:nrow(sites), function(iii){ 
				df = data.frame( t( sapply( 1:nlayers(ht.stack), function(mm){ round( ht.stack[[ mm ]][ sites$cell[iii] ],digits=1 ) } ) ) )
				df[df>sites$power[iii]]=round( sites$power[iii], digits = 1 )
				names(df) = names(ht.stack)
				return(df)
				} ) ), row.names = row.names( sites ) ) )

			# plot the sites
			hcol = c('blue','orange')
			pdf( paste0( 'input/check/indus_hydro_sites_',climate_scenario,'_',climate_model,'_extreme.pdf'), width=6, height=6 )
			plot( basin.spdf, col = NA, border = NA, xlab = 'Longitude', ylab = 'Latitude', main = paste0( climate_model, ' : ', climate_scenario ) )
			plot( 	do.call( rbind, lapply( c('IND','PAK','AFG','CHN','NPL','TJK','UZB','TKM','IRN'), function(cnt){ getData( 'GADM', country = cnt, level = 0 ) } ) ), 
					col = alpha( c('khaki','lightgreen','lightblue','lightcoral','grey75','grey75','grey75','grey75','grey75'), alpha = 0.2 ),
					border = 'gray55', add = TRUE )	
			plot( basin.sp, col = NA, border = 'grey25', lwd = 1.5, add=TRUE )
			text( 69, 30, 'PAK', col = 'forestgreen',cex=1.5)
			text( 76, 29, 'IND', col = 'brown',cex=1.5)
			text( 80, 36, 'CHN', col = 'darkred',cex=1.5)
			text( 67, 36, 'AFG', col = 'navy',cex=1.5)
			pt1 = sites %>% filter( type == 'river' ) %>% select( x, y, power ) %>% 
				mutate( cex = ( sqrt( power ) - min( sqrt( power ) ) ) / ( max( sqrt( power ) ) - min( sqrt( power ) ) ) * 3.5 + 1 ) %>%
				'coordinates<-'(~x+y) %>% 'proj4string<-'(proj4string(basin.spdf))		
			points( pt1, pch = 21, cex = pt1@data$cex, col = hcol[1], bg = alpha( 'white', alpha= 0.3 ) )
			pt2 = sites %>% filter( type == 'canal' ) %>% select( x, y, power ) %>% 
				mutate( cex = ( sqrt( power ) - min( sqrt( power ) ) ) / ( max( sqrt( power ) ) - min( sqrt( power ) ) ) * 3.5 + 1 ) %>%
				'coordinates<-'(~x+y) %>% 'proj4string<-'(proj4string(basin.spdf))		
			points( pt2, pch = 21, cex = pt1@data$cex, col = hcol[2], bg = alpha( 'white', alpha= 0.3 ) )
			sizes = c(1, 10, 50, 100 )
			sgap = c(-0.05,0.5,1.3,2.5)
			for( iii in 1:length( sizes ) ){ 
				points( 82.2, 24+sgap[iii], cex = pt1@data$cex[ which.min( ( pt1@data$power - sizes[iii] )^2 ) ], pch = 21, col = 'gray35', bg = 'gray35' ) 
				text( 82, 24+sgap[iii], paste0( sizes[ iii ], ' MW ' ), col = 'gray35', cex = 0.9, pos = 2 )
				}
			axis(side=1,at=c())
			axis(side=2,at=c())
			box()
			par(new=TRUE,oma=c(0.25,11,14.5,6))
			dat = sites %>% 
					select( pid, type, power ) %>% 
					mutate( country = unlist( strsplit( as.character(pid), '_' ) )[seq(1,2*length(pid),by=2)]) %>%
					group_by( country, type ) %>% summarise( power = sum( power ) ) %>% as.data.frame()	%>%
					reshape( . , direction='wide', idvar = 'type', timevar = 'country' )
			row.names(dat) = dat$type
			names(dat) = unlist( strsplit( names(dat), '[.]' ) )[seq(1,2*length(names(dat)),by=2)]
			dat = dat %>% select(-type)		
			mp = barplot( 	as.matrix( dat ),  
							ylim = c(-0.12*round(1.5*max(colSums(dat,na.rm=TRUE)),digits=-2),round(1.5*max(colSums(dat,na.rm=TRUE)),digits=-2)), col = hcol[c(2,1)], ylab = '', yaxt = 'n', cex.axis = 0.8, cex.lab = 0.8, cex.names = 0.8, names.arg = rep('',ncol(dat) ) )
			axis(side=2,las=2,at=c(0,round(max(colSums(dat,na.rm=TRUE)),digits=-2)), labels = c(0,paste0( round(max(colSums(dat,na.rm=TRUE)),digits=-2), ' MW')),cex=0.8,cex.lab=0.8,cex.axis=0.8)
			text( mp, rep(-0.08*round(1.5*max(colSums(dat,na.rm=TRUE)),digits=-2),length(mp)), names(dat), cex = 0.75 )
			legend('top',legend = c('river','canal'), fill = hcol, bty = 'n', cex = 0.85, ncol = 2)
			dev.off()
			
			# Runoff into PIDs
			pids = nt.df$pid
			ups = unique(pids)
			ro.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
				
				rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ sum( runoff.df[ which( pids == pid_check ), timestep ] ) } ) ) )
				
				names(rs1) = timestep
				
				row.names(rs1) = as.character( basin.spdf@data$PID[ ups ] )
				
				return(rs1)
				
				} ) )
			
			# Groundwater recharge into PIDs
			pids = nt.df$pid
			ups = unique(pids)
			rc.df = do.call( cbind, lapply( names(recharge.df), function( timestep ){
				
				rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ sum( recharge.df[ which( pids == pid_check ), timestep ] ) } ) ) )
				
				names(rs1) = timestep
				
				row.names(rs1) = as.character( basin.spdf@data$PID[ ups ] )
				
				return(rs1)
				
				} ) )	

			# Natural flow between PIDs based on reduced form network and transmission loss assumptions
			vo.df = do.call( cbind, lapply( names( ro.df ), function( timestep ){
				
				rom.df = data.frame( unlist( ro.df[,timestep] ), row.names = row.names( ro.df ) )
				
				names(rom.df) = timestep
				
				bm.df = data.frame( basin.spdf )
				
				bm.df$DPID = bm.df$PID[ match( bm.df$DOWN, bm.df$PID ) ]
				
				while( nrow(bm.df)>0 )
					{
					
					up = bm.df$PID[ which( !( bm.df$PID %in% bm.df$DPID ) & !is.na( bm.df$DPID ) ) ]
					
					dn = bm.df$DPID[ which( !( bm.df$PID %in% bm.df$DPID ) & !is.na( bm.df$DPID ) ) ]	
					
					#windows()
					#plot(basin.spdf[match(up,basin.spdf@data$PID),],col='red')
					#plot(basin.spdf[match(dn,basin.spdf@data$PID),],add=TRUE,col='blue')
					
					if(length(which(dn %in% pids))>0){ for( i in 1:length(dn) ){ rom.df[ as.character( dn[i] ), timestep ] = rom.df[ as.character( dn[i] ), timestep ] + rom.df[ as.character( up[i] ), timestep ] } }
					
					bm.df = bm.df[ -1*match( up, bm.df$PID ), ]
					
					}
				
				return(rom.df)	
				
				} ) )
				
			# Pullout river projects and aggregate to PID-level
			hriv.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
				rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ return( sum( c( 0, sites[ which( sites$pid == pid_check & sites$type == 'river' ), timestep ] ), na.rm=TRUE ) ) } ) ) )
				names(rs1) = timestep
				row.names(rs1) = as.character( ups )
				return(rs1)
				} ) )
			
			# Pullout canal projects and aggregate to PID level
			hcan.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
				rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ return( sum( c( 0, sites[ which( sites$pid == pid_check & sites$type == 'canal' ), timestep ] ), na.rm=TRUE ) ) } ) ) )
				names(rs1) = timestep
				row.names(rs1) = as.character( ups )
				return(rs1)
				} ) )
		
			# Linear transformation coefficients for converting network flow to hydropower potential
			hydro_coeff.df = do.call( rbind, lapply( ups, function( pid_check ){ 
				rr = unlist( ro.df[ as.character( pid_check ), ] )
				cc = unlist( vo.df[ as.character( pid_check ), ] )
				mr = mean(rr)
				mc = mean(cc)
				cp = unlist( hcan.df[ as.character( pid_check ), ] )
				rp = unlist( hriv.df[ as.character( pid_check ), ] )
				cx = sum( sites$power[ which( sites$pid == pid_check & sites$type == 'canal'  ) ]  )
				rx = sum( sites$power[ which( sites$pid == pid_check & sites$type == 'river'  ) ]  )
				if( rx > 0 )
					{
					rr = rr[ -1*which( rp >= 0.95 * rx ) ]
					rp = rp[ -1*which( rp >= 0.95 * rx ) ]
					r = lm( rp ~ 0 + rr )
					riv_coeff = unlist( coef(r)[1] )
					windows()
					plot( rr, rp )
					lines( c( min( rr ), max( rr ) ),  riv_coeff * c( min( rr ), max( rr ) ) )
					}else{ riv_coeff = 0 }
				if( cx > 0 )
					{
					cc = cc[ -1*which( cp >= 0.95 * cx ) ]
					cp = cp[ -1*which( cp >= 0.95 * cx ) ]
					r = lm( cp ~ 0 + cc )
					can_coeff = unlist( coef(r)[1] )
					windows()
					plot( cc, cp )
					lines( c( min( cc ), max( cc ) ),  can_coeff * c( min( cc ), max( cc ) ) )
					}else{ can_coeff = 0 }	
				dfs = round( data.frame( 	canal_max_mw = cx, 
											canal_mw_per_km3_per_day = can_coeff, 
											canal_mean_mw = mc * can_coeff,
											canal_cf = max( 0, mc * can_coeff / cx, na.rm=TRUE ), 
											river_max_mw = rx, 
											river_mw_per_km3_per_day = riv_coeff, 
											river_mean_mw = mr * riv_coeff,
											river_cf = max( 0, mr * riv_coeff / rx, na.rm=TRUE )	), digits = 2 )	
				return( dfs )	
				} ) ) 
			row.names( hydro_coeff.df ) = ups	
			
			# Existing hydropower
			basin_hydropower.df = data.frame( readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_power_plants', verbose = FALSE ) ) %>%
				filter( TPS == 'hydro' ) %>%
				select( PID, STATUS, MW ) %>%
				mutate( STATUS = if_else( STATUS %in% c('PLN','CON'), 'PLN', 'OPR'	) ) %>%
				group_by( PID, STATUS ) %>% summarise( MW = sum(MW ) ) %>% as.data.frame()	
												
			hydro_coeff.df$existing_MW = round( sapply( row.names(hydro_coeff.df), function(iii){ return( sum( basin_hydropower.df$MW[ which( as.character( unlist( basin_hydropower.df$PID ) ) == iii & as.character( unlist( basin_hydropower.df$STATUS ) ) == 'OPR' )  ] ) ) } ) )
			hydro_coeff.df$planned_MW = round( sapply( row.names(hydro_coeff.df), function(iii){ return( sum( basin_hydropower.df$MW[ which( as.character( unlist( basin_hydropower.df$PID ) ) == iii & as.character( unlist( basin_hydropower.df$STATUS ) ) == 'PLN' )  ] ) ) } ) )

			# Output to csv
			names(ro.df) = unlist( lapply( paste( 'runoff_km3_per_day', c(2015,2020,2030,2040,2050,2060), sep = '_' ), function( vv ){ return( paste( vv, c( seq(1,12,by=1) ), sep = '.' ) ) } ) )
			names(rc.df) = unlist( lapply( paste( 'recharge_km3_per_day', c(2015,2020,2030,2040,2050,2060), sep = '_' ), function( vv ){ return( paste( vv, c( seq(1,12,by=1) ), sep = '.' ) ) } ) )
			out.df = cbind( data.frame( PID = row.names(hydro_coeff.df ) ),
							round(ro.df,digits=4),
							round(rc.df,digits=4),
							hydro_coeff.df )
			out.df = out.df[ order( out.df$PID ), ]				
			write.csv( out.df, paste0( 'input/basin_water_resources/basin_water_resources_',climate_model,'_',climate_scenario,'_extreme.csv' ), row.names=FALSE )

			names(vo.df) = unlist( lapply( paste( 'natural_flow_km3_per_day', c(2015,2020,2030,2040,2050,2060), sep = '_' ), function( vv ){ return( paste( vv, c( seq(1,12,by=1) ), sep = '.' ) ) } ) ) 
			write.csv( cbind( data.frame( PID = ups ), vo.df ), paste0('input/basin_water_resources/basin_environmental_flow_',climate_model,'_',climate_scenario,'_extreme.csv'), row.names=FALSE ) 
			
			graphics.off()
			
			gc()
			
			}
		
		}
	
	}
	
# # Summary plot


# # import water resources

# wr.df = bind_rows( lapply( list.files( 'input/basin_water_resources', pattern = 'basin_water_resources_' ), function( fl ){
	
	# read.csv( paste0 ( 'input/basin_water_resources/', fl ) ) %>% 
		# select( names( . )[ grepl( 'runoff|recharge|PID', names( . ) ) ] ) %>%
		# melt( ., id = 'PID' ) %>%
		# mutate( dat = variable ) %>% 
		# mutate( type = unlist( strsplit( as.character( dat ), '_' ) )[ seq( 1, 5*length( dat ), by = 5 ) ],
				# year = unlist( strsplit( unlist( strsplit( as.character( dat ), '_' ) )[ seq( 5, 5*length( dat ), by = 5 ) ], '[.]' ) )[seq(1,2*length(dat),by=2)],
				# month = unlist( strsplit( as.character( dat ), '[.]' ) )[ seq( 2, 2*length( dat ), by = 2 ) ],
				# climate_model = unlist( strsplit( unlist( strsplit( fl, '.csv' ) )[1], '_' ) )[4],
				# climate_scenario = unlist( strsplit( unlist( strsplit( fl, '.csv' ) )[1], '_' ) )[5] ) %>%
				# left_join( ., data.frame( month = as.character(1:12), days = c(31,28.25,31,30,31,30,31,31,30,31,30,31) ) ) %>%
				# left_join( ., bind_rows( data.frame( month = as.character( c( 6, 7, 8, 9, 10, 11 ) ), season = 'kharif' ), bind_rows( data.frame( month = as.character( c( 12, 1, 2, 3, 4, 5 ) ), season = 'rabi' ) ) ) ) %>%
				# mutate( country = unlist( strsplit( as.character( PID ), '_') )[ seq( 1, 2*length( PID ), by = 2 ) ] ) %>% 
				# mutate( value = value * days, units = 'km3' ) %>% 
		# select( PID, country, climate_scenario, climate_model, type, units, year, season, month, value )
	
	# } ) )
	
# # Boxplot by kharif and rabi seasons for each country	
# df = wr.df %>% select( type, country, climate_scenario, climate_model, year, season, value ) %>% 
	# group_by( type, country, climate_scenario, climate_model, season, year ) %>% 
	# summarise( value = mean( value ) ) %>% as.data.frame() 

# pdf( 'input/check/water_inflows.pdf', width = 6.5, height = 8.5 ) 
# p1 = layout( matrix( c(9,9,1,5,2,6,3,7,4,8),5,2,byrow=TRUE ), widths = c(0.3,0.3), heights= c(0.05,0.2,0.2,0.2,0.2) )
# par(mar=c(3,3,2,1),oma=c(0,0,0,0))
# for( tp in unique( wr.df$type ) )
	# {
	# for( cn in unique( wr.df$country ) )
		# {
		# df1 = df %>% filter( country == cn, type == tp ) %>% select( climate_scenario, season, value )
		# boxplot( 	value ~ climate_scenario + season, data = df1,
					# at = c(1,2,3, 4,5,6), boxlwd = 0.5, medlwd = 0.5,
					# col = c('deepskyblue','forestgreen','orange'),
					# names = c( '', unique( df1$season )[1], '', '', unique( df1$season )[2], '' ),
					# xaxs = FALSE, xaxt = 'n',main = bquote( .(cn) ~ ' - ' ~ .(tp) ~ ' [ ' ~ km^3 ~ ' ]' ) ) 
		# axis( side = 1, at = c(1,2,3, 4,5,6), labels = c( '', unique( df1$season )[1], '', '', unique( df1$season )[2], '' ), lwd.ticks=FALSE ) 			
		# abline(v=3.5)
		# }
	# }
# par(mar=c(0,0,0,0))
# plot.new()
# legend('center',legend = c( 'historical','rcp2.6','rcp6.0' ), fill = c('deepskyblue','forestgreen','orange'), ncol= 3, bty = 'n', cex = 1.2 )	
# dev.off()	
