
#source('C:/Users/parkinso/indus_ix/input_data_scripts/water_resources.r')

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

if( basin == 'Indus' )
	{
	basin.spdf@data$river = basin.spdf@data$REGION
	}
	
# Get the climate model and scenario names
fls = list.files( 'input/hydroclimate_input')
climate_models = unique( unlist( strsplit( fls, '_' ) )[seq(3,4*length( fls ),by=4 ) ] )
climate_scenarios = unique( unlist( strsplit( fls, '_' ) )[seq(4,4*length( fls ),by=4 ) ] )
climate_scenarios = unlist( strsplit( climate_scenarios, '[.]' ) )[ seq(1,2*length(climate_scenarios),by=2) ]	
	
# create stack of climate model -scenario combinations	
ghm.list = lapply( climate_models, function( cc ){ 
	r1 = lapply( climate_scenarios, function( ss ){ 
		if( file.exists( paste0( 'input/hydroclimate_input/runoff_metersperday_',cc,'_', ss, '.tif' ) ) ){
			r = stack( paste0( 'input/hydroclimate_input/runoff_metersperday_',cc,'_', ss, '.tif' ) )
			names(r) = c( sapply( c(2010,2020,2030,2040,2050,2060), function(yy){ return( paste( yy, seq(1,12,by=1), sep='.') ) } ) ) 
			return(r)	
			}
		} )
	names(r1) = climate_scenarios
	return(r1)	
	} )
names(ghm.list) = climate_models		

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
# Get the sub-bcu land units and elevation at these unit - limit to cells with at least 5 km2 of upstream area
ae = area( flow_accumulation.raster )
ae[ which( !( 1:length(ae[]) %in% which(!is.na(flow_accumulation.raster[])))) ]=NA
lu = which( !is.na( flow_accumulation.raster[] ) & ( flow_accumulation.raster[] > 0 ) & ( flow_accumulation.raster[] >= round( 5 / mean(ae[],na.rm=TRUE) ) ) )
fa = c( flow_accumulation.raster[ lu ] ) 
fd = c( flow_direction.raster[ lu ] )
hi = c( elevation.raster[ lu ] )
ar = c( ae[ lu ] )

# Get adjacent cells
around = matrix( c(	1, 1, 1, 
					1, 0, 1, 
					1, 1, 1 ), ncol = 3, byrow=TRUE )
around2 = matrix( rep(1,144), ncol = 12, byrow=TRUE )
around2[6,6] = 0
hmat = c( 32, 64, 128, 
		  16, 0, 1, 
		  8, 4, 2 )
adj = data.frame( adjacent( flow_direction.raster, cell = lu, directions=around, sorted=TRUE, include = TRUE ) )
adj = adj[ which( adj$from %in% lu ),  ]
adj$hmat = unlist(lapply(1:length(lu),function(iii){ return(hmat) } ) )

# Use flow direction matrix to determine the direction of downstream
sd = match( lu, adj$from )
se = match( fd, adj$hmat )
tm = unlist(adj$to)
down = sapply( 1:length(lu), function( x ){ tm[ c( sd[x]+se[x] - 1 ) ] } )
dh = hi - hi[ match( down, lu ) ]
dh[ dh < 0 ] = 0	
xloc = xFromCell( flow_accumulation.raster, lu )
yloc = yFromCell( flow_accumulation.raster, lu )
locs = data.frame( x = xloc, y = yloc )

# Get the mapping to cells from the GHM at lower resolution
ghm_cell = ghm.list[[1]][[1]][[1]]
ghm_cell[] = 1:length(ghm_cell[])
ghm_cell = resample( ghm_cell, flow_accumulation.raster, method = 'ngb')
ghm_cell = ghm_cell[ lu ]

if( length(which(is.na(ghm_cell))) > 0 )
	{
	pts = SpatialPoints( data.frame( xyFromCell( flow_accumulation.raster, lu[ which(is.na(ghm_cell)) ] ) ) )
	proj4string(pts) = proj4string(basin.sp)
	ghm_cell[ which(is.na(ghm_cell)) ] = unlist( lapply( 1:length(pts), function(ppp){ basin.spdf@data$PID[ which.min(gDistance(pts[ppp],basin.spdf,byid=TRUE)) ] } ) )
	}
	
# Get the PID	
basin.raster = resample( basin.raster, flow_accumulation.raster, method = 'ngb' )
basin_cell = basin.raster[ lu ]
if( length(which(is.na(basin_cell))) > 0 )
	{
	pts = SpatialPoints( data.frame( xyFromCell( flow_accumulation.raster, lu[ which(is.na(basin_cell)) ] ) ) )
	proj4string(pts) = proj4string(basin.sp)
	basin_cell[ which(is.na(basin_cell)) ] = unlist( lapply( 1:length(pts), function(ppp){ which.min(gDistance(pts[ppp],basin.spdf,byid=TRUE)) } ) )
	}
basin.raster[ lu ] = basin_cell
down_basin_cell = basin_cell[ match( down, lu ) ]
nt.df = data.frame( xloc = xloc, yloc = yloc, cell = lu, downstream_cell = down, elevation = hi, elevation_change = dh, flow_accumulation = fa, ghm_cell = ghm_cell, pid = basin.spdf@data$PID[basin_cell], down_pid = basin.spdf@data$PID[down_basin_cell] )

# Calibrate flow accumulation - get the upstream flow accumulation to calibrate the estimate of internal runoff
act_acc = unlist( nt.df$flow_accumulation )
est_acc = rep( 1, length( nt.df$flow_accumulation ) )
ntm.df = nt.df
while( nrow(ntm.df) > 0  )
	{
	cell_tot = nrow(ntm.df)
	cell_start = as.integer( unlist( ntm.df$cell[ which( !( ntm.df$cell %in% ntm.df$downstream_cell ) ) ] ) )
	ind_start = match( cell_start, nt.df$cell )
	cell_down = as.integer( unlist( nt.df$downstream_cell[ ind_start ] ) )
	ind_down = match( cell_down, nt.df$cell )
	if( length(ind_down) > 0 ){ for( i in 1:length(cell_down) ){ if( !is.na(ind_down[i]) & ind_down[i] != ind_start[i] ){ est_acc[ ind_down[i] ] = est_acc[ ind_down[i] ] + act_acc[ ind_start[i] ] } } }
	ntm.df = ntm.df[ -1*which( ntm.df$cell %in% cell_start ), ]
	}
nt.df$partial_flow_accumulation = act_acc - est_acc
a = area(basin.raster)
a[which(!( 1:length(a[])%in% which(!is.na(basin.raster[]))))]=NA
nt.df$area = ( 1 + nt.df$partial_flow_accumulation ) * a[lu]
nt.df$area_og =  a[lu]

# Existing hydropower plants from van Vliet et al. 2015 used to exclude certain areas
				
# add hydropower
global_hydropower.spdf = readOGR("input/powerplants",'global_hydropower_plants')
global_hydropower.df = data.frame(global_hydropower.spdf)

# add planned projects
planned_projects.df = data.frame( read.csv( "input/powerplants/indus_future_hydro_projects.csv", stringsAsFactors = FALSE ) )
planned_projects.df = planned_projects.df[, c('x','y','opening','capacity_MW') ]
names(planned_projects.df) = c( 'coords.x1', 'coords.x2', 'YEAR', 'MW' )
planned_projects.df$STATUS = 'PLN'
planned_projects.df = planned_projects.df[,c( 'MW','STATUS','YEAR','coords.x1','coords.x2')]
global_hydropower.df = global_hydropower.df[,c( 'MW','STATUS','YEAR','coords.x1','coords.x2')]
global_hydropower.df = rbind( global_hydropower.df, planned_projects.df)
coordinates(global_hydropower.df) = ~ coords.x1 + coords.x2
proj4string(global_hydropower.df) = proj4string(flow_accumulation.raster)
global_hydropower.df = spTransform( global_hydropower.df, crs(basin.sp) )
basin_hydropower.df = global_hydropower.df[ -1*which(is.na(over(global_hydropower.df,basin.sp))), ] # just keep units in basin

# Distance to hydropower stations of different sizes
basin_hydropower.raster = rasterize( basin_hydropower.df, field='MW', basin.raster, fun=sum )
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
temp1[ which( temp1[] <= 5  )  ] = 1 # don't install less than 5 km away for less than 100 MW
temp1[ temp1[] != 1 ] = 0
temp2 = dist_hydro_hi.raster
temp2[ which( temp2[] <= 10  ) ] = 1 # don't install less than 10 km away for greater than 100 MW
temp2[ temp2[] != 1 ] = 0
dist_hydro.raster = temp1 + temp2
dist_hydro.raster[ dist_hydro.raster[] %in% c(1,2) ] = 1
dist_hydro.raster[ dist_hydro.raster[] != 1 ] = NA

# resample to match flow accumulation
dist_hydro.raster = resample(dist_hydro.raster, flow_accumulation.raster, method="bilinear")


## Go through each GCM-scenario combination

for( climate_scenario in climate_scenarios[1:3] )
	{
	
	for( climate_model in climate_models )
		{

		# Get the runoff from low resolution hydrological model
		ghm_dynamic.stack = ghm.list[[ climate_model ]][[ climate_scenario ]]
		f.stack  = ghm_dynamic.stack / 1000 # convert to km per day
		proj4string(f.stack) = proj4string(flow_accumulation.raster)
		runoff.df = data.frame( do.call( cbind, lapply( 1:nlayers( f.stack ), function( nnn ){ return( unlist( f.stack[[ nnn ]][ c(nt.df$ghm_cell) ] ) * unlist( nt.df$area ) ) } ) ) ) # in km3 per day
		names(runoff.df) = names(f.stack)
		runoff.df[is.na(runoff.df)]=0
		annual_inflow_km3 = sapply( 1:6, function(i){ sum( apply( runoff.df[,(12*(i-1)+1):(12*i)], 1, mean ), na.rm=TRUE ) * 365 } )

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
			
			cell_tot = nrow(ntm.df)
			cell_start = as.integer( unlist( ntm.df$cell[ which( !( ntm.df$cell %in% ntm.df$downstream_cell ) ) ] ) )
			ind_start = match( cell_start, nt.df$cell )
			cell_down = as.integer( unlist( nt.df$downstream_cell[ ind_start ] ) )
			ind_down = match( cell_down, nt.df$cell )
			
			trn_loss = as.numeric( unlist( nt.df$trn_loss[ ind_start ] ) )
			
			# print( max( trn_loss ) )
			
			thr = 3
			
			if( length(ind_down) > thr )
				{ 
				
				nm = names(flow.df)
				
				flow.df = data.frame( do.call( cbind, lapply( 1:ncol( flow.df ), function( ttt ){
					
					fl = as.vector( flow.df[ , ttt ] ) 
					
					for( i in which( !is.na( ind_down ) ) ){ if( ind_down[i] != ind_start[i] ){ fl[ ind_down[i] ] = fl[ ind_down[i] ] + fl[ ind_start[i] ] * ( 1 - trn_loss[ i ] ) } }
					
					return(fl)	
					
					} ) ) )
				
				names(flow.df) = nm		
				
				}else
				{ # exclude transmission losses and compute all time steps simultaneuosuly - more efficient for few indices
				
				if( length(ind_down) > 0 ){ for( i in which( !is.na( ind_down ) ) ){ if( ind_down[i] != ind_start[i] ){ flow.df[ ind_down[i], ] = as.vector( flow.df[ ind_down[i], ] ) + as.vector( flow.df[ ind_start[i], ] ) } } }
				
				}
			
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

		# Design flow based on 30 % exceedance
		design_flow.raster = flow_accumulation.raster
		design_flow.raster[] = NA
		design_hist_flow.raster = design_flow.raster
		design_flow.raster[lu] = apply( flow.df, 1, quantile, probs = 0.7 )
		design_hist_flow.raster[lu] = apply( flow.df[,1:12], 1, quantile, probs = 0.7 )

		# Design head and length assuming maximum of approx. 2 km penstock length and no transfers to different catchments
		dni = match( nt.df$downstream_cell, nt.df$cell )
		dnii = match( nt.df$downstream_cell[ dni ], nt.df$cell )
		dniii = match( nt.df$downstream_cell[ dnii ], nt.df$cell )
		dh.df = nt.df$elevation_change[ dni ] + nt.df$elevation_change[ dnii ] + nt.df$elevation_change[ dniii ]

		# Gross hydropower potential -> in MW
		hydro.df = bind_cols( lapply( names(flow.df), function(iii){ return( data.frame(  ( 0.925 * 0.1 * 1e9 * unlist( flow.df[,iii] ) / ( 60 * 60 * 24 ) ) * unlist( dh.df ) * 1000 * 9.81 * 0.85 / 1e6 ) ) } ) )
		names(hydro.df) = names(flow.df)
		hydro.df = round( hydro.df, digits = 4 )
		hydro.df[is.na(hydro.df[])]=0

		# Design hydropower 
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
		exclusion_zone.raster = raster('input/fraction_grid_cell_available_wind_solar.asc') + exclusion_zone.raster
		exclusion_zone.raster[ exclusion_zone.raster[] > 0 ] = 1

		# Hydropower sites remaining 
		ht.stack = do.call( stack, lapply( names(hydro.df), function(mm){		
			tts = flow_accumulation.raster
			tts[] = NA
			tts[ lu ] = hydro.df[,mm]
			tts = tts * -1* ( exclusion_zone.raster - 1 )
			print(sum(tts[],na.rm=TRUE))
			return(tts)
			} ) )
		names(ht.stack) = names(hydro.df)
		htm.raster = flow_accumulation.raster
		htm.raster[] = NA
		htm.raster[ lu ] = unlist( hydro_design.df )
		htm.raster = htm.raster * -1* ( exclusion_zone.raster - 1 )
		htm0.raster=htm.raster

		# Identify sites
		sites = do.call( rbind, lapply( 1:length(basin.spdf), function(ii){
			
			msk = rasterize( as(basin.spdf,'SpatialPolygons')[ii], htm.raster ) 
			are = area(basin.spdf[ii,])/1e6
			arec = mean( area( msk )[], na.rm = TRUE )
			hp = msk * htm.raster 
			flc = msk * flow_accumulation.raster
			
			around2 = matrix( rep(1,144), ncol = 12, byrow=TRUE ) # set for an approximate 5 km buffer around projects
			around2[6,6] = 0
			around3 = matrix( rep(1,24*24), ncol = 24, byrow=TRUE ) # set for an approximate 5 km buffer around projects
			around3[12,12] = 0
			
			cell = which( hp[] > 0 ) # all cells with flow data 
			cell = cell[ -1*which( cell %in% c( which.max(flc[]), data.frame( adjacent( flow_accumulation.raster, cell = which.max(flc[]), directions=around2, sorted=TRUE, include = TRUE ) )$to ) ) ]
			
			sts = NULL
			tts = NULL
			ppr = NULL
			xxs = NULL
			yys = NULL
			pid = NULL
			
			if( length( cell ) > 0 ){ 
				
				while( length(cell) > 0 ){
					
					st = cell[which.max( hp[ cell ] )]
					
					fl = flow_accumulation.raster[ st ]
					
					pr = hp[ st ]
					
					xs = xFromCell( flow_accumulation.raster, st )
					ys = yFromCell( flow_accumulation.raster, st )
					
					if( ( fl * arec ) > 0.9 * are ){ tt = 'river' }else{ tt = 'canal' }
					
					sts = c( sts, st )
					tts  =c( tts, tt )
					
					ppr = c( ppr, pr )
					xxs = c( xxs, xs )
					yys = c( yys, ys )
					
					ad = data.frame( adjacent( flow_direction.raster, cell = st, directions=around3, sorted=TRUE, include = TRUE ) )$to
					
					cell = cell[ -1*which( cell %in% c( st, ad ) ) ] 
					
					} 
				
				print( sum( c(0,hp[ sts ]), na.rm=TRUE ) )	
				pid = rep( basin.spdf@data$PID[ii], length( sts ) )
				
				}else{ print( 0 ) }
			
			
			return(data.frame( cell = sts, x = xxs, y = yys, pid = pid, type = tts, power = ppr ))
			
			} ) )

		sites = sites[ -1*which( sites$power < 1 ), ] 

		temp = sites
		coordinates(temp) = ~x+y
		dds = sapply( 1:nrow(temp), function(ttt){ return( min( gDistance( temp[ttt,],temp[-1*ttt,], byid=TRUE ) ) ) } )

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

		# windows()
		# plot(basin.sp, border = alpha( 'black', alpha = 0.6 ), lwd = 1.5 )
		# symbols(sites$x, sites$y, circles = sqrt(sites$power), inches=0.1, bg = 'blue', fg = 'white', add=TRUE)	
			
		# dd = sites[,c('x','y','power')]
		# coordinates(dd) = ~x+y
		# rr=raster()
		# res(rr) = 0.1
		# rr=crop(rr,extent(dd))
		# rr=rasterize(dd,rr,'power',fun=sum)
		# pdf(paste('input/hydropowerpotential.pdf',sep=''))
		# print( levelplot(rr,zscaleLog=TRUE,margin=FALSE )+layer(sp.polygons( basin.sp,col=alpha('grey31',0.3))) )
		# dev.off()	

		# Runoff into PIDs
		pids = nt.df$pid
		ups = unique(pids)
		ro.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
			
			rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ return( sum( runoff.df[ which( pids == pid_check ), timestep ] ) ) } ) ) )
			
			names(rs1) = timestep
			
			row.names(rs1) = as.character( basin.spdf@data$PID[ ups ] )
			
			return(rs1)
			
			} ) )	

		# Natural flow between PIDs
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
			
		# cc = data.frame( vo.df[,'X2010.1'], row.names = row.names(vo.df) )
		# names(cc) = 'X2010.1'
		# cc$PID = row.names(vo.df)
		# dd = basin.spdf
		# dd = dd[,'PID']
		# dd = merge(dd,cc,by='PID')
		# spplot( dd[,'X2010.1'] )

		hriv.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
			rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ return( sum( c( 0, sites[ which( sites$pid == pid_check & sites$type == 'river' ), timestep ] ), na.rm=TRUE ) ) } ) ) )
			names(rs1) = timestep
			row.names(rs1) = as.character( ups )
			return(rs1)
			} ) )

		hcan.df = do.call( cbind, lapply( names(runoff.df), function( timestep ){
			rs1 = data.frame( unlist( lapply( ups, function( pid_check ){ return( sum( c( 0, sites[ which( sites$pid == pid_check & sites$type == 'canal' ), timestep ] ), na.rm=TRUE ) ) } ) ) )
			names(rs1) = timestep
			row.names(rs1) = as.character( ups )
			return(rs1)
			} ) )
	
		# Linear transformation coefficients
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
			return( round( data.frame( 	canal_max_mw = cx, 
										canal_mw_per_km3_per_day = can_coeff, 
										canal_mean_mw = mc * can_coeff,
										canal_cf = max( 0, mc * can_coeff / cx, na.rm=TRUE ), 
										river_max_mw = rx, 
										river_mw_per_km3_per_day = riv_coeff, 
										river_mean_mw = mr * riv_coeff,
										river_cf = max( 0, mr * riv_coeff / rx, na.rm=TRUE )	), digits = 2 ) )	
			} ) ) 
		row.names( hydro_coeff.df ) = ups	

		# Existing hydropower
		basin_hydropower.df$PID =  over(basin_hydropower.df,basin.spdf[,'PID'])
		hydro_coeff.df$existing_MW = round( sapply( row.names(hydro_coeff.df), function(iii){ return( sum( basin_hydropower.df$MW[ which( as.character( unlist( basin_hydropower.df$PID ) ) == iii & as.character( unlist( basin_hydropower.df$STATUS ) ) == 'OPR' )  ] ) ) } ) )
		hydro_coeff.df$planned_MW = round( sapply( row.names(hydro_coeff.df), function(iii){ return( sum( basin_hydropower.df$MW[ which( as.character( unlist( basin_hydropower.df$PID ) ) == iii & as.character( unlist( basin_hydropower.df$STATUS ) ) == 'PLN' )  ] ) ) } ) )

		# Output to csv
		names(ro.df) = unlist( lapply( paste( 'runoff_km3_per_day', c(2015,2020,2030,2040,2050,2060), sep = '_' ), function( vv ){ return( paste( vv, c( seq(1,12,by=1) ), sep = '.' ) ) } ) )
		out.df = cbind( data.frame( PID = row.names(hydro_coeff.df ) ),
						round(ro.df,digits=4),
						hydro_coeff.df )
		out.df = out.df[ order( out.df$PID ), ]				
		write.csv( out.df, paste0( 'input/basin_water_resources/basin_water_resources_',climate_model,'_',climate_scenario,'.csv' ), row.names=FALSE )

		names(vo.df) = unlist( lapply( paste( 'natural_flow_km3_per_day', c(2015,2020,2030,2040,2050,2060), sep = '_' ), function( vv ){ return( paste( vv, c( seq(1,12,by=1) ), sep = '.' ) ) } ) ) 
		write.csv( cbind( data.frame( PID = ups ), vo.df ), paste0('input/basin_water_resources/basin_environmental_flow_',climate_model,'_',climate_scenario,'.csv'), row.names=FALSE ) 
		
		graphics.off()
		
		}
	
	}
	
# Append historical capacity to include planned projects
out.df = read.csv( 'input/basin_water_resources.csv', stringsAsFactors=FALSE )

# historical capacity csv
historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 

# remove previous if existing
ind = which( as.character( historical_capacity.df1$tec ) == 'hydro' & as.character( historical_capacity.df1$year_all ) == '2020' )
if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] }

# Add the planned hydropower capacity 
ind = which( out.df$planned_MW > 0 )
planned_hydro.df = data.frame( 	node = out.df$PID[ ind ],
								tec = rep( 'hydro', length(ind) ) ,
								year_all = rep( 2020, length(ind) ),
								value = out.df$planned_MW[ ind ],
								units = rep( 'MW', length(ind) ) )
historical_capacity.df1 = rbind( historical_capacity.df1, planned_hydro.df )								
write.csv( 	historical_capacity.df1, 
			"input/historical_new_cap.csv", 
			row.names = FALSE )

	