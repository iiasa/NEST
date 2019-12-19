
require(tidyverse)
require(ncdf4)
require(rgdal)
require(raster)
require(rgeos)
require(ggmap)
require(rasterVis)
require(countrycode)
memory.limit(size=1e9)

ssps= c( 'SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5' )
n_days = c(31,28,31,30,31,30,31,31,30,31,30,31)


# ISWEL folder for data
setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin = 'Indus'
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform(basin.spdf, CRS("+proj=longlat"))
basin.spdf@data$PID = as.character( basin.spdf@data$PID )
basin.spdf@data$CNTRY_ID = unlist( strsplit( basin.spdf@data$PID, '_' ) )[ seq(1,2*length(basin.spdf),by=2) ]
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
buff.sp = gBuffer( basin.sp, width = 0.1 ) 
buff2.sp = gBuffer( basin.sp, width = 10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)
		
## Electricity demand models from historical national data
graphics.off()
national.list = lapply( c('PAK','IND'), function(CNT){
	
	# Import historical socioeconomic and electricity data for estimating national model
	iea_wbi_population_gdp_2000to2015 = data.frame(read.csv(paste("input/",CNT,"_iea_wbi_population_gdp_2000to2015.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=F, as.is=T))
	iea_electricity_consumption_2000to2015_by_sector_gwh = data.frame(read.csv(paste("input/",CNT,"_iea_electricity_consumption_2000to2015_by_sector_gwh.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=F, as.is=T))

	# National SSP data
	national_population.df = data.frame(read.csv("input/OECD_SSP_POP.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) %>% # in millions
		filter( Region == CNT ) %>% dplyr::select( paste('X',seq(2015,2060,by=5),sep='') ) * 1e6
		
	# Fill in missing years	
	national_population.df = data.frame( do.call( rbind, lapply( 1:nrow(national_population.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_population.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_population.df) = as.character(seq(2015,2060,by=1))
	row.names(national_population.df) = ssps
	
	# make sure same start point
	for( yyy in as.character( 2015:2018 ) ){ national_population.df[ 2:5, yyy ] = national_population.df[1, yyy ] }
	
	# now for the gdp
	national_gdp.df = data.frame(read.csv("input/OECD_SSP_GDP_PPP.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) %>% # in billions
		filter( Region == CNT ) %>% dplyr::select( paste('X',seq(2015,2060,by=5),sep='') ) * 1e9
	
	# Fill in missing years	
	national_gdp.df = data.frame( do.call( rbind, lapply( 1:nrow(national_gdp.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_gdp.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_gdp.df) = as.character(seq(2015,2060,by=1))
	row.names(national_gdp.df) = ssps
	
	# make sure same start point
	for( yyy in as.character( 2015:2018 ) ){ national_gdp.df[ 2:5, yyy ] = national_gdp.df[1, yyy ] }
	
	# now urbanization
	national_urbanization.df = data.frame(read.csv("input/NCAR_SSP_Urban_Population_Share.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) %>% # in percent
		filter( Region == CNT ) %>% dplyr::select( paste('X',seq(2015,2060,by=5),sep='') ) * 0.01
	national_urbanization.df = data.frame( do.call( rbind, lapply( 1:nrow(national_urbanization.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_urbanization.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_urbanization.df) = as.character(seq(2015,2060,by=1))
	row.names(national_urbanization.df) = ssps
	
	# make sure same start point
	for( yyy in as.character( 2015:2018 ) ){ national_urbanization.df[ 2:5, yyy ] = national_urbanization.df[1, yyy ] }
		
	# Per capita income
	national_income.df = national_gdp.df / national_population.df
	
	# Calibrate historical data to match scale of ssps 
	iea_wbi_population_gdp_2000to2015$pop = iea_wbi_population_gdp_2000to2015$pop * ( national_population.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$pop[nrow(iea_wbi_population_gdp_2000to2015)] )
	iea_wbi_population_gdp_2000to2015$gdp = iea_wbi_population_gdp_2000to2015$gdp * ( national_gdp.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$gdp[nrow(iea_wbi_population_gdp_2000to2015)] )
	iea_wbi_population_gdp_2000to2015$inc = iea_wbi_population_gdp_2000to2015$inc * ( national_income.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$inc[nrow(iea_wbi_population_gdp_2000to2015)] )
	
	## Estimate models - using most recent years to capture recent accelerated growth rates
	
	# Residential
	inds = 10:16 # years to include
	x = iea_wbi_population_gdp_2000to2015$inc[inds]
	y = 1e6 * iea_electricity_consumption_2000to2015_by_sector_gwh$Residential[inds] / iea_wbi_population_gdp_2000to2015$pop[inds]
	r = lm( log(y) ~ log(x) )
	res.a = unlist( coef(r)[1] )
	res.b = unlist( coef(r)[2] )
	yi = exp( res.a + res.b * log(x) )
	windows()
	plot(x,y,main=paste(CNT,' - Residential',sep=''),xlab='per capita income [USD]',ylab='per capita consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))
	
	# Commercial
	x = iea_wbi_population_gdp_2000to2015$inc[inds]
	y = 1e6 * iea_electricity_consumption_2000to2015_by_sector_gwh$Commercial[inds] / iea_wbi_population_gdp_2000to2015$pop[inds]
	r = lm( log(y) ~ log(x) )
	com.a = unlist( coef(r)[1] )
	com.b = unlist( coef(r)[2] )
	yi = exp( com.a + com.b * log(x) )
	windows()
	plot(x,y,main=paste(CNT,' - Commercial',sep=''),xlab='per capita income [USD]',ylab='per capita consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))

	# Industry
	x = iea_wbi_population_gdp_2000to2015$gdp[inds]
	y = iea_electricity_consumption_2000to2015_by_sector_gwh$Industry[inds]
	r = lm( log(y) ~ log(x) )
	ind.a = unlist( coef(r)[1] )
	ind.b = unlist( coef(r)[2] )
	yi = exp(ind.a + ind.b * log(x))
	windows()
	plot(x,y,main=paste(CNT,' - Industrial',sep=''),xlab='GDP [USD]',ylab='consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))

	## Future demands - use the models to project forward
	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Residential[16]
	mod = national_population.df * ( exp( res.a + res.b * log( national_income.df ) ) ) / 1e6
	lambda = log(0.1) / length(seq(2015,2060,by=1))
	
	iiii = c( 1:length(seq(2015,2030,by=1)), seq(length(seq(2015,2030,by=1)), 30.75, by=0.5 ) )
	if( CNT == 'PAK' )
		{
		eff_res = data.frame( do.call( rbind, lapply( c(-4.5,-4.5,-4,-4.5, -4.5 )/100, function(iii){ ( 1 - iii )^( iiii ) } ) ) )
		eff_ind = data.frame( do.call( rbind, lapply( c(-4.5,-4.5,0,-4.5, -4.5 )/100, function(iii){ ( 1 - iii )^( iiii ) } ) ) )
		}else{
		eff_res = data.frame( do.call( rbind, lapply( c(-4,-4,-4,-4, -4 )/100, function(iii){ ( 1 - iii )^( iiii ) } ) ) )
		eff_ind = data.frame( do.call( rbind, lapply( c(-3,-3,0,-3, -3  )/100, function(iii){ ( 1 - iii )^( iiii ) } ) ) )
		}
		
	tt = seq(1,length(seq(2015,2060,by=1)))
	national_residential_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff_res

	for( ss in 1:nrow( national_residential_gwh.df ) ){ national_residential_gwh.df[ss,] = national_residential_gwh.df[ss,] }
	
	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Commercial[16]
	mod = national_population.df * ( exp( com.a + com.b * log( national_income.df ) ) ) / 1e6
	national_commercial_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff_res
	for( ss in 1:nrow( national_commercial_gwh.df ) ){ national_commercial_gwh.df[ss,] = national_commercial_gwh.df[ss,]  }		
			
	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Industry[16]
	mod = exp( ind.a + ind.b * log( national_gdp.df ) )
	national_industry_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff_ind
	for( ss in 1:nrow( national_industry_gwh.df ) ){ national_industry_gwh.df[ss,] = national_industry_gwh.df[ss,] }
	
	# Check national electricity demand projections
	windows()
	aa = c( unlist(national_residential_gwh.df),
			unlist(national_commercial_gwh.df),
			unlist(national_industry_gwh.df), 
			unlist(iea_electricity_consumption_2000to2015_by_sector_gwh[,2:4]) 
			)
	matplot(iea_electricity_consumption_2000to2015_by_sector_gwh[1],iea_electricity_consumption_2000to2015_by_sector_gwh[2:4]/1000, type = 'l', lty = 1, xlim = c(2000,2060), ylim = c(min(aa),max(aa))/1000, xlab = 'year', ylab= 'Electricity Demand [ TWh ]', col = c('black','green','red'), main = CNT )
	matlines(as.numeric(names(national_residential_gwh.df)),t(national_residential_gwh.df/1000), col = 'green',lty = c(2,3,4,5) )
	matlines(as.numeric(names(national_commercial_gwh.df)),t(national_commercial_gwh.df/1000),col='red',lty = c(2,3,4,5) )
	matlines(as.numeric(names(national_industry_gwh.df)),t(national_industry_gwh.df/1000), col='black',lty = c(2,3,4,5) )
	abline(v=2015,lty=3)
	legend('top',c('Residential','Commercial','Industry'), lty=1,col=c('green','red','black'),bty='n',cex=0.9)
			
	# Downscale to the catchment scale using intensities and spatial GDP projections
	national_residential_kwh_per_usd.df = 1e6* national_residential_gwh.df / national_gdp.df
	national_commercial_kwh_per_usd.df = 1e6* national_commercial_gwh.df / national_gdp.df
	national_industry_kwh_per_usd.df =  1e6* national_industry_gwh.df / national_gdp.df
	
	# check intensities  
	windows()
	aa = c( unlist(national_residential_kwh_per_usd.df),
			unlist(national_commercial_kwh_per_usd.df),
			unlist(national_industry_kwh_per_usd.df), 
			unlist(data.frame( 1e6* iea_electricity_consumption_2000to2015_by_sector_gwh[,2:4]/cbind(iea_wbi_population_gdp_2000to2015$gdp,iea_wbi_population_gdp_2000to2015$gdp,iea_wbi_population_gdp_2000to2015$gdp) )) 
			)
	matplot(iea_electricity_consumption_2000to2015_by_sector_gwh[,1],data.frame( 1e6* iea_electricity_consumption_2000to2015_by_sector_gwh[,2:4]/cbind(iea_wbi_population_gdp_2000to2015$gdp,iea_wbi_population_gdp_2000to2015$gdp,iea_wbi_population_gdp_2000to2015$gdp) ), main = CNT, type = 'l', lty = 1, xlim = c(2000,2060), ylim = c(min(aa),max(aa)), xlab = 'year', ylab= 'Electricity Intensity [ kWh / USD ]', col = c('black','green','red') )
	matlines(as.numeric(names(national_residential_kwh_per_usd.df)),t(national_residential_kwh_per_usd.df), col = 'green',lty = c(2,3,4,5) )
	matlines(as.numeric(names(national_commercial_kwh_per_usd.df)),t(national_commercial_kwh_per_usd.df),col='red',lty = c(2,3,4,5) )
	matlines(as.numeric(names(national_industry_kwh_per_usd.df)),t(national_industry_kwh_per_usd.df), col='black',lty = c(2,3,4,5) )
	abline(v=2015,lty=3)
	legend('top',c('Residential','Commercial','Industry'), lty=1,col=c('green','red','black'),bty='n',cex=0.9)
	temp = list( national_residential_kwh_per_usd.df, national_commercial_kwh_per_usd.df, national_industry_kwh_per_usd.df )
	names(temp) = c( 'national_residential_kwh_per_usd.df', 'national_commercial_kwh_per_usd.df', 'national_industry_kwh_per_usd.df' )
	
	return( temp )
	
	} )
	
# Assume AFG and CHN follow pakistan
national.list = list( national.list[[1]], national.list[[2]], national.list[[1]], national.list[[1]] ) 	
names(national.list) = c('PAK','IND','AFG','CHN')	
	
### Electricity demands from gridded indicators - using harmonized spatial datasets from Parkinson et al. 2016 A spatially explicit ...	
# Simulation horizon
yy = c(2015,seq(2020,2060,by=10))

# import national manufacturing demand projections 
national_manufacturing_withdrawal.df = data.frame(read.csv("input/manufacturing_water_demand_results/national/IIASA_water_withdrawal_manufacturing_Static.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T))
national_manufacturing_return.df = data.frame(read.csv("input/manufacturing_water_demand_results/national/IIASA_water_return_manufacturing_Static.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T))

# National GDP projections for scaling the manufacturing demands
national_gdp.df = data.frame(read.csv("input/OECD_SSP_GDP_PPP.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) %>% # in billions
	filter( Region %in% unique( unlist( strsplit( basin.spdf@data$PID, '_' ) )[ seq( 1, 2*length(basin.spdf@data$PID), by=2 ) ] ) ) %>%
	dplyr::select( Scenario, Region, paste('X',seq(2015,2060,by=5),sep='') )

# hours in each month
hr = c(31,28.25,31,30,31,30,31,31,30,31,30,31) * 24 

# go through each ssp and year and create flat dataframe containing the demand parameters
demands.df = bind_rows( lapply( c(1,2,5), function(ss){ 
	
	bind_rows( lapply( 1:length(yy), function(y){
		
		# haven't generated the data for each SSP - this is the mapping to the RCPs - could be updated to reflect different RCPs
		if(ss %in% c( 1,3,4 )){rr=1}
		if(ss==2){ rr=2 }
		if(ss==5){ rr=4 }
		
		 # 2015 not included in the data so using 2010
		if( yy[y]==2015 ){ yy2=2010 }else{ yy2=yy[y] }
		
		# import the harmonzied gridded data and rename to make generic headings, add PID by overlaying the polygons, and then sum to PID-level
		dat.df = data.frame( readRDS( paste('input/harmonized_rcp_ssp_data/water_use_ssp',ss,'_rcp',rr,'_',yy2,'_data.Rda',sep='') ) )
		cols = c( 'xloc', 'yloc', 'urban_pop', 'rural_pop', 'urban_gdp', 'rural_gdp', 'mean_tas', 'urban_withdrawal', 'rural_withdrawal', 'urban_return', 'rural_return' )
		for( nm in cols ){ 
			ind = which( grepl( nm, names( dat.df ) ) )
			if( length( ind ) > 1 ){ nm2 = paste( nm, 1:12,sep='.' )}else{ nm2 = nm }
			names(dat.df)[ ind ] = nm2 }
		dat.df = dat.df[,grepl( paste(cols,collapse='|'), names(dat.df) ) ]
		dat.df = dat.df %>%
			'coordinates<-'(~xloc+yloc) %>%
			'gridded<-'(TRUE) %>%
			'proj4string<-'( proj4string(basin.spdf) )
		dat.df$PID = unlist( over( dat.df, basin.spdf[,which(names(basin.spdf) == 'PID')] ) )
		dat.df = data.frame( dat.df ) %>% filter( !is.na( PID ) ) %>% dplyr::select( -xloc, -yloc ) 
		
		elec.df = bind_rows( lapply( 1:12, function( mm ){
			left_join( 	dat.df %>% dplyr::select( PID, paste( 'mean_tas', mm, sep = '.' ) ),
						dat.df %>% dplyr::select( PID, urban_pop, rural_pop )  ) %>%
			rename(tas = paste( 'mean_tas', mm, sep = '.' ) ) %>%
			group_by( PID ) %>%
			summarise( 	turb = abs( -18 -273 + sum( tas * urban_pop ) / sum( urban_pop ) ), 	# population weighted temperature
						trur = abs( -18 -273 + sum( tas * rural_pop ) / sum( rural_pop ) ) ) %>%
			as.data.frame( ) %>% 
			mutate( turb = ifelse( is.nan( turb ), 0, turb ), trur = ifelse( is.nan( trur ), 0, trur ) ) %>%
			mutate( time = mm ) %>%
			dplyr::select( PID, time, turb, trur ) } ) ) %>%
			left_join( ., data.frame( time = 1:12, hr = hr ) ) %>% # add the hours in each month
			group_by( PID ) %>%
			mutate( turb =  0.4 * turb / sum( turb ) + 0.6 * hr / 8760, # downscaling factor assuming part of load insensitive to temperature
					trur =  0.4 * trur / sum( trur ) + 0.6 * hr / 8760 ) %>%	
			as.data.frame( ) %>%
			mutate( turb = ifelse( is.nan( turb ), 0, turb ), trur = ifelse( is.nan( trur ), 0, trur ) ) %>%		
			left_join( 	., dat.df %>% # adding gdp and pop for projecting demands based on intensities above
			dplyr::select( PID, urban_gdp, urban_pop, rural_gdp, rural_pop ) %>%
			group_by( PID ) %>% summarise_each( funs( sum ) ) %>% # summarise by PID
			as.data.frame( ) %>%
			mutate( urban_inc = urban_gdp / urban_pop, rural_inc = rural_gdp / rural_pop )	%>% # don't actually use the per capita income buts it's there in case used as alternative indicator
			mutate( urban_inc = ifelse( is.nan( urban_inc ), 0, round( urban_inc ) ), 
					rural_inc = ifelse( is.nan( rural_inc ), 0, round( rural_inc ) ) ) ) %>%
			left_join( ., data.frame( PID =  basin.spdf@data$PID, country = unlist( strsplit( basin.spdf@data$PID, '_' ) )[ seq( 1, 2*length(basin.spdf@data$PID), by=2 ) ] ) ) %>%	# add countries	
			left_join( . ,	as.data.frame( national.list )[ paste0( 'SSP', ss ),  ] %>% 
								gather( parameter, value ) %>% # flatten
								mutate( country = unlist( strsplit( parameter, '[.]' ) )[ seq( 1, 4*length( parameter ), by = 4 ) ],
										var = unlist( strsplit( parameter, '[.]' ) )[ seq( 2, 4*length( parameter ), by = 4 ) ],
										year = unlist( strsplit( parameter, '[.]' ) )[ seq( 4, 4*length( parameter ), by = 4 ) ] ) %>%
								filter( year == yy[y] ) %>% dplyr::select( country, var, value ) %>%
								spread( var, value ) ) %>%
			group_by( PID, time ) %>% # now project the demands using the gdp - might also instead use the per capita demand model
			summarise( 	urban_municipal_mw = round( 1e-6 * urban_gdp * ( national_residential_kwh_per_usd + national_commercial_kwh_per_usd ) * turb / hr / 1e-3 ),
						rural_municipal_mw = round( 1e-6 * rural_gdp * ( national_residential_kwh_per_usd + national_commercial_kwh_per_usd ) * trur / hr / 1e-3 ),
						industry_mw = round( 1e-6 * ( urban_gdp + rural_gdp ) * national_industry_kwh_per_usd / 8760 / 1e-3 ) ) %>%
			as.data.frame( ) %>% mutate( year = yy[y], scenario = paste0( 'SSP', ss ) ) %>%
			dplyr::select( scenario, PID, year, time, urban_municipal_mw, rural_municipal_mw, industry_mw )	%>%
			gather( type, value, urban_municipal_mw, rural_municipal_mw, industry_mw ) %>% 
			mutate( units = 'MW',  
					type = ifelse( grepl( 'urban', type ), 'urban_final', ifelse( grepl( 'rural', type ), 'rural_final', 'industry_final' ) ) ) %>%
			dplyr::select( scenario, PID, type, year, time, value )		
			
		# format for water demands
		dat.df = dat.df[ , ! grepl('mean_tas',names( dat.df )) ] # remove temperature
		dat.df = dat.df %>% group_by( PID ) %>% 
			summarise_each( funs( sum ) ) %>%
			as.data.frame( ) %>%
			mutate( urban_inc = urban_gdp / urban_pop, rural_inc = rural_gdp / rural_pop )	%>%
			mutate( urban_inc = ifelse( is.nan( urban_inc ), 0, round( urban_inc ) ), rural_inc = ifelse( is.nan( rural_inc ), 0, round( rural_inc ) ) )
						
		# Add manufactruring demands
		mf.df = rbind( 
				national_manufacturing_withdrawal.df %>% 
					filter( Country_Code %in% unique( unlist( strsplit( basin.spdf@data$PID, '_' ) )[ seq( 1, 2*length(basin.spdf@data$PID), by=2 ) ] ) ) %>%
					filter( Scenario == paste0( 'SSP', ss ) ) %>%
					rename( scenario = Scenario, country = Country_Code ) %>%
					mutate( type = 'withdrawal' ) %>%
					dplyr::select( scenario, country, type, paste0( 'X', yy2  ) ),
				national_manufacturing_return.df %>% 
					filter( Country_Code %in% unique( unlist( strsplit( basin.spdf@data$PID, '_' ) )[ seq( 1, 2*length(basin.spdf@data$PID), by=2 ) ] ) ) %>%
					filter( Scenario == paste0( 'SSP', ss ) ) %>%
					rename( scenario = Scenario, country = Country_Code ) %>%
					mutate( type = 'return' ) %>%
					dplyr::select( scenario, country, type, paste0( 'X', yy2  ) ) ) 
		names( mf.df )[ ncol( mf.df ) ] = 'value'
		mf.df$value = round( mf.df$value / 365.25, digits = 2 ) # convert to mcm_per_day
		mf.df = mf.df %>% spread( type, value )
		
		# national gdp for scaling mf demands
		gdp.df = national_gdp.df %>% 
			rename( scenario = Scenario, country = Region ) %>% 
			mutate( scenario = unlist( strsplit( scenario, '_' ) )[seq(1,3*length(scenario),by=3)] ) %>%
			dplyr::select( scenario, country, paste0( 'X', yy[y]  ) )
		names( gdp.df )[ ncol( gdp.df ) ] = 'gdp_national'	
		
		# now scale
		dat.df = dat.df %>% 
			mutate( country = unlist( strsplit( PID, '_' ) )[seq(1,2*length(PID),by=2)] ) %>%
			left_join( ., left_join( mf.df, gdp.df ), by = c( 'country' ) ) %>%
			mutate( manufacturing_withdrawal = round( withdrawal * ( urban_gdp + rural_gdp ) / ( 1e9 * gdp_national ), digits = 3 ) )  %>%
			mutate( manufacturing_return = round( return * ( urban_gdp + rural_gdp ) / ( 1e9 * gdp_national ), digits = 3 ) ) %>%
			dplyr::select(  -urban_pop, -urban_gdp, -rural_pop, -rural_gdp, -urban_inc, -rural_inc, -country, -scenario, -return, -withdrawal, -gdp_national )
		
		
		# expand manufacturing withdrawals and retunr flows to months - assuming constant instensity across the year
		mfw = bind_cols( lapply( 1:12, function( mmm ){ dat.df$manufacturing_withdrawal } ) )	%>% as.data.frame() %>% 'names<-'(paste('manufacturing_withdrawal',1:12,sep='.'))
		mfr = bind_cols( lapply( 1:12, function( mmm ){ dat.df$manufacturing_return } ) )	%>% as.data.frame() %>% 'names<-'(paste('manufacturing_return',1:12,sep='.'))
		dat.df = dat.df %>% 
			dplyr::select( -manufacturing_withdrawal ) %>% cbind( ., mfw ) %>%
			dplyr::select( -manufacturing_return ) %>% cbind( ., mfr )
		
		# flatten the df
		dat.df = bind_rows( lapply( c( 'urban_withdrawal', 'urban_return', 'rural_withdrawal', 'rural_return', 'manufacturing_withdrawal', 'manufacturing_return'  ), function( tp ){
			dat.df %>% 
				dplyr::select( PID, paste( tp, 1:12, sep = '.' ) ) %>%  
				gather( 'month','value',-PID) %>% 
				mutate( time = unlist( strsplit( month, '[.]' ) )[ seq(2,2*length(month),by=2) ], type = tp, scenario = paste0( 'SSP', ss ), year = yy[y], value = round( value, digits = 3 ) ) %>%
				dplyr::select( scenario, PID, type, year, time, value ) 
				} ) ) 
		
		# But the urban and rural withdrawal / return flow data are in million cubic meters per month not per day
		type2check = c( 'urban_withdrawal', 'urban_return', 'rural_withdrawal', 'rural_return' )
		dat.df = rbind( dat.df %>% filter( ! type %in% type2check ),
						dat.df %>% filter( type %in% type2check ) %>% 
							left_join( ., data.frame( time = as.character( 1:12 ), days = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
							mutate( value = round( value / days, digits = 3 ) ) %>% dplyr::select( -days ) )
		
		# add units
		dat.df$units = 'mcm_per_day'
		elec.df$units = 'MW'
		
		
		dat.df = rbind( dat.df, elec.df )
		
		return( dat.df )
		
		} ) )
		
	} ) )

# Harmonize with naming convention used previously - might update later
demands.df = demands.df %>% 
	mutate( sector = unlist( strsplit( type, '_') )[seq(1,2*length(type),by=2)] ) %>%
	mutate( sector = ifelse( sector == 'manufacturing', 'industry', sector ) ) %>%
	mutate( type = unlist( strsplit( type, '_') )[seq(2,2*length(type),by=2)] ) %>%
	mutate( type = ifelse( type == 'final', 'electricity', type ) ) %>%
	dplyr::rename( pid = PID, month = time ) %>%
	dplyr::select( scenario, sector,  type,  pid,  year,  month, value, units )
	
write.csv( demands.df, "input/indus_demands_new.csv", row.names=FALSE )
	
demand.df = read.csv( 'input/indus_demands_new.csv', stringsAsFactors=FALSE )

# Plot the results

windows()
p1 = layout( matrix( c(5,5,1,2,3,4),3,2,byrow=TRUE ), widths = c(0.3,0.3), heights= c(0.05,0.3,0.3) )
hh = 1
for( tp in c('withdrawal','electricity') )
	{
	for(s in c('urban','rural'))
		{
		
		tmp = data.frame( do.call( cbind, lapply( unique(demand.df$scenario), function(pp){
			res = do.call( rbind, lapply( unique(demand.df$year), function(y){ 
				res2 = sum( sapply( unique(demand.df$month), function(m){ return( demand.df[ which( demand.df$sector == s & demand.df$type == tp & demand.df$year == y & demand.df$month == m & demand.df$scenario == pp ), 'value' ] ) } ) )
				return(res2)
				} ) )
			return(res)
			} ) ) )
		names(tmp) = unique(demand.df$scenario)		
		row.names(tmp) = unique(demand.df$year)	
		
		if( tp == 'withdrawal'){ yl = c('million m3 per day'); fc = 1 }
		if( tp == 'electricity'){ yl = c('Gigawatts'); fc = 1e3 }
		
		matplot( as.numeric( row.names(tmp) ), tmp/fc, type = 'l', col = c('green','blue','orange'), xlab = 'year', lty = 1, main = paste( s, tp, sep = ' - ' ), ylab = yl )
			
		}
	
	}
	
par(mar=c(0,0,0,0))	
plot.new()
legend('bottom',legend=c('SSP1','SSP2','SSP5'),col = c('green','blue','orange'),lty=1,bty='n',ncol=3)		
