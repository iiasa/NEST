
require(dplyr)
require(ncdf4)
require(rgdal)
require(raster)
require(rgeos)
require(ggmap)
require(rasterVis)
require(countrycode)
memory.limit(size=1e9)

ssps= c('SSP1','SSP2','SSP3','SSP4','SSP5')
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
buff.sp = gBuffer( basin.sp, width=0.1 ) 
buff2.sp = gBuffer( basin.sp, width=10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)
		
## Electricity demands

national.list = lapply( c('PAK','IND'), function(CNT){
	
	# Import Pakistan historical data
	iea_wbi_population_gdp_2000to2015 = data.frame(read.csv(paste("input/",CNT,"_iea_wbi_population_gdp_2000to2015.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=F, as.is=T))
	iea_electricity_consumption_2000to2015_by_sector_gwh = data.frame(read.csv(paste("input/",CNT,"_iea_electricity_consumption_2000to2015_by_sector_gwh.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=F, as.is=T))

	# National SSP data
	national_population.df = data.frame(read.csv("input/OECD_SSP_POP.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) # in millions
	national_population.df = 1e6 * national_population.df[ national_population.df$Region == CNT, c(paste('X',seq(2015,2060,by=5),sep='')) ]
	national_population.df = data.frame( do.call( rbind, lapply( 1:nrow(national_population.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_population.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_population.df) = as.character(seq(2015,2060,by=1))
	row.names(national_population.df) = ssps
	national_population.df[2:5,'2015'] = national_population.df[1,'2015']
	national_population.df[2:5,'2016'] = national_population.df[1,'2016']
	national_population.df[2:5,'2017'] = national_population.df[1,'2017']
	national_gdp.df = data.frame(read.csv("input/OECD_SSP_GDP_PPP.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) # in billions
	national_gdp.df = 1e9 * national_gdp.df[ national_gdp.df$Region == CNT, c(paste('X',seq(2015,2060,by=5),sep='')) ]
	national_gdp.df = data.frame( do.call( rbind, lapply( 1:nrow(national_gdp.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_gdp.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_gdp.df) = as.character(seq(2015,2060,by=1))
	row.names(national_gdp.df) = ssps
	national_gdp.df[2:5,'2015'] = national_gdp.df[1,'2015']
	national_gdp.df[2:5,'2016'] = national_gdp.df[1,'2016']
	national_gdp.df[2:5,'2017'] = national_gdp.df[1,'2017']
	national_urbanization.df = data.frame(read.csv("input/NCAR_SSP_Urban_Population_Share.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T)) # in percent
	national_urbanization.df = 0.01 * national_urbanization.df[ national_urbanization.df$Region == CNT, c(paste('X',seq(2015,2060,by=5),sep='')) ]
	national_urbanization.df = data.frame( do.call( rbind, lapply( 1:nrow(national_urbanization.df), function(j){ unlist(spline( seq(2015,2060,by=5), c(national_urbanization.df[j,]) , n =2060-2015+1, method = 'fmm' )$y ) } ) ) )
	names(national_urbanization.df) = as.character(seq(2015,2060,by=1))
	row.names(national_urbanization.df) = ssps
	national_urbanization.df[2:5,'2015'] = national_urbanization.df[1,'2015']
	national_urbanization.df[2:5,'2016'] = national_urbanization.df[1,'2016']
	national_urbanization.df[2:5,'2017'] = national_urbanization.df[1,'2017']
	national_income.df = national_gdp.df / national_population.df

	# Calibrate historical data to match scale of ssps 
	iea_wbi_population_gdp_2000to2015$pop = iea_wbi_population_gdp_2000to2015$pop * ( national_population.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$pop[nrow(iea_wbi_population_gdp_2000to2015)] )
	iea_wbi_population_gdp_2000to2015$gdp = iea_wbi_population_gdp_2000to2015$gdp * ( national_gdp.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$gdp[nrow(iea_wbi_population_gdp_2000to2015)] )
	iea_wbi_population_gdp_2000to2015$inc = iea_wbi_population_gdp_2000to2015$inc * ( national_income.df[1,'2015'] / iea_wbi_population_gdp_2000to2015$inc[nrow(iea_wbi_population_gdp_2000to2015)] )

	## Estimate models

	# Residential
	x = iea_wbi_population_gdp_2000to2015$inc[10:16]
	y = 1e6 * iea_electricity_consumption_2000to2015_by_sector_gwh$Residential[10:16] / iea_wbi_population_gdp_2000to2015$pop[10:16]
	r = lm( log(y) ~ log(x) )
	res.a = unlist( coef(r)[1] )
	res.b = unlist( coef(r)[2] )
	yi = exp( res.a + res.b * log(x) )
	windows()
	plot(x,y,main=paste(CNT,' - Residential',sep=''),xlab='per capita income [USD]',ylab='per capita consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))

	# Commercial
	x = iea_wbi_population_gdp_2000to2015$inc[10:16]
	y = 1e6 * iea_electricity_consumption_2000to2015_by_sector_gwh$Commercial[10:16] / iea_wbi_population_gdp_2000to2015$pop[10:16]
	r = lm( log(y) ~ log(x) )
	com.a = unlist( coef(r)[1] )
	com.b = unlist( coef(r)[2] )
	yi = exp( com.a + com.b * log(x) )
	windows()
	plot(x,y,main=paste(CNT,' - Commercial',sep=''),xlab='per capita income [USD]',ylab='per capita consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))

	# Industry
	x = iea_wbi_population_gdp_2000to2015$gdp[10:16]
	y = iea_electricity_consumption_2000to2015_by_sector_gwh$Industry[10:16]
	r = lm( log(y) ~ log(x) )
	ind.a = unlist( coef(r)[1] )
	ind.b = unlist( coef(r)[2] )
	yi = exp(ind.a + ind.b * log(x))
	windows()
	plot(x,y,main=paste(CNT,' - Industrial',sep=''),xlab='GDP [USD]',ylab='consumption [kWh]',pch=21)
	points(x,yi,pch=18)
	legend('bottomright',legend = c('data','model'),pch =c(21,18))

	## Future demands

	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Residential[16]
	mod = national_population.df * ( exp( res.a + res.b * log( national_income.df ) ) ) / 1e6
	lambda = log(0.1) / length(seq(2015,2060,by=1))
	eff = data.frame( do.call( rbind, lapply( c(-1.5,-2.5,0,-1,-3)/100, function(iii){ ( 1 - iii )^( 1:length(seq(2015,2060,by=1)) ) } ) ) )
	tt = seq(1,length(seq(2015,2060,by=1)))
	national_residential_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff

	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Commercial[16]
	mod = national_population.df * ( exp( com.a + com.b * log( national_income.df ) ) ) / 1e6
	national_commercial_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff
			
	dat	= iea_electricity_consumption_2000to2015_by_sector_gwh$Industry[16]
	mod = exp( ind.a + ind.b * log( national_gdp.df ) )
	national_industry_gwh.df = mod * data.frame( do.call(rbind,lapply(1:5,function(hhh){ return( 1 + ( ( dat / mod[1,1] - 1 ) * exp( lambda * (tt-1) ) ) ) } ) ) ) * eff

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
names(national.list) = c('PAK','IND')
	
# Simulation horizon
yy = c(2015,seq(2020,2060,by=10))
res.list = lapply( c(1,2,5), function(ss){ 
	res2 = bind_cols( lapply( 1:length(yy), function(y){
		if(ss==1){rr=1}
		if(ss==2){rr=2}
		if(ss==5){rr=4}
		if(yy[y]==2015){yy2=2010}else{yy2=yy[y]}
		dat.df = data.frame( readRDS( paste('input/harmonized_rcp_ssp_data/water_use_ssp',ss,'_rcp',rr,'_',yy2,'_data.Rda',sep='') ) )
		names(dat.df)[which(names(dat.df) == paste('xloc',yy2,sep='.'))] = 'xloc'
		names(dat.df)[which(names(dat.df) == paste('yloc',yy2,sep='.'))] = 'yloc'
		names(dat.df)[which(names(dat.df) == paste('urban_gdp',yy2,sep='.'))] = 'urban_gdp'
		names(dat.df)[which(names(dat.df) == paste('rural_gdp',yy2,sep='.'))] = 'rural_gdp'
		names(dat.df)[which(names(dat.df) == paste('urban_pop',yy2,sep='.'))] = 'urban_pop'
		names(dat.df)[which(names(dat.df) == paste('rural_pop',yy2,sep='.'))] = 'rural_pop'
		coordinates(dat.df) = ~ xloc + yloc
		gridded(dat.df) = TRUE
		dat.spdf = dat.df
		proj4string(dat.df) = proj4string(basin.spdf)
		dat.df = cbind(data.frame(dat.df), over(dat.df,basin.spdf[,which(names(basin.spdf) == 'PID')]))
		dat.df = dat.df[-1*which(is.na(dat.df$PID)),]
		hr = n_days * 24
		utps = do.call( rbind, lapply( 1:length(basin.spdf), function(x){
			alpha = 0.4
			tmp = as.matrix( dat.df[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ), which(grepl('mean_tas',names(dat.df))) ] )
			pop = unlist( dat.df[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ), 'urban_pop' ] )
			tt = sapply( 1:12, function(mm){return( abs( -18 - 273 + sum( tmp[,mm] * pop, na.rm=TRUE ) / sum( pop, na.rm=TRUE ) ) )})
			tt = tt / sum(tt,na.rm=TRUE)
			tt[is.nan(tt)]=0
			tt = alpha * tt + (1-alpha)*hr/8760
			return(tt)
			} ) )	
		rtps = do.call( rbind, lapply( 1:length(basin.spdf), function(x){
			alpha = 0.4
			tmp = as.matrix( dat.df[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ), which(grepl('mean_tas',names(dat.df))) ] )
			pop = unlist( dat.df[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ), 'rural_pop' ] )
			tt = sapply( 1:12, function(mm){return( abs( -18 - 273 + sum( tmp[,mm] * pop, na.rm=TRUE ) / sum( pop, na.rm=TRUE ) ) )})
			tt = tt / sum(tt,na.rm=TRUE)
			tt[is.nan(tt)]=0
			tt = alpha * tt + (1-alpha)*hr/8760
			return(tt)
			} ) )	
		res = data.frame(	unlist( lapply( 1:length(basin.spdf), function(x){ sum( dat.df$urban_gdp[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ) ], na.rm=TRUE ) } ) ),
							unlist( lapply( 1:length(basin.spdf), function(x){ sum( dat.df$rural_gdp[ which( as.character(dat.df$PID) == as.character(basin.spdf@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		names(res) = c( paste('urban_gdp',yy[y],sep='.'), paste('rural_gdp',yy[y],sep='.') )
		modl = sapply( basin.spdf@data$CNTRY_ID, function(x){ if( x == 'IND' ){return('IND')}else{return('PAK')} } )
		res$urban_municipal_gwh = sapply( 1:length(basin.spdf), function(x){ round( 1e-6 * ( res$urban_gdp[x] ) * ( national.list[[modl[x]]]$national_residential_kwh_per_usd.df + national.list[[modl[x]]]$national_commercial_kwh_per_usd.df )[ paste( 'SSP',ss,sep='' ), as.character( yy[y] ) ] ) } )
		res$rural_municipal_gwh = sapply( 1:length(basin.spdf), function(x){ round( 1e-6 * ( res$rural_gdp[x] ) * ( national.list[[modl[x]]]$national_residential_kwh_per_usd.df + national.list[[modl[x]]]$national_commercial_kwh_per_usd.df )[ paste( 'SSP',ss,sep='' ), as.character( yy[y] ) ] ) } )
		res$industry_gwh = sapply( 1:length(basin.spdf), function(x){ round( 1e-6 * ( res$urban_gdp[x] ) * ( sum( res$urban_gdp + res$rural_gdp ) / sum( res$urban_gdp ) ) * national.list[[modl[x]]]$national_industry_kwh_per_usd.df[ paste( 'SSP',ss,sep='' ), as.character( yy[y] ) ] ) } )
		res = res[,c('urban_municipal_gwh','rural_municipal_gwh','industry_gwh')]
		res = round( data.frame( cbind( do.call( cbind, lapply( 1:12, function(mm){ return( res$urban_municipal_gwh * utps[,mm] / hr[ mm ] / 1e-3 ) } ) ),
					 do.call( cbind, lapply( 1:12, function(mm){ return( res$rural_municipal_gwh * rtps[,mm] / hr[ mm ] / 1e-3 ) } ) ),
					 do.call( cbind, lapply( 1:12, function(mm){ return( res$industry_gwh / 8760 / 1e-3 ) } ) ) ) ), digits = 1 )
		names(res) = c( paste(paste('urban_municipal_mw',yy[y],sep='.'),seq(1,12),sep='.'), paste(paste('rural_municipal_mw',yy[y],sep='.'),seq(1,12),sep='.'), paste(paste('industry_mw',yy[y],sep='.'),seq(1,12),sep='.') )					
		return( res )
		} ) ) 
	row.names(res2) = basin.spdf@data$PID
	return(res2)	
	} )		
names(res.list) = c('SSP1','SSP2','SSP5')
elec.list = res.list
rm(res.list)
	
## Water Demands	
national_manufacturing_withdrawal.df = data.frame(read.csv("input/manufacturing_water_demand_results/national/IIASA_water_withdrawal_manufacturing_Static.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T))
national_manufacturing_return.df = data.frame(read.csv("input/manufacturing_water_demand_results/national/IIASA_water_return_manufacturing_Static.csv", header=TRUE, sep=",", stringsAsFactors=F, as.is=T))
res.list = lapply( c(1,2,5), function(ss){ 
	res2 = bind_cols( lapply( 1:length(yy), function(y){
		bsn.df=basin.spdf
		if(ss==1){rr=1}
		if(ss==2){rr=2}
		if(ss==5){rr=4}
		if(yy[y]==2015){yy2=2010}else{yy2=yy[y]}
		dat.df = data.frame( readRDS( paste('input/harmonized_rcp_ssp_data/water_use_ssp',ss,'_rcp',rr,'_',yy2,'_data.Rda',sep='') ) )
		names(dat.df)[which(names(dat.df) == paste('xloc',yy2,sep='.'))] = 'xloc'
		names(dat.df)[which(names(dat.df) == paste('yloc',yy2,sep='.'))] = 'yloc'
		names(dat.df)[which(names(dat.df) == paste('urban_pop',yy2,sep='.'))] = 'urban_pop'
		names(dat.df)[which(names(dat.df) == paste('rural_pop',yy2,sep='.'))] = 'rural_pop'
		names(dat.df)[which(names(dat.df) == paste('urban_gdp',yy2,sep='.'))] = 'urban_gdp'
		names(dat.df)[which(names(dat.df) == paste('rural_gdp',yy2,sep='.'))] = 'rural_gdp'
		coordinates(dat.df) = ~ xloc + yloc
		gridded(dat.df) = TRUE
		dat.spdf = dat.df
		proj4string(dat.df) = proj4string(bsn.df)
		dat.df = cbind(data.frame(dat.df), over(dat.df,bsn.df[,which(names(bsn.df) == 'PID')]))
		dat.df = dat.df[-1*which(is.na(dat.df$PID)),]
		assign(paste('URBAN_POP',yy[y],sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$urban_pop[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('RURAL_POP',yy[y],sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$rural_pop[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('URBAN_GDP',yy[y],sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$urban_gdp[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		assign(paste('RURAL_GDP',yy[y],sep='.'), unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df$rural_gdp[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ) ], na.rm=TRUE ) } ) ) )
		URBAN_INC = get(paste('URBAN_GDP',yy[y],sep='.')) / get(paste('URBAN_POP',yy[y],sep='.')); URBAN_INC[is.na(URBAN_INC)] = 0
		RURAL_INC = get(paste('RURAL_GDP',yy[y],sep='.')) / get(paste('RURAL_POP',yy[y],sep='.')); RURAL_INC[is.na(RURAL_INC)] = 0
		
		# Withdrawal and return flow
		URBAN_WITHDRAWAL = do.call(cbind, lapply( 1:12, function(m){ 1e6* (1/n_days[m]) * data.frame( unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ), which(as.character(names(dat.df)) == paste('urban_withdrawal',yy2,m,sep='.') ) ], na.rm=TRUE ) } ) ) ) } ) )
		names(URBAN_WITHDRAWAL) = unlist(lapply(1:12,function(m){ paste('URBAN_WITHDRAWAL_m3_per_day',yy[y],m,sep='.') } ) )
		URBAN_RETURN = do.call(cbind, lapply( 1:12, function(m){ 1e6* (1/n_days[m]) *  data.frame( unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ), which(as.character(names(dat.df)) == paste('urban_return',yy2,m,sep='.') ) ], na.rm=TRUE ) } ) ) ) } ) )
		names(URBAN_RETURN) = unlist(lapply(1:12,function(m){ paste('URBAN_RETURN_m3_per_day',yy[y],m,sep='.') } ) )
		RURAL_WITHDRAWAL = do.call(cbind, lapply( 1:12, function(m){ 1e6* (1/n_days[m]) *  data.frame( unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ), which(as.character(names(dat.df)) == paste('rural_withdrawal',yy2,m,sep='.') ) ], na.rm=TRUE ) } ) ) ) } ) )
		names(RURAL_WITHDRAWAL) = unlist(lapply(1:12,function(m){ paste('RURAL_WITHDRAWAL_m3_per_day',yy[y],m,sep='.') } ) )
		RURAL_RETURN = do.call(cbind, lapply( 1:12, function(m){ 1e6 * (1/n_days[m]) *  data.frame( unlist( lapply( 1:length(bsn.df), function(x){ sum( dat.df[ which( as.character(dat.df$PID) == as.character(bsn.df@data$PID[x]) ), which(as.character(names(dat.df)) == paste('rural_return',yy2,m,sep='.') ) ], na.rm=TRUE ) } ) ) ) } ) )
		names(RURAL_RETURN) = unlist(lapply(1:12,function(m){ paste('RURAL_RETURN_m3_per_day',yy[y],m,sep='.') } ) )
		temp = data.frame( PID = bsn.df@data$PID, get(paste('URBAN_POP',yy[y],sep='.')), get(paste('RURAL_POP',yy[y],sep='.')), get(paste('URBAN_GDP',yy[y],sep='.')), get(paste('RURAL_GDP',yy[y],sep='.')), URBAN_INC, RURAL_INC, URBAN_WITHDRAWAL, URBAN_RETURN, RURAL_WITHDRAWAL, RURAL_RETURN ) 
		names(temp) = c('PID',paste('URBAN_POP',yy[y],sep='.'),paste('RURAL_POP',yy[y],sep='.'),paste('URBAN_GDP',yy[y],sep='.'),paste('RURAL_GDP',yy[y],sep='.'),paste('URBAN_INC',yy[y],sep='.'),paste('RURAL_INC',yy[y],sep='.'), names(temp)[8:length(names(temp))] )
		temp = replace(temp,is.na(temp),0)
		bsn.df = merge(bsn.df, temp, by='PID')
		
		# Manufacturing demands
		MANUFACTURING_WITHDRAWAL = do.call(cbind, lapply( 1:12, function(m){ data.frame( 1e6* (1/365) * unlist(lapply( 1:length(bsn.df), function(z){if(length(as.numeric(national_manufacturing_withdrawal.df[ which( ( as.character(national_manufacturing_withdrawal.df$Country_Code) == as.character(bsn.df@data$CNTRY_ID[z]) ) & (as.character(national_manufacturing_withdrawal.df$Scenario) == 'SSP2') ) , which(as.character(names(national_manufacturing_withdrawal.df)) == paste('X',yy[y],sep='')) ]))>0){ as.numeric(national_manufacturing_withdrawal.df[ which( ( as.character(national_manufacturing_withdrawal.df$Country_Code) == as.character(bsn.df@data$CNTRY_ID[z]) ) & (as.character(national_manufacturing_withdrawal.df$Scenario) == 'SSP2') ) , which(as.character(names(national_manufacturing_withdrawal.df)) == paste('X',yy[y],sep='')) ]) * ( data.frame(bsn.df[ z, which(as.character(names(bsn.df)) == paste('URBAN_GDP',yy[y],sep='.') ) ]) / sum( data.frame(bsn.df[ which(as.character(bsn.df@data$CNTRY_ID) == as.character(bsn.df@data$CNTRY_ID[z]) ), which(as.character(names(bsn.df)) == paste('URBAN_GDP',yy[y],sep='.') ) ]), na.rm=TRUE ) ) }else{0} } ) ) ) } ) )   
		MANUFACTURING_RETURN = do.call(cbind, lapply( 1:12, function(m){ data.frame( 1e6* (1/365) * unlist(lapply( 1:length(bsn.df), function(z){if(length(as.numeric(national_manufacturing_return.df[ which( ( as.character(national_manufacturing_return.df$Country_Code) == as.character(bsn.df@data$CNTRY_ID[z]) ) & (as.character(national_manufacturing_return.df$Scenario) == 'SSP2') ) , which(as.character(names(national_manufacturing_return.df)) == paste('X',yy[y],sep='')) ]))>0){ as.numeric(national_manufacturing_return.df[ which( ( as.character(national_manufacturing_return.df$Country_Code) == as.character(bsn.df@data$CNTRY_ID[z]) ) & (as.character(national_manufacturing_return.df$Scenario) == 'SSP2') ) , which(as.character(names(national_manufacturing_return.df)) == paste('X',yy[y],sep='')) ]) * ( data.frame(bsn.df[ z, which(as.character(names(bsn.df)) == paste('URBAN_GDP',yy[y],sep='.') ) ]) / sum( data.frame(bsn.df[ which(as.character(bsn.df@data$CNTRY_ID) == as.character(bsn.df@data$CNTRY_ID[z]) ), which(as.character(names(bsn.df)) == paste('URBAN_GDP',yy[y],sep='.') ) ]), na.rm=TRUE ) ) }else{0} } ) ) ) } ) )   
		names(MANUFACTURING_WITHDRAWAL) = unlist(lapply(1:12,function(m){ paste('MANUFACTURING_WITHDRAWAL_m3_per_day',yy[y],m,sep='.') } ) ); row.names(MANUFACTURING_WITHDRAWAL) = row.names(bsn.df)
		names(MANUFACTURING_RETURN) = unlist(lapply(1:12,function(m){ paste('MANUFACTURING_RETURN_m3_per_day',yy[y],m,sep='.') } ) ); row.names(MANUFACTURING_RETURN) = row.names(bsn.df)
		temp = data.frame(PID = bsn.df@data$PID, MANUFACTURING_WITHDRAWAL, MANUFACTURING_RETURN)
		temp = replace(temp,is.na(temp),0)
		bsn.df = merge(bsn.df, temp, by='PID')
		
		res.df = round( data.frame( bsn.df[ , which( grepl('WITHDRAWAL|RETURN',names(bsn.df) ) ) ] ) )
	
		return(res.df)
		
		} ) )
		
	row.names(res2) = basin.spdf@data$PID	
	return(res2) 
	} )
names(res.list) = c('SSP1','SSP2','SSP5')
wat.list = res.list
rm(res.list)
	
# Irrigation
nc = nc_open('input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4', verbose=FALSE)
irrigation.stack = stack( 'input/Wada_groundwater_abstraction/pcrglobwb_WFDEI_historical_PIrrWW_monthly_1960_2010.nc4' )
extent(irrigation.stack) = extent( min( ncvar_get(nc, "longitude") ), max( ncvar_get(nc, "longitude") ), min( ncvar_get(nc, "latitude") ), max( ncvar_get(nc, "latitude") ) )
proj4string( irrigation.stack ) = proj4string( basin.spdf )
irrigation.stack = crop( irrigation.stack, extent(buff.sp) )
names(irrigation.stack) = c( sapply( 1:(nlayers(irrigation.stack)/12), function(yy){ return( paste( ( as.numeric( unlist( strsplit( as.character( as.Date("1901-01-01") + min( ncvar_get(nc, "time") ) ), '-' ) )[1] ) + yy - 1 ), seq(1,12,by=1), sep='.') ) } ) ) 
irrigation.stack = irrigation.stack[[ c( which(grepl( 'X2010',names(irrigation.stack) )) ) ]] # keep 2010
nm = names(irrigation.stack)
irrigation.stack = ( 1e6 * irrigation.stack ) / (1e6 * area(irrigation.stack[[1]]) ) # convert to meters
nc_close(nc)
rr = raster()
res(rr) = 1/16
proj4string(rr)=proj4string(basin.sp)
rr = crop(rr, extent(irrigation.stack))
irrigation.stack = resample(irrigation.stack, rr, method="bilinear")
irrigation.stack[irrigation.stack[]<0]=0
names(irrigation.stack) = nm
irrigation.stack = round( 1e6 * irrigation.stack * area( irrigation.stack[[1]] ) / n_days ) # in m3 per day
irrw.df = data.frame( do.call( cbind, lapply(1:12, function(m){ unlist( lapply( 1:length(basin.spdf), function(x){ sum( unlist( extract( irrigation.stack[[m]], as(basin.spdf[x,],'SpatialPolygons'), na.rm=TRUE ) ) ) } ) ) })) )
names(irrw.df) = nm
row.names(irrw.df) = basin.spdf@data$PID
irrigation.list = lapply( c('SSP1','SSP2','SSP5'), function(ss){ 
	if(ss == 'SSP5'){ss='SSP2'}
	glb_scen = paste(ss,'-Ref-SPA0',sep='' )
	glb_irr.df = data.frame( read.csv( 'P:/ene.model/data/Water/water_demands/cdlinks_globiom_irrigation.csv', stringsAsFactors = FALSE ) )
	frc = unlist( lapply( seq(2010,2060,by=10), function(y){ return( glb_irr.df[ which( glb_irr.df$Scenario == glb_scen & glb_irr.df$Region == 'SAS' ), paste( 'X', y, sep = '' ) ] ) } ) )
	frc = frc / frc[1]
	if(ss == 'SSP5'){frc[2:length(frc)] = frc[2:length(frc)] * 1.3 }
	res = do.call( cbind, lapply( frc, function(ff){ return( irrw.df * ff )  } ) )
	names(res) = unlist(lapply( yy, function(y){ paste( 'IRRIGATION_WITHDRAWAL_m3_per_day', y, 1:12, sep='.') } ) )
	return(res)
	} )
names(irrigation.list) = c('SSP1','SSP2','SSP5')

# Flatten	and combine	
params = c('withdrawal','return')
sectors = c('urban','rural','industry')
mps = data.frame( withdrawal = 'WITHDRAWAL_m3_per_day', return = 'RETURN_m3_per_day', urban = 'URBAN', rural = 'RURAL', industry = 'MANUFACTURING' )
yrs = yy
unts = data.frame( withdrawal = 'm3_per_day', return = 'm3_per_day' )
mnths = seq(1,12,by=1)
nms = names( wat.list[[ 1 ]] )
pid = row.names(wat.list[[1]])
demand.df = bind_rows( lapply( ssps[c(1,2,5)], function(ss){
	res1 = bind_rows( lapply( params, function(pp){ 
		res2 = bind_rows( lapply( sectors, function(cc){
			res3 = bind_rows( lapply( yrs, function(yr){
				res4 = bind_rows( lapply( mnths, function(mm){
					res = data.frame( 	scenario = rep(ss,length(pid)),
										sector = rep(cc,length(pid)),
										type = rep(pp,length(pid)),
										pid = pid,
										year = rep(yr,length(pid)),
										month = rep(mm,length(pid)),
										value = round( unlist( wat.list[[ ss ]][ , paste( paste( as.character( unlist( mps[ cc ] ) ), as.character( unlist( mps[ pp ] ) ), sep = '_' ), yr, mm, sep = '.' )  ] ) ),
										units =  rep( 'm3_per_day', length(pid) ) )
					return(res)
					} ) )
				return(res4)			
				} ) )
			return(res3)
			} ) )	
		return(res2)
		} ) )
	return(res1)
	} ) )
		
# Flatten		
params = c('electricity')
sectors = c('urban','rural','industry')
mps = data.frame( electricity = 'mw', urban = 'urban_municipal', rural = 'rural_municipal', industry = 'industry' )
unts = data.frame( electricity = 'MW' )
yrs = yy
mnths = seq(1,12,by=1)
nms = names( wat.list[[ 1 ]] )
pid = row.names(wat.list[[1]])
demand.df = bind_rows( bind_rows( lapply( ssps[c(1,2,5)], function(ss){
	res1 = bind_rows( lapply( params, function(pp){ 
		res2 = bind_rows( lapply( sectors, function(cc){
			res3 = bind_rows( lapply( yrs, function(yr){
				res4 = bind_rows( lapply( mnths, function(mm){
					res = data.frame( 	scenario = rep(ss,length(pid)),
										sector = rep(cc,length(pid)),
										type = rep(pp,length(pid)),
										pid = pid,
										year = rep(yr,length(pid)),
										month = rep(mm,length(pid)),
										value = round( unlist( elec.list[[ ss ]][ , paste( paste( as.character( unlist( mps[ cc ] ) ), as.character( unlist( mps[ pp ] ) ), sep = '_' ), yr, mm, sep = '.' )  ] ), digits=1),
										units = rep( 'MW', length(pid) ) )
					return(res)
					} ) )
				return(res4)			
				} ) )
			return(res3)
			} ) )	
		return(res2)
		} ) )
	return(res1)
	} ) ), demand.df )
				
params = c('withdrawal')
sectors = c('irrigation')
mps = data.frame( withdrawal = 'WITHDRAWAL_m3_per_day', irrigation = 'IRRIGATION' )
unts = data.frame( withdrawal = 'm3_per_day' )
yrs = yy
mnths = seq(1,12,by=1)
nms = names( irrigation.list[[ 1 ]] )
pid = row.names(irrigation.list[[1]])
demand.df = bind_rows( bind_rows( lapply( ssps[c(1,2,5)], function(ss){
	res1 = bind_rows( lapply( params, function(pp){ 
		res2 = bind_rows( lapply( sectors, function(cc){
			res3 = bind_rows( lapply( yrs, function(yr){
				res4 = bind_rows( lapply( mnths, function(mm){
					res = data.frame( 	scenario = rep(ss,length(pid)),
										sector = rep(cc,length(pid)),
										type = rep(pp,length(pid)),
										pid = pid,
										year = rep(yr,length(pid)),
										month = rep(mm,length(pid)),
										value = round( unlist( irrigation.list[[ ss ]][ , paste( paste( as.character( unlist( mps[ cc ] ) ), as.character( unlist( mps[ pp ] ) ), sep = '_' ), yr, mm, sep = '.' )  ] ) ),
										units = rep( 'm3_per_day', length(pid) ) )
					return(res)
					} ) )
				return(res4)			
				} ) )
			return(res3)
			} ) )	
		return(res2)
		} ) )
	return(res1)
	} ) ), demand.df )

write.csv( demand.df, "input/indus_demands.csv", row.names=FALSE )
	
demand.df = read.csv( 'input/indus_demands.csv', stringsAsFactors=FALSE )
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
		
		if( tp == 'withdrawal'){ yl = c('million m3 per day'); fc = 1e6 }
		if( tp == 'electricity'){ yl = c('Gigawatts'); fc = 1e3 }
		
		matplot( as.numeric( row.names(tmp) ), tmp/fc, type = 'l', col = c('green','blue','orange'), xlab = 'year', lty = 1, main = paste( s, tp, sep = ' - ' ), ylab = yl )
			
		}
	
	}	
par(mar=c(0,0,0,0))	
plot.new()
legend('bottom',legend=c('SSP1','SSP2','SSP3'),col = c('green','blue','orange'),lty=1,bty='n',ncol=3)	
	
