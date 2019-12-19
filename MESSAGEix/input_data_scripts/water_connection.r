
require(reshape)	
require(maptools)
require(countrycode)
require(raster)
require(reshape)
require(dplyr)
require(rgeos)
require(rgdal) 
memory.limit(size=1e9)

basin = c('Indus')

setwd('P:/is-wel/indus/message_indus')

# Grab the basin boundaries
basin.spdf = readOGR( paste( getwd(), 'input', sep = '/' ), 'Indus_bcu', verbose = FALSE )
basin.spdf = spTransform( basin.spdf, CRS("+proj=longlat") ) # temp[which(as.character(temp$BASIN) == basin | as.character(temp$PID) == '2256'),] for Karachi basin
basin.sp = gUnaryUnion( basin.spdf )
basin.sp = SpatialPolygons(list(Polygons(Filter(function(f){f@ringDir==1},basin.sp@polygons[[1]]@Polygons),ID=1)))
buff.sp = gBuffer( basin.sp, width=0.1 ) 
buff2.sp = gBuffer( basin.sp, width=10 ) 
proj4string(basin.sp) = proj4string(basin.spdf)
proj4string(buff.sp) = proj4string(basin.spdf)
proj4string(buff2.sp) = proj4string(basin.spdf)
		
# Country region mapping key
country_region_map_key.df = data.frame( read.csv('input/country_region_map_key.csv', stringsAsFactors=FALSE) )

# historical connection rates
ww.df = data.frame(read.csv('input/sewerage_connection_and_treatment.csv', header=TRUE, sep=',', stringsAsFactors=F, as.is=T))
ww.df$country_id = as.character(countrycode(ww.df$country, 'country.name', 'iso3c'))

# Existing urban wastewater treatment
# Income vs connection from Baum et al 2010
income_vs_connection.df = data.frame(level = c('low','middle','upper','high'), max_income = c(1045/2,(4125-1045)/2+1045,(12375-4125)/2+4125,(30000-12375)/2+12375), connection = c(3.6,12.7,53.6,86.8)/100, treatment=c(0.02,2,13.8,78.9)/100) 
y = c(0.0001,income_vs_connection.df$connection,0.95)
x = c(100,income_vs_connection.df$max_income,60000)
r = nls(y ~ SSlogis(x,a,m,s))
cp.a = coef(r)[1]
cp.m = coef(r)[2]
cp.s = coef(r)[3]
y = c(0.0001,income_vs_connection.df$treatment,0.9)
r = nls(y ~ SSlogis(x,a,m,s))
tp.a = coef(r)[1]
tp.m = coef(r)[2]
tp.s = coef(r)[3]

yy = c(2015,seq(2020,2060,by=10))

for( case in c( 'base', 'sdg' ) )
	{
	
	res.list = lapply( c(1,2,5), function(ss){ 
		
		dat.df = reshape::merge_recurse( lapply( 1:length(yy), function(y){
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
			dat.df = data.frame( dat.df  )
			dat.df = cbind( dat.df[ ,c( 'country_id','PID', 'xloc', 'yloc', 'urban_pop', 'rural_pop', 'urban_gdp', 'rural_gdp' ) ] )
			names(dat.df) = c('country_id','PID','xloc','yloc', paste('urban_pop',yy[y],sep='.'), paste('rural_pop',yy[y],sep='.'),paste('urban_gdp',yy[y],sep='.'), paste('rural_gdp',yy[y],sep='.') )					
			return(dat.df)
			} ), by = c('xloc','yloc','country_id','PID') )

		
		# Countries and regions
		dat.df$country = sapply( dat.df$country_id, function(cc){ if( cc %in% country_region_map_key.df$UN_Code ){ return( country_region_map_key.df$Region[ which(country_region_map_key.df$UN_Code == cc) ] ) }else{ return(NA) } } )	
		dat.df$region = sapply( dat.df$country_id, function(cc){ if( cc %in% country_region_map_key.df$UN_Code ){ return( country_region_map_key.df$Eleven_region[ which(country_region_map_key.df$UN_Code == cc) ] ) }else{ return(NA) } } )	
		dat.df = dat.df[which(!is.na(dat.df$region)),]
			
		if( case == 'sdg' ){ add_SDG_constrain = 1 }else{ add_SDG_constrain = 0 }
		
		
		# piped water access / sewerage connection
		national_connection_rate.df = do.call( rbind, lapply( yy , function(yyy){

			ret1 = data.frame( do.call( rbind, lapply(  unique( dat.df$country_id )[ which( unique( dat.df$country_id ) %in% as.numeric( country_region_map_key.df$UN_Code ) ) ], function(cc){
				
				ret2 = do.call( rbind, lapply( c('urban','rural'), function(tt){
					
					inc_o = max( 0, sum( dat.df[ which(  dat.df$country_id == cc ) , c(  paste( tt, '_gdp.', yy[1], sep='' ) ) ], na.rm=TRUE ) / sum( dat.df[ which(  dat.df$country_id == cc ) , c(  paste( tt, '_pop.', yy[1], sep='' ) ) ], na.rm=TRUE ) , na.rm=TRUE )
					inc_f = max( 0, sum( dat.df[ which(  dat.df$country_id == cc ) , c(  paste( tt, '_gdp.', yyy, sep='' ) ) ], na.rm=TRUE ) / sum( dat.df[ which(  dat.df$country_id == cc ) , c(  paste( tt, '_pop.', yyy, sep='' ) ) ], na.rm=TRUE  ) , na.rm=TRUE )
					ind = which( ww.df$country_id == as.character( country_region_map_key.df$Region[ which( as.numeric( country_region_map_key.df$UN_Code ) == cc ) ] ) )
					if( length( ind ) > 0 )
						{
						c0 = ww.df$connected.2010[ ind ] / 100
						t0 = ww.df$treated.2010[ ind ] / 100
						}else
						{
						if(inc_f > 0)
							{
							c0 = c( SSlogis(inc_o,cp.a,cp.m,cp.s) )
							t0 = c( SSlogis(inc_o,tp.a,tp.m,tp.s) )
							}else
							{
							c0 = 0
							t0 = 0
							}	
						}
						
					decay = max( 0, 1 / length(yy) * log( 1 / 0.01 ), na.rm=TRUE )
					if( yyy > yy[1] )
						{ 
						c_mod = c( SSlogis( inc_f, cp.a, cp.m, cp.s ) )
						cp = ( 1 +  ( c0 / c_mod - 1 ) * exp( -1 * decay * ( which(yy == yyy) - 1 ) ) ) * c_mod 
						if( add_SDG_constrain == 1 & yyy >= 2030 & cp < 0.95 ){ cp = 0.95 } 	
						
						t_mod = c( SSlogis( inc_f, tp.a, tp.m, tp.s ) )
						tp = ( 1 +  ( t0 / t_mod - 1 ) * exp( -1 * decay * ( which(yy == yyy) - 1 ) ) ) * t_mod
						if( add_SDG_constrain == 1 & yyy >= 2030 & tp < 0.5 ){ tp = 0.5 }
							
						}else
						{ 
						cp = c0  
						tp = t0
						}
					
					ret3  = data.frame( cp = cp, tp = tp )
					names(ret3) = paste(tt,names(ret3),sep='.')
					
					return( as.matrix(c(cp,tp)) )
					
					} ) )
				
				ret2 = data.frame(t(ret2))
				names(ret2) = c( 'urban.cp', 'urban.tp', 'rural.cp', 'rural.tp' )
				row.names(ret2) = paste(country_region_map_key.df$Region[ which( as.numeric( country_region_map_key.df$UN_Code ) == cc ) ],yyy,sep='.')
				return(ret2)
				
				} ) ) )
				
			return(ret1)	

			} ) )
			
		dat.df = cbind( dat.df, round( do.call(cbind, lapply(yy, function(yyy){  
			ret = data.frame( res = sapply( dat.df$country, function(cc){ national_connection_rate.df[ paste( cc, yyy, sep = '.' ), 'urban.cp' ] } ) ) 
			ret$res[ which( !( unlist( dat.df[ , paste( 'urban_pop', yyy, sep = '.' ) ] ) > 0 ) ) ] = 0  
			names(ret) = paste( 'urban_connection_rate', yyy, sep = '.' )
			return(ret)
			} ) ), digits = 2 ) )
		dat.df = cbind( dat.df, round( do.call(cbind, lapply(yy, function(yyy){  
			ret = data.frame( res = sapply( dat.df$country, function(cc){ national_connection_rate.df[ paste( cc, yyy, sep = '.' ), 'urban.tp' ] } ) ) 
			ret$res[ which( !( unlist( dat.df[ , paste( 'urban_pop', yyy, sep = '.' ) ] ) > 0 ) ) ] = 0  
			names(ret) = paste( 'urban_treated_rate', yyy, sep = '.' )
			return(ret)
			} ) ), digits = 2 ) )
		dat.df = cbind( dat.df, round( do.call(cbind, lapply(yy, function(yyy){  
			ret = data.frame( res = sapply( dat.df$country, function(cc){ national_connection_rate.df[ paste( cc, yyy, sep = '.' ), 'rural.cp' ] } ) ) 
			ret$res[ which( !( unlist( dat.df[ , paste( 'rural_pop', yyy, sep = '.' ) ] ) > 0 ) ) ] = 0  
			names(ret) = paste( 'rural_connection_rate', yyy, sep = '.' )
			return(ret)
			} ) ), digits = 2 ) )
		dat.df = cbind( dat.df, round( do.call(cbind, lapply(yy, function(yyy){  
			ret = data.frame( res = sapply( dat.df$country, function(cc){ national_connection_rate.df[ paste( cc, yyy, sep = '.' ), 'rural.tp' ] } ) ) 
			ret$res[ which( !( unlist( dat.df[ , paste( 'rural_pop', yyy, sep = '.' ) ] ) > 0 ) ) ] = 0  
			names(ret) = paste( 'rural_treated_rate', yyy, sep = '.' )
			return(ret)
			} ) ), digits = 2 ) )
		
		return(dat.df)
		
		} ) 
		
	names(res.list) = c('SSP1','SSP2','SSP5')
	
	# Now estimate average connection level for each PID
	res_pid.df = do.call( rbind, lapply( c('SSP1','SSP2','SSP5'), function( ssp ){
		
		do.call( rbind, lapply( c( unique( unlist( strsplit( names( res.list[[ssp]] )[ grepl( "urban_pop" , names( res.list[[ssp]] ) ) ], '[.]' ) )[seq(2,2*length(names( res.list[[ssp]] )[ grepl( "urban_pop" , names( res.list[[ssp]] ) ) ]),by=2)] ) ), function( yy ){
			
			do.call( rbind, lapply( unique(res.list[[1]]$PID), function(ppp){
			
				upop = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("urban_pop",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				rpop = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("rural_pop",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				ucon = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("urban_connection_rate",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				rcon = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("rural_connection_rate",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				utrt = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("urban_treated_rate",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				rtrt = data.frame( res.list[[ssp]][ which( res.list[[ssp]]$PID == as.character( ppp ) ), names( res.list[[ssp]] )[ grepl( paste("rural_treated_rate",yy,sep='.') , names( res.list[[ssp]] ) ) ] ] )
				
				df = data.frame( 	SSP = ssp,
									PID = ppp,
									year = yy,
									urban_connection_rate = round( max( weighted.mean( unlist( ucon ), unlist( upop ) ), 0, na.rm=TRUE ), digits = 3 ),
									rural_connection_rate = round( max( weighted.mean( unlist( rcon ), unlist( rpop ) ), 0, na.rm=TRUE ), digits = 3 ),
									urban_treated_rate = round( max( weighted.mean( unlist( utrt ), unlist( utrt ) ), 0, na.rm=TRUE ), digits = 3 ),
									rural_treated_rate = round( max( weighted.mean( unlist( rtrt ), unlist( rtrt ) ), 0, na.rm=TRUE ), digits = 3 )	)
				
				return( df )
				
				} ) )
				
			} ) )	
		
		} ) )

	res_pid.df = res_pid.df[ order( res_pid.df$SSP, res_pid.df$PID ) , ]
		
	write.csv( res_pid.df[ order( res_pid.df$SSP, res_pid.df$PID ) , ], paste0( "input/indus_water_connections_", case, ".csv" ), row.names=FALSE )	
	
	# Add historical capacity
	if( case == 'base' )
		{
		
		res_pid.df  = read.csv( paste0( "input/indus_water_connections_", case, ".csv" ), stringsAsFactors = FALSE )
		names(res_pid.df)[ c(1,2,3) ] = c('scenario','node','year_all')
		ucon.df = 	res_pid.df %>% 
					filter( year_all == 2015 ) %>%
					filter( scenario == 'SSP2' ) %>%
					dplyr::select( node, urban_connection_rate )
		utrt.df = 	res_pid.df %>% 
					filter( year_all == 2015 ) %>%
					filter( scenario == 'SSP2' ) %>%
					dplyr::select( node, urban_treated_rate )			
		rcon.df = 	res_pid.df %>% 
					filter( year_all == 2015 ) %>%
					filter( scenario == 'SSP2' ) %>%
					dplyr::select( node, rural_connection_rate )	
		rtrt.df = 	res_pid.df %>% 
					filter( year_all == 2015 ) %>%
					filter( scenario == 'SSP2' ) %>%
					dplyr::select( node, rural_treated_rate )			
	
		# Demands
		demand.df = read.csv( "input/indus_demands.csv", stringsAsFactors=FALSE )
		
		# Just keep the current SSP and add level and commodity to match core GAMS model
		demand.df = demand.df[ which( demand.df$scenario == 'SSP2' ), c( which( names( demand.df ) != 'scenario' ) ) ]
		demand.df$commodity = NA
		demand.df$commodity[ which( demand.df$type %in% c('withdrawal','return') ) ] = 'freshwater'
		demand.df$commodity[ which( demand.df$type %in% c('electricity') ) ] = 'electricity'
		demand.df$level = NA
		demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == c('urban') ) ] = 'urban_final'
		demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == c('industry') ) ] = 'industry_final'
		demand.df$level[ which(  demand.df$type %in% c('withdrawal','electricity') & demand.df$sector == 'rural' )  ] = 'rural_final'
		demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'urban' ) ] = 'urban_waste'
		demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'industry' ) ] = 'industry_waste'
		demand.df$level[ which(  demand.df$type %in% c('return') & demand.df$sector == 'rural' ) ] = 'rural_waste'
		demand.df = demand.df[ , c( 'pid', 'level', 'commodity', 'year', 'month', 'value' ) ]
		names(demand.df) = c('node','level','commodity','year_all','time', 'value0' ) 

		# Multiply the fraction by the historical demands to calibrate historical gw demands
		urban_connected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('urban_final')) %>% 
			left_join(ucon.df,by = c("node")) %>% 
			mutate(value = value0 * urban_connection_rate) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'urban_piped_distribution') %>%   
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )
		  
		urban_unconnected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('urban_final')) %>% 
			left_join(ucon.df,by = c("node")) %>% 
			mutate(value = value0 * (1-urban_connection_rate)) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'urban_unimproved_distribution') %>%   
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )
		  
		urban_collected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('urban_waste')) %>% 
			left_join(ucon.df,by = c("node")) %>% 
			mutate(value = value0 * urban_connection_rate) %>% 
			mutate(tec = 'urban_piped_collection') %>%  
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'urban_piped_collection') %>%  
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )

		urban_uncollected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('urban_waste')) %>% 
			left_join(ucon.df,by = c("node")) %>% 
			mutate(value = value0 * (1-urban_connection_rate)) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'urban_wastewater_release') %>%
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )   
		  
		urban_treated.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('urban_waste')) %>% 
			left_join(utrt.df,by = c("node")) %>% 
			mutate(value = value0 * urban_treated_rate ) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'urban_wastewater_treatment') %>%
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )   

		rural_connected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('rural_final')) %>% 
			left_join(rcon.df,by = c("node")) %>% 
			mutate(value = value0 * rural_connection_rate) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'rural_piped_distribution') %>% 
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units ) 
				
		rural_unconnected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('rural_final')) %>% 
			left_join(rcon.df,by = c("node")) %>% 
			mutate(value = value0 * (1-rural_connection_rate)) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'rural_unimproved_distribution') %>%
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units ) 
			
		rural_collected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('rural_waste')) %>% 
			left_join(rcon.df,by = c("node")) %>% 
			mutate(value = value0 * rural_connection_rate) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'rural_piped_collection') %>% 
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units ) 

		rural_uncollected.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('rural_waste')) %>% 
			left_join(rcon.df,by = c("node")) %>% 
			mutate(value = value0 * (1-rural_connection_rate)) %>% 
			dplyr::select(node,year_all,time,value) %>%
			group_by(node) %>%
			summarise( value =  round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'rural_wastewater_release') %>% 
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units ) 
		  
		rural_treated.df = demand.df %>% 
			filter(year_all == 2015) %>% 
			filter(commodity == 'freshwater') %>% 
			filter(level %in% c('rural_waste')) %>% 
			left_join(rtrt.df,by = c("node")) %>% 
			mutate(value = value0 * rural_treated_rate ) %>% 
			dplyr::select(node,time,value) %>%
			group_by(node) %>%
			summarise( value = round( max( value ) / 1e6, digits = 2 ) ) %>%
			mutate(units = 'mcm_per_day') %>%
			mutate(tec = 'rural_wastewater_treatment') %>% 
			mutate(year_all = 2015) %>% 
			data.frame() %>% 
			dplyr::select( node, tec, year_all, value, units )
			
						 
		### Write to csv	
		water_access.df = do.call( rbind, list( urban_connected.df, 
												urban_collected.df,
												urban_treated.df,
												rural_connected.df, 
												rural_collected.df,
												rural_treated.df ) )
												
		### Append historical capacity csv to include water connection capacities

		# historical capacity csv
		historical_capacity.df1 = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) 
		ind = which( 	as.character( historical_capacity.df1$tec ) %in% 
						c(	'urban_piped_distribution',
							'urban_unimproved_distribution', 
							'urban_piped_collection', 
							'urban_wastewater_release', 
							'urban_wastewater_treatment',
							'rural_piped_distribution',
							'rural_unimproved_distribution', 
							'rural_piped_collection', 
							'rural_wastewater_release', 
							'rural_wastewater_treatment' ) )
		if( length( ind ) > 0 ){ historical_capacity.df1 = historical_capacity.df1[ -1 * ind, ] }
		historical_capacity.df1 = rbind( 	historical_capacity.df1, 
											urban_connected.df, 
											urban_unconnected.df,
											urban_collected.df,
											urban_uncollected.df,
											urban_treated.df,
											rural_connected.df, 
											rural_unconnected.df,
											rural_collected.df,
											rural_uncollected.df,
											rural_treated.df )
		write.csv( 	historical_capacity.df1, 
					"input/historical_new_cap.csv", 
					row.names = FALSE )
		
		}
				
	}			
		 		 

