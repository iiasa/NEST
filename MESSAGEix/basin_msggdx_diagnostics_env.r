library(tictoc)
tic() #119.4 sec elapsed
require( gdxrrw )
require( tidyverse )
require(ggplot2)
library(gridExtra)
library(broom)
require( rgdal )
library(rgeos)

# Location of input data 
setwd( 'P:/is-wel/indus/message_indus' ) 

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")
setwd(indus_ix_path)
basin.spdf = readOGR( paste( "P:/is-wel/indus/message_indus", 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )


if (!exists('shiny_mode')) shiny_mode = F

if (shiny_mode){} else {
    dir.create(file.path(indus_ix_path,paste0('plots_',sc)),showWarnings = F )
    setwd(paste0(indus_ix_path,'/plots_',sc))
    
} #endif


# Basin analyzed
basin = 'Indus'

# Get the relevant data from the gdx output files
scname = c('baseline',sc)
scen_chk = sapply( scname, function( sss ){ paste( 	'MSGoutput_', sss, '.gdx', sep = '' ) } )
					
upath = paste( indus_ix_path, '/model/output/', sep='')

# Import results from gdx
igdx( gams_path )
res.list = lapply( scen_chk, function(fpath){ # 65.97 sec elapsed
	vars = c( 'demand_fixed','CAP_NEW', 'CAP','historical_new_capacity', 'ACT', 'input', 'output', 'inv_cost', 'fix_cost', 'var_cost','EMISS',
	          'STORAGE','STORAGE_CHG', 'bound_storage_up','bound_storage_lo','PRICE_COMMODITY','PRICE_EMISSION')
	gdx_res = lapply( vars, function( vv ){
		tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
		names(tmp$uels) = tmp$domains
		rs = data.frame( tmp$val )
		names(rs) = c( unlist( tmp$domains ), 'value' )
		rs[ , which( names(rs) != 'value' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'value' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
		return(rs)
		} )
	names(gdx_res) = vars	
	return(gdx_res)
	} )
names( res.list ) = scen_chk	

print('Variables loaded from gdxs')

shiny_vars = c( 'demand_fixed', 'CAP_NEW', 'CAP', 'ACT', 'input', 'output', 'inv_cost', 'EMISS','STORAGE','STORAGE_CHG' )

for (vari in shiny_vars){
  assign(paste0(vari,'.shiny'),bind_rows( lapply( scen_chk, function( sc ){ 
    
    tmp.df = res.list[[ sc ]][[ paste0(vari) ]]
    tmp.df$scenario = paste0(sc)
    return(tmp.df)
  } ) ) )
}

EMISS.shiny = EMISS.shiny %>% rename(type = emission) %>% mutate(type_tec)
##### COSTS #####

cost_by_technology.df = bind_rows( lapply( scen_chk, function( sc ){
	
	cap.df = res.list[[ sc ]][[ 'CAP' ]]
	cap_new.df = res.list[[ sc ]][[ 'CAP_NEW' ]]
	act.df = res.list[[ sc ]][[ 'ACT' ]]
	inv.df = res.list[[ sc ]][[ 'inv_cost' ]]
	fix.df = res.list[[ sc ]][[ 'fix_cost' ]]
	var.df = res.list[[ sc ]][[ 'var_cost' ]]
	
	ic.df = merge( cap_new.df, inv.df, by = c( 'node', 'tec', 'year_all' ) , all.x = TRUE ) %>%
				mutate( invc =  value.x * value.y ) %>% 
    	  group_by( node, tec, year_all ) %>% 
    	  summarise( invc = sum(invc) ) %>% 
				dplyr::select( node, tec, year_all, invc )
	ic.df[ is.na( ic.df ) ] = 0	

	fc.df = merge( cap.df, fix.df, by = c( 'node', 'tec', 'vintage', 'year_all' ) , all.x = TRUE ) %>%
				mutate( fixc =  value.x * value.y ) %>% 
				group_by( node, tec, year_all ) %>% 
				summarise( fixc = sum(fixc) ) %>% 
				ungroup() %>% data.frame() %>%
				dplyr::select( node, tec, year_all, fixc )
	fc.df[ is.na( fc.df ) ] = 0	
	
	vc.df = merge( act.df, var.df, by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) , all.x = TRUE ) %>%
				mutate( varc =  value.x * value.y ) %>% 
				group_by( node, tec, year_all ) %>% 
				summarise( varc = sum(varc) ) %>% 
				ungroup() %>% data.frame() %>%
				dplyr::select( node, tec, year_all, varc )
	vc.df[ is.na( vc.df ) ] = 0	
	
	tc.df = merge( merge( ic.df, fc.df, by = c( 'node', 'tec', 'year_all' ) ), vc.df, by = c( 'node', 'tec', 'year_all' ) ) %>%
				mutate( tot = invc + fixc + varc ) %>%
				# rename( inv = value.x, fix = value.y, var = value ) %>%
				mutate( invc = round( invc, digits = 2 ), fixc = round( fixc, digits = 2 ), varc = round( varc, digits = 2 ), value = round( tot, digits = 2 ) ) %>%
				mutate( units = 'million_USD' ) %>%
				dplyr::select( node, tec, year_all, units, invc, fixc, varc, value )
				
	btc.df = tc.df %>%
				group_by( tec, year_all ) %>% 
				summarise( invc = sum(invc), fixc = sum(fixc), varc = sum(varc), value = sum(value) ) %>% 
				ungroup() %>% 
				mutate( node = basin ) %>%
				mutate( units = 'million_USD' ) %>%
				dplyr::select( node, tec, year_all, units, invc, fixc, varc, value )
	
	res = rbind( tc.df, btc.df )
	res$scenario = sc
	res = res[ which( res$value > 0 ), ]
	
	return(res)
	
	} ) )

tec = unique(c(res.list[[ scen_chk[1] ]]$output$tec,res.list[[ scen_chk[1] ]]$input$tec,res.list[[ scen_chk[2] ]]$output$tec,res.list[[ scen_chk[2] ]]$input$tec) )
tech_type.df = rbind( 	data.frame( type = 'fossil_energy', tec = tec[ grepl( 'coal|oil|gas|igcc', tec ) ] ),
						data.frame( type = 'renewables', tec = tec[ grepl( 'solar|wind|geothermal', tec ) ] ),
						data.frame( type = 'hydro', tec = tec[ grepl( 'hydro_river|hydro_canal|hydro_old', tec ) ] ),
						data.frame( type = 'nuclear & ccs', tec = tec[ grepl( 'nuclear|ccs', tec ) ] ),
						data.frame( type = 'electricity grid', tec = tec[ grepl( 'trs_|electricity_distribution|electricity_short_strg', tec ) ] ),
						data.frame( type = 'water distribution', tec = tec[ grepl( 'diversion|distribution|wastewater_collection|desal|canal', tec ) &
						                                                      !grepl( 'electricity|irrigation|hydro', tec )] ),
						data.frame( type = 'wastewater treatment', tec = tec[ grepl( 'wastewater_treatment|wastewater_recycling', tec ) ] ),
						data.frame( type = 'land use', tec = tec[ grepl( 'crop', tec ) ] ),
						data.frame( type = 'rural generation', tec = tec[ grepl( 'agri|ethanol|genset|solid_biom', tec ) ] ),
						data.frame( type = 'irrigation', tec = tec[ grepl( 'irr_|irrigation', tec ) ] ) )	

import_routes = (tech_type.df %>% filter(!gsub('.*_','',tec) %in% as.character(seq(1,15,1) ),type == 'electricity grid',grepl('trs_',tec)))$tec
tech_type.df = tech_type.df %>% 
  mutate(type = if_else(tec %in% import_routes,'electricity import',as.character(type)) )

#graphic settings ------------------
cost_col = c('electricity import' =     '#d9d9d9',
                  'fossil_energy' =     '#787878',
                  "renewables" =        "#ffffb3",
                  'hydro' =             "#80b1d3",
                  'nuclear & ccs' =     "#fccde5",
                  'rural generation' =  "#549149",
                  'electricity grid' =  "#fb8072",
                  "water distribution" =  "#bebada",
                  'wastewater treatment' =   "#b3de69",
                  'land use' =          "#fdb462",
                  'irrigation' =        "#8dd3c7"
)

#value refers tot eh total
cost_by_technology.df = cost_by_technology.df %>% left_join(tech_type.df) %>% 
  mutate(country = if_else(node != 'Indus', gsub('_.*','',node), node))

cost_by_technology.shiny = cost_by_technology.df %>% select(node,year_all,scenario,type,value) %>% 
  group_by(node,year_all,scenario,type) %>% summarise(value = sum(value)) %>% ungroup()
type.shiny = as.character(unique(cost_by_technology.shiny$type))

if (shiny_mode){} else {
  
  df_cost = cost_by_technology.df %>% 
    filter( year_all %in% c(2020,2030,2040,2050) ) %>% 
    group_by( year_all, country, type, scenario ) %>% 
    summarise( investment = sum( invc ), operational = sum(varc + fixc) ) %>% ungroup() %>% 
    group_by( country, type, scenario ) %>% 
    summarise( investment = mean( investment ),operational = mean( operational ) ) %>% 
    gather('cost','value',investment,operational) %>% 
    mutate(value =  1e-3 * value ) %>% 
    mutate(scenario = gsub('.*MSGoutput_','',gsub('\\.gdx.*','',scenario)) )
  
  pdf( 'indus_invest.pdf', width = 6, height = 6 ) 
  
  v1 = ggplot(df_cost %>% filter(!country %in% c('AFG','Indus','CHN') ),aes(x = scenario,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
    facet_wrap(country~cost)+ylab('Billion USD per year')+
    scale_fill_manual(values = cost_col)+
    theme_bw()+ theme(axis.title.x = element_blank())
  plot(v1)	
  
  v2 = ggplot(df_cost %>% filter(country %in% c('Indus') ),aes(x = scenario,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
    facet_wrap(country~cost)+ylab('Billion USD per year')+
    scale_fill_manual(values = cost_col)+
    theme_bw()+ theme(axis.title.x = element_blank())
  plot(v2)
  
  # difference compared to baseline
  df_cost_base = df_cost %>% filter(scenario == scname[1]) %>% 
    rename(valueB = value) %>% select(-scenario) 
  
  df_cost2 = df_cost %>% filter(scenario != scname[1]) %>%
    left_join(df_cost_base) %>% 
    mutate(valueB = if_else(is.na(valueB),0,valueB)) %>% 
    mutate(diff = round((value - valueB),3) )
  # 
  # V3 = ggplot(df_cost2 %>% filter(!country %in% c('AFG','CHN')),aes(x = type, y = value, fill = type,alpha = cost))+
  #   geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  #   facet_wrap(country~scenario)+ylab('Billion USD per year')+
  #   scale_fill_manual(values = cost_col)+
  #   scale_alpha_discrete(range=c(0.4, 1))+
  #   theme_bw()+ theme(axis.title.x = element_blank(),
  #                     axis.text.x = element_text(angle = 30))
  # plot(V3)
  # 
  # df_cost3 = df_cost2 %>% group_by(country,scenario,cost) %>% 
  #   summarise(value = sum(value), valueB = sum(valueB)) %>% ungroup() %>% 
  #   mutate(diff = round((value - valueB),3) )
  # 
  # V4 = ggplot(df_cost3 %>% filter(!country %in% c('AFG','CHN')),aes(x = country, y = value, fill = cost))+
  #   geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  #   facet_wrap(~scenario)+ylab('Billion USD per year')+
  #   theme_bw()+ theme(axis.title.x = element_blank() )
  # plot(V4)
  
  dev.off()

}

#### Nexus interactions ####

energy_for_water.df = bind_rows( lapply(  scen_chk, function( sc ){
	
	merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
				filter( commodity == 'electricity', !( tec %in% tec[ grepl( 'electricity|trs_', tec ) ] ) ),
			data.frame( res.list[[ sc ]]$ACT ),
			by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
				mutate( value = value.x * value.x ) %>%
				select( node, tec,year_all, value ) %>%
				group_by( node,tec, year_all ) %>% summarise( sum( value ) )  %>% data.frame() %>%
		mutate( scenario = sc ) %>% rename( value = sum.value.) %>%
		select( node,tec, scenario, year_all, value )	
			
	} ) )

energy_for_water.shiny = energy_for_water.df

# irrigation_gw_diversion and so on are to be consider en for water or for irrigation/land use? 
# maybe en for water would be just for urban and rural water management. 
energy_for_irrigation.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( commodity %in% c('energy','electricity'), level %in% c('agriculture_final','irrigation_final')  ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, year_all, value ) %>%
    group_by( node, year_all ) %>% summarise( sum( value ) )  %>% data.frame() %>%
    mutate( scenario = sc ) %>% rename( value = sum.value.) %>%
    select( node, scenario, year_all, value )	
  
} ) )

water_for_energy.df = bind_rows( lapply(  scen_chk, function( sc ){
	
	merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
				filter( commodity == 'freshwater', tec %in% unique( data.frame( res.list[[ sc ]]$output ) %>% 
																		filter( commodity == 'electricity', level == 'energy_secondary'  ) %>%
																		select( tec ) %>% unlist() ) ),
			data.frame( res.list[[ sc ]]$ACT ),
			by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
				mutate( value = value.x * value.y ) %>%
				select( node, year_all, value ) %>%
				group_by( year_all, node ) %>% summarise( value = sum( value ) )  %>% data.frame() %>%
		mutate( scenario = sc ) %>% 
		select( node, scenario, year_all, value )	
			
	} ) )
		
water_for_energy.shiny = water_for_energy.df

water_for_irrigation.df = bind_rows( lapply(  scen_chk, function( sc ){
	
	merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
				filter( commodity == 'freshwater', tec %in% tec[ grepl('irrigation_', tec ) ] ),
			data.frame( res.list[[ sc ]]$ACT ),
			by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
				mutate( value = value.x * value.y ) %>%
				select( node, year_all, value ) %>%
				group_by( year_all, node ) %>% summarise( sum( value ) )  %>% data.frame() %>%
		mutate( scenario = sc ) %>% rename( value = sum.value.) %>%
		select( node, scenario, year_all, value )	
			
	} ) )

water_for_irrigation.shiny = water_for_irrigation.df

biomass_for_energy.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( commodity == 'biomass' ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, year_all, value ) %>%
    group_by( node, year_all ) %>% summarise( sum( value ) )  %>% data.frame() %>%
    mutate( scenario = sc ) %>% rename( value = sum.value.) %>%
    select( node, scenario, year_all, value )	
  
} ) )

biomass_for_energy.shiny = biomass_for_energy.df

if (shiny_mode){} else {	
  
  pdf( 'indus_nexus.pdf', width = 7, height = 3.5 )
  p1 = layout( matrix( c(1,2,3), 1,3, byrow=TRUE ), widths=c(0.2,0.2,0.2), heights=c(0.7) )
  par(mar=c(3,6,2,0.5), oma = c(2,2,2,2))
  colc = c('gray10','gray40','gray70','gray90')
  barplot( 	1e-3 * as.matrix( cbind( 	energy_for_water.df %>% filter( scenario == scen_chk[1], year_all %in% c(2020,2030,2040,2050) ) %>%
                                        group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist(),
  									energy_for_water.df %>% filter( scenario == scen_chk[2], year_all %in% c(2020,2030,2040,2050) ) %>%
  									  group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist() ) ),
  			beside = TRUE,
  			names.arg =  scname,
  			ylim = c(0,1.2*max((energy_for_water.df %>% group_by(scenario,year_all) %>% summarise(value = sum(value)))$value)*1e-3),
  			ylab = 'GWh',
  			col = colc,
  			main = 'Energy for Water'	)
  abline(h=0)
  box()
  legend( 'topleft', legend = c(2020,2030,2040,2050), fill = colc, bty='n' )

  barplot( 	1 * as.matrix( cbind( 	water_for_energy.df %>% filter( scenario == scen_chk[1], year_all %in% c(2020,2030,2040,2050) ) %>%
                                       group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist(),
  										water_for_energy.df %>% filter( scenario == scen_chk[2], year_all %in% c(2020,2030,2040,2050) ) %>%
  										  group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist() ) ),
  			beside = TRUE,
  			names.arg =  scname,
  			col = colc,
  			ylab = 'Mm3/year',
  			ylim = c(0,1.2*max((water_for_energy.df %>% group_by(scenario,year_all) %>% filter(year_all <= 2050 )%>% summarise(value = sum(value)))$value)),
  			main = 'Water for Energy'	)
  abline(h=0)
  box()
  legend( 'topleft', legend = c(2020,2030,2040,2050), fill = colc, bty='n' )

  barplot( 	1 * as.matrix( cbind( 	water_for_irrigation.df %>% filter( scenario == scen_chk[1], year_all %in% c(2020,2030,2040,2050) ) %>%
                                     group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist(),
  										water_for_irrigation.df %>% filter( scenario == scen_chk[2], year_all %in% c(2020,2030,2040,2050) ) %>%
  										  group_by(scenario,year_all) %>% summarise(value = sum(value)) %>% ungroup() %>% select( value ) %>% unlist() ) ),
  			beside = TRUE,
  			col = colc,
  			names.arg =  scname,
  			ylab = 'Mm3/year',
  			ylim = c( 0,1.5*max((water_for_irrigation.df %>% group_by(scenario,year_all) %>% summarise(value = sum(value)))$value*1) ),
  			main = 'Water for Irrigation'	)
  abline(h=0)
  box()
  legend( 'topleft', legend = c(2020,2030,2040,2050), fill = colc, bty='n' )
  dev.off()
  
#   a = crossing(orig = c('Water','Energy','Land'),
#            to = c('Water','Energy','Land'),
#            min = 0) %>% filter(orig != to)
#   ## round plot
#   tmp = rbind(energy_for_water.df %>% select(-tec) %>% mutate(orig = 'Energy', to = 'Water') %>%
#                 mutate(value = value * 1e-3) %>% mutate(unit = 'GWh'),
#               energy_for_irrigation.df %>% mutate(orig = 'Energy', to = 'Land') %>%
#                 mutate(value = value * 1e-3) %>% mutate(unit = 'GWh'),
#           water_for_energy.df %>% mutate(orig = 'Water', to = 'Energy') %>%
#             mutate(value =  value ) %>% mutate(unit = 'MCM'),
#           water_for_irrigation.shiny %>% mutate(orig = 'Water', to = 'Land') %>%
#             mutate(value =  value ) %>% mutate(unit = 'MCM'),
#           biomass_for_energy.df %>% mutate(orig = 'Land', to = 'Energy') %>%
#             mutate(value =  value ) %>% mutate(unit = 'kt') ) %>%
#     group_by(scenario,year_all, orig, to) %>%
#     dplyr::summarise(value = sum(value)) %>% ungroup() %>%
#     group_by(scenario, orig, to) %>%
#     dplyr::summarise(value = mean(value)) %>% ungroup() %>% 
#     right_join(a) %>% mutate(scenario = if_else(is.na(value),sc,scenario)) %>%  ## hardcoded
#     mutate(value = if_else(is.na(value),min,value)) %>% select(-min)
# 
#   # plot using chordDiagram
#   require("circlize")
#   make_nexus_diagram <- function(tmp,sc){
#     m = tmp %>% filter(scenario == sc) %>%
#               tidyr::spread(to,value) %>% dplyr::select(Energy,Land,Water) %>% as.matrix()
#               dimnames(m) <- list(orig = c('Energy [GWh]','Land [kt]','Water [MCM]'), dest = c('Energy [GWh]','Land [kt]','Water [MCM]'))
#               m[is.na(m)] = 0
# 
#     df1 = data.frame(order = c(1:3),
#                       r = c(220,50,0),
#                       g = c(0,140,150),
#                       b = c(0,40,230),
#                       region = c('Energy [GWh]','Land [kt]','Water [MCM]')) %>%
#       mutate(col = rgb(r, g, b, max=255), max = rowSums(m) )
# 
#     #plot
#     circos.clear()
#     #par(mar = rep(0, 4), cex=0.9)
#     #change degree and add gap,trak margin esternal and up to flows
#     circos.par(canvas.ylim=c(-1.8,1.8),track.margin = c(0.01, -0.05),start.degree = 105, gap.degree = min(80,max(50,sum(df1$max))) )
#     chordDiagram(x = m,
#                  directional = 1,
#                  direction.type = c("diffHeight", "arrows"),
#                  link.arr.type = "big.arrow",
#                  order = df1$region,
#                  grid.col = df1$col, annotationTrack = "grid",
#                  transparency = 0.25,  annotationTrackHeight = c(0.1, 0.1),
#                  diffHeight  = -0.24)
# 
#     #add in labels and axis
#     circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#       xlim = get.cell.meta.data("xlim")
#       xlim[2] = (df1 %>% filter(region == get.cell.meta.data("sector.index")))$max
#       sector.index = get.cell.meta.data("sector.index")
#       circos.text(mean(get.cell.meta.data("xlim")), 2.8, sector.index, facing = "bending")
#       circos.axis(labels.cex=0.5,"top", major.at = seq(0, ceiling(max(xlim)/500)*500 , 500),
#                   minor.ticks=1, labels.away.percentage = 0.2, labels.niceFacing = F )
#     }, bg.border = NA)
#     circos.clear()
#     title(main = list(paste0(sc), cex = 1,
#                      col = "black", font = 1),line = -3 )
# 
#   }
# 
#   pdf( 'nexus_flows.pdf', width = 8, height = 6 )
#   for (i in seq_along(scen_chk) )
#   {
# 
#   par(mar = c(0.2,0.2,0.2,0.2))
#   make_nexus_diagram(tmp,scen_chk[i])
#   }
#   dev.off()
# 
#   
}

#### STORAGE ####

storage.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$STORAGE ) %>% 
    # mutate(node = gsub('_.*','',node) ) %>%
    # group_by(node,year_all,time) %>% 
    # summarise(value = sum(value)) %>% ungroup() %>% 
    filter(year_all<= 2050) %>% 
    mutate( scenario = sc ) %>% 
    mutate(month = as.numeric(time) ) %>% 
    select( node, scenario, year_all, month, value )
  
} ) )

if (shiny_mode){} else {

  pdf( 'storage.pdf', width = 7, height = 6 ) 
  
  for (i in seq_along(scen_chk) )  
  {   
    par(mfrow = c(i,1))
    storage.dfi = storage.df %>% filter(scenario == scen_chk[i])
    
    VAR1 = ggplot(storage.dfi,aes(month,value,colour = year_all)) +
      geom_line()+
      facet_wrap(~node)+
      theme_bw()+ ggtitle(paste0('scenario: ',scname[i]))
    plot(VAR1)   
  } 
  dev.off() 
}


#### COMMODITIES ####
# TO DO, fix color when plotting multiple graphs
# electricity generation
#unit is in MW per month, MWmonth, when summing to yearly values we have MWyear = 8.760 GWh
energy_prod_by_source.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter( commodity == 'electricity' & level == 'energy_secondary' |
                      commodity == 'electricity' & level == 'irrigation_final' & tec != 'electricity_distribution_irrigation') %>% 
            filter(!grepl('trs_',tec)),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, tec, year_all, time, value ) %>%
#    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

transmission_flow.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter( grepl('trs',tec) ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    dplyr::select( node, tec, year_all, mode, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) ) %>% dplyr::rename(node_in = node) %>% 
  mutate(node_out = gsub('.*\\|','',tec))

elec_imp_exp = transmission_flow.df %>% 
  mutate(node = node_in) %>% 
  mutate(value = if_else(mode == 1,-value,value)) %>% 
  bind_rows(transmission_flow.df %>% 
              mutate(node = node_out) %>% 
              mutate(value = if_else(mode == 1,value,-value)) ) %>% 
  dplyr::select( node, tec, year_all, time,scenario, value ) %>% 
  mutate(type = 'imp_exp')

natioal_inp_exp = elec_imp_exp %>% 
  filter(node %in% c('AFG','CHN','IND','PAK')) %>% 
  mutate(value = -value) %>% 
  rename(country = node)

# tec = unique(res.list[[ scen_chk[1] ]]$output$tec)
tech_type_ene.df = rbind( 	data.frame( type = 'coal', tec = tec[ grepl( 'coal', tec ) ] ),
                       data.frame( type = 'nuclear_ccs', tec = tec[ grepl( 'nuclear|igcc', tec ) ] ),
                       data.frame( type = 'gas', tec = tec[ grepl( 'gas', tec ) ] ),
                       data.frame( type = 'hydro', tec = tec[ grepl( 'hydro', tec ) ] ),
                       data.frame( type = 'oil', tec = tec[ grepl( 'oil', tec ) ] ),
                       data.frame( type = 'wind', tec = tec[ grepl( 'wind', tec ) ] ),
                       data.frame( type = 'solar', tec = tec[ grepl( 'solar', tec ) ] ),
                       data.frame( type = 'geothermal', tec = tec[ grepl( 'geothermal', tec ) ] ),
                       data.frame( type = 'rural gen.', tec = c('ethanol_genset','irri_diesel_genset','agri_pv') ))

#graphic settings ------------------
en_source_col = c('imp_exp' =     '#dcdcdc',
                  "wind" =        "#4DAF4A",
                  'solar' =       "#FFFF33",
                  'rural gen.' =   "#A65628",
                  'hydro' =       "#377EB8",
                  "geothermal" =  "#984EA3",
                  'gas' =         "#FF7F00",
                  'nuclear_ccs' = "#F781BF",
                  'coal' =        "#E41A1C",
                  'oil' =         '#0a0a0a'
)

en_source_order = as.data.frame(en_source_col) %>% mutate(type = row.names(.))
theme_common = theme(legend.background = element_rect(fill="white",
                                                      size=0.1, linetype="solid", 
                                                      colour ="black"))
#-----------------------------

energy_prod_by_source.df = energy_prod_by_source.df %>% left_join(tech_type_ene.df) %>% 
  mutate(value = (value * 30 *24* 1e-6), #conversion from MWmonth to TWh
         unit = 'TWh')

energy_prod_by_source.shiny = energy_prod_by_source.df %>% select(-tec) %>% 
  bind_rows(elec_imp_exp %>% filter(!node %in% c('AFG','CHN','IND','PAK')) %>% 
              mutate(value = (value * 30 *24* 1e-6), #conversion from MWmonth to TWh
                     unit = 'TWh')            ) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type",'unit' ) %>% 
  summarise(value = sum(value)) %>% ungroup()
type.shiny = unique(c(type.shiny,as.character( unique(energy_prod_by_source.shiny$type)) ))

# ENERGY FINAL 

final_energy_by_use.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( commodity == 'electricity' & level == 'urban_final' |
                      commodity == 'electricity' & level == 'irrigation_final' |
                      commodity == 'electricity' & level == 'rural_final'|
                      commodity == 'electricity' & level == 'industrial_final'|
                      grepl('diversion',tec)),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, tec, commodity,level,year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) ) 

tech_type_ene.df = rbind( 	data.frame( type = 'water distr', tec = tec[ grepl( 'diversion|seawater|distribution', tec ) ] ),
                           data.frame( type = 'canal', tec = tec[ grepl( 'canal', tec ) ] ),
                           data.frame( type = 'water treat', tec = tec[ grepl( 'wastewater', tec ) ] ),
                           data.frame( type = 'smart irr', tec = tec[ grepl( 'irr_smart', tec ) ] ),
                           data.frame( type = 'drip irr', tec = tec[ grepl( 'irr_drip', tec ) ] ),
                           data.frame( type = 'sprinkler irr', tec = tec[ grepl( 'irr_sprinkler', tec ) ] ))

#graphic settings ------------------
en_final_col = c( "industry demand" =     "#FF7F00",
                  "rural demand" =        "#4DAF4A",
                  'urban demand' =   "#A65628",
                  'sprinkler irr' = "#F781BF",
                  'smart irr' =       "#FFFF33",
                  "drip irr" =  "#984EA3",
                  'water distr' =       "#377EB8",
                  'canal' =        "#E41A1C",
                  'water treat' =         "#75cbf4"
)

en_final_order = as.data.frame(en_final_col) %>% mutate(type = row.names(.))
#-----------------------------

final_energy_by_use.df = final_energy_by_use.df %>% left_join(tech_type_ene.df) %>% 
  mutate(type = if_else(is.na(type),'other',as.character(type)) ) %>% 
  mutate(value = (value * 30 *24* 1e-6), #conversion from MWmonth to TWh
         unit = 'TWh')

elec_demand = demand_fixed.shiny %>% filter(year_all>2015 & year_all <2060 & commodity == 'electricity') %>% 
  mutate(type = gsub('_final',' demand',level)) %>% 
  mutate(value = (value * 30 *24* 1e-6), #conversion from MWmonth to TWh
         unit = 'TWh')

final_energy_by_use.shiny = final_energy_by_use.df %>% select(node,scenario,time,type,level,year_all,value,unit) %>% 
  bind_rows(elec_demand %>% select(node,scenario,type,level,year_all,value,unit) ) %>% select(-level) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type" ,'unit') %>% 
  summarise(value = sum(value)) %>% ungroup()
type.shiny = unique(c(type.shiny,as.character( unique(final_energy_by_use.shiny$type)) ))



# %>% group_by(level,year_all,scenario) %>% 
#   summarise(inp = sum(value) ) %>% ungroup()
# 
# final_energy_by_out.df = bind_rows( lapply(  scen_chk, function( sc ){
#   
#   merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
#             filter( commodity == 'electricity' & level == 'urban_final' |
#                       commodity == 'electricity' & level == 'irrigation_final' |
#                       commodity == 'electricity' & level == 'rural_final'  ),
#           data.frame( res.list[[ sc ]]$ACT ),
#           by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
#     filter(year_all <= 2050) %>% 
#     mutate( value = value.x * value.y ) %>%
#     select( node, tec, commodity,level,year_all, time, value ) %>%
#     #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
#     mutate( scenario = sc )
#   
# } ) )
# 
# sum(final_energy_by_out.df$value)-sum(final_energy_by_use.df$value) - sum(elec_demand$value)

# IN SCENARIO WITH SDGS, ELECTRICITY DISTRIBUTION OUTPUTS IN IRRIGATION FINAL IS
# HIGHER THAN THE REQUIREMENTS OF IRRIGATION TECHNOLOGIES, 1800 TWh
# IS THE ENERGY GOING SOMEWHERE ELSE OR IS IT DUE TO INEQUALITY IN COMMODITY BALANCE? WHY HAPPENS ONLY IN THAT CASE?


if (shiny_mode){} else {
  
en_mix_plot = energy_prod_by_source.df %>% 
  mutate(country = gsub('_.*','',node)) %>% 
  group_by(country,type,year_all,scenario) %>% 
  bind_rows(natioal_inp_exp %>% 
              mutate(value = (value * 30 *24* 1e-6) )) %>% # conversion only for imports
  summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(year = as.numeric( year_all) ) %>% 
  filter(country != 'CHN')

en_mix_plot_month = energy_prod_by_source.df %>% 
  mutate(country = gsub('_.*','',node)) %>% 
  group_by(country,time,type,year_all,scenario) %>% 
  bind_rows(natioal_inp_exp %>% 
              mutate(value = (value * 30 *24* 1e-6) )) %>% # conversion only for imports
  summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(year = as.numeric( year_all),
         month = as.numeric(time)) %>% 
  filter(country != 'CHN')

final_en_plot = final_energy_by_use.df %>% select(node,type,level,year_all,scenario,value) %>% 
  bind_rows(elec_demand %>% select(node,type,level,year_all,scenario,value) ) %>% 
  mutate(country = gsub('_.*','',node)) %>% 
  group_by(country,type,year_all,scenario) %>% 
  summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(year = as.numeric( year_all) ) %>% 
  filter(country != 'CHN')

plist1 = list()
plist2 = list()
pdf( 'energy_mix.pdf', width = 7, height = 6 ) 
maxy = max((en_mix_plot %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
maxy2 = max((final_en_plot %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
for (i in seq_along(scen_chk) )  {   
  par(mfrow = c(i,1))
  plist1[[i]] = ggplot(en_mix_plot %>% filter(scenario == scen_chk[i]),
                       aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(~country)+ylim(0,maxy)+ylab('TWh')+
    scale_fill_manual(name = 'technology',values = en_source_col)+
    theme_bw()+ ggtitle(paste0('Electrcity supply. Scenario: ',scname[i]))+
    theme_common
  
  plist2[[i]] = ggplot(final_en_plot %>% filter(scenario == scen_chk[i]),
                       aes(x = year,y = value,fill = factor(type , levels = en_final_order$type)))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(~country)+ylim(0,maxy2)+ylab('TWh')+
    scale_fill_manual(name = 'end use',values = en_final_col)+
    theme_bw()+ ggtitle(paste0('Electrcity use. Scenario: ',scname[i]))+
    theme_common
} 
grid.arrange(grobs=plist1)

maxy = max((en_mix_plot_month %>% group_by(country,month,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
for (i in seq_along(scen_chk) )  { 
  par(mfrow = c(i,1))
  VAR2 = ggplot(en_mix_plot_month %>% filter(scenario == scen_chk[i]),
                aes(x = month,y = value,fill = factor(type , levels = en_source_order$type)))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(country~year)+ylim(0,maxy)+ylab('TWh')+
    scale_fill_manual(name = 'technology',values = en_source_col)+
    scale_x_continuous(breaks = c(1,3,6,9,12))+
    theme_bw()+ ggtitle(paste0('Electrcity supply. Scenario: ',scname[i]))
  plot(VAR2)
}
grid.arrange(grobs=plist2)
dev.off() 

}

# INSTALLED CAPACITY + plot HISTORICAL

historical_capacity.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$historical_new_capacity ) %>% 
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

tech_type_ene.df = rbind( 	data.frame( type = 'coal', tec = tec[ grepl( 'coal', tec ) ] ),
                           data.frame( type = 'nuclear_ccs', tec = tec[ grepl( 'nuclear|igcc', tec ) ] ),
                           data.frame( type = 'gas', tec = tec[ grepl( 'gas', tec ) ] ),
                           data.frame( type = 'hydro', tec = tec[ grepl( 'hydro', tec ) ] ),
                           data.frame( type = 'oil', tec = tec[ grepl( 'oil', tec ) ] ),
                           data.frame( type = 'wind', tec = tec[ grepl( 'wind', tec ) ] ),
                           data.frame( type = 'solar', tec = tec[ grepl( 'solar', tec ) ] ),
                           data.frame( type = 'geothermal', tec = tec[ grepl( 'geothermal', tec ) ] ),
                           data.frame( type = 'rural gen.', tec = c('ethanol_genset','irri_diesel_genset','agri_pv') ))

ene_historical_capacity.df = historical_capacity.df %>% left_join(tech_type_ene.df) %>% 
  filter(!is.na(type)) %>% 
  select(node,type,tec,year_all,scenario,value)

capacity.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$CAP ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( scenario = sc )
  
} ) )

ene_capacity.df = capacity.df %>% left_join(tech_type_ene.df) %>% 
  filter(!is.na(type)) %>% 
  select(node,type,tec,year_all,scenario,value)
  # bind_rows(ene_historical_capacity.df %>% 
  #             group_by(node,type,scenario) %>% 
  #             mutate(value = cumsum(value)))

ene_capacity_plot = ene_capacity.df %>% 
  mutate(year = as.numeric(year_all)) %>% 
  select(node,type,tec,year,scenario,value) %>% 
  mutate(country = gsub('_.*','',node)) 

ggplot(ene_capacity_plot %>% filter(scenario == scen_chk[2]),aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+
  scale_fill_brewer(type = 'qual',palette = 2)+
  theme_bw()+ ggtitle(paste0('Power -plant installed capacity. Scenario: ',scname[2]))

new_ene_capacity.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$CAP_NEW ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( scenario = sc )
  
} ) ) %>% left_join(tech_type_ene.df) %>% 
  filter(!is.na(type)) %>% 
  select(node,type,tec,year_all,scenario,value) %>% 
  bind_rows(ene_historical_capacity.df )

new_ene_capacity_plot = new_ene_capacity.df %>% 
  mutate(year = as.numeric(year_all)) %>% 
  select(node,type,tec,year,scenario,value) %>% 
  mutate(country = gsub('_.*','',node)) %>% 
  group_by(country,type,year,scenario) %>% 
  summarise(value = sum(value))

ggplot(new_ene_capacity_plot %>% filter(scenario == scen_chk[2]),aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+
  scale_fill_brewer(type = 'qual',palette = 2)+
  theme_bw()+ ggtitle(paste0('Power plants new installed capacity. Scenario: ',scname[2]))

# CROP AREA and WATER

area_by_crop.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter( commodity == 'crop_land', level == 'area' , tec != 'fallow_crop' ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )
tech_type_crop.df = rbind( data.frame( type = 'fallow_crop', tec = tec[ grepl( 'fallow_crop', tec ) ] ),
                           data.frame( type = 'cotton', tec = tec[ grepl( 'cotton', tec ) ] ),
                           data.frame( type = 'fodder', tec = tec[ grepl( 'fodder', tec ) ] ),
                           data.frame( type = 'pulses', tec = tec[ grepl( 'pulses', tec ) ] ),
                           data.frame( type = 'maize', tec = tec[ grepl( 'maize', tec ) ] ),
                           data.frame( type = 'rice', tec = tec[ grepl( 'rice', tec ) ] ),
                           data.frame( type = 'sugarcane', tec = tec[ grepl( 'sugarcane', tec ) ] ),
                           data.frame( type = 'wheat', tec = tec[ grepl( 'wheat', tec ) ] ) )

#graphic settings ------------------
crop_col = c('cotton' =    "#1b9e77",
                  "fodder" =    "#d95f02",
                  'pulses' =    "#7570b3",
                  'maize' =    "#25bad9",
                  'rice' =      "#e7298a",
                  'sugarcane' = "#66a61e",
                  'wheat' =     '#e6ab02'
)


crop_order = as.data.frame(crop_col) %>% mutate(type = row.names(.))
#-----------------------------

area_by_crop.df = area_by_crop.df %>% left_join(tech_type_crop.df) %>% 
  mutate(method = if_else(grepl('rainfed_',tec),'rainfed','irrigated'))
#the method dimension does not go in the shiny display, it does not match with plot generalization
area_by_crop.shiny = area_by_crop.df %>% select(-tec) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type" ) %>% 
  summarise(value = sum(value)) %>% ungroup()
type.shiny = unique(c(type.shiny,as.character( unique(area_by_crop.shiny$type)) ))

if (shiny_mode){} else {
  crop_area_plot_year = area_by_crop.df %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,method,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    filter(country != 'CHN')
  
  crop_area_plot_month = area_by_crop.df %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,time,method,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    mutate(month = as.numeric(time)) %>% 
    filter(country != 'CHN')
  
  plist = list()
  pdf( 'crop_land.pdf', onefile = TRUE) 
  maxy = max((crop_area_plot_year %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
  for (i in seq_along(scen_chk) )  {   

    plist[[i]] = ggplot(crop_area_plot_year %>% filter(scenario == scen_chk[i]),
                        aes(x = year,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(~country)+ylim(0,maxy)+ylab('Mha')+
      scale_fill_manual(name = 'crop',values = crop_col)+
    scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
    theme_bw()+ ggtitle(paste0('crop area. Scenario: ',scname[i]))
    
  }
  grid.arrange(grobs=plist)
  maxy = max((crop_area_plot_month %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
  for (i in seq_along(scen_chk) )  { 
    par(mfrow = c(i,1))
    VAR2 = ggplot(crop_area_plot_month %>% filter(scenario == scen_chk[i]),
                  aes(x = month,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(country~year)+ylim(0,maxy)+ylab('Mha')+
      scale_fill_manual(name = 'crop',values = crop_col)+
      scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
      scale_x_continuous(breaks = c(1,3,6,9,12))+
    theme_bw()+ ggtitle(paste0('crop area. Scenario: ',scname[i]))
  plot(VAR2)
  }
  dev.off()
}

# WATER CONSUMPTION BY SOURCE
# groundwater: for the model it is always freshwater.aquifer. however fossil gw = 
# water from freshwater.aquier - water carried by renew_gw_extract
# indeed, renew_gw_extract moves water from renew_gw.aquifer to freshwater_aquifer

surface_water.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( tec == 'sw_extract' |
                    commodity %in% c('wastewater') & level %in% c('urban_secondary','rural_secondary','industry_secondary') ) %>% 
            filter( tec != "environmental_flow") ,
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node,commodity,level, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

desal_and_gw.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter(grepl( 'seawater', tec ) |
                     grepl( 'gw_extract', tec ) ) ,
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node,commodity,level, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

tech_type_waterS.df = rbind( data.frame( type = 'fossil gw', tec = "gw_extract"),
                           data.frame( type = 'renewable gw', tec = 'renew_gw_extract' ),
                           data.frame( type = 'surface water', tec = tec[ grepl( 'sw_extract', tec ) ] ),
                           data.frame( type = 'seawater', tec = tec[ grepl( 'seawater', tec ) ] ),
                           data.frame( type = 'wastewater', tec = tec[ grepl( 'wastewater', tec ) ] ))

#graphic settings ------------------
water_source_col = c( "fossil gw" =     "#a6cee3",
                  "renewable gw" =        "#33a02c",
                  'surface water' =   "#1f78b4",
                  'seawater' = "#b2df8a",
                  'wastewater' =       "#fb9a99",
                  'other' = '#dcdcdc'
                  
)

water_by_source.df = surface_water.df %>% bind_rows(desal_and_gw.df) %>% 
  left_join(tech_type_waterS.df) %>% 
  mutate(type = if_else(is.na(type),'other',as.character(type)) )

water_by_source.shiny = water_by_source.df %>% select(-commodity,-level,-tec) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type" ) %>% 
  summarise(value = sum(value)) %>% ungroup()
type.shiny = unique(c(type.shiny,as.character( unique(water_by_source.shiny$type)) ))

# Water final

final_water_by_use.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( commodity == 'freshwater' & level == 'energy_secondary' |
                      commodity == 'freshwater' & level == 'irrigation_final'),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, tec, commodity,level,year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

tech_type_water.df = rbind( 	data.frame( type = 'power plants', tec = tec[ grepl( 'coal_|gas_|oil_|nuclear_|geothermal_', tec ) ] ),
                             data.frame( type = 'flood irr', tec = tec[ grepl( 'irr_flood', tec ) ] ),
                             data.frame( type = 'flood CL', tec = tec[ grepl( 'irr_canal_lining_flood', tec ) ] ),
                             data.frame( type = 'smart drip', tec = tec[ grepl( 'irr_drip_smart', tec ) ] ),
                             data.frame( type = 'drip irr', tec = tec[ grepl( 'irr_drip', tec ) & !grepl( 'smart', tec ) ] ) ,
                             data.frame( type = 'sprinkler irr', tec = tec[ grepl( 'irr_sprinkler', tec ) & !grepl( 'smart', tec ) ] ),
                             data.frame( type = 'smart sprinkler', tec = tec[ grepl( 'irr_sprinkler_smart', tec ) ] ) )

#graphic settings ------------------
water_final_col = c( "power plants" =     "#33a02c",
                      "flood irr" =       "#1f78b4",
                      "flood CL" =        "#984ea3",
                      'smart drip' =       "#984ea3",
                      'drip irr' =        "#49bfe3",
                      'sprinkler irr' =   "#fdbf6f",
                      'smart sprinkler' = '#fdda4e',
                     'industry demand' =  '#b2df8a',
                     'rural demand' =     '#fb9a99',
                     'urban demand' =     '#e31a1c',
                     'other'        =     '#dcdcdc'
)


final_water_by_use0.df = final_water_by_use.df %>% left_join(tech_type_water.df) %>% 
  mutate(type = if_else(is.na(type),'other',as.character(type)) )

water_for_crops.shiny = final_water_by_use.df %>% filter(level == 'irrigation_final') %>% 
  mutate(type = gsub('.*\\_', '', tec)) %>% 
  select(-tec,-commodity,-level) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type" ) %>% 
  summarise(value = sum(value)) %>% ungroup()

water_demand = demand_fixed.shiny %>% filter(year_all>2015 & year_all <2060 & commodity == 'freshwater' & !level %in% c('inflow','river_in') ) %>% 
  mutate(type = gsub('_final',' demand',level))

final_water_by_use.df = final_water_by_use0.df %>% select(node,scenario,time,type,level,year_all,value) %>% 
  bind_rows(water_demand %>% select(node,scenario,time,type,level,year_all,value) )
final_water_by_use.shiny = final_water_by_use.df %>% select(-level) %>% 
  group_by_("node"  ,   "year_all", "time" ,  "scenario", "type" ) %>% 
  summarise(value = sum(value)) %>% ungroup()
type.shiny = unique(c(type.shiny,as.character( unique(water_for_crops.shiny$type)) ,
                      as.character( unique(final_water_by_use.shiny$type)) ))


groundwater_extr.df = water_by_source.df %>% 
  filter(type %in% c('fossil gw','renewable gw')) %>% 
  group_by(node,year_all,time,scenario) %>% 
  summarise(value = sum(value)) %>% ungroup() #sum fossil and renewable groundwater extractions

# example of map that should be integrated in the loop
basin.spdf2 = basin.spdf
names(basin.spdf2)[1] = 'node'
basin.spdf2 <- basin.spdf2[ 'node']

# Let's add a unique ID column to our data.
basin.spdf2@data$id <- row.names(basin.spdf2@data)
#basin.tidy have the shapefiles coordinate
basin.tidy <- broom::tidy(basin.spdf2)
basin.tidy <<- dplyr::left_join(basin.tidy, basin.spdf2@data, by='id')

theme_map <<- 
  #    theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  ) 

# example, we plot all scenarios, all years, average through months
gw_plot_data = groundwater_extr.df %>% 
  group_by(node,year_all,scenario) %>% 
  summarise(value = mean(value)) %>% ungroup()

#basin.tidy2 has the value data
basin.tidy2 <- dplyr::left_join(basin.tidy, gw_plot_data, by='node') %>% 
  filter(!is.na(value))

ggplot() +
  geom_polygon(data = basin.tidy, aes(long,lat, group=group),color='black',fill = "#f5f5f2",size = 0.1) +
  geom_polygon(data = basin.tidy2, aes(long,lat, group=group,fill=value),alpha=0.8,color='black',size = 0.1) +
  coord_map("azequalarea") +
#  facet_wrap(as.formula(paste("~", input$grouping)))+
  facet_wrap(year_all~scenario)+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  theme_map

# PLOT WATER SUPPLY AND FINAL USE
if (shiny_mode){} else {
  
  water_by_source_plot_year = water_by_source.df %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    filter(country != 'CHN')
  
  water_by_source_plot_month = water_by_source.df %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,time,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    mutate(month = as.numeric(time)) %>% 
    filter(country != 'CHN')
  
  final_water_plot =  final_water_by_use.df %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    filter(country != 'CHN')
  
  crop_water_use_plot_yr = final_water_by_use0.df %>% select(-type) %>% filter(level == 'irrigation_final') %>% 
    mutate(type = gsub('.*\\_', '', tec)) %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,type,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    filter(country != 'CHN')
  
  crop_water_use_plot_mth = final_water_by_use0.df %>% select(-type) %>% filter(level == 'irrigation_final') %>% 
    mutate(type = gsub('.*\\_', '', tec)) %>% 
    mutate(country = gsub('_.*','',node)) %>% 
    group_by(country,type,time,year_all,scenario) %>% 
    summarise(value = sum(value)) %>% ungroup() %>% 
    mutate(year = as.numeric( year_all) ) %>% 
    mutate(month = as.numeric(time)) %>% 
    filter(country != 'CHN')
  
  plist1 = list()
  plist2 = list()
  plist3 = list()
  pdf( 'water_supply_and_final_use.pdf', onefile = TRUE) 
  maxy1 = max((water_by_source_plot_year %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
  maxy2 = max((final_water_plot %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
  maxy3 = max((crop_water_use_plot_yr %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)

  for (i in seq_along(scen_chk) )  {   
    plist1[[i]]= ggplot(water_by_source_plot_year %>% filter(scenario == scen_chk[i]),aes(x = year,y = value,fill = type))+
      geom_bar( stat = "identity", position = "stack") +
      facet_wrap(~country)+ylim(0,maxy1)+ylab('MCM')+
      scale_fill_manual(name = 'type',values = water_source_col)+
      theme_bw()+ ggtitle(paste0('water sources. Scenario: ',scname[i]))+
      theme(axis.title.x = element_blank())
    
    plist2[[i]]= ggplot(final_water_plot %>% filter(scenario == scen_chk[i]),aes(x = year_all,y = value,fill = type))+
      geom_bar( stat = "identity", position = "stack") +
      facet_wrap(~country)+ylim(0,maxy2)+ylab('MCM')+
      scale_fill_manual(name = 'type',values = water_final_col)+
      theme_bw()+ ggtitle(paste0('water final use. Scenario: ',scname[i]))+
      theme(axis.title.x = element_blank())
    
    plist3[[i]]= ggplot(crop_water_use_plot_yr %>% filter(scenario == scen_chk[i]),
                        aes(x = year_all,y = value,fill = factor(type , levels = crop_order$type)))+
      geom_bar( stat = "identity", position = "stack") +
      facet_wrap(~country)+ylim(0,maxy3)+ylab('MCM')+
      scale_fill_manual(name = 'technology',values = crop_col)+
      theme_bw()+ ggtitle(paste0('Crops water use. Scenario: ',scname[i]))+
      theme(axis.title.x = element_blank())
  }
  grid.arrange(grobs=plist1)
  maxy = max((water_by_source_plot_month %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
  for (i in seq_along(scen_chk) )  { 
    par(mfrow = c(i,1))
    VAR2 = ggplot(water_by_source_plot_month %>% filter(scenario == scen_chk[i]),aes(x = month,y = value,fill = type))+
      geom_bar( stat = "identity", position = "stack") +
      facet_wrap(country~year)+ylim(0,maxy)+ylab('MCM')+
      scale_fill_manual(name = 'type',values = water_source_col)+
      theme_bw()+ ggtitle(paste0('water sources. Scenario: ',scname[i]))+
      theme(axis.title.x = element_blank())
    plot(VAR2)
  }
  grid.arrange(grobs=plist2)
  grid.arrange(grobs=plist3)

  maxy = max((crop_water_use_plot_mth %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
  for (i in seq_along(scen_chk) )  {
    par(mfrow = c(i,1))
    VAR2 = ggplot(crop_water_use_plot_mth %>% filter(scenario == scen_chk[i]),
                  aes(x = month,y = value,fill = factor(type , levels = crop_order$type)))+
      geom_bar( stat = "identity", position = "stack") +
      facet_wrap(country~year)+ylim(0,maxy)+ylab('MCM')+
      scale_fill_manual(name = 'technology',values = crop_col)+
      theme_bw()+ ggtitle(paste0('Crops water use. Scenario: ',scname[i]))+
      theme(axis.title.x = element_blank())
    plot(VAR2)
  }
  
  dev.off()
}

## FLOW RESULTS: WATER STREAMS AND ELECTRICITY TRANSMISSION

# river flow:

# get basin shape and simplify them + get centroids

extra_basin.spdf =  readOGR( paste( "P:/is-wel/indus/message_indus", 'input', sep = '/' ), paste0( 'indus_extra_basin_rip_countries'), verbose = FALSE )
extra_basin.spdf = gSimplify(extra_basin.spdf,0.05)

sbasin.spdf = gSimplify(basin.spdf,0.03)
node_data = basin.spdf@data %>% mutate(DOWN = if_else(is.na(DOWN),'SINK',as.character(DOWN) )) %>% 
  dplyr::select(PID,OUTX,OUTY,DOWN)
node_poly = broom::tidy(sbasin.spdf) %>% mutate(group = if_else(group == 4.2,4.1,as.numeric(as.character(group))) )
extra_node_poly = broom::tidy(extra_basin.spdf) %>% mutate(group = as.numeric(as.character(group)) ) %>% 
  filter(group %in% c(0.1,1.1,1.2,2.1,3.1,3.3)) %>% 
  mutate(country = if_else(group <= 1,'AFG',
         if_else(group > 1 & group <= 2,'PAK',
                 if_else(group > 2 & group <= 3,'IND','CHN'))) )

map_node_group = data.frame(node = node_data$PID, group = unique(node_poly$group))

node_poly = node_poly %>% left_join(map_node_group) %>% 
  mutate(country = gsub('_.*','',node))

node_centroids = gCentroid(basin.spdf,byid=T, id = basin.spdf$PID)
coord_sink = node_data %>% filter(PID == 'PAK_4') %>% dplyr::select(OUTX,OUTY)
centroid_ext_node = data.frame(x = c(67, 81.3, 78,   66.7),
                               y = c(35, 33,   29.5, 26),
                               node = c('AFG','CHN','IND','PAK'), stringsAsFactors = F)
cent_tidy = as.data.frame(node_centroids@coords) %>% 
  mutate(node = row.names(.)) %>% bind_rows(data.frame(x = coord_sink$OUTX, y = coord_sink$OUTY, 
                                                       node = 'SINK', stringsAsFactors = F)) %>% 
  bind_rows(centroid_ext_node)

river_flow.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$input ) %>% 
            filter( grepl('river',tec) , tec != 'hydro_river', commodity == 'freshwater'),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    dplyr::select( node, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) ) %>% dplyr::rename(node_in = node) %>% 
  mutate(node_out = gsub('.*\\|','',tec))


# INPUT OF RIVER = OUTPUT ENV FLOW + RETURN FLOWS FORM POWER PLANTS
env_flow.df = bind_rows( lapply(  scen_chk, function( sc ){

  merge( 	data.frame( res.list[[ sc ]]$output ) %>%
            filter( tec == 'environmental_flow', commodity == 'freshwater'),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>%
    filter(year_all <= 2050) %>%
    mutate( value = value.x * value.y ) %>%
  
    dplyr::select( node, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>%
    mutate( scenario = sc )

} ) ) %>% dplyr::rename(node_in = node) %>%
  left_join(node_data , by = c("node_in" = "PID")) %>% dplyr::rename(node_out= DOWN) %>%
  dplyr::select(-OUTX,-OUTY)

env_flow.coord = env_flow.df %>% 
  left_join(cent_tidy %>% dplyr::rename(x.in = x, y.in = y),
            by = c("node_in" = "node")) %>%
  left_join(cent_tidy %>% dplyr::rename(x.out = x, y.out = y),
            by = c("node_out" = "node")) %>%
  dplyr::select(tec,scenario,year_all,time,value,node_in,node_out,x.in,y.in,x.out,y.out)
  
river_flow.coord = river_flow.df %>% 
  left_join(cent_tidy %>% dplyr::rename(x.in = x, y.in = y), 
            by = c("node_in" = "node")) %>% 
  left_join(cent_tidy %>% dplyr::rename(x.out = x, y.out = y), 
            by = c("node_out" = "node")) %>% 
  dplyr::select(tec,scenario,year_all,time,value,node_in,node_out,x.in,y.in,x.out,y.out)

river_flow.shiny = river_flow.coord

# Difference of env_flow (from GAMS & from naturalized flow data) 

diff_env_flow.df = env_flow.df %>%  
  left_join(environmental_flow.df %>%   
              filter( year_all >= 2020) %>%  
              rename(value = value1)) %>%  
  mutate( diff = ((value1 - value) / value1) * 100) %>%  
  
#Plotting diff_env_flow map for all nodes (not sure, if it's okay)( ranges needs to be added) 
env_plot_data = diff_env_flow.df %>%   
  group_by(node,year_all,scenario) %>%   
  summarise(value = mean(value)) %>% ungroup()  

#basin.tidy2 has the value data  
basin.tidy2 <- dplyr::left_join(basin.tidy, env_plot_data, by='node') %>%   
  filter(!is.na(value))  

# if diff (in diff_ev_flow) > 50 , color = #CC0000 ,legend
# diff > 40 , color = #FF0000
# diff > 30 , color = #FF3333
# diff > 20 , color = #FF6666
# diff > 10 , color = #FF9999
# diff <= 10, color = #FFCCCC
# need to add in following map plot 

ggplot() +  
  geom_polygon(data = basin.tidy, aes(long,lat, group=group),color='black',fill = "#f5f5f2",size = 0.1) +  
  geom_polygon(data = basin.tidy2, aes(long,lat, group=group,fill=value),alpha=0.8,color='black',size = 0.1) +  
  coord_map("azequalarea") +  
  #  facet_wrap(as.formula(paste("~", input$grouping)))+  
  facet_wrap(year_all~scenario)+  
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+  
  theme_map  

# Now,for comparison of cost with baseline0 scenario (perc_increase) (needs to be checked)

df_cost_base = df_cost %>% filter(scenario == scname[1]) %>%  
  rename(valueB = value) %>% select(-scenario)  

df_cost2 = df_cost %>% filter(scenario != scname[1]) %>% 
  left_join(df_cost_base) %>%  
  mutate(valueB = if_else(is.na(valueB),0,valueB)) %>%  
  mutate(diff = round((value - valueB),3) ) 

V3 = ggplot(df_cost2 %>% filter(!country %in% c('AFG','CHN')),aes(x = type, y = value, fill = type,alpha = cost))+ 
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) + 
  facet_wrap(country~scenario)+ylab('Billion USD per year')+ 
  scale_fill_manual(values = cost_col)+ 
  scale_alpha_discrete(range=c(0.4, 1))+ 
  theme_bw()+ theme(axis.title.x = element_blank(), 
                    axis.text.x = element_text(angle = 30)) 
plot(V3) 

df_cost3 = df_cost2 %>% group_by(country,scenario,cost) %>%  
  summarise(value = sum(value), valueB = sum(valueB)) %>% ungroup() %>%  
  mutate(diff = round((value - valueB),3) ) 

V4 = ggplot(df_cost3 %>% filter(!country %in% c('AFG','CHN')),aes(x = country, y = value, fill = cost))+ 
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) + 
  facet_wrap(~scenario)+ylab('Billion USD per year')+ 
  theme_bw()+ theme(axis.title.x = element_blank() ) 
plot(V4) 

dev.off() 


# highlight annual transfer for most important sections: boerders and outlet

river_section = data.frame(node_in = c('AFG_2','IND_1','IND_2','IND_3','IND_4','PAK_8','PAK_4'),
                           section = c('Kabul','Jhelam','Chenab','Ravi','Stulej','Upper Indus','Outflow'), stringsAsFactors = F)

annual_flows.df = river_flow.df %>% filter(node_in %in% river_section$node_in) %>% 
  left_join(river_section) %>% mutate(time = as.numeric(time)) %>% 
  left_join( ., data.frame( time = seq(1:12), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
  mutate(value = value * days *1e-3) %>%  # km^3/month, becomes yearly when summarizing
  group_by(section,year_all,scenario) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(units = 'km^3 per year')

## Canals

canal_flow.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter( grepl('canal',tec), tec != 'hydro_canal' ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    dplyr::select( node, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) ) %>% dplyr::rename(node_in = node) %>% 
  mutate(node_out = gsub('.*\\|','',tec)) %>% 
  mutate(type = gsub('_.*','',tec)) %>% 
  filter(!grepl('irr_',tec))


canal_flow.coord = canal_flow.df %>% 
  left_join(cent_tidy %>% dplyr::rename(x.in = x, y.in = y), 
            by = c("node_in" = "node")) %>% 
  left_join(cent_tidy %>% dplyr::rename(x.out = x, y.out = y), 
            by = c("node_out" = "node")) %>% 
  dplyr::select(tec,type,scenario,year_all,time,value,node_in,node_out,x.in,y.in,x.out,y.out)

canal_flow.shiny = canal_flow.coord
## Transmission lines

trans_flow.coord = transmission_flow.df %>% filter(mode == 1) %>% 
  bind_rows(transmission_flow.df %>% filter(mode == 2) %>% 
              rename(node_out = node_in, node_in = node_out) %>%
              dplyr::select( node_in, tec, year_all, mode, time, value,scenario ,node_out )) %>% 
  left_join(cent_tidy %>% dplyr::rename(x.in = x, y.in = y), 
            by = c("node_in" = "node")) %>% 
  left_join(cent_tidy %>% dplyr::rename(x.out = x, y.out = y), 
            by = c("node_out" = "node")) %>% 
  dplyr::select(tec,scenario,mode,year_all,time,value,node_in,node_out,x.in,y.in,x.out,y.out) %>% 
  mutate(value = value *30*24*1e-6,
         unit = 'TWh')

trans_flow.shiny = trans_flow.coord
# PLOTTING
if (shiny_mode){} else {
  
  ocean_poly = data.frame(long = c(65,65,71,71), lat = c(26,20,20,26),
                          order = c(1,2,3,4), hole = FALSE , piece = 1,
                          group = 0.1, id = 0 ) #trip out ocean with other countries, does't work well
  
  #no transparency of filling, yes very light colors
  theme_map_flow = theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank()) 
  country_colors <- c(AFG = "#A2A7D2", CHN = "#F4AA91", IND ="#FAD4A1", PAK = "#C0DDAE")
  
  river_plot = river_flow.coord %>% 
    group_by_("tec","scenario","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out") %>% 
    summarise(value = sum(value)) 
  
  canal_plot = canal_flow.coord %>% 
    group_by_("tec",'type' ,"scenario","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out") %>% 
    summarise(value = sum(value)) 
  
  trans_plot = trans_flow.coord %>% 
    group_by_("tec" ,"scenario","mode","year_all" ,"node_in", "node_out","x.in","y.in","x.out","y.out") %>% 
    summarise(value = sum(value))

  pdf( 'water_electricity_flow_maps.pdf', onefile = TRUE) 
  # river flows
  max_size = max(river_plot$value)
  for (i in seq_along(scen_chk) )  { 
    par(mfrow = c(i,1))
    VAR1 = ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50')+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.5)+
      geom_curve(data = river_plot %>% filter(scenario == scen_chk[i]),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
                 color = 'deepskyblue3', lineend = c('butt'),curvature = -0.1 )+
      # geom_curve(data = test_env_flow,aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),color = 'darkorchid',
      #              arrow = arrow(length = unit(test_plot$value * 0.0002, "npc"),type = 'open'),
      #              lineend = c('butt'), linejoin = c('round'),curvature = -0.2 )+
      facet_wrap(~year_all)+
      ggtitle(paste0('Annual river flows. Scenario: ',scen_chk[i]) )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      scale_size(name = 'MCM',range = c(0.8, 2),limits = c(0,max_size))+
      scale_fill_manual(values=country_colors)+
      theme_bw() + theme_map_flow
  plot(VAR1)
  }
  
  # canal flows
  max_size = max(canal_plot$value)
  for (i in seq_along(scen_chk) )  { 
    par(mfrow = c(i,1))
    VAR1 = ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50', size = 0.5)+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50', size = 0.5)+
      geom_curve(data = canal_plot %>% filter(scenario == scen_chk[i]),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value,colour = type),
                 arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),curvature = -0.2,
                 lineend = c('butt') )+
      facet_wrap(~year_all)+
      ggtitle(paste0('Annual canal flows. Scenario: ',scen_chk[i]) )+
      # geom_curve(data = canal_test_plot %>% filter(grepl('lined',tec)),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
      #            color = 'deepskyblue4', arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
      #            lineend = c('butt'), linejoin = c('round'),curvature = 0.1 )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      scale_size(name = 'MCM',range = c(0.3, 2),limits = c(0,max_size))+
      scale_fill_manual(values=country_colors)+
      scale_color_manual(values = c(conv = 'brown3',lined = 'deepskyblue4'))+
      theme_bw()+theme_map_flow
    plot(VAR1)
  }
  
  # electricity transmission plot
  max_size = max(trans_plot$value)
  for (i in seq_along(scen_chk) )  { 
    par(mfrow = c(i,1))
    VAR1 = ggplot()+
      geom_polygon(data = ocean_poly,aes(long,lat,group = group),colour = 'lightblue',fill = 'lightblue')+
      geom_polygon(data = extra_node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.1)+ # write country name. make country color, 
      geom_polygon(data = node_poly,aes(long,lat,group = group,fill = country),colour = 'grey50',size = 0.1)+
      geom_point(data = cent_tidy,aes(x,y))+
      geom_curve(data = trans_plot%>% filter(scenario == scen_chk[i], mode == 1),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),
                 color = 'brown2', arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
                 lineend = c('butt'), linejoin = c('round'),curvature = -0.3 )+
      geom_curve(data = trans_plot %>% filter(scenario == scen_chk[i],mode == 2),aes(x = x.in,y = y.in,xend = x.out, yend = y.out,size = value),color = 'brown2',
                 arrow = arrow(length = unit( 0.02, "npc"),type = 'open'),
                 lineend = c('butt'), linejoin = c('round'),curvature = 0.1 )+
      coord_cartesian(xlim = c(66,82), ylim = c(24,37), expand = TRUE,
                      default = FALSE, clip = "on")+
      facet_wrap(~year_all)+
      ggtitle(paste0('Annual electricity transmission flows. Scenario: ',scen_chk[i]) )+
      scale_size(name = 'TWh',range = c(0.3, 2),limits = c(0,max_size))+
      scale_fill_manual(values=country_colors)+
      theme_bw()+theme_map_flow
    plot(VAR1)
  }
  dev.off()
}

## CALCULATE RESULTING YIELD PER NODE/CROP

# in Mha
area_by_crop.calc = area_by_crop.df %>% group_by(node,year_all,time,scenario,type) %>% 
  summarise(value = sum(value))

area_crop_2020 = area_by_crop.calc %>% filter(year_all == 2020 ) %>% ungroup() %>% 
  group_by_("node"  ,   "scenario", "type"   ) %>% 
  summarise(value = sum(value)) %>% ungroup() 
  

#in kton
crop_production.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  merge( 	data.frame( res.list[[ sc ]]$output ) %>% 
            filter( grepl( '_yield',commodity), level == 'raw' ),
          data.frame( res.list[[ sc ]]$ACT ),
          by = c( 'node', 'tec', 'vintage', 'year_all', 'mode', 'time' ) ) %>% 
    filter(year_all <= 2050) %>% 
    mutate( value = value.x * value.y ) %>%
    select( node, tec, year_all, time, value ) %>%
    #    group_by( node, year_all ) %>% summarise(value =  sum( value ) )  %>% 
    mutate( scenario = sc )
  
} ) )

crop_production.df = crop_production.df %>% left_join(tech_type_crop.df)

crop_production.calc = crop_production.df %>% 
  group_by(node,year_all,time,scenario,type) %>% 
  summarise(value = sum(value)) %>% 
  left_join(area_by_crop.calc, by = c("node", "year_all", "time", "scenario", "type")) %>% 
  mutate(value = value.x/value.y/1000) %>% ungroup()

crop_prod_avg = crop_production.calc %>% group_by(node,scenario,type) %>% 
  summarise(value = mean(value))

prod_crop_2020 = crop_production.calc %>% filter(year_all == 2020 ) %>% ungroup() %>% 
  group_by_("node"  ,   "scenario", "type"   ) %>% 
  summarise(value = sum(value)) %>% ungroup() %>% 
  left_join(area_crop_2020, by = c('node','scenario','type')) %>% 
  mutate(value = value.x/value.y/1000) %>% ungroup()

## COMMODITY COSTS/PRICES ####

commodity_price.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$PRICE_COMMODITY ) %>% 
  mutate( scenario = sc ,
          country = gsub('_.*','',node) ,
          year_all = as.numeric(year_all) ) %>% # sometime time is 'year' therefore I do not transofr in numeric here
    filter(year_all <= 2050)
  
} ) )

country_colors <- c(AFG = "chartreuse4", CHN = "pink", IND ="red", PAK = "blue")

# Land sector
agriculture_land_cost.df = commodity_price.df %>% 
  filter(commodity == 'crop_land') %>% 
  mutate(unit = 'USD/ha') %>% 
  group_by(scenario,country,year_all,unit) %>% 
  summarise(value = -mean(value))

crop_product_prices.df = commodity_price.df %>% 
  filter(level == 'raw',
         value >= 0) %>% 
  mutate(unit = 'USD/ton',
         commodity = gsub('_yield','',commodity),
         value = value*1000) %>% #from M$/kton = k$/ton -> $/ton) 
  group_by(scenario,country,commodity,year_all,time,unit) %>% 
  summarise(value = mean(value))

# Electricity at final level

final_elec_cost.df = commodity_price.df %>% 
  filter(commodity == 'electricity' & grepl('_final',level),
         value >= 0) %>% 
  mutate(value = (value *1e3 /30 / 24), # ACT is in MWmonth -> *30 d/m * 24 h/d -> MWh
         unit = 'USD/kWh',    
         time = as.numeric(time)) %>% 
  group_by(scenario,country,year_all,time,unit) %>% 
  summarise(value = mean(value))

biofuel_cost.df = commodity_price.df %>% 
  filter(commodity == 'biomass' ,
         value >= 0) %>% 
  mutate(value = (value *1000), # from M$/kton = k$/ton -> $/ton)
         unit = 'USD/ton',    
         time = as.numeric(time)) %>% 
  group_by(scenario,country,level,year_all,time,unit) %>% 
  summarise(value = mean(value)) %>% ungroup()

biofuel_cost_plot= ggplot(biofuel_cost.df,aes(x = time,y = value,color = country))+
  geom_line(size = 0.8)+theme_bw()+
  facet_wrap(level~year_all,nrow = 2)+
  scale_color_manual(values=country_colors)+
  ylab('USD/ton') + xlab('month') + ggtitle('bio-fuel costs')+
  scale_x_continuous(breaks = c(1,3,6,9,12))
# doubts on these biomass costs, it's lower than the variable cost, 
# maybe due to the residue price, which is soimetime negative?

# surface freshwater, groundwater and wastewater costs
# groundwater pumping is now free? or included in the gw_diversion tecchnologies?
# if so, we cannot distinguish, if goes into the freshwater final cost

final_water_cost.df = commodity_price.df %>% 
  filter(commodity %in% c('wastewater','freshwater') & grepl('_final',level),
         value >= 0) %>% 
  mutate(value = (value /30), # from M CM/day to monthly amount? I am not sure
         unit = 'USD/m^3 (per month)',    
         time = as.numeric(time)) %>% 
  group_by(scenario,country,commodity,year_all,time,unit) %>% 
  summarise(value = mean(value)) %>% ungroup()

if (shiny_mode){} else {
  
  pdf( 'commodity_costs.pdf', onefile = TRUE)
  
  max_size = max(agriculture_land_cost.df$value)
  max_size2 = max(crop_product_prices.df$value)
  for (i in seq_along(scen_chk) )  {
    # par(mfrow = c(i,1))
    land_cost_plot = ggplot(agriculture_land_cost.df %>% filter(scenario == scen_chk[i]),aes(x = year_all,y = value,color = country))+
      geom_line()+theme_bw()+
      scale_color_manual(values=country_colors)+
      ylab(unique(agriculture_land_cost.df$unit))+ ggtitle(paste0('Land costs. Scenario: ',scen_chk[i]))
    
    crop_cost_plot = ggplot(crop_product_prices.df %>% filter(scenario == scen_chk[i]),aes(x = year_all,y = value,color = country))+
      geom_line(size = 0.8)+theme_bw()+
      facet_wrap(~commodity)+
      scale_color_manual(values=country_colors)+
      ylab('USD/ton')+ ggtitle(paste0('products costs. Scenario: ',scen_chk[i]))

    two_land_plot = grid.arrange(land_cost_plot,crop_cost_plot, heights = c(0.4,0.6))
    
  }
  
  max_size = max(final_elec_cost.df$value)
  for (i in seq_along(scen_chk) )  {

    elec_cost_plot = ggplot(final_elec_cost.df %>% filter(scenario == scen_chk[i]),aes(x = time,y = value,color = country))+
      geom_line(size = 0.8)+theme_bw()+
      facet_wrap(~year_all)+
      scale_color_manual(values=country_colors)+
      ylab('USD/kWh') + xlab('month') + ggtitle(paste0('electricity costs. Scenario: ',scen_chk[i]))+
      scale_x_continuous(breaks = c(1,3,6,9,12))

    plot(elec_cost_plot)
  }
  
  max_size = max(final_water_cost.df$value)
  for (i in seq_along(scen_chk) )  {
    # par(mfrow = c(i,1))
    water_cost_plot = ggplot(final_water_cost.df %>% filter(scenario == scen_chk[i]),aes(x = time,y = value,color = country))+
      geom_line(size = 0.8)+theme_bw()+
      facet_wrap(commodity~year_all,ncol = 4)+
      scale_color_manual(values=country_colors)+
      ylab('USD/m^3') + xlab('month') + ggtitle(paste0('water costs. Scenario: ',scen_chk[i]))+
      scale_x_continuous(breaks = c(1,3,6,9,12))

    avg_indus_water_cost_plot = ggplot(final_water_cost.df %>%
                                         group_by(scenario,commodity,year_all,time,unit) %>%
                                         summarise(value = mean(value)) %>%
                                                     filter(scenario == scen_chk[i])
                                       ,aes(x = time,y = value,color = as.factor(year_all) ))+
      geom_line(size = 0.8)+theme_bw()+
      facet_wrap(~commodity)+
      ylab('USD/m^3') + xlab('month') + ggtitle(paste0('water costs for whole region. Scenario: ',scen_chk[i]))+
      scale_x_continuous(breaks = c(1,3,6,9,12))

    two_wat_plot = grid.arrange(water_cost_plot,avg_indus_water_cost_plot, heights = c(0.7,0.3))
    
  }
  dev.off()
}

toc()

