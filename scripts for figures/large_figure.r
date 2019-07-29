library(tictoc)
tic() #119.4 sec elapsed
require( gdxrrw )
require( tidyverse )
require(ggplot2)
library(gridExtra)
library(broom)
require( rgdal )
library(rgeos)

# Location of input data, uncomment
# setwd( '' )

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")
setwd(indus_ix_path)

if (!exists('shiny_mode')) shiny_mode = F

if (shiny_mode){} else {
  dir.create(file.path(indus_ix_path,paste0('plots_',sc)),showWarnings = F )
  setwd(paste0(indus_ix_path,'/plots_',sc))
  
} #endif


# Basin analyzed
basin = 'Indus'

# Get the relevant data from the gdx output files
scname = c('baseline','multiple_SDG','SDG2','SDG6','SDG7','SDG13') 
scen_chk = sapply( scname, function( sss ){ paste( 	'MSGoutput_', sss, '.gdx', sep = '' ) } )

upath = paste( indus_ix_path, '/model/output/', sep='')

# Import results from gdx
igdx( gams_path )
res.list = lapply( scen_chk, function(fpath){ # 65.97 sec elapsed
  vars = c( 'demand_fixed', 'DEMAND', 'CAP_NEW', 'CAP','historical_new_capacity', 'ACT', 'input', 'output', 'inv_cost', 'fix_cost', 'var_cost','EMISS',
            'STORAGE','STORAGE_CHG', 'bound_storage_up','bound_storage_lo')
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

shiny_vars = c( 'demand_fixed', 'CAP_NEW', 'CAP', 'ACT', 'input', 'output', 'inv_cost', 'EMISS','STORAGE' )

for (vari in shiny_vars){
  assign(paste0(vari,'.shiny'),bind_rows( lapply( scen_chk, function( sc ){ 
    
    tmp.df = res.list[[ sc ]][[ paste0(vari) ]]
    tmp.df$scenario = paste0(sc)
    return(tmp.df)
  } ) ) )
}

# run diagnostic first, with 'baseline' and 'multiple_SDG_rxp60
country_sel = c('IND','PAK')
# country_sel = c('AFG','CHN')

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

# specific plots

# Costs
df_cost = df_cost %>% 
  mutate(scenario = if_else(scenario == 'multiple_SDG_rcp60','multiple SDG',scenario))
V_ind_pak = ggplot(df_cost %>% filter(country %in% c('IND','PAK') ),aes(x = scenario,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  facet_wrap(country~cost,nrow = 1)+ylab('Billion USD per year')+
  scale_fill_manual(values = cost_col)+
  theme_bw()+ theme(axis.title.x = element_blank(),
                    legend.position = 'none')

V_afg = ggplot(df_cost %>% filter(country %in% c('AFG')) %>% mutate(value = 1000*value) ,aes(x = scenario,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  facet_wrap(country~cost,nrow = 1)+ylab('Million USD per year')+
  scale_fill_manual(values = cost_col)+
  theme_bw()+ theme(axis.title.x = element_blank(),
                    legend.position = 'none')

V_chn = ggplot(df_cost %>% filter(country %in% c('CHN') ) %>% mutate(value = 1000*value),aes(x = scenario,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  facet_wrap(country~cost)+ylab('Million USD per year')+
  scale_fill_manual(values = cost_col)+
  theme_bw()+ theme(axis.title = element_blank(),
                    legend.position = 'none')

V_indus = ggplot(df_cost %>% filter(country %in% c('Indus') ),aes(x = scenario,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack", color = 'grey40',size = 0.1) +
  facet_wrap(country~cost)+
  scale_fill_manual(values = cost_col)+
  theme_bw()+ theme(axis.title = element_blank())

leg = g_legend(V_indus)
V_indus = V_indus+theme(legend.position = 'none')
grid.arrange(V_ind_pak,V_afg,V_chn,V_indus,leg,
             layout_matrix = rbind(c(1,1,4),c(2,3,5)))

# Land
small_leg_theme =   theme(axis.title.x = element_blank(),
                          legend.position = 'none',
                          line = element_line(size = 0.2),
                          rect = element_rect(size = 0.2),
                          text = element_text(size = 9),
                          axis.text = element_text(size = 8),
                          title = element_text(size = 10),
                          strip.text = element_text(size = 8),
                          legend.key.size = unit(0.5,"lines"))

crop_area_plot_year_IP = crop_area_plot_year %>% filter(country %in% country_sel)
  # mutate(scenario = if_else(scenario == 'MSGoutput_Emission_rcp60.gdx','SDG13',scenario))
maxy = max((crop_area_plot_year_IP %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)

for (i in seq_along(scen_chk) )  {   
  
  plist[[i]] = ggplot(crop_area_plot_year_IP %>% filter(scenario == scen_chk[i]),
                      aes(x = year,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(~country)+ylim(0,maxy)+ylab('Mha')+
    scale_fill_manual(name = 'crop',values = crop_col)+
    scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
    theme_bw()+ ggtitle(paste0(scname[i]))+
    small_leg_theme
  
}

for_leg = ggplot(crop_area_plot_year_IP %>% filter(scenario == scen_chk[2]),
                 aes(x = year,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+ylim(0,maxy)+ylab('Mha')+
  scale_fill_manual(name = 'crop',values = crop_col)+
  scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
  theme_bw()+ ggtitle(paste0(scname[i]))+
  small_leg_theme+theme(legend.position = 'bottom')

leg_land = g_legend(for_leg)
V1 = grid.arrange(grobs=plist, ncol = 1)
grid.arrange(V1,leg_land, ncol = 1, heights = c(0.95,0.05))
# save 13x4

# water source, 

water_by_source_plot_year_IP = water_by_source_plot_year %>% filter(country %in% country_sel)
  # mutate(scenario = if_else(scenario == 'MSGoutput_Emission_rcp60.gdx','SDG13',scenario))

maxy1 = max((water_by_source_plot_year_IP %>%  
               group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)

for (i in seq_along(scen_chk) )  {   
  plist1[[i]]= ggplot(water_by_source_plot_year_IP %>% filter(scenario == scen_chk[i]),aes(x = year,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(~country)+ylim(0,maxy1)+ylab('MCM')+
    scale_fill_manual(name = 'type',values = water_source_col)+
    theme_bw()+ ggtitle(paste0(scname[i]))+
    theme(axis.title.x = element_blank())+small_leg_theme
}
for_leg =  ggplot(water_by_source_plot_year_IP %>% filter(scenario == scen_chk[2]),aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+ylim(0,maxy1)+ylab('MCM')+
  scale_fill_manual(name = 'type',values = water_source_col)+
  theme_bw()+ ggtitle(paste0(scname[i]))+
  theme(axis.title.x = element_blank())+small_leg_theme+theme(legend.position = 'bottom')

leg = g_legend(for_leg)
V1 = grid.arrange(grobs=plist1, ncol = 1)
grid.arrange(V1,leg, ncol = 1, heights = c(0.95,0.05))

# energy mix
en_mix_plot_IP = en_mix_plot %>% filter(country %in% country_sel)

maxy = max((en_mix_plot_IP %>% group_by(country,year,scenario) %>%
              summarise(value = sum(value)) %>% ungroup())$value)

for (i in seq_along(scen_chk) )  {   
  plist2[[i]] =  ggplot(en_mix_plot_IP %>% filter(scenario == scen_chk[i]) %>% 
               group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
             aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_bar( stat = "identity", position = "stack") +ylab('TWh')+
  facet_wrap(~country)+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxy))+
  theme_bw()+ ggtitle(paste0(scname[i]) )+
  small_leg_theme
}

V2 = ggplot(en_mix_plot_IP %>% filter(scenario == scen_chk[2]) %>% 
              group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
            aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_bar( stat = "identity", position = "stack") +ylab('TWh')+
  facet_wrap(~country)+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxy))+
  theme_bw()+ ggtitle(paste0(scname[2]) )+
  small_leg_theme+theme(legend.position = 'bottom')

leg = g_legend(V2)
V2 = grid.arrange(grobs=plist2, ncol = 1)
grid.arrange(V2,leg, ncol = 1, heights = c(0.95,0.05))
