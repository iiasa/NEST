# run basin_msggdx_diagnostics.r first, with 'baseline' and 'multiple_SDG

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

# specific plots

# Costs
df_cost = df_cost %>% 
  mutate(scenario = if_else(scenario == 'multiple_SDG_rcp60','SDG',scenario)) %>% 
  filter(!is.na(type))
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

maxy = max((crop_area_plot_year %>% group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
i = 1

V1 = ggplot(crop_area_plot_year %>% filter(scenario == scen_chk[i],country != 'AFG'),
       aes(x = year,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+ylim(0,maxy)+ylab('Mha')+
  scale_fill_manual(name = 'technology',values = crop_col)+
  scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
  theme_bw()+ ggtitle(paste0('crop area. ',scname[i]))+
  theme(axis.title.x = element_blank(),
        legend.position = 'right',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"lines"))
i = 2
V2 = ggplot(crop_area_plot_year %>% filter(scenario == scen_chk[i],country != 'AFG'),
            aes(x = year,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(~country)+ylim(0,maxy)+ylab('Mha')+
  scale_fill_manual(name = 'crop',values = crop_col)+
  scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
  theme_bw()+ ggtitle(paste0('crop area. ',gsub('_',' ',scname[i]) ))+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        
        legend.key.size = unit(0.5,"lines"))

leg = g_legend(V2)
grid.arrange(V1+theme(legend.position = 'none'),V2+theme(legend.position = 'none'),leg,
             layout_matrix = rbind(c(1,2),c(3,3)),heights = c(0.9,0.1))
#####
# montlhy plot
maxy = max((crop_area_plot_month %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
i = 1
  VAR1 = ggplot(crop_area_plot_month %>% filter(scenario == scen_chk[i], country != 'AFG'),
                aes(x = month,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('Mha')+
    scale_fill_manual(name = 'technology',values = crop_col)+
    scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
    theme_bw()+ ggtitle(paste0('crop area. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
    scale_x_continuous(breaks = c(1,3,6,9,12))+
    theme(axis.title.x = element_blank(),
          legend.position = 'bottom',
          line = element_line(size = 0.2),
          rect = element_rect(size = 0.2),
          text = element_text(size = 10),
          title = element_text(size = 10),
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"lines"))
  
  i = 2
  VAR2 = ggplot(crop_area_plot_month %>% filter(scenario == scen_chk[i], country != 'AFG'),
                aes(x = month,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
    geom_bar( stat = "identity", position = "stack") +
    facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('Mha')+
    scale_fill_manual(name = 'technology',values = crop_col)+
    scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
    theme_bw()+ ggtitle(paste0('crop area. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
    scale_x_continuous(breaks = c(1,3,6,9,12))+
    theme(axis.title.x = element_blank(),
          legend.position = 'bottom',
          line = element_line(size = 0.2),
          rect = element_rect(size = 0.2),
          text = element_text(size = 10),
          title = element_text(size = 10),
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"lines"))
  
  
grid.arrange(VAR1+theme(legend.position = 'none'),VAR2+theme(legend.position = 'none'),leg,
               layout_matrix = rbind(c(1,2),c(3,3)),heights = c(0.9,0.1))


# water source, 

small_water_theme = theme(legend.position = 'bottom',
                          line = element_line(size = 0.2),
                          rect = element_rect(size = 0.2),
                          text = element_text(size = 8),
                          title = element_text(size = 8),
                          legend.title = element_blank(),
                          legend.key.size = unit(0.5,"lines"),
                          legend.text = element_text(margin = margin(r = 2,l = 1, unit = "pt"))
)
maxyAFG = max((water_by_source_plot_year %>% filter(country %in% c('AFG')) %>% 
               group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
maxy1 = max((water_by_source_plot_year %>% filter(country %in% c('IND','PAK')) %>% 
               group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)

   
i=1  
  V1_AFG =  ggplot(water_by_source_plot_year %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                        group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                      aes(x = year,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
    scale_fill_brewer(type = 'qual',palette = 3)+
    scale_y_continuous(limits = c(0,maxyAFG))+
    facet_wrap(~country)+
    theme_bw()+ ggtitle('')+
    theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none',
                                                axis.title = element_blank())
  
  V1 =  ggplot(water_by_source_plot_year %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
                     group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                   aes(x = year,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
    facet_wrap(~country)+
    scale_fill_brewer(type = 'qual',palette = 3)+
    scale_y_continuous(limits = c(0,maxy1))+
    theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
    theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none')
  
  i=2  
  V2_AFG =  ggplot(water_by_source_plot_year %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                     group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                   aes(x = year,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
    scale_fill_brewer(type = 'qual',palette = 3)+
    scale_y_continuous(limits = c(0,maxyAFG))+
    facet_wrap(~country)+
    theme_bw()+ ggtitle('')+
    theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none',
                                                axis.title = element_blank())
  
  V2 =  ggplot(water_by_source_plot_year %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
                 group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
               aes(x = year,y = value,fill = type))+
    geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
    facet_wrap(~country)+
    scale_fill_brewer(type = 'qual',palette = 3)+
    scale_y_continuous(limits = c(0,maxy1))+
    theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
    theme(axis.title.x = element_blank())+small_water_theme
  
leg = g_legend(V2)
V2 = V2 + theme(legend.position = 'none')
grid.arrange(V1,V1_AFG,V2,V2_AFG,leg,layout_matrix = rbind(c(1,2),c(3,4),c(5,5)),widths = c(0.66,0.33),heights = c(0.45,0.45,0.1))

# by month

maxy = max((water_by_source_plot_month %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
i = 1
VAR1 = ggplot(water_by_source_plot_month %>% filter(scenario == scen_chk[i], country != 'AFG'),
              aes(x = month,y = value,fill = type ) )+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('MCM')+
  scale_fill_brewer(type = 'qual',palette = 3)+
  theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  scale_x_continuous(breaks = c(1,3,6,9,12))+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"lines"))

i = 2
VAR2 = ggplot(water_by_source_plot_month %>% filter(scenario == scen_chk[i], country != 'AFG'),
              aes(x = month,y = value,fill = type) )+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('MCM')+
  scale_fill_brewer(type = 'qual',palette = 3)+
  theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  scale_x_continuous(breaks = c(1,3,6,9,12))+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"lines"))


grid.arrange(VAR1+theme(legend.position = 'none'),VAR2+theme(legend.position = 'none'),leg,
             layout_matrix = rbind(c(1,2),c(3,3)),heights = c(0.9,0.1))

# water requirements

maxyAFG = max((final_water_plot %>% filter(country %in% c('AFG')) %>% 
                 group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)
maxy1 = max((final_water_plot %>% filter(country %in% c('IND','PAK')) %>% 
               group_by(country,year,scenario) %>% summarise(value = sum(value)) %>% ungroup())$value)


i=1  
V1_AFG =  ggplot(final_water_plot %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                   group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                 aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
  scale_fill_brewer(type = 'qual',palette = 3)+
  scale_y_continuous(limits = c(0,maxyAFG))+
  facet_wrap(~country)+
  theme_bw()+ ggtitle('')+
  theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none',
                                                                  axis.title = element_blank())

V1 =  ggplot(final_water_plot %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
               group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
             aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
  facet_wrap(~country)+
  scale_fill_brewer(type = 'qual',palette = 3)+
  scale_y_continuous(limits = c(0,maxy1))+
  theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none')

i=2  
V2_AFG =  ggplot(final_water_plot %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                   group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                 aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
  scale_fill_brewer(type = 'qual',palette = 3)+
  scale_y_continuous(limits = c(0,maxyAFG))+
  facet_wrap(~country)+
  theme_bw()+ ggtitle('')+
  theme(axis.title.x = element_blank())+small_water_theme + theme(legend.position = 'none',
                                                                  axis.title = element_blank())

V2 =  ggplot(final_water_plot %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
               group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
             aes(x = year,y = value,fill = type))+
  geom_bar( stat = "identity", position = "stack") +ylab('MCM')+
  facet_wrap(~country)+
  scale_fill_brewer(type = 'qual',palette = 3)+
  scale_y_continuous(limits = c(0,maxy1))+
  theme_bw()+ ggtitle(paste0('water sources. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  theme(axis.title.x = element_blank())+small_water_theme

leg = g_legend(V2)
V2 = V2 + theme(legend.position = 'none')
grid.arrange(V1,V1_AFG,V2,V2_AFG,leg,layout_matrix = rbind(c(1,2),c(3,4),c(5,5)),widths = c(0.66,0.33),heights = c(0.45,0.45,0.1))

# flow
trans_plot = trans_plot %>% filter(year_all %in% c(2020,2050)) %>% ungroup()
#  mutate(scenario = gsub('.*MSGoutput_','',gsub('\\.gdx.*','',scenario)) )
max_size = max(trans_plot$value)
i = 1
VB1 = ggplot()+
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
  ggtitle(paste0('Annual electricity transmission flows. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ) )+
  scale_size(name = 'TWh',range = c(0.3, 2),limits = c(0,max_size))+
  scale_fill_manual(values=country_colors)+
  theme_bw()+theme_map_flow + theme(axis.title = element_blank())

i=2
VB2 = ggplot()+
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
  ggtitle(paste0('Annual electricity transmission flows. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ) )+
  scale_size(name = 'TWh',range = c(0.3, 2),limits = c(0,max_size))+
  scale_fill_manual(values=country_colors)+
  theme_bw()+theme_map_flow + theme(axis.title = element_blank())

leg = g_legend(VB2)
grid.arrange(VB1+theme(legend.position = 'none'),VB2+theme(legend.position = 'none'),leg,
             layout_matrix = rbind(c(1,3),c(2,3)),widths = c(0.85,0.15))

# energy mix

theme_energy = theme(legend.position = 'right',
                     line = element_line(size = 0.2),
                     rect = element_rect(size = 0.2),
                     text = element_text(size = 11),
                     title = element_text(size = 10),
                     legend.title = element_blank(),
                     legend.key.size = unit(0.5,"lines"),
                     legend.background = element_blank(),
                     legend.text = element_text(margin = margin(r = 2,l = 1, unit = "pt"))  )

maxyAFG = max((en_mix_plot %>% group_by(country,year,scenario) %>% filter(country %in% c('AFG')) %>% 
                 summarise(value = sum(value)) %>% ungroup())$value)

maxy = max((en_mix_plot %>% group_by(country,year,scenario) %>% filter(country %in% c('IND','PAK')) %>% 
              summarise(value = sum(value)) %>% ungroup())$value)

i=1  
V1_AFG =  ggplot(en_mix_plot %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                   group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                 aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_area( stat = "identity", position = "stack") +ylab('TWh')+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxyAFG))+
  facet_wrap(~country)+
  theme_bw()+ ggtitle('')+
  theme(axis.title.x = element_blank())+theme_energy + theme(legend.position = 'none',
                                              axis.title = element_blank())

V1 =  ggplot(en_mix_plot %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
               group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
             aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_area( stat = "identity", position = "stack") +ylab('TWh')+
  facet_wrap(~country)+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxy))+
  theme_bw()+ ggtitle(paste0('electrcity supply. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  theme(axis.title.x = element_blank())+theme(legend.position = 'none',
                                              line = element_line(size = 0.2),
                                              rect = element_rect(size = 0.2),
                                              text = element_text(size = 11),
                                              title = element_text(size = 10),
                                              legend.title = element_blank(),
                                              legend.key.size = unit(0.5,"lines")
  )

i=2  
V2_AFG =  ggplot(en_mix_plot %>% filter(scenario == scen_chk[i],country == 'AFG') %>% 
                   group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
                 aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_area( stat = "identity", position = "stack") +ylab('TWh')+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxyAFG))+
  facet_wrap(~country)+
  theme_bw()+ ggtitle('')+
  theme(axis.title.x = element_blank())+theme_energy + theme(legend.position = 'none',
                                              axis.title = element_blank())

V2 =  ggplot(en_mix_plot %>% filter(scenario == scen_chk[i],country %in% c('IND','PAK')) %>% 
               group_by(year,type,scenario,country) %>% summarise(value = sum(value)),
             aes(x = year,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_area( stat = "identity", position = "stack") +ylab('TWh')+
  facet_wrap(~country)+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_y_continuous(limits = c(0,maxy))+
  theme_bw()+ ggtitle(paste0('electrcity supply. ',gsub('.*MSGoutput_','',gsub('\\.gdx.*','',gsub('_',' ',scname[i]))) ))+
  theme(axis.title.x = element_blank())+theme_energy

leg = g_legend(V2)
V2 = V2 + theme(legend.position = 'none')
grid.arrange(V1,V1_AFG,V2,V2_AFG,leg,layout_matrix = rbind(c(1,2,5),c(3,4,5)),widths = c(0.58,0.29,0.13))


## storage plots
theme_stg = theme(axis.title.y = element_blank())

storage_chg.df = bind_rows( lapply(  scen_chk, function( sc ){
  
  data.frame( res.list[[ sc ]]$STORAGE_CHG ) %>% 
    # mutate(node = gsub('_.*','',node) ) %>%
    # group_by(node,year_all,time) %>% 
    # summarise(value = sum(value)) %>% ungroup() %>% 
    filter(year_all<= 2050) %>% 
    mutate( scenario = sc ) %>% 
    mutate(month = as.numeric(time) ) %>% 
    select( node, scenario, year_all, month, value )
  
} ) )
node_pl = c('PAK_8')


ggplot()+
  geom_col(data = (storage_chg.df %>% filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx')),
           aes(x = month, y = value)) +
  facet_wrap(node~year_all)+
  # geom_col(data = (storage_chg.df %>% filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG_no_new_stg.gdx')),
  #          aes(x = month, y = value),fill = 'brown1') +
  facet_wrap(node~year_all)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(limits = c(-100,120))+
  xlab('month')+ylab('MCM')+
  theme_bw()


hydro_gen = energy_prod_by_source.df %>% filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>% 
  filter(tec %in% c("hydro_old" ) ) %>% 
  group_by(node,year_all,scenario,time,type) %>% 
  summarise(value = sum(value)) %>% ungroup() %>%  
  mutate(time = as.numeric(time),
         value = value / ( 30 *24* 1e-6)) 

ggplot(data = hydro_gen,aes(x = time,y = value))+
  geom_line()+
  facet_wrap(node~year_all)+
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab('month')+ylab('MWh')+
  theme_bw()

# take plot of water sources and water use, stacking lines instead of bars

final_water_use = final_water_by_use.df %>% filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>%
  filter(!grepl('demand',type)) %>% 
  group_by(node,year_all,scenario,time,type) %>% 
  summarise(value = sum(value)) %>% ungroup() %>%  
  mutate(time = as.numeric(time))

ggplot(final_water_use)+
  geom_col(aes(x = time, y = value,fill = type),size = 2)+
  facet_wrap(node~year_all)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_fill_manual(name = 'type',values = water_final_col)+
  scale_y_continuous(limits = c(0,50))+
    xlab('month')+ylab('MCM')+
  theme_bw()

wat_supply = water_by_source.df %>% filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>%
  group_by(node,year_all,scenario,time,type) %>% 
  summarise(value = sum(value)) %>% ungroup() %>%  
  mutate(time = as.numeric(time))

ggplot(wat_supply)+
  geom_line(aes(x = time, y = value,color = type),size = 1.5)+
  facet_wrap(node~year_all)+
  scale_color_manual(name = 'type',values = water_source_col)+
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab('month')+ylab('MCM')+
  theme_bw()

# internal inflow


int_runoff = demand_fixed.shiny %>% filter(level == 'river_in') %>% 
  filter(node %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>% 
  mutate(value = -value,
         time = as.numeric(time) ,
         type = 'internal runoff') %>% 
  select(node,year_all,time,scenario,type,value)

# river inflow

river_flow_in = river_flow.df %>% filter(node_out %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>% 
  mutate(time = as.numeric(time),
         type = 'inflow') %>% 
  rename(node = node_out) %>% 
  select(node,year_all,time,scenario,type,value)

ggplot(river_flow_in %>% rbind(int_runoff))+
  geom_line(aes(x = time, y = value,color = type),size = 1.5)+
  facet_wrap(node~year_all)+
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab('month')+ylab('MCM')+
  theme_bw()

# outflow

river_flowout = river_flow.df %>% filter(node_in %in% node_pl,year_all %in% c(2050),scenario == 'MSGoutput_multiple_SDG.gdx') %>% 
  mutate(time = as.numeric(time))

ggplot(river_flowout)+
  geom_line(aes(x = time, y = value),size = 1.5)+
  facet_wrap(node_in~year_all)+
  scale_x_continuous(breaks = seq(1,12,1))+
  xlab('month')+ylab('MCM')+
  theme_bw()


# seasonality in energy supply and land use
maxy = max((en_mix_plot_month %>% group_by(country,month,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
ggplot(en_mix_plot_month %>% filter(scenario == scen_chk[i],country != 'AFG'),
       aes(x = month,y = value,fill = factor(type , levels = en_source_order$type)))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('TWh')+
  scale_fill_manual(name = 'technology',values = en_source_col)+
  scale_x_continuous(breaks = c(1,3,6,9,12))+
  theme_bw()+ ggtitle(paste0('Electrcity supply. Scenario: multiple SDG'))+
  theme(axis.title.x = element_blank(),
        legend.position = 'right',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"lines"))

maxy = max((crop_area_plot_month %>% group_by(country,year,scenario,time) %>% summarise(value = sum(value)) %>% ungroup())$value)
ggplot(crop_area_plot_month %>% filter(scenario == scen_chk[i],country != 'AFG'),
       aes(x = month,y = value,fill = factor(type , levels = crop_order$type),alpha = method))+
  geom_bar( stat = "identity", position = "stack") +
  facet_wrap(country~year,nrow = 2)+ylim(0,maxy)+ylab('Mha')+
  scale_fill_manual(name = 'crop',values = crop_col)+
  scale_alpha_manual(values = c(irrigated = 1,rainfed = 0.5))+
  scale_x_continuous(breaks = c(1,3,6,9,12))+
  theme_bw()+ ggtitle(paste0('Crop area. Scenario: multiple SDG'))+
  theme(axis.title.x = element_blank(),
        legend.position = 'right',
        line = element_line(size = 0.2),
        rect = element_rect(size = 0.2),
        text = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"lines"))
