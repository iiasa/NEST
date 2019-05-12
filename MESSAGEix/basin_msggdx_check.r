# check trends and prinf pdfs in P:/is-wel/indus/message_indus/input/check
library(ggplot2)
#####
# check total runoff
runoff_by_admin.df = inflow.df %>% 
  left_join( ., data.frame( time = seq(1:12), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
  left_join( ., data.frame( node = node, country = unlist( strsplit( node, '_' ) )[ seq( 1, 2*length(node), by = 2 ) ] ) ) %>%
  group_by(year_all, time, country) %>% summarise( value = sum( abs( value * days )/1e3 ) ) %>%
  ungroup() %>% data.frame() 
runoff_by_admin.df = rbind( runoff_by_admin.df, runoff_by_admin.df %>%
                              group_by(year_all, time) %>% summarise( value = sum( value ) ) %>%
                              mutate( country = 'Indus' ) %>% ungroup() %>% data.frame() %>% select( year_all, time, country, value ) ) 
pdf( 'input/check/runoff.pdf', width = 7, height = 6 )
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( runoff_by_admin.df$country ) ){
  runoff_by_admin.df %>% filter( country == ccc ) %>%
    select( year_all, time, value ) %>%
    reshape( ., idvar = 'time', timevar = 'year_all', direction = 'wide' ) %>%
    select( -time ) %>%
    as.matrix() %>%
    matplot( ., type = 'l', ylab = 'cubic kilometers', xlab = 'month of year', main = paste0( ccc, ' - Runoff' ), lty = 1, col = c('black','cyan','blue','purple','brown','red')  )
}
plot.new()
legend( 'left', legend = c(2015,seq(2020,2060,by=10) ), title = 'Decade', bty = 'n', cex = 1.2, seg.len = 3, col = c('black','cyan','blue','purple','brown','red'), lty = 1 )	
dev.off()
###

###
# Check municipal and manufacturing water demands
demand_by_admin.df = demand.df %>% filter( level %in% c('urban_final','rural_final'), commodity == c('freshwater') ) %>%
  left_join( ., data.frame( time = as.character(c(seq(1:12)) ), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
  left_join( ., data.frame( node = node, country = unlist( strsplit( node, '_' ) )[ seq( 1, 2*length(node), by = 2 ) ] ) ) %>%
  group_by(year_all, time, country) %>% summarise( value = sum( abs( value * days )/1e3 ) ) %>%
  ungroup() %>% data.frame() 
demand_by_admin.df = rbind( demand_by_admin.df, demand_by_admin.df %>%
                              group_by(year_all, time) %>% summarise( value = sum( value ) ) %>%
                              mutate( country = 'Indus' ) %>% ungroup() %>% data.frame() %>% select( year_all, time, country, value ) ) 	
pdf( 'input/check/nonirrigation_water_demand.pdf', width = 7, height = 6 ) 
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( demand_by_admin.df$country ) ){
  demand_by_admin.df %>% filter( country == ccc ) %>%
    select( year_all, time, value ) %>%
    reshape( ., idvar = 'time', timevar = 'year_all', direction = 'wide' ) %>%
    select( -time ) %>%
    as.matrix() %>%
    matplot( ., type = 'l', ylab = 'cubic kilometers', xlab = 'month of year', main = paste0( ccc, ' - Withdrawal' ), lty = 1, col = c('black','cyan','blue','purple','brown','red')  )
}
plot.new()
legend( 'left', legend = c(2015,seq(2020,2060,by=10) ), title = 'Decade', bty = 'n', cex = 1.2, seg.len = 3, col = c('black','cyan','blue','purple','brown','red'), lty = 1 )	
dev.off()	

###
# Check municipal and manufacturing return flow
return_by_admin.df = demand.df %>% filter( level %in% c('urban_final','rural_final'), commodity == c('wastewater') ) %>%
  left_join( ., data.frame( time = as.character(c(seq(1:12)) ), days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) ) ) %>%
  left_join( ., data.frame( node = node, country = unlist( strsplit( node, '_' ) )[ seq( 1, 2*length(node), by = 2 ) ] ) ) %>%
  group_by(year_all, time, country) %>% summarise( value = sum( abs( value * days )/1e3 ) ) %>%
  ungroup() %>% data.frame() 
return_by_admin.df = rbind( return_by_admin.df, return_by_admin.df %>%
                              group_by(year_all, time) %>% summarise( value = sum( value ) ) %>%
                              mutate( country = 'Indus' ) %>% ungroup() %>% data.frame() %>% select( year_all, time, country, value ) ) 	
pdf( 'input/check/nonirrigation_wastewater_demand.pdf', width = 7, height = 6 ) 
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( return_by_admin.df$country ) ){
  return_by_admin.df %>% filter( country == ccc ) %>%
    select( year_all, time, value ) %>%
    reshape( ., idvar = 'time', timevar = 'year_all', direction = 'wide' ) %>%
    select( -time ) %>%
    as.matrix() %>%
    matplot( ., type = 'l', ylab = 'cubic kilometers', xlab = 'month of year', main = paste0( ccc, ' - Return' ), lty = 1, col = c('black','cyan','blue','purple','brown','red')  )
}
plot.new()
legend( 'left', legend = c(2015,seq(2020,2060,by=10) ), title = 'Decade', bty = 'n', cex = 1.2, seg.len = 3, col = c('black','cyan','blue','purple','brown','red'), lty = 1 )	
dev.off()	

###
# Check municipal / manufacturing consumption
consumption_by_admin.df	= left_join( demand_by_admin.df %>% rename( dem = value ), return_by_admin.df %>% rename( ret = value ) ) %>% mutate( value = dem - ret ) %>% select( -dem, -ret )
pdf( 'input/check/nonirrigation_consumption_demand.pdf', width = 7, height = 6 ) 
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par(mar=c(4,4,3,3), oma = c(1,1,1,1))
for( ccc in unique( consumption_by_admin.df$country ) ){
  return_by_admin.df %>% filter( country == ccc ) %>%
    select( year_all, time, value ) %>%
    reshape( ., idvar = 'time', timevar = 'year_all', direction = 'wide' ) %>%
    select( -time ) %>%
    as.matrix() %>%
    matplot( ., type = 'l', ylab = 'cubic kilometers', xlab = 'month of year', main = paste0( ccc, ' - Consumption' ), lty = 1, col = c('black','cyan','blue','purple','brown','red')  )
}
plot.new()
legend( 'left', legend = c(2015,seq(2020,2060,by=10) ), title = 'Decade', bty = 'n', cex = 1.2, seg.len = 3, col = c('black','cyan','blue','purple','brown','red'), lty = 1 )	
dev.off()	

###
# Check crop yield demand
yield_by_admin.df0 = demand.df %>% 
  filter( level == 'raw' ) %>% 
  mutate( commodity = unlist( strsplit( commodity, '_' ) )[seq(1,2*length(commodity),by=2)]) %>% 
  select(node, commodity, year_all, value)
yield_by_admin.df = rbind( yield_by_admin.df0, yield_by_admin.df0 %>% 
                             group_by( commodity, year_all ) %>% 
                             summarise( value = sum( value ) ) %>% 
                             ungroup() %>% data.frame() %>%
                             mutate( node = 'Indus' ) %>% 
                             select(node, commodity, year_all, value) ) %>% rename( country = node )

ggplot()+
  geom_line(data = yield_by_admin.df,aes(year_all,value/1000,color = commodity))+
  facet_wrap(~country)+
  ggtitle('Yield demand')+ylab('Mton product demand')+
  theme_bw()
pdf( 'input/check/yield_demand.pdf', width = 7, height = 6 ) 
cols = data.frame( row.names = c('cotton', 'fodder', 'pulses', 'rice', 'sugarcane', 'wheat', 'all'), col = c('green', 'blue','purple','brown','orange', 'tan', 'black'))
p1 = layout( matrix( c(1,2,3,4,5,6), 2,3, byrow=TRUE ), widths=c(0.24,0.24,0.24), heights=c(0.4,0.4) )
par( mar=c(4,4,3,3), oma = c(1,1,1,1) )
for( ccc in unique( yield_by_admin.df$country ) ){
  df = yield_by_admin.df %>% filter( country == ccc ) %>%
    filter( year_all %in% c(2015, seq(2020, 2060, by = 10) ) ) %>%
    select( year_all, commodity, value ) %>%
    mutate( value = value / 1e3 ) %>%
    reshape( ., idvar = 'commodity', timevar = 'year_all', direction = 'wide' ) 
  barplot( as.matrix( df %>% select( -commodity ) ), ylab = 'million tons', names.arg = c(2015, seq(2020, 2060, by = 10) ), las = 2, main = paste0( ccc, ' - Yield' ), col = as.character(cols[ df$commodity, ]) )
  abline( h = 0 )
}
plot.new()
legend( 'left', legend = row.names(cols), title = 'Crop', bty = 'n', cex = 1.25, fill = as.character(unlist(cols)) )	
dev.off()
###

#### check water supply>demand in 2015
## Run load_inputs first

inflow2015 = inflow.df %>% filter(year_all == 2015) %>% 
  rename(inflow = value) %>% mutate(time = as.character(time)) %>% 
  select(node,year_all,time,inflow)

return2015 = demand.df %>% filter(commodity == 'wastewater', year_all == 2015) %>% 
  group_by(node, year_all , time) %>% 
  summarize(return = sum(value)) %>% 
  select(node,year_all,time,return)

# max groundwater that can be used


# demand, rural and urban
UR_demand2015 = demand.df %>% filter(commodity == 'freshwater', level %in% c('urban_final','rural_final'), year_all == 2015) %>% 
  group_by(node,year_all,time) %>% summarise(URdem = sum(value)) %>% 
  select(node,year_all,time,URdem)

#agriculture missing
total_node_time = inflow2015 %>% left_join(return2015) %>% 
  left_join(UR_demand2015) %>% 
  mutate(surplus = inflow + return + URdem)

#water scarcity without accounting for agriculture
total_node_time %>% filter(surplus >= 0) %>% 
  ggplot()+
  geom_point(aes(x= node,y = surplus,shape = time))+
  ggtitle('Water scarcity, no river support, no agriculture')+
  ylab('decifit in demand [Mm3]')+
  theme_bw()

downs = basin.spdf@data %>% select(PID,DOWN) %>% 
  rename(node = PID, node_dw = DOWN) %>% mutate(node_dw = if_else(is.na(node_dw),'SINK',as.character(node_dw)))
# it is quite tricky to calculate the max available water for each node, considering possible upstream flow from river.
# need to map upstream nodes first

#calculate 


## Crop Yields per country
countr_avg_yield = crop_input_data.df %>% 
  filter(par %in% c('irrigation_yield','rain-fed_yield')) %>% 
  mutate(country = gsub('_.*','',node) ) %>% 
  group_by(crop,par,country,unit) %>% 
  summarise(value = mean (value)) %>% 
  ungroup()

new_residue = countr_avg_yield %>% 
  group_by(crop,par) %>% summarise(value = mean(value)) %>% 
  spread(par,value) %>% mutate(ratio = `rain-fed_yield`/irrigation_yield) %>% 
  select(crop,ratio) %>% left_join(residue_data %>% select(crop,res_yield)) %>% 
  rename(irrigation_yield = res_yield) %>% mutate(`rain-fed_yield` = irrigation_yield*ratio) %>% 
  gather(key ='par',value = 'value',3:4) %>% select(-ratio)

p1 = ggplot() +
  geom_bar(data = countr_avg_yield,aes(x = crop,y = value/1000,fill = country),
           stat = 'identity',position = position_dodge2(width = 1,preserve = c("single"),padding = 0.1 ) )+ 
  facet_wrap(~par,nrow = 2)+
  geom_point(data = new_residue,aes(x = crop,y = value/1000, color = ' '))+
  ylab('Average highest country crop yield [t/ha]')+
  scale_fill_brewer(type = 'qual',palette = 2)+
  scale_color_manual(name = "residue", values = c(" " = "black"))+
  theme_bw()

pdf( 'input/check/avg_crop_yields.pdf', width = 7, height = 6 ) 
p1 
dev.off()

## check area requirements to meet crop demand
calc_land_crop = countr_avg_yield %>% filter(par == 'irrigation_yield') %>% rename(node = country,yield = value) %>% select(-par) %>% 
  left_join(yield_by_admin.df0 %>% filter(year_all == 2020) %>% select(-year_all) %>%  rename(crop = commodity,demand = value)) %>% 
  mutate(area_req = demand/yield) %>% 
  mutate(area_req = if_else(is.na(area_req),0,area_req))

land_av = read.csv( 'input/land_availability_map.csv', stringsAsFactors=FALSE ) %>% 
  mutate(node = gsub('_.*','',node)) %>% group_by(node,units) %>% 
  summarise(area_av = sum(value))

check_land_av = calc_land_crop %>% group_by(node,unit) %>% 
  summarise(area_req = sum(area_req)) %>% 
  left_join(land_av) %>% 
  mutate(LACK_LAND = area_req - area_av)

## check production trends and projections

maps_path = paste0(getwd(),'/input/land_maps_crop_yields')
national_trend.path = path.expand(paste0(maps_path,'/FAO_hist_national_prod'))
nat_trend.files = list.files(path = national_trend.path,pattern = '.csv')

prod_hist = NULL
for (jj in seq_along(nat_trend.files)){
  
  tmp_national = read.csv(paste0(national_trend.path,'/',nat_trend.files[jj])) %>% 
    filter(Unit == 'tonnes', Year <= 2015 ) %>% 
    mutate(Value = Value * 1E-6, Unit = 'mton') %>% 
    left_join(data.frame(Area = c('Pakistan','Afghanistan','India'),country = c('PAK','AFG','IND'))) %>% 
    left_join(data.frame(Item = c("Seed cotton","Sorghum", "Pulses, nes", "Rice, paddy", "Sugar cane" , "Wheat" ),crop = crop_names)) %>% 
    select(country,crop,Year,Unit,Value) %>% 
    rename(year_all = Year, unit = Unit, value = Value)
  prod_hist = bind_rows(prod_hist,tmp_national)
}
yield_by_admin.df2 = yield_by_admin.df %>% mutate(unit = 'Mton') %>% filter(!country == 'Indus') %>% rename(crop = commodity) %>% 
  select(country,crop,year_all,unit,value) %>% mutate(value = value/1000)
  # rbind(prod_hist) %>% 
  # arrange(year_all)

# from crop_yield, take prod_out, production in 2000
prod_out_sum = prod_out %>% mutate(country = gsub('_.*','',node)) %>% 
  group_by(country,crop,unit) %>% 
  summarise(value = sum(value)/1000) %>% ungroup() %>% 
  mutate(year_all = 2000,unit = 'Mton')

p11 = ggplot()+
  geom_line(data = yield_by_admin.df2 %>% filter(country == 'PAK'),aes(year_all,value,colour = crop))+
  geom_line(data = prod_hist %>% filter(country == 'PAK'),aes(year_all,value,colour = crop))+
  geom_point(data = prod_out_sum %>% filter(country == 'PAK'),aes(year_all,value,colour = crop),size = 2)+
  geom_vline(xintercept=2015)+
  ylab('PAK Production [Mton]')+
  theme_bw()+
  scale_x_continuous(limits = c(2000,2020))

p12 = ggplot()+
  geom_line(data = yield_by_admin.df2 %>% filter(country == 'IND'),aes(year_all,value,colour = crop))+
  geom_line(data = prod_hist %>% filter(country == 'IND'),aes(year_all,value,colour = crop))+
  geom_point(data = prod_out_sum %>% filter(country == 'IND'),aes(year_all,value,colour = crop),size = 2)+
  geom_vline(xintercept=2015)+
  ylab('IND Production [Mton]')+
  theme_bw()+
  scale_x_continuous(limits = c(2000,2020))

p13 = ggplot()+
  geom_line(data = yield_by_admin.df2 %>% filter(country == 'AFG'),aes(year_all,value,colour = crop))+
  geom_line(data = prod_hist %>% filter(country == 'AFG'),aes(year_all,value,colour = crop))+
  geom_point(data = prod_out_sum %>% filter(country == 'AFG'),aes(year_all,value,colour = crop),size = 2)+
  geom_vline(xintercept=2015)+
  ylab('AFG Production [Mton]')+
  theme_bw()+
  scale_x_continuous(limits = c(2000,2020))

library(gridExtra)
grid.arrange(p11,p12,p13,nrow = 3)
# the demand we impose is much higher that historical trends for pakistan, order matches
# for india it might make sense, has to be quite lover after 2015 because the portion of india we consider is small
# crop do not match in term of importance, but could be that sugarcane are mostly grown in other parts or india
# similar for AFG, prevalent crop also match

## check imports/exports
impo_exp = read.csv( "input/land_maps_crop_yields/FAO_prices/FAOSTAT_IMPORT_EXPORT.csv", stringsAsFactors=FALSE ) %>% 
  select(Country,Item,Element,Year,Unit,Value)

names(impo_exp) = tolower(names(impo_exp))

impo_exp = impo_exp %>% mutate(unit = 'kton') %>% 
  mutate(element =  gsub(' .*','',element) ) %>% 
  mutate(item =  gsub(' .*','',item) ) %>% 
  mutate(item =  gsub(',','',item) ) %>% 
  filter(year >= 1990)

p1 = ggplot(impo_exp %>% filter(item %in% c('Rice', 'Wheat')))+
  geom_line(aes(x = year,y = value,color = item, linetype = element),size = 1)+
  facet_wrap(~country)+
  scale_color_brewer(type = 'qual',palette = 2)+
  theme_bw()

p2 = ggplot(impo_exp %>% filter(!item %in% c('Rice', 'Wheat')))+
  geom_line(aes(x = year,y = value,color = item, linetype = element),size = 1)+
  facet_wrap(~country)+
  scale_color_brewer(type = 'qual',palette = 6)+
  theme_bw()

grid.arrange(p1,p2,nrow = 2)

## storage

level_dams = read_csv("P:/is-wel/indus/message_indus/input/storage/wapda_indus_reservoir_levels.csv")

# from GRanD_Technical_Documentation_v1_1, V[Mm3] = 0.678 * ( A[km2] * h[m] )^0.9229
# maximum volumes from WAPDA
monthly_level = rbind(data.frame(month = level_dams$`PERIOD DATE 2017-18`,
                                 DAM_NAME = c('Tarbela'),
                                 node = 'PAK_8',
                                 h_max = c(max(level_dams$TARBELA_LEVELS_FEET)),
                                 h_avg = c(level_dams$TARBELA_LEVELS_FEET)/mean(c(level_dams$TARBELA_LEVELS_FEET))*470,
                                 A_avg = (dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(AREA_REP))[,1],
                                 V_max = (dam_capacity.spdf@data %>% filter(DAM_NAME == 'Tarbela') %>% select(CAP_REP))[,1],
                                 stringsAsFactors = F),
                      data.frame(month = level_dams$`PERIOD DATE 2017-18`,
                                 DAM_NAME = c('Mangla'),
                                 node = 'PAK_1',
                                 h_max = c(max(level_dams$MANGLA_LEVELS_FEET)),
                                 h_avg = c(level_dams$MANGLA_LEVELS_FEET)/mean(c(level_dams$MANGLA_LEVELS_FEET))*490,
                                 A_avg = (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(AREA_REP))[,1],
                                 V_max = (dam_capacity.spdf@data %>%  filter(DAM_NAME == 'Mangla') %>%  select(CAP_REP))[,1],
                                 stringsAsFactors = F) ) %>% 
  mutate(h_avg2 = h_avg *0.3048) %>% 
  mutate(V_avg = 0.678 * ( A_avg * h_avg2 )^0.9229) %>% 
  mutate(V_avg2 = 30.684 * ( A_avg)^0.9578) %>%
  mutate(month = gsub('.*-','',month))

map_months = data.frame(month = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                        time = seq(1,12,1) ,stringsAsFactors = F)

monthly_lvl_agg = monthly_level %>% 
  left_join(map_months) %>% ungroup() %>% 
  group_by(time,DAM_NAME,node) %>% 
  summarise(h_avg2 = mean(h_avg2),V_max = mean(V_max),V_avg = mean(V_avg)) %>% ungroup() %>% 
  group_by(DAM_NAME) %>% mutate(V_avg = V_avg - (max(V_avg) - V_max)) %>% ungroup()
  

ggplot(data = monthly_lvl_agg)+
  geom_line(aes(time,V_avg))+
  geom_line(aes(time,V_max),color = 'red',linetype="dashed")+
  facet_wrap(~DAM_NAME)+ylab('Mm^3')+xlab('month')+
  scale_x_continuous(expand = c(0,0),breaks=seq(1,12,1))+
  theme_bw()
