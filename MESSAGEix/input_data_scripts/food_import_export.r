# load national crop products import and export adata from FAO
# scale thes according to population of each country in the basin
# to assess import/export of the countries

setwd( 'P:/is-wel/indus/message_indus' )

				
pid_pop_gdp = read.csv( "input/PID_pop_gdp.csv", stringsAsFactors=FALSE )

#consider SSP2, million people
country_pop = read.csv('input/OECD_SSP_POP.csv', stringsAsFactors = F) %>% 
  filter(Country_Name %in% c('Afghanistan', 'India','Pakistan')) %>% 
  filter(grepl('SSP2',Scenario)) %>% 
  dplyr::select(Region,X2020) %>% rename(node = Region)

imp_exp.df0 = read.csv( "input/land_maps_crop_yields/FAO_prices/FAOSTAT_IMPORT_EXPORT.csv", stringsAsFactors=FALSE ) %>% 
  dplyr::select(Country,Element,Item,Year,Unit,Value)

names(imp_exp.df0) = c('country','imp_exp','item','year','unit','value')


ggplot(imp_exp.df0,aes(year,value,color = item))+
  geom_line()+
  facet_wrap(imp_exp~country)+
  theme_bw()


# consider only year > 2000 . make avg import-export per country, per crop type
# unit 1000 tonnes
imp_exp.df = imp_exp.df0 %>% filter(year >= 2000) %>% 
  mutate(imp_exp = gsub('\\ .*$','',imp_exp)) %>% 
  group_by(country,item,imp_exp) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# the current demand implementation is based on current production, qhich includes current export and imports
# therefore, a policy that decrease imports, will increase the deamand of crop products
# a policy that limit ezportation, will DECREASE the internal demand

map_country = data.frame(country = unique(imp_exp.df$country),
                         node = c('AFG','IND','PAK') )
  
map_crop = data.frame(item = unique(imp_exp.df$item),
                         crop = c('cotton',"Maize and products",'pulses','rice','wheat','fodder','sugarcane') )

# scale with population
pop_ratio = pid_pop_gdp %>% mutate(node = gsub('_.*','',PID)) %>% 
  filter(year == 2020, node != 'CHN') %>% 
  group_by(node) %>% 
  summarise(basin_pop = sum(total_pop)) %>% ungroup() %>% 
  left_join(country_pop) %>% 
  mutate(X2020 = X2020 * 1e6, ratio = basin_pop / X2020) %>% 
  dplyr::select(node,ratio)

#consider SSP2, million people
country_gdp = read.csv('input/OECD_SSP_GDP_PPP.csv', stringsAsFactors = F) %>% 
  filter(Country_Name %in% c('Afghanistan', 'India','Pakistan')) %>% 
  filter(grepl('SSP2',Scenario)) %>% 
  dplyr::select(Region,X2020) %>% rename(node = Region)

# scale with population
gdp_ratio = pid_pop_gdp %>% mutate(node = gsub('_.*','',PID)) %>% 
  filter(year == 2020, node != 'CHN') %>% 
  group_by(node) %>% 
  summarise(basin_gdp = sum(total_gdp)) %>% ungroup() %>% 
  left_join(country_gdp) %>% 
  mutate(X2020 = X2020 * 1e9, ratio = basin_gdp / X2020) %>% 
  dplyr::select(node,ratio)

imp_exp_avg = imp_exp.df %>% left_join(map_country) %>% 
  left_join(map_crop) %>% 
  dplyr::select(node,crop,imp_exp,value) %>% 
  left_join(pop_ratio) %>% 
  mutate(value = value * ratio ) %>% 
  mutate(imp_exp = tolower(imp_exp)) %>% 
  dplyr::select(-ratio)

write.csv(imp_exp_avg, paste0(getwd(), '/input/land_maps_crop_yields/FAO_prices/basin_crop_import_export.csv'), row.names = F )

