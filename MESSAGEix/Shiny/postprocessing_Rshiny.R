## use gdxtools
# library("devtools")
# install_github('lolow/gdxtools')
library(broom)
indus_ix_path = Sys.getenv("INDUS_IX_PATH")
setwd(indus_ix_path)

shiny_mode = TRUE
source(paste( indus_ix_path, 'basin_msggdx_diagnostics.r', sep = '/' ) )

shiny_check_list = objects()[grep('.shiny',objects())]
# shiny_var_names = c("Activity","Capacity","New Capacity","Tot. cost by tech","Demand","Emissions","energy_for_water","input",
#                     "inv_cost","output", "water_for_energy","water_for_irrigation" )
raw_extraction = lapply(shiny_check_list, function(a){
  tmp = get(a)
  return(tmp)
})
names(raw_extraction) = gsub('.shiny','',shiny_check_list)

# calculate totals for Indus region, or groups of categories
# some have val, some have nothing
raw_extraction = lapply(raw_extraction, function(a){
    if ('node' %in% (names(a) )){
      nm = names(a)[!names(a) %in% c('node','value')]
      nv = names(a)[!names(a) %in% c('value')]
      col = names(a)
      a2 <- a %>% 
        filter(!node %in% c('World','Indus')) %>% 
        group_by_at(nm ) %>% 
        summarise(value = sum(value)) %>% ungroup() %>% 
        mutate(node = 'Indus') %>% 
        select_(.dots = names(a)) %>% 
        bind_rows(a,.) %>% 
        distinct_(.dots = nv, .keep_all = T)# in case the df already has values defined for Indus, we do not make a double
      a3 <- a %>% 
        filter(!node %in% c('World','Indus')) %>% 
        mutate(node = gsub('_.*','',node)) %>% 
        group_by_at(nv ) %>% 
        summarise(value = sum(value)) %>% ungroup() %>% 
        select_(.dots = names(a)) %>% 
        bind_rows(a2,.) %>% 
        distinct_(.dots = nv, .keep_all = T)
    } else {
      a3 = a
    }
  return(a3)
}
)

# similar thing, calculat yearly value
raw_extraction = lapply(raw_extraction, function(a){
  if ('time' %in% (names(a) )){
    nm = names(a)[!names(a) %in% c('time','value')]
    nv = names(a)[!names(a) %in% c('value')]
    col = names(a)
    a2 <- a %>% 
      filter(!time %in% c('year')) %>% 
      group_by_at(nm ) %>% 
      summarise(value = sum(value)) %>% ungroup() %>% 
      mutate(time = 'year') %>% 
      select_(.dots = names(a)) %>% 
      bind_rows(a,.) %>% 
      distinct_(.dots = nv, .keep_all = T)# in case the df already has values defined for year, we do not make a double
  } else {
    a2 = a
  }
  return(a2)
}
)

# for app_maps

#basin shapefile
basin.spdf <- readOGR( paste( "P:/is-wel/indus/message_indus", 'input', sep = '/' ), paste( 'Indus', 'bcu', sep = '_' ), verbose = FALSE )
names(basin.spdf)[1] = 'node'
basin.spdf <- basin.spdf[ 'node']

# Let's add a unique ID column to our data.
basin.spdf@data$id <- row.names(basin.spdf@data)

# Get polygons that are only in continental Europe.
# europe.clipped <-
#   rgeos::gIntersection(europe, europe.bbox, byid = TRUE, id=europe$id)

basin.tidy <- broom::tidy(basin.spdf)
basin.tidy <<- dplyr::left_join(basin.tidy, basin.spdf@data, by='id')

# for app_flows_maps

# get basin shape and simplify them + get centroids

extra_basin.spdf =  readOGR( paste( "P:/is-wel/indus/message_indus", 'input', sep = '/' ), paste0( 'indus_extra_basin_rip_countries'), verbose = FALSE )
extra_basin.spdf = gSimplify(extra_basin.spdf,0.05)
basin.spdf = readOGR( paste( "P:/is-wel/indus/message_indus", 'input', sep = '/' ), paste( basin, 'bcu', sep = '_' ), verbose = FALSE )
sbasin.spdf = gSimplify(basin.spdf,0.03)
node_data = basin.spdf@data %>% mutate(DOWN = if_else(is.na(DOWN),'SINK',as.character(DOWN) )) %>% 
  dplyr::select(PID,OUTX,OUTY,DOWN)
node_poly = broom::tidy(sbasin.spdf) %>% mutate(group = if_else(group == 4.2,4.1,as.numeric(as.character(group))) )
extra_node_poly <<- broom::tidy(extra_basin.spdf) %>% mutate(group = as.numeric(as.character(group)) ) %>% 
  filter(group %in% c(0.1,1.1,1.2,2.1,3.1,3.3)) %>% 
  mutate(country = if_else(group <= 1,'AFG',
                           if_else(group > 1 & group <= 2,'PAK',
                                   if_else(group > 2 & group <= 3,'IND','CHN'))) )

map_node_group = data.frame(node = node_data$PID, group = unique(node_poly$group))

node_poly <<- node_poly %>% left_join(map_node_group) %>% 
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


## calculate composed outcomes, I think the best is to make a gms file reporting and use $ and mapping
## from gdx or ixmp, run reporting sctipt annd use that gdx here with all_ithems

scenarios = unique(raw_extraction$CAP_NEW$scenario) # need to be kind of generic, once I define an unique way of importing model outputs
nodes = unique(raw_extraction$CAP_NEW$node)
commodity = unique(raw_extraction$demand_fixed$commodity)
tec = unique(raw_extraction$CAP_NEW$tec)
level = unique(raw_extraction$demand_fixed$level)
time = unique(raw_extraction$demand_fixed$time)
type = unique(type.shiny)
unique(c(names(raw_extraction$inv_cost),names(raw_extraction$CAP_NEW),names(raw_extraction$demand_fixed) ))

# install.packages("shiny")
