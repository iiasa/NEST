

require(gdxtools)
require(tidyverse)

myfile = file.path('P:/is-wel/indus/message_indus/input/fuel_cost/MsgOutput_MESSAGEix-GLOBIOM_res-test_baseline_res_eq_test.gdx')

mygdx <- gdx(myfile)
fuel_price_rw <- mygdx['PRICE_COMMODITY'] 

# chose SAS as region, prices differe regionally (check the variation is the same)
fuel_price <- fuel_price_rw %>% 
  filter(commodity %in% c("gas","coal","crudeoil")) %>% 
  filter(level == "primary") %>% 
  filter(year_all >= "2020") %>% 
  filter(node == "R11_SAS") %>% 
  dplyr::select(-time,-level)

# cost variation wrt 2020
cost_variation <- fuel_price %>% 
  group_by(commodity) %>% 
  mutate(ref = fuel_price$value[fuel_price$commodity == commodity & fuel_price$year_all == "2020"]) %>% 
  mutate(variation = (value-ref)/ref)

to_save <- cost_variation %>% 
  dplyr::select(commodity,year_all,variation)

write.csv(to_save, 'P:/is-wel/indus/message_indus/input/fuel_cost/cost_variation_gams.csv',row.names = FALSE)
