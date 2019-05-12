
*----------------------------------------------------------------------------------------------------------------------*
* load sets and parameters from dataset gdx                                                                            *
*----------------------------------------------------------------------------------------------------------------------*

put_utility 'log' /"+++ Importing data from '%in%'... +++ " ;

* all sets and general parameters from the gdx file
$GDXIN '%in%'
$LOAD node, tec=technology, year_all=year, commodity, level, grade, mode, time, rating
$LOAD emission
$LOAD lvl_spatial, lvl_temporal, map_spatial_hierarchy, map_temporal_hierarchy
$LOAD map_node, map_time, map_commodity, map_stocks, map_tec, map_tec_time, map_tec_mode
$LOAD map_storage
$LOAD type_tec, cat_tec, type_year, cat_year, type_emission, cat_emission
$LOAD inv_tec
$LOAD shares
$LOAD full_balance
$GDXIN

Execute_load '%in%'
* general parameters
duration_period, duration_time, interestrate,
* technology technical-engineering parameters and economic costs
input, output, construction_time, technical_lifetime
capacity_factor, operation_factor, min_utilization_factor, inv_cost, fix_cost, var_cost,
* upper and lower bounds on new capacity investment, total installed capacity and activity (including mapping sets)
is_bound_new_capacity_up, is_bound_new_capacity_lo, bound_new_capacity_up, bound_new_capacity_lo,
is_bound_total_capacity_up, is_bound_total_capacity_lo, bound_total_capacity_up, bound_total_capacity_lo,
is_bound_activity_up, bound_activity_up, bound_activity_lo, bound_storage_lo, bound_storage_up, storage_loss,
* dynamic constraints on new capacity investment and activity of technologies
is_dynamic_new_capacity_up, initial_new_capacity_up, growth_new_capacity_up,
is_dynamic_new_capacity_lo, initial_new_capacity_lo, growth_new_capacity_lo,
is_dynamic_activity_up, initial_activity_up, growth_activity_up,
is_dynamic_activity_lo, initial_activity_lo, growth_activity_lo,
* parameters for reliability, flexibility and renewable potential constraints
rating_bin, reliability_factor, peak_load_factor,
* share constraints
map_shares_commodity_share,map_shares_commodity_total,share_commodity_lo 
* emission factors, bounds and taxes on emissions (including mapping sets)
historical_emission, emission_factor, emission_scaling, is_bound_emission, bound_emission, tax_emission,
* historical values of new capacity investment, activity and extraction
historical_new_capacity, historical_activity,
* energy stocks
commodity_stock,
* demand parameters
demand_fixed=demand
* fixing variables to pre-specified values
is_fixed_stock, is_fixed_new_capacity, is_fixed_capacity, is_fixed_activity,
fixed_stock, fixed_new_capacity, fixed_capacity, fixed_activity
;

*----------------------------------------------------------------------------------------------------------------------*
* assignment and computation of MESSAGE-specific auxiliary parameters                                                  *
*----------------------------------------------------------------------------------------------------------------------*

* get assignment of auxiliary parameter for period mappings and duration
$INCLUDE includes/period_parameter_assignment.gms

* compute auxiliary parameters for relative duration of subannual time periods
duration_time_rel(time,time2)$( map_time(time,time2) ) = duration_time(time2) / duration_time(time) ;

** mapping and other stuff for technologies **

* assign an additional mapping set for technologies to nodes, modes and subannual time slices (for shorter reference)
map_tec_act(node,tec,year_all,mode,time)$( map_tec_time(node,tec,year_all,time) AND
   map_tec_mode(node,tec,year_all,mode) ) = yes ;

* mapping of technology lifetime to all 'current' periods (for all non-investment technologies)
map_tec_lifetime(node,tec,year_all,year_all)$( map_tec(node,tec,year_all) ) = yes ;

* mapping of technology lifetime to all periods 'year_all' which are within the economic lifetime
* (if built in period 'vintage')
map_tec_lifetime(node,tec,vintage,year_all)$( map_tec(node,tec,vintage) AND map_tec(node,tec,year_all)
    AND map_period(vintage,year_all)
    AND duration_period_sum(vintage,year_all) < technical_lifetime(node,tec,vintage) ) = yes ;

* mapping of technology lifetime to all periods 'year_all' which were built prior to the beginning of the model horizon
map_tec_lifetime(node,tec,historical,year_all)$( map_tec(node,tec,year_all) AND map_period(historical,year_all)
    AND historical_new_capacity(node,tec,historical)
    AND duration_period_sum(historical,year_all)
        < sum(first_period, technical_lifetime(node,tec,first_period) ) ) = yes ;

* mapping of technologies to commodities and ratings
map_rating(node,inv_tec,commodity,level,rating,year_all)$(
    SUM(time, reliability_factor(node,inv_tec,year_all,commodity,level,time,rating) ) ) = yes;

* set the default capacity factor for technologies where no parameter value is provided in the input data
capacity_factor(node,tec,year_all2,year_all,time)$( map_tec_time(node,tec,year_all,time)
    AND map_tec_lifetime(node,tec,year_all2,year_all) AND NOT capacity_factor(node,tec,year_all2,year_all,time) ) = 1 ;

* assign the yearly average capacity factor (used in equation OPERATION_CONSTRAINT)
capacity_factor(node,tec,year_all2,year_all,'year') =
    sum(time$map_tec_time(node,tec,year_all,time), duration_time(time)
        * capacity_factor(node,tec,year_all2,year_all,time) ) ;

* set the default operation factor for technologies where no parameter value is provided in the input data
operation_factor(node,tec,year_all2,year_all)$( map_tec(node,tec,year_all)
    AND map_tec_lifetime(node,tec,year_all2,year_all) AND NOT operation_factor(node,tec,year_all2,year_all) ) = 1 ;

* set the emission scaling parameter to 1 if only one emission is included in a category
emission_scaling(type_emission,emission)$( cat_emission(type_emission,emission)
        and not emission_scaling(type_emission,emission) ) = 1 ;

*----------------------------------------------------------------------------------------------------------------------*
* sanity checks on the data set                                                                                        *
*----------------------------------------------------------------------------------------------------------------------*

Parameter check ;

* check whether all relevant technology/vintage/year combinations have positove input/output values assigned
*loop((node,tec,vintage,year_all)$( map_tec_lifetime(node,tec,vintage,year_all) ),
*    if ( sum( (mode,node2,commodity,level,time,time2),
*            input(node,tec,vintage,year_all,mode,node2,commodity,level,time,time2)
*            + output(node,tec,vintage,year_all,mode,node2,commodity,level,time,time2) ) eq 0 ,
*        put_utility 'log'/" Warning: No input or output not defined for '"node.tl:0"|"tec.tl:0"|"vintage.tl:0"|"year_all.tl:0"' !" ;
*    ) ;
*) ;

* check that the economic and technical lifetime are defined and consistent for all investment technologies
loop((node,inv_tec,model_horizon)$( map_tec(node,inv_tec,model_horizon) ),
    if ( technical_lifetime(node,inv_tec,model_horizon) <= 0 ,
        put_utility 'log'/" Error: Technical lifetime not defined for '"node.tl:0"|"inv_tec.tl:0"|"model_horizon.tl:0"' !" ;
        check = 1 ;
    ) ;
) ;
if (check,
    abort "There is a problem with the definition of the technical lifetime!" ;
) ;

* check for validity of temporal resolution
* loop(lvl_temporal,
*     loop(time2$( sum(time, map_temporal_hierarchy(lvl_temporal,time,time2) ) ),
*         check = 1$( sum( time$( map_temporal_hierarchy(lvl_temporal,time,time2) ),
*             duration_time(time) ) ne duration_time(time2) ) ;
*     ) ;
* ) ;
* if (check,
*     abort "There is a problem in the definition of the temporal resolution!" ;
* );
