***
* Solve statement workflow
* ========================
* This page is generated from the auto-documentation in ``MESSAGE/model_solve.gms``.
*
* This part of the code includes the perfect-foresight, myopic and rolling-horizon model solve statements
* including the required accounting of investment costs beyond the model horizon.
***
           
***
* Perfect-foresight model
* ~~~~~~~~~~~~~~~~~~~~~~~
* For the perfect foresight version of |MESSAGEix|, include all years in the model horizon and solve the entire model.
* This is the standard option; the GAMS global variable ``%foresight%=0`` by default.
*
* .. math::
*    \min_x OBJ = \sum_{y \in Y} OBJ_y(x_y)
***

* reset year in case it was set by MACRO to include the base year before
    year(year_all) = no ;
* include all model periods in the optimization horizon (excluding historical periods prior to 'first_period')
    year(year_all)$( model_horizon(year_all) ) = yes ;

* write a status update to the log file, solve the model
    put_utility 'log' /'+++ Solve the perfect-foresight version of MESSAGEix +++ ' ;
    Solve MESSAGE_LP using LP minimizing OBJ ;

* write model status summary
    status('perfect_foresight','modelstat') = MESSAGE_LP.modelstat ;
    status('perfect_foresight','solvestat') = MESSAGE_LP.solvestat ;
    status('perfect_foresight','resUsd')    = MESSAGE_LP.resUsd ;
    status('perfect_foresight','objEst')    = MESSAGE_LP.objEst ;
    status('perfect_foresight','objVal')    = MESSAGE_LP.objVal ;

* write an error message if model did not solve to optimality
    IF( NOT ( MESSAGE_LP.modelstat = 1 OR MESSAGE_LP.modelstat = 8 ),
        put_utility 'log' /'+++ MESSAGEix did not solve to optimality - run is aborted, no output produced! +++ ' ;
        ABORT "MESSAGEix did not solve to optimality!"
    ) ;

* rescale the dual of the emission constraint to account that the constraint is defined on the average year, not total
EMISSION_CONSTRAINT.m(node,type_emission,type_tec,type_year)$(
        EMISSION_CONSTRAINT.m(node,type_emission,type_tec,type_year) ) =
    EMISSION_CONSTRAINT.m(node,type_emission,type_tec,type_year)
        / SUM(year$( cat_year(type_year,year) ), duration_period(year) )
        * SUM(year$( map_first_period(type_year,year) ), duration_period(year) / df_period(year) * df_year(year) );

* assign auxiliary variables DEMAND, PRICE_COMMODITY and PRICE_EMISSION for integration with MACRO and reporting
    DEMAND.l(node,commodity,level,year,time) = demand_fixed(node,commodity,level,year,time) ;
    PRICE_COMMODITY.l(node,commodity,level,year,time) =
        ( COMMODITY_BALANCE_GT.m(node,commodity,level,year,time) + COMMODITY_BALANCE_LT.m(node,commodity,level,year,time) )
            / df_period(year) ;
    PRICE_EMISSION.l(node,type_emission,type_tec,year)$( SUM(type_year$( cat_year(type_year,year) ), 1 ) ) =
        SMAX(type_year$( cat_year(type_year,year) ),
               - EMISSION_CONSTRAINT.m(node,type_emission,type_tec,type_year) )
            / df_year(year) ;
    PRICE_EMISSION.l(node,type_emission,type_tec,year)$(
        PRICE_EMISSION.l(node,type_emission,type_tec,year) = - inf ) = 0 ;