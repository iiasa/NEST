***
* Mathematical formulation (core model)
* =====================================
* This page is generated from the auto-documentation mark-up in ``MESSAGE/model_core.gms``.
*
* The |MESSAGEix| systems-optimization model minimizes total costs
* while satisfying given demand levels for commodities/services
* and considering a broad range of technical/engineering constraints and societal restrictions
* (e.g. bounds on greenhouse gas emissions, pollutants).
* Demand levels are static (i.e. non-elastic), but the demand response can be integrated by linking |MESSAGEix|
* to the single sector general-economy MACRO model included in this framework.
*
* For the complete list of sets, mappings and parameters,
* refer to the auto-documentation pages :ref:`sets_maps_def` and :ref:`parameter_def`.
***

*----------------------------------------------------------------------------------------------------------------------*
* Notation declaration                                                                                                 *
*----------------------------------------------------------------------------------------------------------------------*

***
* Notation declaration
* --------------------
* The following short notation is used in the mathematical description relative to the GAMS code:
*
* Mathematical notation of sets
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* ================================== ===================================================================================
* Math notation                      GAMS set & index notation
* ================================== ===================================================================================
* :math:`n \in N`                    node (across spatial hierarchy levels)
* :math:`y \in Y`                    year (all periods including historical and model horizon)
* :math:`y \in Y^M \subset Y`        time periods included in model horizon
* :math:`y \in Y^H \subset Y`        historical time periods (prior to first model period)
* :math:`c \in C`                    commodity
* :math:`l \in L`                    level
* :math:`g \in G`                    grade
* :math:`t \in T`                    technology (a.k.a tec)
* :math:`h \in H`                    time (subannual time periods)
* :math:`m \in M`                    mode
* :math:`e \in E`                    emission, pollutants
* :math:`s \in S`                    scenarios of land use (for land-use model emulator)
* :math:`u \in U`                    land-use types
* :math:`r \in R`                    set of generic relations (linear constraints)
* :math:`t \in T^{INV} \subseteq T`  all technologies with investment decisions and capacity constraints
* :math:`t \in T^{REN} \subseteq T`  all technologies which draw their input from the renewable level
* :math:`n \in N(\widehat{n})`       all nodes that are subnodes of node :math:`\widehat{n}`
* :math:`y \in Y(\widehat{y})`       all years mapped to the category ``type_year`` :math:`\widehat{y}`
* :math:`t \in T(\widehat{t})`       all technologies mapped to the category ``type_tec`` :math:`\widehat{t}`
* :math:`e \in E(\widehat{e})`       all emissions mapped to the category ``type_emission`` :math:`\widehat{e}`
* ================================== ===================================================================================
*
***

*----------------------------------------------------------------------------------------------------------------------*
* Variable definitions                                                                                                 *
*----------------------------------------------------------------------------------------------------------------------*

***
* Decision variables
* ^^^^^^^^^^^^^^^^^^
* ============================================= ========================================================================
* Variable                                      Explanatory text
* ============================================= ========================================================================
* :math:`OBJ \in \mathbb{R}`                    Objective value of the optimization program
* :math:`EXT_{n,c,g,y} \in \mathbb{R}_+`        Extraction of non-renewable/exhaustible resources from reserves
* :math:`STOCK_{n,c,l,y} \in \mathbb{R}_+`      Quantity in stock (storage) at start of period :math:`y`
* :math:`STOCK\_CHG_{n,c,l,y,h} \in \mathbb{R}` Input or output quantity into intertemporal commodity stock (storage)
* :math:`REN_{n,t,c,g,y,h}`                     Activity of renewable technologies per grade
* :math:`CAP\_NEW_{n,t,y} \in \mathbb{R}_+`     New installed capacity (yearly average over period duration
* :math:`CAP_{n,t,y^V,y} \in \mathbb{R}_+`      Maintained capacity in year :math:`y` of vintage :math:`y^V`
* :math:`ACT_{n,t,y^V,y,m,h} \in \mathbb{R}`    Activity of a technology (by vintage, mode, subannual time)
* :math:`CAP\_NEW\_UP_{n,t,y} \in \mathbb{R}_+` Relaxation of upper dynamic constraint on new capacity
* :math:`CAP\_NEW\_LO_{n,t,y} \in \mathbb{R}_+` Relaxation of lower dynamic constraint on new capacity
* :math:`CAP\_FIRM_{n,t,c,l,y,q}`                   Dispatchable capacity of renewable technologies per grade
* :math:`ACT\_UP_{n,t,y,h} \in \mathbb{R}_+`    Relaxation of upper dynamic constraint on activity [#ACT]_
* :math:`ACT\_LO_{n,t,y,h} \in \mathbb{R}_+`    Relaxation of lower dynamic constraint on activity [#ACT]_
* :math:`LAND_{n,s,y} \in [0,1]`                Relative share of land-use scenario (for land-use model emulator)
* :math:`EMISS_{n,e,\widehat{t},y}`             Auxiliary variable for aggregate emissions by technology type
* :math:`REL_{r,n,y} \in \mathbb{R}`            Auxiliary variable for left-hand side of relations (linear constraints)
* :math:`COMMODITY\_USE_{n,c,l,y}`              Auxiliary variable for amount of commodity used at specific level
* ============================================= ========================================================================
*
* The index :math:`y^V` is the year of construction (vintage) wherever it is necessary to
* clearly distinguish between year of construction and the year of operation.
*
* All decision variables are by year, not by (multi-year) period, except :math:`STOCK_{n,c,l,y}`.
* In particular, the new capacity variable :math:`CAP\_NEW_{n,t,y}` has to be multiplied by the number of years
* in a period :math:`|y| = duration\_period_{y}` to determine the available capacity in subsequent periods.
* This formulation gives more flexibility when it comes to using periods of different duration
* (more intuitive comparison across different periods).
*
* The current model framework allows both input or output normalized formulation.
* This will affect the parametrization, see Section :ref:`efficiency_output` for more details.
*
* .. [#ACT] The dynamic activity constraints are implemented as summed over all modes;
*    therefore, the variables for the relaxation are not indexed over the set ``mode``.
*
***

Variables
    OBJ objective value of the optimisation problem
;

Positive Variables
* commodity in inter-temporal stock
    STOCK(node,commodity,level,year_all) total quantity in intertemporal stock (storage)
* commodity in inter-temporal stock
    STORAGE(node,commodity,level,year_all,time) total quantity in intertemporal stock (storage) considering intra-year time steps (time)
* investment and capacity variables
    CAP_NEW(node,tec,year_all)       new capacity by year
    CAP(node,tec,vintage,year_all)   total installed capacity by year
    CAP_FIRM(node,tec,commodity,level,year_all,rating)   renewable firm capacity
* variables for soft relaxation of dynamic activity constraints
    CAP_NEW_UP(node,tec,year_all)    relaxation variable for dynamic constraints on new capacity (upwards)
    CAP_NEW_LO(node,tec,year_all)    relaxation variable for dynamic constraints on new capacity (downwards)
    ACT_UP(node,tec,year_all,time)   relaxation variable for dynamic constraints on activity (upwards)
    ACT_LO(node,tec,year_all,time)   relaxation variable for dynamic constraints on activity (downwards)

Variables
* intertemporal stock variables (input or output quantity into the stock)
    STOCK_CHG(node,commodity,level,year_all,time) annual input into and output from stocks of commodities
* intertemporal stock variables (input or output quantity into the stock)
    STORAGE_CHG(node,commodity,level,year_all,time) input into and output from stocks of commodities in time frame year-time
* technology activity variables (can be negative for some technologies, upper and lower bounds stated explicitly)
    ACT(node,tec,vintage,year_all,mode,time)     activity of technology by mode-year-timeperiod
* auxiliary variables for finrm-capacity formulation
    COMMODITY_USE(node,commodity,level,year_all) total amount of a commodity & level that was used or consumed
* nodal system costs over time
    COST_NODAL(node, year_all)                   system costs at the node level over time
* auxiliary variable for aggregate emissions by technology type and land-use model emulator
    EMISS(node,emission,type_tec,year_all)       aggregate emissions by technology type and land-use model emulator
;

***
* Auxiliary variables
* ^^^^^^^^^^^^^^^^^^^
* ============================================= ========================================================================
* Variable                                      Explanatory text
* ============================================= ========================================================================
* :math:`DEMAND_{n,c,l,y,h} \in \mathbb{R}`     Demand level (in equilibrium with MACRO integration)
* :math:`PRICE\_COMMODITY_{n,c,l,y,h}`          Commodity price (undiscounted marginals of COMMODITY_BALANCE constraint)
* :math:`PRICE\_EMISSION_{n,e,\widehat{t},y}`   Emission price (undiscounted marginals of EMISSION_BOUND constraint)
* :math:`COST\_NODAL\_NET_{n,y} \in \mathbb{R}` System costs at the node level net of energy trade revenues/cost
* :math:`GDP_{n,y} \in \mathbb{R}`              gross domestic product (GDP) in market exchange rates for MACRO reporting
* ============================================= ========================================================================
*
***

Variables
* auxiliary variables for demand, prices, costs and GDP (for reporting when MESSAGE is run with MACRO)
    DEMAND(node,commodity,level,year_all,time) demand
    PRICE_COMMODITY(node,commodity,level,year_all,time)  commodity price (derived from marginals of COMMODITY_BALANCE constraint)
    PRICE_EMISSION(node,type_emission,type_tec,year_all) emission price (derived from marginals of EMISSION_BOUND constraint)
    COST_NODAL_NET(node,year_all)              system costs at the node level over time including effects of energy trade
    GDP(node,year_all)                         gross domestic product (GDP) in market exchange rates for MACRO reporting
;


*----------------------------------------------------------------------------------------------------------------------*
* auxiliary bounds on activity variables (debugging mode, avoid inter-vintage arbitrage, investment technology)                                                        *
*----------------------------------------------------------------------------------------------------------------------*

* include upper and lower bounds (to avoid unbounded models)
%AUX_BOUNDS% ACT.lo(node,tec,year_all,year_all2,mode,time)$( map_tec_lifetime(node,tec,year_all,year_all2)
%AUX_BOUNDS%    AND map_tec_act(node,tec,year_all2,mode,time) ) = -%AUX_BOUND_VALUE% ;
%AUX_BOUNDS% ACT.up(node,tec,year_all,year_all2,mode,time)$( map_tec_lifetime(node,tec,year_all,year_all2)
%AUX_BOUNDS%    AND map_tec_act(node,tec,year_all2,mode,time) ) = %AUX_BOUND_VALUE% ;

* to avoid "inter-vintage arbitrage" (across different vintages of technologies), all activities that
* have positive upper bounds are assumed to be non-negative
ACT.lo(node,tec,year_all,year_all2,mode,time)$( map_tec_lifetime(node,tec,year_all,year_all2)
    AND map_tec_act(node,tec,year_all2,mode,time) AND bound_activity_lo(node,tec,year_all2,mode,time) >= 0 ) = 0 ;
* previous implementation using upper bounds
* ACT.lo(node,tec,year_all,year_all2,mode,time)$( map_tec_lifetime(node,tec,year_all,year_all2)
*    AND map_tec_act(node,tec,year_all2,mode,time)
*    AND ( NOT bound_activity_up(node,tec,year_all2,mode,time)
*        OR bound_activity_up(node,tec,year_all2,mode,time) >= 0 ) ) = 0 ;

* assume that all "investment" technologies must have non-negative activity levels
ACT.lo(node,inv_tec,year_all,year_all2,mode,time)$( map_tec_lifetime(node,inv_tec,year_all,year_all2)
    AND map_tec_act(node,inv_tec,year_all2,mode,time) ) = 0 ;

*----------------------------------------------------------------------------------------------------------------------*
* fixing variables to pre-specified values                                                                             *
*----------------------------------------------------------------------------------------------------------------------*

STOCK.fx(node,commodity,level,year_all)$( is_fixed_stock(node,commodity,level,year_all) ) =
    fixed_stock(node,commodity,level,year_all) ;
CAP_NEW.fx(node,tec,year_all)$( is_fixed_new_capacity(node,tec,year_all) ) =
    fixed_new_capacity(node,tec,year_all) ;
CAP.fx(node,tec,vintage,year_all)$( is_fixed_capacity(node,tec,vintage,year_all) ) =
    fixed_capacity(node,tec,vintage,year_all) ;
ACT.fx(node,tec,vintage,year_all,mode,time)$( is_fixed_activity(node,tec,vintage,year_all,mode,time) ) =
    fixed_activity(node,tec,vintage,year_all,mode,time) ;

*----------------------------------------------------------------------------------------------------------------------*
* auxiliary variables for debugging mode (identifying infeasibilities)                                                 *
*----------------------------------------------------------------------------------------------------------------------*

* slack variables for debugging
Positive variables
    SLACK_COMMODITY_BALANCE_UP(node,commodity,level,year_all,time) slack variable for commodity balance (upwards)
    SLACK_COMMODITY_BALANCE_LO(node,commodity,level,year_all,time) slack variable for commodity balance (downwards)
    SLACK_FULL_COMMODITY_BALANCE_UP(node,commodity,level,year_all,time) slack variable for commodity balance (upwards)
    SLACK_FULL_COMMODITY_BALANCE_LO(node,commodity,level,year_all,time) slack variable for commodity balance (downwards)
    SLACK_CAP_NEW_BOUND_UP (node,tec,year_all)        slack variable for bound on new capacity (upwards)
    SLACK_CAP_NEW_BOUND_LO (node,tec,year_all)        slack variable for bound on new capacity (downwards)
    SLACK_CAP_TOTAL_BOUND_UP (node,tec,year_all)      slack variable for upper bound on total installed capacity
    SLACK_CAP_TOTAL_BOUND_LO (node,tec,year_all)      slack variable for lower bound on total installed capacity
    SLACK_CAP_NEW_DYNAMIC_UP(node,tec,year_all)       slack variable for dynamic new capacity constraint (upwards)
    SLACK_CAP_NEW_DYNAMIC_LO(node,tec,year_all)       slack variable for dynamic new capacity constraint (downwards)
    SLACK_ACT_BOUND_UP(node,tec,year_all,mode,time)   slack variable for upper bound on activity
    SLACK_ACT_BOUND_LO(node,tec,year_all,mode,time)   slack variable for lower bound on activity
    SLACK_ACT_DYNAMIC_UP(node,tec,year_all,time)      slack variable for dynamic activity constraint relaxation (upwards)
    SLACK_ACT_DYNAMIC_LO(node,tec,year_all,time)      slack variable for dynamic activity constraint relaxation (downwards)
;

*----------------------------------------------------------------------------------------------------------------------*
* equation definitions                                                                                                 *
*----------------------------------------------------------------------------------------------------------------------*

Equations
    OBJECTIVE                       objective value of the optimisation problem
    COST_ACCOUNTING_NODAL           cost accounting at node level over time
    COMMODITY_BALANCE               commodity supply-demand balance constraint
    COMMODITY_BALANCE_FULL          full commodity supply-demand balance constraint
    STOCKS_BALANCE                  commodity inter-temporal balance of stocks
    STORAGE_BALANCE                 commodity inter-temporal balance of storage
    STORAGE_BOUND_UP                upper bound on storage level
    STORAGE_BOUND_LO                lower bound on storage level
    CAPACITY_CONSTRAINT             capacity constraint for technology (by sub-annual time slice)
    CAPACITY_MAINTENANCE            constraint for technology capacity maintainance
    OPERATION_CONSTRAINT            constraint on maximum yearly operation (scheduled down-time for maintainance)
    MIN_UTILIZATION_CONSTRAINT      constraint for minimum yearly operation (aggregated over the course of a year)
    COMMODITY_USE_LEVEL             defines the COMMODITY_USE as the amount of a commodity at a level that was consumed
    FIRM_CAPACITY_CONSTRAINT        constraint to maintaint sufficient firm (dispatchable) power generation capacity
    FIRM_CAPACITY_PROVISION         lower bound on CAP as the minimum installed capacity of each technology
    FIRM_CAPACITY_SHARE             upper bound of CAP_FIRM per rating to the size of the penetration bin of this rating
    NEW_CAPACITY_BOUND_UP           upper bound on technology capacity investment
    NEW_CAPACITY_BOUND_LO           lower bound on technology capacity investment
    TOTAL_CAPACITY_BOUND_UP         upper bound on total installed capacity
    TOTAL_CAPACITY_BOUND_LO         lower bound on total installed capacity
    NEW_CAPACITY_CONSTRAINT_UP      dynamic constraint for capacity investment (learning and spillovers upper bound)
    NEW_CAPACITY_CONSTRAINT_LO      dynamic constraint on capacity investment (lower bound)
    ACTIVITY_BOUND_UP               upper bound on activity summed over all vintages
    ACTIVITY_BOUND_LO               lower bound on activity summed over all vintages
*    SHARE_CONSTRAINT_COMMODITY_UP   upper bounds on share constraints for commodities
    SHARE_CONSTRAINT_COMMODITY_LO   lower bounds on share constraints for commodities
*    SHARE_CONSTRAINT_MODE_UP        upper bounds on share constraints for modes of a given technology
*    SHARE_CONSTRAINT_MODE_LO        lower bounds on share constraints for modes of a given technology
    ACTIVITY_CONSTRAINT_UP          dynamic constraint on the market penetration of a technology activity (upper bound)
    ACTIVITY_CONSTRAINT_LO          dynamic constraint on the market penetration of a technology activity (lower bound)
    EMISSION_EQUIVALENCE            auxiliary equation to simplify the notation of emissions
    EMISSION_CONSTRAINT             nodal-regional-global constraints on emissions (by category)
;

*----------------------------------------------------------------------------------------------------------------------*
* equation statements                                                                                                  *
*----------------------------------------------------------------------------------------------------------------------*

***
* Objective function
* ------------------
*
* The objective function of the |MESSAGEix| core model
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation OBJECTIVE
* """"""""""""""""""
*
* The objective function (of the core model) minimizes total discounted systems costs including costs for emissions,
* relaxations of dynamic constraints
*
* .. math::
*    OBJ = \sum_{n,y \in Y^{M}} discountfactor_{y} \cdot COST\_NODAL_{n,y}
*
***

OBJECTIVE..
    OBJ =E= SUM((node,year), discountfactor(year) * COST_NODAL(node, year))
;

***
* Regional system cost accounting function
* ----------------------------------------
*
* Accounting of regional system costs over time
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation COST_ACCOUNTING_NODAL
* """"""""""""""""""""""""""""""
*
* Accounting of regional systems costs over time as well as costs for emissions (taxes),
* land use (from the model land-use model emulator), relaxations of dynamic constraints,
* and linear relations.
*
* .. math::
*    COST\_NODAL_{n,y} & = \sum_{c,g} \ resource\_cost_{n,c,g,y} \cdot EXT_{n,c,g,y} \\
*      & + \sum_{t} \
*          \bigg( inv\_cost_{n,t,y} \cdot construction\_time\_factor_{n,t,y} \\
*      & \quad \quad \quad \cdot end\_of\_horizon\_factor_{n,t,y} \cdot CAP\_NEW_{n,t,y} \\[4 pt]
*      & \quad \quad + \sum_{y^V \leq y} \ fix\_cost_{n,t,y^V,y} \cdot CAP_{n,t,y^V,y} \\
*      & \quad \quad + \sum_{\substack{y^V \leq y \\ m,h}} \ var\_cost_{n,t,y^V,y,m,h} \cdot ACT_{n,t,y^V,y,m,h} \\
*      & \quad \quad + \Big( abs\_cost\_new\_capacity\_soft\_up_{n,t,y} \\
*      & \quad \quad \quad
*          + level\_cost\_new\_capacity\_soft\_up_{n,t,y} \cdot\ inv\_cost_{n,t,y}
*          \Big) \cdot CAP\_NEW\_UP_{n,t,y} \\[4pt]
*      & \quad \quad + \Big( abs\_cost\_new\_capacity\_soft\_lo_{n,t,y} \\
*      & \quad \quad \quad
*          + level\_cost\_new\_capacity\_soft\_lo_{n,t,y} \cdot\ inv\_cost_{n,t,y}
*          \Big) \cdot CAP\_NEW\_LO_{n,t,y} \\[4pt]
*      & \quad \quad + \sum_{m,h} \ \Big( abs\_cost\_activity\_soft\_up_{n,t,y,m,h} \\
*      & \quad \quad \quad
*          + level\_cost\_activity\_soft\_up_{n,t,y,m,h} \cdot\ levelized\_cost_{n,t,y,m,h}
*          \Big) \cdot ACT\_UP_{n,t,y,h} \\
*      & \quad \quad + \sum_{m,h} \ \Big( abs\_cost\_activity\_soft\_lo_{n,t,y,m,h} \\
*      & \quad \quad \quad
*          + level\_cost\_activity\_soft\_lo_{n,t,y,m,h} \cdot\ levelized\_cost_{n,t,y,m,h}
*          \Big) \cdot ACT\_LO_{n,t,y,h} \bigg) \\
*      & + \sum_{\substack{\widehat{e},\widehat{t} \\ e \in E(\widehat{e})}}
*            emission\_scaling_{\widehat{e},e} \cdot \ emission\_tax_{n,\widehat{e},\widehat{t},y}
*            \cdot EMISS_{n,e,\widehat{t},y} \\
*      & + \sum_{r} relation\_cost_{r,n,y} \cdot REL_{r,n,y}
***

COST_ACCOUNTING_NODAL(node, year)..
    COST_NODAL(node, year) =E=
* technology capacity investment, maintainance, operational cost
    + SUM((tec)$( map_tec(node,tec,year) ),
            ( inv_cost(node,tec,year) * construction_time_factor(node,tec,year)
                * end_of_horizon_factor(node,tec,year) * CAP_NEW(node,tec,year)
            + SUM(vintage$( map_tec_lifetime(node,tec,vintage,year) ),
                fix_cost(node,tec,vintage,year) * CAP(node,tec,vintage,year) ) )$( inv_tec(tec) )
            + SUM((vintage,mode,time)$( map_tec_lifetime(node,tec,vintage,year) AND map_tec_act(node,tec,year,mode,time) ),
                var_cost(node,tec,vintage,year,mode,time) * ACT(node,tec,vintage,year,mode,time) )
            )
* emission taxes (by parent node, type of technology, type of year and type of emission)
    + SUM((type_emission,emission,type_tec,type_year)$( emission_scaling(type_emission,emission)
            AND cat_year(type_year,year) ),
        emission_scaling(type_emission,emission)
        * tax_emission(node,type_emission,type_tec,type_year)
        * EMISS(node,emission,type_tec,year) )
* implementation of slack variables for constraints to aid in debugging
    + SUM((commodity,level,time)$( map_commodity(node,commodity,level,year,time) ), ( 0
%SLACK_COMMODITY_BALANCE%   + SLACK_COMMODITY_BALANCE_UP(node,commodity,level,year,time)
%SLACK_COMMODITY_BALANCE%   + SLACK_COMMODITY_BALANCE_LO(node,commodity,level,year,time)
%SLACK_FULL_COMMODITY_BALANCE%   + SLACK_FULL_COMMODITY_BALANCE_UP(node,commodity,level,year,time)
%SLACK_FULL_COMMODITY_BALANCE%   + SLACK_FULL_COMMODITY_BALANCE_LO(node,commodity,level,year,time)
        ) * 1e6 )
    + SUM((tec)$( map_tec(node,tec,year) ), ( 0
%SLACK_CAP_NEW_BOUND_UP%    + 10 * SLACK_CAP_NEW_BOUND_UP(node,tec,year)
%SLACK_CAP_NEW_BOUND_LO%    + 10 * SLACK_CAP_NEW_BOUND_LO(node,tec,year)
%SLACK_CAP_NEW_DYNAMIC_UP%  + 10 * SLACK_CAP_NEW_DYNAMIC_UP(node,tec,year)
%SLACK_CAP_NEW_DYNAMIC_LO%  + 10 * SLACK_CAP_NEW_DYNAMIC_LO(node,tec,year)
%SLACK_CAP_TOTAL_BOUND_UP%  + 10 * SLACK_CAP_TOTAL_BOUND_UP(node,tec,year)
%SLACK_CAP_TOTAL_BOUND_LO%  + 10 * SLACK_CAP_TOTAL_BOUND_LO(node,tec,year)
        ) * ABS( 1000 + inv_cost(node,tec,year) ) )
    + SUM((tec,time)$( map_tec_time(node,tec,year,time) ), ( 0
%SLACK_ACT_BOUND_UP%   + 10 * SUM(mode$( map_tec_act(node,tec,year,mode,time) ), SLACK_ACT_BOUND_UP(node,tec,year,mode,time) )
%SLACK_ACT_BOUND_LO%   + 10 * SUM(mode$( map_tec_act(node,tec,year,mode,time) ), SLACK_ACT_BOUND_LO(node,tec,year,mode,time) )
%SLACK_ACT_DYNAMIC_UP% + 10 * SLACK_ACT_DYNAMIC_UP(node,tec,year,time)
%SLACK_ACT_DYNAMIC_LO% + 10 * SLACK_ACT_DYNAMIC_LO(node,tec,year,time)
        ) * ( 1e8
            + ABS( SUM(mode$map_tec_act(node,tec,year,mode,time), var_cost(node,tec,year,year,mode,time) ) )
            + fix_cost(node,tec,year,year) ) )
;

***
* Here, :math:`n^L \in N(n)` are all nodes :math:`n^L` that are sub-nodes of node :math:`n`.
* The subset of technologies :math:`t \in T(\widehat{t})` are all tecs that belong to category :math:`\widehat{t}`,
* and similar notation is used for emissions :math:`e \in E`.
***

*----------------------------------------------------------------------------------------------------------------------*
***
* Resource and commodity section
* ------------------------------
*
***
* Constraints on commodities and stocks
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation COMMODITY_BALANCE
* """"""""""""""""""""""""""
* This constraint ensures the nodal and temporal balance of outputs/imports and inputs/exports for all commodities.
*
*  .. math::
*     \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y}} output_{n^L,t,y^V,y,m,n,c,l,h^A,h}
*         \cdot duration\_time\_rel_{h,h^A} \cdot & ACT_{n^L,t,y^V,y,m,h^A} \\
*     - \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y}} input_{n^L,t,y^V,y,m,n,c,l,h^A,h}
*         \cdot duration\_time\_rel_{h,h^A} \cdot & ACT_{n^L,t,m,y,h^A} \\
*     + \ STOCK\_CHG_{n,c,l,y,h} & \\[4pt]
*     + \ \sum_s \Big( land\_output_{n,s,y,c,l,h} - land\_input_{n,s,y,c,l,h} \Big) \cdot & LAND_{n,s,y} \\[4pt]
*     - \ demand\_fixed_{n,c,l,y,h}
*     & \geq 0 \quad \forall \ l \notin L^{RES} \subseteq L
*
* The commodity balance constraint at the resource level is included in the `Equation RESOURCE_CONSTRAINT`_,
* while at the renewable level, it is included in the `Equation RENEWABLES_EQUIVALENCE`_.
***
COMMODITY_BALANCE(node,commodity,level,year,time)$( map_commodity(node,commodity,level,year,time)
                 AND NOT full_balance(commodity))..
    SUM( (location,tec,vintage,mode,time2)$( map_tec_act(location,tec,year,mode,time2)
            AND map_tec_lifetime(location,tec,vintage,year) ),
* import into node and output by all technologies located at 'location' sending to 'node' and 'time2' sending to 'time'
        output(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        * duration_time_rel(time,time2) * ACT(location,tec,vintage,year,mode,time2)
* export from node and input into technologies located at 'location' taking from 'node' and 'time2' taking from 'time'
        - input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        * duration_time_rel(time,time2) * ACT(location,tec,vintage,year,mode,time2) )
* quantity taken out from ( >0 ) or put into ( <0 ) inter-period stock (storage)
    + STOCK_CHG(node,commodity,level,year,time)$( map_stocks(node,commodity,level,year) )
* final consumption (exogenous parameter to be satisfied by the energy+water+xxx system)
    - demand_fixed(node,commodity,level,year,time)
* relaxation of constraints for debugging
%SLACK_COMMODITY_BALANCE%   + SLACK_COMMODITY_BALANCE_UP(node,commodity,level,year,time)
%SLACK_COMMODITY_BALANCE%   - SLACK_COMMODITY_BALANCE_LO(node,commodity,level,year,time)
    =G= 0 ;

* This constraint ensures the FULL (i.e., no spilling) nodal and temporal balance of outputs/imports and inputs/exports commodities listed in full_balance(commodity).
COMMODITY_BALANCE_FULL(node,commodity,level,year,time)$( map_commodity(node,commodity,level,year,time)
                  AND full_balance(commodity))..
    SUM( (location,tec,vintage,mode,time2)$( map_tec_act(location,tec,year,mode,time2)
            AND map_tec_lifetime(location,tec,vintage,year) ),
* import into node and output by all technologies located at 'location' sending to 'node' and 'time2' sending to 'time'
        output(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        * duration_time_rel(time,time2) * ACT(location,tec,vintage,year,mode,time2)
* export from node and input into technologies located at 'location' taking from 'node' and 'time2' taking from 'time'
        - input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        * duration_time_rel(time,time2) * ACT(location,tec,vintage,year,mode,time2) )
* quantity taken out from ( >0 ) or put into ( <0 ) inter-period stock (storage)
    + STOCK_CHG(node,commodity,level,year,time)$( map_stocks(node,commodity,level,year) )
    - STORAGE_CHG(node,commodity,level,year,time)$( map_storage(node,commodity,level,year,time) AND bound_storage_up(node,commodity,level,year,time) )
* final consumption (exogenous parameter to be satisfied by the energy+water+xxx system)
    - demand_fixed(node,commodity,level,year,time)
* relaxation of constraints for debugging
%SLACK_FULL_COMMODITY_BALANCE%   + SLACK_FULL_COMMODITY_BALANCE_UP(node,commodity,level,year,time)
%SLACK_FULL_COMMODITY_BALANCE%   - SLACK_FULL_COMMODITY_BALANCE_LO(node,commodity,level,year,time)
    =E= 0 ;

***
* Equation STOCKS_BALANCE
* """""""""""""""""""""""
* This constraint ensures the inter-temporal balance of commodity stocks.
* The parameter :math:`commodity\_stocks_{n,c,l}` can be used to model exogenous additions to the stock
*
*  .. math::
*     STOCK_{n,c,l,y} + commodity\_stock_{n,c,l,y} =
*         duration\_period_{y} \cdot & \sum_{h} STOCK\_CHG_{n,c,l,y,h} \\
*                                    & + STOCK_{n,c,l,y+1}
*
***
STOCKS_BALANCE(node,commodity,level,year)$( map_stocks(node,commodity,level,year) )..
    STOCK(node,commodity,level,year)$( NOT first_period(year) )
    + commodity_stock(node,commodity,level,year) =E=
    duration_period(year) * SUM(time$( map_commodity(node,commodity,level,year,time) ),
         STOCK_CHG(node,commodity,level,year,time) )
    + SUM(year2$( seq_period(year,year2) ), STOCK(node,commodity,level,year2) ) ;

***
* Equation STORAGE_BALANCE
* """""""""""""""""""""""
* This constraint ensures the inter-temporal balance of commodity stocks.
* The parameter :math:`commodity\_stocks_{n,c,l}` can be used to model exogenous additions to the stock
*
*  .. math::
*     STORAGE_{n,c,l,y} + commodity\_stock_{n,c,l,y} =
*         duration\_period_{y} \cdot & \sum_{h} STORAGE\_CHG_{n,c,l,y,h} \\
*                                    & + STORAGE_{n,c,l,y+1}
*
***

Parameter
    storage_to_activity(time)
                         /  1   31,
                            2   28
                            3   31
                            4   30
                            5   31
                            6   30
                            7   31
                            8   31
                            9   30
                            10  31
                            11  30
                            12  31 /;

set first_time_stg(year_all,time) /2020.1/;
parameter commodity_storage(node,commodity,level,year_all,time);

commodity_storage(node,commodity,level,'2020','1') = ( bound_storage_up(node,commodity,level,'2020','1') + bound_storage_lo(node,commodity,level,'2020','1') )/2;

STORAGE_BALANCE(node,commodity,level,year,time)$( map_storage(node,commodity,level,year,time) AND bound_storage_up(node,commodity,level,year,time) )..
    STORAGE(node,commodity,level,year,time)
* decide whether want to use commodity stock or make a new parameter
    - commodity_storage(node,commodity,level,year,time) =E=
*    SUM(time2$( map_commodity(node,commodity,level,year,time2) ),
       ( storage_to_activity(time)*STORAGE_CHG(node,commodity,level,year,time) )
    + SUM((time2,year2)$seq_year_time(year2,year,time2,time), STORAGE(node,commodity,level,year2,time2) * (1 - storage_loss(node,commodity,level,year2,time2)) ) ;

STORAGE_BOUND_UP(node,commodity,level,year,time)$( map_storage(node,commodity,level,year,time) AND bound_storage_up(node,commodity,level,year,time) )..
    STORAGE(node,commodity,level,year,time) =L= bound_storage_up(node,commodity,level,year,time) ;
* might be possible to add here the CAP of a storage_expansio technology, that increases the maximum storage limit

STORAGE_BOUND_LO(node,commodity,level,year,time)$( map_storage(node,commodity,level,year,time) AND bound_storage_up(node,commodity,level,year,time) )..
*    STORAGE(node,commodity,level,year,time) =G= 0 ;
    STORAGE(node,commodity,level,year,time) =G= bound_storage_lo(node,commodity,level,year,time) ;

*----------------------------------------------------------------------------------------------------------------------*
***
* Technology section
* ------------------
*
* Technical and engineering constraints
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation CAPACITY_CONSTRAINT
* """"""""""""""""""""""""""""
* This constraint ensures that the actual activity of a technology at a node/time cannot exceed available (maintained)
* capacity summed over all vintages, including the technology capacity factor :math:`capacity\_factor_{n,t,y,t}`.
*
*  .. math::
*     \sum_{m} ACT_{n,t,y^V,y,m,h}
*         \leq duration\_time_{h} \cdot capacity\_factor_{n,t,y^V,y,h} \cdot CAP_{n,t,y^V,y}
*         \quad t \ \in \ T^{INV}
*
* where :math:`T^{INV} \subseteq T` is the set of all technologies
* for which investment decisions and capacity constraints are relevant.
***
CAPACITY_CONSTRAINT(node,inv_tec,vintage,year,time)$( map_tec_time(node,inv_tec,year,time)
        AND map_tec_lifetime(node,inv_tec,vintage,year) )..
    SUM(mode$( map_tec_act(node,inv_tec,year,mode,time) ), ACT(node,inv_tec,vintage,year,mode,time) )
        =L= duration_time(time) * capacity_factor(node,inv_tec,vintage,year,time) * CAP(node,inv_tec,vintage,year) ;

***
* Equation CAPACITY_MAINTENANCE
* """"""""""""""""""""""""""""""
* This constraint deals with fixed costs for operation and maintainance (O&M) of technology capacity_maintainance.
* Capacity must be maintained over time until decommissioning (no mothballing), and fixed O\&M costs must be paid
* immediately after commissioning.
*
*  .. math::
*     CAP_{n,t,y^V,y} \leq
*     remaining\_capacity_{n,t,y^V,y} \cdot
*     \left\{ \begin{array}{ll}
*         duration\_period_{y^V} \cdot historical\_new\_capacity_{n,t,y^V} \quad & \text{if } y \ \text{is first model period} \\
*         duration\_period_{y^V} \cdot CAP\_NEW_{n,t,y^V} \quad & \text{if } y = y^V \\
*         CAP_{n,t,y^V,y-1} & \text{if } y > y^V \text{ and }
*                                  |y| - |y^V| < technical\_lifetime_{n,t,y^V} \end{array} \right\}
*         \quad \forall \ t \in T^{INV}
*
* The current formulation does not account for construction time in the constraints, but only adds a mark-up
* to the investment costs in the objective function.
***
CAPACITY_MAINTENANCE(node,inv_tec,vintage,year)$( map_tec_lifetime(node,inv_tec,vintage,year) )..
    CAP(node,inv_tec,vintage,year) =L=
* discount the capacity in case the technical lifetime ends within a period
    remaining_capacity(node,inv_tec,vintage,year) * (
* historical installation (built before start of model horizon)
        ( duration_period(vintage) * historical_new_capacity(node,inv_tec,vintage)
            )$( historical(vintage) AND first_period(year) )
* new capacity built in the current period (vintage == year)
        + ( duration_period(vintage) * CAP_NEW(node,inv_tec,vintage)
            )$( year_order(year) EQ year_order(vintage) AND NOT historical(vintage) )
* total installed capacity at the end of the previous period
        + SUM(year2$( seq_period(year2,year) AND map_tec_lifetime(node,inv_tec,vintage,year2) ),
            CAP(node,inv_tec,vintage,year2) )
    ) ;

***
* Equation OPERATION_CONSTRAINT
* """""""""""""""""""""""""""""
* This constraint provides an upper bound on the total operation of installed capacity over a year.
*
*   .. math::
*      \sum_{m,h} ACT_{n,t,y^V,y,m,h}
*          \leq operation\_factor_{n,t,y^V,y} \cdot capacity\_factor_{n,t,y^V,y,m,\text{'year'}} \cdot CAP_{n,t,y^V,y}
*
* This constraint is only active if :math:`operation\_factor_{n,t,y^V,y} < 1`.
***
OPERATION_CONSTRAINT(node,inv_tec,vintage,year)$( map_tec_lifetime(node,inv_tec,vintage,year)
        AND operation_factor(node,inv_tec,vintage,year) < 1 )..
    SUM((mode,time)$( map_tec_act(node,inv_tec,year,mode,time) ), ACT(node,inv_tec,vintage,year,mode,time) ) =L=
        operation_factor(node,inv_tec,vintage,year) * capacity_factor(node,inv_tec,vintage,year,'year')
        * CAP(node,inv_tec,vintage,year) ;

***
* Equation MIN_UTILIZATION_CONSTRAINT
* """""""""""""""""""""""""""""""""""
* This constraint provides a lower bound on the total utilization of installed capacity over a year.
*
*   .. math::
*      \sum_{m,h} ACT_{n,t,y^V,y,m,h} \geq min\_utilization\_factor_{n,t,y^V,y} \cdot CAP_{n,t,y^V,y}
*
* This constraint is only active if :math:`min\_utilization\_factor_{n,t,y^V,y}` is defined.
***
MIN_UTILIZATION_CONSTRAINT(node,inv_tec,vintage,year)$( map_tec_lifetime(node,inv_tec,vintage,year)
        AND min_utilization_factor(node,inv_tec,vintage,year) )..
    SUM((mode,time)$( map_tec_act(node,inv_tec,year,mode,time) ), ACT(node,inv_tec,vintage,year,mode,time) ) =G=
        min_utilization_factor(node,inv_tec,vintage,year) * CAP(node,inv_tec,vintage,year) ;

***
* Constraints representing the firm capacity requirement
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* The following constraint ensures that there is sufficient firm (dispatchable) capacity in each period.
* The formulation is based on Sullivan et al., 2013 :cite:`sullivan_VRE_2013`.
*
* The firm capacity a technology provides depends on their reliability factor per rating.
* The rating are defined depending on the share the single technology provides to the
* system. The reliablitiy factor of conventional powerplants is equal to 1. Therefore
* they provide their nameplate capacity as firm capacity.
* The reliability factor of wind and solar dependens on the share they have in the
* energy system. Therefore their reliability factor depend on the rating.
*
* Equation COMMODITY_USE_LEVEL
* """"""""""""""""""""""""""""
* This constraint defines the COMMODITY_USE_LEVEL as summed consumption of a
* commodity at a certain level during one year.
*
*   .. math::
*      COMMODITY\_USE_{n,c,l,y} = \sum_{n,t,y^V,m,h} input_{n,t,y^V,y,m,n,c,l,h,h} \cdot \\
*                duration\_time\_rel_{h,h} \cdot ACT_{n,t,y^V,y,m,h}
*
* This constraint is only active if :math:`peak\_load\_factor_{n,c,l,y,h}` is defined.
* The auxiliary variable :math:`COMMODITY\_USE_{n,c,l,y}` is only required
* for the equations :math:`FIRM\_CAPACITY\_CONSTRAINT` and :math:`FIRM\_CAPACITY\_SHARE`.
***

COMMODITY_USE_LEVEL(node,commodity,level,year,time)$( peak_load_factor(node,commodity,level,year,time) )..
    COMMODITY_USE(node,commodity,level,year)
    =E= SUM( (location,tec,vintage,mode,time2)$( map_tec_act(location,tec,year,mode,time2)
                                                    AND map_tec_lifetime(location,tec,vintage,year) ),
                input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
                * duration_time_rel(time,time2)
                * ACT(location,tec,vintage,year,mode,time2) )
;

***
* Equation FIRM_CAPACITY_CONSTRAINT
* """""""""""""""""""""""""""""""""
* This constraint ensures that there is sufficient firm (dispatchable) capacity in each period.
* The formulation is based on Sullivan et al., 2013 :cite:`sullivan_VRE_2013`.
*
*   .. math::
*      \sum_{t, q \substack{t \in T^{INV} \\ y^V \leq y} } reliability\_factor_{n,t,y,c,l,h,q} \cdot
*                CAP\_FIRM_{n,t,c,l,y,q} \geq \\
*         peak\_load\_factor_{n,c,l,y,h} \cdot COMMODITY\_USE_{n,c,l,y}
*
* This constraint is only active if :math:`peak\_load\_factor_{n,c,l,y,h}` is defined.
***
FIRM_CAPACITY_CONSTRAINT(node,commodity,level,year,time)$( peak_load_factor(node,commodity,level,year,time) )..
    SUM((inv_tec,rating), CAP_FIRM(node,inv_tec,commodity,level,year,rating)
                        * reliability_factor(node,inv_tec,year,commodity,level,time,rating) )
    =G= peak_load_factor(node,commodity,level,year,time) * COMMODITY_USE(node,commodity,level,year)
;

***
* Equation FIRM_CAPACITY_SHARE
* """"""""""""""""""""""""""""
* Limits the firm capacity per rating to the size of the penetration bin of this rating.
*
*   .. math::
*      CAP\_FIRM_{n,t,c,l,y,q} \leq rating\_bin_{n,t,y,c,l,h,q} \cdot COMMODITY\_USE_{n,c,l,y}
*
* This constraint is only active if :math:`reliability\_bin_{n,t,y,c,l,t,q}` is defined.
***
FIRM_CAPACITY_SHARE(node,inv_tec,commodity,level,year,rating,time)$(
        rating_bin(node,inv_tec,year,commodity,level,time,rating) ) ..
    CAP_FIRM(node,inv_tec,commodity,level,year,rating)
    =L= rating_bin(node,inv_tec,year,commodity,level,time,rating) * COMMODITY_USE(node,commodity,level,year)
;

***
* .. _reliability_constraint:
*
* Equation FIRM_CAPACITY_PROVISION
* """"""""""""""""""""""""""""""""
* Limits the firm capacity of the renewables technologies to the total installed capacity of each technology.
*
*   .. math::
*      \sum_{r,h} CAP\_FIRM_{n,t,c,l,y,q} \leq \sum_{y^V \leq y} CAP_{n,t,y^Y,y} \quad \forall t \in T^{INV}
*
* This constraint is only active if :math:`reliability\_factor_{n,t,y,c,l,h,q}` is defined.
***
FIRM_CAPACITY_PROVISION(node,inv_tec,year,commodity,level)$(
        SUM(rating, map_rating(node,inv_tec,commodity,level,rating,year) ) )..
    SUM( (rating,time)$( map_rating(node,inv_tec,commodity,level,rating,year) AND
                   reliability_factor(node,inv_tec,year,commodity,level,time,rating) ) ,
         CAP_FIRM(node,inv_tec,commodity,level,year,rating) )
    =L= SUM(vintage$( map_tec_lifetime(node,inv_tec,vintage,year) ), CAP(node,inv_tec,vintage,year) )
;

***
* .. _flexibility_constraint:
*
* Equation OPERATING_RESERVE_CONSTRAINT
* """""""""""""""""""""""""""""""""""""
* This constraint ensures that, in each sub-annual time slice, there is a sufficient share of flexible technologies in
* the power generation mix. This heading is a placeholder for a new formulation using the extended index set structure.
***


***
* Bounds on capacity and activity
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation NEW_CAPACITY_BOUND_UP
* """"""""""""""""""""""""""""""
* This constraint provides upper bounds on new capacity installation.
*
*   .. math::
*      CAP\_NEW_{n,t,y} \leq bound\_new\_capacity\_up_{n,t,y} \quad \forall \ t \ \in \ T^{INV}
*
***
NEW_CAPACITY_BOUND_UP(node,inv_tec,year)$( is_bound_new_capacity_up(node,inv_tec,year) )..
    CAP_NEW(node,inv_tec,year) =L= bound_new_capacity_up(node,inv_tec,year)
%SLACK_CAP_NEW_BOUND_UP% + SLACK_CAP_NEW_BOUND_UP(node,inv_tec,year)
;

***
* Equation NEW_CAPACITY_BOUND_LO
* """"""""""""""""""""""""""""""
* This constraint provides lower bounds on new capacity installation.
*
*   .. math::
*      CAP\_NEW_{n,t,y} \geq bound\_new\_capacity\_lo_{n,t,y} \quad \forall \ t \ \in \ T^{INV}
*
***
NEW_CAPACITY_BOUND_LO(node,inv_tec,year)$( is_bound_new_capacity_lo(node,inv_tec,year) )..
    CAP_NEW(node,inv_tec,year) =G= bound_new_capacity_lo(node,inv_tec,year)
%SLACK_CAP_NEW_BOUND_LO% - SLACK_CAP_NEW_BOUND_LO(node,inv_tec,year)
;

***
* Equation TOTAL_CAPACITY_BOUND_UP
* """"""""""""""""""""""""""""""""
* This constraint gives upper bounds on the total installed capacity of a technology in a specific year of operation
* summed over all vintages.
*
*   .. math::
*      \sum_{y^V \leq y} CAP_{n,t,y,y^V} \leq bound\_total\_capacity\_up_{n,t,y} \quad \forall \ t \ \in \ T^{INV}
*
***
TOTAL_CAPACITY_BOUND_UP(node,inv_tec,year)$( is_bound_total_capacity_up(node,inv_tec,year) )..
    SUM(vintage$( map_period(vintage,year) AND map_tec_lifetime(node,inv_tec,vintage,year) ),
        CAP(node,inv_tec,vintage,year) )
    =L= bound_total_capacity_up(node,inv_tec,year)
%SLACK_CAP_TOTAL_BOUND_UP% + SLACK_CAP_TOTAL_BOUND_UP(node,inv_tec,year)
;

***
* Equation TOTAL_CAPACITY_BOUND_LO
* """"""""""""""""""""""""""""""""
* This constraint gives lower bounds on the total installed capacity of a technology.
*
*   .. math::
*      \sum_{y^V \leq y} CAP_{n,t,y,y^V} \geq bound\_total\_capacity\_lo_{n,t,y} \quad \forall \ t \ \in \ T^{INV}
*
***
TOTAL_CAPACITY_BOUND_LO(node,inv_tec,year)$( is_bound_total_capacity_lo(node,inv_tec,year) )..
    SUM(vintage$( map_period(vintage,year) AND map_tec_lifetime(node,inv_tec,vintage,year) ),
        CAP(node,inv_tec,vintage,year) )
     =G= bound_total_capacity_lo(node,inv_tec,year)
%SLACK_CAP_TOTAL_BOUND_LO% - SLACK_CAP_TOTAL_BOUND_LO(node,inv_tec,year)
;

***
* Equation ACTIVITY_BOUND_UP
* """"""""""""""""""""""""""
* This constraint provides lower bounds of a technology activity by mode, summed over all vintages.
*
*   .. math::
*      \sum_{y^V \leq y} ACT_{n,t,y^V,y,m,h} \leq bound\_activity\_up_{n,t,m,y,h}
*
***
ACTIVITY_BOUND_UP(node,tec,year,mode,time)$( map_tec_act(node,tec,year,mode,time)
        AND is_bound_activity_up(node,tec,year,mode,time) )..
    SUM(vintage$( map_tec_lifetime(node,tec,vintage,year) ), ACT(node,tec,vintage,year,mode,time) ) =L=
    bound_activity_up(node,tec,year,mode,time)
%SLACK_ACT_BOUND_UP% + SLACK_ACT_BOUND_UP(node,tec,year,mode,time)
;

***
* Equation ACTIVITY_BOUND_LO
* """"""""""""""""""""""""""
* This constraint provides lower bounds of a technology activity by mode summed over all vintages.
*
*   .. math::
*      \sum_{y^V \leq y} ACT_{n,t,y^V,y,m,h} \geq bound\_activity\_lo_{n,t,y,m,h}
*
* We assume that :math:`bound\_activity\_lo_{n,t,y,m,h} = 0`
* unless explicitly stated otherwise.
***
ACTIVITY_BOUND_LO(node,tec,year,mode,time)$( map_tec_act(node,tec,year,mode,time) )..
    SUM(vintage$( map_tec_lifetime(node,tec,vintage,year) ), ACT(node,tec,vintage,year,mode,time) ) =G=
    bound_activity_lo(node,tec,year,mode,time)
%SLACK_ACT_BOUND_LO% - SLACK_ACT_BOUND_LO(node,tec,year,mode,time)
;

*----------------------------------------------------------------------------------------------------------------------*
***
* Constraints on shares of technologies and commodities
* -----------------------------------------------------
* This section allows to include upper and lower bounds on the shares of modes used by a technology
* or the shares of commodities produced or consumed by groups of technologies.
*
* Share constraints on activity by mode
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Equation SHARES_MODE_UP
* """""""""""""""""""""""
* This constraint provides upper bounds of the share of the activity of one mode
* of a technology. For example, it could limit the share of heat that can be produced
* in a combined heat and electricity power plant.
*
*   .. math::
*     ACT_{n^L,t,y^V,y,m,h^A}
*     \leq share\_mode\_up_{s,n,y,m,h} \cdot
*     \sum_{m\prime} ACT_{n^L,t,y^V,y,m\prime,h^A}
*
***
$ONTEXT
 SHARE_CONSTRAINT_MODE_UP(shares,node,tec,mode,year,time)$(
    map_tec_act(node,tec,year,mode,time) AND
    share_mode_up(shares,node,tec,mode,year,time)
)..
* activity of mode to be constrained
    SUM(
        vintage$( map_tec_lifetime(node,tec,vintage,year) ),
        ACT(node,tec,vintage,year,mode,time)
    )
    =L=
    share_mode_up(shares,node,tec,mode,year,time) *
* activity aggregated over all modes
    SUM(
        (vintage,mode2)$( map_tec_lifetime(node,tec,vintage,year) AND map_tec_mode(node,tec,year,mode2) ),
        ACT(node,tec,vintage,year,mode2,time)
    ) ;

***
* Equation SHARES_MODE_LO
* """""""""""""""""""""""
* This constraint provides lower bounds of the share of the activity of one mode of a technology.
*
*   .. math::
*     ACT_{n^L,t,y^V,y,m,h^A}
*     \geq share\_mode\_lo_{s,n,y,m,h} \cdot
*     \sum_{m\prime} ACT_{n^L,t,y^V,y,m\prime,h^A}
*
***
SHARE_CONSTRAINT_MODE_LO(shares,node,tec,mode,year,time)$(
    map_tec_act(node,tec,year,mode,time) AND
    share_mode_lo(shares,node,tec,mode,year,time)
)..
* activity of mode to be constrained
    SUM(
        vintage$( map_tec_lifetime(node,tec,vintage,year) ),
        ACT(node,tec,vintage,year,mode,time)
    )
    =G=
    share_mode_lo(shares,node,tec,mode,year,time) *
* activity aggregated over all modes
    SUM(
        (vintage,mode2)$( map_tec_lifetime(node,tec,vintage,year) AND map_tec_mode(node,tec,year,mode2) ),
        ACT(node,tec,vintage,year,mode2,time)
    ) ;
$OFFTEXT

***
* Share constraints on commodities
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* These constraints allow to set upper and lower bound on the quantity of commodities produced or consumed by a group
* of technologies relative to the commodities produced or consumed by another group.
*
* The implementation is generic and flexible, so that any combination of commodities, levels, technologies and nodes
* can be put in relation to any other combination.
*
* The notation :math:`S^{share}` represents the mapping set `map_shares_commodity_share` denoting all technology types,
* nodes, commodities and levels to be included in the numerator, and :math:`S^{total}` is
* the equivalent mapping set `map_shares_commodity_total` for the denominator.
*
* Equation SHARE_CONSTRAINT_COMMODITY_UP
* """"""""""""""""""""""""""""""""""""""
*   .. math::
*      & \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y, (n,\widehat{t},m,c,l) \sim S^{share}}}
*         ( output_{n^L,t,y^V,y,m,n,c,l,h^A,h} + input_{n^L,t,y^V,y,m,n,c,l,h^A,h} ) \\
*      & \quad \cdot duration\_time\_rel_{h,h^A} \cdot ACT_{n^L,t,y^V,y,m,h^A} \\
*      & \geq
*        share\_commodity\_up_{s,n,y,h} \cdot
*        \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y, (n,\widehat{t},m,c,l) \sim S^{total}}}
*            ( output_{n^L,t,y^V,y,m,n,c,l,h^A,h} + input_{n^L,t,y^V,y,m,n,c,l,h^A,h} ) \\
*      & \quad \cdot duration\_time\_rel_{h,h^A} \cdot ACT_{n^L,t,y^V,y,m,h^A}
*
* This constraint is only active if :math:`share\_commodity\_up_{s,n,y,h}` is defined.
***
$ONTEXT
 SHARE_CONSTRAINT_COMMODITY_UP(shares,node_share,year,time)$( share_commodity_up(shares,node_share,year,time) )..
* activity by type_tec_share technologies with map_shares_generic_share entries and a specific mode
    SUM( (node,location,type_tec_share,tec,vintage,mode,commodity,level,time2)$(
        ( map_shares_commodity_share(shares,node_share,node,type_tec_share,mode,commodity,level) OR
           map_shares_commodity_share(shares,node_share,node,type_tec_share,'all',commodity,level) ) AND
        cat_tec(type_tec_share,tec) AND
        map_tec_act(location,tec,year,mode,time2) AND
        map_tec_lifetime(location,tec,vintage,year)
    ),
        (
            output(location,tec,vintage,year,mode,node,commodity,level,time2,time) +
            input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        ) *
        duration_time_rel(time,time2) *
        ACT(location,tec,vintage,year,mode,time2)
    )
    =L=
    share_commodity_up(shares,node_share,year,time) * (
* total input and output by `type_tec_total` technologies mapped to respective commodity, level and node
    SUM( (node,location,type_tec_total,tec,vintage,mode,commodity,level,time2)$(
        ( map_shares_commodity_total(shares,node_share,node,type_tec_total,mode,commodity,level) OR
           map_shares_commodity_total(shares,node_share,node,type_tec_total,'all',commodity,level) ) AND
        cat_tec(type_tec_total,tec) AND
        map_tec_act(location,tec,year,mode,time2) AND
        map_tec_lifetime(location,tec,vintage,year)
    ),
        (
            output(location,tec,vintage,year,mode,node,commodity,level,time2,time) +
            input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        ) *
        duration_time_rel(time,time2) *
        ACT(location,tec,vintage,year,mode,time2)
    ) ) ;
$OFFTEXT

***
* Equation SHARE_CONSTRAINT_COMMODITY_LO
* """"""""""""""""""""""""""""""""""""""
*   .. math::
*      & \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y, (n,\widehat{t},m,c,l) \sim S^{share}}}
*         ( output_{n^L,t,y^V,y,m,n,c,l,h^A,h} + input_{n^L,t,y^V,y,m,n,c,l,h^A,h} ) \\
*      & \quad \cdot duration\_time\_rel_{h,h^A} \cdot ACT_{n^L,t,y^V,y,m,h^A} \\
*      & \leq
*        share\_commodity\_lo_{s,n,y,h} \cdot
*        \sum_{\substack{n^L,t,m,h^A \\ y^V \leq y, (n,\widehat{t},m,c,l) \sim S^{total}}}
*            ( output_{n^L,t,y^V,y,m,n,c,l,h^A,h} + input_{n^L,t,y^V,y,m,n,c,l,h^A,h} ) \\
*      & \quad \cdot duration\_time\_rel_{h,h^A} \cdot ACT_{n^L,t,y^V,y,m,h^A}
*
* This constraint is only active if :math:`share\_commodity\_lo_{s,n,y,h}` is defined.
***
SHARE_CONSTRAINT_COMMODITY_LO(shares,node_share,year,time)$( share_commodity_lo(shares,node_share,year,time) )..
* total input and output by `type_tec_share` technologies mapped to respective commodity, level and node
    SUM( (node,location,type_tec_share,tec,vintage,mode,commodity,level,time2)$(
         map_shares_commodity_share(shares,node_share,node,type_tec_share,mode,commodity,level)  AND
        cat_tec(type_tec_share,tec) AND
        map_tec_act(location,tec,year,mode,time2) AND
        map_tec_lifetime(location,tec,vintage,year)
    ),
        (
            output(location,tec,vintage,year,mode,node,commodity,level,time2,time) +
            input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        ) *
        duration_time_rel(time,time2) *
        ACT(location,tec,vintage,year,mode,time2)
    )
    =G=
    share_commodity_lo(shares,node_share,year,time) * (
* total input and output by `type_tec_total` technologies mapped to respective commodity, level and node
    SUM( (node,location,type_tec_total,tec,vintage,mode,commodity,level,time2)$(
         map_shares_commodity_total(shares,node_share,node,type_tec_total,mode,commodity,level)  AND
        cat_tec(type_tec_total,tec) AND
        map_tec_act(location,tec,year,mode,time2) AND
        map_tec_lifetime(location,tec,vintage,year)
    ),
        (
            output(location,tec,vintage,year,mode,node,commodity,level,time2,time) +
            input(location,tec,vintage,year,mode,node,commodity,level,time2,time)
        ) *
        duration_time_rel(time,time2) *
        ACT(location,tec,vintage,year,mode,time2)
    ) ) ;

***
* .. _dynamic_constraints:
*
* Dynamic constraints on market penetration
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* The constraints in this section specify dynamic upper and lower bounds on new capacity and activity,
* i.e., constraints on market penetration and rate of expansion or phase-out of a technology.
*
* The formulation directly includes the option for 'soft' relaxations of dynamic constraints
* (cf. Keppo and Strubegger, 2010 :cite:`keppo_short_2010`).
*
* Equation NEW_CAPACITY_CONSTRAINT_UP
* """""""""""""""""""""""""""""""""""
* The level of new capacity additions cannot be greater than an initial value (compounded over the period duration),
* annual growth of the existing 'capital stock', and a "soft" relaxation of the upper bound.
*
*  .. math::
*     CAP\_NEW_{n,t,y}
*         \leq & ~ initial\_new\_capacity\_up_{n,t,y}
*             \cdot \frac{ \Big( 1 + growth\_new\_capacity\_up_{n,t,y} \Big)^{|y|} - 1 }
*                        { growth\_new\_capacity\_up_{n,t,y} } \\
*              & + \Big( CAP\_NEW_{n,t,y-1} + historical\_new\_capacity_{n,t,y-1} \Big) \\
*              & \hspace{2 cm} \cdot \Big( 1 + growth\_new\_capacity\_up_{n,t,y} \Big)^{|y|} \\
*              & + CAP\_NEW\_UP_{n,t,y} \cdot \Bigg( \Big( 1 + soft\_new\_capacity\_up_{n,t,y}\Big)^{|y|} - 1 \Bigg) \\
*         & \quad \forall \ t \ \in \ T^{INV}
*
* Here, :math:`|y|` is the number of years in period :math:`y`, i.e., :math:`duration\_period_{y}`.
***
NEW_CAPACITY_CONSTRAINT_UP(node,inv_tec,year)$( map_tec(node,inv_tec,year)
        AND is_dynamic_new_capacity_up(node,inv_tec,year) )..
* actual new capacity
    CAP_NEW(node,inv_tec,year) =L=
* initial new capacity (compounded over the duration of the period)
        initial_new_capacity_up(node,inv_tec,year) * (
            ( ( POWER( 1 + growth_new_capacity_up(node,inv_tec,year) , duration_period(year) ) - 1 )
                / growth_new_capacity_up(node,inv_tec,year) )$( growth_new_capacity_up(node,inv_tec,year) )
              + ( duration_period(year) )$( NOT growth_new_capacity_up(node,inv_tec,year) )
            )
* growth of 'capital stock' from previous period
        + SUM(year_all2$( seq_period(year_all2,year) ),
            CAP_NEW(node,inv_tec,year_all2)$( map_tec(node,inv_tec,year_all2) AND model_horizon(year_all2) )
              + historical_new_capacity(node,inv_tec,year_all2) )
              # placeholder for spillover across nodes, technologies, periods (other than immediate predecessor)
            * POWER( 1 + growth_new_capacity_up(node,inv_tec,year) , duration_period(year) )
* optional relaxation for calibration and debugging
%SLACK_CAP_NEW_DYNAMIC_UP% + SLACK_CAP_NEW_DYNAMIC_UP(node,inv_tec,year)
;

* GAMS implementation comment:
* The sums in the constraint have to be over `year_all2` (not `year2`) to also get the dynamic effect from historical
* new capacity. If one would sum over `year2`, periods prior to the first model year would be ignored.

***
* Equation NEW_CAPACITY_CONSTRAINT_LO
* """""""""""""""""""""""""""""""""""
* This constraint gives dynamic lower bounds on new capacity.
*
*  .. math::
*     CAP\_NEW_{n,t,y}
*         \geq & - initial\_new\_capacity\_lo_{n,t,y}
*             \cdot \frac{ \Big( 1 + growth\_new\_capacity\_lo_{n,t,y} \Big)^{|y|} }
*                        { growth\_new\_capacity\_lo_{n,t,y} } \\
*              & + \Big( CAP\_NEW_{n,t,y-1} + historical\_new\_capacity_{n,t,y-1} \Big) \\
*              & \hspace{2 cm} \cdot \Big( 1 + growth\_new\_capacity\_lo_{n,t,y} \Big)^{|y|} \\
*              & - CAP\_NEW\_LO_{n,t,y} \cdot \Bigg( \Big( 1 + soft\_new\_capacity\_lo_{n,t,y}\Big)^{|y|} - 1 \Bigg) \\
*         & \quad \forall \ t \ \in \ T^{INV}
*
***
NEW_CAPACITY_CONSTRAINT_LO(node,inv_tec,year)$( map_tec(node,inv_tec,year)
        AND is_dynamic_new_capacity_lo(node,inv_tec,year) )..
* actual new capacity
    CAP_NEW(node,inv_tec,year) =G=
* initial new capacity (compounded over the duration of the period)
        - initial_new_capacity_lo(node,inv_tec,year) * (
            ( ( POWER( 1 + growth_new_capacity_lo(node,inv_tec,year) , duration_period(year) ) - 1 )
                / growth_new_capacity_lo(node,inv_tec,year) )$( growth_new_capacity_lo(node,inv_tec,year) )
              + ( duration_period(year) )$( NOT growth_new_capacity_lo(node,inv_tec,year) )
            )
* growth of 'capital stock' from previous period
        + SUM(year_all2$( seq_period(year_all2,year) ),
                CAP_NEW(node,inv_tec,year_all2)$( map_tec(node,inv_tec,year_all2) AND model_horizon(year_all2) )
                + historical_new_capacity(node,inv_tec,year_all2)
                # placeholder for spillover across nodes, technologies, periods (other than immediate predecessor)
            ) * POWER( 1 + growth_new_capacity_lo(node,inv_tec,year) , duration_period(year) )
* optional relaxation for calibration and debugging
%SLACK_CAP_NEW_DYNAMIC_LO% - SLACK_CAP_NEW_DYNAMIC_LO(node,inv_tec,year)
;


***
* Equation ACTIVITY_CONSTRAINT_UP
* """""""""""""""""""""""""""""""
* This constraint gives dynamic upper bounds on the market penetration of a technology activity.
*
*  .. math::
*     \sum_{y^V \leq y,m} ACT_{n,t,y^V,y,m,h}
*         \leq & ~ initial\_activity\_up_{n,t,y,h}
*             \cdot \frac{ \Big( 1 + growth\_activity\_up_{n,t,y,h} \Big)^{|y|} - 1 }
*                        { growth\_activity\_up_{n,t,y,h} } \\
*             & + \bigg( \sum_{y^V \leq y-1,m} ACT_{n,t,y^V,y-1,m,h}
*                         + \sum_{m} historical\_activity_{n,t,y-1,m,h} \bigg) \\
*             & \hspace{2 cm} \cdot \Big( 1 + growth\_activity\_up_{n,t,y,h} \Big)^{|y|} \\
*             & + ACT\_UP_{n,t,y,h} \cdot \Bigg( \Big( 1 + soft\_activity\_up_{n,t,y,h} \Big)^{|y|} - 1 \Bigg)
*
***
ACTIVITY_CONSTRAINT_UP(node,tec,year,time)$( map_tec_time(node,tec,year,time)
        AND is_dynamic_activity_up(node,tec,year,time) )..
* actual activity (summed over modes)
    SUM((vintage,mode)$( map_tec_lifetime(node,tec,vintage,year) AND map_tec_mode(node,tec,year,mode) ),
            ACT(node,tec,vintage,year,mode,time) ) =L=
* initial activity (compounded over the duration of the period)
        initial_activity_up(node,tec,year,time) * (
            ( ( POWER( 1 + growth_activity_up(node,tec,year,time) , duration_period(year) ) - 1 )
                / growth_activity_up(node,tec,year,time) )$( growth_activity_up(node,tec,year,time) )
              + ( duration_period(year) )$( NOT growth_activity_up(node,tec,year,time) )
            )
* growth of 'capital stock' from previous period
        + SUM((year_all2)$( seq_period(year_all2,year) ),
            SUM((vintage,mode)$( map_tec_lifetime(node,tec,vintage,year_all2) AND map_tec_mode(node,tec,year_all2,mode)
                                 AND model_horizon(year_all2) ),
                        ACT(node,tec,vintage,year_all2,mode,time) )
                + SUM(mode, historical_activity(node,tec,year_all2,mode,time) )
                # placeholder for spillover across nodes, technologies, periods (other than immediate predecessor)
                )
            * POWER( 1 + growth_activity_up(node,tec,year,time) , duration_period(year) )
* optional relaxation for calibration and debugging
%SLACK_ACT_DYNAMIC_UP% + SLACK_ACT_DYNAMIC_UP(node,tec,year,time)
;

***
* Equation ACTIVITY_CONSTRAINT_LO
* """""""""""""""""""""""""""""""
* This constraint gives dynamic lower bounds on the market penetration of a technology activity.
*
*  .. math::
*     \sum_{y^V \leq y,m} ACT_{n,t,y^V,y,m,h}
*         \geq & - initial\_activity\_lo_{n,t,y,h}
*             \cdot \frac{ \Big( 1 + growth\_activity\_lo_{n,t,y,h} \Big)^{|y|} - 1 }
*                        { growth\_activity\_lo_{n,t,y,h} } \\
*             & + \bigg( \sum_{y^V \leq y-1,m} ACT_{n,t,y^V,y-1,m,h}
*                         + \sum_{m} historical\_activity_{n,t,y-1,m,h} \bigg) \\
*             & \hspace{2 cm} \cdot \Big( 1 + growth\_activity\_lo_{n,t,y,h} \Big)^{|y|} \\
*             & - ACT\_LO_{n,t,y,h} \cdot \Bigg( \Big( 1 + soft\_activity\_lo_{n,t,y,h} \Big)^{|y|} - 1 \Bigg)
*
***
ACTIVITY_CONSTRAINT_LO(node,tec,year,time)$( map_tec_time(node,tec,year,time)
        AND is_dynamic_activity_lo(node,tec,year,time) )..
* actual activity (summed over modes)
    SUM((vintage,mode)$( map_tec_lifetime(node,tec,vintage,year) AND map_tec_mode(node,tec,year,mode) ),
            ACT(node,tec,vintage,year,mode,time) ) =G=
* initial activity (compounded over the duration of the period)
        - initial_activity_lo(node,tec,year,time) * (
            ( ( POWER( 1 + growth_activity_lo(node,tec,year,time) , duration_period(year) ) - 1 )
                / growth_activity_lo(node,tec,year,time) )$( growth_activity_lo(node,tec,year,time) )
              + ( duration_period(year) )$( NOT growth_activity_lo(node,tec,year,time) )
            )
* growth of 'capital stock' from previous period
        + SUM((year_all2)$( seq_period(year_all2,year) ),
            SUM((vintage,mode)$( map_tec_lifetime(node,tec,vintage,year_all2) AND map_tec_mode(node,tec,year_all2,mode)
                                 AND model_horizon(year_all2)),
                        ACT(node,tec,vintage,year_all2,mode,time) )
                + SUM(mode, historical_activity(node,tec,year_all2,mode,time) )
                # placeholder for spillover across nodes, technologies, periods (other than immediate predecessor)
                )
            * POWER( 1 + growth_activity_lo(node,tec,year,time) , duration_period(year) )
* optional relaxation for calibration and debugging
%SLACK_ACT_DYNAMIC_LO% - SLACK_ACT_DYNAMIC_LO(node,tec,year,time)
;

*----------------------------------------------------------------------------------------------------------------------*
***
* Emission section
* ----------------
*
* Auxiliary variable for aggregate emissions
* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
* Equation EMISSION_EQUIVALENCE
* """""""""""""""""""""""""""""
* This constraint simplifies the notation of emissions aggregated over different technology types
* and the land-use model emulator. The formulation includes emissions from all sub-nodes :math:`n^L` of :math:`n`.
*
*   .. math::
*      EMISS_{n,e,\widehat{t},y} =
*          \sum_{n^L \in N(n)} \Bigg(
*              \sum_{t \in T(\widehat{t}),y^V \leq y,m,h }
*                  emission\_factor_{n^L,t,y^V,y,m,e} \cdot ACT_{n^L,t,y^V,y,m,h} \\
*              + \sum_{s} \ land\_emission_{n^L,s,y,e} \cdot LAND_{n^L,s,y}
*                   \text{ if } \widehat{t} \in \widehat{T}^{LAND} \Bigg)
*
***
EMISSION_EQUIVALENCE(node,emission,type_tec,year)..
    EMISS(node,emission,type_tec,year)
    =E=
    SUM(location$( map_node(node,location) ),
* emissions from technology activity
        SUM((tec,vintage,mode,time)$( cat_tec(type_tec,tec)
            AND map_tec_act(location,tec,year,mode,time) AND map_tec_lifetime(location,tec,vintage,year) ),
        emission_factor(location,tec,vintage,year,mode,emission) * ACT(location,tec,vintage,year,mode,time) )
* emissions from land use if 'type_tec' is included in the dynamic set 'type_tec_land'
*        + SUM(land_scenario$( type_tec_land(type_tec) ),
*            land_emission(location,land_scenario,year,emission) * LAND(location,land_scenario,year) )
      ) ;

***
* Bound on emissions
* ^^^^^^^^^^^^^^^^^^
*
* Equation EMISSION_CONSTRAINT
* """"""""""""""""""""""""""""
* This constraint enforces upper bounds on emissions (by emission type). For all bounds that include multiple periods,
* the parameter :math:`bound\_emission_{n,\widehat{e},\widehat{t},\widehat{y}}` is scaled to represent average annual
* emissions over all years included in the year-set :math:`\widehat{y}`.
*
* The formulation includes historical emissions and allows to model constraints ranging over both the model horizon
* and historical periods.
*
*   .. math::
*      \frac{
*          \sum_{y' \in Y(\widehat{y}), e \in E(\widehat{e})}
*              \begin{array}{l}
*                  duration\_period_{y'} \cdot emission\_scaling_{\widehat{e},e} \cdot \\
*                  \Big( EMISS_{n,e,\widehat{t},y'} + \sum_{m} historical\_emission_{n,e,\widehat{t},y'} \Big)
*              \end{array}
*          }
*        { \sum_{y' \in Y(\widehat{y})} duration\_period_{y'} }
*      \leq bound\_emission_{n,\widehat{e},\widehat{t},\widehat{y}}
*
***
EMISSION_CONSTRAINT(node,type_emission,type_tec,type_year)$is_bound_emission(node,type_emission,type_tec,type_year)..
    SUM( (year_all2,emission)$( cat_year(type_year,year_all2) AND cat_emission(type_emission,emission) ),
        duration_period(year_all2) * emission_scaling(type_emission,emission) *
            ( EMISS(node,emission,type_tec,year_all2)$( year(year_all2) )
                + historical_emission(node,emission,type_tec,year_all2) )
      )
    / SUM(year_all2$( cat_year(type_year,year_all2) ), duration_period(year_all2) )
    =L= bound_emission(node,type_emission,type_tec,type_year) ;

*----------------------------------------------------------------------------------------------------------------------*
***
* .. _section_of_generic_relations:
*

*----------------------------------------------------------------------------------------------------------------------*
* model statements                                                                                                     *
*----------------------------------------------------------------------------------------------------------------------*

Model MESSAGE_LP /
    OBJECTIVE
    COST_ACCOUNTING_NODAL
    COMMODITY_BALANCE
    COMMODITY_BALANCE_FULL
    STOCKS_BALANCE
    STORAGE_BALANCE
    STORAGE_BOUND_LO
    STORAGE_BOUND_UP
    CAPACITY_CONSTRAINT
    CAPACITY_MAINTENANCE
    OPERATION_CONSTRAINT
    MIN_UTILIZATION_CONSTRAINT
    COMMODITY_USE_LEVEL
    FIRM_CAPACITY_CONSTRAINT
    FIRM_CAPACITY_PROVISION
    FIRM_CAPACITY_SHARE
    NEW_CAPACITY_BOUND_UP
    NEW_CAPACITY_BOUND_LO
    TOTAL_CAPACITY_BOUND_UP
    TOTAL_CAPACITY_BOUND_LO
    ACTIVITY_BOUND_UP
    ACTIVITY_BOUND_LO
    NEW_CAPACITY_CONSTRAINT_UP
    NEW_CAPACITY_CONSTRAINT_LO
    ACTIVITY_CONSTRAINT_UP
    ACTIVITY_CONSTRAINT_LO
    EMISSION_EQUIVALENCE
    EMISSION_CONSTRAINT
    SHARE_CONSTRAINT_COMMODITY_LO
/ ;

MESSAGE_LP.holdfixed = 1 ;
MESSAGE_LP.optfile = 1 ;
MESSAGE_LP.optcr = 0 ;



