

require( gdxrrw )
require( dplyr )

indus_ix_path = Sys.getenv("INDUS_IX_PATH")

scname = 'message_indus_test_3'

# Get the relevant data from the gdx output files
scen_chk = paste( 'MSGoutput_', scname, '.gdx',sep='') 

scenarios = data.frame( x1 = 'test' )
names(scenarios) = scen_chk		

upath = paste( indus_ix_path, '/model/output/', sep='')

# Import results from gdx
igdx( 'C:/GAMS/win64/25.1' )
vars = c( 'inv_cost', 'DEMAND', 'CAP_NEW' )
raw_extraction = lapply( vars, function( vv ){
  bind_rows( lapply( scen_chk, function(fpath){
    tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
    names(tmp$uels) = tmp$domains
    rs = data.frame( tmp$val )
    names(rs) = c( unlist( tmp$domains ), 'val' )
    rs[ , which( names(rs) != 'val' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'val' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
    rs = cbind( data.frame( gdx = rep( scen_chk, nrow(rs) ) ), rs )
    return(rs)
  } ) ) %>% dplyr::rename(value = val)
} )
names(raw_extraction) = vars

scenarios = unique(raw_extraction$CAP_NEW$gdx) # need to be kind of generic, once I define an unique way of importing model outputs
nodes = unique(raw_extraction$CAP_NEW$node)
commodity = unique(raw_extraction$DEMAND$commodity)
tec = unique(raw_extraction$CAP_NEW$tec)
level = unique(raw_extraction$DEMAND$level)
time = unique(raw_extraction$DEMAND$time)
unique(c(names(raw_extraction$inv_cost),names(raw_extraction$CAP_NEW),names(raw_extraction$DEMAND) ))
