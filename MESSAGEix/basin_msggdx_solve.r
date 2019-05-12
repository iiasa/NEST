
#### SOLVE on DB, at the moment only works on Jupyter Notebook, not on R
#scen$solve()

#### SOLVE LOCALLY

print( 'Solving' )

current_working_drive = getwd()
setwd(paste( indus_ix_path, '/model', sep = '' ) ) # set based on local machine

if( use_ixmp ){ sc_out = paste0( sc, '_ixmp' ) }else{ sc_out = sc }

runm = 'MESSAGE'
cmd = paste( "gams ",runm,"_run.gms --in=data\\MSGdata_", data_gdx, ".gdx --out=output\\MSGoutput_", sc_out, ".gdx", sep='' )
res = system(cmd)

setwd(current_working_drive)

if( res == 0 )
	{
	
	
	print( 'Feasible solution' )
	
	if( use_ixmp ) # upload to db
		{
		# scen$read_sol_from_gdx( paste( indus_ix_path, '/model/output', sep=''), 
								# paste( 'MSGoutput_', sc, '.gdx',sep='' ),
								# var_list = list("STORAGE","STORAGE_CHG"), 
								# equ_list = list( "COMMODITY_BALANCE_FULL", "STORAGE_BALANCE", "STORAGE_BALANCE_LO", "STORAGE_BALANCE_UP" ), 
								# check_sol = TRUE )	
		}						
	}else{

	print( 'Error during model solve or model is infeasible' )
	
	}	

if( use_ixmp )
	{
	scen$set_as_default()
	mp$close_db()
	}

