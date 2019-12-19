require( raster )
require( dplyr )
require( tidyr )
require( rgdal )
require( tictoc )
require( maptools )
require( rgeos )


#### Initialization / Model setup #####

tic()

# Location of input data
setwd( 'P:/is-wel/indus/message_indus' )

# Use ixmp? Takes time to upload - can choose option to debug w/o ixmp
use_ixmp = FALSE

# Local location of indus ix model - MAKE SURE TO ADD TO SYSTEM ENVIRONMENT VARIABLES
indus_ix_path = Sys.getenv("INDUS_IX_PATH")

# Location of GAMS - need to add to system environemtn variables but can't change from remote desktop :(
gams_path = 'C:/GAMS/win64/25.1'

# Basin analyzed
basin = 'Indus'

# Historical and planned electricity generation

# Historical capacity of technologies
hist_new_cap.df = read.csv( "input/historical_new_cap.csv", stringsAsFactors=FALSE ) %>%
					dplyr::select( -units )

ppls = 	unique( hist_new_cap.df$tec )[1:24]		
cap = hist_new_cap.df	%>% 
	filter( tec %in% ppls ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type, year_all ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()		
cap_2017 = hist_new_cap.df	%>% 
	filter( tec %in% ppls, year_all < 2020 ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()  	
cap_2020 = hist_new_cap.df	%>% 
	filter( tec %in% ppls, year_all >= 2020 ) %>% 
	mutate( country = unlist( strsplit( node, '_' ) )[seq(1,2*length(node),by=2)] ) %>%
	mutate( type = sapply( tec, function( ttt ){ rs = unlist( strsplit( ttt, '_' ) )[1] } ) ) %>%
	group_by( country, type ) %>% summarise( value = sum(value) ) %>% ungroup() %>% data.frame()  	

pdf( 'input/indus_electricity_capacity.pdf', width=5, height=6 )
p1 = layout( matrix( c(2,1),2,1, byrow=TRUE ), widths=c(1), heights=c(0.12,0.9) )
cap2plot = reshape(cap_2017, direction='wide', idvar = 'type', timevar = 'country' )
row.names(cap2plot) = cap2plot$type
cap2plot = cap2plot[,c(2:ncol(cap2plot))]
names(cap2plot) = unlist(strsplit(names(cap2plot),'[.]'))[seq(2,2*length(names(cap2plot)),by=2)]
cap2plot_tot = colSums( cap2plot, na.rm=TRUE )
cap2plot_fr = do.call( cbind, lapply( 1:ncol( cap2plot ), function( iii ){ ( 100 * cap2plot[,iii] /  colSums( cap2plot, na.rm=TRUE )[iii] ) } ) )
cap2plot_fr[is.na(cap2plot_fr)]=0
par(mar=c(2,4,0.5,3),oma=c(2,2,2,2))
cols = c( 'deepskyblue', 'brown', 'mistyrose', 'springgreen', 'black', 'yellow', 'orange', 'purple' )
cntrs = barplot( as.matrix( cap2plot_fr ), names.arg = names( cap2plot ), col = cols, ylab = 'Percent of Total Capacity [ % ]' )
abline(h=0)
abline(h=100)
abline(v=par()$usr[1])
abline(v=par()$usr[2],col='red')
xlim0=par()$usr[1:2]
par(new=TRUE)
plot.new()
plot.window(xlim=xlim0,ylim=c(700,round(max(cap2plot_tot)*1.05, digits=-3)),xaxs="i")
points( as.vector(cap2plot_tot) ~ cntrs, col = 'black', bg = 'red', pch = 21, cex = 1.2, lwd = 1.5 )
axis(side = 4, col = 'red' )
mtext('Total Installed Capacity [ MW ]', side = 4, line = 2.75 )
par(mar=c(0.5,4,0,3))
plot.new()
legend( 'center', legend = row.names(cap2plot), fill = cols, bty = 'n', ncol = 3, cex = 0.9 )
dev.off()