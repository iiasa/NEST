Readme:
PB 7/02/2018

How to create a mask map for a specific catchment

1.) Select the last outlet of a catchment
   a.) select a catchment e.g. Yangtze
   b.) use the ups.map (upstream area map) in aguila or ArcGis or QGis 
   c.) use the biggest upstream area of the catchment and note down the lon/lat location e.g. yangtze lon/lat 120.84 31.75 

2.) run catchment.exe to create mask map
  a.) run : e> .\catch\catchment
      you see some explanation (hopefully)
  b.) if it is not working chnage the pathes in ./catch/config_win.ini
  c.)  put in the location and the river network
	.\catch\catchment 120.84 31.75 ldd.map a1.map
  d.) check if the catchment fits to your expectations e.g if you do .\catch\catchment 120.84 31.36 ldd.map a1.map  -> very tiny catchment

3.) resample to the smallest area
  The map is still global, to shrink it:
  a.) resample -c 0 a1.map yangtze.map
  b. ) check again with ArcGis, Aguila etc.

4.) if you do not like pcraster .map change it to .tif (e.g. with ArcGis) or .nc (CWEATM is reading all types .map, .tif , .nc)

5.) use it in CWATM as maskmap 


  
  
