
Please find the documentation of CWAT model:
http://www.iiasa.ac.at/cwatm 
https://cwatm.github.io/
To set it up see:
https://cwatm.github.io/setup.html

Source code on github:
https://github.com/CWatM/CWatM

On private github:
https://github.com/iiasa/CWATM_priv

OR: 
P:\watmodel\CWATM\model\source


Requirements:
Python 64 bit 2.7.x
Libraries: NetCDF4, gdal, numpy, scipy
-> either standard python and libraries from: https://www.lfd.uci.edu/~gohlke/pythonlibs/
-> or package e.g. anaconda


Global dataset 30min/5min
ftp.iiasa.ac.at
User: rcwatm
Password: Water1090

cwatm_input.zip  -> static data
OR:
P:\watmodel\CWATM\cwatm_input
P:\watmodel\CWATM\CWAT_input_5min\input5min_netcdf


And climate forcing (for testing WATCH-Forcing-Data-ERA-Interim Dataset) http://www.eu-watch.org/data_availability
wfdei.tar ->  climatic forcing (WFDEI)

OR:
P:\watmodel\CWATM\climate\Isi-Mip2\wfdei


For climate forcing:
It runs with the ISI-MIP datasets as they are (as netcdfs, doesnt matter if it is split into years or one file)

OR:
ISIMIP2 dataset on:

P:\watxene\ISIMIP\
e.g. P:\watxene\ISIMIP\ISIMIP2b\input\historical\GFDL-ESM2M


