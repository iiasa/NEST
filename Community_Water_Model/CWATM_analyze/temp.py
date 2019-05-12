#!/usr/bin/env python
'''
Convert a bunch of GDAL readable grids to a NetCDF Time Series.
Here we read a bunch of files that have names like:
/usgs/data0/prism/1890-1899/us_tmin_1895.01
/usgs/data0/prism/1890-1899/us_tmin_1895.02
...
/usgs/data0/prism/1890-1899/us_tmin_1895.12
'''

import numpy as np
import datetime as dt
import os
import gdal
import netCDF4
import re

ds = gdal.Open('P:/ene.yssp/Yaoping_Wang/2018/CWatM/CWATM_data/cwatm_input5min/meteo/wc2.0_5m_prec/wc2.0_5m_prec_01.tif')
a = ds.ReadAsArray()
nlat,nlon = np.shape(a)

b = ds.GetGeoTransform() #bbox, interval
lon = np.arange(nlon)*b[1]+b[0]
lat = np.arange(nlat)*b[5]+b[3]


basedate = dt.datetime(1901,1,1,0,0,0)

# create NetCDF file
nco = netCDF4.Dataset('P:/ene.yssp/Yaoping_Wang/2018/CWatM/CWATM_data/cwatm_input5min/meteo/wc2.0_5m_prec.nc','w',clobber=True)

# chunking is optional, but can improve access a lot: 
# (see: http://www.unidata.ucar.edu/blogs/developer/entry/chunking_data_choosing_shapes)
chunk_lon=864
chunk_lat=360
chunk_time=2

# create dimensions, variables and attributes:
nco.createDimension('lon',nlon)
nco.createDimension('lat',nlat)
nco.createDimension('time',None)
timeo = nco.createVariable('time','f4',('time'))
timeo.units = 'Days since 1901-01-01'
timeo.standard_name = 'time'

lono = nco.createVariable('lon','f4',('lon'))
lono.units = 'degrees_east'
lono.standard_name = 'longitude'

lato = nco.createVariable('lat','f4',('lat'))
lato.units = 'degrees_north'
lato.standard_name = 'latitude'

# create short integer variable for temperature data, with chunking
tmno = nco.createVariable('wc_prec', 'f8',  ('time', 'lat', 'lon'), 
   zlib=True,chunksizes=[chunk_time,chunk_lat,chunk_lon],fill_value=1.0e20)
tmno.units = '[m]'
tmno.standard_name = 'wc_prec'

nco.Conventions='CF-1.6'

#write lon,lat
lono[:]=lon
lato[:]=lat

pat = re.compile('wc2.0_5m_prec_[0-12].tif')
itime=0

#step through data, writing time and data to NetCDF
for xx in range(1,13):
    f = 'P:/ene.yssp/Yaoping_Wang/2018/CWatM/CWATM_data/cwatm_input5min/' + \
        'meteo/wc2.0_5m_prec/wc2.0_5m_prec_' + ('%02i' % xx) + '.tif'

    # read the time values by parsing the filename
    date=dt.datetime(2000,xx,1,0,0,0)
    print(date)
    dtime=(date-basedate).total_seconds()/86400.
    timeo[itime]=dtime

    tmn=gdal.Open(f)
    a=tmn.ReadAsArray() * 1.  #data
    a[a < 0.] = np.nan
    tmno[itime,:,:]=a
    itime=itime+1

nco.close()