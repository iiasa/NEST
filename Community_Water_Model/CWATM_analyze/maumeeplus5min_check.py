# -*- coding: utf-8 -*-
"""
Created on Tue Oct 23 23:33:30 2018

@author: wang.3866
"""
import os
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt

if ('EBRO' in os.environ['COMPUTERNAME']):
    os.chdir('L:\ESRL\Chun\CWatM\CWATM_outputTemp')
else:
    os.chdir('L:\ESRL\Chun\CWatM\CWATM_outputTemp')

gauge = [-83.71278, 41.5]
#gauge = [-83.7, 41.465]

data = xr.open_dataset('discharge_daily.nc')
q = data.discharge.mean(dim='time')
fig, ax = plt.subplots(figsize=(12,9))
X, Y = np.meshgrid(q.lon, q.lat)
h = ax.imshow(q)
ax.plot(np.argmin(abs(q.lon - gauge[0])), 
        np.argmin(abs(q.lat - gauge[1])), 
        '*', markeredgecolor='k', markerfacecolor='y', markersize=10)
ax.set_xticks(range(0, len(q.lon), 5))
ax.set_xticklabels(['%.2f' % x for x in q.lon.values[::5]])
ax.set_yticks(range(0, len(q.lat), 3))
ax.set_yticklabels(['%.2f' % x for x in q.lat.values[::5]])
cbar = fig.colorbar(h)
data.close()

# =============================================================================
# data = xr.open_dataset('ETRef_daily.nc')
# print(data.ETRef)
# data.close()
# 
# data = xr.open_dataset('Precipitation_daily.nc')
# print(data.Precipitation)
# data.close()
# 
# 
# os.chdir('L:\ESRL\Chun\CWatM\CWATM_data\climate\wfdei')
# 
# data = xr.open_dataset('pr.nc')
# (data.pr.isel(time=0) * 86400).plot()
# data.close()
# =============================================================================
