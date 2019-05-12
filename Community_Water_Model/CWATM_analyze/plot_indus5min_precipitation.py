# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 12:12:11 2018

@author: wangy
"""

import xarray as xr
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import matplotlib.colors as colors


outpath = os.path.join('..', 'CWATM_outputTemp')

dom = [31,28.25,31,30,31,30,31,31,30,31,30,31]
            
f = xr.open_dataset(os.path.join(outpath, 'Precipitation_monthavg.nc'), \
                    decode_times=False)

precip = f.Precipitation_monthavg.assign_coords(time = \
         pd.date_range(start='1995-01-01', end='2010-12-31', freq='MS'))

precip_clim = precip.groupby('time.month').mean(dim='time')

f.close()


bounds = np.array([0., 30., 50., 70., 90., 125., 175., 250., 350., 475., \
                   625., 800., 1200.])
norm = colors.BoundaryNorm(boundaries=bounds, ncolors=256)

fig, axes = plt.subplots(4, 3, figsize=(12,12))
axes = axes.flat
for count in range(12):
    # (with converstion to mm/month)
    monthdata = precip_clim[{'month':count}] * dom[count] * 1000.
    
    X, Y = np.meshgrid(list(monthdata.lon.values), list(monthdata.lat.values))

    im = axes[count].pcolormesh(X, Y, \
             monthdata.values, cmap='Spectral_r', norm=norm)

    axes[count].set_title('Month = '+str(count+1))

fig.subplots_adjust(bottom=0.1)
cbar_ax = fig.add_axes([0.15, 0.05, 0.7, 0.01])
cbar = fig.colorbar(im, ax=axes[0], cax=cbar_ax, \
                    ticks = (bounds[1:] + bounds[:-1])/2, \
                    orientation = 'horizontal', label='Precipitation [mm/month]')
fig.savefig('plot_indus5min_precipitation.png')