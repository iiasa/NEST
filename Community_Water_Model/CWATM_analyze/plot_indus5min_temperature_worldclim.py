# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 12:12:11 2018

@author: wangy

Compare the annual mean temperature in the Indus River Basin in the WorldClim
dataset and the WFDEI dataset
"""

import xarray as xr
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import matplotlib.colors as colors
import rasterio.features as rf
from shapely.geometry import shape
import fiona
from affine import Affine
from calcAffine import calcAffine

dom = [31,28.25,31,30,31,30,31,31,30,31,30,31]


# ---- mask to the Indus River Basin
def create_mask(var_lon, var_lat):
    c = fiona.open(os.path.join('..', '..', 'hydrosheds-d875a5caec4b04119dbc', \
                                'as_bas_15s_beta', 'Indus_River_Basin.shp'))
    pol = c.next()
    geom = shape(pol['geometry'])
    c.close()

    x, y = np.meshgrid(np.arange(0, len(var_lon)), np.arange(0, len(var_lat)))
    u, v = np.meshgrid(var_lon, var_lat)
    af = calcAffine(x.reshape(-1), y.reshape(-1), u.reshape(-1), v.reshape(-1))
    
    mask = rf.rasterize([(geom, 1)], out_shape=(len(var_lon), len(var_lat)),
                        fill=0, all_touched=True, dtype=np.uint8, \
                        transform=Affine(*af))
    return mask


# WFDEI
outpath = os.path.join('..', 'CWATM_data', 'climate', 'wfdei')
f = xr.open_dataset(os.path.join(outpath, 'tavg.nc'), decode_times=True)
tavg_clim = f.tavg.sel({'lon': f.lon[(f.lon.values > 65.) * \
                                     (f.lon.values < 85.)], \
                        'lat': f.lat[(f.lat.values > 20.) * \
                                     (f.lat.values < 40.)]}).copy()
tavg_clim = tavg_clim.groupby('time.month').mean(dim='time')
tavg_clim = tavg_clim.rename({'month': 'time'})
tavg_clim = tavg_clim.transpose('time', 'lon', 'lat')
mask = create_mask(tavg_clim.lon.values, tavg_clim.lat.values)
f.close()


# World Clim
outpath2 = os.path.join('..', 'CWATM_data', 'cwatm_input5min', 'meteo')
f = xr.open_dataset(os.path.join(outpath2, 'worldclim_tavg.nc'))
tavg_wc = f.wc_tavg.sel({'lon': f.lon[(f.lon.values > 65.) * \
                                        (f.lon.values < 85.)], \
                            'lat': f.lat[(f.lat.values > 20.) * \
                                         (f.lat.values < 40.)]}).copy()
tavg_wc = tavg_wc.transpose('time', 'lon', 'lat')
mask2 = create_mask(tavg_wc.lon.values, tavg_wc.lat.values)
f.close()


def draw_tavg(dataset, scale, mask):
    bounds = np.arange(-10., 55., 5)
    norm = colors.BoundaryNorm(boundaries=bounds, ncolors=256)

    n = int(dataset.shape[0]/3)
    fig, axes = plt.subplots(n, 3, figsize=(12, n*3))
    axes = axes.flat
    for count in range(dataset.shape[0]):
        # (with converstion to degC)
        monthdata = dataset[{'time':count}].where(mask) - scale

        X, Y = np.meshgrid(list(monthdata.lon.values), list(monthdata.lat.values))
    
        im = axes[count].pcolormesh(X, Y, \
                 monthdata.values, cmap='Spectral_r', norm=norm)

        axes[count].set_title('Month = '+str(count+1))

    fig.subplots_adjust(hspace=0.25, bottom=0.05)
    cbar_ax = fig.add_axes([0.15, 0., 0.7, 0.01])
    cbar = fig.colorbar(im, ax=axes[0], cax=cbar_ax, \
                        ticks = (bounds[1:] + bounds[:-1])/2, \
                        orientation = 'horizontal', label='Temperature [$^o$C]')
    return fig, axes, cbar
draw_tavg(tavg_clim[{'time': range(12)}], 273.15, mask)
draw_tavg(tavg_wc[{'time': range(12)}], 273.15, mask2)


def draw_tavg_mean(dataset, scale, mask):
    bounds = np.arange(-10., 55., 5)
    norm = colors.BoundaryNorm(boundaries=bounds, ncolors=256)

    fig, ax = plt.subplots(figsize=(8,6))
    for count in range(tavg_clim.shape[0]):
        # (with converstion to degC)
        monthdata = (dataset[{'time':count}].where(mask) - scale) * dom[count]
        if (count==0):
            yeardata = monthdata
        else:
            yeardata += monthdata
    yeardata /= sum(dom)

    X, Y = np.meshgrid(list(yeardata.lon.values), list(yeardata.lat.values))

    im = ax.pcolormesh(X, Y, \
             yeardata.values, cmap='Spectral_r', norm=norm)
    ax.set_title('Climatology')

    fig.subplots_adjust(bottom=0.1)
    cbar_ax = fig.add_axes([0.15, 0.05, 0.7, 0.01])
    cbar = fig.colorbar(im, ax=ax, cax=cbar_ax, \
                        ticks = (bounds[1:] + bounds[:-1])/2, \
                        orientation = 'horizontal', label='Temperature [$^o$C]')
    return fig, ax, cbar

draw_tavg_mean(tavg_clim, 273.15, mask)
draw_tavg_mean(tavg_wc, 273.15, mask2)