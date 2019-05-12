# -*- coding: utf-8 -*-
"""
Created on Wed Jun 13 11:44:44 2018

@author: wangy
"""
import xarray as xr
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

outpath = os.path.join('..', 'CWATM_outputTemp')

dom = [31,28.25,31,30,31,30,31,31,30,31,30,31]
time = pd.date_range('1995-01-01', '2010-12-31', freq='1MS')

# Fluxes
fluxlist = ['Precipitation', 'sum_actualET', 'runoff']
flux_components = {}
for ff in fluxlist:
    f = xr.open_dataset(os.path.join(outpath, ff + '_monthavg.nc'), \
                        decode_times=False)
    print(ff + ' Unit: '+f[ff+'_monthavg'].units)
    flux_components[ff] = f[ff+'_monthavg'].sum(dim=['lat', 'lon'])
    # ---- the flux have to be multiplied by the number of days in a month
    #      because of the monthly average
    flux_components[ff] *= np.array(dom * int(len(flux_components[ff])/12))
    f.close()
flux_components = pd.DataFrame.from_dict(flux_components)
flux_in = flux_components['Precipitation']
flux_out = flux_components.iloc[:,1:]


# Human activities
# 'waterDemand' is not in the water balance in, because it is implicit in 
# ET & Runoff
humlist = ['waterDemand', 'sumIrrDemand']
hum_components = {}
for ff in humlist:
    f = xr.open_dataset(os.path.join(outpath, ff + '_monthavg.nc'), \
                        decode_times=False)
    print(ff + ' Unit: '+f[ff+'_monthavg'].units)
    hum_components[ff] = f[ff+'_monthavg'].sum(dim=['lat', 'lon'])
    # ---- the flux have to be multiplied by the number of days in a month
    #      because of the monthly average
    hum_components[ff] *= np.array(dom * int(len(hum_components[ff])/12))
    f.close()
hum_components = pd.DataFrame.from_dict(hum_components)


# Storage change
storlist = ['totalSto', 'storGroundwater']
stor_components = {}
for ff in storlist:
    f = xr.open_dataset(os.path.join(outpath, ff + '_monthavg.nc'), \
                        decode_times=False)
    print(ff + ' Unit: '+f[ff+'_monthavg'].units)
    stor_components[ff] = f[ff+'_monthavg'].sum(dim=['lat', 'lon'])
    f.close()
stor_components = pd.DataFrame.from_dict(stor_components)
stor_change = stor_components.diff() # + more; - less


# Streamflow at outlet
Qsim_all = pd.read_csv(os.path.join('..', 'CWATM_outputTemp', \
                                    'discharge_monthavg.tss'), \
                       skiprows=17, header=None, sep='\s+', \
                       index_col=0)
Qsim1 = Qsim_all.iloc[:, -1]

# Plot
# ---- out components of water balance
wb_out = pd.concat([flux_out, stor_change * (stor_change>0.)], axis=1)
temp = list(wb_out.columns)
wb_out['default'] = 0.
wb_out = wb_out[['default'] + temp]
wb_out = wb_out.cumsum(axis=1)

# ---- in components of water balance
wb_in = pd.concat([flux_in, - stor_change * (stor_change<0.)], axis=1)
temp = list(wb_in.columns)
wb_in['default'] = 0.
wb_in = wb_in[['default'] + temp]
wb_in = wb_in.cumsum(axis=1)

# ---- plot the water balance and separately plot water demand
fig, axes = plt.subplots(5,1, sharex=True, sharey=True, figsize=(10,10))
axes = axes.flat
for ff in range(1, len(wb_out.columns)):
    axes[0].fill_between(time, wb_out.iloc[:, ff-1], wb_out.iloc[:, ff], \
                    label=wb_out.columns[ff])
axes[0].legend(loc='best', ncol=len(wb_out.columns)-1)
axes[0].set_title('Components Out')
axes[0].set_xticks(time[::12])
axes[0].grid('on')

for ff in range(1, len(wb_in.columns)):
    axes[1].fill_between(time, wb_in.iloc[:, ff-1], wb_in.iloc[:, ff], \
                    label=wb_in.columns[ff])
axes[1].legend(loc='best', ncol=len(wb_in.columns)-1)
axes[1].set_title('Components In')
axes[1].set_xticks(time[::12])
axes[1].grid('on')

axes[2].plot(time, wb_out.iloc[:,-1]-wb_in.iloc[:,-1])
axes[2].set_title('Components Out - Components In')
axes[2].set_xticks(time[::12])
axes[2].grid('on')

# ---- plot water demand
axes[3].plot(time, hum_components['waterDemand'], label='Total Water Demand')
axes[3].plot(time, hum_components['sumIrrDemand'], label='Irrigation Demand')
axes[3].legend(loc='best', ncol=2)
axes[3].set_title('Water Demand')
axes[3].set_xticks(time[::12])
axes[3].grid('on')

# ---- Components Out - Components In as 0.1% of Components In
axes[4].plot(time, (wb_out.iloc[:,-1]-wb_in.iloc[:,-1]) / \
                    wb_in.iloc[:,-1] * 1000.)
axes[4].set_title('(Components Out - Components In) / Components In*1000')
axes[4].set_xticks(time[::12])
axes[4].grid('on')

fig.savefig('plot_indus5min_waterbalance.png')
