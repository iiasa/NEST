# -*- coding: utf-8 -*-
"""
"""

import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import HydroStats
import matplotlib.gridspec as gridspec


##out_path = os.path.join('..', 'calibration', 'calibration_indus5min', \
##                        'catchments', '22_010')
out_path = os.path.join('..', 'calibration', 'calibration_indus5min_2018-06-18', \
                        'catchments', '00_156')
##out_path = os.path.join('..', 'CWATM_output5min')
station = 0 # Number that corresponds to the besham station


Qobs1 = pd.read_csv(os.path.join('..', 'calibration', \
                                 'calibration_indus5min', \
                                 'observed_data', 'besham_1995.csv'), \
                    index_col=0).iloc[:,0]
Qobs1.index = pd.to_datetime(Qobs1.index)
Qsim_all = pd.read_csv(os.path.join(out_path, 'discharge_monthavg.tss'), \
                       skiprows=4, header=None, sep='\s+', \
                       index_col=0)
Qsim1 = Qsim_all.iloc[:, station]
Qsim1.index = Qobs1.index

Qobs1[Qobs1 < -9000] = np.nan
Qobs = Qobs1[~np.isnan(Qobs1)]
Qsim = Qsim1[~np.isnan(Qobs1)]


# Make dataframe with aligned Qsim and Qobs columns
Q = pd.concat([Qsim, Qobs], axis=1)#.reset_index()

########################################################################
#   Make figure consisting of several subplots
########################################################################
fig = plt.figure()
gs = gridspec.GridSpec(7,7)

# TEXT OF CALIBRATION RESULTS
ax0 = plt.subplot(gs[0,:])
#texts = r"\huge \bfseries "+str(row["ID"])+": "+str(row["RiverName"])+" at "+str(row["StationName"])
texts = "River: Indus, Station: UIB Besham"
ax0.text(0.5, 0.0, texts, fontdict={'size': 14}, verticalalignment='top', \
         horizontalalignment='center', transform=ax0.transAxes)
plt.axis('off')


# FIGURE OF CALIBRATION PERIOD TIME SERIES
Dates_Cal = Qobs.index
Q_obs_Cal = Qobs
Q_sim_Cal = Qsim
WarmupDays = 0

ax1 = plt.subplot(gs[1:4,0:7])
ax1.plot(Dates_Cal,Q_sim_Cal,'r',Dates_Cal, Q_obs_Cal,'b')
ax1.set_title('(a) Streamflow time series for calibration period')
locs, labels = plt.xticks()
plt.setp(labels,rotation=0)
plt.ylabel(r'Streamflow [m$^{3}$ s$^{-1}$]')

ns = HydroStats.NS(s=Q_sim_Cal,o=Q_obs_Cal,warmup=WarmupDays)
statsum = r' ' \
		+r'KGE$='+r'{0:.2f}'.format(HydroStats.KGE(s=Q_sim_Cal, o=Q_obs_Cal, warmup=WarmupDays)) \
		+r'$, NSE$='+r'{0:.2f}'.format(HydroStats.NS(s=Q_sim_Cal,o=Q_obs_Cal,warmup=WarmupDays)) \
		+r'$, R-sq$='+r'{0:.2f}'.format(HydroStats.correlation(s=Q_sim_Cal, o=Q_obs_Cal, warmup=WarmupDays)) \
		+r'$, $B='+r'{0:.2f}'.format(HydroStats.pc_bias2(s=Q_sim_Cal,o=Q_obs_Cal,warmup=WarmupDays)) \
		+r'$ %'
ax1.text(0.3, 0.93, statsum, verticalalignment='top' \
         ,horizontalalignment='left', transform=ax1.transAxes)


# FIGURE OF XY scatter plot
ax2 = plt.subplot(gs[4:7,0:3])
t = np.arange(len(Q_sim_Cal))
print(t.shape)
qmax = max(np.max(Q_sim_Cal),np.max(Q_obs_Cal)) * 1.1

#ax2.plot(Q_sim_Cal,'r',Q_obs_Cal,'b')
#ax2.scatter(Q_obs_Cal, Q_sim_Cal, c=t, cmap='viridis_r')
ax2.scatter(Q_obs_Cal, Q_sim_Cal, c='blue')
	
ax2.plot([0,qmax],[0,qmax], 'r--')
ax2.set_title('(b) Scatterplot for calibration period')
plt.xlim([0,qmax])
plt.ylim([0,qmax])
plt.ylabel(r'Sim. Streamflow [m$^{3}$ s$^{-1}$]')
plt.xlabel(r'Obs. Streamflow [m$^{3}$ s$^{-1}$]')


# FIGURE OF MONTHLY CLIMATOLOGY FOR CALIBRATION PERIOD
Q_obs_clim_Cal = np.zeros(shape=(12,1))*np.NaN
Q_sim_clim_Cal = np.zeros(shape=(12,1))*np.NaN
Q_obs_clim_Cal_stddev = np.zeros(shape=(12,1))*np.NaN
Q_sim_clim_Cal_stddev = np.zeros(shape=(12,1))*np.NaN
for month in range(1,13):
	mask = ~np.isnan(Q_obs_Cal) & ~np.isnan(Q_sim_Cal)
	Q_obs_clim_Cal[month-1] = np.mean(Q_obs_Cal[(Dates_Cal.month==month) & mask])
	Q_sim_clim_Cal[month-1] = np.mean(Q_sim_Cal[(Dates_Cal.month==month) & mask])
	Q_obs_clim_Cal_stddev[month-1] = np.std(Q_obs_Cal[(Dates_Cal.month==month) & mask])
	Q_sim_clim_Cal_stddev[month-1] = np.std(Q_sim_Cal[(Dates_Cal.month==month) & mask])

ax3 = plt.subplot(gs[4:7,4:7])
months = np.array([9,10,11,12,1,2,3,4,5,6,7,8,9,10]) # water year
ax3.fill_between(np.arange(0,14),(Q_sim_clim_Cal[months-1]+0.5*Q_sim_clim_Cal_stddev[months-1]).reshape(-1),(Q_sim_clim_Cal[months-1]-0.5*Q_sim_clim_Cal_stddev[months-1]).reshape(-1),facecolor='red',alpha=0.1,edgecolor='none')
ax3.fill_between(np.arange(0,14),(Q_obs_clim_Cal[months-1]+0.5*Q_obs_clim_Cal_stddev[months-1]).reshape(-1),(Q_obs_clim_Cal[months-1]-0.5*Q_obs_clim_Cal_stddev[months-1]).reshape(-1),facecolor='blue',alpha=0.1,edgecolor='none')
ax3.plot(range(0,14),Q_sim_clim_Cal[months-1],'r',range(0,14),Q_obs_clim_Cal[months-1],'b')
ax3.set_title('(c) Monthly $Q$ climatology cal.\ period')
#leg2 = ax3.legend(['121', '122','343','334'], loc='best', fancybox=True, framealpha=0.5,prop={'size':12},labelspacing=0.1)
plt.xticks(range(0,14), months)
plt.xlim([0.5,12.5])
plt.ylabel(r'Streamflow [m$^3$ s$^{-1}$]')
plt.xlabel(r'Month')
leg = ax3.legend(['Simulated', 'Observed'], fancybox=True, framealpha=0.8,prop={'size':12},labelspacing=0.1)
leg.get_frame().set_edgecolor('white')


fig.subplots_adjust(left=0.2, bottom=0.1, right=0.9, top=0.99, \
                    wspace=0.2, hspace=1.2)
plt.draw()

#gs.tight_layout(fig,rect=[0,0.03,1,0.95]) #pad=0.1, w_pad=0.1, h_pad=0.1
	
fig.set_size_inches(12, 6.5)

fig.savefig('FIGURES2_summary.png', dpi=400, format='PNG')
fig.savefig('FIGURES2_summary.pdf')

#plt.close("all")
