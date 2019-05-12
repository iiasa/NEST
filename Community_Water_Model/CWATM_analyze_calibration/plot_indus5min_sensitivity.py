# -*- coding: utf-8 -*-
"""
Created on Mon Jun 18 13:54:07 2018

@author: wangy

Use the DEAP-calibration results to analyze the sensitivity of the streamflow
performance, measured by the mean bias in June, July, August.
"""
import random
from one_sim import one_sim
import os
import hydrostats as hs
import pandas as pd
import matplotlib.pyplot as plt


params = ['SnowMeltCoef', 'crop_correct', 'IceMeltCoef', \
          'preferentialFlowConstant', 'arnoBeta_add', 'factor_interflow', \
          'recessionCoeff_factor', 'runoffConc_factor', 'manningsN', \
          'normalStorageLimit', 'lakeAFactor', 'lakeEvaFactor']
fileslist = [('%02i' % xx) + '_' + ('%03i' % yy) for xx in range(1,31) \
             for yy in range(1,33)]
##fileslist = [('%02i_' % xx) + ('%03i' % yy) for xx in range(0,2) for yy in range(1,33)]
calibration_path = os.path.join('..', 'calibration', 'calibration_indus5min')
##calibration_path = os.path.join('..', 'calibration', 'calibration_indus5min_2018-06-18')


random.seed(1000)

files_look = random.sample(fileslist, 30)

params_val = pd.DataFrame(data = 0., index = params, columns = files_look)
metric = pd.DataFrame(data = 0., index = files_look, columns = ['Mean Bias'])
for ff in files_look:
    sim = one_sim(os.path.join(calibration_path, 'catchments', ff, \
                               'discharge_monthavg.tss'), \
                  os.path.join(calibration_path, 'observed_data', \
                               'besham_1995.csv'), 
                  os.path.join(calibration_path, 'catchments', ff, \
                               'settings5min-Run' + ff + '.ini'))
    sim.read(ref_date = '1989-12-31')
    sim.read_obs(realign = 'ME')

    ##def subset(df):
        ##return df.loc[(df.index.month >= 3) & (df.index.month <= 5), :]
    def subset(df):
        return df.loc[(df.index.month >= 6) & (df.index.month <= 8), :]

    metric.loc[ff, 'Mean Bias'] = sim.performance(hs.me, subset)

    run_params = pd.DataFrame.from_dict(sim.read_params(params), \
                                        orient='index', \
                                        columns = ['none'])['none']
    run_params.index = params
    run_params = run_params.astype(float)

    params_val.loc[run_params.index, ff] = run_params.values


fig, axes = plt.subplots(3, 4, sharex = False, sharey = True, \
                         figsize = [12, 8])
fig.subplots_adjust(hspace = 0.3)
axes = axes.flat
for ii in range(len(params)):
    axes[ii].scatter(params_val.iloc[ii, :], metric)
    axes[ii].set_xlabel(params[ii])
axes[4].set_ylabel('Mean Bias in JJA Streamflow (m$^3$/s)')
##axes[4].set_ylabel('Mean Bias in MAM Streamflow (m$^3$/s)')
# fig.savefig('plot_indus5min_sensitivity.png')