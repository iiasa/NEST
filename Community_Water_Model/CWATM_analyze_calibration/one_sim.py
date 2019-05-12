# -*- coding: utf-8 -*-
"""
Created on Mon Jun 18 09:25:28 2018

@author: wangy

One simulation of the CWATM model.

Methods:
    - read((sim_path), ref_date = '1990-01-01')
        ---- read the "discharge_daily.tss" or "discharge_monthavg.tss" file.
        ---- if there is no existing pandas dataframe of streamflow, create 
             a pandas dataframe of the streamflow index by datetime.
        ---- if there is already an existing pandas dataframe of [sim, obs], 
             replace the sim in the dataframe.
    - read_obs((obs_path), realign = int or 'Ignore' or 'MS' or 'ME')
        ---- read the observed streamflow file, which must be a .csv, with the 
             1st column being the datetime, and the second column the observed
             streamflow.
        ---- match the observed streamflow to the simulated streamflow; if the 
             simulated streamflow does not exist, throw error.
        ---- the obs will be added to the same dataframe as the sim. If the 
             dates of the obs and sim do not match, use 'realign' to move the
             dates in the obs forward (positive int) or backward (negative int). 
             Set realign = 'Ignore' to ignore the dates from the obs file. Set
             realign = 'MS' or 'ME' to shift the dates of the obs to the 
             beginning/end of month.
    - performance(@metric(sim, obs), resample = @func([sim, obs]) )
        ---- use the @metric function to compute a performance metric between
             the observed and simulated streamflow. The function @metric must
             take two arguments, the first argument being sim and the second
             being obs. Usually, @metric is a function from hydrostats. 
        ---- if resample = @func, use the @func to operate on the dataframe of
             sim and obs to obtain another dataframe, and then call @metric to
             calculate the metric of performance. Usually, the @func is a 
             resample or groupby function to select the time period of
             evaluation. 
    - read_params(params = 'name' or ['name1', 'name2', ...])
        ---- parse the values of the parameter(s) from the settings file of 
             this simulation.
"""
import pandas as pd
import numpy as np
import configparser as cp

class one_sim:
    #
    def __init__(self, sim_path, obs_path, settings_path):
        # (sim_path = the full path to the simulated .tss streamflow)
        self.sim_path = sim_path
        # (obs_path = the full path to the simulated .tss streamflow)
        self.obs_path = obs_path
        # (settings_path = the full path to the settings.ini file to this
        #  simulation)
        self.settings_path = settings_path

        self.sim_obs = None
        self.params = {}

    #
    def read(self, ref_date = '1989-12-31'):
        # (read the simulated .tss streamflow to a pandas dataframe)
        temp = pd.read_csv(self.sim_path, sep = '\s+', header = None, \
                           skiprows = 1, names = ['Time', 'Streamflow'])
        temp.set_index('Time', inplace=True)
        # (drop documentation rows)
        temp = temp.loc[[~np.isnan(x) for x in temp['Streamflow']], :]
        temp.index = [int(x) for x in temp.index]

        # (convert to datetime index)
        temp.index = pd.to_datetime(temp.index, unit = 'D', \
                                    origin = ref_date)

        if (type(self.sim_obs) == type(None)):
            self.sim_obs = pd.DataFrame(data = np.nan, index = temp.index, \
                                        columns = ['Sim', 'Obs'])
            self.sim_obs.index.name = 'Time'

        self.sim_obs.loc[:, 'Sim'] = temp

    #
    def read_obs(self, realign = None):
        if (type(self.sim_obs) == type(None)):
            print("Need to read simulation before adding the observation.")
            raise

        # (read the observed streamflow file in .csv)
        temp = pd.read_csv(self.obs_path, sep = ',', header = 0, 
                           names = ['Time', 'Streamflow'])
        temp.set_index('Time', inplace=True)
        temp.index = pd.to_datetime(temp.index)

        # (align dates if needed)
        if (type(realign) == int):
            temp.index = temp.index.shift(realign, freq = 'D')
        elif (type(realign) == str):
            if (realign == 'Ignore'):
                temp.index = self.sim_obs.index
            elif (realign == 'MS'):
                temp.index -= pd.offsets.MonthEnd(0)
            elif (realign == 'ME'):
                temp.index += pd.offsets.MonthEnd(0)
        self.sim_obs.loc[:, 'Obs'] = temp

    #
    def performance(self, metric, resample = None):
        if (type(resample) != type(None)):
            new_sim_obs = resample(self.sim_obs)
        else:
            new_sim_obs = self.sim_obs

        return metric(new_sim_obs['Sim'], new_sim_obs['Obs'])

    #
    def read_params(self, params):
        # (convert params to lower case because Config.items(ss) returns all
        #  lower case)
        params2 = [x.lower() for x in params]

        Config = cp.ConfigParser()
        Config.read(self.settings_path)

        if (type(params) == str):
            for ss in Config.sections():
                self.params[params] = Config.get(ss, params)
        elif (type(params) == list):
            for ss in Config.sections():
                for xx in Config.items(ss):
                    if (xx[0] in params2):
                        self.params[xx[0]] = Config.get(ss, xx[0])

        return self.params