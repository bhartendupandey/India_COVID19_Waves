import numpy as np
import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
from scipy.signal import savgol_filter, find_peaks
import math, os
from functools import reduce
from datetime import date, timedelta


os.chdir(r'F:\Projects\Health\COVID-19\Script_for_paper_v1\coviddataprocessingcodeanddata\dailymetrics')
from Calculate_WaveMetrics import *


def check_params(qvalue,dvalue):
    censusshp = gpd.read_file(r"F:\Projects\Health\COVID-19\Data\Intermediate\Shapefiles\COVID_CENSUS_District_Conf_Edited.shp")
    censusshp['State'] = censusshp['NAME'] +'__' +censusshp['STATE_UT']
    censusshp.drop(['C_CODE11', 'NAME', 'R_TOT_POP', 'STATE_UT', 'TOTPOP',
           'TOT_POP', 'U_TOT_POP', 'geometry'],axis=1,inplace=True)
    Confirmed = censusshp.melt(id_vars="State",var_name="Date",value_name="Confirmed")
    censusshp = gpd.read_file(r"F:\Projects\Health\COVID-19\Data\Intermediate\Shapefiles\COVID_CENSUS_District_Death_Edited.shp")
    censusshp['State'] = censusshp['NAME'] +'__' +censusshp['STATE_UT']
    censusshp.drop(['C_CODE11', 'NAME', 'R_TOT_POP', 'STATE_UT', 'TOTPOP',
           'TOT_POP', 'U_TOT_POP', 'geometry'],axis=1,inplace=True)
    Death = censusshp.melt(id_vars="State",var_name="Date",value_name="Deceased")
    
    
    covid_df = Confirmed.merge(Death,on=["State","Date"],how="left")
    dateind=pd.to_datetime(covid_df["Date"], infer_datetime_format=True) 
    covid_df = covid_df.set_index(dateind)
    
    
    # data processing and calculate metrics
    cleanedList = [x for x in list(covid_df["State"]) if str(x) != 'nan']
    state_name_list = set(cleanedList)
    
    admin_list = []
    for state_name in state_name_list:
        covid_admin_df = covid_df[covid_df["State"] == state_name].copy()
        #if (covid_admin_df.Confirmed.sum() > 0) & (covid_admin_df.Deceased.sum() > 0):
        if (covid_admin_df.Confirmed.sum() > 0):
            # covid_df.to_csv(csv_file.replace("State", state_name))
        
            # 2-pre-processing the data
            # > remove the negative and interpolate the values
            df_interpolate = interpolate_negative(covid_admin_df)
        
            # > remove the data outliers
            df_outlier = remove_outlier(df_interpolate)
        
            # > smooth the time series data
            smoothed_df = smooth(df_outlier, window_size="7d")
        
            # 2- calculate the metrics from the wave data
            # > find peaks
            peak_df = find_wave_peaks(smoothed_df,covid_admin_df,qvalue,dvalue)
        
            # > get the starting date
            starting_date = get_start_date(covid_admin_df, smoothed_df, peak_df)
        
            # > get the ending date
            ending_date = get_end_date(covid_admin_df, smoothed_df, peak_df)
        
            # 3- postprocessing for output
            # > merge the metrics
            data_frames = [peak_df, starting_date, ending_date]
            metrics = reduce(lambda left, right: pd.merge(left, right, on=['State']), data_frames)
            admin_list.append(metrics)
    
    # concat the metrics of each admin unit into one table
    metrics_table = pd.concat(admin_list, axis=0, ignore_index=True)
    # reorder the column names
    metrics_table = reorder_column_names(metrics_table)
    return(metrics_table)

#metrics_table.to_csv(r"F:\Projects\Health\COVID-19\Data\Intermediate\TimeseriesMetrics_Confirmed_Deaths.csv")

## Find optimal parameters to identify a maximum of 4 and a minimum of 2 waves.

params_df = pd.DataFrame(columns=["index", "Confirmed_NPeaks","qval","dval","ID"])


qval_ = [0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95]
dval_ = [4]
id_ = 0
for ii  in qval_:
    for jj in dval_:
        id_ = id_ + 1
        metrics_table = check_params(ii,jj)
        aa = metrics_table.Confirmed_NPeaks.value_counts().reset_index()
        aa["qval"] = ii
        aa["dval"] = jj
        aa["ID"] = id_
        params_df = params_df.append(aa,ignore_index=True)

params_df.head()
params_df.to_csv(r"F:\Projects\Health\COVID-19\Data\Intermediate\calc_metrics_optim_4.csv")

## 0.75 and 4 are the select parameters.

metrics_table = check_params(0.75,4)
metrics_table.to_csv(r"F:\Projects\Health\COVID-19\Data\Intermediate\TimeseriesMetrics_Confirmed.csv")

#

