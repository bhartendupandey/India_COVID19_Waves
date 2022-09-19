# import required libraries
import numpy as np
import pandas as pd
import math
from datetime import timedelta
from scipy.signal import savgol_filter, find_peaks

# temporary
import matplotlib.pyplot as plt


# fill the negative value with 0 then interpolate the value using daily data
def interpolate_negative(df):
    # fill negative value with nan
    #for filed in ["Confirmed", "Deceased"]:
    for filed in ["Confirmed"]:
        df[df[filed] < 0] = np.nan

    # interpolate nan using linear method
    df = df.interpolate(method="linear", imit_direction='forward', axis=0)
    return df


# remove the outlier data
def remove_outlier(df, window_size="14d", q1=0.1, q3=0.9, factor=3):
    # calculate the quartiles
    #for filed in ["Confirmed", "Deceased"]:
    for filed in ["Confirmed"]:
        Q1 = df[filed].rolling(window_size).quantile(q1)
        Q3 = df[filed].rolling(window_size).quantile(q3)
        IQR = Q3 - Q1
        outlier_positions = ((df[filed] < (Q1 - factor * IQR)) | (df[filed] > (Q3 + factor * IQR)))

        # remove
        try:
            first_date = outlier_positions.index[outlier_positions == True][0]

            outlier_positions[first_date] = False

            df.loc[outlier_positions, filed] = np.nan
            df[filed] = df[filed].interpolate(method="linear", imit_direction='forward', axis=0)
        except IndexError:
            pass
    return df


# smooth the daily wave data
def smooth(df, window_size="7d"):
    def wave_filter(x, step_size):
        step_size = int(step_size.replace("d", ""))
        # print(x)
        return savgol_filter(x, window_length=step_size, polyorder=2, axis=0)

    #for field in ["Confirmed", "Deceased"]:
    for field in ["Confirmed"]:
        temp_array = df[field].to_numpy()
        filtered_temp = wave_filter(temp_array, window_size)
        df[field] = filtered_temp

    return df


# find the wave peaks, return the peak's data and value

def calc_peaks(vec_data,qvalue,dvalue):
    vec_data = vec_data.copy()
    # get height (val at 75% threshold) and dist between waves
    height = np.nanquantile(vec_data, q=qvalue)#0.75
    distance = math.ceil(len(vec_data) / dvalue)#4

    # calc num peaks
    peaks_position, peaks_height = find_peaks(vec_data, height=height, distance=distance)

    if len(peaks_position):☻
        return peaks_position, peaks_height
    else:
        return 0

def find_wave_peaks(smoothed_df, orginal_df,qvalue,dvalue):
    # function to calculate the peaks

    peak_dic = {"State": smoothed_df["State"][0]}

    #for field in ["Confirmed", "Deceased"]:
    for field in ["Confirmed"]:
        temp_array = smoothed_df[field].to_numpy()
        peaks_date, peak_height = calc_peaks(temp_array,qvalue,dvalue)
        peak_dic[field + "_NPeaks"] = len(peaks_date)
        peaks = {}
        for i in range(len(peaks_date)):
            peak_date = smoothed_df.first_valid_index() + timedelta(days=int(peaks_date[i]))
            peaks[field + "_PDate_" + str(i + 1)] = peak_date
            peaks[field + "_PHeight_" + str(i + 1)] = orginal_df.loc[peak_date, field]

        peak_dic.update(peaks)

    # transform into table
    peak_df = pd.DataFrame([peak_dic])

    return peak_df


# get the starting dates of a wave consisting of multiple peaks
def get_start_date(original_df, smoothed_df, peak_df):
    start_date_dic = {"State": smoothed_df["State"][0]}
    #for field in ["Confirmed", "Deceased"]:
    for field in ["Confirmed"]:
        num_peaks = peak_df[field + "_NPeaks"][0]
        temp_dic = {}
        for i in range(num_peaks):

            # get the starting date of first wave
            if i == 0:
                peak_date = peak_df[field + "_PDate_" + str(i + 1)][0]
                df = original_df.loc[:peak_date]
                start_date = df[df[field] != 0].first_valid_index()
                temp_dic[field + "_StartDate_" + str(i + 1)] = start_date

            # get other starting date
            elif i > 0:
                last_peak_date = peak_df[field + "_PDate_" + str(i)][0]
                this_peak_date = peak_df[field + "_PDate_" + str(i + 1)][0]
                between_peaks_df = smoothed_df.loc[last_peak_date:this_peak_date].copy()
                between_peaks_df[field] = between_peaks_df[field] - between_peaks_df[field].min()
                threshold = between_peaks_df.loc[this_peak_date, field] * 0.05
                start_date = between_peaks_df[between_peaks_df[field] <= threshold].last_valid_index()
                temp_dic[field + "_StartDate_" + str(i + 1)] = start_date
            start_date_dic.update(temp_dic)
    start_date_df = pd.DataFrame([start_date_dic])

    return start_date_df


# get the starting dates of a wave consisting of multiple peaks
def get_end_date(original_df, smoothed_df, peak_df):
    end_date_dic = {"State": smoothed_df["State"][0]}
    # num_peaks = peak_df["NPeaks"][0]
    #for field in ["Confirmed", "Deceased"]:
    for field in ["Confirmed"]:
        num_peaks = peak_df[field + "_NPeaks"][0]
        temp_dic = {}
        try:
            for i in range(num_peaks):
                last_peak_date = peak_df[field + "_PDate_" + str(i + 1)][0]
                this_peak_date = peak_df[field + "_PDate_" + str(i + 2)][0]
                between_peaks_df = smoothed_df.loc[last_peak_date:this_peak_date].copy()
                between_peaks_df[field] = between_peaks_df[field] - between_peaks_df[field].min()
                threshold = between_peaks_df.loc[last_peak_date, field] * 0.05
                end_date = between_peaks_df[between_peaks_df[field] <= threshold].first_valid_index()
                temp_dic[field + "_EndDate_" + str(i + 1)] = end_date
        except KeyError:
            pass
        end_date_dic.update(temp_dic)
    end_date_df = pd.DataFrame([end_date_dic])
    return end_date_df


# reorder the column names
#def reorder_column_names(metrics_df):
#    maximum_confirmed_peaks = metrics_df["Confirmed_NPeaks"].max()
#    maximum_deceased_peaks = metrics_df["Deceased_NPeaks"].max()
#    column_names = ["State", "Confirmed_NPeaks"]
#    for i in range(maximum_confirmed_peaks):
#        column_names += ["Confirmed_StartDate_" + str(i + 1), "Confirmed_PDate_" + str(i + 1),
#                         "Confirmed_PHeight_" + str(i + 1), "Confirmed_EndDate_" + str(i + 1)]
#    column_names.remove(column_names[-1])
#    column_names += ["Deceased_NPeaks"]
#    for i in range(maximum_deceased_peaks):
#        column_names += ["Deceased_StartDate_" + str(i + 1), "Deceased_PDate_" + str(i + 1),
#                         "Deceased_PHeight_" + str(i + 1), "Deceased_EndDate_" + str(i + 1)]
#    column_names.remove(column_names[-1])
#    metrics_df = metrics_df[column_names]
#
#    return metrics_df

def reorder_column_names(metrics_df):
    maximum_confirmed_peaks = metrics_df["Confirmed_NPeaks"].max()
    column_names = ["State", "Confirmed_NPeaks"]
    for i in range(maximum_confirmed_peaks):
        column_names += ["Confirmed_StartDate_" + str(i + 1), "Confirmed_PDate_" + str(i + 1),
                         "Confirmed_PHeight_" + str(i + 1), "Confirmed_EndDate_" + str(i + 1)]
    column_names.remove(column_names[-1])
    metrics_df = metrics_df[column_names]
    return metrics_df
