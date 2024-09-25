# -*- coding: utf-8 -*-
"""
Created on Mon Mar  8 13:00:23 2021

@author: lzh_6
"""
#------------------------------------------------------
# Required Input:
#   driver_path: path of the selenium driver
#   download_parent_path: path to a filefolder aiming to save all download files
#   executor: number of thread used to perform asychronous download
#   sdate_year, sdate_month, sdate_day: year, month, day of the start date
#   edate_year, edate_month, edate_day: year, month, day of the end date
#   market:
#   tertiary_res_dir: direction of the tertiary market
#------------------------------------------------------


import time
from concurrent.futures.thread import ThreadPoolExecutor
import asynchronous_download_process as async_download

import os

from datetime import date, timedelta
import pandas as pd
import numpy as np
from itertools import cycle
from dst_start_and_end_day import dst_start_day, dst_end_day


driver_path = r'D:\chromedriver\chromedriver.exe'
download_parent_path = r'D:\test' 
executor = ThreadPoolExecutor(8)


try:
    os.system("taskkill /f /im chromedriver.exe /t")
except:
    pass

sdate_year = 2016
sdate_month = 5
sdate_day = 23


edate_year = 2016
edate_month = 5
edate_day = 23

market = 'Tertiary reserve' #Secondary Reserve or Tertiary reserve
tertiary_res_dir = 'Down' # If market = 'Secondary Reserve' then '', otherwise Up or Down


sdate = date(sdate_year, sdate_month, sdate_day)   # start date
edate = date(edate_year, edate_month, edate_day)   # end date


delta = edate - sdate       # as timedelta
date_list = []
# all date in the interval
for i in range(delta.days + 1):
    day = sdate + timedelta(days=i)
    date_list.append(day.strftime('%d/%m/%Y'))

# generating parameters dataframe df
df = pd.DataFrame()    
df["date"] = np.repeat(date_list, 24)
hour_list = cycle([i for i in range(24)])
df['hour'] = [next(hour_list) for hour in range(len(df))]
df['market'] = market
df['tertiary_res_dir'] = tertiary_res_dir
df['dst_offset'] = 0
df = df[['market', 'date', 'hour', 'tertiary_res_dir','dst_offset']]

# allocating all the dst(daylight saving time start) start days and end days

years_btw_sdate_edate = [year for year in range(sdate_year, edate_year)]

years = [sdate_year, *years_btw_sdate_edate, edate_year] 
years = list(set(years))


dst_start_day_list = list(map(dst_start_day, years))
if any(df['date'].isin(dst_start_day_list)) == True: 
    dst_start_day_index = df[(df['date'].isin(dst_start_day_list) ) & ( df['hour'] == 23)].index
    df.loc[df['date'].isin(dst_start_day_list),'dst_offset'] = 1
    df = df.drop(index= dst_start_day_index)
    



dst_end_day_list = list(map(dst_end_day, years))
if any(df['date'].isin(dst_end_day_list)) == True: 
    dst_end_day_addition_hour= pd.DataFrame({'market' : market,
                                      'date' : dst_end_day_list,
                                      'hour' : 24,
                                      'tertiary_res_dir' : tertiary_res_dir,
                                      'dst_offset' : 0})
    df = pd.concat([df, dst_end_day_addition_hour])
    df.loc[df['date'].isin(dst_end_day_list),'dst_offset'] = -1

#df = df[df['hour'].isin([22])]
paras_list = df.values.tolist()




# downloading files
download_files_path_list = async_download.async_download_event(driver_path,  paras_list, executor, download_parent_path)
wait_time = 40 + 10 * len(paras_list) 
time.sleep(wait_time)
times_redownload = 0
redownload_paras_list = paras_list
redownload_files_path_list = download_files_path_list

# if any downloading session fails, retry this session at maximum two times
# the programme prints the parameters corresponding to the failed session
while times_redownload < 2:
    redownload_paras_list = async_download.check_missing_files(redownload_paras_list,redownload_files_path_list)
    if len(redownload_paras_list) == 0:
        break
    else:
        print(redownload_paras_list)
        redownload_files_path_list = async_download.async_download_event(driver_path, redownload_paras_list, executor, download_parent_path)   
        wait_time =  40 + 10 * len(redownload_paras_list) 
        time.sleep(wait_time)
        #download_files_path_list = [*download_files_path_list, *redownload_files_path_list]
        times_redownload = times_redownload +1
        

wait_time =  20 * len(redownload_paras_list) 
time.sleep(wait_time)       
 
if times_redownload == 2:
    missing_files_paras_list = async_download.check_missing_files(paras_list,download_files_path_list)
    if len(missing_files_paras_list) >0:
        print('\x1b[0;30;41m' + 'Fail to download data w/ paras: \n' + '\n'.join('{}: {}'.format(*l) for l in enumerate(missing_files_paras_list)) + '\x1b[0m')
   