# -*- coding: utf-8 -*-
"""
Created on Mon Mar  1 14:07:58 2021

@author: lzh_6
"""


import numpy as np
import os
#from itertools import chain
from download_script import download_esios, auto_allocate_download_path
import asyncio


import nest_asyncio
nest_asyncio.apply()


# dowmloading the files in an asynchronous way
def async_download_event(driver_path, 
                         download_paras_list, 
                         executor, 
                         download_parent_path= 'D:\Script\Python_script\esios_data'):
    download_files_path_list = []
    #executor = ThreadPoolExecutor(4)
    
    len_download_paras_list = len(download_paras_list)
    
    loop = asyncio.get_event_loop()
    for _iter in range(len_download_paras_list):
        curr_market = download_paras_list[_iter][0]
        curr_date = download_paras_list[_iter][1]
        curr_hour = download_paras_list[_iter][2]
        curr_tertiary_res_dir = download_paras_list[_iter][3]
        curr_dst_offset = download_paras_list[_iter][4]
        download_files_path = auto_allocate_download_path(curr_market, curr_date, curr_hour, curr_tertiary_res_dir, download_parent_path)
        download_files_path_list.append(download_files_path)
        loop.run_in_executor(executor, download_esios, driver_path, curr_market, curr_date, curr_hour, curr_tertiary_res_dir,curr_dst_offset,download_parent_path)
    
    loop.run_until_complete(asyncio.gather(*asyncio.all_tasks(loop)))
    return download_files_path_list
    
# checking whether the filefolder exists 
# cheching whether the filefolder is empty
def check_missing_files(download_paras_list,download_files_path_list):
    _temp_paras_list = []
    _con_file_folder_exsits = [os.path.exists(file) for file in download_files_path_list]

    if all(_con_file_folder_exsits) == True:
        _con_file_in_folder = [len(os.listdir(file)) > 0 for file in download_files_path_list]
        
        if all(_con_file_in_folder) == True:
            
            print('\x1b[4;30;42m' + "All Files have been download successfully" + '\x1b[0m')
            pass
        else:
            redownload_paras = np.array(download_paras_list)[np.array(_con_file_in_folder) == False]
            redownload_paras = redownload_paras.tolist()
            #redownload_paras = list(chain.from_iterable(redownload_paras))
            _temp_paras_list.extend(redownload_paras)
    else:
        redownload_paras = np.array(download_paras_list)[np.array(_con_file_folder_exsits) == False]
        redownload_paras = redownload_paras.tolist()
        #redownload_paras = list(chain.from_iterable(redownload_paras))
        _temp_paras_list.extend(redownload_paras)
        
    return _temp_paras_list




     


        
























