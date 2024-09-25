# -*- coding: utf-8 -*-
"""
Created on Fri Oct 15 12:39:41 2021

@author: lzh_6
"""
import csv
import sys,os
import pandas as pd
from joblib import Parallel, delayed

# scan CSV files in the root path


root= 'D:/day-ahead_data/curve'
file_list = []
for path, subdirs, files in os.walk(root):
    for name in files:
        file_list.append(os.path.join(path, name))

def gen_csv(n):

    ds_path = file_list[n]
    f = open(ds_path, "r",encoding="latin-1")
    lines = f.readlines()[2:-1]
    f.close()
    
      
    name_list=lines[0].replace('\n', '')
    name_list= name_list.split(";")[:-1]
    df = pd.DataFrame(columns=name_list)
    
    for i in range(1,len(lines)):
        d =lines[i].replace('\n', '')
        d = d.replace(".","")
        d = d.replace(",",".")
        df.loc[i-1] = d.split(";")[:-1]
        
    
    
    save_path = os.path.join('D:\day-ahead_data\curve_csv', ds_path[-10:-2]+".csv")
    
    df.to_csv(save_path,index=False,encoding="utf-8")

Parallel(n_jobs=10)(delayed(gen_csv)(n) for n in range(18,2024))


import pandas as pd
import glob
import os

path = r'D:\day-ahead_data\curve_csv' # use your path
all_files = glob.glob(os.path.join(path, "*.csv"))

li = []

for filename in all_files:
    print(filename)
    df = pd.read_csv(filename, index_col=None, header=0)
    li.append(df)

frame = pd.concat(li, axis=0, ignore_index=True)
frame.to_csv(r'D:\day-ahead_data\day_ahead.csv',index=False,encoding="utf-8")


import pandas as pd

data = pd.read_csv(r'D:\day-ahead_data\day_ahead.csv', nrows=10)

