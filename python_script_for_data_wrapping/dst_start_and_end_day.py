# -*- coding: utf-8 -*-
"""
Created on Sat Mar  6 16:08:56 2021

@author: lzh_6
"""
from dateutil.rrule import rrule, WEEKLY
from dateutil.rrule import SU as Sunday
import datetime

def get_last_sunday(year, month):
    date = datetime.datetime(year=year, month=month, day=1)
    # we can find max 5 sundays in a months
    days = rrule(freq=WEEKLY, dtstart=date, byweekday=Sunday, count=5)
    # Check if last date is same month,
    # If not this couple year/month only have 4 Sundays
    if days[-1].month == month:
        return days[-1]
    else:
        return days[-2]

def get_march_switch(year):
    # Get 5 next Sundays from first March
    day = get_last_sunday(year, 3)
    return day.day

def get_october_switch(year):
    day = get_last_sunday(year, 10)
    return day.day


def dst_start_day(year):
    
    dst_start_day = get_march_switch(year)
    dst_start_day = str(dst_start_day) + '/03/' + str(year)
    return dst_start_day
    
def dst_end_day(year):   
    dst_end_day =  get_october_switch(year)
    dst_end_day = str(dst_end_day) + '/10/' + str(year)
    return dst_end_day
    
   