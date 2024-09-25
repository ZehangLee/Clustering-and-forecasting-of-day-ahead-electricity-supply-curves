# -*- coding: utf-8 -*-
"""
Created on Mon Mar  1 12:54:29 2021

@author: lzh_6
"""

#!/usr/bin/env python
# coding: utf-8




from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import StaleElementReferenceException
import re
from selenium.webdriver.chrome.options import Options
import time
import datetime as dt
import os
#from seleniumwire import webdriver


# automatically creating download path corresponding to the parameters 
def auto_allocate_download_path(market,
                                date, 
                                hour, 
                                tertiary_res_dir,
                                download_parent_path):
    add_market_to_download_path = re.sub(' ', '-',market)
    add_date_to_download_path = '_' + re.sub('/', '-', date)
    add_hour_to_download_path = '_' + str(hour)
    add_dir_to_download_path = '_' + tertiary_res_dir if market == 'Tertiary reserve' else ''
    
    download_path= os.path.join(download_parent_path, add_market_to_download_path) + add_date_to_download_path  + add_hour_to_download_path  + add_dir_to_download_path
    #download_parent_path + add_market_to_download_path + add_date_to_download_path  + add_hour_to_download_path  + add_dir_to_download_path
    return download_path

# initializing the selenium session and conducting download
def download_esios(driver_path, 
                   market, 
                   date, 
                   hour, 
                   tertiary_res_dir,
                   dst_offset,
                   download_parent_path,
                   timeout = 50):
    

    # auth_options = {
    #     'proxy': {
    #         'http': 'http://zliaqj:kK0gNwb6@51.91.195.142:29842', 
    #         'https': 'https://zliaqj:kK0gNwb6@51.91.195.142:29842',
    #         'no_proxy': 'localhost,127.0.0.1' # excludes
    #     }
    # }
    chrome_options = Options()
    auto_allocated_download_path = auto_allocate_download_path(market, date, hour, tertiary_res_dir,download_parent_path)
    prefs = {"download.default_directory": auto_allocated_download_path, "profile.default_content_settings.popups": 0,  "directory_upgrade": True}
    chrome_options.add_experimental_option('prefs', prefs)
    
    user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.50 Safari/537.36'
    chrome_options.add_argument(f'user-agent={user_agent}')

    #chrome_options.add_argument("--proxy-server=http://145.40.68.155:9480")
    driver = webdriver.Chrome(driver_path, options=chrome_options)
    # driver = webdriver.Chrome(driver_path, seleniumwire_options=auth_options, options=chrome_options) #
    url = 'https://www.esios.ree.es/en/supply-curves'
    driver.get(url)
    time.sleep(5)
    market_optionXPATH_dict = dict(secondary_res = '//*[@id="curveSelector_chzn_o_3"]',
                             tertiary_res = '//*[@id="curveSelector_chzn_o_4"]')
    market_optionXPATH = market_optionXPATH_dict['secondary_res'] if market == "Secondary Reserve" else market_optionXPATH_dict['tertiary_res']

    try:
        curve_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="curveSelector_chzn"]')))
        curve_dropdown_trigger.click()
    except TimeoutException:
        print("Timed out waiting for page to click [curveSelector]")
        driver.close()
        
    try:
        curve_select = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,market_optionXPATH)))
        curve_select.click()
    except TimeoutException:
        print("Timed out waiting for page to click [Hyperlink]")
        driver.close()


    market_index= int(Select(driver.find_element_by_id('curveSelector')).first_selected_option.get_attribute("value"))
    selected_market= "Tertiary reserve" if market_index == 31 else "Secondary reserve" if market_index == 29 else "Others"


    # Here we notice that the datepicker is a textbox. Therefore, instead of select the date from the it, we change the feature of datepicker and make it editable. More specifically, we drop the readonly feature of the datepicker.

    # need to click dateSelector two times, otherwise the date parameter will not be submitted correctly

    time.sleep(5)
    try:
        WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.ID,'dateSelector')))
        js_date_remove_readonly = "document.getElementById('dateSelector').removeAttribute('readonly')"
        driver.execute_script(js_date_remove_readonly)
        try:
            date_selector = driver.find_element_by_id('dateSelector')
            date_selector.send_keys(Keys.CONTROL, 'a')
            date_selector.send_keys(date)
        except StaleElementReferenceException:
            date_selector = driver.find_element_by_id('dateSelector')
            date_selector.send_keys(Keys.CONTROL, 'a')
            date_selector.send_keys(date)        
    except TimeoutException:
        print("Timed out waiting for page to load [dateSelector]")
        driver.close()

        
    ActionChains(driver).move_by_offset(10, 200).click().perform()

    selected_date= driver.find_element_by_xpath('//*[@id="dateSelector"]').get_attribute('value')

    # if the hour in the real world is equal to the value of the 'hour' parameter, then click the timeSelector one time
    # otherwise click two times (not double clicl, but submit click request two times)
    # when it is during the dst_start_day, the condition turns into "hour in the real world = the 'hour' parameter +1"
    # when it is during the dst_end_day, it becomes "hour in the real world = the 'hour' parameter +1"
    time.sleep(5)
    _cont = 0
    now_hour = dt.datetime.now().hour
    while True:
        try:
            WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.ID,'timeSelector')))
        except TimeoutException:
            print("Timed out waiting for page to load [timeSelector]")
            driver.close()
    
        try:
            toolbar_time_selector_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="datetimeSelector"]/div/div[2]')))
            toolbar_time_selector_dropdown_trigger.click()
        except TimeoutException:
            print("Timed out waiting for page to load [toolbar_time_selector]")
            driver.close()
    
        try:
            time_selector_tooltip_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="datetimeSelector"]/div/div[2]/div[2]')))
            time_selector_tooltip_dropdown_trigger.click()
        except TimeoutException:
            print("Timed out waiting for page to load [time_selector_tooltip]")
            driver.close()
    
    
        # html_list = driver.find_element(By.XPATH,"/html/body/div[3]/div[2]/div/div[2]/div/div/div/div[1]/div[2]/div/div[2]/div[2]/div/div/ul")
        # items = html_list.find_elements_by_tag_name("li")
        # max_hour = len(items)
    
    
        _setence= "/html/body/div[3]/div[2]/div/div[2]/div/div/div/div[1]/div[2]/div/div[2]/div[2]/div/div/ul/li[@id[contains(.,\'_chzn_o_" + str(hour) + "\')]]"

    
    
        driver.find_element(By.XPATH,_setence).click()
        
        _cont = _cont + 1
        
        if now_hour == hour + dst_offset or _cont >= 2:
            break
        

    selected_hour= driver.find_element_by_xpath('//*[@id="timeSelector"]').get_attribute('value')





    if (selected_market== "Tertiary reserve"):
        tertiary_res_dirXPATH_dict = dict(Up = '//*[@id="directionSelector_chzn_o_0"]',
                               Down = '//*[@id="directionSelector_chzn_o_1"]')
        tertiary_res_dirXPATH = tertiary_res_dirXPATH_dict['Up'] if tertiary_res_dir == "Up" else tertiary_res_dirXPATH_dict['Down']

        try:
            tertiary_reserve_direction_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="directionSelector_chzn"]')))
            tertiary_reserve_direction_dropdown_trigger.click()
        except TimeoutException:
            print("Timed out waiting for page to click [tertiary_reserve_direction_dropdown]")
            driver.close()

        try:
            direction_select = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,tertiary_res_dirXPATH)))
            direction_select.click()
        except TimeoutException:
            print("Timed out waiting for page to click [direction_select]")
            driver.close()
            
        time.sleep(10)
        
        download_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="supplyCurvesContent"]/div/div[1]/div[5]/div')))
        download_dropdown_trigger.click()

        csv_select = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="supplyCurvesContent"]/div/div[1]/div[5]/div/ul/li[1]/a')))
        csv_select.click()
        
        selected_tertiary_res_dir = driver.find_element_by_xpath('//*[@id="directionSelector"]').get_attribute('value')
    elif (selected_market== "Secondary reserve"):
        time.sleep(10)
        download_dropdown_trigger = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="supplyCurvesContent"]/div/div[1]/div[4]/div')))
        download_dropdown_trigger.click()

        csv_select = WebDriverWait(driver,timeout).until(EC.element_to_be_clickable((By.XPATH,'//*[@id="supplyCurvesContent"]/div/div[1]/div[4]/div/ul/li[1]/a')))
        csv_select.click()
        
    else:
        print("Wrong market")


    selected_market_direction = '' if selected_market == 'Secondary reserve' else selected_tertiary_res_dir
    
    
    time.sleep(10)
    driver.close()
    return {'selected_market' : selected_market,
            'selected_hour' : selected_hour,
            'selected_date' : selected_date,
            'selected_market_direction' : selected_market_direction}








