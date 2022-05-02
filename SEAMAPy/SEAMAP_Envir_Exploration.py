# -*- coding: utf-8 -*-
"""
Created on Sun May  1 14:51:56 2022

@author: Owner
"""


print("loading libraries")

import pandas as pd
import os
import geopy.distance
import numpy as np

os.chdir(r"D:\public_seamap_csvs")
envrec = pd.read_csv('ENVREC.csv', encoding='cp1252')


genera = {"snapper": "LUTJANU", 
              "shrimp": "PENAEUS", 
              "menhaden": "BREVOOR", 
              "seatrout": "CYNOSCI", 
              "drum": "SCIAENO", 
              "croaker": "MICROPO",
              "grouper" : "MYCTERO",
              "mackerel" : "SCOMBER",
              "anchovy" : "ANCHOA"}

gens = [genera[i] for i in list(genera.keys())]

data_dict = {}
cpue_dict = {}

for k in list(genera.keys()):
    tempdf = pd.read_csv("D:\public_seamap_csvs\\" + genera[k] + "_CPUE.csv")
    tempdf = tempdf.dropna().reset_index()
    
    arr = np.array(tempdf['CPUE_MTSQKM'].tolist())
    
    cpue_dict[k] = np.sum(arr[np.isfinite(arr)]) / tempdf.shape[0]
    
    data_dict[k] = pd.merge(tempdf, 
                            envrec, 
                            how="inner", 
                            on=["STATIONID"])
    
df = data_dict["grouper"] 

# bin into latitutde and longitude for spatial averaging
df["lat_round"] = [round(i,1) for i in df['Lat'].tolist()]
df["lon_round"] = [round(i,1) for i in df['Lon'].tolist()]
# use pandas groupby to spatially average by lat and lon
df = df.groupby(['lat_round', 'lon_round']).mean()

#df = df[df["CPUE_MTSQKM"]>0]


import matplotlib.pyplot as plt

"""
Index(['Unnamed: 0', 'GENUS', 'STATIONID', 'Lat', 'Lon', 'CPUE_MTSQKM',
       'ENVRECID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'CLD_TYPE',
       'CLD_COVER', 'SECCHI_DSK', 'WECOLOR', 'STA_LOC', 'PRECIP', 'DEPTH_ESRF',
       'DEPTH_EMID', 'DEPTH_EMAX', 'DEPTH_EWTR', 'TEMPSURF', 'TEMPMID',
       'TEMPMAX', 'SALSURF', 'SALMID', 'SALMAX', 'CHLORSURF', 'CHLORMID',
       'CHLORMAX', 'OXYSURF', 'OXYMID', 'OXYMAX', 'TURBSURF', 'TURBMID',
       'TURBMAX', 'COMENV', 'CTDFILE', 'LIGHT_CODE', 'LATITUDE', 'LONGITUDE'],
      dtype='object')
"""
plt.scatter((df['SALMAX']-df['SALSURF']), df['CPUE_MTSQKM'].tolist(), s=1)
plt.yscale('log')
plt.show()

import matplotlib.pyplot as plt
# Pie chart, where the slices will be ordered and plotted counter-clockwise:
labels = gens
sizes = [cpue_dict[k] for k in list(genera.keys())]

#explode = (0, 0.1, 0, 0)  # only "explode" the 2nd slice (i.e. 'Hogs')

fig1, ax1 = plt.subplots()
#ax1.pie(sizes, explode=explode, labels=labels, autopct='%1.1f%%',
ax1.pie(sizes,  labels=labels, autopct='%1.1f%%',
        shadow=True, startangle=90)
ax1.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

plt.show()

cpue = pd.DataFrame({'Fish' : list(cpue_dict.keys()),
                    'Mean CPUE MT/SQKM' : list(cpue_dict.values())})

cpue = cpue.sort_values(by=['Mean CPUE MT/SQKM'], ascending=False)
import seaborn as sns
sns.barplot(x='Fish', y="Mean CPUE MT/SQKM", data=cpue)
plt.xticks(rotation=45)