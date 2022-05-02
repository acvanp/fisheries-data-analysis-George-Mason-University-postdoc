# -*- coding: utf-8 -*-
"""
Created on Sat Apr 30 18:08:41 2022

@author: Owner
"""

    
"""
for each species,
go row by row in invrec,
if there is an area trawled,
look for that fish species by station in GLFREC,
if they are present, get average length 
and plug into length-weight equation to get average weight in kg, convert to average MT (1000kg),
then multiply by CNTEXP to get weight of fish species per station
save that weight in one column in invrec,
then divide catch in MT by area in SQKM to get CPUE (MT/SQKM)

"""

print("loading libraries")

import pandas as pd
import os
import geopy.distance

os.chdir(r"D:\public_seamap_csvs")



genera = {"snapper": "LUTJANU", 
              "shrimp": "PENAEUS", 
              "menhaden": "BREVOOR", 
              "seatrout": "CYNOSCI", 
              "drum": "SCIAENO", 
              "croaker": "MICROPO",
              "grouper" : "MYCTERO",
              "mackerel" : "SCOMBER",
              "anchovy" : "ANCHOA"}

# centimeters to kg
len_AB = {"LUTJANU" : [0.00001, 3.076],
          "PENAEUS" : [0.00000005, 3],
          "BREVOOR" : [0.00000955, 3.12],
          "CYNOSCI" : [0.00000759, 3.04],
          "SCIAENO" : [0.00000832, 3.07],
          "MICROPO" : [0.00000832, 3.17],
          "MYCTERO" : [0.00000891, 3.05],
          "SCOMBER" : [0.00000912, 3.02],
          "ANCHOA" :  [0.00000537, 3.16]}

fish_type = "snapper"

#gen = genera[fish_type]

gens = [genera[i] for i in list(genera.keys())]

print("loading CSVs")

bgsrec = pd.read_csv('BGSREC.csv')

bgsrec = bgsrec[bgsrec['GENUS_BGS'].isin(gens)]

bgsrec = bgsrec.reset_index()

glfrec = pd.read_csv('GLFREC.csv')

glfrec = glfrec[glfrec['GENUS_GLF'].isin(gens)] 

glfrec = glfrec.reset_index()

starec = pd.read_csv('STAREC.csv', encoding='cp1252')

invrec = pd.read_csv('INVREC.csv', error_bad_lines=False)

#bgs_ids = bgsrec[bgsrec["GENUS_BGS"] == gen].drop_duplicates(['BGSID'])['BGSID'].tolist()

#glf_ids = glfrec[glfrec["GENUS_GLF"] == gen].drop_duplicates(['GLFID'])['GLFID'].tolist()

# calculate CPUE (MT/SQKM) area (SQKM) using info from starec but populate invrec

# gear codes for trawls
trawl_codes = ['BT', 'BB', 'FT', 'EF', 'FD', 'FE', 'HO', 'MT', 'PT', 'SN', 'SH', 'ST', 'ES', 'SM', '*T', 'TN', 'TT', 'KT']

area_invrec_list = []

invrec = invrec[invrec['GEAR_TYPE'].isin(trawl_codes)]
invrec = invrec.reset_index()

starec = starec[starec['STATIONID'].isin(invrec['STATIONID'].tolist())]
starec = starec.dropna(subset=['S_LATD','S_LATM','S_LOND','S_LONM','E_LATD','E_LATM','E_LOND','E_LONM'])
satrec = starec.sort_values(by=['STATIONID'])
starec = starec.reset_index(drop=True)

# aggregate invrec for only stationid in starec and get sum of net widths
invrec = invrec[invrec["STATIONID"].isin(starec['STATIONID'].tolist())].groupby("STATIONID").sum().sort_values(by='STATIONID').reset_index()

print("getting net widths")
# feet across divide by 3.2 to get meters / divide by 1000 to get km
starec['netwidths_km'] = invrec['GEAR_SIZE'].tolist()[0] / 3.28084 / 1000

#starec = starec.reset_index()

slat = starec['S_LATD'] + starec['S_LATM'] / 60
slon = starec['S_LOND'] + starec['S_LONM'] / 60
elat = starec['E_LATD'] + starec['E_LATM'] / 60
elon = starec['E_LOND'] + starec['E_LONM'] / 60

print("getting trawl distances")

distances = []
for w,x,y,z in zip(slat,slon,elat,elon):
    # need to specify km for distance
    distances.append(geopy.distance.geodesic((w,x),(y,z)).km)

starec['distances_km'] = distances

starec['area_trawled'] = starec['netwidths_km'] * starec['distances_km']
starec['area_trawled'] = starec['area_trawled'].tolist()
#starec = starec[starec['area_trawled'] > 0] # what to do with zero area trasled
starec = starec.reset_index()
#cpue_list = []
#A = len_AB[fish_type][0]
#B = len_AB[fish_type][1]

print("aggregating GLFREC and BGSREC")

glfrec_agg = glfrec.groupby(["GENUS_GLF", "STATIONID"]).mean() # mean length in mm
glfrec_agg = glfrec_agg.sort_index()

bgsrec_agg = bgsrec.groupby(["GENUS_BGS", "STATIONID"]).sum() # sum of count-extrapolated CNTEXP
bgsrec_agg = bgsrec_agg.sort_index()

glfrec_agg = glfrec_agg[glfrec_agg.index.isin(bgsrec_agg.index)]
bgsrec_agg = bgsrec_agg[bgsrec_agg.index.isin(glfrec_agg.index)]

#glfrec_agg.shape
#bgsrec_agg.shape
glfrec_agg = glfrec_agg.reset_index()
bgsrec_agg = bgsrec_agg.reset_index()        
a_vals = []
b_vals = []
for row in range(bgsrec_agg.shape[0]):
    lookupfish = bgsrec_agg['GENUS_BGS'][row]
    a_vals.append(len_AB[lookupfish][0])
    b_vals.append(len_AB[lookupfish][1])

bgsrec_agg['A_Vals'] = a_vals
bgsrec_agg['B_Vals'] = b_vals

    
#bgsrec_agg
# divide by 10 because the formulas are for CM, data in MM
# divide by 1000 because answers are in kg, but need to provide it in MT
bgsrec_agg['MT'] = bgsrec_agg['CNTEXP'] * (bgsrec_agg['A_Vals'] * ((glfrec_agg['LEN_GLF'] / 10) ** bgsrec_agg['B_Vals'] ) ) / 1000
bgsrec_agg['MT'] = bgsrec_agg['MT'].tolist()
# divide MT in BGSREC_agg by area tralwed from STAREC
print("getting trawl areas")
"""
areas = []
for r in range(bgsrec_agg.shape[0]):
    station = bgsrec_agg.index[r][1]
    if len(starec[starec['STATIONID']==station]['area_trawled'].tolist()) == 1:
        areas.append( starec[starec['STATIONID']==station]['area_trawled'].tolist()[0] )
    else: areas.append(0)

bgsrec_agg['area_trawled'] = areas
bgsrec_agg = bgsrec_agg[bgsrec_agg['area_trawled'] != '']
bgsrec_agg = bgsrec_agg.reset_index()
bgsrec_agg['cpue_MTSQKM'] = bgsrec_agg['MT'] / bgsrec_agg['area_trawled'].tolist()
bgsrec_agg['cpue_MTSQKM'] = bgsrec_agg['cpue_MTSQKM'].tolist()
bgsrec_agg = bgsrec_agg[bgsrec_agg['cpue_MTSQKM'] > 0]
"""

print("saving output to CSV")

"""
genus	station	elat	elon	cpue
                				>0
                				>0
                				>0
                				0
                				0
                				0
                				0

"""

bgsrec_agg = bgsrec_agg.reset_index(drop=True) # makes the genus values in the agg indices accessible in the columns
bgsrec_agg = bgsrec_agg[bgsrec_agg['STATIONID'].isin(starec['STATIONID'].tolist())] # there are some extra stations that got eliminated from the starec df, so they have to be removed
bgsrec_agg = bgsrec_agg.reset_index(drop=True)        
                 
# add entries for genus-station for stations where 0 of genus are found
for gen in gens:
    temp_present = bgsrec_agg[bgsrec_agg['GENUS_BGS'] == gen]
    temp_present = temp_present.sort_values(by='STATIONID')
    #temp_present = temp_present.drop_duplicates("STATIONID")
    #temp_present = temp_present.reset_index(drop=True)
    
    temp_absent = starec[~starec['STATIONID'].isin(temp_present['STATIONID'].tolist())]
    #temp_absent = temp_absent.drop_duplicates("STATIONID")
    #temp_absent = temp_absent.reset_index(drop=True)
    
    genus = [gen] * (starec.shape[0])
    station = temp_present['STATIONID'].tolist() + temp_absent['STATIONID'].tolist()
    #cpue = temp_present['cpue_MTSQKM'].tolist() + [0] * temp_absent.shape[0]
        
    sorterIndex = dict(zip(station, range(len(station))))
    starec['station_rank'] = starec['STATIONID'].map(sorterIndex)   
    starec = starec.sort_values(by='station_rank')
    starec = starec.reset_index(drop=True)
    
    temp_present = temp_present.reset_index(drop=True)
    # calculate cpue by combining present MT / area trawled
    temp_present['CPUE_MTSQKM'] = temp_present['MT'] / starec['area_trawled'][:temp_present.shape[0]]
    
    cpue = temp_present['CPUE_MTSQKM'].tolist() + temp_absent.shape[0] * [0]
    
    elat = starec['E_LATD'] + starec['E_LATM'] / 60
    elon = starec['E_LOND'] + starec['E_LONM'] / 60
    
    newdf = pd.DataFrame({"GENUS" : genus,
                  "STATIONID" : station,
                  "Lat" : elat,
                  "Lon" : elon,
                  "CPUE_MTSQKM" : cpue})
    
    newdf = newdf.dropna().reset_index()
    
    newdf.to_csv(gen + "_CPUE.csv")
    
    
#bgsrec_agg.to_csv('myoutput.csv')
