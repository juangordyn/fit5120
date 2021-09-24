import pandas as pd
import numpy as np
from datetime import datetime
import json
import requests
import math
# import time
import random

"""
All of these functions will be called within the Shiny app using the reticulate package to connect Python and R

"""
"""
Helper Function to calculate Haversine Distance between parking bays and final destination to be able to order them and subset them accordingly

"""
def distance_calc_helper(lat1, lon1, destination):
    lat2, lon2 = destination
    radius = 6378 # km

    dlat = math.radians(lat2-lat1)
    dlon = math.radians(lon2-lon1)
    a = math.sin(dlat/2) * math.sin(dlat/2) + math.cos(math.radians(lat1)) \
        * math.cos(math.radians(lat2)) * math.sin(dlon/2) * math.sin(dlon/2)
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    d = radius * c

    return d*1000
"""

Distance calculation function

"""
def distance_calculation(data, lat1, lon1):
    distance_list =[]
    for location in data.location:
        distance_list.append(distance_calc_helper(lat1, lon1, eval(location)))
    data['dist'] = distance_list
    return data

"""

Function to get parking cost in our parking data

"""
def parking_locations_stay_cost(all_sensors_df):
    maximum_stay_cost = pd.read_csv('maximum_stay_cost.csv')
    parking_locations_stay_cost_df = pd.merge(all_sensors_df, maximum_stay_cost, on = 'marker_id', how = 'left').drop_duplicates()
    return parking_locations_stay_cost_df

"""

Function to query historical parking data from SODA data base, being able to aggregate and filter right from the query using SoSQL

"""
def query_statistics(all_sensors_df, destination_lat, destination_long, length_of_stay, max_walk, input_hour, dayofweek_letters):
    # start = time.time()
    dayofweek_dict = dict()
    dayofweek_dict['Monday'] = 1
    dayofweek_dict['Tuesday'] = 2
    dayofweek_dict['Wednesday'] = 3
    dayofweek_dict['Thursday'] = 4
    dayofweek_dict['Friday'] = 5
    dayofweek_dict['Saturday'] = 6
    dayofweek_dict['Sunday'] = 0
    dayofweek = dayofweek_dict[dayofweek_letters]
    min_hour = int(datetime.strptime(input_hour,'%H:%M').strftime('%H'))
    max_hour = min_hour + 1
    parking_distance_df = distance_calculation(all_sensors_df, destination_lat, destination_long)
    parking_distance_df = parking_distance_df[parking_distance_df.dist<=max_walk]
    parking_df = parking_locations_stay_cost(parking_distance_df)
    parking_df = parking_df.sort_values(by='dist').reset_index(drop = True)
    parking_df = parking_df[~parking_df.maximum_stay.isna()]
    full_marker_ids = pd.read_csv('marker_ids.csv')
    full_marker_ids_set = set(full_marker_ids['marker_id'])
    marker_id_unique = parking_df.marker_id.unique()
    marker_id_set = set(marker_id_unique)
    marker_common = marker_id_set.intersection(full_marker_ids_set)
    if len(marker_common) > 100:
        random.seed(10)
        marker_ids_sample = random.sample(marker_common, 100)
    else:
        marker_ids_sample = marker_common
    marker_id_tuples = str(tuple(marker_ids_sample))
    url = "https://data.melbourne.vic.gov.au/resource/5532-ig9r.json?$limit=20000&$select=StreetMarker,vehiclepresent,avg(durationminutes),sum(durationminutes),sum(case(inviolation='True',1,true,0)) as fine_count,sum(case(inviolation='False',1,true,0)) as no_fine_count&$group=StreetMarker,vehiclepresent&$where=(StreetMarker in "+ marker_id_tuples +") and ((date_extract_dow(arrivaltime)='"+str(dayofweek)+"' and date_extract_hh(arrivaltime) between '"+str(min_hour)+"' and '"+str(max_hour)+"'))"
    query_response = json.loads(requests.get(url).content)
    # end = time.time()
    # print('query_response', str(end-start))
    return[query_response]

"""

Same as above but for Live sensor data

"""
def query_statistics_live(marker_ids, time, dayofweek):
    min_hour = int(datetime.strptime(time,'%H:%M').strftime('%H'))
    max_hour = min_hour + 1
    marker_id_tuples = str(tuple(marker_ids))
    url2 = "https://data.melbourne.vic.gov.au/resource/5532-ig9r.json?$limit=20000&$select=StreetMarker,vehiclepresent,avg(durationminutes),sum(durationminutes),sum(case(inviolation='True',1,true,0)) as fine_count,sum(case(inviolation='False',1,true,0)) as no_fine_count&$group=StreetMarker,vehiclepresent&$where=(StreetMarker in "+ marker_id_tuples +") and ((date_extract_dow(arrivaltime)='"+str(dayofweek)+"' and date_extract_hh(arrivaltime) between '"+str(min_hour)+"' and '"+str(max_hour)+"'))"
    average_occupation = json.loads(requests.get(url2).content)
    return[average_occupation]
"""

Function to calculate occupation and vacancy times, given the data queried in the above functions

"""
def retrieve_occupation_vacancy_time(query_result):
    # start = time.time()
    occupation_vacancy_df = pd.DataFrame.from_dict(query_result[0], orient='columns')
    occupation_vacancy_statistics = pd.DataFrame()
    j=0
    for marker_id in occupation_vacancy_df.StreetMarker.unique():
        occupation_vacancy_subset = occupation_vacancy_df[occupation_vacancy_df.StreetMarker==marker_id].reset_index(drop=True)
        occupation_vacancy_statistics.loc[j,'marker_id'] = marker_id
        total_minutes = 0
        for i in range(len(occupation_vacancy_subset)):
            vehiclepresent = occupation_vacancy_subset.loc[i, 'vehiclepresent']
            statistic = occupation_vacancy_subset.loc[i, 'avg_durationminutes']
            sum_statistic = occupation_vacancy_subset.loc[i, 'sum_durationminutes']
            total_minutes += int(sum_statistic)
            if vehiclepresent == False:
                occupation_vacancy_statistics.loc[j, 'avg_vacancy'] = round(float(statistic), 2)
            else:
                occupation_minutes = int(sum_statistic)
                occupation_vacancy_statistics.loc[j, 'avg_occupation'] = round(float(statistic),2)
                fine_count = int(occupation_vacancy_subset.loc[i, 'fine_count'])
                no_fine_count = int(occupation_vacancy_subset.loc[i, 'no_fine_count'])
                occupation_vacancy_statistics.loc[j, 'fine_prob'] = round((fine_count/(fine_count + no_fine_count))*100)
        try:
            occupation_ratio = round((occupation_minutes/total_minutes)*100)
            if occupation_ratio > 100:
                occupation_ratio = 100
            elif occupation_ratio < 0:
                occupation_ratio = 0
            occupation_vacancy_statistics.loc[j,'occupation_ratio'] = occupation_ratio
        except:
            continue
        j+=1
    occupation_vacancy_statistics['occupation_ratio'] = 1.25*occupation_vacancy_statistics['occupation_ratio']
    occupation_vacancy_statistics['occupation_ratio'] = occupation_vacancy_statistics['occupation_ratio'].apply(lambda x: 99.99 if x>100 else x)
    occupation_vacancy_statistics['avg_vacancy'] = 0.5*occupation_vacancy_statistics['avg_vacancy']
    # end = time.time()
    # print('occupation_vacancy_statistics', str(end - start))
    return occupation_vacancy_statistics

"""

Function to create the string that will pop up when hovering each sensor

"""
def text_hover_over(new_df):
    # start = time.time()
    for i in range(len(new_df)):
        bay_id = new_df.loc[i, 'bay_id']
        dist = round(new_df.loc[i, 'dist'])
        occupation_ratio = new_df.loc[i, 'occupation_ratio']
        # avg_vacancy = round(new_df.loc[i, 'avg_vacancy'])
        # avg_occupation = round(new_df.loc[i, 'avg_occupation'])
        maximum_stay = new_df.loc[i, 'maximum_stay']
        cost_per_hour = new_df.loc[i, 'cost_per_hour']
        new_df.loc[i, 'hover_information'] = "Bay ID: " + str(bay_id) + "<br />Distance to destination (mts): " + str(
            dist) + "<br />Historical Occupation (%): " + str(occupation_ratio) + "<br />Maximum stay (mins): " + str(maximum_stay) + "<br />Hourly Cost ($): " + str(cost_per_hour / 100)
    # end = time.time()
    # print('hover_over', str(end - start))
    return new_df

"""

Function to perform parking simulation:
1) We order the parkings in ascendent distance.
2) We randomly initialize their states as occupied or unoccupied using the Occupation ratio as probability of being occupied.
3) We simulate the user visiting one by one in order of proximity.
4) The time spent until the user comes across the first free parking is stored.
5) We repeat 100 times and take the average

Note: The different parking spaces can change states, using average vacacny time to transition from free to occupied and average occupation
time to transition from occupied to free.

"""

def parking_simulation_funct(parking_statistics_df_complete, length_of_stay, day_of_week, hour):
    # start = time.time()
    parking_statistics_df_complete = parking_statistics_df_complete.sort_values(by='dist')
    if day_of_week != 'Sunday' and(datetime.strptime(hour, '%H:%M') > datetime.strptime('7:30', '%H:%M')) and (datetime.strptime(hour, '%H:%M') < datetime.strptime('18:30', '%H:%M')):
        parking_statistics_df = parking_statistics_df_complete[parking_statistics_df_complete.maximum_stay>=length_of_stay].reset_index(drop=True)
    else:
        parking_statistics_df = parking_statistics_df_complete
    if len(parking_statistics_df)<10:
        parking_statistics_df = parking_statistics_df_complete
        parking_time = 15
    else:
        parking_time = 0
    for j in range(100):
        parking_simulation = []
        for occupation_ratio in parking_statistics_df.occupation_ratio:
            random.seed(10)
            parking_simulation.append(np.random.choice(2, 1, p=[occupation_ratio/100, 1 - occupation_ratio/100])[0])
        time_results = []
        distance_results = []
        parking_cost_results = []
        car_speed = 5
        walk_speed = 5
        parking_cost = (parking_statistics_df.loc[0, 'cost_per_hour'])/100
        distance_to_dest = (parking_statistics_df.loc[0, 'dist'])/1000
        total_time_cum = (distance_to_dest/car_speed)*60
        unoccupied = parking_simulation[0]
        parking_statistics_df = parking_statistics_df.reset_index(drop=True)
        i = 0
        while not unoccupied:
            i += 1
            if i>=len(parking_simulation):
                i=1
            distance_to_dest = parking_statistics_df.loc[i, 'dist']
            distance = (distance_to_dest - parking_statistics_df.loc[i-1, 'dist'])/1000
            parking_cost = parking_statistics_df.loc[i, 'cost_per_hour']
            total_time_cum += (distance/car_speed)*60
            initial_state = parking_simulation[i]
            avg_vacancy = parking_statistics_df.loc[i, 'avg_vacancy']
            avg_occupation = parking_statistics_df.loc[i, 'avg_occupation']
            if initial_state == 0:
                initial_time = avg_occupation
                second_time = avg_vacancy
            else:
                initial_time = avg_vacancy
                second_time = avg_occupation
            time_cum_park = 0
            while total_time_cum > time_cum_park:
                time_cum_park += initial_time
                last_time = initial_time
                if total_time_cum < time_cum_park:
                    break
                time_cum_park += second_time
                last_time = second_time
            if last_time == avg_occupation:
                unoccupied = 0
            else:
                unoccupied = 1
        time_results.append(total_time_cum)
        distance_results.append(distance_to_dest)
        parking_cost_results.append(parking_cost)
    if parking_time == 0:
        parking_time = math.ceil(np.mean(time_results))
    parking_distance = math.ceil(np.mean(distance_results))
    walk_time = ((parking_distance/1000)/walk_speed)*60
    parking_cost = np.mean(parking_cost_results)
    parking_statistics_df_complete['parking_time'] = parking_time
    parking_statistics_df_complete['parking_distance'] = parking_distance
    parking_statistics_df_complete['parking_cost'] = parking_cost
    parking_statistics_df_complete['walking_time'] = round(walk_time)
    # end = time.time()
    # print('parking_simulation', str(end - start))
    return parking_statistics_df_complete
"""

Function to calculate statistics for historical data

"""
def calculate_parking_statistics(dest_lat, dest_lng, length_of_stay, max_walk, hour, day_of_week):
    try:
        all_sensors_df = pd.read_csv('all_sensors_df.csv')
        query_results = query_statistics(all_sensors_df, dest_lat, dest_lng, length_of_stay, max_walk, hour, day_of_week)
        parking_statistics_df = retrieve_occupation_vacancy_time(query_results)
        maximum_stay_cost = pd.read_csv('maximum_stay_cost.csv')
        new_df = pd.merge(all_sensors_df, parking_statistics_df, on='marker_id')
        new_df = pd.merge(new_df, maximum_stay_cost, on='marker_id', how='left').drop_duplicates()
        new_df['occupation_ratio'] = new_df['occupation_ratio'].fillna(np.mean(new_df.occupation_ratio))
        new_df['avg_vacancy'] = new_df['avg_vacancy'].fillna(np.mean(new_df.avg_vacancy))
        new_df['avg_occupation'] = new_df['avg_occupation'].fillna(np.mean(new_df.avg_occupation))
        new_df = new_df.reset_index(drop=True)
        new_df = parking_simulation_funct(new_df, length_of_stay, day_of_week, hour)
        new_df = text_hover_over(new_df).reset_index(drop=True)
        return new_df
    except:
        return 'No results'
"""

Function to calculate statistics for live data

"""
def calculate_parking_statistics_live(marker_ids, hour, day_of_week):
    query_results = query_statistics_live(marker_ids, hour, day_of_week)
    parking_statistics_df = retrieve_occupation_vacancy_time(query_results)
    parking_statistics_df['occupation_ratio'] = parking_statistics_df['occupation_ratio'].fillna(np.mean(parking_statistics_df.occupation_ratio))
    parking_statistics_df['avg_vacancy'] = parking_statistics_df['avg_vacancy'].fillna(np.mean(parking_statistics_df.avg_vacancy))
    parking_statistics_df['avg_occupation'] = parking_statistics_df['avg_occupation'].fillna(np.mean(parking_statistics_df.avg_occupation))
    return parking_statistics_df



# calculate_parking_statistics(-37.82523, 144.9513, 90, 500, '8:00', 'Wednesday')
