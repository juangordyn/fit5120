import pandas as pd
import numpy as np
from datetime import datetime
import json
import requests
import math

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

def distance_calculation(data, lat1, lon1):
    distance_list =[]
    for location in data.location:
        distance_list.append(distance_calc_helper(lat1, lon1, eval(location)))
    data['dist'] = distance_list
    return data

def parking_locations_stay_cost(all_sensors_df):
    maximum_stay_cost = pd.read_csv('maximum_stay_cost.csv')
    parking_locations_stay_cost_df = pd.merge(all_sensors_df, maximum_stay_cost, on = 'marker_id', how = 'left').drop_duplicates()
    return parking_locations_stay_cost_df

def query_statistics(all_sensors_df, destination_lat, destination_long, length_of_stay, max_walk, time, dayofweek_letters):
    dayofweek_dict = dict()
    dayofweek_dict['Monday'] = 1
    dayofweek_dict['Tuesday'] = 2
    dayofweek_dict['Wednesday'] = 3
    dayofweek_dict['Thursday'] = 4
    dayofweek_dict['Friday'] = 5
    dayofweek_dict['Saturday'] = 6
    dayofweek_dict['Sunday'] = 0
    dayofweek = dayofweek_dict[dayofweek_letters]
    min_hour = int(datetime.strptime(time,'%H:%M').strftime('%H'))
    max_hour = min_hour + 1
    parking_distance_df = distance_calculation(all_sensors_df, destination_lat, destination_long)
    parking_distance_df = parking_distance_df[parking_distance_df.dist<=max_walk]
    parking_df = parking_locations_stay_cost(parking_distance_df)
    parking_df = parking_df.sort_values(by='dist').reset_index(drop = True)
    parking_df = parking_df[~parking_df.maximum_stay.isna()]
    marker_id_tuples = str(tuple(parking_df.marker_id.unique()))
    url = "https://data.melbourne.vic.gov.au/resource/5532-ig9r.json?$limit=20000&$select=StreetMarker,vehiclepresent,avg(durationminutes),sum(durationminutes)&$group=StreetMarker,vehiclepresent&$where=(StreetMarker in "+ marker_id_tuples +") and ((date_extract_dow(arrivaltime)='"+str(dayofweek)+"' and date_extract_hh(arrivaltime) between '"+str(min_hour)+"' and '"+str(max_hour)+"'))"
    query_response = json.loads(requests.get(url).content)
    return[query_response]

def query_statistics_live(marker_ids, time, dayofweek):
    min_hour = int(datetime.strptime(time,'%H:%M').strftime('%H'))
    max_hour = min_hour + 1
    marker_id_tuples = str(tuple(marker_ids))
    url2 = "https://data.melbourne.vic.gov.au/resource/5532-ig9r.json?$limit=20000&$select=StreetMarker,vehiclepresent,avg(durationminutes),sum(durationminutes)&$group=StreetMarker,vehiclepresent&$where=(StreetMarker in "+ marker_id_tuples +") and ((date_extract_dow(arrivaltime)='"+str(dayofweek)+"' and date_extract_hh(arrivaltime) between '"+str(min_hour)+"' and '"+str(max_hour)+"'))"
    average_occupation = json.loads(requests.get(url2).content)
    return[average_occupation]

def retrieve_occupation_vacancy_time(query_result):

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
        try:
            occupation_vacancy_statistics.loc[j,'occupation_ratio'] = round((occupation_minutes/total_minutes)*100)
        except:
            continue
        j+=1
    return occupation_vacancy_statistics

def text_hover_over(new_df):
    for i in range(len(new_df)):
        bay_id = new_df.loc[i, 'bay_id']
        dist = round(new_df.loc[i, 'dist'])
        occupation_ratio = new_df.loc[i, 'occupation_ratio']
        avg_vacancy = round(new_df.loc[i, 'avg_vacancy'])
        avg_occupation = round(new_df.loc[i, 'avg_occupation'])
        maximum_stay = new_df.loc[i, 'maximum_stay']
        cost_per_hour = new_df.loc[i, 'cost_per_hour']
        new_df.loc[i, 'hover_information'] = "Bay ID: " + str(bay_id) + "<br />Dist. to dest. (mts): " + str(
            dist) + "<br />Avg. Occupation(%): " + str(occupation_ratio) + "<br />Avg. Vacancy (minutes): " + str(avg_vacancy) + \
                                                  "<br />Avg. Occupation (minutes): " + str(
            avg_occupation) + "<br />Maximum stay (min): " + str(maximum_stay) + "<br />Hourly Cost ($): " + str(cost_per_hour / 100)
    return new_df

def parking_simulation_funct(parking_statistics_df_complete, length_of_stay):
    parking_statistics_df_complete = parking_statistics_df_complete.sort_values(by='dist')
    parking_statistics_df = parking_statistics_df_complete[parking_statistics_df_complete.maximum_stay>=length_of_stay].reset_index(drop=True)
    if len(parking_statistics_df)==0:
        parking_statistics_df = parking_statistics_df_complete
        parking_time = 15
    else:
        parking_time = 0
    for j in range(100):
        parking_simulation = []
        for occupation_ratio in parking_statistics_df.occupation_ratio:
            parking_simulation.append(np.random.choice(2, 1, p=[occupation_ratio/100, 1- occupation_ratio/100])[0])
        time_results = []
        distance_results = []
        parking_cost_results = []
        car_speed = 3
        walk_speed = 5
        parking_cost = (parking_statistics_df.loc[0, 'cost_per_hour'])/100
        distance_to_dest = (parking_statistics_df.loc[0, 'dist'])/1000
        total_time_cum = (distance_to_dest/car_speed)*60
        unoccupied = parking_simulation[0]
        i = 0
        while not unoccupied:
            i += 1
            if i>len(parking_simulation):
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
    return parking_statistics_df_complete

def calculate_parking_statistics(dest_lat, dest_lng, length_of_stay, max_walk, hour, day_of_week):
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
    new_df = parking_simulation_funct(new_df, length_of_stay)
    new_df = text_hover_over(new_df).reset_index(drop=True)
    return new_df

def calculate_parking_statistics_live(marker_ids, hour, day_of_week):
    query_results = query_statistics_live(marker_ids, hour, day_of_week)
    parking_statistics_df = retrieve_final_statistics(retrieve_occupation_vacancy_time(query_results))
    parking_statistics_df['occupation_ratio'] = parking_statistics_df['occupation_ratio'].fillna(np.mean(parking_statistics_df.occupation_ratio))
    parking_statistics_df['avg_vacancy'] = parking_statistics_df['avg_vacancy'].fillna(np.mean(parking_statistics_df.avg_vacancy))
    parking_statistics_df['avg_occupation'] = parking_statistics_df['avg_occupation'].fillna(np.mean(parking_statistics_df.avg_occupation))
    return parking_statistics_df



# calculate_parking_statistics(-37.8103, 144.9614, 110, 800, '19:00', 'Friday').to_csv('parking_statistics.csv', index=False)