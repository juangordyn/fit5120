# wrangling function to be able to calculate average coordenates of the given polygons for the parking bays
def wrangle_polygon():
    all_sensors_coord = all_sensors_df
    for i in range(0, len(all_sensors_df)):
        lat_list = []
        long_list = []
        for polygon_coord in all_sensors_df.loc[i, 'the_geom'].split('MULTIPOLYGON (((')[1].split(')))')[0].split(','):
            try:
                lat_long = polygon_coord.strip().split(' ')
                lat_list.append(float(lat_long[1]))
                long_list.append(float(lat_long[0]))
            except:
                if '))' in polygon_coord:
                    polygon_coord = polygon_coord.split('))')[0]
                elif '((' in polygon_coord:
                    polygon_coord = polygon_coord.split('((')[1]
                lat_long = polygon_coord.strip().split(' ')
                lat_list.append(float(lat_long[1]))
                long_list.append(float(lat_long[0]))
            all_sensors_coord.loc[i, 'mean_lat'] = np.mean(lat_list)
            all_sensors_coord.loc[i, 'mean_long'] = np.mean(long_list)
    all_sensors_coord['location'] = list(zip(all_sensors_coord.mean_lat, all_sensors_coord.mean_long))
    return all_sensors_coord

# wrangling function to connect the different datasets using attributes they have in common. We store the result to use it within our Shiny App. To do it while
# the app is running would take a lot of time.
def wrangle_cost_stay():
    street_segments = pd.read_csv('street_segments.csv')
    street_segments = street_segments.rename(columns = {'street_segment_id':'rd_seg_id'})
    restrictions_df = pd.read_csv('restrictions.csv')
    restrictions_df = pd.merge(restrictions_df, street_segments, on ='pay_stay_zone')
    rd_seg_df = restrictions_df[['rd_seg_id', 'maximum_stay', 'cost_per_hour']].drop_duplicates()
    ordered = rd_seg_df.sort_values(by = ['rd_seg_id', 'maximum_stay'])
    rd_seg_df_final = ordered.drop_duplicates(subset='rd_seg_id', keep="first")
    merged = pd.merge(all_sensors_df, rd_seg_df_final, on = 'rd_seg_id')[['marker_id', 'maximum_stay', 'cost_per_hour']].dropna()
    merged.to_csv('maximum_stay_cost.csv', index = False)
    return merged
