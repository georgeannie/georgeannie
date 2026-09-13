from shapely.ops import cascaded_union
import ipywidgets as widgets
import ipyleaflet as leaflet
import geopandas as gpd
import pandas as pd
import numpy as np
import datetime
import calendar
import shapely
import asyncio
import pyproj
import boto3
import json
import pytz
import io

month_dic = {'January':1, 'February':2, 'March':3, 'April':4, 'May':5, 'June':6, 'July':7,
                                         'August':8, 'September':9, 'October':10, 'November':11, 'December':12}
utc = pytz.timezone('UTC')


'''HELPER FUNCTIONS USED FOR Scenario_1_visualization.ipynb'''

def formatdate(date):
    '''
        Function to produce a pretty date string for the datetime_slider
    '''
    date = [str(date.month), str(date.day), str(date.year), str(date.hour), str(date.minute)]
    day_str = "/".join([date[0], date[1], date[2]])
    time_str = ":".join([date[3], date[4]])
    final_str = " ".join([day_str, time_str])
    return final_str

def get_sat_view(df):
    '''
      Function to convert a dataframe 
      polygon string column to a list of polygons
      
      Ouput Format: [(x1, y1), (x2, y2), ...]
    '''
    ## Note to self: This is the function from the last map that hasn't yet been tested on the others
    points, count = [], 0
    df['FOV'] = df['FOV'].astype(str)
    for col, row in df.iterrows():
        poly, count = [], 0
        for point in row.FOV.split(")"):
            p = point.replace("[", "").replace("(", "").replace(")", "").split(",")
            if len(p) > 0:
                if p[0] == '':
                    del p[0]                        
                if(len(p) == 2):
                    poly.append((float(p[0]), float(p[1])))
        points.append(poly)
    return points

def create_circle_marker(row):
    '''
      Function to create a circle marker for 
      a vessel from a row in a dataframe
      
      Output: circle marker that can be mapped with m.add_layer()
    '''
    
    # Create circle marker and give it attributes
    circle_marker = leaflet.CircleMarker()
    circle_marker.location = (row.LAT, row.LON)
    circle_marker.radius = 5
    circle_marker.weight = 1
    circle_marker.color = "black"
    circle_marker.fill_color = "black"
    
    # Popup creation
    message2 = widgets.HTML()
    message2.value = "<b>Vessel Name: </b>" + str(row.VesselName) + "<br>" + \
                     "<b>Heading: </b>"     + str(row.Heading)    + "<br>" + \
                     "<b>Length: </b>"      + str(row.Length)     + "<br>" + \
                     "<b>Width: </b>"       + str(row.Width)      + "<br>" + \
                     "<b>Draft: </b>"       + str(row.Draft)      + "<br>" + \
                     "<b>Cargo: </b>"       + str(row.Cargo)      
    circle_marker.popup = message2
    
    return circle_marker



def create_colored_circle_marker(row, color):
    '''
      Function to create a circle marker for 
      a vessel from a row in a dataframe
      
      Output: circle marker that can be mapped with m.add_layer()
    '''
    
    # Create circle marker and give it attributes
    circle_marker = leaflet.CircleMarker()
    circle_marker.location = (row.LAT, row.LON)
    circle_marker.radius = 5
    circle_marker.weight = 1
    circle_marker.color = color
    circle_marker.fill_color = color
    
    # Popup creation
    message2 = widgets.HTML()
    message2.value = "<b>Vessel Name: </b>" + str(row.VesselName) + "<br>" + \
                     "<b>Heading: </b>"     + str(row.Heading)    + "<br>" + \
                     "<b>Length: </b>"      + str(row.Length)     + "<br>" + \
                     "<b>Width: </b>"       + str(row.Width)      + "<br>" + \
                     "<b>Draft: </b>"       + str(row.Draft)      + "<br>" + \
                     "<b>Cargo: </b>"       + str(row.Cargo)      
    circle_marker.popup = message2
    
    return circle_marker

# def pull_fov(satellite_selector, satcat):
#     '''
#         Function to pull the selected satellites FoV data from the s3 Bucket.
#         Output: FoV dataframe
#     '''
#     row = satcat[satcat['SATNAME'] == satellite_selector.value]
#     row = row.drop_duplicates(subset = ['SATNAME'])
#     norad_id = row['NORAD_CAT_ID'].to_list()[0]
#     bucket = 'afv-scenario'
#     key = 'analytics-products/fovs_combined/fov_' + str(norad_id) + '.csv'
#     fov = client.get_object(Bucket=bucket, Key=key)
#     fov = pd.read_csv(io.BytesIO(fov['Body'].read()))
#     return fov

def get_usa_fovs(satca, client):
    '''
        Function to pull the selected US satellites FoV data from the s3 Bucket.
        Output: FoV dataframe
    '''
    satcat = satcat[satcat['COUNTRY'] == 'US']
    norad_ids = satcat['NORAD_CAT_ID'].unique()
    bucket = 'afv-scenario'
    final = pd.DataFrame()
    count = 1
    for norad_id in norad_ids:
        cur = client.get_object(Bucket=bucket, Key=key)
        cur = pd.read_csv(io.BytesIO(cur['Body'].read()))
        if len(final) == 0:
            final = cur
        else:
            final.append(cur)
        count += 1
    return final

def get_existing_satellites(s3):
    '''
        Function to get a list of satellites that have data available in the s3 Bucket
        Output: List of available Norad ID's
    '''
    bucket = s3.Bucket('afv-scenario')
    fnames = []
    for file_name in bucket.objects.filter(Prefix = "analytics-products/fovs_2015_5min/"):
        fnames.append(str(file_name))
    norad_ids =  [int(i[88:].split(".")[0]) for i in fnames]
    return norad_ids


def initialize_date_options(year_selector, month_selector, day_selector):
    utc = pytz.timezone('UTC')
    f, l = list(calendar.monthrange(year_selector.value, month_dic[month_selector.value]))
    start = datetime.datetime(year_selector.value, month_dic[month_selector.value], day_selector.value, 0, 0, 0,  tzinfo=utc)   # start date
    end = datetime.datetime(year_selector.value, month_dic[month_selector.value], day_selector.value, 23, 59, 59,  tzinfo=utc)   # end date
    cur = start + datetime.timedelta(minutes = 5)
    dates = [cur]
    while cur <= end:
        cur = cur + datetime.timedelta(minutes = 5)
        dates.append(cur)
    # dates = [":".join([str(i.hour), str(i.minute)]) for i in sorted(dates)]
    return dates

def pull_fov(satellite_selector, satcat, client):
    '''
        Function to pull the selected satellites FoV data from the s3 Bucket.
        Output: FoV dataframe
    '''
    row = satcat[satcat['SATNAME'] == satellite_selector.value]
    row = row.drop_duplicates(subset = ['SATNAME'])
    norad_id = row['NORAD_CAT_ID'].to_list()[0]
    bucket = 'afv-scenario'
    key = 'analytics-products/fovs_2015_5min/fov_' + str(norad_id) + '.csv'
    fov = client.get_object(Bucket=bucket, Key=key)
    fov = pd.read_csv(io.BytesIO(fov['Body'].read()))
    return fov, norad_id

def reset_map2_observer(dc, m2, basemap_layer, progress_text2, us_num_vssels_seen, us_area_cov_num, other_area_cov_num, oth_num_vssels_seen):
    '''
        Reset map2 to blank map and clear loading message.
        No output.
    '''
    dc.clear()
    m2.clear_layers()
    m2.add_layer(basemap_layer)
    m2.center = (0,0)
    m2.zoom = 1
    progress_text2.value = ""
    us_num_vssels_seen.value = ""
    us_area_cov_num.value = ""
    other_area_cov_num.value = ""
    oth_num_vssels_seen.value = ""
    
    
def fix_coordinates(geom):
    '''
        Leaflet seems to be saving the drawn polygon coordinates in oppposite orientation.
        This function flips the coordinates so the displayed polygon matches the drawn one.
    '''
    coords = geom['geometry']['coordinates'][0]
    coords_list = []
    for point in coords:
        cur_point = [point[1], point[0]]
        coords_list.append(cur_point)
    return coords_list


def get_ais(year, client):
    '''
        Function to grab the AIS data for the selected year from s3 Bucket
        Output: AIS dataframe
    '''
    bucket = 'afv-scenario'
    if year == 2015:
        key = "data-products/AIS/AIS_2015_01.parquet"
        ais = client.get_object(Bucket=bucket, Key=key)
        return pd.read_parquet(io.BytesIO(ais['Body'].read()))
    elif year == 2016:
        key = "data-products/AIS/AIS_2016_01.parquet"
        ais = client.get_object(Bucket=bucket, Key=key)
        return pd.read_parquet(io.BytesIO(ais['Body'].read()))
    elif year == 2017:
        key = "data-products/AIS/AIS_2017_01.parquet"
        ais = client.get_object(Bucket=bucket, Key=key)
        return pd.read_parquet(io.BytesIO(ais['Body'].read()))
    
    
def filter_month_observer(month_selector, year_selector, day_selector, start_date, end_date):
    '''
        Function to update dates on the interface.
    '''
    month_dic = {'January':1, 'February':2, 'March':3, 'April':4, 'May':5, 'June':6, 'July':7,
                                             'August':8, 'September':9, 'October':10, 'November':11, 'December':12}
    dates = initialize_date_options(year_selector, month_selector, day_selector)
    start_date.options = dates
    end_date.options = dates
    end_date.value = dates[-2]
    day_selector.options = get_days_observer(year_selector, month_selector)
    return month_dic[month_selector.value]
    
    
def get_sat_fovs(country_selector2, satcat, satellite_selector2, client):
    '''
        Function to grab FoV's for a list of satellites.
        Output: Combined FoV dataframe for all satellites.
    '''
    satcat = satcat[satcat['SATNAME'].isin(list(satellite_selector2.value))]
    norad_ids = satcat['NORAD_CAT_ID'].unique()
    bucket = 'afv-scenario'
    final = pd.DataFrame()
    for norad_id in norad_ids:
        key = 'analytics-products/fovs_2015_5min/fov_' + str(norad_id) + '.csv'
        cur = client.get_object(Bucket=bucket, Key=key)
        cur = pd.read_csv(io.BytesIO(cur['Body'].read()))        
        if len(final) == 0:
            final = cur
        else:
            final.append(cur)
    return final



def fix_geometry(polys):
    '''
        FoV data reads in iwth coordiantes flipped. This function corrects them.
        Output: List of Polygons that can be passed in as a geometry column
    '''
    final_polys = []
    for poly in polys:
        x, y = poly.exterior.coords.xy
        p_list = []
        for j in range(0, len(x)):
            p_list.append(shapely.geometry.Point(y[j], x[j]))
        poly = shapely.geometry.Polygon([[p.x, p.y] for p in p_list])
        final_polys.append(poly)
    return final_polys
    
    
def format_date_new(date_selector, year_selector, month_selector, day_selector):
    '''
        Function to get proper datetime object from datetime_slider string input.
        Output: Start and end tz aware datetime objects.
    '''
    start, end = date_selector.value[0], date_selector.value[1]
    start_hour, start_min = int(start.split(":")[0]), int(start.split(":")[1])
    end_hour, end_min = int(end.split(":")[0]), int(end.split(":")[1])
                                             
    utc = pytz.timezone('UTC')
    start = datetime.datetime(year_selector.value, month_dic[month_selector.value], day_selector.value, start_hour, start_min, 0,  tzinfo=utc)   # start date
    end = datetime.datetime(year_selector.value, month_dic[month_selector.value], day_selector.value, end_hour, end_min, 0,  tzinfo=utc)   # start date
    
    return start, end


'''UNCLEAN STUFF'''

def update_day_observer(datetime_slider, year_selector, month_selector, day_selector):
    dates = initialize_date_options(year_selector, month_selector, day_selector)
    datetime_slider.options = dates
    datetime_slider.index = (0, len(dates) - 1)


def get_days_observer(year_selector, month_selector):
    f, l = list(calendar.monthrange(year_selector.value, month_dic[month_selector.value]))
    days = []
    for i in range(0, l):
        days.append(i + 1)
    return days



def filter_satellites_observer(satcat, country_selecter, satellite_selector):
    cur = satcat[satcat['COUNTRY'] == country_selecter.value]
    satellite_selector.options = sorted(cur['SATNAME'].unique())
    return sorted(satcat['SATNAME'].unique())
    
    
'''HELPER FUNCTIONS USED FOR Scenario_2_visualization.ipynb'''

def pd_read_s3_parquet(key, bucket, s3_client=None, **args):
    '''
    	Function to read an s3 parquet file.
    	Output: Pandas data frame
    '''
    if s3_client is None:
        s3_client = boto3.client('s3')
    obj = s3_client.get_object(Bucket=bucket, Key=key)
    return pd.read_parquet(io.BytesIO(obj['Body'].read()), **args)


def load_ais(year, s3_client):
    '''
    	Function to build datasets for visualizations.
    	Output: List if data frames
    '''
    tmp_df = pd_read_s3_parquet(key=f'data-products/AIS/AIS_{year}_01.parquet', 
                             bucket='afv-scenario', s3_client=s3_client)
    tmp_df = tmp_df.rename(columns = {'MMSI': 'Vessel_MMSI', 'BaseDateTime': 'Time', 
                            'LAT': 'xcoord', 'LON': 'ycoord', 
                            'VesselName': 'Vessel_Name'})

    tmp_df['Time'] = pd.to_datetime(tmp_df['Time'], infer_datetime_format=True)
    tmp_df['Year'] = tmp_df.Time.apply(lambda x: x.year)
    tmp_df['Date'] = tmp_df.Time.apply(lambda x: x.date())
    null_vessel_name = list(tmp_df.Vessel_MMSI[tmp_df.Vessel_Name.isna()].unique())
    tmp_df['Missing'] = tmp_df['Vessel_MMSI'].apply(lambda x: 1 if int(x) in null_vessel_name else 0)
    #tmp_df['Year'] = year
        
    # cumulative counts by MMSI, separated by vessels missing metadata and vessels with metadata
    mmsi_count_df = tmp_df[['Vessel_MMSI', 'Time']].groupby(['Vessel_MMSI']).count()
    mmsi_count_df = mmsi_count_df.rename(columns = {'Time': 'Count'})
    mmsi_count_df['Vessel_MMSI'] = mmsi_count_df.index.astype(str)
    mmsi_count_df['Missing'] = mmsi_count_df['Vessel_MMSI'].apply(lambda x: 1 if int(x) in null_vessel_name else 0)
    mmsi_count_df['Count'] = mmsi_count_df['Count'].apply(np.log)
    mmsi_count_df = mmsi_count_df.sort_values(by=['Count'], ascending=False)
    mmsi_count_df['Year'] = year
    
    # missing vessel counts by date
    missing_df = tmp_df[['Date', 'Missing']].groupby(['Date']).sum()
    missing_df['Date'] = missing_df.index
    missing_df['Year'] = year

    # vessel data points by hour 
    hour_df = tmp_df[['Time', 'Vessel_MMSI']].groupby(['Time']).count()
    hour_df['Time'] = hour_df.index
    hour_df = hour_df.rename(columns={'Vessel_MMSI': 'Count'})
    hour_df = hour_df.resample('60min', offset="0min", label='right').sum()
    hour_df['Time'] = hour_df.index    
    hour_df['Year'] = year
    
    return tmp_df, mmsi_count_df, missing_df, hour_df


def concat_ais_data(s3_resource, s3_client):
    '''
    	Function to build and save visualization datasets in s3.
    '''
    list_df, list_mmsi_count_df, list_missing_df, list_hour_df = [], [], [], []

    for i in range(2009, 2018):
        tmp_df, mmsi_count_df, missing_df, hour_df = load_ais(year=i, s3_client=s3_client)
        list_df.append(tmp_df[tmp_df.Missing == 1]) # only keep records with missing data
        list_mmsi_count_df.append(mmsi_count_df)
        list_missing_df.append(missing_df)
        list_hour_df.append(hour_df)

    # save processed data to s3 bucket
    for k,v in {'ais_missing_long': list_df,
                'ais_mmsi_count': list_mmsi_count_df, 
                'ais_missing': list_missing_df, 
                'ais_hour': list_hour_df}.items():
        df = pd.concat(v, ignore_index=True)
        bucket = 'afv-scenario' # already created on S3
        csv_buffer = io.StringIO()
        df.to_csv(csv_buffer)
        s3_resource.Object(bucket, f'analytics-products/{k}.csv').put(Body=csv_buffer.getvalue())
 


