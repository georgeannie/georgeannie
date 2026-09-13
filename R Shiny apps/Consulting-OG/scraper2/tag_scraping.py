# -*- coding: utf-8 -*-
"""
Created on Fri Feb  1 16:51:53 2019

@author: subhashjaini
"""

# coding: utf-8

# In[1]:

import re
import pandas as pd
import requests
from bs4 import BeautifulSoup
import json
from urllib.parse import unquote


def update_dict(kv_store, new_value):
    """
    Determines how to update the dict.
    e.g update_dict({'a': 'b'}, {'a': 'c'}
    will be converted to {'a': ['b', 'c']}
    :param dict kv_store: The dict to update
    :param dict new_value: A key value pair to add
        to kv_store. Must be a flat dict.
    :rtype: dict
    """
    keys = new_value.keys()
    for key in keys:
        cleaned_key = key.replace('@', '') if isinstance(key, str) else key
        # if key doesnt exist do
        # a normal update
        if kv_store.get(cleaned_key) is None:
            kv_store[cleaned_key] = new_value[key]
        else:
            # handle case with similar keys
            val = kv_store.get(cleaned_key)
            if isinstance(val, list):
                kv_store[cleaned_key] = \
                    kv_store[cleaned_key].append(new_value[key])
            else:
                kv_store[cleaned_key] = [val, new_value[key]]

    return kv_store


def parse_dict(old_dict, old_key, new_dict, new_key):
    """
    Process chained dict values.
    e.g {'a': 'b', 'c': { 'd': 'e': {'f': 'g'}}
    is {'a': 'b', 'c.d': 'e', 'c.d.f': 'g'}

    :param dict old_dict: the content to parse
    :param str old_key: The key to parse.
    :param dict new_dict: Stores the flattened dict
    :param str new_key: Computes what to use as the new key
    """

    # handles first call to this funtion
    # when old_key and new_key are none
    if old_key is None and new_key is None:
        new_key = None
    # handles the second call when
    # old_key is set but new key is nont
    elif new_key is None and old_key is not None:
        new_key = old_key
    # handles all other calls to the method
    elif new_key is not None and old_key is not None:
        new_key = f'{new_key}.{old_key}'

    if old_key is not None and isinstance(old_dict, dict):
        value = old_dict.get(old_key, '')
    else:
        value = old_dict

    # handle dict of dict
    if isinstance(value, dict):
        # loop over all keys in the dict
        for key in value.keys():
            old_dict = value
            parse_dict(old_dict, key, new_dict, new_key)
    # handle array of dict
    elif isinstance(value, list):
        for d in value:
            parse_dict(d, None, new_dict, new_key)

    # recurssion exit condition. At this point
    # we have a key that is not a dict
    elif not isinstance(value, dict):
        return update_dict(new_dict, {new_key: value})

    return new_dict





URLS = ['https://www.surlatable.com/sku/4433397/Date+Night%3A+Everyday+Mediterranean','https://www.visitkc.com/event-detail/panic-disco#sm.0001hk6iq7bttco2y5w2i7f01wej8','https://www.visitcharlottesville.org/event/dali%3a-the-art-of-surrealism-and-paris-school/21325/']
df_urls= pd.read_csv("og_provided_test_data.csv")

URLS = df_urls['url']
URLS = URLS[1000:]
counter = 0   
run_num = 5
group_of_sites = []
bad_urls = open("bad_urls.csv", "w")
bad_urls.write("url,counter,run_num\n")
for url in URLS:
    try:
        #url = URLS[566]
        headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36'}
        response = requests.get(url, headers=headers)
        #------------------parse html page ----------------------------------------------------#
        data=response.text
        soup = BeautifulSoup(data, 'html.parser')
        body_length = len(soup.body.find_all())
        site_length = len(soup.body.find_all()) + len(soup.head.find_all())


        #dealing with overall page
        flattened_attr = []
        title = soup.title.text
        url_text = re.sub("\\+"," ",re.sub(".*/","",unquote(url)))
        headers = {'h1s' : soup.find_all("h1"),
        'h2s' : soup.find_all("h2"),
        'h3s' : soup.find_all("h3")}
        try:
            item_prop = soup.find(itemprop="name").get_text()
        except:
            print("no itemprop")

        for header in headers.values():
            for header_item in header:
                flattened_attr.append(pd.DataFrame(data={'name':[header_item.name],'value': header_item.get_text()}))
        flattened_attr.append(pd.DataFrame(data={"name" : ['title'], 'value' : title}))
        flattened_attr.append(pd.DataFrame(data={"name" : ['url_text'], 'value' : url_text}))
        try:
            flattened_attr.append(pd.DataFrame(data={"name" : ['itemprop'], 'value' : item_prop}))
        except:
            1+1
        df_ready_for_db = pd.concat(flattened_attr)
        df_ready_for_db['position'] = 0
        df_ready_for_db['type'] = "overall"
        #df_ready_for_db.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
        overall_df = df_ready_for_db



        #dealing with head
        flattened_attr = []
        index_track = 0
        for head_element in soup.head.find_all():
            if("name" in head_element.attrs or "meta" in head_element.attrs or "property" in head_element.attrs):
                try:
                    DF_for_content = pd.DataFrame.from_records(head_element.attrs,index=[0])

                    DF_for_content.columns = ["value","name"]
                    flattened_attr.append(
                            DF_for_content
                            )
                except:
                    flattened_attr.append(pd.DataFrame(data={'nested': ['TRUE']}))
                index_track = index_track + 1
        df_ready_for_db = pd.concat(flattened_attr)
        df_ready_for_db['position'] = 0
        df_ready_for_db['type'] = "head"
        #df_ready_for_db.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
        head_df = df_ready_for_db



        json_df = pd.DataFrame()
        #dealing with getting the good json
        flattened_attr = []
        index_track = 0
        for json_script_element in soup.find_all("script"):
            if("pageName:" in json_script_element.get_text() and "digitalData" in json_script_element.get_text()):
                DF_with_name = json_script_element.get_text().split("pageName: ")[1].split(",")[0]
                flattened_attr.append(
                pd.DataFrame(data={'name': 'digitalData','value' : [DF_with_name]}))
            if("type" in json_script_element.attrs and "function" not in json_script_element.get_text() and "<div" not in json_script_element.get_text()):
                try:
                    json_val = json.loads(json_script_element.get_text())
                    flattened_dict = dict()
                    parsed_content = parse_dict(
                        json_val, None, flattened_dict, None)
                    flattened_attr.append(
                            pd.DataFrame({'name': list(parsed_content),'value' : list(parsed_content.values())}))
                except:
                    #flattened_attr.append(pd.DataFrame(data={'value': ['false']}))
                    1+1
            index_track = index_track + 1
        try:
            df_ready_for_db = pd.concat(flattened_attr)
            df_ready_for_db['position'] = 0
            df_ready_for_db['type'] = "json"
            #df_ready_for_db.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
            json_df = df_ready_for_db
        except:
            1+1
            #print("no json worthy")


        itemprop_df = pd.DataFrame()
        #dealing with getting the good itemprop
        try:
            flattened_attr = []
            ItemPropLists = soup.find_all(itemprop="name")
            for ItemPropList in ItemPropLists:
                #ItemPropList= soup.find(itemprop="name").parent.find_all()
                for itemprops in ItemPropList.parent.find_all():

                    #i = soup.find(itemprop="name").parent.find_all()[7]
                    #get tag
                    #i.name
                    #number of elements
                    #item_prop_attribute_count = sum(len(v) for v in i.attrs)
                    if("function(" not in itemprops.get_text()):
                        try:
                            flattened_attr.append(pd.DataFrame(data={'value': [itemprops.get_text()], "name": itemprops.attrs['itemprop']}))
                        except:
                            1+1
            df_ready_for_db = pd.concat(flattened_attr)
            df_ready_for_db['position'] = 0
            df_ready_for_db['type'] = "itemprop"
            #df_ready_for_db.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
            itemprop_df = df_ready_for_db
        except:
            print("no item prop")



        meta_df = pd.DataFrame()
        #dealing with getting the good meta
        try:
            MetaTags= soup.find_all("meta")
            flattened_attr = []
            index_track = 0
            for meta_tag_element in MetaTags:
                 if("function(" not in meta_tag_element.get_text()):
                    try:
                       flattened_attr.append( pd.io.json.json_normalize(meta_tag_element.attrs, sep='_'))
                    except:
                        1+1

                 index_track = index_track + 1
            flattened_attr
            ready_for_cleanup_df = pd.concat(flattened_attr)
            potential_dataframes = []
            try:
                name_value_format_df = ready_for_cleanup_df[['itemprop','content']].dropna()
                name_value_format_df.columns=["name","value"]
                potential_dataframes.append(name_value_format_df)
            except:
                1+1
            try:
                name_value_format_df = ready_for_cleanup_df[['property','content']].dropna()
                name_value_format_df.columns=["name","value"]
                potential_dataframes.append(name_value_format_df)
            except:
                1+1
            try:
                name_value_format_df = ready_for_cleanup_df[['name','content']].dropna()
                name_value_format_df.columns=["name","value"]
                potential_dataframes.append(name_value_format_df)
            except:
                1+1
            df_ready_for_db = pd.concat(potential_dataframes)
            df_ready_for_db['position'] = 0
            df_ready_for_db['type'] = "meta"
            #df_ready_for_db.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
            meta_df = df_ready_for_db
        except:
            print("no meta")



        #dealing with body
        #------------------parse html page ----------------------------------------------------#
        try:
            soup.body.footer.decompose()
        except:
            print("footer happen")
        try:
            soup.body.header.decompose()
        except:
            print("header happen")
        flattened_attr = []
        index_track = 0
        for body_element in soup.body.find_all():
            #i = soup.body.find_all()[64]
            #print(i)
            str_list=body_element.get_text()
            potential_string = ''.join(str_list).strip()
            check_to_see_if_blank = re.sub('\\n|\\t| ', '', str_list)

            if("function(" not in body_element.get_text() and len(check_to_see_if_blank) > 3 and len(check_to_see_if_blank) < 100 ):
                try:

                    DF_of_stuff = pd.DataFrame( {"position":index_track, "name" :body_element.name,"value" : potential_string },index=[0])
                    flattened_attr.append(
                            DF_of_stuff)
                except:
                    flattened_attr.append(pd.DataFrame(data={'innerHTML': ['']}))
            index_track = index_track + 1

        df_ready_for_db = pd.concat(flattened_attr)
        list_tag_tag_remove = []
        for single_tag in df_ready_for_db.name:
            list_tag_tag_remove.append(single_tag  in(['div','li','ul','span']))
        df_ready_for_db['match'] = df_ready_for_db.name.eq(df_ready_for_db.name.shift())
        df_ready_for_db = df_ready_for_db[  (  (df_ready_for_db.match==True) &  (list_tag_tag_remove) ) == False]
        df_ready_for_db = df_ready_for_db[['position','name','value']]
        df_ready_for_db['type'] = "body"
        body_df = df_ready_for_db
        body_df
        #body_df.to_csv("C:\Git\Consulting-OG\sample_data\difference3.csv")
        full_db_load = pd.concat([body_df,meta_df,itemprop_df,json_df, head_df, overall_df])
        full_db_load['body_length'] = body_length
        full_db_load['site_length'] = site_length
        full_db_load['url'] = url
        group_of_sites.append(full_db_load)
        counter = counter +1
        print(url)
        print(counter)
        print(run_num)
        if(counter == 250):
           group_full_db_load = pd.concat(group_of_sites)
           group_full_db_load["index"] = range(0,len(group_full_db_load))
           group_full_db_load['run_num'] = run_num
           group_full_db_load.to_csv("scraped_data_run"  + str(run_num) + ".csv", index=False)
           group_of_sites = []
           counter = 0
           run_num = run_num + 1
           del group_full_db_load
    except:
        bad_urls.write(url + "," + str(counter) + "," + str(run_num) + "\n")

bad_urls.close()
group_full_db_load = pd.concat(group_of_sites)
group_full_db_load["index"] = range(0,len(group_full_db_load)) 
group_full_db_load['run_num'] = run_num
group_full_db_load.to_csv("scraped_data_run"  + str(run_num) + ".csv", index=False)   
    






#article-wrapper > p:nth-child(2) > strong













































