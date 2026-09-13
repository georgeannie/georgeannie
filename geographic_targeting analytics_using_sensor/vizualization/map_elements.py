import ipywidgets as widgets
import ipyleaflet as leaflet



# MAP 1
country_select_text = widgets.HTML(value = "<b>Select the country or entity that owns the satellite: </b>")
sat_selector_label = widgets.HTML(value="<b> Select the satellite: </b>")
basemap_layer = leaflet.basemap_to_tiles(leaflet.basemaps.OpenStreetMap.Mapnik)
vessels_missed_number = widgets.HTML(value = "<center>")
date_slider_text = widgets.HTML(value="<b> Choose the time period to show satellite field of view for: </b>")
area_covered_number = widgets.HTML(value = "<center>")
vessels_seen_number = widgets.HTML(value = "<center>")
# progress_test = widgets.HTML(value = "start")
reset_map1_button = widgets.Button(description = "Update Map")
# progress_text1 = widgets.HTML(value = "")
year_select_text = widgets.HTML(value = "<b>Select the year to show field of view for: </b>")
year_selector = widgets.Dropdown(options = [2015, 2016, 2017], description = 'Year: ')
month_selector = widgets.Dropdown(options = ['January', 'February', 'March', 'April', 'May', 'June', 'July',
                                             'August', 'September', 'October', 'November', 'December'], description = 'Month: ')
br = widgets.HTML(value = "<br>")




# Map 2
header2 = widgets.HTML("<h2> Given a time and location, what vessels were in the field of view of a satellite?</h2>", layout=widgets.Layout(height='auto'))
basemap_layer = leaflet.basemap_to_tiles(leaflet.basemaps.OpenStreetMap.Mapnik)
sat_selector_label2 = widgets.HTML(value="<b> Select the satellite: </b>")
date_slider_text2 = widgets.HTML(value="<b> Choose the time period to show satellite views for: </b>")
reset_map_button = widgets.Button(description = "Reset Map")
loading_text = widgets.HTML(value = "")
out = widgets.Output(layout={'border': '1px solid black'})
us_area_cov_text = widgets.HTML(value="<center> <br> <b> Percentage of drawn area covered by US satellites: </b>")
us_area_cov_num = widgets.HTML(value="<br>")
other_area_cov_num = widgets.HTML(value="<br>")
# instructions = widgets.HTML(value = "<center> <b>Draw a polygon on the map and select <br> a time range to see veesels viewed by satellies <br> within the selected area. </b>")
# progress_text = widgets.HTML(value = "")
