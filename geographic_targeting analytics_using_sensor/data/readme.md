Cleaned.csv is an enriched data set.
It include the difference in time, UTM-easting, UTM-northing, and raw height for consecutive events by a single wolf. Note that the first measurement for each wolf will have NaN (Null) as the difference.

Differences are used to calculate new features:

distance: three-dimensional distance traveled in meters since last measurement, calculated using Euclidean method: for easting, northing, and height
elasped hours: number of hours between consecutive measurements
speed m/hr: distance divided by hours

Original Data from SOCOM

Movement data from grey wolves (Canis lupus) in northeastern Alberta's Athabasca Oil Sands Region. Data have been used to investigate habitat use and selection (Boutin et al. 2015), predator-prey dynamics (Neilson and Boutin 2017), effects of human activity (Boutin et al. 2015; Neilson and Boutin 2017), and responses to snow conditions (Droghini and Boutin 2018). This study is participating in the Arctic Animal Movement Archive (AAMA).

Data Dictionary
https://www.movebank.org/cms/movebank-content/movebank-attribute-dictionary

Study - ABoVE: Boutin Alberta Grey Wolf
https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study492444603 

Map View
https://www.movebank.org/cms/webapp?gwt_fragment=page=search_map 

Study Statistics

Number of Animals	46
Number of Tags	43
Number of Deployments	46
Time of First Deployed Location	2012-03-17 16:01:33.000
Time of Last Deployed Location	2014-09-13 09:01:53.000
Taxa	Canis lupus
Number of Deployed Locations	239194
Number of Records	Deployed (outliers) / Total (outliers)
GPS	239194 (0) / 239196 (0)

Individual Wolf Reference data (with Study-Site attribute that we might use for a wolf pack designation) also at https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study492444603

PUBLISHED FINDINGS:

The calm during the storm: Snowfall events decrease the movement rates of grey wolves (Canis lupus)
Inferring movement behavior from telemetry data
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6209196/ 

Snowfall Data: 
https://github.com/adroghini/wolves_snowfall
