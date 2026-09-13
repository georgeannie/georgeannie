dwell-time-analysis.ipynb - Python Notebook to create enriched data set ("cleaned.csv.zip")

Calculates the difference in time, UTM-easting, UTM-northing, and raw height for consecutive events by a single wolf. Note that the first measurement for each wolf will have NaN (Null) as the difference.

Differences are used to calculate new features:
- distance: three-dimensional distance traveled in meters since last measurement, calculated using Euclidean method: for easting, northing, and height
- elasped hours: number of hours between consecutive measurements
- speed m/hr: distance divided by hours

Wolf Dwell Time.twb - Tableau Notebook to explore distance, elapsed time, and speed.  Isolates a few outliers - but overall the data is consistent.

"cleaned.csv.zip" is stored in the WolfAnalytics/data folder

![Alt text](./hideouts.png?raw=true "Sample Hidout locations for one Wolf Pack")
