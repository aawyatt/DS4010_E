## COBS Weather Data
`COBS weather data.csv` contains data for the mean air temperature, precipitation amounts, and reference evapotranspiration for the growing season and annually. Reference evapotranspiration is the estimate of evapotranspiration from the reference crop. According to the U.S. Geological Survey, "Evapotranspiration is the sum of all processes by which water moves from the land surface to the atmosphere via evaporation and transpiration" (https://www.usgs.gov/special-topics/water-science-school/science/evapotranspiration-and-water-cycle).

Daily precipitation and mean air temperature data were collected at a weather station ca. 11 km from the research site. Growing season precipitation and mean air temperature data spanned from **1 April through 31 October for 2009-2022**; total annual precipitation and mean annual air temperature data spanned from **1 January through 31 December of each year**. Cumulative reference evapotranspiration (ET) was calculated for **each growing season and year (except for 2014)** using the American Society of Civil Engineers standardized reference ET equation and relevant local data for temperature, humidity, wind speed, radiation, and soil heat flux applied to a reference crop of alfalfa. Reference ET assumes a full plant canopy and a water-rich soil environment and allows for evaluation of the combined effects of temperature, humidity, wind, and radiation on the amount of water moving from soil and plants into the atmosphere. Due to a change in data management systems, not all data required for calculation of ET were available for May and June 2014, so growing season and annual ET were not available for that year.

## Data Dictionary

*Year*: year of measurement, between 2009-2022 (date, YYYY)

*Annual precip mm*: cumulative precipitation in mm, 1 January-31 December (positive float)

*Growing season precip mm (1Apr-31Oct)*: cumulative precipitation in mm, 1 April-31 October (positive float)

*Annual ave. air temp C*: average daily air temperature in °C, 1 January-31 December (positive float)

*Growing season ave. air temp C (1Apr-31Oct)*: average daily air temperature in °C, 1 April-31 October (positive float)

*Annual ET mm*: cumulative reference evapotranspiration in mm, 1 January-31 December (positive float)

*Growing season ET mm (1Apr-31Oct)*: cumulative reference evapotranspiration in mm, 1 April-31 October (positive float)

The data dictionary is adapted from the study the data is taken from: "Multi-year productivity and nitrate-nitrogen loss from corn and prairie bioenergy cropping systems" (https://iastate.figshare.com/articles/dataset/Data_for_Multi-year_productivity_and_nitrate-nitrogen_loss_from_corn_and_prairie_bioenergy_cropping_systems/27144879/1?file=49550532)
