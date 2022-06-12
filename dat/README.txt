
Bath & North East Somerset ANPR Camera Survey Data
==================================================

Contents
--------

notes.txt - this file
sites.csv - ANPR camera survey sites
vehicles.csv - vehicles observed during survey
observations.csv - observations of vehicles at ANPR sites
copert2016_emissions.csv - vehicle / emissions lookup based on simplification of the Excel files with Emission Factor Functions.xlsx file at https://www.emisia.com/utilities/copert/documentation/
naei2016_emissions.csv - vehicle / emissions lookup based on manual simplification of Table 3 from Fleet Weighted Road Transport Emission Factor 2016 file at http://naei.beis.gov.uk/data/ef-transport

sites.csv
---------

id - sequential identifier
name - original name of camera site
description - description of site location
location_id - camera location id
direction - direction of travel
radial_group - location relative to city centre - 1 = outer ring, 2 = inner ring, 3 = city centre car park, 4 = unclassified
in_out - direction towards city centre or entry/exit for car park - 1 = inbound, 2 = outbound
longitude - WGS84 longitude
latitude - WGS84 latitude

Site 63 (ANPR31c) found in original list of sites but unused in observations, thus removed. **Location of site 61 has been changed**.

vehicles.csv
------------

id - identifier (UUID to anonymise vehicles)
make - make of vehicle e.g. Ford, Ferrari
type - type of vehicle e.g. CARS, PSVs
subtype - subtype of vehicle e.g. Moped, "Fare Stage Buses"
intro_date - date vehicle was introduced
euro_status - Euro status (see https://en.wikipedia.org/wiki/European_emission_standards)
engine_capacity - engine capacity in cm3
gross_vehicle_weight - gross vehicle weight in kg
fuel_type - fuel type e.g. Diesel, Petrol
co2 - CO2 emissions in g/km
fc_combined - fuel consumption across combined conditions in L/100km
fc_extra_urban - fuel consumption outside urban area in L/100km
fc_urban_cold - fuel consumption from cold start within urban area in L/100km

Vehicle make may be "Suppressed" where 5 or less vehicles of that make were observed.

Some data values maybe null where information is not available for a vehicle.

Text values are original raw values and may require consistency improvements e.g. Petrol & PETROL may need to be converted to a single value.

observations.csv
------------

id - sequential identifier
t - UTC timestamp of observation
site_id - site of observation
vehicle_id - vehicle observed

copert2016_emissions.csv
------------------------

type - vehicle type
fuel_type - vehicle fuel type
euro_status - vehicle euro status E0 - E6
nox - NOx in g/km
pm_exhaust - PM from exhaust in g/km
voc - Volatile organic compounds in g/km
co - CO in g/km
nh3 - Ammonia in g/km
ch4 - Methane in g/km
n2o - Nitrous oxide in g/km


Values are averages across much more complex criteria for which we have no vehicle data. See original spreadsheet for details and comparison.

Vehicles should be matched using all of type, fuel_type and euro_status. Data not available for unmatched vehicles. Not all matches have all pollutant types.

These data are used as input data to multiple official tools and are likely to be more useful than the NAEI data.


naei2016_emissions.csv
----------------------

type - vehicle type
subtype - vehicle subtype
fuel_type - vehicle fuel type
nox - NOx in g/km
pm10 - PM10 in g/km
pm2_5 - PM2.5 in g/km
co - CO in g/km
voc - Volatile organic compounds in g/km
nh3 - Ammonia in g/km
so2 - SO2 in g/km
benzene - Benzene in g/km
n2o - Nitrous oxide in g/km

Values have been manually corresponded to appropriate vehicle type / subtype / fuel type.

Vehicles should be matched using all of type, subtype and fuel_type. Data not available for unmatched vehicles.

These data are much more generalised than the COPERT data and tend to underestimate pollutants when compared the COPERT data. 


CAVEAT ABOUT POLLUTANT DATA
---------------------------

The lookup tables provided here are far from rigorous and are intended to be illustrative.

Calculating pollutant values for vehicles is a very complex topic. Much more information than we have about a vehicle is required to make an accurate prediction of pollutants produced.

At best these pollutant tables should be used in an internally comparative way i.e. we can compare aspects of pollution within our own system but we should not compare our output data with data produced by other organisations using different pollutant modelling techniques. At best our results will be suggestive, at worst our results may be massively over-simplified.

In short, if your results are at odds with others', don't pick a fight with them.


Licence
-------

For COPERT and NAEI, the data appear to be open but please refer to the accompanying documents to be found at the URLs above.

Data are licenced under the Open Government Licence (http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) by Bath & North East Somerset Council.

When publishing these data or data derived from them you should include the following attribution :-

Contains data published by B&NES Council under the Open Government Licence v3.0.

You may also include the following attribution :-

Powered by Bath: Hacked


Contact
-------

For further information about this data contact Mark Owen <mark@bathhacked.org>




