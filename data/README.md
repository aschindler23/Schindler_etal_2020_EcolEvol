# A multi-species approach to manage effects of land cover and weather on upland game birds

This foler contains the data and used in the following publication:

Schindler, A. R., D. A. Haukos, C. A. Hagen, B. E. Ross. (2021). A multi-species approach to manage effects of land cover and weather on upland game birds. Ecology and Evolution 10(24):14330-14345. https://doi.org/10.1002/ece3.7034.

All analyses were completed using R. See manuscript for details about analyses and supplementary data for example code.

Please contact the corresponding author with questions about this data package or to seek potential collaborations using these data.

## Count data:
- GRPC_count_data.csv
- LEPC_count_data.csv
- NOBO_count_data.csv
- RNEP_count_data.csv

These files contain count data collected for greater prairie-chickens (GRPC), lesser prairie-chickens (LEPC), northern bobwhites (NOBO), and ring-necked pheasants (RNEP) during annual surveys conducted by the Kansas Department of Wildlife, Parks, and Tourism.

### Column names:
- Year: Year of observation
- Route: Unique ID for each survey route
- Visit: Which survey visit number (1 or 2) to the route in a given year (GRPC and LEPC data only)
- Count: The number of males counted on identified leks (GRPC and LEPC data), number of calling males counted (NOBO data), or number of crowing calls counted (RNEP data)

## Environmental data:
- GRPC_environmental_data.csv
- LEPC_environmental_data.csv
- NOBO_environmental_data.csv
- RNEP_environmental_data.csv

These files contain the landscape metrics and weather data for greater prairie-chicken (GRPC), lesser prairie-chicken (LEPC), northern bobwhite (NOBO), and ring-necked pheasant (RNEP) routes used in analyses.

### Coumn names:
- Year: Year of landscape metric or weather data
- Route: Unique ID for each survey route
- GRASS_3km: % grassland cover within a 3-km buffer around the survey route
- GRASS_5km: % grassland cover within a 5-km buffer around the survey route
- GRASS_10km: % grassland cover within a 10-km buffer around the survey route
- ED_3km: Edge density (m/ha) of grassland patches within a 3-km buffer around the survey route
- ED_5km: Edge density (m/ha) of grassland patches within a 5-km buffer around the survey route
- ED_10km: Edge density (m/ha) of grassland patches within a 10-km buffer around the survey route
- PDSI: Palmer Drought Severity Index of summer months
- TMAX: Maximum temperature of summer months (degrees Fahrenheit)
- PCP: Precipitation index of winter months
- TMIN: Minimum temperature of winter months (degrees Fahrenheit)
