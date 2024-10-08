# A multi-species approach to manage effects of land cover and weather on upland game birds

This repository contains the data and code used in the following publication:

Schindler, A. R., D. A. Haukos, C. A. Hagen, B. E. Ross. (2020). A multi-species approach to manage effects of land cover and weather on upland game birds. *Ecology and Evolution*, **10**(24):14330-14345. https://doi.org/10.1002/ece3.7034.

Code/Data DOI: https://doi.org/10.5061/dryad.c59zw3r5w

All analyses were completed using R. See manuscript for details about analyses and supplementary data for example code.

Please contact the corresponding author with questions about this data package or to seek potential collaborations using these data.

# Description

To run the models described in the manuscript, use the R code with the corresponding species code:
- `LEPC`: lesser prairie-chicken
- `GRPC`: greater prairie-chicken
- `NOBO`: northern bobwhite
- `RNEP`: ring-necked pheasant

The .txt files contain the JAGS model code for either the % grassland cover (grass) or edge density (ed) models. Use the .txt files with the corresponding prefixes:
- `lepc_grpc*`: lesser and greater prairie-chicken models
- `nobo_rnep*`: northern bobwhite and ring-necked pheasant models

All data from the analyses is contained in [data](./data).

# Abstract
Loss and degradation of grasslands in the Great Plains region have resulted in major declines in abundance of grassland bird species. To ensure future viability of grassland bird populations, it is crucial to evaluate specific effects of environmental factors among species to determine drivers of population decline and develop effective conservation strategies. We used threshold models to quantify the effects of land cover and weather changes in lesser prairie-chicken and greater prairie-chicken (*Tympanuchus pallidicinctus* and *T. cupido*, respectively), northern bobwhites (*Colinus virginianus*), and ring-necked pheasants (*Phasianus colchicus*). We demonstrated a novel approach for estimating landscape conditions needed to optimize abundance across multiple species at a variety of spatial scales. Abundance of all four species was highest following wet summers and dry winters. Prairie-chicken and ring-necked pheasant abundance was highest following cool winters, while northern bobwhite abundance was highest following warm winters. Greater prairie-chicken and northern bobwhite abundance was also highest following cooler summers. Optimal abundance of each species occurred in landscapes that represented a grassland and cropland mosaic, though prairie chicken abundance was optimized in landscapes with more grassland and less edge habitat than northern bobwhites and ring-necked pheasants. Because these effects differed among species, managing for an optimal landscape for multiple species may not be the optimal scenario for any one species.
