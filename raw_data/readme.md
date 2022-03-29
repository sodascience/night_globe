# data sources

## GaN2020.csv
Globe at night data from https://www.globeatnight.org/2020data/GaN2020.csv

Downloaded 23-06-2021

License: http://opendatacommons.org/licenses/odbl/

## us_states
Us state shapefiles from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

License: unknown

## Radiance
Download the 10.8GB file `VNL_v2_npp_2020_global_vcmslcfg_c202101211500.median.tif.gz` from [`https://eogdata.mines.edu/nighttime_light/annual/v20/2020/`](https://eogdata.mines.edu/nighttime_light/annual/v20/2020/), and unzip it in the `raw_data/` folder.

License: unknown

For description, see [`https://eogdata.mines.edu/products/vnl/`](https://eogdata.mines.edu/products/vnl/)

Earth Observation Group, Payne Institute for Public Policy

C. D. Elvidge, K. Baugh, M. Zhizhin, F. C. Hsu, and T. Ghosh, “VIIRS night-time lights,” International Journal of Remote Sensing, vol. 38, pp. 5860–5879, 2017.

Elvidge, C.D, Zhizhin, M., Ghosh T., Hsu FC, Taneja J. Annual time series of global VIIRS nighttime lights derived from monthly averages: 2012 to 2019. Remote Sensing 2021, 13(5), p.922, doi:10.3390/rs13050922

## Land use
The land use data is downloaded as part of the script `01_data_loading.R` from the OGC WMS service of the MRLC: https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?service=WMS.