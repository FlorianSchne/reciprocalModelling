## Data-driven, reciprocal modelling

This repository contains R scripts, which were used for the following paper:   
Schneider, F., Poeplau, C., & Don, A. (submitted). Predicting ecosystem responses by data-driven reciprocal modelling.

Here, we (the authors) provide materials to reproduce the case study about soil organic carbon. The goal was to quantify the amount of atmospheric carbon that could be sequestered in European agricultural soils if today's cropland were converted to grassland. 

There are three main scripts, which build upon each other:  

- `01_compileData.R` was used to compile the training dataset `data/derived/goodData.rds`
- `02_reciprocalModels.R` was used to implement the data-driven, reciprocal modelling
- `03_postHoc.R` was used to examine site-specific differences in soil carbon sequestration

The files in the *annex* folder were used to create the Supplemental Material provided along the paper. 

All raw data used was publicly available and can be obtained free of charge. The training data originated from the following sources:  

- [European Soil Data Centre (ESDAC)](https://esdac.jrc.ec.europa.eu/), European Commission, Joint Research Centre<sup>1</sup> 
  - [LUCAS 2015 TOPSOIL data](https://esdac.jrc.ec.europa.eu/content/lucas2015-topsoil-data)
    - `LUCAS_Topsoil_2015_20200323.xlsx`
    - `LUCAS2015_AncillaryData_20201007.xlsx`  
  - [LUCAS 2009 TOPSOIL data](https://esdac.jrc.ec.europa.eu/content/lucas-2009-topsoil-data)  
    - `LUCAS_Topsoil_2009.xlsx`  
    - `Bulgaria.xlsx`  
    - `Romania.xlsx`  
- [European Statistical Office (EUROSTAT)](https://ec.europa.eu/eurostat)  
  - [LUCAS primary data 2015. LUCAS micro data 2015](https://ec.europa.eu/eurostat/web/lucas/data/primary-data/2015)  
  - `EU_2015_20200225.csv`  
  - Agriculture (agr) database  
    - This data was downloaded with the [eurostat R package](https://ropengov.github.io/eurostat/)<sup>2</sup> from the [EUROSTAT database](https://ec.europa.eu/eurostat/web/agriculture/data/database) as documented in `01_compileData.R`  
  - [Timeseries of NUTS polygons](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts)  
    - `NUTS_RG_01M_2003_4326.shp`  
    - `NUTS_RG_01M_2006_4326.shp`  
    - `NUTS_RG_01M_2010_4326.shp`  
    - `NUTS_RG_01M_2013_4326.shp`  
    - `NUTS_RG_01M_2016_4326.shp`  
    - `NUTS_RG_01M_2021_4326.shp`
- [OpenDataScience.eu](https://maps.opendatascience.eu)
  - [Mean annual NDVI values (2000-2019)](https://data.opendatascience.eu/geonetwork/srv/eng/catalog.search#/metadata/b69476b8-4d6c-4381-a719-649574cb917e) from Landsat images, as pre-processed by Tomislav Hengl
    - Cloud Optimized GeoTIFF (COG) files were accessed via `terra::rast()` as described in `01_compileData.R`  
    

<sup>1</sup> The LUCAS topsoil datasets used in this work was made available by the European Commission through the European Soil Data Centre managed by the Joint Research Centre (JRC).  
<sup>2</sup> Leo Lahti, Przemyslaw Biecek, Markus Kainu and Janne Huovari. Retrieval and analysis of Eurostat open data with the eurostat package. R Journal 9(1):385-392, 2017. R package version 3.6.84 