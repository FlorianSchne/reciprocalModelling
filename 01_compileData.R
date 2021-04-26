# Basics
library(tidyverse)
library(here)
library(readxl)
library(writexl)
# Geodata
library(sf)
library(lwgeom)
# Eurostat data
library(eurostat)
library(rvest)
# Speed
library(tictoc)
library(foreach)
library(doParallel)
library(data.table)
setDTthreads(threads=0) # spare on memory

### Load helper functions
source(here("functions.R"))

#### LUCAS soil point data #####
# 1) Load LUCAS 2015 core data 
# 2) Fill missing soil texture values with values from 2009 or 2012
# 3) Add the readily available ancillary data
# 4) Clean
rawData = read_excel(here("data","raw","esdac","15",
                          "LUCAS_Topsoil_2015_20200323.xlsx"),
                     sheet=1,
                     .name_repair="universal") %>% 
  left_join(read_excel(here("data","raw","esdac","09"
                            ,"LUCAS_Topsoil_2009.xlsx"),
                       sheet=1,
                       .name_repair="universal") %>% 
              rename(Point_ID=POINT_ID,
                     Coarse09=coarse,
                     Clay09=clay,
                     Silt09=silt,
                     Sand09=sand) %>% 
              select(Point_ID,
                     ends_with("09")) %>% 
              mutate(Point_ID=as.numeric(Point_ID))) %>% 
  left_join(read_excel(here("data","raw","esdac","12",
                            "Bulgaria.xlsx"),
                       sheet=1,
                       .name_repair="universal") %>% 
              bind_rows(read_excel(here("data","raw","esdac","12",
                                        "Romania.xlsx"),
                                   sheet=1,
                                   .name_repair="universal")) %>% 
              rename(Point_ID=POINT_ID,
                     Coarse12=coarse,
                     Clay12=clay,
                     Silt12=silt,
                     Sand12=sand) %>% 
              select(Point_ID,
                     ends_with("12")) %>% 
              mutate(Point_ID=as.numeric(Point_ID)) %>% 
              mutate_all(~ replace(., . ==(-999), NA))) %>% 
  mutate(Clay=case_when(!is.na(Clay)~Clay,
                        is.na(Clay)&is.na(Clay12)~Clay09,
                        is.na(Clay)&is.na(Clay09)~Clay12,
                        T~NA_real_),
         Silt=case_when(!is.na(Silt)~Silt,
                        is.na(Silt)&is.na(Silt12)~Silt09,
                        is.na(Silt)&is.na(Silt09)~Silt12,
                        T~NA_real_),
         Sand=case_when(!is.na(Sand)~Sand,
                        is.na(Sand)&is.na(Sand12)~Sand09,
                        is.na(Sand)&is.na(Sand09)~Sand12,
                        T~NA_real_),
         Coarse=case_when(!is.na(Coarse)~Coarse,
                          is.na(Coarse)&is.na(Coarse12)~as.numeric(Coarse09),
                          is.na(Coarse)&is.na(as.numeric(Coarse09))~Coarse12,
                          T~NA_real_)) %>% 
  left_join(read_csv(here("data","raw","esdac","15",
                          "LUCAS2015_AncillaryData_20201007.csv")) %>% 
              rename(Point_ID=POI) %>% 
              select(-Elevation) %>% 
              mutate(Aspect=ifelse(is.na(Aspect),0,Aspect))) %>% 
  left_join(st_read(here("data","raw","esdac","15","LUCAS_Topsoil_2015_20200323-shapefile",
                         "LUCAS_Topsoil_2015_20200323.shp")) %>% 
              mutate(lon = st_coordinates(.)[,1],
                     lat = st_coordinates(.)[,2]) %>% 
              select(Point_ID,
                     lon,lat,
                     LC,LU,LC0_Desc) %>% 
              as.data.frame() %>% 
              select(-geometry)) %>% 
  left_join(read_csv(here("data","raw","eurostat",
                          "EU_2015_20200225.csv")) %>% 
              rename(Point_ID=POINT_ID) %>% 
              select(Point_ID,
                     WM) %>% 
              mutate(WM=case_when(WM==1~"Irrigation",
                                  WM==2~"Potential irrigation",
                                  WM==3~"Drainage",
                                  WM==4~"Irrigation and drainage",
                                  WM==5~"not visible",
                                  T~"not relevant"))) %>% 
  
  mutate(
    # Convert organic C to mass-%
    OC=OC/10, 
    # Delete organic C values below detection limit
    OC=ifelse(OC<0.2,NA,OC), 
    # Pedotransfer function to estimate dry bulk density 
    # Source: Hollis JM, Hannam J, Bellamy PH (2012) Empirically-derived pedotransfer functions 
    # for predicting bulk density in European soils. European Journal of Soil Science, 63, 96–109.
    BD=0.80806 + (0.823844 * exp(-0.27993*OC) + (0.0014065*Sand) - (0.0010299*Clay)),
    # Calculation of organic C stocks in 0-20 cm depth (target)
    OCstock=OC*BD*20*(1-(Coarse/100)),
    # Convert nutrients to mass-%
    N=N/10,
    P=P/10^4,
    K=K/10^4,
    # Delete nitrogen values below detection limit
    N=ifelse(N<0.02,NA,N),
    # Calculate C:N ratios
    CN=ifelse(!is.na(OC)&!is.na(N),OC/N,NA),
    # Calculate corresponding nutrient stocks
    Nstock=N*BD*20*(1-(Coarse/100)),
    Pstock=P*BD*20*(1-(Coarse/100)),
    Kstock=K*BD*20*(1-(Coarse/100)),
    # Convert CaCO3 to mass-%
    CaCO3=CaCO3/10,
    # Make "natura" variable indicating natural reserves to binary (yes/no)
    natura=ifelse(!is.na(Natura2000_sitecode1)|!is.na(Natura2000_sitecode2),"yes","no"),
    # Shorten the names of Bio-geographical regions
    BioGeo=str_remove(BioGeo," Bio-geographical Region")) %>% 
  
  # Rename to make it look nicer
  rename(pH_CaCl2=pH.CaCl2.,
         pH_H2O=pH.H2O.) 

saveRDS(rawData,
        here("data","derived",
             "rawData.rds"))


#### Omit rows...
plyr::count(rawData$LC0_Desc%in%c("Cropland","Grassland")) # 8136 sites
plyr::count(rawData$OC<=15/1.72) # 1971 sites
plyr::count(!is.na(rawData$CN)) # 73 sites
plyr::count(rawData$CaCO3<5) # 4514 sites

smallData = rawData %>% 
  filter(
    # with land use neither grassland nor cropland
    LC0_Desc%in%c("Cropland","Grassland"),
    # with missing organic C measurements
    !is.na(OC),
    # with organic soil (restrict analyses to mineral soils only)
    OC<=15/1.72,
    # with >= 5 % CaCO3 (analytical uncertainty in organic C measurements)
    CaCO3<5)
saveRDS(smallData,
        here("data","derived",
             "smallData.rds"))

#### EUROSTAT covariates #####
# EUROSTAT offers potentially relevant explanatory variables aggregated at various administrative levels. 
# This data was downloaded from the 'agriculture' (agr) database.
# Downloads were restricted to data reported on average at NUTS2 level or at finer resolution. 
# Processing was as follows:
# 1. For each sampling location of LUCAS soil, determine corresponding NUTS IDs between 2003 and 2021
# 2. For each variable, restrict to highest NUTS level (finest spatial resolution) available
# 3. Merge with LUCAS point data by NUTS ID and year
# 4. For each point, calculate mean value across all years

## Step 1
lucas=st_read(here("data","raw","esdac","15","LUCAS_Topsoil_2015_20200323-shapefile",
                   "LUCAS_Topsoil_2015_20200323.shp")) %>%
  select(Point_ID)

if (.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(detectCores()-1)
} else {
  cl = makePSOCKcluster(detectCores()-1)
  registerDoParallel(cl)
}
tic()
key=foreach(i=c(2003,2006,2010,2013,2016,2021),
            .combine = 'rbind',
            .packages = c("tidyverse","sf","lwgeom","here")) %dopar% {
              nuts=st_read(here("data","raw","eurostat",paste0("nuts",i),
                                paste0("NUTS_RG_01M_",i,"_4326.shp"))) %>%
                st_make_valid() %>%
                select(NUTS_ID) %>%
                mutate(nutYear=i,
                       nutArea=units::set_units(st_area(.), km^2))
              
              nutsLucas=st_join(nuts,
                                lucas,
                                join=st_contains) %>%
                filter(!is.na(Point_ID)) %>%
                st_drop_geometry() %>%
                as_tibble()
              
              # If LUCAS sampling point is without NUTS region, choose closest
              # NUTS neighbor (if it is <5 km away)
              missing=lucas$Point_ID[!(lucas$Point_ID%in%nutsLucas$Point_ID)]
              if(length(missing)>0){
                patch=st_distance(lucas %>%
                                    filter(Point_ID%in%missing) %>%
                                    # Transform crs so that units are in meter 
                                    st_transform(3857), 
                                  nuts %>%
                                    st_transform(3857),
                                  sparse=F) %>%
                  as_tibble() %>%
                  rename_all(~nuts$NUTS_ID) %>%
                  mutate(Point_ID=missing) %>%
                  gather(NUTS_ID,dist,1:ncol(.)-1) %>%
                  group_by(Point_ID) %>%
                  mutate(mindist=min(dist),
                         nutYear=2021) %>%
                  filter(dist==mindist,
                         dist<units::set_units(5000,"m")) %>%
                  select(-ends_with("dist")) %>%
                  arrange(NUTS_ID)
                nutsLucas=nutsLucas %>%
                  bind_rows(patch)
              }
              
              nutsLucas
              
            }
toc() # 10min
try(registerDoSEQ(),silent=T)
try(stopCluster(cl),silent=T)

key=key %>%
  mutate(nutLevel=case_when(nchar(NUTS_ID)==2~0,
                            nchar(NUTS_ID)==3~1,
                            nchar(NUTS_ID)==4~2,
                            nchar(NUTS_ID)==5~3)) %>%
  select(Point_ID,nutYear,nutLevel,NUTS_ID,nutArea) %>%
  arrange(Point_ID,nutYear,nutLevel) %>%
  distinct()

# QS: Uniqueness - manual correction
temp=key %>%
  distinct() %>%
  group_by(Point_ID,nutYear,nutLevel) %>%
  add_tally() %>%
  ungroup() %>%
  filter(n>1)
drop=key %>%
  distinct() %>%
  group_by(Point_ID,nutYear,nutLevel) %>%
  add_tally(name="n0") %>%
  ungroup() %>%
  filter(Point_ID %in% Point_ID[n0>1]) %>%
  filter(nutLevel %in% nutLevel[n0>1]) %>%
  group_by(Point_ID,NUTS_ID) %>%
  add_tally(name="n1") %>%
  ungroup() %>%
  filter(n0>1,
         n1==1)
key=key %>%
  anti_join(drop) %>%
  drop_na()

saveRDS(key,
        here("data","derived",
             "key.rds"))

## Steps 2 to 4
key=readRDS(here("data","derived",
                 "key.rds")) %>% 
  select(-nutYear) %>% 
  distinct()

### Area by land use (absolute) 
# UAA   - Utilized agricultural area
# ARA   - Arable land 
# J0000 - Permanent grassland 
# PECR  - Permanent crops 
# WA    - Wooded areas 
raw=get_eurostat("ef_lus_main",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                farmtype="TOTAL",
                                unit="HA",
                                crops=c("UAA","ARA","J0000","PECR","WA"),
                                so_eur="TOTAL")) %>% 
  select(-agrarea,-farmtype,-so_eur,-unit)

#--
# Total agricultural area EU27+UK
raw %>% 
  filter(nchar(geo)==2) %>% 
  group_by(geo,crops) %>% 
  mutate(temp=max(time)) %>% 
  filter(time==temp) %>% 
  filter(!(geo%in%c("NO","MK","EU"))) %>% 
  group_by(crops) %>% 
  summarise(values=sum(values)) %>% 
  mutate(ref=values[crops=="UAA"],
         rel=values/ref)
12.1*(103106090+10505460)/10^9 # Mg C
#--

temp=raw %>% 
  filter(nchar(geo)==4) %>% 
  mutate(geo=substr(geo,1,3)) %>%
  group_by(crops,geo,time) %>% 
  summarise(values=sum(values)) %>% 
  ungroup()
area=raw %>% 
  bind_rows(temp) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  rename(class=crops) %>% 
  group_by(Point_ID,nutLevel,class) %>% 
  summarise(area=mean(values,na.rm=T)) %>% 
  ungroup() %>% 
  arrange(Point_ID,class,nutLevel)

### In percent of total utilized agricultural area (UAA) 
### Farm structure - main indicators
raw=get_eurostat("ef_m_farmleg",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                indic_agr=c("UAA_HA","FARM_LSU_LS"),
                                leg_form="TOTAL",
                                so_eur="TOTAL")) %>% 
  select(-agrarea,-leg_form,-so_eur) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("farmtype","indic_agr"))

# Livestock units per ha
dat=raw %>% 
  filter(farmtype=="TOTAL") %>% 
  spread(indic_agr,values) %>% 
  mutate(animalDens=FARM_LSU_LS/UAA_HA) %>% 
  select(Point_ID,
         animalDens   # Total livestock units per hectare total
  )

# Farm types (area in percent of total)
dat=dat %>% 
  full_join(raw %>% 
              filter(indic_agr=="UAA_HA") %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[farmtype=="TOTAL"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID ,farmtype,share) %>% 
              spread(farmtype,share) %>% 
              select(-TOTAL))

### Farm structure 
# Organic farming and land use (area in percent of total)
raw=get_eurostat("ef_lus_main",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                farmtype="TOTAL",
                                unit="HA",
                                crops=c("UAA","ARA","J0000","PECR","WA","FCONV_UCONV"),
                                so_eur="TOTAL")) %>% 
  select(-agrarea,-farmtype,-so_eur,-unit) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("crops"))

dat=dat %>% 
  full_join(raw %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[crops=="UAA"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,crops,share) %>% 
              mutate(crops=paste0(crops,"relUAA")) %>% 
              spread(crops,share) %>% 
              select(-UAArelUAA))

### Farm structure 
# Crops in percent of cropland / grassland / permanent crops
raw=get_eurostat("ef_lus_allcrops",
                 time_format="num") %>% 
  filter(agrarea=="TOTAL",
         unit=="HA") %>% 
  select(-agrarea,-unit) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("crops"))

# Cropland
dat=dat %>% 
  full_join(raw %>%
              filter(str_detect(crops, "ARA|^C|^P0|^P1|^P9|^R|^I|^G|^V|^N|^Q")) %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[crops=="ARA"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,crops,share) %>% 
              spread(crops,share) %>% 
              select(-ARA))

# Grassland
dat=dat %>% 
  full_join(raw %>%
              filter(str_detect(crops, "^J")) %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[crops=="J0000"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,crops,share) %>% 
              spread(crops,share) %>% 
              select(-J0000))

# Permanent crops
dat=dat %>% 
  full_join(raw %>%
              filter(str_detect(crops, "^PECR|^F|^T|^W|^O")) %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[crops=="PECR"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,crops,share) %>% 
              spread(crops,share))

### Farm structure 
# Livestock in percent of total area
raw=get_eurostat("ef_lsk_main",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                farmtype="TOTAL",
                                lsu="TOTAL",
                                unit="LSU",
                                so_eur="TOTAL")) %>% 
  select(-agrarea,-farmtype,-lsu,-unit,-so_eur) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  left_join(area %>%
              filter(class=="UAA")) %>%
  mutate(values=values/area) %>%
  data.table() %>% 
  dasMittel(groups=c("animals"))
dat=dat %>% 
  full_join(raw %>% 
              spread(animals,values) %>% 
              select(-starts_with("A6"),-A0010)
  )

### Farm structure 
# Irrigation
raw=get_eurostat("ef_mp_irri",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                so_eur="TOTAL",
                                unit="HA")) %>% 
  select(-agrarea,-unit,-so_eur) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  left_join(area %>%
              filter(class=="UAA")) %>%
  mutate(values=values/area*100) %>%
  data.table() %>% 
  dasMittel(groups=c("irr_area"))

# Irrigation in percent of total area
dat=dat %>% 
  full_join(raw %>%
              spread(irr_area,values))

### Farm structure - Arable land by crop rotation
raw=get_eurostat("ef_mp_soil",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                farmtype="TOTAL",
                                soil_cov="ARA",
                                unit="HA"
                 )) %>%
  select(-agrarea,-unit,-farmtype) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("crop_rot"))
dat=dat %>%
  full_join(raw %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[crop_rot=="TOTAL"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,crop_rot,share) %>% 
              spread(crop_rot,share) %>% 
              select(-TOTAL) %>% 
              as_tibble(.name_repair = "universal") %>% 
              select(Point_ID,
                     PC0,PC1.24,PC25.49,PC50.74,PC_GE75))

### Farm structure 
# Arable land by soil cover
raw=get_eurostat("ef_mp_soil",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                farmtype="TOTAL",
                                crop_rot="TOTAL",
                                unit="HA"
                 )) %>%
  select(-agrarea,-unit,-farmtype) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("soil_cov"))
dat=dat %>%
  full_join(raw %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[soil_cov=="ARA"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,soil_cov,share) %>% 
              spread(soil_cov,share) %>% 
              select(-ARA))


### Farm structure 
# Arable land by tillage
raw=get_eurostat("ef_mp_prac",
                 time_format="num",
                 filters = list(agrarea="TOTAL",
                                crop_rot="TOTAL",
                                farmtype="TOTAL",
                                unit="HA"
                 )) %>%
  select(-agrarea,-unit,-crop_rot,-farmtype) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("tillage"))
dat=dat %>%
  full_join(raw %>% 
              group_by(Point_ID) %>% 
              mutate(total=values[tillage=="ARA"],
                     share=values/total*100) %>% 
              ungroup() %>% 
              select(Point_ID,tillage,share) %>% 
              spread(tillage,share) %>% 
              select(-ARA))

### Agricultural production 
# Exclude because mean NUTS level 0
raw=get_eurostat("apro_cpshr",
                 time_format="num") %>% 
  filter(strucpro=="YI_HU_EU") %>% 
  select(-strucpro) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("crops"))

# Yield per hectare
# Focus on those crops with > 90% non-NA values
# temp=raw %>%
#   mutate(values=ifelse(values==0,NA,values)) %>%
#   group_by(crops) %>%
#   summarise(n=sum(!is.na(values))/length(values)) %>%
#   filter(n>.9) %>%
#   pull(crops)
# 
# dat=dat %>%
#   left_join(raw %>%
#               filter(crops%in%temp) %>% 
#               mutate(values=ifelse(values==0,NA,values)) %>% 
#               mutate(crops=paste0(crops,"yield")) %>%
#               spread(crops,values))

### Agriculture and environment
# Gross nutrient balance
# Exclude because mean NUTS level 0
raw=get_eurostat("aei_pr_gnb",
                 time_format="num",
                 filters = list(indic_ag=c("BAL_UAA"))) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("nutrient"))

# dat=dat %>% 
#   full_join(raw %>%
#               mutate(nutrient=paste0(nutrient,"_balance")) %>% 
#               spread(nutrient,values))

### Agriculture and environment 
# Pesticides 
# Exclude because mean NUTS level 0
raw=get_eurostat("aei_fm_salpest09",
                 time_format="num",
                 filters = list(pesticid=c("F","H","I","M","PGR","ZR03"))) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  left_join(area %>%
              filter(class=="UAA")) %>%
  mutate(values=values/area) %>%
  data.table() %>% 
  dasMittel(groups=c("pesticid"))

# dat=dat %>% 
#   full_join(raw %>%
#               spread(pesticid,values) %>% 
#               rename(Fungicide="F",
#                      Herbicide="H",
#                      Insecticide="I",
#                      Molluscicide="M",
#                      PlantGrowthRegulator="PGR",
#                      Sterilant="ZR03"))

### Agriculture and environment 
# Inorganic fertilizer consumption (kg) per ha UAA
raw=get_eurostat("aei_fm_usefert",
                 time_format="num") %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  left_join(area %>%
              filter(class=="UAA")) %>%
  mutate(values=1000*values/area) %>%
  data.table() %>% 
  dasMittel(groups=c("nutrient"))

dat=dat %>% 
  full_join(raw %>%
              spread(nutrient,values) %>% 
              rename(Nfert="N",
                     Pfert="P"))

### Agriculture and environment
# Sale of manufactured fertilizer (kg) per ha UAA
# (exclude because mean NUTS level: 0)
raw=get_eurostat("aei_fm_manfert",
                 time_format="num") %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  left_join(area %>%
              filter(class=="UAA")) %>%
  mutate(values=1000*values/area) %>%
  data.table() %>% 
  dasMittel(groups=c("nutrient"))

# dat=dat %>% 
#   full_join(raw %>%
#               filter(nutrient%in%c("N","P","K")) %>% 
#               spread(nutrient,values) %>% 
#               rename(NfertSale="N",
#                      PfertSale="P",
#                      KfertSale="K"))

### Agriculture and environment
# High, low or medium input farms in percent of total UAA
raw=get_eurostat("aei_ps_inp",
                 time_format="num",
                 filters=list(unit="PC_AREA")) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  data.table() %>% 
  dasMittel(groups=c("indic_ag"))

dat=dat %>% 
  full_join(raw %>%
              spread(indic_ag,values))

### Agriculture and environment
# Number of manure storage facilities per km2
raw=get_eurostat("aei_fm_ms",
                 time_format="num",
                 filters=list(unit="NR")) %>% 
  inner_join(key,
             by=c("geo"="NUTS_ID")) %>% 
  mutate(values=values/as.numeric(nutArea)) %>%
  data.table() %>% 
  dasMittel(groups=c("indic_ag"))

dat=dat %>% 
  full_join(raw %>%
              spread(indic_ag,values))

# Export eurostat
saveRDS(as_tibble(dat),
        here("data","derived",
             "eurostat.rds"))

#### NDVI covariates ####
# Source: https://maps.opendatascience.eu
# https://data.opendatascience.eu/geonetwork/srv/eng/catalog.search#/metadata/b69476b8-4d6c-4381-a719-649574cb917e
# Data author: Tomislav Hengl

# List of available layers:
# https://gitlab.com/geoharmonizer_inea/eumap/-/blob/master/gh_raster_layers.csv
# file=read.csv(here("data",
#                    "raw",
#                    "opendatascience",
#                    "gh_raster_layers.csv")) %>% 
#   filter(str_detect(folder.name,"ndvi")) %>% 
#   mutate(folder.name=str_replace(folder.name,";","/"),
#          folder.name=paste0("/vsicurl/http://s3.eu-central-1.wasabisys.com/",
#                             folder.name)) %>% 
#   pull(folder.name)
# 
# library(terra)
# lucasEPSG3035=lucas %>% 
#   st_transform(crs(rast(file[1]),proj4=T))
# 
# if (.Platform$OS.type == "unix") {
#   library(doMC)
#   registerDoMC(detectCores()-1)
# } else {
#   cl = makePSOCKcluster(detectCores()-1)
#   registerDoParallel(cl)
# }
# tic()
# ndvi=foreach(i=file,
#              .combine = 'cbind',
#              .packages = c("terra","sf")) %dopar% {
#                terra::extract(rast(i),
#                               st_coordinates(lucasEPSG3035))
#              }
# toc() # 111min on server
# try(registerDoSEQ(),silent=T)
# try(stopCluster(cl),silent=T)
# detach("package:terra", unload=T)
# 
# ndvi=ndvi %>% 
#   bind_cols(Point_ID=lucasEPSG3035$Point_ID) %>% 
#   gather(key,ndvi,1:ncol(ndvi)) %>% 
#   mutate(key=str_replace(key,"lcv_ndvi_landsat.glad.ard_p50_30m_0..0cm_",""),
#          key=str_replace(key,"_eumap_epsg3035_v1.0",""),
#          year=as.numeric(str_sub(key,1,4)),
#          month=as.numeric(str_sub(key,5,6)),
#          # NDVI conversion based on 
#          # https://gitlab.com/geoharmonizer_inea/spatial-layers/-/issues/2
#          ndvi=(ndvi-100)/100) %>% 
#   dplyr::select(Point_ID,
#                 year,month,ndvi) 
# saveRDS(ndvi,here("data","derived","ndvi.rds"))
ndvi=readRDS(here("data","derived","ndvi.rds")) %>% 
  group_by(Point_ID,month) %>% 
  summarise(q5=median(ndvi)) %>% 
  bind_rows(readRDS(here("data","derived","ndvi.rds")) %>% 
              mutate(month=17) %>% 
              group_by(Point_ID,month) %>% 
              summarise(q5=median(ndvi))) %>% 
  mutate(month=case_when(month==3~"ndvi13",
                         month==6~"ndvi14",
                         month==9~"ndvi15",
                         month==12~"ndvi16",
                         month==17~"ndvi17",
                         T~NA_character_)) %>% 
  spread(month,q5)

## Calculate NDVIs of nearest grassland/cropland neighbors
temp=readRDS(here("data","derived",
                       "smallData.rds")) %>% 
  select(Point_ID,lon,lat,
         LC0_Desc) %>% 
  left_join(ndvi) %>% 
  filter(LC0_Desc%in%c("Cropland","Grassland")) %>% 
  st_as_sf(coords=c("lon","lat"),
           crs=st_crs(lucas))

## Distance matrix
# dist=st_distance(temp)
# colnames(dist)=temp$Point_ID
# dist=data.table(dist)
# dist=cbind(dist,
#             Point_ID=temp$Point_ID,
#             LC0_Desc=temp$LC0_Desc)
# dist=melt(dist, 
#            id.vars = c("Point_ID","LC0_Desc"),
#            measure.vars = 1:length(temp$Point_ID),
#            value.name = "dist")
# dist=dist%>% 
#   left_join(temp %>% 
#               st_drop_geometry() %>% 
#               select(Point_ID,LC0_Desc,starts_with("ndvi")) %>% 
#               rename_at(vars(contains("ndvi")),
#                         ~paste0("partner_",.)) %>% 
#               rename(variable=Point_ID,
#                      partnerUse=LC0_Desc) %>% 
#               mutate(variable=as.character(variable)))  %>% 
#   filter(LC0_Desc!=partnerUse) %>%
#   group_by(Point_ID,LC0_Desc,partnerUse) %>%
#   arrange(dist) %>%
#   slice(1:100) 
# saveRDS(dist,
#         here("data","derived","dist.rds"))
partner=readRDS(here("data","derived","dist.rds")) %>% 
  group_by(Point_ID,LC0_Desc,partnerUse) %>%
  arrange(dist) %>%
  # Median of five nearest neighbors
  slice(1:5) %>%
  group_by(Point_ID) %>% 
  summarise(partner_ndvi13=median(partner_ndvi13),
            partner_ndvi14=median(partner_ndvi14),
            partner_ndvi15=median(partner_ndvi15),
            partner_ndvi16=median(partner_ndvi16),
            partner_ndvi17=median(partner_ndvi17),
            median_dist=median(dist)
            ) %>% 
  as_tibble() 

ndvi=temp %>% 
  left_join(partner) %>% 
  mutate(partner_ndvi13diff=(ndvi13+1)-(partner_ndvi13+1),
         partner_ndvi14diff=(ndvi14+1)-(partner_ndvi14+1),
         partner_ndvi15diff=(ndvi15+1)-(partner_ndvi15+1),
         partner_ndvi16diff=(ndvi16+1)-(partner_ndvi16+1),
         partner_ndvi17diff=(ndvi17+1)-(partner_ndvi17+1))

# Median distance of 5 nearest grasslands to cropland
# 33 km
ndvi %>% 
  filter(LC0_Desc=="Cropland") %>% 
  st_drop_geometry() %>% 
  summarise(median=median(median_dist))


#### Variable selection ####
# First: Export bigData
readRDS(here("data","derived",
             "smallData.rds")) %>%
  left_join(readRDS(here("data","derived",
                         "eurostat.rds")) %>%
              as_tibble(.name_repair="universal")) %>%
  left_join(ndvi %>% select(-LC0_Desc,-median_dist) %>% st_drop_geometry()) %>%
  saveRDS(here("data","derived",
               "bigData.rds"))

# Continue...
bigData=readRDS(here("data","derived",
                     "bigData.rds"))

### Drop variables with missing values in > 1% of cases
overview=mlr::summarizeColumns(bigData) %>% 
  mutate(dropNA=ifelse(na>0.01*nrow(bigData),1,0))

### Adress collinearity
## Aim: Any correlation lower than r=0.8 (Spearman)
# # 1) Bioclimatic WorldClim variables
# # --> Conduct PCA and only chose one variable representing each component
# temp = bigData %>%
#   select(starts_with("BIO")) %>%
#   # select(contains("ndvi")) %>%
#   select_if(is.numeric) %>%
#   drop_na()
# pca = prcomp(temp,
#              center=T,
#              scale.=T)
# # 4 PCs with Eigenvalue > 1:
# screeplot(pca, type = "l", npcs = 10)
# abline(h = 1, col="red", lty=5)
# # These PCs explain 91% of the variance:
# summary(pca)
# # Varimax rotation
# # https://stats.stackexchange.com/a/137003
# ncomp=4
# rawLoadings     =pca$rotation[,1:ncomp] %*% diag(pca$sdev, ncomp, ncomp)
# rotatedLoadings =varimax(rawLoadings)$loadings
# # Convert loadings object to data frame and plot results
# rotatedLoadings.df=data.frame(matrix(as.numeric(rotatedLoadings), attributes(rotatedLoadings)$dim, dimnames=attributes(rotatedLoadings)$dimnames)) %>%
#   rownames_to_column() %>%
#   mutate(across(where(is.numeric),~ifelse(abs(.x)<.65,NA,.x))) %>%
#   rowwise() %>%
#   mutate(temp=sum(X1,X2,X3,X4,na.rm=T))
# temp = rotatedLoadings.df %>%
#   gather(PCA,value,X1:X4)
# pos=position_jitter(width = 0.2, seed = 2)
# ggplot()+
#   geom_jitter(data=temp,
#               mapping=aes(PCA,value),
#               position=pos,col="red")+
#   geom_jitter(data=temp,
#               mapping=aes(PCA,value),
#               position=pos,shape=1)+
#   ggrepel::geom_text_repel(
#     data = subset(temp,abs(value) > .6),
#     mapping = aes(PCA,value,label=rowname),
#     size          = 3,
#     segment.color = "grey50",
#     position=pos)+
#   ylim(-1.1,1.1)+
#   labs(y="Rotated loadings")+
#   theme_bw()
# # Continue with 
# # BIO6 (Min Temperature of Coldest Month)
# # BIO5 (Max Temperature of Warmest Month)
# # BIO13 (Precipitation of Wettest Month)
# # BIO14 (Precipitation of Driest Month)
overview=overview %>% 
  mutate(dropClim=ifelse(str_detect(name,"BIO")&!(name%in%c("BIO6","BIO5","BIO13","BIO14")),1,0))

# 2) Manual checks among other variables
overview=overview %>% 
  mutate(dropManual=ifelse(name%in%c("P1000","F1000","F2000","FT48_SO","A2000",
                                     "I07A1_EQ_Y","I07A2_EQ_YC","I07A31_EQ_Y",
                                     "ARArelUAA","N0000T","R1000",
                                     "V0000_S0000T","V0000_S0000TO",
                                     "J2000","F1000_2000","O1100","O1910","RENT","UAAIB",
                                     "CLC2_321","CLC2_3X331_332_335","CLC23_321",
                                     "Nstock"),
                           1,0))

### Overview
overview %>% 
  left_join(read_excel(here("vars.xlsx")) %>% 
              select(name,nameFull,unit,starts_with("class"),source,
                     modelSOC,modelEffect)) %>% 
  rowwise(name) %>% 
  mutate(drop=max(c_across(starts_with("drop")))) %>% 
  ungroup() %>% 
  mutate(modelEffect=case_when(modelSOC==1~1,
                              class0=="0info"~0,
                              drop==1~0,
                              T~modelEffect)) %>%
  select(name,
         type,na,mean,min,max,nlevs,
         nameFull,unit,starts_with("class"),source,
         modelSOC,modelEffect) %>%
  arrange(class0,class1,class2) %>% 
  writexl::write_xlsx(here("vars.xlsx"))

### Delete unrealistic values
bigData$I1130[bigData$I1130==850]=NA
bigData$Silt[!is.na(bigData$Silt)&bigData$Silt==-44]=NA

### Identify remaining collinearity issues
cor = cor(bigData %>%
            select(read_excel(here("vars.xlsx")) %>% 
                                filter(class0!="0info",
                                       modelEffect==1) %>% 
                     pull(name)) %>% 
            select_if(is.numeric),
          method="spearman",
          use="pairwise.complete.obs")
cor[lower.tri(cor,diag=T)] = NA
as.data.frame(as.table(cor))  %>%
  drop_na() %>%
  mutate(across(is.numeric,round,2)) %>%
  filter(Freq>.8|Freq<(-.8))

goodData=bigData %>% 
  select(read_excel(here("vars.xlsx")) %>% 
           filter(modelEffect==1) %>% 
           pull(name),
         "OCstock") %>% 
  drop_na()
# Export
saveRDS(goodData,
        here("data","derived",
             "goodData.rds"))

