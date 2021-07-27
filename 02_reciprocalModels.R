# Basics
library(tidyverse)
library(here)
library(patchwork)
library(ranger)
library(boot)
# Geodata
library(sf)
library(ggsn)
# Speed
library(foreach)
library(doParallel)
library(tictoc)

cores = detectCores() - 1
if(cores == 0) {cores = 1}

source(here("functions.R"))

## Settings
target="OCstock"        # OC OCstock
errorType="Abs"         # Abs Rel
direction="cropToGrass" # grassToCrop or cropToGrass

### Load data #############################################################################################
goodData = readRDS(here("data","derived",
                       "goodData.rds")) %>% 
  as.data.frame()
goodData$obs=goodData[,target]

vars=readxl::read_excel(here("vars.xlsx")) %>%
  filter(modelSOC==1|modelEffect==1) 

europe=st_as_sf(rnaturalearth::countries110) %>%
  filter(region_un=="Europe"&!(name%in%c('Russia',"Ukraine","Belarus","Moldova"))) %>%
  st_crop(xmin=-10,
          xmax=45,
          ymin=30,
          ymax=73)


### Reciprocal Model ######################################################################################
if(direction=="grassToCrop"){
  ref="Cropland"
}
if(direction=="cropToGrass"){
  ref="Grassland"
}

train0=goodData %>%
  filter(LC0_Desc==ref) %>%
  select(one_of(vars %>%
                  filter(class0!="0info",
                         modelSOC==1) %>% 
                  pull(name)),
         obs) %>%
  drop_na() %>% 
  distinct()
test0=goodData %>%
  filter(LC0_Desc%in%c("Cropland","Grassland")) %>%
  mutate(split=ifelse(LC0_Desc==ref,"ref","treat")) %>%
  select(one_of(vars %>%
                  filter(class0!="0info",
                         modelEffect==1) %>% 
                  pull(name)),
         -OCstock,split,Point_ID,obs,lon,lat) %>%
  drop_na() %>% 
  distinct()

# Step 1: Train on grassland to predict SOC ###################################################################
set.seed(123)
model0=ranger(obs~.,
              data=train0,
              num.trees=1000,
              mtry=floor(sqrt(ncol(train0))),
              importance="permutation")
print(paste0("Train in ",ref," to predict SOC in ",c("Cropland","Grassland")[!(c("Cropland","Grassland")%in%ref)],
             ". Model SOC. R2=",round(model0$r.squared,2)))

# # Compare R2(oob) with R2(5-fold random CV)
# set.seed(123)
# indices = caret::createFolds(1:nrow(train0),
#                               k=5)
# 
# if (.Platform$OS.type == "unix") {
#   library(doMC)
#   registerDoMC(cores)
# } else {
#   cl = makePSOCKcluster(cores)
#   registerDoParallel(cl)
# }
# 
# tic()
# cv = foreach(i=1:length(indices),
#              .combine = 'rbind',
#              .packages=c("ranger","dplyr")) %dopar% {
# 
#                cal=train0[-indices[[i]],]
#                val=train0[ indices[[i]],]
# 
#                model=ranger(obs~.,
#                             data=cal,
#                             num.trees=1000,
#                             mtry=floor(sqrt(ncol(cal))))
# 
#                val %>%
#                  select(obs) %>%
#                  mutate(fold=i,
#                         pred=predict(model,
#                                      data=val)$predictions)
# 
#              }
# 
# try(registerDoSEQ(),silent=T)
# try(stopCluster(cl),silent=T)
# toc()
# 
# cv %>%
#   summarise(RMSE=sqrt(mean((pred - obs)^2)),
#             R2=1 - sum((pred - obs)^2) / sum((obs - mean(obs))^2),
#             bias=mean(obs)- mean(pred))

# Export files for further analyses
saveRDS(train0,here("data","derived",paste0(direction,"_train0.rds")))
saveRDS(model0,here("data","derived",paste0(direction,"_model0.rds")))

# Step 2: Drop cropland outside the models' space of applicability #####################
### Restrict to area of applicability
# Dummy coding
datAOA=test0 %>%
  
  # Select variables used for model training
  select(names(train0),
         Point_ID,
         -obs) %>%
  
  # Dummy code character / factor variables
  mutate(across(where(is.character), function(x) fct_lump_prop(as.factor(x),.05))) %>% 
  drop_na() %>% 
  fastDummies::dummy_cols(remove_selected_columns=T) %>%
  as_tibble(.name_repair="universal") %>%  
  
  # Restrict to testing data + add splitting variable
  left_join(test0 %>% 
              select(Point_ID,split)) %>% 
  distinct()

# Weights
weight=tibble(name=names(datAOA)) %>% 
  left_join(enframe(model0$variable.importance)) %>% 
  filter(!is.na(value)) %>% 
  
  # For dummy coded variable, assign VI value of grouping variable for all
  bind_rows(tibble(name=names(datAOA)) %>% 
              left_join(enframe(model0$variable.importance)) %>% 
              filter(is.na(value)) %>% 
              mutate(name1=sub('_[^_]*$', '', name)) %>% 
              select(-value) %>% 
              left_join(enframe(model0$variable.importance),by=c("name1"="name"))) %>% 
  filter(!(name%in%c("Point_ID","split")))

aoa=datAOA %>% 
  filter(split=="treat") %>% 
  select(Point_ID) %>% 
  bind_cols(aoa1(newData = datAOA %>% 
                   filter(split=="treat"),
                 train = datAOA %>% 
                   filter(split=="ref"),
                 weight = weight,
                 variables = weight$name,
                 threshold = 0.95))

## Interpret space of applicability results
# Summary
aoa %>%
  group_by(AOA) %>%
  tally() %>%
  mutate(rel=n/nrow(aoa))

# Plot
imp=enframe(model0$variable.importance) %>%
  arrange(-value) %>% 
  # top_n(5) %>% 
  left_join(vars) %>% 
  filter(type=="numeric")
label=paste0(imp$nameFull," (",imp$unit,")")
label=str_replace(label,"(NA)","-")
label=str_replace(label,"\\\\","")
names(label)=imp$name
plot=aoa %>%
  left_join(test0 %>%
              select(Point_ID,
                     one_of(names(train0)))) %>%
  select_at(vars("Point_ID","AOA",imp$name)) %>% 
  mutate(AOA=ifelse(AOA==0,"no","yes"),
         AOA=fct_relevel(AOA,
                         "yes","no")) %>%
  gather(key,value,3:(nrow(imp)+2)) %>% 
  mutate(key=fct_relevel(as.factor(key),
                         imp %>% 
                           pull(name))) %>%
  ggplot(aes(value,AOA,fill=AOA))+
  ggridges::geom_density_ridges(quantile_lines = T, 
                                quantiles = 2,
                                alpha = 0.7,
                                size=.1)+
  scale_fill_manual(values=c(colorspace::diverge_hcl(6,"Green-Brown",rev=T)[5],"grey90"))+
  facet_wrap("key",
             nrow=3,
             scales="free_x",
             strip.position="bottom",
             labeller=labeller(key=label))+
  labs(y="Space of applicability")+
  ggthemes::theme_tufte(base_family="",
                        base_size=7)+
  scale_y_discrete(expand=c(0.1,0.1))+
  theme(legend.position = "none",
        axis.ticks=element_line(colour="black",size=.2),
        axis.text = element_text(colour="black"),
        axis.title.x=element_blank(),
        strip.placement = "outside")

# cairo_pdf(filename = here("figures",
#                           paste0(direction,"_Fig3_AOAbyPredictor.pdf")),
#           width=170*0.0393701,
#           height=60*0.0393701) # conversion 'in' to 'mm'
# print(plot)
# dev.off()

# Step 3: Apply grassland model on cropland and calculate effect size ####################################################################
pred=test0  %>% 
  mutate(pred=predict(model0,
                      test0)$predictions
         ) %>% 
  mutate(effect=case_when(errorType=="Rel"~log(pred/obs),
                          errorType=="Abs"~pred-obs,
                          T~NA_real_)) %>%
  select(Point_ID,lon,lat,
         split,
         obs,pred,effect) %>% 
  left_join(aoa)
# Export for postHoc
saveRDS(pred,here("data","derived",paste0(direction,"_pred.rds")))


### Average effect size
# # All
# sub=pred %>% 
#   filter(is.na(AOA)|AOA==1,
#          split=="treat") %>% 
#   left_join(readRDS(here("data","derived",
#                          "rawData.rds")))
# stat=function(data, index) {
#   temp=sub %>% 
#     slice(index) 
#   mean(temp$effect)
# }
# set.seed(123)
# broom::tidy(boot(sub, stat, 10000),
#             conf.int=T,
#             conf.level=.95,
#             conf.method="bca")
# 
# # By country
# if (.Platform$OS.type == "unix") {
#   library(doMC)
#   registerDoMC(cores)
# } else {
#   cl = makePSOCKcluster(cores)
#   registerDoParallel(cl)
# }
# 
# tic()
# temp = foreach(i=c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR",
#                    "HR","HU","IE","IT","LT","LU","LV","NL","PL","PT","RO","SE",
#                    "SI","SK","UK"),
#                .combine = 'rbind',
#                .packages=c("here","broom","boot","dplyr")) %dopar% {
# 
#                  sub=pred %>%
#                    filter(is.na(AOA)|AOA==1,
#                           split=="treat") %>%
#                    left_join(readRDS(here("data","derived",
#                                           "rawData.rds"))) %>%
#                    filter(NUTS_0==i)
# 
#                  stat=function(data, index) {
#                    temp=sub  %>%
#                      slice(index)
#                    mean(temp$effect)
#                  }
#                  set.seed(123)
#                  broom::tidy(boot(sub, stat, 10000),
#                              conf.int=T,
#                              conf.level=.95,
#                              conf.method="bca") %>%
#                    mutate(NUTS_0=i)
#                }
# try(registerDoSEQ(),silent=T)
# try(stopCluster(cl),silent=T)
# toc()


# # Plot residuals
# ggplot(pred %>%
#          filter(AOA==1),
#        aes(obs,pred))+
#   geom_point(alpha=.1)+
#   geom_abline(slope=1)+
#   ggthemes::theme_tufte(base_family="")
  
### Plot map
temp=pred %>%
  filter(split=="treat") %>%
  sf::st_as_sf(coords=c("lon","lat"),
               crs=4326)
pal=colorspace::diverge_hcl(6,"Green-Brown",rev=T)[-2]
plot=ggplot() + 
  geom_sf(data = europe, 
          fill = "white",
          size = .25, 
          color="black") +
  geom_sf(data = temp%>%
            filter(AOA==1) %>% 
            mutate(#effect=Hmisc::cut2(effect,cuts=c(-5,5,15,25)),
                   effect=case_when(effect<(-5)~"Less than -5",
                                    effect<  5 ~"-5 to  5",
                                    effect< 15 ~"5 to 15",
                                    effect< 25 ~"15 to 25",
                                    T          ~"More than 25"),
                   effect=fct_relevel(effect,
                                      "Less than -5",
                                      "-5 to  5",
                                      "5 to 15",
                                      "15 to 25",
                                      "More than 25")),
          aes(color = effect),
          shape=1,
          alpha=.5,
          stroke=.4,
          size = 2.5)+
  geom_sf(data=temp %>%
            filter(AOA==0),
          col="black",
          shape=3,
          size=.5)+
  coord_sf(xlim = c(-8.1,30),
           ylim = c(36,65.9))+
  # north(temp,
  #       symbol=3,
  #       location="topleft") +
  # scalebar(temp,
  #          location="topleft",
  #          dist = 500,
  #          transform=T,
  #          dist_unit = "km",
  #          model = "WGS84",
  #          st.size=3,
  #          border.size=.25,
  #          height=.01) +
  scale_color_manual(expression(Delta*"SOC"~"(Mg ha"^{-1}*')'),
                     values=pal) +
  theme_void(base_size = 12)+
  theme(# Legend
        legend.position="right",
        legend.justification="bottom",
        # legend.margin = margin(0,2,0,-10,"mm"),
        legend.key.height=unit(5,"mm"),
        legend.key.width=unit(0,"mm"),
        legend.spacing.x=unit(2,"mm"),
        legend.spacing.y=unit(.2,"mm")) +
  guides(col=guide_legend(override.aes = list(size=2.5,alpha=1,stroke=1),
                          label.position = "left"))
# ggsave(plot=plot,
#        file=here("figures",
#                  paste0(direction,
#                         "_SOCmap.png")),
#        width=12,height=10,
#        dpi=600,
#        units = "cm")
# cairo_pdf(filename = here("figures",
#                           paste0(direction,
#                                  "_Fig4_SOCmap.pdf")),
#           width=120*0.0393701,
#           height=100*0.0393701) # conversion of in to mm
# print(plot) 
# dev.off() 


# Step 4: Explain effect sizes ###################################################################
train1=pred %>% 
  filter(AOA==1) %>% 
  select(Point_ID,effect) %>% 
  left_join(test0) %>% 
  select(-split,-lon,-lat, -obs, -Point_ID) %>%
  as.data.frame()

set.seed(123)
model1=ranger(effect~.,
              data=train1,
              num.trees=1000,
              mtry=floor(sqrt(ncol(train1)-1)),
              importance="permutation")
print(paste0("Train in ",c("Cropland","Grassland")[!(c("Cropland","Grassland")%in%ref)]," to predict effect size in ",ref,
             ". R2=",round(model1$r.squared,2)))

# # Compare R2(oob) with R2(5-fold random CV)
# set.seed(123)
# indices = caret::createFolds(1:nrow(train1),
#                               k=5)
# 
# if (.Platform$OS.type == "unix") {
#   library(doMC)
#   registerDoMC(cores)
# } else {
#   cl = makePSOCKcluster(cores)
#   registerDoParallel(cl)
# }
# 
# tic()
# cv = foreach(i=1:length(indices),
#              .combine = 'rbind',
#              .packages=c("ranger","dplyr")) %dopar% {
# 
#                cal=train1[-indices[[i]],]
#                val=train1[ indices[[i]],]
# 
#                model1=ranger(effect~.,
#                             data=cal,
#                             num.trees=1000,
#                             mtry=floor(sqrt(ncol(cal))))
# 
#                val %>%
#                  select(effect) %>%
#                  mutate(fold=i,
#                         pred=predict(model1,
#                                      data=val)$predictions)
# 
#              }
# 
# try(registerDoSEQ(),silent=T)
# try(stopCluster(cl),silent=T)
# toc()
# 
# cv %>%
#   summarise(RMSE=sqrt(mean((pred - effect)^2)),
#             R2=1 - sum((pred - effect)^2) / sum((effect - mean(effect))^2),
#             bias=mean(effect)- mean(pred)) # R2=0.43 (similar as OOB error)

saveRDS(train1,here("data","derived",paste0(direction,"_train1.rds")))
saveRDS(model1,here("data","derived",paste0(direction,"_model1.rds")))

### Add On #############
### Delta NDVI plot
temp=pred %>% 
  filter(AOA==1) %>% 
  select(Point_ID,effect) %>% 
  left_join(test0) %>%
  sf::st_as_sf(coords=c("lon","lat"),
               crs=4326)

p1=ggplot() + 
  geom_sf(data = europe, 
          fill = "white",
          size = .25, 
          color="black") +
  geom_sf(data = temp,
          aes(color = partner_ndvi17diff),
          shape=1,
          alpha=.5,
          stroke=.4,
          size = 2.5)+
  coord_sf(xlim = c(-8.1,30),
           ylim = c(36,65.9))+
  colorspace::scale_color_continuous_diverging("Green-Brown",
                                               rev=T)+
  labs(color=expression(Delta*"NDVI"~'(-)'))+
  theme_void(base_size = 10)+
  theme(# Legend
    legend.position="right",
    legend.justification="bottom",
    legend.margin = margin(0,2,2,-5,"mm")
    # legend.key.height=unit(5,"mm"),
    # legend.key.width=unit(0,"mm"),
    # legend.spacing.x=unit(2,"mm"),
    # legend.spacing.y=unit(.2,"mm")
    ) +
  guides(color = guide_colourbar(label.position = "left",
                                 frame.colour="black",
                                 ticks.colour = "black",
                                 frame.linewidth=.75,
                                 ticks.linewidth=.75,
                                 barwidth=.5))

p2=ggplot(train1 %>% 
         mutate(effect1=case_when(effect<(-5)~"Less than -5",
                                  effect<  5 ~"-5 to  5",
                                  effect< 15 ~"5 to 15",
                                  effect< 25 ~"15 to 25",
                                  T          ~"More than 25"),
                effect1=fct_relevel(effect1,
                                    "Less than -5",
                                    "-5 to  5",
                                    "5 to 15",
                                    "15 to 25",
                                    "More than 25")),
       aes(effect1,partner_ndvi17diff))+
  geom_violin(draw_quantiles = .5,
              scale="count")+
  labs(x=expression(Delta*"SOC"~"(Mg ha"^{-1}*')'),
       y=expression(Delta*"NDVI"~'(-)'))+
  scale_y_continuous(breaks=c(-.4,-.2,0,.2))+
  ggthemes::theme_tufte(base_family="",
                        base_size=10)+
  theme(legend.position = "none")


plot=p1+p2+plot_layout(design ="
AAA#
AAA#
AAAB
")
# cairo_pdf(filename = here("figures",
#                           paste0(direction,
#                                  "_Fig6_deltaNDVI.pdf")),
#           width=120*0.0393701,
#           height=100*0.0393701) # conversion of in to mm
# print(plot) # cairo_pdf only opens the graphics device, need to print the plot
# dev.off() # and need to close the device

### Some grassland/cropland neighbors...
mapview::mapview(temp,
                 zcol=c("partner_ndvi17diff","Point_ID"))

readRDS(here("data","derived","dist.rds")) %>% 
  filter(Point_ID==53244012) %>% 
  arrange(dist) %>%
  # Median of five nearest neighbors
  slice(1:5)

# Photos 
# Spanien
# https://gisco-services.ec.europa.eu/lucas/photos/2015/ES/343/818/34381810P.jpg
# https://gisco-services.ec.europa.eu/lucas/photos/2015/ES/344/419/34441974N.jpg
# Frankreich
# https://gisco-services.ec.europa.eu/lucas/photos/2015/FR/362/828/36282894W.jpg
# https://gisco-services.ec.europa.eu/lucas/photos/2015/FR/370/628/37062892E.jpg
# Finland
# https://gisco-services.ec.europa.eu/lucas/photos/2015/FI/513/442/51344234P.jpg
# https://gisco-services.ec.europa.eu/lucas/photos/2015/FI/515/442/51544238S.jpg
