# Basics
library(tidyverse)
library(here)
library(ranger)
library(colorspace)
library(patchwork)
# Speed
library(foreach)
library(doParallel)
library(tictoc)

cores = detectCores() - 1
if(cores == 0) {cores = 1}

direction="cropToGrass"  # grassToCrop cropToGrass
what=1                   # 0=SOC (Fig.2); 1=Delta SOC (Fig.5)

train=readRDS(here("data","derived",
                   paste0(direction,"_train",what,".rds")))
train0=train
train1=train
model=readRDS(here("data","derived",
                   paste0(direction,"_model",what,".rds")))
vars=readxl::read_excel(here("vars.xlsx"))

### Partial dependence plots - 1D plots ##########
imp=enframe(model$variable.importance) %>%
  arrange(-value) %>% 
  top_n(7) %>% 
  left_join(vars) 
imp$nameFull[imp$name=="obs"]="Initial SOC"
imp$unit[imp$name=="obs"]="Mg ha-1"

ggplot(imp,
       aes(fct_reorder(nameFull,value),value))+
  geom_bar(stat="identity", 
           width=.4,
           size=.2,
           col="grey20") +
  coord_flip()+
  labs(y="Permutation importance") +
  theme_minimal(base_size=7)+
  theme(panel.border=element_rect(colour = "black",fill=NA),
        axis.title.y=element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x=element_line(size=.2))


tic()
pd = foreach(i=imp$name,
             .combine = 'rbind',
             .packages = c("pdp","dplyr")) %do% {
               
               pdp::partial(model,
                            pred.var=i,
                            parallel=F) %>% 
                 as_tibble() %>% 
                 rename(value=1) %>% 
                 mutate(name=i)
               
             }

toc() # ntree1000, n=7; 100s (single)

for(i in 1:length(imp$name)){
  titleX=paste0(paste(strwrap(imp$nameFull[i],25), collapse="\n"),
                "\n(",imp$unit[i],")")
  titleX=str_replace(titleX,"(NA)","-")
  titleX=str_replace(titleX,"\\\\","")
  
  plot=ggplot(pd %>% 
                filter(name==imp$name[i]),
              aes(value,yhat))+
    geom_line(size=.2)+
    coord_cartesian(ylim=range(pd$yhat)) +
    theme_bw(base_size=7)
  
  if(i==1){
    plot=plot+
      theme(panel.border=element_rect(colour = "black",fill=NA,size=.2),
            panel.grid = element_blank(),
            axis.ticks=element_line(colour="black",size=.2),
            axis.text = element_text(colour="black",size=7),
            plot.margin = unit(c(.2,.5,0,.5), "mm"))
  } else {
    plot=plot+
      theme(panel.border=element_rect(colour = "black",fill=NA,size=.2),
            panel.grid = element_blank(),
            axis.ticks=element_line(colour="black",size=.2),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x = element_text(colour="black",size=7),
            plot.margin = unit(c(.2,.5,0,.5), "mm"))
  }
  
  if(what==0){
    plot=plot+
      labs(x=titleX,
           y=expression("Predicted SOC"~"(Mg ha"^{-1}*')'))
  } else {
    plot=plot+
      labs(x=titleX,
           y=expression("Predicted"~Delta*"SOC"~"(Mg ha"^{-1}*')'))
  }
  
  marg=ggplot(train %>% 
                mutate(value=train[,imp$name[i]]),
              aes(value))+
    geom_density(size=.2,
                 fill="grey90")+
    theme_void()+
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
  
  plot=marg/plot+plot_layout(heights=c(.25,1))
  assign(imp$name[i],plot)
}

if(what==0){
  plot=BIO14|BIO5|CN|Coarse|Clay  # what==0
  name=paste0(direction,"_Fig2_pdp",what,".png")
}
if(what==1){
  plot=BIO14|Nfert|CN|ndvi17|partner_ndvi17diff # what==1
  name=paste0(direction,"_Fig5_pdp",what,".png")
}

# cairo_pdf(filename = here("figures",
#                           name),
#           width=150*0.0393701,
#           height=45*0.0393701) # conversion 'in' to 'mm'
# print(plot) 
# dev.off() 