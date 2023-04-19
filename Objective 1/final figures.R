# this plots the final figures for objective 1
library(tidyverse)
library(car)
library(emmeans)
library(ggpubr)
library(cowplot)

# load data; clean numeric outliers -------------------------------
{
  numtraits <- read.csv("data/processed/Cscores_num_traits.csv")
  factraits <-  read.csv("data/processed/Cscores_fac_traits.csv")
  
  # clean outliers with values > 3SD
  {
    head(numtraits)
    numtraits %>%
      select(-Region, -Cscore) %>% 
      distinct() %>%  # make sure only one record per sp
      gather(., "Trait", "TraitValue", 2:ncol(.)) %>% 
      filter(!is.na(TraitValue)) %>% 
      ggplot(., aes(x=TraitValue, fill=Trait)) +
      geom_histogram() +
      facet_wrap(~Trait, scales="free") +
      theme(legend.position="none")
    # looks like seed_dry_mass and veg_height might need some cleaning
    
    numtraits %>% 
      select(-Region, -Cscore) %>% 
      distinct() %>% # make sure we are only getting one set of traits per sp
      gather(., key="Trait", value="TraitValue", 2:ncol(.)) %>% 
      filter(!is.na(TraitValue)) %>% 
      filter(Trait=="Seed_dry_mass" | Trait=="Veg_height" | Trait == "LeafN" | Trait == "SLA_no_pet") %>% 
      ggplot(., aes(x=TraitValue, fill=Trait)) +
      geom_histogram(bins=100) +
      facet_wrap(~Trait, scales="free") +
      theme(legend.position="none")
    
    # remove seed_dry_mass outliers
    {
      # 1148 total observations
      numtraits %>% 
        filter(!is.na(Seed_dry_mass)) %>% 
        distinct(Species,Seed_dry_mass) %>% nrow()
      
      # these are sp with the largest seed_dry_mass values
      numtraits %>% 
        filter(!is.na(Seed_dry_mass)) %>% 
        distinct(Species,Seed_dry_mass) %>% 
        arrange(desc(Seed_dry_mass))  %>% 
        head(20) 
      
      # exclude values for 20 sp w/ seed dry mass values > 3 x SD (i.e. 3 sigma)
      # these are the sp w/ seed mass values to be excluded
      
      numtraits %>% 
        filter(!is.na(Seed_dry_mass)) %>% 
        distinct(Species,Seed_dry_mass) %>% 
        filter(Seed_dry_mass > (mean(Seed_dry_mass) + (3*sd(Seed_dry_mass)))) %>%
        arrange(desc(Seed_dry_mass)) # 20 out of 1148 observations
      
      # convert outlier values to NA
      numtraits %>% 
        distinct(Species,Seed_dry_mass) %>% 
        filter(!is.na(Seed_dry_mass)) %>% 
        summarize(upper3sigma = mean(Seed_dry_mass) + (3*sd(Seed_dry_mass)),
                  lower3sigma = mean(Seed_dry_mass) - (3*sd(Seed_dry_mass)))
      
      numtraits <- numtraits %>% 
        mutate(Seed_dry_mass = ifelse(Seed_dry_mass > 
                                        mean(Seed_dry_mass, na.rm = T) + (3*sd(Seed_dry_mass, na.rm = T)), 
                                      NA, 
                                      Seed_dry_mass)) 
      
      }
    
    # exclude values for 10 sp with Veg_height > 3 x SD
    {
      # 1013 total observations
      numtraits %>% 
        filter(!is.na(Veg_height)) %>% 
        distinct(Species,Veg_height) %>% nrow()
      
      # these are the 10 sp w/ veg height values > 3 x SD (i.e. 2 sigma)
      numtraits %>% 
        distinct(Species,Veg_height) %>% 
        filter(!is.na(Veg_height)) %>% 
        filter(Veg_height > (mean(Veg_height) + (3*sd(Veg_height)))) %>% 
        arrange(desc(Veg_height)) 
      
      # convert outlier values to NA
      numtraits <- numtraits %>% 
        mutate(Veg_height = ifelse(Veg_height > (mean(Veg_height, na.rm = T) + 3*sd(Veg_height, na.rm=T)), 
                                   NA, 
                                   Veg_height))
      
    }
    
    # exclude values for 4 sp with LeafN > 3 x SD
    {
      # 483 total observations
      numtraits %>% 
        filter(!is.na(LeafN)) %>% 
        distinct(Species,LeafN) %>% nrow()
      
      # these are the 4 sp w/ veg height values > 3 x SD (i.e. 2 sigma)
      numtraits %>% 
        distinct(Species,LeafN) %>% 
        filter(!is.na(LeafN)) %>% 
        filter(LeafN > (mean(LeafN) + (3*sd(LeafN)))) %>% 
        arrange(desc(LeafN)) 
      
      # convert outlier values to NA
      numtraits <- numtraits %>% 
        mutate(LeafN = ifelse(LeafN > (mean(LeafN, na.rm = T) + 3*sd(LeafN, na.rm=T)), 
                              NA, 
                              LeafN))
      
    }
    
    # exclude values for 3 sp with SLA_no_pet > 3 x SD
    {
      # 378 total observations
      numtraits %>% 
        filter(!is.na(SLA_no_pet)) %>% 
        distinct(Species,SLA_no_pet) %>% nrow()
      
      # these are the 3 sp w/ veg height values > 3 x SD (i.e. 2 sigma)
      numtraits %>% 
        distinct(Species,SLA_no_pet) %>% 
        filter(!is.na(SLA_no_pet)) %>% 
        filter(SLA_no_pet > (mean(SLA_no_pet) + (3*sd(SLA_no_pet)))) %>% 
        arrange(desc(SLA_no_pet)) 
      
      # convert outlier values to NA
      numtraits <- numtraits %>% 
        mutate(SLA_no_pet = ifelse(SLA_no_pet > (mean(SLA_no_pet, na.rm = T) + 3*sd(SLA_no_pet, na.rm=T)), 
                                   NA, 
                                   SLA_no_pet))
      
    }
    
  }
  
}

# exclude traits we don't use
{
  numtraits <- numtraits %>% select(-SLA_with_pet, 
                                    -Drought_tol_Mpa)
}


# set default plotting theme options
{
  mytheme <- theme_classic() + 
    theme(axis.text = element_text(color=1, size=6), 
          axis.title = element_text(color=1, size=8),
          legend.position = "top",
          legend.title=element_blank()) 
}

# C-score vs region ####
cscore_p <- ggplot(factraits, aes(x=Region, y=Cscore)) +
  stat_summary(fun.data="mean_se", geom="bar", fill="grey70") +
  stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
  labs(x="Region", y="C-score") +
  expand_limits(y=c(0,10)) +
  scale_y_continuous(breaks=seq(0, 10, by=2)) +
  scale_x_discrete(labels=c("Dakotas", "Illinois", "Kansas", "New York", "south Florida")) +
  annotate(geom="text", x=1:5, y=c(7,7.2,5.7,7.2,6.7), label=c("ab", 
                                                               "ac", 
                                                               "d", 
                                                               "c", 
                                                               "b"), size=3) +
  mytheme  
cscore_p
ggsave(plot=cscore_p,
       filename="figs/Cscore vs Region.jpg")

# fig 2 - multipanel ####
{
  # Germination rate ####
  germ_p <- ggplot(numtraits, aes(x=Germ_rate, y=Cscore)) +
    geom_jitter(col="grey70", alpha=0.5) +
    geom_smooth(method="lm", se=F, col=1) +
    annotate(geom="text", x=0,y=9, hjust=0, label="R^2==0.16", parse=TRUE, size=2.5) +
    labs(x="Germination rate (%)", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  germ_p
  # ggsave(plot=germ_p,
  #        filename="figs/Germ rate.jpg")
  
  # plant growth rate ####
  growthrate_p <- ggplot(filter(factraits, !is.na(Growth_rate)), aes(x=fct_relevel(Growth_rate,"slow", "moderate", "fast"), y=Cscore)) +
    stat_summary(fun.data="mean_se", geom="bar", fill="grey70") +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
    annotate(geom="text", x=0.5,y=9, hjust=0, label="R^2==0.10", parse=TRUE, size=2.5) +
    labs(x="Plant growth rate", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  growthrate_p
  # ggsave(plot=growthrate_p,
  #        filename="figs/Growth rate.jpg")
  
  #SLA ####
  sla_p <- ggplot(numtraits, aes(x=SLA_no_pet, y=Cscore)) +
    geom_jitter(col="grey70", alpha=0.5) +
    geom_smooth(method="lm", se=F, col=1) +
    annotate(geom="text", x=0,y=9, hjust=0, label="R^2==0.09", parse=TRUE, size=2.5) +
    labs(x="SLA (mm2/mg)", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  sla_p
  # ggsave(plot=sla_p,
  #        filename="figs/SLA.jpg")
  
  
  #leafN ####
  leafn_p <- ggplot(numtraits, aes(x=LeafN, y=Cscore)) +
    geom_jitter(col="grey70", alpha=0.5) +
    geom_smooth(method="lm", se=F, col=1) +
    annotate(geom="text", x=0,y=9, hjust=0, label="R^2==0.14", parse=TRUE, size=2.5) +
    labs(x="Leaf N (%)", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  leafn_p
  # ggsave(plot=leafn_p,
  #        filename="figs/LeafN.jpg")
  
  
  
  # propogation type ####
  proptype_p <- ggplot(filter(factraits, !is.na(Propagation_type)), 
                       aes(x=fct_recode(Propagation_type, "veg" = "vegetative", "seed + veg" = "seed and vegetative"), y=Cscore)) +
    stat_summary(fun.data="mean_se", geom="bar", fill="grey70") +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
    annotate(geom="text", x=0.5,y=9, hjust=0, label="R^2==0.21", parse=TRUE, size=2.5) +
    labs(x="Propagation Type", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  proptype_p 
  # ggsave(plot=proptype_p,
  #        filename="figs/Propagation type.jpg")
  
  # dispersal unit type ####
  disptype_p <- ggplot(filter(factraits, Dispersal_unit_type!="vegetative"), aes(x=Dispersal_unit_type, y=Cscore)) +
    stat_summary(fun.data="mean_se", geom="bar", fill="grey70") +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
    annotate(geom="text", x=0.5,y=9, hjust=0, label="R^2==0.20", parse=TRUE, size=2.5) +
    labs(x="Dispersal unit type", y="C-score") +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    mytheme
  disptype_p
  # ggsave(plot=disptype_p,
  #        filename="figs/Dispersal unit type.jpg")
  
  # combine into multi panel ####
  fig2 <- plot_grid(germ_p, growthrate_p, leafn_p, proptype_p, disptype_p,
                    ncol=2, nrow=3,
                    labels="auto")
  fig2
  ggsave(fig2,
         filename = "figs/Fig 2.jpg")
}

# fig s1 
{
  lifespan_p <- ggplot(filter(factraits, !is.na(Plant_lifespan_fac)), aes(x=Region, y=Cscore, fill=Plant_lifespan_fac)) +
    stat_summary(fun.data="mean_se", geom="bar", position = "dodge") +
    stat_summary(fun.data="mean_se", geom="errorbar", position = position_dodge(width=.9), width=0.5) +
    labs(x="Plant lifespan", y="C-score") +
    scale_fill_manual(values=c("grey60", "grey80")) +
    labs(x=NULL) +
    expand_limits(y=c(0,10)) +
    scale_y_continuous(breaks=seq(0, 10, by=2)) +
    scale_x_discrete(labels=c("Dakotas", "Illinois", "Kansas", "New York", "south Florida")) +
    mytheme
  
  lifespan_p
  ggsave(plot=lifespan_p,
         filename="figs/Lifespan.jpg")
}
