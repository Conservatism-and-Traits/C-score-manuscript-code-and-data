# this file conducts final linear regressions to determine if the cscore ~ trait relationship varies by region
# these analyses are to meet objective 1

library(tidyverse)
library(car)
library(emmeans)
library(ggpubr)

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

# how many sp in total
{
  head(numtraits)
  numtraits %>%
    select(-Region, -Cscore) %>% 
    distinct() -> tmp1
  factraits %>% 
    select(-Region, -Cscore) %>% 
    distinct() -> tmp2
  
  # how many sp in total
  bind_rows(select(numtraits, Species),
            select(factraits, Species)) %>% 
    distinct() %>% 
    nrow()
  
  factraits %>% 
    distinct(Species) %>% 
    nrow()
  
  numtraits %>% 
    distinct(Species) %>% 
    nrow()
}

# Cscore across regions -------------------------------------------
{
  mycomparisons <- list(c("Dakotas", "IL"), 
                        c("Dakotas", "KS"), 
                        c("Dakotas", "NY"),
                        c("Dakotas", "SouthFL"),
                        c("IL", "KS"),
                        c("IL", "NY"),
                        c("IL", "SouthFL"),
                        c("KS", "NY"),
                        c("KS", "SouthFL"),
                        c("NY", "SouthFL"))

  
  region_m <- lm(Cscore ~ Region, data = factraits)
  Anova(region_m)
  emmeans(region_m, pairwise ~ Region) # tukey adjustment
  # compare_means(Cscore ~ Region, data = factraits, method="t.test") # note: sig marks aren't adjusted
  TukeyHSD(aov(Cscore ~ Region, data = factraits)) # same results
}



# ================ MORPHOLOGICAL TRAITS ============================

# Growth_form ------------------------------------------------------
{
  # NOTE: removing epiphytes b/c of low reps
  growth_form_m <- lm(Cscore ~ Growth_form*Region, 
                      data=filter(factraits, Growth_form!="epiphyte"))
  
  Anova(growth_form_m, type=2, singular.ok = TRUE)
  summary(growth_form_m)
  # must remove epiphyte (low reps) to estimate region main effect
  # sig interaction
}

# Woodiness ------------------------------------------------------------
{
  wood_m <- lm(Cscore ~ Woodiness*Region, data=factraits)
  Anova(wood_m, type=2) # sig interaction
  summary(wood_m)
  
}

# log(Veg_height) -------------------------------------------------------
{
  height_m <- lm(Cscore ~ log(Veg_height)*Region, data=numtraits)
  Anova(height_m, type=2) # sig interaction 
  summary(height_m)
  
}

# ================ LIFE HISTORY TRAITS ============================

# Germ_rate -------------------------------------------------------
{
  germ_m <- lm(Cscore ~ Germ_rate*Region, data=numtraits)
  Anova(germ_m, type=2) # not sig 
  summary(germ_m)
  
  
  germ_m1 <- lm(Cscore ~ Germ_rate + Region, data=numtraits)
  Anova(germ_m1, type=2) # germ rate predicts Cscore but coverage is very low at germ rate < 50%
  summary(germ_m1)
  summary(germ_m1)
  
}

# Growth_rate ------------------------------------------------------
{
  growth_rate_m <- lm(Cscore ~ Growth_rate*Region, data=factraits)
  Anova(growth_rate_m, type=2) # no sig interaction
  summary(growth_rate_m)
  
  growth_rate_m1 <- lm(Cscore ~ Growth_rate + Region, data=factraits)
  
  Anova(growth_rate_m1, type=2) # growth rate predicts Cscore
  summary(growth_rate_m1)
  
  emmeans(growth_rate_m1, pairwise ~ Growth_rate)
  
}

# Plant_lifespan_fac ----------------------------------------------- 
{
  lifespan_fac_m <- lm(Cscore ~ Plant_lifespan_fac*Region, data=factraits)
  Anova(lifespan_fac_m, type=2) # sig interaction
  summary(lifespan_fac_m)
  
  # annuals consistently have sig lower Cscores than perennials
  emmip(lifespan_fac_m, Plant_lifespan_fac ~ Region) 
  emmeans(lifespan_fac_m, pairwise ~ Plant_lifespan_fac | Region)
  
  # annuals consistently have sig lower Cscores than perennials
  emmip(lifespan_fac_m, Region ~ Plant_lifespan_fac) 
  emmeans(lifespan_fac_m, pairwise ~ Region | Plant_lifespan_fac)
  # Cscores of annuals are much higher in NY than other regions
  # again KS + FL appear to separate out from the other region (in perennials)
  
}

# Seed_dry_mass -------------------------------------------------------
{
  seed_mass_m <- lm(Cscore ~ Seed_dry_mass*Region, data=numtraits)
  Anova(seed_mass_m, type=2) # no sig 
  summary(seed_mass_m)
  
  seed_mass_m1 <- lm(Cscore ~ Seed_dry_mass + Region, data=numtraits)
  Anova(seed_mass_m1, type=2) # seed dry mass doesn't predict Cscore
}

# log(Plant_lifespan_yrs) -------------------------------------------------------
{
  lifespan_yrs_m <- lm(Cscore ~ log(Plant_lifespan_yrs)*Region, data=numtraits)
  Anova(lifespan_yrs_m, type=2) # sig interaction 
  summary(lifespan_yrs_m)
  
  emmip(lifespan_yrs_m, Region ~ log(Plant_lifespan_yrs), cov.reduce = range) 
  emtrends(lifespan_yrs_m, pairwise ~ Region, var = "log(Plant_lifespan_yrs)" ) # "$emtrends" output shows the slopes
  # all regions show pos relationships between lifespan and Cscore, but the magnitude of change differs

}

# ================ DISPERSAL ABILITY TRAITS ============================

# Dispersal_syndrome ----------------------------------------------
{
  disp_synd_m <- lm(Cscore ~ Dispersal_syndrome*Region, data=factraits)
  Anova(disp_synd_m, type=2) # sig interaction
  summary(disp_synd_m)
  
  # plot to examine interactions
  emmip(disp_synd_m, Dispersal_syndrome~Region) + mytheme
  # human always has lowest Cscore
  # KS and FL also especially low
  emmip(disp_synd_m, Region ~ Dispersal_syndrome) + mytheme
  
  emmeans(disp_synd_m, pairwise ~ Region)
  emmeans(disp_synd_m, pairwise ~ Dispersal_syndrome)
  
  # compare disp syndromes conditioned on region
  emmeans(disp_synd_m, pairwise ~ Dispersal_syndrome | Region)
  
  # testing beta distribution
  {
    library(betareg)
    factraits_breg <- factraits # copy dataset to manipulate Cscore
    # rescale Cscore (0-10) as being >0 - < 1.0
    factraits_breg <- factraits_breg %>% 
      mutate(Cscore_b = Cscore/10, 
             Cscore_b = ifelse(Cscore_b == 0, 0.01, Cscore_b),
             Cscore_b = ifelse(Cscore_b == 1.00, 0.99, Cscore_b))
    
    # make regression model based on beta dist of Cscore
    disp_synd_b_m <- betareg(Cscore_b ~ Dispersal_syndrome*Region, data=factraits_breg)
    
    # compare the residuals vs fitted plot for linear regression vs beta regression
    plot(fitted(disp_synd_b_m), residuals(disp_synd_b_m)) # not actually sure this is much improved
    plot(disp_synd_m, which=1)
    
    summary(disp_synd_b_m)
    joint_tests(disp_synd_b_m) # all are significant
    emmeans(disp_synd_b_m, pairwise ~ Dispersal_syndrome | Region)
    
    lmtest::lrtest(disp_synd_b_m)
    
  }
}

# Propagation_type ----------------------------------------------------
{
  propagaion_m <- lm(Cscore ~ Propagation_type*Region, data=factraits)
  factraits %>% 
    filter(!is.na(Propagation_type)) %>% 
    group_by(Region, Propagation_type) %>% 
    tally() %>% 
    arrange(n) # some low reps 
  
  Anova(propagaion_m, type=2, singular.ok = TRUE) # no sig interaction
  summary(propagaion_m)
  
  propagaion_m1 <- lm(Cscore ~ Propagation_type + Region, data=factraits)
  Anova(propagaion_m1, type=2)
  # mean Cscores per group
  factraits %>% 
    filter(!is.na(Propagation_type)) %>% 
    group_by(Propagation_type) %>% 
    summarize(meanC = mean(Cscore),
              seC = sd(Cscore)/sqrt(length(Cscore)))
  
  ggplot(filter(factraits, !is.na(Propagation_type)), 
         aes(x=Propagation_type, 
             y = Cscore)) +
    geom_boxplot() +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    geom_jitter(alpha=0.5, color="grey70") +
    mytheme
}


# Dispersal_unit_type ----------------------------------------------
{
  factraits %>% 
    filter(!is.na(Dispersal_unit_type)) %>% 
    group_by(Dispersal_unit_type) %>% # exclude vegetative
    tally() %>% 
    arrange(n) # note that we have some levels with 1 rep
  
  disp_unit_m <- lm(Cscore ~ Dispersal_unit_type*Region, 
                    data=filter(factraits, Dispersal_unit_type!="vegetative")) 
  
  Anova(disp_unit_m, type=2, singular.ok = TRUE) # no sig interaction
  summary(disp_unit_m)
  
  disp_unit_m2 <- lm(Cscore ~ Dispersal_unit_type + Region, 
                     data=filter(factraits, Dispersal_unit_type!="vegetative")) 
  Anova(disp_unit_m2, type=2)
  # mean Cscores per group
  factraits %>% 
    filter(Dispersal_unit_type!="vegetative") %>% 
    filter(!is.na(Dispersal_unit_type)) %>% 
    group_by(Dispersal_unit_type) %>% 
    summarize(meanC = mean(Cscore),
              seC = sd(Cscore)/sqrt(length(Cscore)))
  
  # fruit has lower Cscore than seed and spore; spore > seed
  factraits %>% 
    filter(Dispersal_unit_type!="vegetative") %>% 
    filter(!is.na(Dispersal_unit_type)) %>% 
    ggplot(., 
           aes(x=Dispersal_unit_type, y=Cscore)) + 
    geom_jitter(color="grey70", alpha=0.5) +
    geom_boxplot(alpha=0) +
    mytheme
  
  emmeans(disp_unit_m2, pairwise ~ Dispersal_unit_type)
  
}





# ================ RESOURCE USE EFFICIENCY TRAITS ============================

# LDMC -----------------------------------------------------------------
{
  ldmc_m <- lm(Cscore ~ LDMC*Region, data=numtraits)
  Anova(ldmc_m, type=2) # non sig interaction
  summary(ldmc_m)
  
  ldmc_m2 <- lm(Cscore ~ LDMC + Region, data=numtraits)
  Anova(ldmc_m2, type=2) # LDMC not sig
  
}

# LeafN -----------------------------------------------------------------
{
  leaf_n_m <- lm(Cscore ~ LeafN*Region, data=numtraits)
  Anova(leaf_n_m, type=2) # non sig interaction (p=0.217)
  summary(leaf_n_m)
  
  leaf_n_m2 <- lm(Cscore ~ LeafN + Region, data=numtraits)
  Anova(leaf_n_m2, type=2) # both sig; best model
  summary(leaf_n_m2)
  AIC(leaf_n_m, leaf_n_m2)
  
  ggplot(filter(numtraits, !is.na(LeafN)),
         aes(
           x = LeafN,
           y = Cscore,
           color = Region)) +
    geom_jitter(alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    facet_wrap(~Region, scales="free") +
    mytheme +
    theme(legend.position = "top")
  
  
}

# Photo_pathway ----------------------------------------------------
{
  factraits %>% 
    filter(!is.na(Photo_pathway)) %>% 
    group_by(Region,Photo_pathway) %>% 
    tally() %>% 
    arrange(n) # note that CAM has low reps (sometimes 1)
  # must remove CAM (due to low reps) to estimate marginal means
  
  photo_m <- lm(Cscore ~ Photo_pathway*Region, data=filter(factraits, Photo_pathway!="cam"))
  
  Anova(photo_m, type=2, singular.ok = TRUE) # sig interaction
  summary(photo_m)
  
  # plot to examine interactions
  # c4 always lowest; cam highest except in FL
  emmip(photo_m, Photo_pathway ~ Region) + mytheme  # don't use this one with cam
  # c4 < c3
  emmeans(photo_m, pairwise ~ Photo_pathway)
  # compare photo pathway conditioned on region
  # c4 < c3 in all regions
  emmeans(photo_m, pairwise ~ Photo_pathway | Region) 
  
  # NY generally highest
  # Dak is high too except very low for c4 plants
  emmip(photo_m, Region ~ Photo_pathway) 
  # compare region conditioned on photo pathway
  emmeans(photo_m, pairwise ~ Region | Photo_pathway) 
  # for c3 plants there are 2 groups; Dak + IL + NY vs KS and FL
  # but c4 plants in Dak have lowest Cscores (only sig diff from NY)
  
  factraits %>% 
    filter(!is.na(Photo_pathway) & Photo_pathway!="cam") %>% 
    ggplot(., aes(x=Photo_pathway, y=Cscore, color=Region)) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line", aes(group=Region))
  
}

# SLA_no_pet -----------------------------------------------------------
{
  numtraits %>% 
    filter(!is.na(SLA_no_pet)) %>% 
    # group_by(Region) %>% 
    tally()
  
  sla_no_pet_m <- lm(Cscore ~ SLA_no_pet*Region, data=numtraits)
  Anova(sla_no_pet_m, type=2) # nonsig interaction but non-sig main effect
  summary(sla_no_pet_m)
  
  sla_no_pet_m2 <- lm(Cscore ~ SLA_no_pet + Region, data=numtraits) # best model
  Anova(sla_no_pet_m2, type=2)
  summary(sla_no_pet_m2)
  
  # plot
  ggplot(filter(numtraits, !is.na(SLA_no_pet)),
         aes(x=SLA_no_pet,
             y=Cscore,
             color=Region)) +
    geom_jitter(alpha=0.5) +
    geom_smooth(method="lm", se=F, aes(color=Region)) +
    facet_wrap(~Region, scales="free") +
    theme_minimal() +
    theme(legend.position = "top")
}

# SLA_with_pet - NOT USING ---------------------------------------------------------
{
  numtraits %>% 
    filter(!is.na(SLA_with_pet)) %>% 
    # group_by(Region) %>%
    tally()
  
  sla_with_pet_m <- lm(Cscore ~ SLA_with_pet*Region, data=numtraits)
  Anova(sla_with_pet_m, type=3) # non sig interaction
  
  sla_with_pet_m2 <- lm(Cscore ~ SLA_with_pet + Region, data=numtraits) # best model
  Anova(sla_with_pet_m2, type=2) # SLA not sig here
}




# ================ DISTURBANCE TOLERANCE TRAITS ============================

# Resprout -------------------------------------------------------------
{
  resprout_m <- lm(Cscore ~ Resprout*Region, data=factraits)
  Anova(resprout_m, type=2) # marginally sig (p = 0.089)
  summary(resprout_m)
  
  resprout_m2 <- lm(Cscore ~ Resprout + Region, data=factraits)
  Anova(resprout_m2, type=2) # margially sig (p = 0.092)
  # no sig effect of resprout
  
  emmip(resprout_m, Resprout ~ Region) + mytheme  
  emmeans(resprout_m, pairwise ~ Resprout)
  
  # compare resprouting ability conditioned on region
  # c4 < c3 in all regions
  emmeans(resprout_m, pairwise ~ Resprout | Region) 
  emmip(resprout_m, Region ~ Resprout) + mytheme 
  
}

# Fire_tol -------------------------------------------------------------
{
  fire_m <- lm(Cscore ~ Fire_tol*Region, data=factraits)
  Anova(fire_m, type=2) # sig interaction
  summary(fire_m)
  
  # biggest response is that Cscore of Dakotas changes drastically across fire tol levels
  emmip(fire_m, Region ~ Fire_tol) + mytheme
  
  ggplot(filter(factraits, !is.na(Fire_tol)), 
         aes(x = factor(Fire_tol, levels=c("low", "medium", "high")),
             y=Cscore,
             color=Region,
             group=Region)) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    # stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme
  
  emmeans(fire_m, pairwise ~ Region | Fire_tol)
  
  # plants with medium fire tol generally have highest Cscores
  emmip(fire_m, Fire_tol ~ Region) + mytheme
  ggplot(filter(factraits, !is.na(Fire_tol)), 
         aes(x = Region,
             y=Cscore,
             color=factor(Fire_tol, levels=c("low", "medium", "high")),
             group=factor(Fire_tol, levels=c("low", "medium", "high")))) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    # stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme +
    theme(legend.title = element_blank())
  
  emmeans(fire_m, pairwise ~ Fire_tol | Region)
  
}

# Shade_tol ------------------------------------------------------------
{
  shade_m <- lm(Cscore ~ Shade_tol*Region, data=factraits)
  Anova(shade_m, type=2) # sig interaction
  summary(shade_m)
  
  # Cscores generally increase low < med < high shade tol, except for NY not really
  emmip(shade_m, Region ~ Shade_tol) + mytheme
  ggplot(filter(factraits, !is.na(Shade_tol)), 
         aes(x = factor(Shade_tol, levels=c("low", "medium", "high")),
             y=Cscore,
             color=Region,
             group=Region)) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme
  emmeans(shade_m, pairwise ~ Region | Shade_tol)
  
  # plants with high shade tol have highest Cscores except for in NY; 
  emmip(shade_m, Shade_tol ~ Region)
  ggplot(filter(factraits, !is.na(Shade_tol)), 
         aes(x = Region,
             y=Cscore,
             color=factor(Shade_tol, levels=c("low", "medium", "high")),
             group=factor(Shade_tol, levels=c("low", "medium", "high")))) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme +
    theme(legend.title = element_blank())
  emmeans(shade_m, pairwise ~ Shade_tol | Region)
}

# Salt_tol -------------------------------------------------------------
{
  salt_m <- lm(Cscore ~ Salt_tol*Region, data=factraits)
  Anova(salt_m, type=2) # sig interaction
  summary(salt_m)
  
  # cscores bouncing around a lot across levels
  # sig differences across region for plants w/ low salt tol, but inconsistent responses at med and high salt tol
  emmip(salt_m, Region ~ Salt_tol)
  ggplot(filter(factraits, !is.na(Salt_tol)), 
         aes(x = factor(Salt_tol, levels=c("low", "medium", "high")),
             y=Cscore,
             color=Region,
             group=Region)) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme
  emmeans(salt_m, pairwise ~ Region | Salt_tol)
  
  # plants with high salt tol have highest Cscores except for in Dak; 
  # a lot of variability
  emmip(salt_m, Salt_tol ~ Region)
  ggplot(filter(factraits, !is.na(Salt_tol)), 
         aes(x = Region,
             y=Cscore,
             color=factor(Salt_tol, levels=c("low", "medium", "high")),
             group=factor(Salt_tol, levels=c("low", "medium", "high")))) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme +
    theme(legend.title = element_blank())
  emmeans(salt_m, pairwise ~ Salt_tol | Region)
}

# Drought_tol ----------------------------------------------------------
{
  drought_m <- lm(Cscore ~ Drought_tol*Region, data=factraits)
  Anova(drought_m, type=2) # sig interaction
  summary(drought_m)
  
  factraits %>% select(Region, Species, Cscore, Shade_tol, Drought_tol) %>%View() 
  
  # really the only big difference is that the Cscore of the Dakota moves from being highest in plants with high Drought_tol to moderate in plants w low drought tol
  emmip(drought_m, Region ~ Drought_tol)
  ggplot(filter(factraits, !is.na(Drought_tol)), 
         aes(x = factor(Drought_tol, levels=c("low", "medium", "high")),
             y=Cscore,
             color=Region,
             group=Region)) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    mytheme
  
  emmeans(drought_m, pairwise ~ Region | Drought_tol)
  
  # plants with high drought tol generally have highest Cscores
  emmip(drought_m, Drought_tol ~ Region)
  ggplot(filter(factraits, !is.na(Drought_tol)), 
         aes(x = Region,
             y=Cscore,
             color=factor(Drought_tol, levels=c("low", "medium", "high")),
             group=factor(Drought_tol, levels=c("low", "medium", "high")))) +
    stat_summary(fun.data="mean_se", geom="point", size=2) +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0) +
    stat_summary(fun.data="mean_se", geom="line") +
    theme(legend.title = element_blank())
  emmeans(drought_m, pairwise ~ Drought_tol | Region)
}

# Drought_tol_Mpa - DO NOT USE -------------------------------------------------------
{
  # let's delete this b/c coverage is very low and problems getting data to meeet normality assumptions 
  drought_mpa_m <- lm(Cscore ~ Drought_tol_Mpa*Region, data=numtraits)
  Anova(drought_mpa_m, type=3) # not sig 
  
  drought_mpa_m1 <- lm(Cscore ~ Drought_tol_Mpa + Region, data=numtraits)
  Anova(drought_mpa_m1, type=2) # drought_mpa_m1 predicts Cscore but coverage is very low
  
  ggplot(filter(numtraits, !is.na(Drought_tol_Mpa)),
         aes(x=Drought_tol_Mpa,
             y=Cscore)) +
    geom_jitter(alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    mytheme +
    theme(legend.position = "none")
}
