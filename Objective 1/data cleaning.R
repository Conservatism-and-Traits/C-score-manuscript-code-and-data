# this file is used to clean the trait data for use in the ft x geographic range analyses

library(tidyverse)

# try data - load all; separate factors and num traits -----------------
{
  # we requested try data in 6 different requests. first compile them all
  try0 <- read.delim("./data/base/final TRY request - full data/13803.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " ")) 
  
  try1 <- read.delim("data/base/final TRY request - full data/13804.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " ")) 
  
  try2 <- read.delim("data/base/final TRY request - full data/13805.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " "))
  
  try3 <- read.delim("data/base/final TRY request - full data/14041.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " "))
  
  try4 <- read.delim("data/base/final TRY request - full data/14042.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " "))
  
  try5 <- read.delim("data/base/final TRY request - full data/14043.txt", 
                     header=T, sep="\t", dec = ".", quote="", 
                     # fileEncoding = "latin1", 
                     na.strings=c("NA", "", " "))
  
  try_num <- bind_rows(try0, try1, try2)
  try_fac <- bind_rows(try3, try4, try5)
  
  try_num <- try_num %>% 
    select(LastName, 
           DatasetID, 
           "Species"=AccSpeciesName, 
           TraitName, 
           "Value" = StdValue, # standardized value
           "Unit"=UnitName, # standardized units
           "Value2"=OrigValueStr, # name for non-standard traits (eg categorical)
           "Unit2"=OrigUnitStr) %>% # units for non-standard traits (eg categorical)
    filter(TraitName!="")
  
  try_fac <- try_fac %>% 
    select(LastName, 
           DatasetID, 
           "Species"=AccSpeciesName, 
           TraitName, 
           "Value" = StdValue, # standardized value
           "Unit"=UnitName, # standardized units
           "Value2"=OrigValueStr, # name for non-standard traits (eg categorical)
           "Unit2"=OrigUnitStr) %>% # units for non-standard traits (eg categorical)
    filter(TraitName!="")
  
  # some categorical traits have nonsensical units; update these as able
  # note that there are still some irrelevant trait values that will need to be cleaned later
  try_fac <-  try_fac %>% 
    mutate(Unit2 = case_when(TraitName == "Plant growth form" ~ "string",
                             TraitName == "Plant growth rate" ~ "string",
                             TraitName == "Plant lifespan (longevity)" & !is.na(Unit2) ~ "years", # note: some ppl report in # yrs; others in annual/perennial
                             TraitName == "Plant resprouting capacity" ~ "string",
                             TraitName == "Species tolerance to salt" ~ "string",
                             TRUE ~ as.character(Unit2)),
           TraitName = case_when(Unit2 == "years" ~ "Plant lifespan (years)", # create a new trait 
                                 Unit2 == "Mpa" ~ "Species tolerance to drought - Mpa",
                                 TRUE ~ as.character(TraitName)),
           Unit = case_when(TraitName == "Plant lifespan (years)" ~ "years",
                            TRUE ~ as.character(Unit)))
  
  try_num <- try_num %>% 
    mutate(Value=ifelse(!is.na(Value), # Value and Unit are not always consistent; sometimes value entered with proper units, but if not TRY standardized and put values in Value2/Unit2
                        as.character(Value),
                        as.character(Value2)),
           Unit=ifelse(!is.na(Unit),
                       as.character(Unit),
                       as.character(Unit2))) %>% 
    select(-Value2, -Unit2)
  
  try_fac <- try_fac %>% 
    mutate(Value=ifelse(!is.na(Value), # Value and Unit are not always consistent; sometimes value entered with proper units, but if not TRY standardized and put values in Value2/Unit2
                        as.character(Value),
                        as.character(Value2)),
           Unit=ifelse(!is.na(Unit),
                       as.character(Unit),
                       as.character(Unit2))) %>% 
    select(-Value2, -Unit2)
  
  try_fac <- try_fac %>%
    replace_na(list(Unit="string"))  # for consistency, add "string" as unit type for cat traits
  
  # export for transparency
  # write.csv(x=bind_rows(try_num, try_fac),
  #           file="data/base/full_try_data_raw.csv", row.names = F)
  
  # check number of units per trait name
  try_fac %>% 
    distinct(TraitName, Unit) %>% 
    group_by(TraitName) %>% 
    tally() %>%  
    filter(n>1) # good; each trait has only 1 unit
  

} 

# clean try_num df -----------------------------------------------------
{
  units_num <- try_num %>% distinct(TraitName, Unit) # create lookup table of units for each trait
  
  # convert all values to numeric  
  trywide <- try_num %>% 
    mutate(rown = 1:nrow(.)) %>% # we have replicate measures so add placeholder row num
    select(-Unit) %>% 
    group_by(rown, LastName, DatasetID, Species) %>% 
    spread(., key=TraitName, value=Value)
  
  # there are some values with incorrect notation for NA & values of 0 or other numbers which can't be right; fix these manually
  # names(trywide)
  # View(unique(trywide[,11]))
  trywide <- trywide %>% 
    mutate(`Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included` = case_when(`Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included` == "na" ~ NA_character_,
                                                                                                          `Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included` == "null" ~ NA_character_,
                                                                                                          `Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included` == "NULL" ~ NA_character_,
                                                                                                          TRUE ~ as.character(`Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included`)),
           
           `Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)` = case_when(`Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)` == "1350" ~ NA_character_, # not possible
                                                                        TRUE ~ as.character(`Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)`)) ,
           
           
           `Plant height vegetative` = str_remove(`Plant height vegetative`, "to"),
           `Plant height vegetative` = str_remove(`Plant height vegetative`, "cm"),
           `Plant height vegetative` = str_remove(`Plant height vegetative`, "ft"),
           `Plant height vegetative` = str_remove(`Plant height vegetative`, "m"),
           `Plant height vegetative` = str_trim(`Plant height vegetative`, "both"), # trim extra spaces before and after
           
           `Plant height vegetative` = case_when(`Plant height vegetative` == "0" ~ NA_character_, # not possible
                                       TRUE ~ as.character(`Plant height vegetative`)) ,
           
           `Seed germination rate (germination efficiency)` = case_when(`Seed germination rate (germination efficiency)` == "0" ~ NA_character_, # not possible
                                                                        `Seed germination rate (germination efficiency)` == "Germ" ~ NA_character_,
                                                                        TRUE ~ as.character(`Seed germination rate (germination efficiency)`)) ,
           
           `Seed dry mass` = case_when(`Seed dry mass` == "0" ~ NA_character_,
                                       TRUE ~ as.character(`Seed dry mass`)) 
           ) 
  
  # convert all trait values to numeric
  trylong <- trywide %>% 
    ungroup() %>% 
    select(-rown) %>% # remove placeholder column
    gather(., key=TraitName, value=Value, 4:ncol(.)) %>% # gather all values into one column
    mutate(Value=as.numeric(Value)) # convert value column to numeric
  
  trylong_num <- left_join(trylong, units_num, by=c("TraitName")) # add units back in
  
  head(trylong_num)
}

# clean try_fac df -----------------------------------------------------
{
  units_fac <- try_fac %>% distinct(TraitName, Unit) # create lookup table of units for each trait
  
  # clean trait levels levels  
  try_fac <- try_fac %>% 
    mutate(Value = tolower(Value), # convert to lower case
           Value = str_trim(Value, side="both")) # remove any extra spaces

  
  # the cleaning steps are: 
    # 1. assess num of trait levels for a trait
    # 2. if they can be easily condensed by hand, do this
    # 3. if not, go back to see if there are large groups that are using the same trait levels
          # This will probably require going back to the original try3, try4, or try5 dfs

  # plant growth form
  {
    try_fac %>% 
      filter(TraitName=="Plant growth form") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # rename 
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant growth form" & Value == "t" ~ "tree",
                               TraitName=="Plant growth form" & Value == "tree/treelet" ~ "tree",
                               
                               TraitName=="Plant growth form" & Value == "tree, shrub" ~ "woody",
                               TraitName=="Plant growth form" & Value == "shrub/tree" ~ "woody",
                               TraitName=="Plant growth form" & Value == "subshrub" ~ "woody",
                               TraitName=="Plant growth form" & Value == "st" ~ "woody",
                               
                               TraitName=="Plant growth form" & Value == "forb/herb" ~ "forb",
                               TraitName=="Plant growth form" & Value == "h" ~ "forb",
                               TraitName=="Plant growth form" & Value == "f" ~ "forb",
                               TraitName=="Plant growth form" & Value == "herbaceous" ~ "forb",
                               TraitName=="Plant growth form" & Value == "herb" ~ "forb",
                               TraitName=="Plant growth form" & str_detect(Value, "herb/") ~ "forb",
                               
                               
                               TraitName=="Plant growth form" & Value == "s" ~ "shrub",
                               TraitName=="Plant growth form" & Value == "subshrub/shrub" ~ "shrub",
                               TraitName=="Plant growth form" & Value == "subshrub, shrub" ~ "shrub",
                               
                               TraitName=="Plant growth form" & Value == "g" ~ "graminoid",
                               TraitName=="Plant growth form" & Value == "grass" ~ "graminoid",
                               TraitName=="Plant growth form" & str_detect(Value, "graminoid") ~ "graminoid",
                               TRUE ~ as.character(Value)))
    
    # remove all other non-specified trait levels
    growthforms <- c("fern", "shrub", "vine", "tree", "woody", "forb", "graminoid", "liana", "epiphyte")
    # delete all other trait values that aren't in accepted list
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant growth form" & !Value %in% growthforms ~ NA_character_,
                               TRUE ~ as.character(Value))) 

}

  # Plant lifespan (longevity)  
  {
    try_fac %>% 
      filter(TraitName=="Plant lifespan (longevity)") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # rename 
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant lifespan (longevity)" & Value == "always pluriennial-pollakanthic" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "perennials" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "per" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "moderate" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "long" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "poly-annuals 5-50 years (medium-lived perennials)" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "strict monocarpic bi-annuals and poly-annuals" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "biennial" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "poly-annuals < 5 years (short-lived perennials)" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "biennial, perennial" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "biennial/perennial" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "poly-annuals > 50 years (long-lived perennials)" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "always biennial" ~ "perennial",
                               TraitName=="Plant lifespan (longevity)" & Value == "always biennial, always pluriennial-pollakanthic" ~ "perennial",
                               
                               TraitName=="Plant lifespan (longevity)" & Value == "summer annuals" ~ "annual",
                               TraitName=="Plant lifespan (longevity)" & Value == "always annual" ~ "annual",
                               TraitName=="Plant lifespan (longevity)" & Value == "ann" ~ "annual",
                               TraitName=="Plant lifespan (longevity)" & Value == "annuals" ~ "annual",
                               TraitName=="Plant lifespan (longevity)" & Value == "winter annuals" ~ "annual",
                               TraitName=="Plant lifespan (longevity)" & Value == "annual-winter annual" ~ "annual",
                               
                               TRUE ~ as.character(Value)))
    
    # remove all other non-specified trait levels
    lifespan <- c("annual", "perennial")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant lifespan (longevity)" & !Value %in% lifespan ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Leaf photosynthesis pathway  
  {
    try_fac %>% 
      filter(TraitName=="Leaf photosynthesis pathway") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # rename - no need for this one, just remove bad levels

    # remove all other non-specified trait levels
    photo <- c("c3", "c4", "cam", "c3/c4")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Leaf photosynthesis pathway" & !Value %in% photo ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Dispersal syndrome 
  {
    try_fac %>% 
      filter(TraitName=="Dispersal syndrome") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
              
    # target levels: human, animal, multi, water, gravity, self, wind
    # https://www.ecologycenter.us/plant-ecology/info-dnr.html
    # rename 
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Dispersal syndrome" & str_detect(Value, "nauto") ~ "water",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "water") ~ "water",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "ombro") ~ "water",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "bythiso") ~ "water",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "rain") ~ "water",

                               TraitName=="Dispersal syndrome" & str_detect(Value, "zoo") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "cattle") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "bird") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "eaten") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "dyso") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "sheep") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "ant") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "deer") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "horse") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "pig") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "bird") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "rabbit") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "snail") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "mammal") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "pacarana") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "donkey") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "hare") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "mouse") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "goat") ~ "animal", 
                               TraitName=="Dispersal syndrome" & str_detect(Value, "vertebrate") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "fox") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "squirrel") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "dog") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "reptile") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "ants") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "chamois") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "boar") ~ "animal",   
                               TraitName=="Dispersal syndrome" & str_detect(Value, "monkey") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "marten") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "adhesion") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "ursus") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "animal") ~ "animal",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "carried") ~ "animal",


                               TraitName=="Dispersal syndrome" & str_detect(Value, "meteo") ~ "wind",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "wind") ~ "wind",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "boleo") ~ "wind",
                               TraitName=="Dispersal syndrome" & str_detect(Value, "anemo") ~ "wind",
                               TraitName=="Dispersal syndrome" & Value == "chamaechor" ~ "wind",
                               TraitName=="Dispersal syndrome" & LastName=="Miller" & Value=="1" ~ "wind",
                               TraitName=="Dispersal syndrome" & LastName=="Miller" & Value=="0" ~ "wind",
                               
                               TraitName=="Dispersal syndrome" & str_detect(Value, "agochor") ~ "human",
                                TraitName=="Dispersal syndrome" & str_detect(Value, "vehicle") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "domestic animal") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "speiro") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "ethelo") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "commerce") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "machin") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "clothes") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "hemero") ~ "human",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "contamination") ~ "human",

                              TraitName=="Dispersal syndrome" & str_detect(Value, "autoch") ~ "self",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "unassisted") ~ "self",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "beneath") ~ "self",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "explosive") ~ "self",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "ball") ~ "self",
                              TraitName=="Dispersal syndrome" & str_detect(Value, "baro") ~ "self",
                              TraitName=="Dispersal syndrome" & Value=="man" ~ "self",


                               TRUE ~ as.character(Value)))
    
    # remove all other non-specified trait levels
    disp <- c("water", "animal", "wind", "human", "self")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Dispersal syndrome" & !Value %in% disp ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Plant woodiness
  {
    try_fac %>% 
      filter(TraitName=="Plant woodiness") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    try_fac <- try_fac %>% 
        mutate(Value = case_when(TraitName=="Plant woodiness" & Value == "h" ~ "herbaceous",
                             TraitName=="Plant woodiness" & Value == "herb" ~ "herbaceous",
                             TraitName=="Plant woodiness" & Value == "grass&sedges" ~ "herbaceous",
                             TraitName=="Plant woodiness" & Value == "0" ~ "herbaceous",
                             TraitName=="Plant woodiness" & Value == "non-woody" ~ "herbaceous",
                             
                             TraitName=="Plant woodiness" & LastName=="Díaz" & Value == "1" ~ "semi-woody",
                             
                             TraitName=="Plant woodiness" & Value == "w" ~ "woody",
                             TraitName=="Plant woodiness" & Value == "suffrutex" ~ "woody",
                             TraitName=="Plant woodiness" & LastName=="Chapin" & Value == "1" ~ "woody",
                             TraitName=="Plant woodiness" & Value == "2" ~ "woody",

                             
                             TRUE ~ as.character(Value)))
  
  # remove all other non-specified trait levels
  wood <- c("herbaceous", "semi-woody", "woody")
  try_fac <- try_fac %>% 
    mutate(Value = case_when(TraitName=="Plant woodiness" & !Value %in% wood ~ NA_character_,
                             TRUE ~as.character(Value))) 
  }
  
  # Dispersal unit type
  {
    try_fac %>% 
      filter(TraitName=="Dispersal unit type") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # seed, fruit, veg, spore  
    try_fac <- try_fac %>% 
          mutate(Value = case_when(TraitName=="Dispersal unit type" & str_detect(Value, "fruit") ~ "fruit",
                                   TraitName=="Dispersal unit type" & str_detect(Value, "infructescence") ~ "fruit",
                                   
                                   TraitName=="Dispersal unit type" & Value == "mericarp" ~ "seed",
                                   TraitName=="Dispersal unit type" & str_detect(Value, "generative") ~ "seed",
                                   TraitName=="Dispersal unit type" & Value == "germinule" ~ "seed",

                                   TRUE ~ as.character(Value)))
  
  # remove all other non-specified trait levels
  disp2 <- c("fruit", "seed", "spore", "vegetative")
  try_fac <- try_fac %>% 
    mutate(Value = case_when(TraitName=="Dispersal unit type" & !Value %in% disp2 ~ NA_character_,
                             TRUE ~as.character(Value))) 
  }
  
  # Plant resprouting capacity
  {
    # remove all other non-specified trait levels - just yes/no
    resp <- c("no", "yes")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant resprouting capacity" & !Value %in% resp ~ NA_character_,
                             TRUE ~as.character(Value))) 
  }

  
  # Species tolerance to fire
  {
    try_fac %>% 
      filter(TraitName=="Species tolerance to fire") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # low, med, high
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to fire" & str_detect(Value, "killed") ~ "low",
                               TraitName=="Species tolerance to fire" & Value == "not fire resistant" ~ "low",
                               TraitName=="Species tolerance to fire" & Value == "no" ~ "low",
                               TraitName=="Species tolerance to fire" & Value == "none" ~ "low",

                               TraitName=="Species tolerance to fire" & str_detect(Value, "survives") ~ "high",
                               TraitName=="Species tolerance to fire" & Value == "fire resistant" ~ "high",
                               TraitName=="Species tolerance to fire" & Value == "yes" ~ "high",
    
                               TRUE ~ as.character(Value)))
  
    # remove all other non-specified trait levels
    fire <- c("low", "medium", "high")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to fire" & !Value %in% fire ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Plant lifespan (years)  ************ numeric ************
  {
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant lifespan (years)" & str_detect(Value, "-") ~ NA_character_,
                               TraitName=="Plant lifespan (years)" & str_detect(Value, "<") ~ NA_character_,
                               TraitName=="Plant lifespan (years)" & str_detect(Value, ">") ~ NA_character_,
                             
                             TRUE ~ as.character(Value)))
  }
  
  # Plant growth rate
  {
    # fast moderate slow
    try_fac %>% 
      filter(TraitName=="Plant growth rate") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))

    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant growth rate" & Value == "medium" ~ "moderate",
                               TraitName=="Plant growth rate" & Value == "rapid" ~ "fast",
                             
                             TRUE ~ as.character(Value)))
    
  # remove all other non-specified trait levels
  gr <- c("fast", "moderate", "slow")
  try_fac <- try_fac %>% 
    mutate(Value = case_when(TraitName=="Plant growth rate" & !Value %in% gr ~ NA_character_,
                             TRUE ~as.character(Value))) 
  }
      
  # Species tolerance to drought
  {
    
    try_fac %>% 
      filter(TraitName=="Species tolerance to drought") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # remove all other non-specified trait levels
    drought <- c("none", "low", "medium", "high")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to drought" & !Value %in% drought ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
      
  # Species tolerance to drought
  {
    
    try_fac %>% 
      filter(TraitName=="Species tolerance to drought") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # remove all other non-specified trait levels
    drought <- c("none", "low", "medium", "high")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to drought" & !Value %in% drought ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  #Species tolerance to salt
  {
    try_fac %>% 
      filter(TraitName=="Species tolerance to salt") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to salt" & Value == "none" ~ "low",
                               TraitName=="Species tolerance to salt" & Value == "fresh" ~ "low",
                               
                               TraitName=="Species tolerance to salt" & Value == "brackish" ~ "medium",
                               
                               TraitName=="Species tolerance to salt" & Value == "saline" ~ "high",
                               TraitName=="Species tolerance to salt" & Value == "sw" ~ "high",
                               TRUE ~ as.character(Value)))
    
    # remove all other non-specified trait levels
    salt <- c("low", "medium", "high")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to salt" & !Value %in% salt ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  #Species tolerance to shade
  {
    try_fac %>% 
      filter(TraitName=="Species tolerance to shade") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to shade" & Value == "intolerant" ~ "low",
                               TraitName=="Species tolerance to shade" & Value == "none" ~ "low",
                               TraitName=="Species tolerance to shade" & Value == "light" ~ "low",
                               
                               TraitName=="Species tolerance to shade" & Value == "intermediate" ~ "medium",
                               TraitName=="Species tolerance to shade" & Value == "mid" ~ "medium",
                               
                               TraitName=="Species tolerance to shade" & Value == "deep" ~ "high",
                               TraitName=="Species tolerance to shade" & Value == "tolerant" ~ "high",
                               
                               TRUE ~ as.character(Value)))
    
    # remove all other non-specified trait levels
    shade <- c("low", "medium", "high")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Species tolerance to shade" & !Value %in% shade ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Plant propagation type
  {
    # seed, seed + veg, veg
    try_fac %>% 
      filter(TraitName=="Plant propagation type") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))
    
    # remove all other non-specified trait levels
    prop <- c("seed", "seed and vegetative", "vegetative")
    try_fac <- try_fac %>% 
      mutate(Value = case_when(TraitName=="Plant propagation type" & !Value %in% prop ~ NA_character_,
                               TRUE ~as.character(Value))) 
  }
  
  # Species tolerance to drought - Mpa ************ numeric ************
  {
    # ok; no changes needed
    try_fac %>% 
      filter(TraitName=="Species tolerance to drought - Mpa") %>% 
      distinct(Value)
    
    try_fac %>% 
      filter(TraitName=="Species tolerance to drought - Mpa") %>% 
      group_by(Value) %>% 
      tally() %>% 
      arrange(desc(n))

  }

}

# add extra num traits to num df ---------------------------------------
{
   extra_num_traits <- try_fac %>% 
    filter(TraitName=="Species tolerance to drought - Mpa" |
             TraitName == "Plant lifespan (years)") %>% 
    mutate(Value=as.numeric(as.character(Value)))
  
  trylong_num <- bind_rows(trylong_num, extra_num_traits)

  # remove numeric traits from fac df
  try_fac <- try_fac %>% 
    filter(TraitName!="Species tolerance to drought - Mpa" &
             TraitName != "Plant lifespan (years)")  
}

# check num of reps per fac trait level -- lump or delete levels -------
{
  # this calculates the num of reps per trait level (n) and the total number of samples for that Trait
  # consider excluding trait levels with few reps
  # or lump trait levels together
  try_fac %>% 
    distinct() %>% 
    filter(!is.na(Value)) %>% # remove rows with no trait value
    group_by(TraitName, Value) %>% 
    tally() %>% 
    group_by(TraitName) %>% 
    mutate(TotTraitN = sum(n)) %>% 
    arrange(n) %>% 
    head(15) 
  
  try_fac <- try_fac %>% 
    mutate(Value = case_when(TraitName == "Leaf photosynthesis pathway" & Value == "c3/c4" ~ NA_character_,
                             
                             TraitName == "Plant growth form" & Value == "fern" ~ "forb",
                             TraitName == "Plant growth form" & Value == "liana" ~ "vine",
                             
                             TRUE ~ as.character(Value)))
}

# check for errors in numerical traits ---------------------------------
{
  # what is happening with the drought tolerance Mpa values - why do we have pos and neg Mpa values??
  trylong_num %>% 
    filter(TraitName == "Species tolerance to drought - Mpa" & !is.na(Value)) %>% 
    arrange(desc(Value)) %>% 
    filter(Value >= 0) 
  
  head(extra_num_traits)
  extra_num_traits %>% 
    filter(TraitName == "Species tolerance to drought - Mpa") %>% 
    distinct(LastName)
  
  # recall these were originally coded as factorial traits
  try3 %>% 
    filter(LastName=="Craine" & TraitName=="Species tolerance to drought") %>% 
    select(DataName, OriglName, OrigValueStr, Comment)
  # according to Craine's paper, "Psicrit" is the critical leaf Psi value below which functioning stops
  # this should be negative; more negative is more drought tolerant
  
  # can't tell what the positive values are, but I think they need to be deleted
  trylong_num <- trylong_num %>% 
    mutate(Value = ifelse(TraitName == "Species tolerance to drought - Mpa" & Value > 0, NA, Value))
}

# export species average trait values - numeric traits -----------------
{
  # calculate the AVERAGE trait value for each sp x trait
  meantraitvalues_num <-   trylong_num %>% 
    filter(!is.na(Value)) %>% #  exclude entries where trait value was deleted 
    group_by(Species, TraitName, Unit) %>% 
    summarize(meanValue=mean(Value), # average
              N_measurements=length(Value), # num of reps used to get average
              semvalue=sd(Value)/sqrt(N_measurements)) # sem of average
  head(meantraitvalues_num)
  # write.csv(meantraitvalues_num, file="data/processed/Mean trait values plus info - num traits.csv", row.names = F) # export

  # spread wide
  meantraitvalues_num_clean <- meantraitvalues_num %>% 
    select(-Unit, -N_measurements, -semvalue) %>% 
    spread(., key=TraitName, value=meanValue) 
  head(meantraitvalues_num_clean)
  # write.csv(meantraitvalues_num_clean, file="data/processed/Mean Trait Values - num traits.csv", row.names = F)
  
}

# export sp mode trait values - factorial traits -----------------------
{
  Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)] # this allows for multiple modes
  }
  
  # calculate trait value mode for each species
  # do any sp x traits have multimodes
  # calc total num of measuremnts per TRAIT
  nmeas_pertrait <- try_fac %>% 
    filter(!is.na(Value)) %>% 
    group_by(Species, TraitName) %>%
    tally(name = "N_measurements_per_trait") # calc the num of measurements per trait 
  
  # cal total num of measurements per trait VALUE
  nmeas_pervalue <- try_fac %>% 
    filter(!is.na(Value)) %>% 
    group_by(Species, TraitName, Value) %>%
    tally(name = "N_measurements_per_trait_value") # calc the num of measurements per trait value
  
  nmeas_pervalue %>% 
    arrange(Species, TraitName, Value)
  
  # calculate mode(s) for each sp x trait
  modevals_tmp <- try_fac %>% 
    filter(!is.na(Value)) %>% 
    group_by(Species, TraitName) %>% 
    summarize(modeValue = Modes(Value)) 
  
  # do any sp x traits have multiple modes?
  multimodes <- modevals_tmp %>% 
    group_by(Species, TraitName) %>% 
    tally() %>% 
    filter(n>1) %>%  # keep only entries with >1 mode
    arrange(desc(n))
  nrow(multimodes)#  yes many do
  head(multimodes) 
  # make look-up col for filtering
  multimodes$Lookup <- paste(multimodes$Species, multimodes$TraitName, sep="_")
  
  try_fac %>% 
    filter(TraitName == "Species tolerance to shade" & Species=="Poa nemoralis") %>% 
    filter(!is.na(Value))
  
  # combine into single df in num of measurements   
  modevals <- full_join(modevals_tmp, nmeas_pertrait, by=c("Species", "TraitName"))
  modevals <- left_join(modevals, nmeas_pervalue, by=c("Species", "TraitName", "modeValue" = "Value"))
  head(modevals)
  # make look-up col for filtering
  modevals$Lookup <- paste(modevals$Species, modevals$TraitName, sep="_")
  
  # make df with only the sp x traits with multi-modes
  multimodes2 <- modevals %>% 
    filter(Lookup %in% multimodes$Lookup) %>% # keep only particular sp x trait pairs
    select(-Lookup)
  
  # make df with only the sp x traits with single mode 
  nondup_modes <- modevals %>% 
    filter(!Lookup %in% multimodes$Lookup) %>% # exclude particular sp x trait pairs
    select(-Lookup)
  
  # update the mode values for the duplicated sp x traits
  multimodes_cleaned <- multimodes2 %>% 
    mutate(modeValue = case_when(TraitName == "Species tolerance to fire" ~ "medium",
                                 TraitName == "Species tolerance to salt" ~ "medium",
                                 TraitName == "Species tolerance to shade" ~ "medium",
                                 TraitName == "Species tolerance to drought" ~ "medium",
                                 
                                 TraitName == "Plant growth form" & Species == "Corchorus siliquosus" ~ "shrub",
                                 TraitName == "Plant growth form" & Species == "Mitchella repens" ~ "forb",
                                 TraitName == "Plant growth form" & Species == "Vitis rupestris" ~ "vine",
                                 TraitName == "Plant growth form" & modeValue == "woody" ~ NA_character_,
                                 TraitName == "Plant growth form" & modeValue == "forb" ~ NA_character_,
                                 
                                 TraitName == "Dispersal syndrome" ~ "multi", # many have multiple dispersal syndromes; ok to add extra factor level
                                 
                                 TraitName == "Plant woodiness" ~ "semi-woody",
                                 
                                 TraitName == "Plant lifespan (longevity)" ~ NA_character_, # converting these to NA to not add additional factor level
                                 
                                 TraitName == "Plant resprouting capacity" ~ NA_character_,
                                 
                                 TraitName == "Dispersal unit type" ~ NA_character_,
                                 
                                 TraitName == "Leaf photosynthesis pathway" ~ NA_character_,

                                 TraitName == "Plant growth rate" ~ NA_character_,
                                 
                                 TraitName == "Plant propagation type" ~ NA_character_,
                                 
                                  TRUE ~ as.character(modeValue))) %>% 
    filter(!is.na(modeValue)) %>% 
    select(-N_measurements_per_trait_value) %>% 
    distinct()
  
  # add the cleaned mode vals to the df with single mode values
  modeVals_cleaned <- bind_rows(nondup_modes, multimodes_cleaned) %>% 
    select(-N_measurements_per_trait_value)
  
  # check that each sp x trait has one mode value
  modeVals_cleaned %>% 
    group_by(Species, TraitName) %>% 
    tally() %>% 
    arrange(desc(n)) # good
  
  # export
  # write.csv(modeVals_cleaned, file="data/processed/Mode trait values plus info - fac traits.csv", row.names = F) # export

  # convert to wide dimensions; add order to factor levels
  modeVals_cleaned_wide <- modeVals_cleaned %>% 
    select(-N_measurements_per_trait) %>% 
    spread(key=TraitName, value=modeValue)
  names(modeVals_cleaned_wide)
  levels(as.factor(modeVals_cleaned_wide$`Species tolerance to shade`))
  
  modeVals_cleaned_wide <- modeVals_cleaned_wide %>% 
    mutate(`Plant growth rate` = factor(`Plant growth rate`, levels=c("slow", "moderate", "fast"), ordered=T),
           `Species tolerance to drought` = factor(`Species tolerance to drought`, levels=c("none", "low", "medium", "high"), ordered=T),
           `Species tolerance to fire` = factor(`Species tolerance to fire`, levels=c("low", "medium", "high"), ordered=T),
           `Species tolerance to salt` = factor(`Species tolerance to salt`, levels=c("low", "medium", "high"), ordered=T),
           `Species tolerance to shade` = factor(`Species tolerance to shade`, levels=c("low", "medium", "high"), ordered=T)
           )
  
  head(modeVals_cleaned_wide)
  # export
  # write.csv(modeVals_cleaned_wide, file="data/processed/Mode Trait Values - fac traits.csv", row.names = F)
  
}

# join cscore and trait dfs --------------------------------------------
{
  cscore <- read.csv("data/base/Cscores_final.csv", na.strings = c("NA", "")) %>% distinct()
  
  # which sp show duplicate cscores?
  cscore %>% 
    group_by(Region, Name) %>% 
    tally() %>% 
    filter(n>1) -> dupsp
  
  # these are sp w/ 2 diff names prior to Suneeti's taxonomic review. (previously distinct sp that have since been lumped)
  # delete these 
  left_join(select(dupsp, -n),
            cscore, by=c("Region", "Name")) %>% 
    arrange(Region, Name) 
  
  cscore <- anti_join(cscore,
            select(dupsp, -n),
            by=c("Region", "Name")) 
  
  
  # join cscores and num traits
  num_trait_df <- left_join(cscore, 
            meantraitvalues_num_clean,
            by=c("Name" = "Species"))
    
  # join cscores and fac traits
  fac_trait_df <- left_join(cscore, 
            modeVals_cleaned_wide,
            by=c("Name" = "Species"))
  
  # rename traits for clarity
  num_trait_df <- num_trait_df %>% 
    select(Region, Name, Cscore, everything()) %>% 
    rename(., 
           "Species"="Name", 
           "SLA_no_pet" = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
           "SLA_with_pet" = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included",
           "LDMC" = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
           "LeafN" = "Leaf nitrogen (N) content per leaf dry mass",
           "Veg_height" = "Plant height vegetative" ,
           "Plant_lifespan_yrs" = "Plant lifespan (years)",
           "Seed_dry_mass" = "Seed dry mass",
           "Germ_rate" = "Seed germination rate (germination efficiency)",
           "Drought_tol_Mpa" = "Species tolerance to drought - Mpa")
  
  fac_trait_df <- fac_trait_df %>% 
    select(Region, Name, Cscore, everything()) %>% 
    rename(., 
           "Species"="Name", 
           "Dispersal_syndrome" = "Dispersal syndrome",
           "Dispersal_unit_type" = "Dispersal unit type",
           "Photo_pathway" = "Leaf photosynthesis pathway",
           "Growth_form" = "Plant growth form",
           "Growth_rate" = "Plant growth rate" ,
           "Plant_lifespan_fac" = "Plant lifespan (longevity)",
           "Propagation_type" = "Plant propagation type",
           "Resprout" = "Plant resprouting capacity",
           "Woodiness" = "Plant woodiness",
           "Drought_tol" = "Species tolerance to drought",
           "Fire_tol" = "Species tolerance to fire",
           "Salt_tol" = "Species tolerance to salt",
           "Shade_tol" = "Species tolerance to shade")

  
}

# check for outliers in numerical traits (sp means) --------------------
{
  head(num_trait_df)
  num_trait_df %>% 
    select(-Region, -Cscore) %>% 
    distinct() %>%  # make sure only one record per sp
    gather(., "Trait", "TraitValue", 2:ncol(.)) %>% 
    filter(!is.na(TraitValue)) %>% 
    ggplot(., aes(x=TraitValue, fill=Trait)) +
      geom_histogram() +
      facet_wrap(~Trait, scales="free") +
      theme(legend.position="none")
  # looks like seed_dry_mass and veg_height might need some cleaning
  
  num_trait_df %>% 
    select(-Region, -Cscore) %>% 
    distinct() %>% # make sure we are only getting one set of traits per sp
    gather(., key="Trait", value="TraitValue", 2:ncol(.)) %>% 
    filter(!is.na(TraitValue)) %>% 
    filter(Trait=="Seed_dry_mass" | Trait=="Veg_height") %>% 
    ggplot(., aes(x=TraitValue, fill=Trait)) +
    geom_histogram(bins=100) +
    facet_wrap(~Trait, scales="free") +
    theme(legend.position="none")
  
  # seed_dry_mass outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(Seed_dry_mass)) %>% 
      distinct(Species,Seed_dry_mass) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,Seed_dry_mass) %>% 
      arrange(desc(Seed_dry_mass)) %>% 
      head(20) # these are sp with the largest seed_dry_mass values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,Seed_dry_mass) %>% 
      filter(!is.na(Seed_dry_mass)) %>% 
      summarize(upper3sigma = mean(Seed_dry_mass) + (3*sd(Seed_dry_mass)),
                lower3sigma = mean(Seed_dry_mass) - (3*sd(Seed_dry_mass)),
                
                upper2sigma = mean(Seed_dry_mass) + (2*sd(Seed_dry_mass)),
                lower2sigma = mean(Seed_dry_mass) - (2*sd(Seed_dry_mass)))
    
    # 26 sp have values > 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Seed_dry_mass) %>% 
      filter(!is.na(Seed_dry_mass)) %>% 
      filter(Seed_dry_mass > (mean(Seed_dry_mass) + (2*sd(Seed_dry_mass)))) %>% 
      arrange(desc(Seed_dry_mass)) 
    
    # 20 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Seed_dry_mass) %>% 
      filter(!is.na(Seed_dry_mass)) %>% 
      filter(Seed_dry_mass > (mean(Seed_dry_mass) + (3*sd(Seed_dry_mass)))) %>% 
      arrange(desc(Seed_dry_mass)) 
    
    }
  
  # Veg_height outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(Veg_height)) %>% 
      distinct(Species,Veg_height) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,Veg_height) %>% 
      arrange(desc(Veg_height)) %>% 
      head(20) # these are sp with the largest Veg_height values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,Veg_height) %>% 
      filter(!is.na(Veg_height)) %>% 
      summarize(upper3sigma = mean(Veg_height) + (3*sd(Veg_height)),
                lower3sigma = mean(Veg_height) - (3*sd(Veg_height)),
                
                upper2sigma = mean(Veg_height) + (2*sd(Veg_height)),
                lower2sigma = mean(Veg_height) - (2*sd(Veg_height)))
    
    # 33 sp have values > 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Veg_height) %>% 
      filter(!is.na(Veg_height)) %>% 
      filter(Veg_height > (mean(Veg_height) + (2*sd(Veg_height)))) %>% 
      arrange(desc(Veg_height)) 
    
    # 10 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Veg_height) %>% 
      filter(!is.na(Veg_height)) %>% 
      filter(Veg_height > (mean(Veg_height) + (3*sd(Veg_height)))) %>% 
      arrange(desc(Veg_height)) 
    
  }
  
  # Plant_lifespan_yrs outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(Plant_lifespan_yrs)) %>% 
      distinct(Species,Plant_lifespan_yrs) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,Plant_lifespan_yrs) %>% 
      arrange(desc(Plant_lifespan_yrs)) %>% 
      head(20) # these are sp with the largest Plant_lifespan_yrs values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,Plant_lifespan_yrs) %>% 
      filter(!is.na(Plant_lifespan_yrs)) %>% 
      summarize(upper3sigma = mean(Plant_lifespan_yrs) + (3*sd(Plant_lifespan_yrs)),
                lower3sigma = mean(Plant_lifespan_yrs) - (3*sd(Plant_lifespan_yrs)),
                
                upper2sigma = mean(Plant_lifespan_yrs) + (2*sd(Plant_lifespan_yrs)),
                lower2sigma = mean(Plant_lifespan_yrs) - (2*sd(Plant_lifespan_yrs)))
    
    # 8 sp have values > 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Plant_lifespan_yrs) %>% 
      filter(!is.na(Plant_lifespan_yrs)) %>% 
      filter(Plant_lifespan_yrs > (mean(Plant_lifespan_yrs) + (2*sd(Plant_lifespan_yrs)))) %>% 
      arrange(desc(Plant_lifespan_yrs)) 
    
    # 5 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Plant_lifespan_yrs) %>% 
      filter(!is.na(Plant_lifespan_yrs)) %>% 
      filter(Plant_lifespan_yrs > (mean(Plant_lifespan_yrs) + (3*sd(Plant_lifespan_yrs)))) %>% 
      arrange(desc(Plant_lifespan_yrs)) 
    
  }
  
  # Plant_lifespan_yrs outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(SLA_no_pet)) %>% 
      distinct(Species,SLA_no_pet) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,SLA_no_pet) %>% 
      arrange(desc(SLA_no_pet)) %>% 
      head(20) # these are sp with the largest SLA_no_pet values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_no_pet) %>% 
      filter(!is.na(SLA_no_pet)) %>% 
      summarize(upper3sigma = mean(SLA_no_pet) + (3*sd(SLA_no_pet)),
                lower3sigma = mean(SLA_no_pet) - (3*sd(SLA_no_pet)),
                
                upper2sigma = mean(SLA_no_pet) + (2*sd(SLA_no_pet)),
                lower2sigma = mean(SLA_no_pet) - (2*sd(SLA_no_pet)))
    
    # 14 sp have values > 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_no_pet) %>% 
      filter(!is.na(SLA_no_pet)) %>% 
      filter(SLA_no_pet > (mean(SLA_no_pet) + (2*sd(SLA_no_pet)))) %>% 
      arrange(desc(SLA_no_pet)) 
    
    # 3 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_no_pet) %>% 
      filter(!is.na(SLA_no_pet)) %>% 
      filter(SLA_no_pet > (mean(SLA_no_pet) + (3*sd(SLA_no_pet)))) %>% 
      arrange(desc(SLA_no_pet)) 
    
  }
  
  # SLA_with_pet outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(SLA_with_pet)) %>% 
      distinct(Species,SLA_with_pet) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,SLA_with_pet) %>% 
      arrange(desc(SLA_with_pet)) %>% 
      head(20) # these are sp with the largest SLA_with_pet values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_with_pet) %>% 
      filter(!is.na(SLA_with_pet)) %>% 
      summarize(upper3sigma = mean(SLA_with_pet) + (3*sd(SLA_with_pet)),
                lower3sigma = mean(SLA_with_pet) - (3*sd(SLA_with_pet)),
                
                upper2sigma = mean(SLA_with_pet) + (2*sd(SLA_with_pet)),
                lower2sigma = mean(SLA_with_pet) - (2*sd(SLA_with_pet)))
    
    # 9 sp have values > 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_with_pet) %>% 
      filter(!is.na(SLA_with_pet)) %>% 
      filter(SLA_with_pet > (mean(SLA_with_pet) + (2*sd(SLA_with_pet)))) %>% 
      arrange(desc(SLA_with_pet)) 
    
    # 9 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,SLA_with_pet) %>% 
      filter(!is.na(SLA_with_pet)) %>% 
      filter(SLA_with_pet > (mean(SLA_with_pet) + (3*sd(SLA_with_pet)))) %>% 
      arrange(desc(SLA_with_pet)) 
    
  }
  
  # Germ_rate outliers; have not removed any
  {
    num_trait_df %>% 
      filter(!is.na(Germ_rate)) %>% 
      distinct(Species,Germ_rate) %>% nrow()
    
    num_trait_df %>% 
      distinct(Species,Germ_rate) %>% 
      arrange(Germ_rate) %>% 
      head(20) # these are sp with the smallest Germ_rate values
    
    # calculate upper and lower limits for 2 and 3 sd (ie 2 and 3 sigma)
    num_trait_df %>% 
      distinct(Species,Germ_rate) %>% 
      filter(!is.na(Germ_rate)) %>% 
      summarize(upper3sigma = mean(Germ_rate) + (3*sd(Germ_rate)),
                lower3sigma = mean(Germ_rate) - (3*sd(Germ_rate)),
                
                upper2sigma = mean(Germ_rate) + (2*sd(Germ_rate)),
                lower2sigma = mean(Germ_rate) - (2*sd(Germ_rate)))
    
    # 13 sp have values < 2 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Germ_rate) %>% 
      filter(!is.na(Germ_rate)) %>% 
      filter(Germ_rate < (mean(Germ_rate) - (2*sd(Germ_rate)))) %>% 
      arrange(desc(Germ_rate)) 
    
    # 7 sp have values > 3 x SD (i.e. 2 sigma)
    num_trait_df %>% 
      distinct(Species,Germ_rate) %>% 
      filter(!is.na(Germ_rate)) %>% 
      filter(Germ_rate < (mean(Germ_rate) - (3*sd(Germ_rate)))) %>% 
      arrange(desc(Germ_rate)) 
    
  }
  
}

# export trait x cscore files ------------------------------------------
{
  # export
  # write.csv(num_trait_df, file="data/processed/Cscores_num_traits.csv", row.names=F)
  # write.csv(fac_trait_df, file="data/processed/Cscores_fac_traits.csv", row.names=F)
  
  # combine and export as one
  all_trait_df <- full_join(num_trait_df, 
                            fac_trait_df,
                            by=c("Region", "Species", "Cscore"))
  
  # write.csv(all_trait_df, file="data/processed/Cscores_all_traits.csv", row.names=F)
  
}

