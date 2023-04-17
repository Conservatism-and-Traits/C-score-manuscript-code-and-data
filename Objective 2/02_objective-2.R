#
# Title: Objective 2: Can C-scores be transferred across geographic regions using functional traits?
# Created: December 21st, 2020
# Last Updated: March 10th, 2023
# Authors: Brandon Allen
# Objective: Explore methods for creating a multi-class classification model (e.g., Random Forest)
# Keywords: Notes, Data standardization, Random Forest Model, Visualizations
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# C-score definitions (Floristic quality assessment index (FQAI) for vascular plants and mosses for the State of Ohio; 2004)
# 0: Plants with a wide range of ecological tolerances. Often these are opportunistic invaders of natural areas 
# (e.g. Phragmites australis, Phalaris arundinacea) or native taxa that are typically part of a ruderal community
# (e.g. Polygonum pensylvanicum, Ambrosia artemisiifolia)
#
# 1-2: Widespread taxa that are not typical of (or only marginally typical of) a particular community like
# Solidago canadensis or Impatiens capensis
#
# 3-5: Plants with an intermediate range of ecological tolerances that typify a stable phase of some
# native community, but persist under some disturbance (Asclepias incarnata, Ulmus rubra, Spartina pectinata)
#
# 6-8: Plants with a narrow range of ecological tolerances that typify a stable or near "climax" community 
# (e.g. Goodyera pubescens, Veronicastrum virginicum, Cephalanthus occidentalis)
#
# 9-10: Plants with a narrow range of ecological tolerances that exhibit relatively high degrees of fidelity to a 
# narrow range of habitat requirements (e.g. Potamogeton robbinsii, Cypripedium candidum)
#
#
########################
# Data standardization # 
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load relevant libraries
library(caret)
library(e1071)
library(MASS)
library(randomForest)
library(reshape2)
library(scales)
library(VIM)

# Load trait data
trait.data <- read.csv("data/processed/Cscores_all_traits.csv")

# Subset the traits based on our candidate sets
trait.data <- trait.data[, c("Region", "Species", "Cscore", 
                             "Dispersal_unit_type", "Growth_rate", "Propagation_type", 
                             "Plant_lifespan_fac","LeafN", "SLA_with_pet", "Germ_rate")]

# Correct names
colnames(trait.data) <- c("Region", "Species", "Cscore","DispersalUnit", "GrowthRate", "PropagationType",
                          "Lifespan", "LeafNitrogen", "SLA", "GerminationRate")

# Check completeness of traits
apply(trait.data, MARGIN = 2, function(x) table(!is.na(x))) 

# There are only a small number of species with the PropagationType trait. This will be excluded from the analysis.
trait.data <- trait.data[, colnames(trait.data) != "PropagationType"]

# Remove observations with leaf nitrogen outside 3SD
threshold <- sd(trait.data$LeafNitrogen, na.rm = TRUE) * 3
trait.data <- trait.data[!(!is.na(trait.data$LeafNitrogen) & trait.data$LeafNitrogen > mean(trait.data$LeafNitrogen, na.rm = TRUE) + threshold), ]
trait.data <- trait.data[!(!is.na(trait.data$LeafNitrogen) & trait.data$LeafNitrogen < mean(trait.data$LeafNitrogen, na.rm = TRUE) - threshold), ]

# Check data cleaning steps
str(trait.data)
apply(trait.data[, 3:9], MARGIN = 2, FUN = table) 

# Remove non-native species C-score == 0
# This is done because non-native status can change between states and predicting if a species is non-native is a different question
trait.data <- trait.data[trait.data$Cscore != 0, ]

# Factor the cscore
trait.data$Cscore <- factor(trait.data$Cscore)

# There are a few species with naming discrepancies (spaces at the end of names)
# "e.g., Acer negundo " vs "Acer negundo"

name.updates <- NULL

for(row.id in rownames(trait.data)) {
  
  species <- trait.data[row.id, "Species"]
  
  if(substr(species, start = nchar(species), stop = nchar(species)) == " ") {
    
    species <- substr(species, start = 1, stop = nchar(species)-1)
    trait.data[row.id, "Species"] <- species
    
    name.updates <- c(name.updates, species)
    
  }
  
}

# Reassign missing trait values for those species with name changes
for (species in name.updates) {
  
  # Isolate single species
  species.temp <- trait.data[trait.data$Species == species, ]
  
  # Extract values if present for each trait
  for (trait in colnames(species.temp)[4:9]) {
    
    replace.values <- species.temp[, trait][!is.na(species.temp[, trait])]
    
    species.temp[, trait] <- replace.values[1]
    
  }
  
  # Replace values 
  trait.data[rownames(species.temp), ] <- species.temp
  
  rm(trait, species.temp)
  
}

# Convert long form of C-scores to individual species results (e.g., Species, Dakota, Florida, Illinois... )
trait.short.form <- NULL

for (species in unique(trait.data$Species)) {
  
  # Isolate single species
  species.temp <- trait.data[trait.data$Species == species, ]
  
  # If region does not exist, create fake rows
  missing.regions <- unique(trait.data$Region)[!(unique(trait.data$Region) %in% species.temp$Region)]
  
  for(regions in missing.regions) {
    
    species.missing <- data.frame(Region = regions,
                                  Species = species,
                                  Cscore = NA,
                                  DispersalUnit = species.temp[1, "DispersalUnit"],
                                  GrowthRate = species.temp[1, "GrowthRate"],
                                  Lifespan = species.temp[1, "Lifespan"],
                                  LeafNitrogen = species.temp[1, "LeafNitrogen"],
                                  SLA = species.temp[1, "SLA"],
                                  GerminationRate = species.temp[1, "GerminationRate"])
    
    species.temp <- rbind.data.frame(species.temp, species.missing)
    
    rm(species.missing)
    
  }
  
  # Create shortform
  species.temp <- data.frame(Species = species,
                             Dakota = species.temp[species.temp$Region == "Dakotas", "Cscore"],
                             Illinois = species.temp[species.temp$Region == "IL", "Cscore"],
                             Kansas = species.temp[species.temp$Region == "KS", "Cscore"],
                             NewYork = species.temp[species.temp$Region == "NY", "Cscore"],
                             Florida = species.temp[species.temp$Region == "SouthFL", "Cscore"],
                             DispersalUnit = species.temp[1, "DispersalUnit"],
                             GrowthRate = species.temp[1, "GrowthRate"],
                             Lifespan = species.temp[1, "Lifespan"],
                             LeafNitrogen = species.temp[1, "LeafNitrogen"],
                             SLA = species.temp[1, "SLA"],
                             GerminationRate = species.temp[1, "GerminationRate"])
  
  trait.short.form <- rbind.data.frame(trait.short.form, species.temp)
  
  rm(species.temp, missing.regions)
  
}

rm(trait.data, species, name.updates, row.id, replace.values, regions)

# Some of the traits did not get properly assigned in this file. Assign missing values from the Cscores_traits.csv file
missing.traits <- read.csv("data/processed/Cscores_traits.csv")
missing.traits <- missing.traits[, c("ScientificName", "Dispersal_unit_type", "Plant_growth_rate", 
                                     "Plant_lifespan", "leafN_drymass", 
                                     "SLA_plus_petiole_and_rachis", "Seed_germ_rate")]

colnames(missing.traits) <- c("Species", "DispersalUnit", "GrowthRate", 
                              "Lifespan", "LeafNitrogen", 
                              "SLA", "GerminationRate")

for (species in missing.traits$Species) {
  
  trait.short.form[trait.short.form$Species == species, c("DispersalUnit", "GrowthRate", 
                                                          "Lifespan", "LeafNitrogen", 
                                                          "SLA", "GerminationRate")] <- missing.traits[missing.traits$Species == species, c("DispersalUnit", "GrowthRate", 
                                                                                                                                            "Lifespan", "LeafNitrogen", 
                                                                                                                                            "SLA", "GerminationRate")]
  
}

rm(missing.traits, species)

# Set blank values to NA for the factors
trait.short.form$DispersalUnit[!is.na(trait.short.form$DispersalUnit) & trait.short.form$DispersalUnit == ""] <- NA
trait.short.form$GrowthRate[!is.na(trait.short.form$GrowthRate) & trait.short.form$GrowthRate == ""] <- NA

# Combine Annual and Biennial
trait.short.form$Lifespan[!is.na(trait.short.form$Lifespan) & trait.short.form$Lifespan == "biennial"] <- "annual"

# Remove the few species with vegetative dispersal unit
removal.logical <- trait.short.form$DispersalUnit != "vegetative"
removal.logical[is.na(removal.logical)] <- TRUE
trait.short.form <- trait.short.form[removal.logical, ]

# Convert the traits to factors
trait.short.form$DispersalUnit <- as.factor(trait.short.form$DispersalUnit)
trait.short.form$GrowthRate <- as.factor(trait.short.form$GrowthRate)
trait.short.form$Lifespan <- as.factor(trait.short.form$Lifespan)

# Remove SLA
trait.short.form <- trait.short.form[, -11]

# Remove species with no traits
trait.short.form <- trait.short.form[ifelse(apply(trait.short.form[, 7:11], MARGIN = 1, function(x) sum(!is.na(x))) > 0, TRUE, FALSE), ]

########################
# Random Forest Models # 
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (region in c("Dakota", "Illinois", "Kansas", "NewYork", "Florida")) {
  
  confusion.matrix <- list()
  trait.template <- data.frame(Species = rep(seq(0.1, 1, 0.1), 5),
                               Trait = c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10),
                                         rep(5, 10)),
                               OOB = NA)
  best.model <- NULL
  
  for(tresholds in 1:nrow(trait.template)) {
    
    trait.exploration <- trait.short.form # Store the prefilterd results for the missing data exploration section
    
    # Filter the number of species
    trait.exploration <- trait.exploration[ifelse(apply(trait.exploration[, 7:11], MARGIN = 1, function(x) sum(!is.na(x))) >= trait.template[tresholds, "Trait"], TRUE, FALSE), ]
    
    # Filter the number of traits
    trait.exploration <- trait.exploration[, c(rep(TRUE, 6), ifelse(apply(trait.exploration[, 7:11], MARGIN = 2, function(x) sum(!is.na(x)) / nrow(trait.exploration)) >= trait.template[tresholds, "Species"], TRUE, FALSE))]
    
    # Fill in the missing data
    col.index <- ncol(trait.exploration)
    if (col.index <= 7) { next }
    
    trait.exploration <- kNN(data = trait.exploration, 
                             variable = colnames(trait.exploration)[7:col.index],
                             k = 10)
    
    # Set seed and create a random subset of the data (50% training, 50% validate)
    set.seed(1234)
    
    # Make sure to only use species with scores in Dakota
    region.data <- trait.exploration[!is.na(trait.exploration[, region]), ]
    
    # If the data set has fewer than 50 species, next
    if (nrow(region.data) < 50) { next }
    
    species.subset <- sample(x = 1:nrow(region.data), 
                             size = round(nrow(region.data)/2),
                             replace = FALSE)
    
    region.train <- region.data[species.subset, ]
    region.validate <- region.data[-species.subset, ]
    
    # Create barebones random forest model
    region.rf <- randomForest(y = region.train[, region],
                              x = region.train[, 7:col.index], 
                              ntree = 5000, 
                              importance = TRUE,
                              mtry = 2)
    
    confusion.matrix[[tresholds]] <- region.rf$confusion
    trait.template[tresholds, "OOB"] <- mean(region.rf$err.rate[,1])
    
    if(tresholds == 1) {
      
      best.model <- region.rf
      
    } else {
      
      if(mean(region.rf$err.rate[,1]) < min(trait.template[, "OOB"], na.rm = TRUE)) {
        
        best.model <- region.rf
        
      }
      
    }
    
    print(tresholds)
    
  }
  
  print(region)
  save(confusion.matrix, trait.template, best.model, file = paste0("results/tables/", region, "-rf-resultsv2.Rdata"))
  
}

#################
# Visualization #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(MetBrewer)
library(ggpubr)

# Create the dataset
model.output <- list.files(paste0("results/tables/"), full.names = TRUE)
model.output <- model.output[-6]

data.vis <- data.frame(Region = c(rep("Dakota", 5), rep("Illinois", 5),
                                  rep("Kansas", 5), rep("NewYork", 5), rep("Florida", 5)),
                       Trait = rep(c("Dispersal Unit", "Growth Rate",
                                     "Lifespan", "Leaf Nitrogen","Germination Rate"), 5),
                       Value = NA)

for(region in c("Dakota", "Illinois", "Kansas", "NewYork", "Florida")) {
  
  # Load data
  load(model.output[grep(region, model.output)])
  data.vis[data.vis$Region == region, "Value"] <- as.numeric(best.model$importance[, 4])
  
}

data.vis$Region <- gsub("NewYork", "New York", data.vis$Region)
data.vis$Region <- gsub("Florida", "south Florida", data.vis$Region)

# Going to make a heat map to represent this matrix
png(file = paste("results/figures/variable-contribution.png", sep = ""),
    width = 3600,
    height = 2400, 
    res = 300)

print(ggplot(data = data.vis, aes(x = Region, y = Trait, fill = Value), show.legend = FALSE) +
        geom_tile() +
        scale_fill_gradientn(name = "Mean Decrease\nAccuracy", colors = rev(met.brewer(name = "OKeeffe1", n = 15, type = "continuous"))[8:1],
                             limits = c(0,0.20),
                             breaks = c(0, 0.05, 0.1, 0.15, 0.20),
                             labels = c(0, 0.05, 0.1, 0.15, 0.20)) +
        theme_light() +
        theme(axis.title = element_text(size=20),
              axis.text.x = element_text(size=20, angle = 90, hjust = 1),
              axis.text.y = element_text(size=20),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1)))

dev.off()

# Print model results for filling in supplementary tables

for(region in c("Dakota", "Illinois", "Kansas", "NewYork", "Florida")) {
  
  # Load data
  load(model.output[grep(region, model.output)])
  print(region)
  print(mean(best.model$err.rate[, 1]))
  print(best.model$confusion)
  trait.template$OOB[is.na(trait.template$OOB)] <- 1
  print(trait.template[trait.template$OOB == min(trait.template$OOB, na.rm = TRUE), ])
  
}

rm(list=ls())
gc()