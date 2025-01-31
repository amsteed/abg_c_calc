---
title: "Compute Stump C"
author: "Ari Murphy-Steed" 
edits: "TH"
date: "2024-07-01, last edits ams 2025-1-30"
output: html_document
---

## 0.0 Install packages -------------------
library(dplyr)
library(tidyverse)
library(here)
library(readr)
library(purrr)
  
## 0.1 Read in and clean data -----------------------
stumps_raw <- read.csv(here("rawd/STUMPS_2022_2023_ZEROES.csv"))
dim(stumps_raw)
n_distinct(stumps_raw$PLOT)

#### 0.1.1 get rid of NA columns -----------------
names(stumps_raw)
stumps_raw <- tibble(select(stumps_raw, c("MEAS_DATE", "LOC", "PLOT", "POSITION", "GENUS", "SPECIES", "DIB_CM", "DOB_CM", "LENGTH_M","DECAY.CLASS")))
names(stumps_raw)

Names <- c("MEAS_DATE", "LOC", "PLOT", "POSITION", "GENUS", "SPECIES", "DIB_CM", "DOB_CM", "LENGTH_M","DECAY_CLASS")
names(stumps_raw) <- Names
names(stumps_raw)

#check for NA's in columns
summary(stumps_raw)


## 1.0 Compute stump C --------------
#### 1.1 Compute stump volumes --------

stumps_raw$DIB_M <- stumps_raw$DIB_CM/100 #convert DIB in cm to DIB in m
stumps_raw$r_M <- stumps_raw$DIB_M/2
stumps_raw$stump_vol_m3 <- pi*(stumps_raw$r_M^2)*stumps_raw$LENGTH_M # vol (m3) = pi * r^2 * ht
head(stumps_raw$stump_vol_m3) #these are comparable to jeans

#### 1.2 Get densities ----------
##### 1.2.1 Subset out NA's first to separate dataframe ------
stumps_raw$DECAY_CLASS[stumps_raw$DECAY_CLASS == 0] <- NA #replace 0 values with NA

stumps_zeroes <- stumps_raw[is.na(stumps_raw$DECAY_CLASS), ]
stumps <- stumps_raw[!is.na(stumps_raw$DECAY_CLASS), ]
unique(stumps$DECAY_CLASS)



#### Function to pull densities given decay class and species. Copied from CWD , same densities -----

stumps$SPEC_GRAV <- NA #create empty column 

stumps <- stumps %>% mutate(SPEC_GRAV = case_when(
  GENUS == "THUJ" ~ list(Cw = c(460,330,290,190,110)),
  GENUS == "UNKN" ~ list(Uc = c(414,345,276,207,138)),
  GENUS == "TSUG" ~ list(Hw = c(460,420,330,250,185)), 
  GENUS == "PSEU" ~ list(Fd = c(414,345,276,207,138)),
  GENUS == "ABIE" ~ list(Fd = c(444,361,285,200,121)),
  GENUS == "PICE" ~ list(Sx = c(414,345,276,207,138)),
  TRUE ~ SPEC_GRAV
))


specificGravityFunction <- function () {
  
  stumps$FAC <- NA
  
  for (i in 1:nrow(stumps)) {
    
    decay_class_num <- stumps$DECAY_CLASS[[i]]
    
    wood_density <- stumps$SPEC_GRAV[[i]][[decay_class_num]]
    
    stumps$FAC[[i]] <- wood_density
    
  }
  
  stumps$FAC
  
}


stumps$FAC <- specificGravityFunction()  #column with specific gravity factors 
head(stumps) 

#### 1.3 Compute biomass in kg ---------------
stumps$stump_biomass_kg <- stumps$FAC*stumps$stump_vol_m3
print(stumps$stump_biomass_kg) #checked, these seem reasonable based on jeans sheet 

#### 1.4 Compute C in Mg/ha -----------
stumps$stump_C_Mgha <- (stumps$stump_biomass_kg*0.5*200/1000)

## 2.0 Compute C in stump roots -----------
stumps$ln_rt_bio <- -4.38 + 2.41* log(stumps$DIB_CM) #ln of root biomass

stumps$stump_root_biomass <- exp(stumps$ln_rt_bio) #root bio in kg - according to jean. seems good based on her sheet but descrip says to raise it to negative 1* biomass which isnt possible.... 

print(stumps[1,])

stumps$stump_root_C_Mgha <- stumps$stump_root_biomass*0.5*200/1000

## 3.0 Total Stump C and Stump Root C per subplot -------------
#### 3.1 Add subplots with zeroes (NAs) back in --------
stumps_c <- tibble(select(stumps, c("PLOT", "POSITION", "DECAY_CLASS", "stump_C_Mgha" ,"stump_root_C_Mgha"))) 
stumps_zeroes <- tibble(select(stumps_zeroes, c("PLOT", "POSITION", "DECAY_CLASS")))

#### 3.2 Add in zero values for stumps in those subplots ------
stumps_zeroes$stump_C_Mgha <- 0
stumps_zeroes$stump_root_C_Mgha <- 0

#### 3.3 Join zeroes and non zeroes by plot ------------
stumps_all <- rbind(stumps_c, stumps_zeroes)

dim(stumps_all) #should have 105 rows.... 
summary(stumps_all)
head(stumps_all)

#### 3.4 Total C per subplot -------------

# Write function to generate dfs --------------

library(reshape2)

df_function <- function(df, var, sub1, sub2, df_names, func){
  
  output_df <- with(df, tapply(var, list(sub1, sub2), func))
  
  output_df2 <- melt(output_df)
  
  names_df <- df_names
  
  names(output_df2) <- names_df
  
  return(output_df2)
  
}

## Call function to generate dfs with components of stump C by plot and position ------
names(stumps_all)

stump_CMgha_df <- df_function(df = stumps_all, 
                       stumps_all$stump_C_Mgha, stumps_all$PLOT, stumps_all$POSITION, 
                       df_names = list("PLOT", "POSITION", "stump_C_Mgha"),
                       func = sum)

stump_root_CMgha_df <- df_function(df = stumps_all, 
                             stumps_all$stump_root_C_Mgha, stumps_all$PLOT, stumps_all$POSITION, 
                             df_names = list("PLOT", "POSITION", "stump_root_C_Mgha"),
                             func = sum)

head(stump_root_CMgha_df)
head(stump_CMgha_df)

## Merge dfs by plot -------
stump_c_plot <- merge(stump_CMgha_df, stump_root_CMgha_df, by = c("PLOT", "POSITION"))
head(stump_c_plot)

stump_c_plot$total_stump_c <- stump_c_plot$stump_C_Mgha + stump_c_plot$stump_root_C_Mgha

stump_c_plot <- stump_c_plot %>%
  group_by(PLOT) %>%
  filter(!is.na(total_stump_c)) %>%
  summarize(plot_stump_c_Mgha = mean(total_stump_c))


head(stump_c_plot)
dim(stump_c_plot)


## 5.0 Save final table as .rds --------------
saveRDS(stump_c_plot, file = "stump_c_plot.rds")

