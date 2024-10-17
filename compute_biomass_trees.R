---
title: "1_compute_biomass_trees_NEW"
author: "Ari Murphy-Steed"
edits: "Ari Murphy-Steed - based on OJ excel"
date: "2024-02-16"
output: html_document
---
  #Fixing work flow -- need to apply different eqn for each species, and then apply multipliers for each biomass component 
  #equations taken from "example equations" in OJ's excel (same as Amanda's)
  #use same equations (separate them into parts, see below)
  #i.e. if full tree equation is: ((0.0113*DBH^1.9332*HEIGHT^1.1125)+(0.0019*DBH^2.3356*HEIGHT^0.6371)+ (0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HEIGHT^-0.7963))
  # wood = (0.0113*DBH^1.9332*HEIGHT^1.1125)
  # bark = (0.0019*DBH^2.3356*HEIGHT^0.6371)
  # branch = (0.0609*DBH^2.0021)
  # foliage = (0.2656*DBH^2.0107*HEIGHT^-0.7963)
  # biomass = wood + bark + branch + foliage
  
  #library(here)
library(dplyr)
library(tidyverse) 
library(data.table)
library(tidyr)
library(here)

# 1. Bring Data into R -----------------------------------------------------

#trees_clean <- read.csv (here("clean", "trees_clean.csv")) <- no, wrong file
trees_clean <- read.csv("TREE DETAILS_2022_2023_FINAL.csv")
summary(trees_clean)
unique(trees_clean$AGE_CLASS)
unique(trees_clean$PLOT)


# 2. Create new table ----------------------------------------------------------
trees_bio_table <- tibble(select(trees_clean, c("LOC", "PLOT", "AGE_CLASS", "LGTREE_GENUS", "LGTREE_STATUS", 
                                                "DBH", "REM_.BARK", "HEIGHT", "APPEARANCE", "CROWN_COND", 
                                                "BARK_RET", "WOOD_COND", "LICHEN")))


trees_bio_table$HEIGHT <- as.numeric(trees_bio_table$HEIGHT)
trees_bio_table <- trees_bio_table %>% drop_na(HEIGHT)
trees_bio_table <- trees_bio_table %>% drop_na(CROWN_COND)

summary(trees_bio_table)


#3. Subset data by genus ----------
#some can be grouped because they will all use the same biomass eqn (i.e. unknown is also used for cupr, rham, alnu)
unique(trees_bio_table$LGTREE_GENUS)
thuj <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "THUJ" | #Cw
                          trees_bio_table$LGTREE_GENUS == "CUPR", ] #Yc, treat as Cw
abie <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "ABIE", ] #Ba (using Bl like Alana does)
pseu <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "PSEU", ] 
tsug <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "TSUG", ] 
pice <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "PICE", ]


unkn <- trees_bio_table[trees_bio_table$LGTREE_GENUS == "UNKN" |  #all these use unknown sp
                          trees_bio_table$LGTREE_GENUS == "ALNU" |
                          trees_bio_table$LGTREE_GENUS == "RHAM", ]




#2. For each species, calculate each biomass component -------------------------
# ideally would store this as a bunch of functions

#2a) thuj ===== 
#full equation; 
# (0.0188*DBH^1.3376*HEIGHT^1.5293)+(0.0002*DBH^2.4369*HEIGHT^1.1315)+
# (0.0265*DBH^3.6747*HEIGHT^-1.5958)+(0.1097*DBH^1.5530)
#wood + bark + branch + foliage 

#thuj------STEM COMPONENT (kg) -------------------------------------------------
thuj$WOOD <- NA
thuj <- thuj %>% mutate(WOOD = ((0.0188*DBH^1.3376*HEIGHT^1.5293)))

#BARK COMPONENT (kg) --------

thuj$BARK <- NA

thuj <- thuj %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0061*DBH^1.8603*HEIGHT^0.7693)*1),    
  REM_.BARK <= "99"  ~ ((0.0061*DBH^1.8603*HEIGHT^0.7693)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
thuj$BRANCH <- NA

thuj <- thuj %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0),    
  CROWN_COND < "3" ~  ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*1),
  CROWN_COND == "3" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  CROWN_COND == "4" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  CROWN_COND == "5" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
thuj$FOLIAGE <- NA

thuj <- thuj %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.1097*DBH^1.5530)*0),    
  CROWN_COND < "3" ~ ((0.1097*DBH^1.5530)*1),
  CROWN_COND == "3" ~ ((0.1097*DBH^1.5530)*0.5),
  CROWN_COND == "4" ~ ((0.1097*DBH^1.5530)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
thuj <- thuj %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

thuj$C_Mgha <- (thuj$BIOMASS*(0.5*25/1000))
#OMG IT WORKED!!!!


#2b) pice===== 
#full equation (Sx): 
#(0.0133*DBH^1.3303*HT^1.6877)+(0.0086*DBH^1.6216*HT^0.8192)+
#(0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630)
#wood + bark + branch + fol

#pice ---- STEM COMPONENT (kg) -------------------------------------------------
pice$WOOD <- NA
pice <- pice %>% mutate(WOOD = ((0.0133*DBH^1.3303*HEIGHT^1.6877)))

#BARK COMPONENT (kg) --------

pice$BARK <- NA

pice <- pice %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0086*DBH^1.6216*HEIGHT^0.8192)*1),    
  REM_.BARK <= "99"  ~ ((0.0086*DBH^1.6216*HEIGHT^0.8192)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
pice$BRANCH <- NA

pice <- pice %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0428*DBH^2.7965*HEIGHT^-0.7328)*0),    
  CROWN_COND < "3" ~  ((0.0428*DBH^2.7965*HEIGHT^-0.7328)*1),
  CROWN_COND == "3" ~ ((0.0428*DBH^2.7965*HEIGHT^-0.7328)*0.5),
  CROWN_COND == "4" ~ ((0.0428*DBH^2.7965*HEIGHT^-0.7328)*0.5),
  CROWN_COND == "5" ~ ((0.0428*DBH^2.7965*HEIGHT^-0.7328)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
pice$FOLIAGE <- NA

pice <- pice %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.0854*DBH^2.4388*HEIGHT^-0.7630)*0),    
  CROWN_COND < "3" ~  ((0.0854*DBH^2.4388*HEIGHT^-0.7630)*1),
  CROWN_COND == "3" ~ ((0.0854*DBH^2.4388*HEIGHT^-0.7630)*0.5),
  CROWN_COND == "4" ~ ((0.0854*DBH^2.4388*HEIGHT^-0.7630)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
pice <- pice %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

pice$C_Mgha <- (pice$BIOMASS*(0.5*25/1000))


#2c) abie===== 
#full equation: 
#(0.0220*DBH^1.6469*HEIGHT^1.1714)+(0.0061*DBH^1.8603*HEIGHT^0.7693)+
#(0.0265*DBH^3.6747*HEIGHT^-1.5958)+(0.0509*DBH^2.9909*HEIGHT^-1.2271)
#wood + bark + branch + fol

#abie ---- STEM COMPONENT (kg) -------------------------------------------------
abie$WOOD <- NA
abie <- abie %>% mutate(WOOD = ((0.0220*DBH^1.6469*HEIGHT^1.1714)))

#BARK COMPONENT (kg) --------

abie$BARK <- NA

abie <- abie %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0061*DBH^1.8603*HEIGHT^0.7693)*1),    
  REM_.BARK <= "99"  ~ ((0.0061*DBH^1.8603*HEIGHT^0.7693)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
abie$BRANCH <- NA

abie <- abie %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0),    
  CROWN_COND < "3" ~  ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*1),
  CROWN_COND == "3" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  CROWN_COND == "4" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  CROWN_COND == "5" ~ ((0.0265*DBH^3.6747*HEIGHT^-1.5958)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
abie$FOLIAGE <- NA

abie <- abie %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.0509*DBH^2.9909*HEIGHT^-1.2271)*0),    
  CROWN_COND < "3" ~ ((0.0509*DBH^2.9909*HEIGHT^-1.2271)*1),
  CROWN_COND == "3" ~ ((0.0509*DBH^2.9909*HEIGHT^-1.2271)*0.5),
  CROWN_COND == "4" ~ ((0.0509*DBH^2.9909*HEIGHT^-1.2271)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
abie <- abie %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

abie$C_Mgha <- (abie$BIOMASS*(0.5*25/1000))


#2d) pseu===== 
#full equation: 
#(0.0191*DBH^1.5365*HEIGHT^1.3634)+(0.0083*DBH^2.4811)+
#(0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HEIGHT^-0.4744))
#wood + bark + branch + fol

#pseu ---- STEM COMPONENT (kg) -------------------------------------------------
pseu$WOOD <- NA
pseu <- pseu %>% mutate(WOOD = ((0.0191*DBH^1.5365*HEIGHT^1.3634)))

#BARK COMPONENT (kg) --------

pseu$BARK <- NA

pseu <- pseu %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0083*DBH^2.4811)*1),    
  REM_.BARK <= "99"  ~ ((0.0083*DBH^2.4811)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
pseu$BRANCH <- NA

pseu <- pseu %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0351*DBH^2.2421)*0),    
  CROWN_COND < "3" ~  ((0.0351*DBH^2.2421)*1),
  CROWN_COND == "3" ~ ((0.0351*DBH^2.2421)*0.5),
  CROWN_COND == "4" ~ ((0.0351*DBH^2.2421)*0.5),
  CROWN_COND == "5" ~ ((0.0351*DBH^2.2421)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
pseu$FOLIAGE <- NA

pseu <- pseu %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.0718*DBH^2.2935*HEIGHT^-0.4744)*0),    
  CROWN_COND < "3" ~ ((0.0718*DBH^2.2935*HEIGHT^-0.4744)*1),
  CROWN_COND == "3" ~ ((0.0718*DBH^2.2935*HEIGHT^-0.4744)*0.5),
  CROWN_COND == "4" ~ ((0.0718*DBH^2.2935*HEIGHT^-0.4744)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
pseu <- pseu %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

pseu$C_Mgha <- (pseu$BIOMASS*(0.5*25/1000))



#2e) tsug ===== 
#full equation: 
#(0.0113*DBH^1.9332*HEIGHT^1.1125)+(0.0019*DBH^2.3356*HEIGHT^0.6371)+
#(0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HEIGHT^-0.7963)
#wood + bark + branch + fol

#tsug ---- STEM COMPONENT (kg) -------------------------------------------------
tsug$WOOD <- NA
tsug <- tsug %>% mutate(WOOD = ((0.0113*DBH^1.9332*HEIGHT^1.1125)))

#BARK COMPONENT (kg) --------

tsug$BARK <- NA

tsug <- tsug %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0019*DBH^2.3356*HEIGHT^0.6371)*1),    
  REM_.BARK <= "99"  ~ ((0.0019*DBH^2.3356*HEIGHT^0.6371)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
tsug$BRANCH <- NA

tsug <- tsug %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0609*DBH^2.0021)*0),    
  CROWN_COND < "3" ~  ((0.0609*DBH^2.0021)*1),
  CROWN_COND == "3" ~ ((0.0609*DBH^2.0021)*0.5),
  CROWN_COND == "4" ~ ((0.0609*DBH^2.0021)*0.5),
  CROWN_COND == "5" ~ ((0.0609*DBH^2.0021)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
tsug$FOLIAGE <- NA

tsug <- tsug %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.2656*DBH^2.0107*HEIGHT^-0.7963)*0),    
  CROWN_COND < "3" ~ ((0.2656*DBH^2.0107*HEIGHT^-0.7963)*1),
  CROWN_COND == "3" ~ ((0.2656*DBH^2.0107*HEIGHT^-0.7963)*0.5),
  CROWN_COND == "4" ~ ((0.2656*DBH^2.0107*HEIGHT^-0.7963)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
tsug <- tsug %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

tsug$C_Mgha <- (tsug$BIOMASS*(0.5*25/1000))





#2f) unkn ===== 
#full equation: 
#(0.0276*DBH^1.6868*HEIGHT^1.0953)+(0.0101*DBH^1.8486*HEIGHT^0.5525)+
#(0.0313*DBH^2.9974*HEIGHT^-1.0383)+(0.1379*DBH^2.3981*HEIGHT^-1.0418)
#wood + bark + branch + fol

#unkn ---- STEM COMPONENT (kg) -------------------------------------------------
unkn$WOOD <- NA
unkn <- unkn %>% mutate(WOOD = ((0.0276*DBH^1.6868*HEIGHT^1.0953)))

#BARK COMPONENT (kg) --------

unkn$BARK <- NA

unkn <- unkn %>% mutate(BARK = case_when( 
  
  REM_.BARK == "100" ~ ((0.0101*DBH^1.8486*HEIGHT^0.5525)*1),    
  REM_.BARK <= "99"  ~ ((0.0101*DBH^1.8486*HEIGHT^0.5525)*(REM_.BARK/100)),
  
  TRUE ~ BARK))

#BRANCH COMPONENT (kg) ------
unkn$BRANCH <- NA

unkn <- unkn %>% mutate(BRANCH= case_when(  
  
  APPEARANCE > "5" ~  ((0.0313*DBH^2.9974*HEIGHT^-1.0383)*0),    
  CROWN_COND < "3" ~  ((0.0313*DBH^2.9974*HEIGHT^-1.0383)*1),
  CROWN_COND == "3" ~ ((0.0313*DBH^2.9974*HEIGHT^-1.0383)*0.5),
  CROWN_COND == "4" ~ ((0.0313*DBH^2.9974*HEIGHT^-1.0383)*0.5),
  CROWN_COND == "5" ~ ((0.0313*DBH^2.9974*HEIGHT^-1.0383)*0.5),
  
  TRUE ~ BRANCH))

#FOLIAGE COMPONENT (kg) ------
unkn$FOLIAGE <- NA

unkn <- unkn %>% mutate(FOLIAGE = case_when( 
  
  CROWN_COND > "4" ~  ((0.1379*DBH^2.3981*HEIGHT^-1.0418)*0),    
  CROWN_COND < "3" ~ ((0.1379*DBH^2.3981*HEIGHT^-1.0418)*1),
  CROWN_COND == "3" ~ ((0.1379*DBH^2.3981*HEIGHT^-1.0418)*0.5),
  CROWN_COND == "4" ~ ((0.1379*DBH^2.3981*HEIGHT^-1.0418)*0.5),
  
  TRUE ~ FOLIAGE))


#TOTAL BIOMASS PER TREE (kg) ------ 
unkn <- unkn %>% mutate(BIOMASS = WOOD + BARK + BRANCH + FOLIAGE)

unkn$C_Mgha <- (unkn$BIOMASS*(0.5*25/1000))


#3. Merge species subsets into one table ------
unique(trees_clean$LGTREE_GENUS)
summary(trees_bio_table)
trees_biomass <- rbind(abie, pice, pseu, thuj, tsug, unkn)
summary(trees_biomass) #same length, good. has one NA?? why?

# 2.1 missing obs for wa_9 , insert zero value ---
dim(trees_biomass)
Names <- names(trees_biomass)
wa_9 <- data.frame("WA", "WA_9", "0-10.1", "NA", "NA", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
dim(wa_9)
names(wa_9) <- Names

trees_biomass <- rbind(trees_biomass, wa_9)

#4. Export ---

write.csv(trees_biomass, file = here("trees_biomass_2024-04-03.csv"), row.names = FALSE)



