###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######           INDIA - CHILD IMMUNIZATION            ####### 
#######                                                 ####### 
#######         CODE: RETRIEVING THE DHS DATA           #######
###############################################################
###############################################################


## Contents:
## 1. Retrieve the DHS data 
##
##


## NOTE: make sure vaccination is assessed for appropriate age groups only
## DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm

rm(list =ls())
library(dplyr)
library(foreign)
library(tidyverse)
library(data.table)
library(zoo)
library(psych)
library(lubridate)
library(MAPLES)
#library("readxl")
library(rdhs) 
options(scipen=999)
options(digits=5)


setwd("C:/Users/annak/Dropbox/Projects/2023_San Diego/Francois_India_vaccine/data/")

################################################################################
## 1. Retrieve the DHS data 
################################################################################
## Set up your DHS credentials (password: barabani122!)
set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health")


set_rdhs_config(email = "anna.k.dimitrova@gmail.com",
                project = "Climate shocks and childhood health",
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

## Make a list of surveys and download them
surveys <- dhs_datasets() %>% 
  dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode") %>% 
  dplyr::filter(CountryName == "India") %>% 
  dplyr::filter(SurveyYear > 2016)
  


downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
print(downloads)
#vec <- unlist(downloads)
#vec  

## Select relevant variables
vars = c(
  #maternal & household characteristics
  "caseid", "bidx", "midx", "v001", "v002", "v003", "v005", "v006", "v007", "v008","v008a",
  "v012", "v016", "v017", "v021", "v023", "v024", "v025", "v040", "v104", "v106","v113",
  "v115", "v116", "v133", "v134", "v135", "v137", "v139", "v140", "v149", "v151", "v152", 
  "v155", "v157", "v158", "v159", "v160",  "v190", "v191", "v438", "v445", "v467d", "v467b", 
  "v465","v502", "v632", "v704", "v705", "v716", "v717", "v208", "v501",
  #child characteristics and anthropometric measures
  "bidx", "bord", "b0", "b1", "b2" ,"b3", "b4", "b5", "b8", "b9", "b11", "b17", "b18", "b19", "b20",
  "hw70", "hw71", "hw72", "hw73", "hw1", "hw2", "hw3", "hw13", "hw16", "hw17", "hw18", "hw19", 
  #vaccines
  "h1", "h0", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h51", "h52", "h53", "h54", "h55", "h56", "h57", "h58", 
  #variables for recalculating the wealth index
  "v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b"
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

## Extract the data (adding geographical covariates: add_geo = TRUE)
extract <- extract_dhs(questions, add_geo = T)

## Quick check 
head(extract[1])

data_extract <- rbindlist(extract, use.names=TRUE, fill=TRUE)

## Keep only observations with valid GPS coordinates
data_extract <- data_extract %>% 
  filter(!is.na(LATNUM)) %>% 
  filter(!is.na(LONGNUM)) %>% 
  filter(LATNUM!=0 | LONGNUM!=0)  #LAT=0 and LONG=0 are missing coordinates  

unique(df0$SurveyId)

rm(list=setdiff(ls(), c("data_extract", "questions")))

save.image(file='DHS_extract.RData')

################################################################################
## 2. Harmonize the survey data 
################################################################################

load(file='DHS_extract.RData')

data_children_1to3yrs <- data_extract %>% 
  filter(b5 == "yes") %>% #keep observation only if child is alive
  dplyr::rename(psu           = v001, 
                hh.id         = v002,
                woman.id      = v003,
                intYr         = v007, #year of interview
                intMo         = v006, #month of interview
                intDay        = v016, #day of interview
                intCMC        = v008, #interview date in CMC format (century month code)
                birthYr       = b2,
                birthMo       = b1,
                birthCMC      = b3,
                wealth        = v190,
                age           = b8,   #age of the child measured in years 
                age.months    = b19,
                sex           = b4,
                birth.int     = b11,  #preceding birth interval - only for 2nd and higher order births
                children.u5   = v137, #number of children under 5 in the household
                births        = v208, #number of births in the last 5 years
                age.mother    = v012,
                edu.level     = v106,
                edu.years     = v133,
                hh.head       = v151,
                hh.size       = v136, 
                #region        = v024,
                residence     = v025,
                stratum       = v023,
                rcvd.BCG      = h2,
                rcvd.OPV0     = h0,
                rcvd.OPV1     = h4,
                rcvd.OPV2     = h6,
                rcvd.OPV3     = h8,
                rcvd.DTP1     = h3,
                rcvd.DTP2     = h5,
                rcvd.DTP3     = h7,
                rcvd.MCV      = h9,
                health.card   = h1) %>%
  dplyr::rename(region = ADM1NAME,
                district = DHSREGNA) %>% 
  #generate weights for the mother
  mutate(wt=v005/1000000) %>%   
  mutate(intMo = as.numeric(intMo)) %>% 
  mutate(intYr = as.numeric(intYr)) %>% 
  mutate(birthYr = as.numeric(birthYr)) %>% 
  mutate(birthMo = as.numeric(birthMo)) %>% 
  #interview year missing for 2007 wave
  mutate(intYr_new   = as.integer((intCMC - 1)/12)+1900) %>%
  mutate(intYr = ifelse(is.na(intYr), intYr_new, intYr)) %>% 
  #correct education level
  mutate(edu.level = ifelse(edu.level == "9", NA, edu.level)) %>% 
  #impute the age of the child in months (in case value is missing for some children) 
  #mutate(age.months = intCMC-birthCMC) %>% 
  #convert mother's age into 5-year age groups
  mutate(age.mother.gr = cut(age.mother, c(10, 14, 19, 24, 29, 34, 39, 44, 49, Inf), c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), include.lowest=TRUE)) %>% 
  #household size grouped
  mutate(hh.size.gr = cut(hh.size, c(0, 3, 5, 7, Inf), c("1-3", "4-5", "6-7", "8+"), include.lowest=TRUE)) %>% 
  #usual resident at the place of interview
  mutate(resident = ifelse(v135=="visitor", 0, 1)) %>% 
  #filter to usual residents only
  filter(resident == 1) %>% 
  #marital status
  mutate(marital.status = ifelse(v501 == "not living together" | v501 == "divorced" | v501 == "no longer living together/separated", "divorced/separated", v501)) %>% 
  # Harmonize vaccination information
  mutate(BCG.vac   = ifelse(rcvd.BCG  == "vaccination date on card" | rcvd.BCG  == "reported by mother" | rcvd.BCG   == "vaccination marked on card" | rcvd.BCG == "vacc. date on card" | rcvd.BCG == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP1.vac  = ifelse(rcvd.DTP1 == "vaccination date on card" | rcvd.DTP1 == "reported by mother" | rcvd.DTP1  == "vaccination marked on card" | rcvd.DTP1 == "vacc. date on card" | rcvd.DTP1 == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP2.vac  = ifelse(rcvd.DTP2 == "vaccination date on card" | rcvd.DTP2 == "reported by mother" | rcvd.DTP2  == "vaccination marked on card" | rcvd.DTP2 == "vacc. date on card" | rcvd.DTP2 == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP3.vac  = ifelse(rcvd.DTP3 == "vaccination date on card" | rcvd.DTP3 == "reported by mother" | rcvd.DTP3  == "vaccination marked on card" | rcvd.DTP3 == "vacc. date on card" | rcvd.DTP3 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV0.vac  = ifelse(rcvd.OPV0 == "vaccination date on card" | rcvd.OPV0 == "reported by mother" | rcvd.OPV0  == "vaccination marked on card" | rcvd.OPV0 == "vacc. date on card" | rcvd.OPV0 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV1.vac  = ifelse(rcvd.OPV1 == "vaccination date on card" | rcvd.OPV1 == "reported by mother" | rcvd.OPV1  == "vaccination marked on card" | rcvd.OPV1 == "vacc. date on card" | rcvd.OPV1 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV2.vac  = ifelse(rcvd.OPV2 == "vaccination date on card" | rcvd.OPV2 == "reported by mother" | rcvd.OPV2  == "vaccination marked on card" | rcvd.OPV2 == "vacc. date on card" | rcvd.OPV2 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV3.vac  = ifelse(rcvd.OPV3 == "vaccination date on card" | rcvd.OPV3 == "reported by mother" | rcvd.OPV3  == "vaccination marked on card" | rcvd.OPV3 == "vacc. date on card" | rcvd.OPV3 == "vacc. marked on card", 1, 0)) %>% 
  mutate(MCV.vac   = ifelse(rcvd.MCV  == "vaccination date on card" | rcvd.MCV  == "reported by mother" | rcvd.MCV   == "vaccination marked on card" | rcvd.MCV  == "vacc. date on card" | rcvd.MCV  == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP.vac = ifelse((DTP1.vac + DTP2.vac + DTP3.vac) == 3, 1, 0)) %>% 
  mutate(OPV.vac = ifelse((OPV1.vac + OPV2.vac + OPV3.vac) == 3, 1, 0)) %>% 
  # Generate a variable for fully immunized (FIC) and partially immunized (PIC) child
  mutate(FIC = ifelse((BCG.vac + DTP1.vac + DTP2.vac + DTP3.vac + OPV1.vac + OPV2.vac + OPV3.vac + MCV.vac) == 8 , 1, 0)) %>%
  mutate(FIC = ifelse(age.months<12, NA, FIC)) %>% 
  dplyr::select(SurveyId, psu, LATNUM, LONGNUM, region, district, hh.id, woman.id, wt, age.months, BCG.vac, DTP1.vac, DTP2.vac, DTP3.vac, OPV0.vac, OPV1.vac, OPV2.vac, OPV3.vac, MCV.vac, DTP.vac, OPV.vac, FIC) %>% 
  #restrict the sample to children between 12 and 35 months of age (no info for older children for the latest survey wave since 2017)
  filter(age.months >= 12 & age.months <= 35)

  
# Harmonize the 
data_children_1to3yrs$district[data_children_1to3yrs$district == "Buxar"] = "Buxer"  
data_children_1to3yrs$district[data_children_1to3yrs$district == "Janjgir - Champa"] = "Janjgir-Champa"  
data_children_1to3yrs$district[data_children_1to3yrs$district == "Mahrajganj"] = "Maharajganj"  
data_children_1to3yrs$district[data_children_1to3yrs$district == "Sant Ravidas Nagar (Bhadohi)"] = "Sant Ravidas Nagar"  

data_psu_level <- data_children_1to3yrs %>% 
  group_by(SurveyId, psu) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            DTP.all = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            OPV.all = weighted.mean(OPV.vac, wt, na.rm=T),
            MCV.vac = weighted.mean(MCV.vac, wt, na.rm=T),
            SampleSize = n()) %>% 
  ungroup()


data_district_level <- data_children_1to3yrs %>% 
  group_by(SurveyId, district) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            DTP.all = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            OPV.all = weighted.mean(OPV.vac, wt, na.rm=T),
            MCV.vac = weighted.mean(MCV.vac, wt, na.rm=T),
            SampleSize = n()) %>% 
  ungroup()


## Retrieve the geographical boundaries
borders <- download_boundaries(surveyId = "IA2020DHS", method = "sf")
borders <- do.call(rbind.data.frame, borders) #convert to special feature dataframe

borders <- borders %>% filter(LEVELCO == "Admin2") %>% dplyr::select(DHSREGEN) %>% rename(district = DHSREGEN)

data_spatial <-borders %>% 
  left_join(data_district_level) %>% 
  na.omit()


ggplot(data_spatial)+ 
  geom_sf(aes(fill=FIC), col="darkgray",  size = 0.1) +
  scale_fill_distiller(palette="Spectral", direction=1, 
                       labels = scales::percent_format(accuracy = 1),  
                       na.value = "darkgray",
                       limits=c(0,1))  



save.image(file='DHS_data.RData')

