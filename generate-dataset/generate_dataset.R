#############################################
## Background ###############################
#############################################

## This script contains code to process raw data into a dataset for workshop use
## original data are archived in the inputs/ subdirectory or are otherwise online
## assume working directory is set to data-science-basics-2024

## Workforce data from https://www.who.int/data/gho/data/themes/topics/health-workforce
## Pulled June 11, 2024

## Income group data from https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html
## for year 2022
## Pulled June 12, 2024

## GDP data from https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
## for year 2022
## Pulled June 12, 2024

## Population data from https://databank.worldbank.org/source/population-estimates-and-projections# 
## for year 2022
## Pulled June 12, 2024

#############################################
## Setup ####################################
#############################################

## NOTE: if you don't already have these libraries installed, you can do so by running install.packages()
## example: install.packages("dplyr")

## Load libraries
library(tidyr) ## reshape, reformat, recode data: https://dplyr.tidyverse.org/reference/recode.html
library(readxl) ## to work with Excel files
library(readr) ## to use the convenience function write_delim
library(dplyr) ## for data manipulation

#############################################
## Country names ############################
#############################################

names <- read_excel("generate-dataset/inputs/country_names.xlsx")

#############################################
## ISO lookup ###############################
#############################################

iso_lookup <- read_excel("generate-dataset/inputs/iso_lookup.xls")

#############################################
## Country population data ##################
#############################################

population <- read.csv("generate-dataset/inputs/population_2022.csv", fileEncoding = "Latin1")

## long to wide dataset
population_wide <- population %>% 
  filter(Series.Name %in% c("Population, total", 
                            "Rural population (% of total population)",
                            "Mortality rate, under-5 (per 1,000)")) %>%
  select(Country.Code, Series.Name, X2022..YR2022.) %>%
  dplyr::rename(iso_code = Country.Code,
                metric = Series.Name,
                value = X2022..YR2022.) %>%
  mutate(value = as.numeric(na_if(value, ".."))) %>%
  pivot_wider(id_cols = iso_code,
              names_from = metric,
              values_from = value)

names(population_wide)[which(names(population_wide) == "Rural population (% of total population)")] <- "pct_rural"
names(population_wide)[which(names(population_wide) == "Population, total")] <- "total_population"
names(population_wide)[which(names(population_wide) == "Mortality rate, under-5 (per 1,000)")] <- "under_5_mortality_per1000"

#############################################
## GDP per capita data ######################
#############################################

gdp <- read.csv("generate-dataset/inputs/GDP per capita (current US$).csv", fileEncoding = "Latin1")
names(gdp) <- c("country", "year", "iso_code", "gdp_per_capita")
gdp_partial <- gdp[,-1]

#############################################
## World bank income groups #################
#############################################

income_groups <- read.csv("generate-dataset/inputs/wb_income_groups.csv", fileEncoding = "Latin1")
names(income_groups) <- c("country", "iso_code", "region", "income_group", "lending_category")
income_groups_partial <- income_groups[,-c(1,5)]


#############################################
## Burden of disease ########################
#############################################

disease <- read.csv("generate-dataset/inputs/IHME-GBD_2021_DATA-6037c6b5-1.csv", fileEncoding = "Latin1")
disease[which(disease$location == "TÃ¼rkiye"),]$location <- "Turkey"
disease[which(disease$location == "CÃ´te d'Ivoire"),]$location <- "Côte d'Ivoire"

disease_with_iso <- merge(disease, iso_lookup, by.x = "location", by.y = "country_name")

disease_with_iso_2021 <- disease_with_iso[which(disease_with_iso$year == 2021 & 
                         disease_with_iso$sex == "Both" &
                         disease_with_iso$age == "All ages" &
                         disease_with_iso$measure == "Prevalence" &
                         disease_with_iso$metric == "Percent"),]

wide_disease <- pivot_wider(disease_with_iso_2021[,c(5,8,12)], names_from = cause, values_from = c(val))

names(wide_disease) <- paste(gsub(" ", "_", gsub("/", "_", gsub("-", "_", tolower(names(wide_disease))))), "_prevalence_pct", sep = "")

#############################################
## CHW data (most recent) ###################
#############################################

chw <- read.csv("generate-dataset/inputs/community_health_workers.csv", fileEncoding = "Latin1")

recent_chw <- chw %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_chw_number = FactValueNumeric,
                most_recent_chw_year = Period)

#############################################
## Medical doctor data (most recent) ########
#############################################

md <- read.csv("generate-dataset/inputs/medical_doctors.csv", fileEncoding = "Latin1")

recent_md <- md %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Medical doctors (per 10,000)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_medical_doctor_per10000 = FactValueNumeric,
                most_recent_medical_doctor_year = Period)

#############################################
## Dentists (most recent) ###################
#############################################

dentist <- read.csv("generate-dataset/inputs/dentists.csv", fileEncoding = "Latin1")

recent_dentist <- dentist %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Dentists (per 10,000)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_dentist_per10000 = FactValueNumeric,
                most_recent_dentist_year = Period)

#############################################
## Environmental health (most recent) #######
#############################################

eh <- read.csv("generate-dataset/inputs/environmental_health.csv", fileEncoding = "Latin1")

eh_recent <- eh %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Environmental and Occupational Health and Hygiene Professionals  (number)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_eh_professionals_number = FactValueNumeric,
                most_recent_eh_professionals_year = Period)

#############################################
## Lab professionals (most recent) ##########
#############################################

lab <- read.csv("generate-dataset/inputs/lab_personnel.csv", fileEncoding = "Latin1")

lab_recent <- lab %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Medical and Pathology Laboratory scientists  (number)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_lab_scientists_number = FactValueNumeric,
                most_recent_lab_scientists_year = Period)

#############################################
## Lab professionals (most recent) ##########
#############################################

nmw <- read.csv("generate-dataset/inputs/nurse_midwives.csv", fileEncoding = "Latin1")

nmw_recent <- nmw %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_nurse_midwives_per10000 = FactValueNumeric,
                most_recent_nurse_midwives_year = Period)

#############################################
## Pharmacists (most recent) ################
#############################################

pharm <- read.csv("generate-dataset/inputs/pharmacists.csv", fileEncoding = "Latin1")

pharm_recent <- pharm %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Pharmacists  (per 10,000)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_pharmacists_per10000 = FactValueNumeric,
                most_recent_pharmacists_year = Period)

#############################################
## Pharmacists (most recent) ################
#############################################

physio <- read.csv("generate-dataset/inputs/physiotherapy_personnel.csv", fileEncoding = "Latin1")

physio_recent <- physio %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Physiotherapists (number)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_physiotherapists_number = FactValueNumeric,
                most_recent_physiotherapists_year = Period)

######################################################
## Traditional medicine (most recent) ################
######################################################

tm <- read.csv("generate-dataset/inputs/traditional_complementary_medicine.csv", fileEncoding = "Latin1")

tm_recent <- tm %>%
  filter(Location.type == "Country") %>%
  filter(IsLatestYear == "true") %>%
  filter(Indicator == "Traditional and Complementary Medicine Professionals  (number)") %>%
  select(c("SpatialDimValueCode", "FactValueNumeric", "Period")) %>%
  dplyr::rename(iso_code = SpatialDimValueCode,
                most_recent_traditional_med_professionals_number = FactValueNumeric,
                most_recent_traditional_med_professionals_year = Period)

#############################################
## Merge into one wide dataset ##############
#############################################

wide_dataset <- merge(
  merge(
  merge(
  merge(
  merge(
  merge(
  merge(
  merge(
  merge(
  merge(
  merge(recent_chw, recent_md, by = "iso_code", all.x = TRUE, all.y = TRUE),
  recent_dentist, all.x = TRUE, all.y = TRUE),
  eh_recent, all.x = TRUE, all.y = TRUE),
  lab_recent, all.x = TRUE, all.y = TRUE),
  nmw_recent, all.x = TRUE, all.y = TRUE),
  pharm_recent, all.x = TRUE, all.y = TRUE),
  physio_recent, all.x = TRUE, all.y = TRUE),
  tm_recent, all.x = TRUE, all.y = TRUE),
  population_wide, all.x = TRUE, all.y = TRUE),
  gdp_partial, all.x = TRUE, all.y = TRUE),
  income_groups_partial, all.x = TRUE, all.y = TRUE)

wide_filtered_dataset <- merge(names, wide_dataset, by = "iso_code")

#############################################################
## Standardize all workforce data to per 10000 ##############
#############################################################

wide_filtered_dataset$most_recent_chw_per10000 <- wide_filtered_dataset$most_recent_chw_number/wide_filtered_dataset$total_population*10000
wide_filtered_dataset$most_recent_eh_professionals_per10000 <- wide_filtered_dataset$most_recent_eh_professionals_number/wide_filtered_dataset$total_population*10000
wide_filtered_dataset$most_recent_lab_scientists_per10000 <- wide_filtered_dataset$most_recent_lab_scientists_number/wide_filtered_dataset$total_population*10000
wide_filtered_dataset$most_recent_physiotherapists_per10000 <- wide_filtered_dataset$most_recent_physiotherapists_number/wide_filtered_dataset$total_population*10000
wide_filtered_dataset$most_recent_traditional_med_professionals_per10000 <- wide_filtered_dataset$most_recent_traditional_med_professionals_number/wide_filtered_dataset$total_population*10000

################################################################
## Add burden of disease info with YET ANOTHER JOIN ############
################################################################

## and yet one more merge
## the name of alpha_3 is bad and lazy but I'm not going to fix it right now
wide_filtered_dataset_plus <- as.data.frame(merge(wide_filtered_dataset, wide_disease,
                                    by.x = "iso_code", by.y = "alpha_3_prevalence_pct"),
                                    all.x = TRUE, all.y = FALSE)

################################################################
## Save single neat dataset ####################################
################################################################

# write.table(wide_filtered_dataset_plus[c(1,2,26,27,25,21,22,23,5,6,13,14,15,16,7,8,29,4,30,10,31,12,32,18,33,20, 34:52)],
#             sep = "\t",
#             file = "country_workforce_data.tsv", 
#             na = "NA",
#             row.names = FALSE,
#             fileEncoding = "Latin1")
