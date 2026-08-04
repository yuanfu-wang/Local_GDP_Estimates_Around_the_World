# --------------------------------- Task Summary --------------------------------- #
# This file obtains DOSE's subnational GDP data for the following developing countries:
#   THA, MOZ, UZB, KEN, VNM, SRB, ECU, BLR, ALB, LKA, BIH.
# -------------------------------------------------------------------------------- #

# use R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
rm(list = ls())
gc()

Sys.getlocale()
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(readxl)
library(sf)
library(jsonlite)
library(exactextractr)
library(terra)
library(qgisprocess)

iso_to_include <- c("THA", "MOZ", "UZB", "KEN", "VNM", "SRB", "ECU", "BLR",
                    "ALB", "LKA", "BIH")

# load DOSE v2.11 dataset
DOSE_v2_11 <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/DOSE/DOSE_V2.11.csv") %>%
  dplyr::select(c(GID_0, GID_1, year, grp_lcu, pop, grp_pc_lcu)) %>%  # grp_lcu means regional product in local currency
  rename(iso = GID_0, id = GID_1) %>%
  dplyr::filter(year >= 2012, iso %in% iso_to_include) %>%
  arrange(iso, year, id)

# load DOSE v2.14 dataset
DOSE_v2_14 <- read.csv("step2_obtain_gdp_data/inputs/gdp_data/regional/DOSE/DOSE_V2.14.csv") %>%
  dplyr::select(c(GID_0, GID_1, year, grp_lcu, pop, grp_pc_lcu)) %>%
  rename(iso = GID_0, id = GID_1) %>%
  dplyr::filter(year >= 2012, iso %in% iso_to_include) %>%
  arrange(iso, year, id)

# identify the keys used for merging
merge_keys <- c("iso", "id", "year")

# identify the region-year that has (1) the same GRP and (2) missing population in v2.14
population_fallback_keys <- DOSE_v2_11 %>%
  dplyr::select(all_of(merge_keys), grp_lcu_v2_11 = grp_lcu, pop_v2_11 = pop) %>%
  inner_join(
    DOSE_v2_14 %>%
      dplyr::select(all_of(merge_keys), grp_lcu_v2_14 = grp_lcu, pop_v2_14 = pop), by = merge_keys
  ) %>%
  dplyr::filter(
    !is.na(grp_lcu_v2_11),
    !is.na(grp_lcu_v2_14),
    grp_lcu_v2_11 == grp_lcu_v2_14,
    !is.na(pop_v2_11),
    is.na(pop_v2_14)
  ) %>%
  dplyr::select(all_of(merge_keys))

# extract 2020 observations from v2.11
DOSE_v2_11_priority <- bind_rows(
    DOSE_v2_11 %>%
      dplyr::filter(year == 2020),
    DOSE_v2_11 %>%
      semi_join(population_fallback_keys, by = merge_keys)
  ) %>%
  distinct(iso, id, year, .keep_all = TRUE)

# add 2020 observations to v2.14; replace missing populations using v2.11 if GRP is the same
DOSE_gdp_pre <- bind_rows(
    DOSE_v2_14 %>%
      anti_join(DOSE_v2_11_priority %>% dplyr::select(all_of(merge_keys)), by = merge_keys),
    DOSE_v2_11_priority
  ) %>%
  arrange(iso, year, id)

# there are some years for some countries have missing gdp data
which <- DOSE_gdp_pre  %>% 
  filter(is.na(grp_lcu))  %>% 
  distinct(iso, year)

# get rid of those countries in those years
DOSE_gdp_full <- DOSE_gdp_pre  %>% 
  anti_join(which, by = c("iso", "year"))

write.csv(DOSE_gdp_full, "step2_obtain_gdp_data/temp/DOSE_gdp_full.csv", row.names = F)
