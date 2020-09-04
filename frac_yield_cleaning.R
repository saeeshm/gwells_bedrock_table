# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03
# Description: Extracting and organizing fracture and yield information for the bedrock wells in the gwells lithology table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
source("regex_parsing_functions.R")
source("tidying_functions.R")

# ------------------------------------------------------------------------------------------------------------
# Reading data
# ------------------------------------------------------------------------------------------------------------
litho <- read_csv("data/classified_testing_dataset.csv")
well <- read_csv("data/well.csv")

# ------------------------------------------------------------------------------------------------------------
# Prerequisite data tidying
# ------------------------------------------------------------------------------------------------------------
# The lithology table to be cleaned - selecting only bedrock wells to run frac-yield extraction
litho <- litho %>% 
  select(wtn, record_index, depth_from, depth_to, general_remarks, lithology, matclass, well_type) %>%
  filter(wtn %in% unique(pull(filter(litho, well_type == "BEDROCK"), wtn)))

# The well dataset: selecting only relevant variables and filtering for only the wtns present in the lithology
# table (for the full dataset these should ideally be the same number of wells, if the data has been correctly
# reported)
well <- well %>% 
  select("wtn" = well_tag_number, diameter, finished_well_depth, final_casing_stick_up, bedrock_depth, 
         static_water_level, well_yield, well_yield_unit_code, surface_seal_material_code, surface_seal_depth,
         liner_material_code, liner_from, liner_to, screen_material_code, hydro_fracturing_performed, 
         aquifer_lithology_code, comments) %>% 
  filter(wtn %in% unique(litho$wtn))

# Adding all the relevant data (specifically the general comment information) from the well table to the
# lithology table to create a single cleanable dataset
litho <- litho %>% 
  mutate(wtn_grp = wtn) %>% 
  # grouping all the rows that refer to a single well
  group_by(wtn_grp) %>%
  # For each well, extracting the general comment and other relevant data associated with the well from the
  # well table and adding it to the litho table
  group_modify( ~ {
    comment <- well %>% filter(wtn == unique(.x$wtn)) %>% pull(comments)
    .x$general_remarks <- comment
    return(.x)
  })

# ------------------------------------------------------------------------------------------------------------
# Testing
# ------------------------------------------------------------------------------------------------------------
df <- litho %>% filter(wtn == 61872)
df

comment <- 

# ------------------------------------------------------------------------------------------------------------
# Frac-yield extraction
# ------------------------------------------------------------------------------------------------------------





