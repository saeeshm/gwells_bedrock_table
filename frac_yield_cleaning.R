# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03
# Description: Extracting and organizing fracture and yield information for the bedrock wells in the gwells lithology table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tm, stringi)
source("regex_parsing_functions.R")
source("frac-yield_extraction_functions.R")

# ------------------------------------------------------------------------------------------------------------
# Reading data
# ------------------------------------------------------------------------------------------------------------
litho <- read_csv("data/classified_testing_dataset.csv")
litho <- read_csv("data/my_classified_testing_dataset.csv", col_types = "ddddcccc") %>% 
  mutate(well_type = NA_character_, general_remarks = NA_character_) %>% 
  select(wtn, record_index, "depth_from" = from_depth, "depth_to" = to_depth, lithology, general_remarks, matclass, well_type, water_bearing_estimated_flow, well_yield_unit_code)
  
well <- read_csv("data/well.csv")

# ------------------------------------------------------------------------------------------------------------
# Prerequisite data tidying
# ------------------------------------------------------------------------------------------------------------
# The lithology table to be cleaned - selecting only bedrock wells to run frac-yield extraction
litho <- litho %>% 
  select(wtn, record_index, depth_from, depth_to, general_remarks, lithology, matclass, well_type) %>%
  filter(wtn %in% unique(pull(filter(litho, matclass == "bedrock"), wtn)))

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
  }) %>% 
  # Performing some basic text cleaning operations on the lithology and comment rows: first, turning all text to lower case
  mutate(lithology = tolower(lithology)) %>% 
  mutate(general_remarks = tolower(general_remarks)) %>% 
  # Removing plus signs
  mutate(lithology = str_remove_all(lithology, "\\+")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "\\+")) %>% 
  # Transforming all gal.pm signs to gpm
  mutate(lithology = str_replace_all(lithology, "gal.PM", "gpm")) %>% 
  mutate(general_remarks = str_replace_all(general_remarks, "gal.PM", "gpm")) %>% 
  # Removing punctuation
  
  # Adding columns for storing fracture and yield information
  mutate(fracture_from = NA_character_) %>% 
  mutate(fracture_to = NA_character_) %>% 
  mutate(fracture_type = NA_character_) %>% 
  mutate(single_frac_yield = NA_character_) %>% 
  mutate(cum_frac_yield = NA_character_) %>% 
  mutate(unit = NA_character_)

# ------------------------------------------------------------------------------------------------------------
# Testing
# ------------------------------------------------------------------------------------------------------------

# Running functions to extract lithology data and organize the dataset
out_table <- litho %>% 
  group_by(wtn_grp) %>% 
  group_modify(~ extract_litho_data(.x)) %>% 
  group_modify(~ extract_comment_data(.x))

# Filter out the wells that have had issues and need review
review_wtns <- out_table %>% 
  filter(single_frac_yield == "review") %>% 
  pull(wtn) %>% 
  unique()

# Joining these with the wells that have thrown errors and saving these wells in a separate table by taking the data from the original litho table.
# Effectively, due to errors thrown, these wells were not edited
error_wtns <- c(error_wtns, review_wtns)
error_table <- litho %>% 
  filter(wtn %in% error_wtns)

df <- litho %>% filter(wtn == 18074) %>% ungroup()
df


# ------------------------------------------------------------------------------------------------------------
# Frac-yield extraction
# ------------------------------------------------------------------------------------------------------------





