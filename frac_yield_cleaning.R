# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03
# Description: Extracting and organizing fracture and yield information for the bedrock wells in the gwells lithology table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringi, lubridate)
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
  # Making copies of the lithology and comment rows so that we can manipulate the original rows in ways that are more favourable to text cleaning
  mutate(lithology_copy = lithology) %>% 
  mutate(general_remarks_copy = general_remarks) %>% 
  # Next,performing some basic text cleaning operations on the lithology and comment rows: first, turning all text to lower case
  mutate(lithology = tolower(lithology)) %>% 
  mutate(general_remarks = tolower(general_remarks)) %>% 
  # Removing unneccessary whitespace:
  mutate(lithology = str_squish(lithology)) %>% 
  mutate(general_remarks = str_squish(general_remarks)) %>% 
  # Removing plus signs
  mutate(lithology = str_remove_all(lithology, "\\+")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "\\+")) %>% 
  # Transforming all gal.pm signs to gpm
  mutate(lithology = str_replace_all(lithology, "gal.PM|gal/min", "gpm")) %>% 
  mutate(general_remarks = str_replace_all(general_remarks, "gal.PM|gal/min", "gpm")) %>% 
  # Transforming all missing lithology and general remark values to empty strings to prevent code breakage
  mutate(lithology = ifelse(is.na(lithology), "", lithology)) %>% 
  mutate(general_remarks = ifelse(is.na(general_remarks), "", general_remarks)) %>% 
  # Removing all slashed dates as these confuse the regex parser. These will then be added back when the orginal lithology comments are copied back
  # over
  mutate(lithology = str_remove_all(lithology, "((?:\\d{1,4})\\/)?(\\d{1,2}|jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\/(\\d{2,4})")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "(?:(?:\\d{1,4})\\/)?(?:\\d{1,2}|jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:tember)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\/(?:\\d{2,4})")) %>% 
  
  # Adding columns for storing fracture and yield information
  mutate(fracture_from = NA_character_) %>% 
  mutate(fracture_to = NA_character_) %>% 
  mutate(fracture_type = NA_character_) %>% 
  mutate(single_frac_yield = NA_character_) %>% 
  mutate(single_frac_yield2 = NA_character_) %>% 
  mutate(cum_frac_yield = NA_character_) %>% 
  mutate(unit = NA_character_)

# ------------------------------------------------------------------------------------------------------------
# Testing
# ------------------------------------------------------------------------------------------------------------

# Running functions to extract lithology data and organize the dataset
out_table1 <- litho %>% 
  group_by(wtn_grp) %>% 
  group_modify(~ extract_litho_data(.x))

out_table <- out_table1 %>% 
  group_by(wtn_grp) %>% 
  group_modify(~ extract_comment_data(.x))

# Filter out the wells that have had issues and need review
review_wtns <- out_table %>% 
  filter(single_frac_yield == "review") %>% 
  pull(wtn) %>% 
  unique()

# Joining these with the wells that have thrown errors and saving these wells in a separate table by taking the data from the original litho table.
# Effectively, due to errors thrown, these wells were not edited
error_wtns <- unique(c(error_wtns, review_wtns))
error_table <- litho %>% 
  filter(wtn %in% error_wtns)

df <- litho %>% filter(wtn == 20832) %>% ungroup() %>% mutate_all(as.character)
df <- df %>% extract_litho_data()
df$general_remarks[1]
df$lithology
df

df <- df %>% extract_litho_data()
extract_comment_data(df)

df <- out_table %>% filter(wtn == 82538) %>% ungroup()
df$general_remarks[1]
df

out_table <- extract_litho_data(df[1:7,])

row <- rows(df)[[7]]
comment <- row$lithology

# ------------------------------------------------------------------------------------------------------------
# Frac-yield extraction
# ------------------------------------------------------------------------------------------------------------

str_squish(pairs[i,9]) != max(str_squish(pairs[,9]))

