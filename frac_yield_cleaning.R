# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03
# Description: Extracting and organizing fracture and yield information for the bedrock wells in the gwells lithology table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringi, furrr, future)
source("frac_yield_data_extraction.R")

# ------------------------------------------------------------------------------------------------------------
# Reading data
# ------------------------------------------------------------------------------------------------------------

# Defining column types and names from the outset
litho <- read_csv("data/my_classified_testing_dataset.csv", col_types = "ddddcccc") %>% 
  mutate(well_type = NA_character_, general_remarks = NA_character_) %>% 
  select(wtn, record_index, "depth_from" = from_depth, "depth_to" = to_depth, lithology, general_remarks, matclass, well_type, water_bearing_estimated_flow, well_yield_unit_code)
  
# The relevant columns frmo the well dataset, which contains reference comment information that will be used for subsequent data extraction.
well <- read_csv("data/well.csv") %>% select("wtn" = well_tag_number, comments)

# ==== Tidying and formatting dataset for data extraction ====

# Selecting relevant columns as well as filtering only wells that have a bedrock part to them as these will be the only ones where it will be relevant
# to run frac-yield extraction
litho <- litho %>% 
  select(wtn, record_index, depth_from, depth_to, general_remarks, lithology, matclass, well_type) %>%
  filter(wtn %in% unique(pull(filter(litho, matclass == "bedrock"), wtn)))

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
  # Making copies of the lithology and comment rows so that we can manipulate text without changing the original information
  mutate(lithology_copy = lithology) %>% 
  mutate(general_remarks_copy = general_remarks) %>% 
  # Turning all text to lower case
  mutate(lithology = tolower(lithology)) %>% 
  mutate(general_remarks = tolower(general_remarks)) %>% 
  # Removing unneccessary whitespace:
  mutate(lithology = str_squish(lithology)) %>% 
  mutate(general_remarks = str_squish(general_remarks)) %>% 
  # Removing plus signs
  mutate(lithology = str_remove_all(lithology, "\\+")) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "\\+")) %>% 
  # Transforming all gal.pm and gal/min signs to gpm
  mutate(lithology = str_replace_all(lithology, "gal.PM|gal/min", "gpm")) %>% 
  mutate(general_remarks = str_replace_all(general_remarks, "gal.PM|gal/min", "gpm")) %>% 
  # Transforming all missing lithology and general remark values to empty strings to prevent code breakage
  mutate(lithology = ifelse(is.na(lithology), "", lithology)) %>% 
  mutate(general_remarks = ifelse(is.na(general_remarks), "", general_remarks)) %>% 
  # Removing all dates as these confuse the regex parser. These will be added back when the orginal lithology comments are copied back over
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

# ==== Extracting Bedrock Lithology Data ====

# Splitting the table by well
litho_split <- litho %>% 
  group_by(wtn_grp) %>% 
  group_split()

# Checking the row count: If there are more than 10,000 rows being cleaned, using parallel processing.
if (nrow(litho) > 10000){
  # Setting the plan for parallel processing
  plan(multiprocess)
  
  # Running the extraction functions
  out_table_temp <- furrr::future_map(litho_split, extract_litho_data)
  out_table_temp2 <- furrr::future_map(out_table_temp, extract_litho_data)
  
  # Resetting the plan
  plan(sequential)
}else{
  # Running the extraction functions
  out_table_temp <- map(litho_split, extract_litho_data)
  out_table_temp2 <- map(out_table_temp, extract_comment_data)
}

# Joining the split table back together and removing the grouping variable
out_table <- out_table_temp2 %>% 
  bind_rows() %>% 
  group_by(wtn_grp) %>% 
  arrange(as.numeric(wtn_grp), as.numeric(depth_from), as.numeric(depth_to), as.numeric(record_index)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(wtn_grp = NULL)

# ==== Tidying final tables ====

# Replacing the original lithology and general remarks columns and also removing the wtn_grp variable
out_table <- out_table %>% 
  mutate(lithology = lithology_copy) %>% 
  mutate(lithology_copy = NULL) %>% 
  mutate(general_remarks = general_remarks_copy) %>% 
  mutate(general_remarks_copy = NULL)

# getting the wtns of wells have had issues and need review
review_wtns <- out_table %>% 
  filter(single_frac_yield == "review") %>% 
  pull(wtn) %>% 
  unique()
# Joining these with the wells that threw errors
review_wtns <- unique(c(error_wells$wtn, review_wtns))

# Adding a column titled review to the out_table to indicate which wells need manual review
out_table <- out_table %>% 
  mutate(needs_review = ifelse(wtn %in% review_wtns, T, F))

# ==== Saving data to disk and deleting objects from memory ====
write_csv(out_table, "output/bedrock_table-frac_yield_extracted.csv")
write_csv(error_wells, "output/error_record_table.csv")
# rm(list=ls())




