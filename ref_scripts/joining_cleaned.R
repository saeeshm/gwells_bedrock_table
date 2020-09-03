# Name: Saeesh Mangwani
# Date: 2020-05-26
# Description: Joining the 2 cleaned tables into a single table, and applying completion functions on the single and cumulative yield columns

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(readxl)

og_data <- data <- read_excel("data/highlands_intervals.xlsx") %>% select(-depth_from, -depth_to) %>% mutate_all(as.character)
data1 <- read_csv("highlands_cleaned_first.csv") %>% mutate_all(as.character)
data2 <- read_csv("review_table_cleaned.csv") %>% mutate_all(as.character)

# Joining the 2 tables, as well as the one well we had separated out at the beginning because it was previously manually cleaned
cleaned <- data1 %>% 
  bind_rows(data2) %>% 
  bind_rows(og_data %>% filter(wtn == 21315))

# Other manipulations
cleaned <- cleaned %>% 
  # Wherever the fracture comment says "depth comment", changing it to NA
  mutate(fracture = if_else(fracture == "depth comment", NA_character_, fracture)) %>% 
  # Wherever the fracture column says "fracture/yield" but there are no fracture values present, using the depth values present
  mutate(fracture_from = if_else((fracture %in% c("fracture/yield", "fracture") & is.na(fracture_from)),
                                 depth_from_ft,
                                 fracture_from)) %>% 
  mutate(fracture_to= if_else((fracture %in% c("fracture/yield", "fracture") & is.na(fracture_to)),
                                 depth_to_ft,
                                 fracture_to)) %>% 
  # Wherever the column says fracture/yield, but no yield value is present, changing it to NA
  mutate(fracture = if_else(is.na(single_frac_yield) & is.na(cum_frac_yield),
                           NA_character_,
                           fracture))

cleaned$fracture %>% table(., useNA = "always")

# Filtering those that remain, splitting them by group based on their wtn and then applying the transform_cumulative function created above to each
# group to ensure that the allocation of single to cumulative yields is appropriate. Once this is done, applying a function to complete the cumulative
# and single yield columns for all the wells
final <- cleaned %>% 
  group_by(wtn) %>% 
  group_modify(~ transform_cumulative(.x)) %>% 
  arrange(as.numeric(record_index), as.numeric(depth_from_ft), as.numeric(depth_to_ft)) %>% 
  group_modify(~ fill_yield_vals(.x))

# Exporting the final table
write_csv(final, "highlands_cleaned.csv")