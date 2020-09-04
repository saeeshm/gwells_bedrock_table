# Name: Saeesh Mangwani
# Date: 2020-05-19
# Description: Extracting fracture and yield information from comments in highlands data table

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(readxl)
library(tidytext)
source("cleaning_helper_functions.R")

# Reading data and creating an out_table to store parsed rows
data <- read_excel("data/highlands_intervals.xlsx") %>% select(-depth_from, -depth_to) %>% mutate_all(as.character)

# Turning all text to lower case, removing the '+' quantifier which is unhelpful and changing "gal.PM" values to "gpm"
data <- data %>% 
  mutate(lithology = tolower(lithology)) %>% 
  mutate(lithology = str_remove_all(lithology, "\\+")) %>% 
  mutate(lithology = str_replace_all(lithology, "gal.PM", "gpm")) %>% 
  mutate(general_remarks = tolower(general_remarks)) %>% 
  mutate(general_remarks = str_remove_all(general_remarks, "\\+")) %>% 
  mutate(general_remarks = str_replace_all(general_remarks, "gal.PM", "gpm"))

error_wtns <- c()
# Completing the borehole rows
# data %>% 
#   group_by(wtn) %>% 
#   group_modify(., .f = function(.x, ...){
#     bore_val <- .x$borehole[1]
#     mutate(.x, borehole = bore_val)
#   })

# The approach in this script is to group all the wells by their wtns, treat the rows in each group as a single mini-dataframe and then apply a
# function that can extract information from each of group and adding it to a separate table. The first task is thus to write 2 functions that can
# extract this information from lithology rows and general remarks respectively. These can then be applied to grouped data across the whole dataset.

# Testing df
df <- data %>% filter(wtn == 100421)
# row <- rows(df)
# getFracVals(row$lithology)
df
# A function that takes a dataframe, goes through all of its lithology comments and adds values and rows using data extraction functions where
# applicable
extract_litho_data <- function(df){
    # Print the wtn being worked on
    print(df$borehole[1])
    # Creating an empty out_table to store the resulting data
    out_table <- df[0,]
    # Iterating through all the rows in the passed dataframe
    for(row in rows(df)){
      #checking for frac-yield pairs and fractures from the lithology comment
      pairs <- getYieldFracPairs(row$lithology)
      fractures <- getFracVals(row$lithology)
      # If fractures or pairs were present, removing them from the string being considered
      temp <- ifelse(nrow(pairs) > 0, 
                     str_remove_all(row$lithology, paste(pairs$string, collapse = "|")), 
                     row$lithology)
      temp <- ifelse(nrow(fractures) > 0, 
                     str_remove_all(temp, paste(fractures$string, collapse = "|")), 
                     temp)
      # Having now removed any strings that were used to identify fractures or yield value pairs, we finally check for yields alone from the remnant string
      yields <- getYieldVals(temp)
      
      # If none of the three sorts of matches are found, simply adding the row to the out_table as-is and moving on
      if( ((nrow(pairs) == 0) | sum(!is.na(pairs)) == 0) & 
          ((nrow(fractures) == 0) | sum(!is.na(fractures)) == 0) & 
          is.na(yields$yield) ) {
        print("nothing found")
        row$fracture <- NA_character_
        out_table <- bind_rows(row, out_table)
        next
      }
      
      # If there exist any fractures that were found
      tryCatch({
        if(nrow(fractures) > 0){
          print("checking fractures")
          # Iterating through all the fractures 
          for(frac in rows(fractures)){
            # If any of the fracture values are found to lie within the depth range associated with this row:
            if( between(as.numeric(frac$depth), 
                        as.numeric(row$depth_from_ft), 
                        as.numeric(row$depth_to_ft)) & 
                is.na(row$fracture_from)){
              # If the fracture columns are currently empty, edit the row to add the fracture values at the correct place
              row$fracture_from <- frac$depth
              row$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
              # Adding a comment indiciating that this row contains only fracture information
              row$fracture <- "fracture"
            # If either the fracture columns already contain data or the depth does not fall within the depth range for this row, a new row needs to be
            # created, with only some, not all, relevant information copied over.
            }else{
              row2 <- row
              # Setting some values to NA as it doesn't make sense to have these copied over
              row2$borehole <- NA_character_
              row2$lid <- NA_character_
              row2$record_index <- NA_character_
              # Adding relevant values
              row2$depth_from_ft <- frac$depth
              row2$depth_to_ft <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
              row2$fracture_from <- frac$depth
              row2$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
              # A comment to highlight the fact that this is a new addition
              row2$lithology <- "row added to account for fracture. No row-specific comment"
              # Adding a comment indiciating that this row contains only fracture information
              row2$fracture <- "fracture"
              # Appending this row to the out_table, to be added to the dataframe later
              out_table <- bind_rows(row2, out_table)
            }
          }
        }
      },
      warning = function(w){
        print(paste("Error in processing row while looking for fractures", df$borehole[1]))
        error_wtns <<- append(error_wtns, df$borehole[1])
      })
      
      # If there exist any pairs that were found
      tryCatch({
        if(nrow(pairs) > 0){
          print("checking pairs")
          # Iterating through all the pairs discovered
          for(pair in rows(pairs)){
            # If any of the pair values are found to lie within the depth range associated with this row:
            if( between(as.numeric(pair$depth), 
                        as.numeric(row$depth_from_ft), 
                        as.numeric(row$depth_to_ft)) & 
                is.na(row$fracture_from)){
              # If the fracture and yield columns are currently empty, editing the row and adding the
              # fractures and yields at the correct places
              row$fracture_from <- pair$depth
              row$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
              row$single_frac_yield <- pair$yield
              row$unit <- pair$yield_unit
              # Adding a comment indiciating that this row contains fracture and yield information
              row$fracture <- "fracture/yield"
            # If either the fracture columns already contain data or the depth does not fall within the depth
            # range for this row, a new row needs to be created, with only some, not all, relevant information
            # copied over.
            }else{
              # Checking if the current depth value already exists within some row in the out_table (added
              # perhaps from the previous pass of detecting fractures), and such a row does not have any
              # associated yield values. In this case, this row in the out_table is simply edited, rather than
              # having a new one be created
              if(pair$depth %in% out_table$depth_from_ft){
                # If the depth value already exists and has no associated yield values, editing this row
                # instead of creating a new one
                out_table[out_table$depth_from_ft %in% pair$depth, ] <- out_table %>% 
                  filter(depth_from_ft %in% pair$depth) %>% 
                  mutate(single_frac_yield = pair$yield) %>% 
                  mutate(unit = pair$yield_unit) %>% 
                  mutate(fracture = "fracture/yield")
              
              # If the depth value doesn't exist, creating a new row to add these data
              }else{
                row2 <- row
                # Setting some values to NA as it doesn't make sense to have these copied over
                row2$borehole <- NA_character_
                row2$lid <- NA_character_
                row2$record_index <- NA_character_
                # Adding relevant values
                row2$depth_from_ft <- pair$depth
                row2$depth_to_ft <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
                row2$fracture_from <- pair$depth
                row2$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
                row2$single_frac_yield <- pair$yield
                row2$unit <- pair$yield_unit
                # A comment to highlight the fact that this is a new addition
                row2$lithology <- "row added to account for fracture. No row-specific comment"
                # Adding a comment indiciating that this row contains fracture and yield information
                row2$fracture <- "fracture/yield"
                
                # Appending this row to the out_table, to be added to the dataframe later
                out_table <- bind_rows(row2, out_table)
              }
            }
          }
        }
      },
      warning = function(w){
        print(paste("Error in processing row while looking for pairs", df$borehole[1]))
        error_wtns <<- append(error_wtns, df$borehole[1])
      })
      
      # If a yield value is found, checking to see if values already exist in the row. If not, adding it to
      # the row. If yield values exist, creating a new row to store it as a cumulative, rather than a single
      # yield. If multiple values are found, the function returns the word "review" instead of a value itself.
      tryCatch({
        if( !is.na(yields$yield) ){
          print("checking yields")
          if(is.na(row$single_frac_yield)){
            row$single_frac_yield <- yields$yield
            row$unit <- yields$unit
          }else{
            row2 <- row
            # Setting some values to NA as it doesn't make sense to have these copied over
            row2$borehole <- NA_character_
            row2$lid <- NA_character_
            row2$record_index <- NA_character_
            # Adding relevant values. Setting the depth range to the depth of the whole well
            row2$depth_from_ft <- "0"
            row2$depth_to_ft <- as.character(max(as.numeric(out_table$depth_to_ft), na.rm = T))
            row2$cum_frac_yield <- yields$yield
            row2$unit <- yields$yield_unit
            # A comment to highlight the fact that this is a new addition
            row2$lithology <- "row added to account for cumulative well yield. No row-specific comment"
            # Adding a comment indiciating that this row contains only yield information
            row2$fracture <- "yield"
            # Appending the row to the out_table
            out_table <- bind_rows(row2, out_table)
          }
        }
      },
      warning = function(w){
        print(paste("Error in processing row while looking for yields", df$borehole[1]))
        error_wtns <<- append(error_wtns, df$borehole[1])
      })
      # Appending the original row from this iteration to the out_table, whether or not it has been edited
      out_table <- bind_rows(row, out_table)
    }
    return(out_table %>% arrange(as.numeric(depth_from_ft), as.numeric(depth_to_ft), as.numeric(record_index)))
}
  
# A function that takes a dataframe, goes through the general comment associated with the wtn whose data is in the dataframe and uses data extraction
# functions to add rows/values where applicable
extract_comment_data <- function(df){
  print(df$borehole[1])
  # Since there is only comment associated with all the rows, extracting it first and treating it separately
  comment <- df$general_remarks[1]
  # Getting any pair, fracture or yield information stored in the comment
  pairs <- getYieldFracPairs(comment)
  fractures <- getFracVals(comment)
  # If fractures or pairs were present, removing them from the string being considered
  temp <- ifelse(nrow(pairs) > 0, str_remove_all(comment, paste(pairs$string, collapse = "|")), comment)
  temp <- ifelse(nrow(fractures) > 0, str_remove_all(temp, paste(fractures$string, collapse = "|")), temp)
  # Having now removed any strings that were used to identify fractures or yield value pairs, we finally check for yields alone from the remnant string
  yields <- getYieldVals(temp)
  
  # If nothing is detected, returning the table as-is
  if( ((nrow(pairs) == 0) | sum(!is.na(pairs)) == 0) & ((nrow(fractures) == 0) | sum(!is.na(fractures)) == 0) & is.na(yields$yield) ){
    print("nothing found")
    return(df %>% arrange(as.numeric(depth_from_ft), as.numeric(depth_to_ft), as.numeric(record_index)))
  }
  
  # If fractures are detected
  tryCatch({
    if(nrow(fractures)> 0){
      print("checking fractures")
      # iterating through the fractures
      for(frac in rows(fractures)){
        # First identifying which depth range the fracture exists in
        row_index <- (as.numeric(frac$depth) >= as.numeric(df$depth_from_ft)) & (as.numeric(frac$depth) < as.numeric(df$depth_to_ft))
        # If the found fractures lie within any of the existing lithology ranges, and there are no fracture values associated with the row for this range,
        # editing this row by adding the fracture values
        if( (sum(row_index) > 0) ){
          df[row_index,]$fracture_from <- case_when(is.na(df[row_index,]$fracture_from) ~ frac$depth,
                                                    df[row_index,]$fracture_from == frac$depth ~ df[row_index,]$fracture_from,
                                                    TRUE ~ df[row_index,]$fracture_from)
          # Creating a temporary variable for the fracture_to value
          temp_depth <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          # Updating the fracture_to value
          df[row_index,]$fracture_to <- case_when(is.na(df[row_index,]$fracture_to) ~ temp_depth,
                                                  df[row_index,]$fracture_from == temp_depth ~ df[row_index,]$fracture_from,
                                                  TRUE ~ df[row_index,]$fracture_from)
          df[row_index,]$fracture <- "fracture"
        # If either the fractures found are not within any of the existing ranges, or fracture values that do exist are not the same as those that we
        # are adding, creating a new row to store these data
        }else{
          # Copying any row and setting some values to NA since we don't actually have info about them as well as setting its depth range to just the
          # range of the fracture
          row <- df[1, ]
          row$bedrock <- NA_character_
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$depth_from_ft <- frac$depth
          row$depth_to_ft <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          # Setting some values to NA as it doesn't make sense to have these copied over
          row$borehole <- NA_character_
          row$lid <- NA_character_
          row$record_index <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row$fracture_from <- frac$depth
          row$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          row$fracture <- "fracture"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for yields", df$borehole[1]))
    error_wtns <<- append(error_wtns, df$borehole[1])
  })
  
  # If pairs are detected
  tryCatch({
    if(nrow(pairs)> 0){
      print("checking pairs")
      # iterating through the pairs
      for(pair in rows(pairs)){
        # First identifying which depth range the pair exists in. If there is such a row where both the from and to depths match exactly with those from
        # the pair we're adding, using those as the index. If not, then comparing the available ranges to see where the pair might fall
        index_test <- (as.numeric(pair$depth) == as.numeric(df$depth_from_ft) & as.numeric(pair$depth) == as.numeric(df$depth_to_ft))
        row_index <- if_else(sum(index_test) > 0, 
                            list(index_test), 
                            list(as.numeric(pair$depth) >= as.numeric(df$depth_from_ft) & as.numeric(pair$depth) <= as.numeric(df$depth_to_ft)))
        row_index <- unlist(row_index)
        # If the found pairs lie within any of the existing lithology ranges:
        if( (sum(row_index) > 0) ){
          # If the fracture_from is missing, replacing it with the value from the current pair. If it is the same as the current value, leaving it as
          # is. If there already exists a value, leaving it as-is.
          df[row_index,]$fracture_from <- case_when(is.na(df[row_index,]$fracture_from) ~ pair$depth,
                                                    df[row_index,]$fracture_from == pair$depth ~ df[row_index,]$fracture_from ,
                                                    TRUE ~ df[row_index,]$fracture_from)
          # Creating a temporary variable for the fracture_to value
          temp_depth <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          df[row_index,]$fracture_to <- case_when(is.na(df[row_index,]$fracture_to) ~ temp_depth,
                                                  df[row_index,]$fracture_to == temp_depth ~ df[row_index,]$fracture_to ,
                                                  TRUE ~ df[row_index,]$fracture_to)
          df[row_index,]$single_frac_yield <- case_when(is.na(df[row_index,]$single_frac_yield) ~ pair$yield,
                                                        df[row_index,]$single_frac_yield == pair$yield ~ df[row_index,]$single_frac_yield ,
                                                        TRUE ~ df[row_index,]$single_frac_yield)
          df[row_index,]$unit <- case_when(is.na(df[row_index,]$unit) ~ pair$yield_unit,
                                           df[row_index,]$unit == pair$yield_unit ~ df[row_index,]$unit ,
                                           TRUE ~ df[row_index,]$unit)
          df[row_index,]$fracture <- "fracture/yield"
        # If either the pairs found are not within any of the existing ranges, or fracture-yield values exist already for the present range, creating a
        # new row and adding it to the dataframe
        }else{
          # Copying any row and setting some values to NA since we don't actually have info about them as well as setting its depth range to just the
          # range of the fracture
          row <- df[1, ]
          row$bedrock <- NA_character_
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$depth_from_ft <- pair$depth
          row$depth_to_ft <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          # Setting some values to NA as it doesn't make sense to have these copied over
          row$borehole <- NA_character_
          row$lid <- NA_character_
          row$record_index <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row$fracture_from <- pair$depth
          row$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          row$single_frac_yield <- pair$yield
          row$unit <- pair$yield_unit
          row$fracture <- "fracture/yield"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for yields", df$borehole[1]))
    error_wtns <<- append(error_wtns, df$borehole[1])
  })
  
  # If yields are detected
  tryCatch({
    if(!is.na(yields$yield)){
      print("checking yields")
      # Selecting the last row in the dataframe
      df <- arrange(df, as.numeric(depth_from_ft))
      last <- df[nrow(df),]
      # Updating the cumulative yield value in this row, with the value extracted
      last$cum_frac_yield <- yields$yield
      last$unit <- yields$unit
      last$fracture <- if_else(is.na(last$fracture_from), "yield", "fracture/yield")
      # Updating the table with this new row
      df[nrow(df),] <- last
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for yields", df$borehole[1]))
    error_wtns <<- append(error_wtns, df$borehole[1])
  })
  
  # Return the manipulated table
  return(df %>% arrange(as.numeric(depth_from_ft), as.numeric(depth_to_ft), as.numeric(record_index)))
}

# An randomly chosen subset of 10 wtns (picked using a random sampling function) that can help us test how good these funtions are at achieving what
# we want
# sample(unique(data$wtn), 10)
# wtns <- c("54218", "75642", "895", "30288", "48813", "85654", "73328", "90335", "81950",  "85707")

# Having tested the functions, running them on the entire table (except for wtn 21315, which we've already edited manually beforehand)
out_table <- data %>% filter(!wtn == 21315) %>% 
  group_by(wtn) %>% 
  group_modify(~ extract_litho_data(.x)) %>% 
  group_modify(~ extract_comment_data(.x))

review_wtns <- out_table %>% 
  filter(single_frac_yield == "review") %>% 
  pull(wtn) %>% 
  unique()

manual_wtns <- c(error_wtns, review_wtns)

# Writing the resulting table to a csv file.
write_csv(out_table %>% filter(!wtn %in% manual_wtns), "highlands_cleaned_first.csv")
write_csv(out_table %>% filter(wtn %in% manual_wtns), "review_table.csv")
