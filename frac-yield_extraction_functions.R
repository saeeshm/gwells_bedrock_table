# !usr/bin/env Rscript

# Author: Saeesh Mangwani Date: 2020-09-10 Description: A script containing functions which call the regex parsing functions on the bedrock dataset to
# extract fracture and yield data and organize it into a table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
source("regex_parsing_functions.R")

# Creating a global variable named error_wtns that stores the tag numbers for wells that throw an error in the classification process
error_wtns <- c()

# A function that takes a dataframe, goes through all of its lithology comments and adds values and rows using data extraction functions where
# applicable
extract_litho_data <- function(df){
  # Print the wtn being worked on
  print(df$wtn[1])
  # Creating an empty out_table to store the resulting data
  out_table <- df[0,] %>% mutate_all(as.character)
  # Iterating through all the rows in the passed dataframe
  for(row in rows(df)){
    #checking for frac-yield pairs and fractures from the lithology comment
    pairs <- getYieldFracPairs(row$lithology)
    fractures <- getFracVals(row$lithology)
    # If fractures or pairs were present, removing them from the string being considered
    temp <- ifelse(nrow(pairs) > 0, 
                   str_remove_all(row$lithology, str_replace_all(paste(pairs$string, collapse = "|"), "\\(", "\\\\(")), 
                   row$lithology)
    temp <- ifelse(nrow(fractures) > 0, 
                   str_remove_all(temp, str_replace_all(paste(fractures$string, collapse = "|"), "\\(", "\\\\(")), 
                   temp)
    # Having now removed any strings that were used to identify fractures or yield value pairs, we finally check for yields alone from the remnant string
    yields <- getYieldVals(temp)
    
    # If none of the three sorts of matches are found, simply adding the row to the out_table as-is and moving on
    if( ((nrow(pairs) == 0) | sum(!is.na(pairs)) == 0) & 
        ((nrow(fractures) == 0) | sum(!is.na(fractures)) == 0) & 
        is.na(yields$yield) ) {
      print("nothing found")
      row$fracture_type <- NA_character_
      row <- map(row, as.character)
      out_table <- bind_rows(row, out_table)
      next
    }
    
    # If there exist any fractures that were found
    tryCatch({
      if(nrow(fractures) > 0){
        print("checking fractures")
        # Iterating through all the fractures 
        for(frac in rows(fractures)){
          # If the fracture values is found to lie within the depth range associated with THIS row and the fracture fields for this row are still
          # empty
          if( between(as.numeric(frac$depth), 
                      as.numeric(row$depth_from), 
                      as.numeric(row$depth_to)) & 
              is.na(row$fracture_from)){
            # If the fracture columns are currently empty, edit the row to add the fracture values at the correct place
            row$fracture_from <- frac$depth
            row$fracture_to <- if_else(is.na(frac$depth2) | (frac$depth2 == ""), frac$depth, frac$depth2)
            # Adding a comment indiciating that this row contains only fracture information
            row$fracture_type <- "fracture"
          
          # If instead the fracture lies within a depth range covered by another row in the table and that row contains fracture columns that are
          # empty (the following function helps check for these conditions and returns the appropriate row if they are met, see the help_funcs
          # script.)
          }else if(nrow(find_depth_range(frac$depth, out_table)) > 0){
            # If there is such a row, simply editing its contents
            out_table[out_table$depth_from %in% find_depth_range(frac$depth, out_table)$depth_from, ] <- out_table %>% 
              filter(depth_from %in% find_depth_range(frac$depth, out_table)$depth_from) %>% 
              mutate(fracture_from = frac$depth) %>% 
              mutate(fracture_to = if_else(is.na(frac$depth2) | (frac$depth2 == ""), frac$depth, frac$depth2)) %>% 
              mutate(fracture_type = "fracture")
          }else{
            # Otherwise, creating a new row  
            row2 <- row
            # Setting some values to NA as it doesn't make sense to have these copied over
            row2$record_index <- NA_character_
            # Adding relevant values
            row2$depth_from <- frac$depth
            row2$depth_to <- if_else(is.na(frac$depth2) | frac$depth2 == "", frac$depth, frac$depth2)
            row2$fracture_from <- frac$depth
            row2$fracture_to <- if_else(is.na(frac$depth2) | frac$depth2 == "", frac$depth, frac$depth2)
            # A comment to highlight the fact that this is a new addition
            row2$lithology <- "row added to account for fracture. No row-specific comment"
            # Adding a comment indiciating that this row contains only fracture information
            row2$fracture_type <- "fracture"
            # Appending this row to the out_table, to be added to the dataframe later 
            row2 <- map(row2, as.character)
            out_table <- bind_rows(row2, out_table)
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for fractures", df$wtn[1]))
      error_wtns <<- append(error_wtns, df$wtn[1])
    })
    
    
    # If there exist any pairs that were found
    tryCatch({
      if(nrow(pairs) > 0){
        print("checking pairs")
        # Iterating through all the pairs discovered
        for(pair in rows(pairs)){
          # If any of the pair values are found to lie within the depth range associated with this row:
          if( between(as.numeric(pair$depth), 
                      as.numeric(row$depth_from), 
                      as.numeric(row$depth_to)) & 
              is.na(row$fracture_from)){
            # If the fracture and yield columns are currently empty, editing the row and adding the
            # fractures and yields at the correct places
            row$fracture_from <- pair$depth
            row$fracture_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
            row$single_frac_yield <- pair$yield
            row$unit <- pair$yield_unit
            # Adding a comment indiciating that this row contains fracture and yield information
            row$fracture_type <- "fracture/yield"
            # If either the fracture columns already contain data or the depth does not fall within the depth
            # range for this row, a new row needs to be created, with only some, not all, relevant information
            # copied over.
          }else{
            # Checking if the current depth value already exists within some row in the out_table (added perhaps from the previous pass of detecting
            # fractures). If the fracture_depth associated with this row is the same as fracture_depth of this pair value, then simply editing this
            # row such a row does not have any associated yield values. In this case, this row in the out_table is simply edited, rather than having a
            # new one be created
            if(pair$depth %in% out_table$depth_from & pair$depth %in% out_table[out_table$depth_from %in% pair$depth, ]$fracture_from){
              # If the depth value already exists and has no associated yield values, editing this row
              # instead of creating a new one
              out_table[out_table$depth_from %in% pair$depth, ] <- out_table %>% 
                filter(depth_from %in% pair$depth) %>% 
                mutate(fracture_from = pair$depth) %>% 
                # for the fracture to depth
                mutate(fracture_to = case_when(
                  # if it is presently empty, using the reported fracture depth from the pair
                  (is.na(fracture_to) | fracture_to == "") ~ if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2),
                  # If the one from the pair is equal to what is already there, keeping it as-is
                  fracture_to == if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2) ~ fracture_to,
                  # If the depth already present and the one from the pair are not equal, checking if the current one is the same as the fracture_from
                  # depth. If it is not, keeping this one
                  fracture_to != fracture_from ~ fracture_to,
                  # If it is, using the correctly reported value from the pair table
                  TRUE ~ if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
                  )) %>% 
                mutate(single_frac_yield = pair$yield) %>% 
                mutate(single_frac_yield2 = if_else(is.na(pair$yield2) | (pair$yield2 == ""), pair$yield, pair$yield2)) %>% 
                mutate(unit = pair$yield_unit1) %>% 
                mutate(fracture_type = "fracture/yield")
              
              # If the depth value doesn't exist, creating a new row to add these data
            }else{
              row2 <- row
              # Setting some values to NA as it doesn't make sense to have these copied over
              row2$record_index <- NA_character_
              # Adding relevant values
              row2$depth_from <- pair$depth
              row2$depth_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
              row2$fracture_from <- pair$depth
              row2$fracture_to <- if_else(is.na(pair$depth2) | (pair$depth2 == ""), pair$depth, pair$depth2)
              row2$single_frac_yield <- pair$yield
              row2$single_frac_yield2 = if_else(is.na(pair$yield2) | (pair$yield2 == ""), pair$yield, pair$yield2)
              row2$unit <- pair$yield_unit1
              # A comment to highlight the fact that this is a new addition
              row2$lithology <- "row added to account for fracture. No row-specific comment"
              # Adding a comment indiciating that this row contains fracture and yield information
              row2$fracture_type <- "fracture/yield"
              
              # Appending this row to the out_table, to be added to the dataframe later
              row2 <- map(row2, as.character)
              out_table <- bind_rows(row2, out_table)
            }
          }
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for pairs", df$wtn[1]))
      error_wtns <<- append(error_wtns, df$wtn[1])
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
          row2$record_index <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row2$depth_from <- "0"
          row2$depth_to <- as.character(max(as.numeric(out_table$depth_to), na.rm = T))
          row2$cum_frac_yield <- yields$yield
          row2$unit <- yields$yield_unit
          # A comment to highlight the fact that this is a new addition
          row2$lithology <- "row added to account for cumulative well yield. No row-specific comment"
          # Adding a comment indiciating that this row contains only yield information
          row2$fracture_type <- "yield"
          # Appending the row to the out_table
          row2 <- map(row2, as.character)
          out_table <- bind_rows(row2, out_table)
        }
      }
    },
    error = function(e){
      print(paste("Error in processing row while looking for yields", df$wtn[1]))
      error_wtns <<- append(error_wtns, df$wtn[1])
    })
    # Appending the original row from this iteration to the out_table, whether or not it has been edited
    row <- map(row, as.character)
    out_table <- bind_rows(row, out_table)
  }
  return(out_table %>% arrange(as.numeric(depth_from), as.numeric(depth_to), as.numeric(record_index)))
}

# A function that takes a dataframe, goes through the general comment associated with the wtn whose data is in the dataframe and uses data extraction
# functions to add rows/values where applicable
extract_comment_data <- function(df){
  print(df$wtn[1])
  # Ensuring that all columns are characters
  df <- df %>% mutate_all(as.character)
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
    return(df %>% arrange(as.numeric(depth_from), as.numeric(depth_to), as.numeric(record_index)))
  }
  
  # If fractures are detected
  tryCatch({
    if(nrow(fractures)> 0){
      print("checking fractures")
      # iterating through the fractures
      for(frac in rows(fractures)){
        # If the found fractures are containined by a row already within the table, and there are no fracture values already associated with the row
        # for this range, editing this row by adding the fracture values
        if(nrow(find_depth_range(frac$depth, df)) > 0){
          df[df$depth_from %in% find_depth_range(frac$depth, df)$depth_from, ]$fracture_from <- frac$depth
          df[df$depth_from %in% find_depth_range(frac$depth, df)$depth_from, ]$fracture_tp <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          df[df$depth_from %in% find_depth_range(frac$depth, df)$depth_from, ]$fracture_type <- "fracture"
        # If either the fractures found are not within any of the existing ranges, or fracture values that do exist are not the same as those that we
        # are adding, creating a new row to store these data
        }else{
          # Copying any row and setting some values to NA since we don't actually have info about them as well as setting its depth range to just the
          # range of the fracture
          row <- df[1, ]
          # Setting some values to NA as it doesn't make sense to have these copied over
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$matclass <- NA_character_
          row$record_index <- NA_character_
          row$single_frac_yield <-NA_character_
          row$single_frac_yield2 <- NA_character_
          row$cum_frac_yield <- NA_character_
          row$unit <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row$depth_from <- frac$depth
          row$depth_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          row$fracture_from <- frac$depth
          row$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
          row$fracture_type <- "fracture"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for yields", df$wtn[1]))
    error_wtns <<- append(error_wtns, df$wtn[1])
  })
  
  # If pairs are detected
  tryCatch({
    if(nrow(pairs)> 0){
      print("checking pairs")
      # iterating through the pairs
      for(pair in rows(pairs)){
        # First identifying which depth range the pair exists in. If there is such a row where both the from and to depths match exactly with those from
        # the pair we're adding, using those as the index. If not, then comparing the available ranges to see where the pair might fall
        index_test <- (as.numeric(pair$depth) == as.numeric(df$depth_from) & as.numeric(pair$depth) == as.numeric(df$depth_to))
        row_index <- if_else(sum(index_test) > 0, 
                             list(index_test), 
                             list(as.numeric(pair$depth) >= as.numeric(df$depth_from) & as.numeric(pair$depth) <= as.numeric(df$depth_to)))
        row_index <- unlist(row_index)
        # If the found pairs lie within any of the existing lithology ranges and the associated fracture column for this row is empty
        if( nrow(find_depth_range(pair$depth, df)) > 0 ){
          # If the fracture_from is missing, replacing it with the value from the current pair. If it is the same as the current value, leaving it as
          # is. If there already exists a value, leaving it as-is.
          df[row_index,]$fracture_from <- case_when(is.na(df[row_index,]$fracture_from) ~ pair$depth,
                                                    df[row_index,]$fracture_from == pair$depth ~ df[row_index,]$fracture_from,
                                                    TRUE ~ df[row_index,]$fracture_from)
          # Creating a temporary variable for the fracture_to value
          temp_depth2 <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          temp_yield2 <- if_else(is.na(pair$yield2), pair$yield, pair$yield2)
          df[row_index,]$fracture_to <- case_when(is.na(df[row_index,]$fracture_to) ~ temp_depth2,
                                                  df[row_index,]$fracture_to == temp_depth2 ~ df[row_index,]$fracture_to,
                                                  TRUE ~ df[row_index,]$fracture_to)
          df[row_index,]$single_frac_yield <- case_when(is.na(df[row_index,]$single_frac_yield) ~ pair$yield,
                                                        df[row_index,]$single_frac_yield == pair$yield ~ df[row_index,]$single_frac_yield,
                                                        TRUE ~ df[row_index,]$single_frac_yield)
          df[row_index,]$single_frac_yield2 <- case_when(is.na(df[row_index,]$single_frac_yield2) ~ temp_yield2,
                                                        df[row_index,]$single_frac_yield2 == temp_yield2 ~ df[row_index,]$single_frac_yield2,
                                                        TRUE ~ df[row_index,]$single_frac_yield2)
          df[row_index,]$unit <- case_when(is.na(df[row_index,]$unit) ~ pair$yield_unit1,
                                           df[row_index,]$unit == pair$yield_unit1 ~ df[row_index,]$unit ,
                                           TRUE ~ df[row_index,]$unit)
          df[row_index,]$fracture_type <- "fracture/yield"
          # If either the pairs found are not within any of the existing ranges, or fracture-yield values exist already for the present range, creating a
          # new row and adding it to the dataframe
        }else{
          # Copying any row and setting some values to NA since we don't actually have info about them as well as setting its depth range to just the
          # range of the fracture
          row <- df[1, ]
          # Setting some values to NA as it doesn't make sense to have these copied over
          row$record_index <- NA_character_
          row$lithology <- "row added to account for fracture. No row-specific comment"
          row$matclass <- NA_character_
          row$cum_frac_yield <- NA_character_
          # Adding relevant values. Setting the depth range to the depth of the whole well
          row$depth_from <- pair$depth
          row$depth_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          row$fracture_from <- pair$depth
          row$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          row$single_frac_yield <- pair$yield
          row$single_frac_yield2 <- if_else(is.na(pair$yield2), pair$yield, pair$yield2)
          row$unit <- pair$yield_unit1
          row$fracture_type <- "fracture/yield"
          df <- bind_rows(row, df)
        }
      }
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for pairs", df$wtn[1]))
    error_wtns <<- append(error_wtns, df$wtn[1])
  })
  
  # If yields are detected
  tryCatch({
    if(!is.na(yields$yield)){
      print("checking yields")
      # Selecting the last row in the dataframe
      df <- arrange(df, as.numeric(depth_from))
      last <- df[nrow(df),]
      # Updating the cumulative yield value in this row, with the value extracted
      last$cum_frac_yield <- yields$yield
      last$unit <- yields$unit
      last$fracture_type <- if_else(is.na(last$fracture_from), "yield", "fracture/yield")
      # Updating the table with this new row
      df[nrow(df),] <- last
    }
  },
  warning = function(w){
    print(paste("Error in processing row while looking for yields", df$wtn[1]))
    error_wtns <<- append(error_wtns, df$wtn[1])
  })
  
  # Return the manipulated table
  return(df %>% arrange(as.numeric(depth_from), as.numeric(depth_to), as.numeric(record_index)))
}
