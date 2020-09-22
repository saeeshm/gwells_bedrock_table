# !usr/bin/env Rscript

# Author: Saeesh Mangwani 
# Date: 2020-09-10 

# Description: A script containing functions which call the regex parsing functions on the bedrock dataset to extract fracture and yield data and
# organize it into a table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringi)
source("regex_parsing_functions.R")

# ==== Initializing Global Variables ====

# Creating a global variable named error_wtns that stores the tag numbers for wells that throw an error in the classification process
error_wells <- as_tibble(list("wtn" = character(), "error_comment" = character()))


# ==== Data Extraction and Table Organization Function (Highest Level) ====

# A function that takes a dataframe, goes through all of its lithology comments and adds values and rows using data extraction functions where
# applicable
extract_litho_data <- function(df){
  # Print the wtn being worked on
  print(df$wtn[1])
  # Creating an empty out_table to store the resulting data
  out_table <- df[0,] %>% mutate_all(as.character)
  
  # Iterating through all the rows in the passed dataframe
  for(row in rows(df)){
    
    #checking for frac-yield pairs
    tryCatch({
      pairs <- getYieldFracPairs(row$lithology)
    },error = function(e){
      message <- paste("Error in extracting fracture-yield pairs for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # For fractures
    tryCatch({
      fractures <- getFracVals(row$lithology)
    },error = function(e){
      message <- paste("Error in extracting fracture information for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
    # If fractures or pairs were present, removing them from the string being considered
    temp <- ifelse(nrow(pairs) > 0, 
                   str_remove_all(row$lithology, str_replace_all(paste(pairs$string, collapse = "|"), "\\(", "\\\\(")), 
                   row$lithology)
    temp <- ifelse(nrow(fractures) > 0, 
                   str_remove_all(temp, str_replace_all(paste(fractures$string, collapse = "|"), "\\(", "\\\\(")), 
                   temp)
    # Having now removed any strings that were used to identify fractures or yield value pairs, we finally check for yields alone from the remnant string
    tryCatch({
      yields <- getYieldVals(temp)
    },error = function(e){
      message <- paste("Error in extracting yield information for well", row$wtn, "from the lithology comment for row index", row$record_index)
      print(message)
      error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
      error_wells <<- bind_rows(error_wells, error_row)
    })
    
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
      error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for fractures")
      error_wells <<- bind_rows(error_wells, error_row)
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
      error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for pairs")
      error_wells <<- bind_rows(error_wells, error_row)
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
      error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for yields")
      error_wells <<- bind_rows(error_wells, error_row)
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
  
  # Getting pair information
  tryCatch({
    pairs <- getYieldFracPairs(comment)
  },error = function(e){
    message <- paste("Error in extracting fracture-yield pairs from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # Getting fracture information
  tryCatch({
    fractures <- getFracVals(comment)
  },error = function(e){
    message <- paste("Error in extracting fracture information from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If fractures or pairs were present, removing them from the string being considered
  temp <- ifelse(nrow(pairs) > 0, str_remove_all(comment, paste(pairs$string, collapse = "|")), comment)
  temp <- ifelse(nrow(fractures) > 0, str_remove_all(temp, paste(fractures$string, collapse = "|")), temp)
  # Having now removed any strings that were used to identify fractures or yield value pairs, we finally check for yields alone from the remnant string
  tryCatch({
    yields <- getYieldVals(temp)
  },error = function(e){
    message <- paste("Error in extracting yield information from well", df$wtn[1], "in general remarks")
    print(message)
    error_row <- list("wtn" = row$wtn[1], "error_comment" = message)
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
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
          df[df$depth_from %in% find_depth_range(frac$depth, df)$depth_from, ]$fracture_to <- if_else(is.na(frac$depth2), frac$depth, frac$depth2)
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
    print(paste("Error in processing row while looking for fractures", df$wtn[1]))
    error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for fractures")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # If pairs are detected
  tryCatch({
    if(nrow(pairs)> 0){
      print("checking pairs")
      # iterating through the pairs
      for(pair in rows(pairs)){
        # If the found pairs lie within any of the existing lithology ranges and the associated fracture column for this row is empty
        if( nrow(find_depth_range(pair$depth, df)) > 0 ){
          # If the fracture_from is missing, replacing it with the value from the current pair. If it is the same as the current value, leaving it as
          # is. If there already exists a value, leaving it as-is.
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$fracture_from <- pair$depth
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$fracture_to <- if_else(is.na(pair$depth2), pair$depth, pair$depth2)
          # Creating a temporary variable for the fracture_to value
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$single_frac_yield <- pair$yield
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$single_frac_yield2 <- if_else(is.na(pair$yield2), pair$yield, pair$yield2)
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$unit <- pair$yield_unit1
          df[df$depth_from %in% find_depth_range(pair$depth, df)$depth_from, ]$fracture_type <- "fracture/yield"
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
    error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for pairs")
    error_wells <<- bind_rows(error_wells, error_row)
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
    error_row <- list("wtn" = df$wtn[1], "error_comment" = "Error in processing row while looking for yields")
    error_wells <<- bind_rows(error_wells, error_row)
  })
  
  # Return the manipulated table
  return(df %>% arrange(as.numeric(depth_from), as.numeric(depth_to), as.numeric(record_index)))
}


# ==== Regular Expression Parsing Functions (Intermediate Level) ====

# A function that applies regular expression matching to return a dataframe of yield-fracture pairs extracted from a given comment
getYieldFracPairs <- function(comment){
  # Looking for yield-value pairs using the paired regex
  pattern <- "((?:frac\\D*(?:ft|feet)?|p\\.l\\.?\\s*|pump(?:ing)?\\D*|heav(?:ing)?\\D*)?(?:(\\.?\\d+(?:(?:\\/|\\.)\\d+)?)\\s?(gpm|gph|gpd|usgpm|ukgpm|ft|feet|')?\\s?(?:(to|-+)\\s?(\\.?\\d+(?:(?:\\/|\\.)\\d+)?)\\s?(gpm|gph|gpd|usgpm|ukgpm|ft|feet|')?)?)|(?:w\\.?b|water|trickle|trace|moisture))\\s?(@|-+|at|in|\\s*\\()\\s?(\\.?\\d+(?:(?:\\/|\\.)\\d+)?)\\s?(gpm|gph|gpd|usgpm|ukgpm|ft|feet|')?\\s?(?:(to|-+)\\s?(\\.?\\d+(?:(?:\\/|\\.)\\d+)?)\\s?(gpm|gph|gpd|usgpm|ukgpm|ft|feet|')?)?"
  pairs <- str_match_all(comment, pattern)[[1]]
  
  # If pairs are detected and are not all missing, checking to see the order in which yield depth values are
  # presented, and organizing a resulting dataframe such that this process is preserved
  if( (sum(!is.na(pairs)) > 0) & (nrow(pairs) > 0) ){
    
    # An empty "temp" matrix to store only those pairs that have been cleaned for the phrases "fracture" and
    # "pumping level"
    temp <- pairs[0,]
    
    # Iterating through all the rows of matches to remove inconsistencies 
    for (i in seq(nrow(pairs))){
      # If the words p.l or pumping level or heaving are found, skipping this match
      if(str_detect(pairs[i, 1], "(p\\.l\\.?)|(pump(?:ing)?\\D*)|(?:heav(?:ing)?\\D*)")){
        next
        # if the unit columns are not empty and are exactly the same for both columns (that is both are depth or both are yield units), this isn't a
        # paired value so it needs to be dropped
      }else if(!is.na(pairs[i,4]) & !is.na(pairs[i,10]) &
               (str_detect(pairs[i,4],"gpm|gph|gpd|ukgpm|usgpm") & str_detect(pairs[i,10],"gpm|gph|gpd|ukgpm|usgpm") | 
                str_detect(pairs[i,4],"ft|feet|'") & str_detect(pairs[i,10],"ft|feet|'")) ){
        next
        # The same check but for the second unit columns
      }else if(!is.na(pairs[i,7]) & !is.na(pairs[i,13]) &
               (str_detect(pairs[i,7],"gpm|gph|gpd|ukgpm|usgpm") & str_detect(pairs[i,13],"gpm|gph|gpd|ukgpm|usgpm") | 
                str_detect(pairs[i,7],"ft|feet|'") & str_detect(pairs[i,13],"ft|feet|'")) ){
        next
        # If there is a single predominant way of specifying the "at" part of the pair (i.e most pairs are formatted in a certain style of x gpm "at" y
        # ft), and there is a row that does not meet this pattern AND does not contain both units, removing it from the pairs being considered as we're
        # not certain enough that it is a frac-yield pair. See the helper function specification below for how this function completes this comparison.
      }else if(is_common_connector(i, pairs) & (all(is.na(pairs[i,c(4,7)])) | all(is.na(pairs[i,c(10,13)]))) ){
        next
        # If the phrase "frac" or related phrases are found in a match, or there is only 1 pair that's
        # been found
      }else if( str_detect(pairs[i, 1], "frac\\D*") | nrow(pairs) == 1){
        # Checking whether the units associated with the values are not the same, and are both present. That
        # is, either there must be a depth-yield or a yield-depth unit pair. In the absence of these, we don't
        # have enough confidence to know whether this is a pair or just a range, so we must skip it
        first_test <- (any(stri_detect(pairs[i, c(4,7)], regex="ft|feet|'")) & 
                         any(stri_detect(pairs[i, c(10,13)], regex="gpm|gph|gpd|ukgpm|usgpm")))
        sec_test <- (any(stri_detect(pairs[i, c(4,7)], regex="gpm|gph|gpd|ukgpm|usgpm")) & 
                       any(stri_detect(pairs[i, c(10,13)], regex="ft|feet|'")))
        
        # Checking if any of the above conditions have been met (i.e, that the found pair is either yield-frac
        # or frac-yield). The condition check is broken into 2 steps to correct for the strange behaviour in R
        # where TRUE and NA equates to NA, while FALSE and NA equates to FALSE.
        if( if_else(is.na(first_test), FALSE, first_test) | if_else(is.na(sec_test), FALSE, sec_test) ){
          # If the pair is valid, adding it to the temp table 
          temp <- rbind(temp, pairs[i, ])
        }
        # If none of these questionable phrases are found, adding the match result directly to the temp table  
      }else{
        temp <- rbind(temp, pairs[i,])
      }
    }
    # Overwriting the pairs variable with the now filtered pairs
    pairs <- temp
    
    # Next, finding the appropriate units for each of these pairs. If the latter column contains depth units AND the former contains yield units,
    # naming the columns as such by creating a vector of names to add to the table
    if((sum(str_detect(pairs[,c(4,7)], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0 & 
        sum(str_detect(pairs[,c(10,13)], "ft|feet|'"), na.rm = T) > 0) ){
      # yield <- list("num" = 3, "unit" = 4)
      # depth <- list("num" = 9, "unit" = 10)
      names <- c("string", "match_col", "yield", "yield_unit1","x5", "yield2", "yield_unit2", "at", "depth", "depth_unit1", "x11", "depth2", "depth_unit2")
      # If either the latter column contains yield units AND the former depth units, assigning them as such
    }else if( (sum(str_detect(pairs[,c(10,13)], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0) &
              (sum(str_detect(pairs[,c(4,7)], "ft|feet|'"), na.rm = T) > 0) ){
      # yield <- list("num" = 9, "unit" = 10)
      # depth <- list("num" = 3, "unit" = 4)
      names <- c("string", "match_col","depth", "depth_unit1", "x5", "depth2", "depth_unit2", "at", "yield", "yield_unit1",  "x11", "yield2", "yield_unit2")
      # If none of the units are found, we do not know what the pattern of information is and so the pairs are flagged for review and the table is returned right away.
    }else{ 
      # yield <- list("num" = 2, "unit" = 4)
      # depth <- list("num" = 9, "unit" = 10)
      names <- c("string", "match_col", "yield", "yield_unit1","x5", "yield2", "yield_unit2", "at", "depth", "depth_unit1", "x11", "depth2", "depth_unit2")
      # Turning the output matrix into a named df and setting the values to allow for review before returning
      # it right away
      pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
      names(pairs) <- names
      pairs <- pairs %>% mutate(yield = "review", depth = "review", depth_unit1 = "ft")
      return(pairs %>% select(string, match_col, yield, yield_unit1, yield2, yield_unit2, at, depth, depth_unit1, depth2, depth_unit2) %>% distinct())
    }
    
    # If valid units are found, turning the output matrix into a named df to make it easier to work with and
    # giving it the right names
    pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
    names(pairs) <- names
    
    # Replacing all "trace" indicators with a 0.01, which is a numeric unit chosen to to refer a trickle
    pairs <- pairs %>% 
      mutate(yield = case_when(
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(yield_unit1, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(yield) ~ yield,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(depth_unit1, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(depth) ~ depth,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(yield_unit2, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(yield2) ~ yield2,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(depth_unit2, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(depth2) ~ depth2,
        is.na(yield) ~ "0.01",
        TRUE ~ yield
      )) %>% 
      mutate(yield_unit1 = case_when(
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(yield_unit1, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(yield) ~ yield_unit1,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(depth_unit1, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(depth) ~ depth_unit1,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(yield_unit2, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(yield2) ~ yield_unit2,
        str_detect(match_col, "water|trickle|trace|moisture") & str_detect(depth_unit2, "gpm|gph|gpd|ukgpm|usgpm") & !is.na(depth2) ~ depth_unit2,
        is.na(yield_unit1) ~ "gpm",
        TRUE ~ yield_unit1
      ))
    
    # Checking for any redunancies introduced through the above replacement and removing them
    pairs <- pairs %>% 
      # If there is depth and yield column that match exactly on both value and unit, it is a redundancy and is marked for removal
      mutate(depth = case_when(
        !is.na(depth) & !is.na(yield) & (yield == depth) & (yield_unit1 == depth_unit1) ~ "remove",
        !is.na(depth) & !is.na(yield2) & (yield2 == depth) & (yield_unit2 == depth_unit1) ~ "remove",
        TRUE ~ depth)) %>% 
      # If this depth column has been marked, marking the unit value as well
      mutate(depth_unit1 = case_when(
        !is.na(depth) & (depth == "remove") ~ "remove",
        TRUE ~ depth_unit1)) %>% 
      # The same logic for comparing with the second depth column
      mutate(depth2 = case_when(
        !is.na(depth2) & !is.na(yield) & (yield == depth2) & (yield_unit1 == depth_unit2) ~ "remove",
        !is.na(depth2) & !is.na(yield2) & (yield2 == depth2) & (yield_unit2 == depth_unit2) ~ "remove",
        TRUE ~ depth2)) %>% 
      mutate(depth_unit2 = case_when(
        !is.na(depth2) & (depth2 == "remove") ~ "remove",
        TRUE ~ depth_unit2)) %>% 
      # Turning any marked values to NA
      mutate_all(list( ~ case_when(
        str_detect(., "remove") ~ NA_character_,
        TRUE ~ .
      )))
    # For each row in the dataframe, checking for consistency in the storage of yield-depth pairs and filling
    # in empty units
    
    # First creating an empty out-table to store the results from this iteration
    out_table <- pairs[0,]
    
    # iterating
    for (row in rows(pairs)){
      
      # If any units are found in the wrong column, inverting the values and units in that column
      if( (!is.na(row$yield_unit1) & str_detect(row$yield_unit1, "ft|feet|'")) |
          (!is.na(row$yield_unit2) & str_detect(row$yield_unit2, "ft|feet|'")) | 
          (!is.na(row$depth_unit1) & str_detect(row$depth_unit1, "gpm|gph|gpd|ukgpm|usgpm")) | 
          (!is.na(row$depth_unit2) & str_detect(row$depth_unit2, "gpm|gph|gpd|ukgpm|usgpm")) ){
        # Inverting values for all relevant columns
        temp <- row$yield
        temp2 <- row$yield2
        row$yield <- row$depth
        row$yield2 <- row$depth2
        row$depth <- temp
        row$depth2 <- temp2
        # Doing a similar inversion for the unit columns
        temp <- row$yield_unit1
        temp2 <- row$yield_unit2
        row$yield_unit1 <- row$depth_unit1
        row$yield_unit2 <- row$depth_unit2
        row$depth_unit1 <- temp
        row$depth_unit2 <- temp2
      }
      
      # If any units are missing or are in the wrong column, replacing them with the appropriate defaults ones
      row$yield_unit1 <- case_when(
        is.na(row$yield_unit1) ~ select_unit(pairs$yield_unit1),
        str_detect(row$yield_unit1, "ft|feet|'") ~ select_unit(pairs$yield_unit1),
        TRUE ~ row$yield_unit1
      )
      
      row$yield_unit2 <- case_when(
        is.na(row$yield_unit2) ~ select_unit(pairs$yield_unit2),
        str_detect(row$yield_unit2, "ft|feet|'") ~ select_unit(pairs$yield_unit2),
        TRUE ~ row$yield_unit2
      )
      
      row$depth_unit1 <- case_when(
        is.na(row$depth_unit1) ~ select_unit(pairs$depth_unit1, T),
        str_detect(row$depth_unit1, "ft|feet|'") ~ select_unit(pairs$depth_unit1, T),
        TRUE ~ row$depth_unit1
      )
      
      row$depth_unit2 <- case_when(
        is.na(row$depth_unit2) ~ select_unit(pairs$depth_unit2, T),
        str_detect(row$depth_unit2, "ft|feet|'") ~ select_unit(pairs$depth_unit2, T),
        TRUE ~ row$depth_unit2
      )
      
      # Replacing "'" with ft in the depth unit
      row$depth_unit1 <- if_else(row$depth_unit1 == "'", "ft", row$depth_unit1)
      row$depth_unit2 <- if_else(row$depth_unit2 == "'", "ft", row$depth_unit2)
      
      # Binding the row to the temporary out_table
      out_table <- bind_rows(out_table, row)
    }
    
    # Overwriting the pairs table with the edited out_table from the previous iteration
    pairs <- out_table
    # 
    # # If either the primary yield or the depth unit is missing, altering the row and flagging it for review
    # pairs <- pairs %>% 
    #   mutate(yield = if_else(is.na(yield), "review", yield)) %>% 
    #   mutate(yield = if_else(is.na(depth), "review", yield)) %>% 
    #   mutate(depth = if_else(is.na(depth), "review", depth))
    # 
    # Returning the dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield_unit1, yield2, yield_unit2, depth, depth_unit1, depth2, depth_unit2) %>% 
             distinct() %>% 
             arrange(as.numeric(depth)))
  }else{
    # If no matches are found, returning an empty dataframe (it is still named with the default naming pattern
    # for consistency)
    pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
    names(pairs) <- c("string", "match_col","yield", "yield_unit1", "x5","yield2", "yield_unit2", "at", "depth", "depth_unit1", "x11", "depth2", "depth_unit2")
    # Returning the empty dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield_unit1, yield2, yield_unit2, depth, depth_unit1, depth2, depth_unit2) %>% 
             distinct())
  }
}

# A function that applies regular expression matching to return a vector of yield values extracted from a given comment
getYieldVals <- function(comment){
  # Looking for yield values using the yield-only regex. First looking for the world "yield" and if found
  # splitting the string on that word to only take the latter half of the comment
  pattern <- "(pu?mp[A-Za-z0-9\\s:\\-@]*)(yi?e?ld[A-Za-z _\\.,:\\-]{0,10})?((?:\\d+(?:\\/)?)\\.{0,1}\\d*)+\\s*(&|,|-|and|to)?\\s*((?:\\d+(?:\\/)?)\\.{0,1}\\d*)?\\s*(gpm|gph|gpd|usgpm|ukgpm)(?![\\s\\S]*\\d+\\s?(?:ho?u?r|min))"
  
  # An empty dataframe to store the output of this function
  output <- tibble(yield = NA_character_, yield2 = NA_character_, unit = NA_character_, .rows = 1)
  
  # Using the pattern to extract matches
  yields <- str_match_all(comment, pattern)[[1]]
  
  # If no values are found or the values are all missing, returning an empty dataframe
  if( (sum(!is.na(yields)) == 0) | (nrow(yields) == 0) ){
    return(output)
  }
  
  # If the word pump or heave or any associated values is located in any row, removing that row from the row of yields
  if(str_detect(yields[, 2], "(p\\.l\\.?)|(pump(?:ing)?\\D*)|(?:heav(?:ing)?\\D*)")){
    temp <- yields[0,]
    for(i in 1:nrow(yields)){
      if(str_detect(yields[i, 2], "(p\\.l\\.?)|(pump(?:ing)?\\D*)|(?:heav(?:ing)?\\D*)")){
        next 
      }
      temp <- rbind(temp, yields[i,])
    }
    yields <- temp
  }
  
  # With the above filter complete, checking again if the table is empty and returning an empty dataframe if yes
  if( (sum(!is.na(yields)) == 0) | (nrow(yields) == 0) ){
    return(output)
    # If the second column is filled for any row, it means the word "yield" was detected, and we assume the
    # value associated with it as the cumulative yield of the row.
  }else if( sum(!is.na(yields[,2])) > 0 ){
    # We then loop through to extract this value and return it
    for(i in seq(nrow(yields))){
      if(!is.na(yields[i, 2])){ 
        output$yield <- yields[i, 3]
        output$yield2 <- yields[i, 4]
        output$unit <- yields[i, 5]
        break
      }
    }
    # If the word "yield" isn't found, then looking at how many measures were actually found. If it is exactly
    # one, returning it. If its more than one but they're all equal, returning that. Otherwise, returning the word review
  }else{
    output$yield <- case_when(nrow(yields) == 1 ~ str_trim(yields[1, 3]), 
                              nrow(yields) > 1 ~ "review")
    output$yield2 <- case_when(nrow(yields) == 1 ~ str_trim(yields[1, 4]), 
                               nrow(yields) > 1 ~ "review")
    output$unit <- yields[1, 6]
  }
  return(distinct(output))
}

# A function that applies regular expression matching to return a tibble of fracture values extracted from a given comment
getFracVals <- function(comment){
  # Looking for fracture values using the fracture-only regex. This is the first step of a multi part process
  pattern <- "((be?dro?ck|[fg]rac\\w*|seam|moisture|w\\.?b|(?:pump(?:\\w|\\s)+)?water)(?:[A-Za-z0-9\\s&,-\\/#]){0,50}(:|at|@|-|\\s|from)?\\s*)(((?:\\d+(?:\\/|\\s)?)\\.{0,1}\\d*)\\s?(ft|feet|gpm|gph|gpd|usgpm|ukgpm|')?\\s?((&|,|-|and|to|at|with)?\\s?((?:\\d(?:\\/|\\s)?)\\.{0,1}\\d*)?\\s?(ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?)*)(?!\\\\|\"|st|nd|rd|th|psi)"
  fracs <- str_extract_all(comment, pattern)[[1]]
  # If the expression above captures anything, then proceeding with the rest of the steps. Otherwise ignoring
  # and proceeding to the return statement
  if ( (sum(!is.na(fracs)) > 0) & (length(fracs) > 0) ){
    
    # Removing any captured expressions that contain the word "pump" or related, as this refers to pumping
    # depth not fracture depth and we don't want that. Creating a temp var to assist with this process
    temp <- c()
    for(i in seq(fracs)){
      # If the word pump is detected, skipping this row
      if(str_detect(fracs[i], "pu?mp")){
        next
        # If words related to pressure are detected
      }else if(str_detect(fracs[i], "pre?ssu?re|psi")){
        # Checking for and extracing only those numerics
        extract <- str_match_all(fracs[i], "((?:\\d+(?:\\/|\\s)?)\\.{0,1}\\d*)\\s*(ft|feet|')")[[1]]
        # If there are no appropriate numerics, skipping this row. Otherwise saving only the correct numerics back into the temp var.
        if(nrow(extract) < 1) next else temp <- append(temp, paste("frac @ ", extract[,1], collapse = ", "))
        # If there are no issues, simply adding the row to the temp table
      }else{
        temp <- append(temp, fracs[i])
      }
    }
    # Passing the now filtered expressions back into the fracs variable
    fracs <- temp
    # Collapsing them into one string separated by spaces
    fracs <- paste(fracs, collapse = ' ')
    
    # Now using second regex to extract the actual numeric values from the character strings extracted. Note
    # that it ignores any values followed by yield units
    fractures <- str_match_all(fracs, "(?<!#)(\\d+(?:\\/)?\\d*)+\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?(?:\\s*(-|to)\\s*(\\d+(?:\\/)?\\d*)?\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?)?")[[1]]
    # Turning this into a named dataframe
    fractures <- suppressMessages(as_tibble(fractures,.name_repair = "unique"))
    names(fractures) <- c("string", "depth", "to", "depth2")
    
    # If the length of the resulting dataframe is not 0, checking the unit associated with any numeric values captured and removing any rows where the
    # units are yield (instead of depth) related
    if(nrow(fractures) > 0){
      fractures <- fractures %>% filter(!str_detect(string, "gpm|gph|gpd|usgpm|ukgpm"))
    }
    
  }else{
    # If there are no fractures detected, returning an empty named dataframe using the same structure as the one that would be created above
    fractures <- tibble(string = character(), depth = character(), to = character(), depth2 = character(), .rows = 0)
  }
  return(fractures %>% mutate(string = str_squish(string)) %>% distinct() %>% arrange(as.numeric(depth)))
}



# ==== Helper Functions (Lowest Level) ====

# Function to help extract and iterate through the rows of a dataframe
rows <-  function(df) {
  lapply(seq_len(nrow(df)), function(i) unclass(df[i,,drop=F]))
}

# A helper function to select the right unit when reassigning a yield unit. It checks for whether units have otherwise been reported for other yield
# values and if so, uses the most common reported unit. Otherwise, it just uses the default unit
select_unit <- function(unit_vector, depth = F){
  # If there are no units reported, returning gpm. Otherwise returning the unit most frequently reported
  output <- suppressWarnings(case_when(
    !isTRUE(depth) & is.na(max(unit_vector, na.rm = T)) ~ "gpm", 
    !isTRUE(depth) ~ max(unit_vector, na.rm = T),
    isTRUE(depth) & is.na(max(unit_vector, na.rm = T)) ~ "ft", 
    isTRUE(depth) ~ max(unit_vector, na.rm = T)
  ))
  return(output)
}

# Function that takes a dataframe and completes its cumulative and single yield columns
fill_yield_vals <- function(df){
  # Arranging the dataframe in the right order and turning the yield columns to numeric
  df <- df %>% arrange(as.numeric(record_index), as.numeric(depth_from), as.numeric(depth_to)) %>% 
    mutate(single_frac_yield = as.numeric(single_frac_yield)) %>% 
    mutate(cum_frac_yield = as.numeric(cum_frac_yield))
  
  # A variable that stores the present cumulative yield and single yield value
  curr_cumulative <- 0
  curr_single <- 0
  
  # Iterating through all the rows in the dataframe
  df_rows <- rows(df)
  for (i in seq(df_rows)){
    # Updating first the value of the current single. If there is a single_frac_value present for this, it is assigned as the current single
    curr_single <- case_when(!is.na(df_rows[[i]]$single_frac_yield) ~ df_rows[[i]]$single_frac_yield,
                             # If there is no single value, but there is a cumulative value, the current single is the difference between the
                             # cumulative on this row and the cumulative stored so far
                             is.na(df_rows[[i]]$single_frac_yield) & !is.na(df_rows[[i]]$cum_frac_yield) ~ (df_rows[[i]]$cum_frac_yield - curr_cumulative),
                             # if neither are present, the current single is 0
                             is.na(df_rows[[i]]$single_frac_yield) & is.na(df_rows[[i]]$cum_frac_yield) ~ 0)
    # Next updating the current cumulative. If there is a single value present, the sum of that plus the cumulative stored so far is the current
    # cumulative
    curr_cumulative <- case_when(!is.na(df_rows[[i]]$single_frac_yield) ~ (curr_cumulative + df_rows[[i]]$single_frac_yield),
                                 # If there is no single value, but there is a current cumulative value, the new cumulative value simply
                                 # replaces/updates the old one
                                 is.na(df_rows[[i]]$single_frac_yield) & !is.na(df_rows[[i]]$cum_frac_yield) ~ df_rows[[i]]$cum_frac_yield,
                                 # If neither are present, the value is not updated as we assume a 0 addition to the cumulative yield on this row
                                 is.na(df_rows[[i]]$single_frac_yield) & is.na(df_rows[[i]]$cum_frac_yield) ~ curr_cumulative)
    
    # With the values now updated, overwriting the values stored in the row with the clarified values
    df_rows[[i]]$cum_frac_yield <- curr_cumulative
    df_rows[[i]]$single_frac_yield <- curr_single
  }
  out_table <- bind_rows(df_rows)
  return(out_table)
}

# A function that takes the depth of an inputted fracture and returns the row where its data should best be added. If there is no good
# row, and a new row is needed, the function returns an empty table
find_depth_range <- function(depth, df){
  # Ensuring all values are numeric
  df$depth_from <- as.numeric(df$depth_from)
  df$depth_to <- as.numeric(df$depth_to)
  depth <- as.numeric(depth)
  
  # Filtering out only those rows where the inputted depth is >= the from_depth AND is <= the to_depth
  temp <- df[depth >= df$depth_from,]
  temp <- temp[depth <= temp$depth_to,]
  
  # If there are rows in which meet this condition:
  if(nrow(temp) > 0){
    # Filtering only those whose fracture data is still empty
    temp <- temp[is.na(temp$fracture_from),]
    # If there is more than one appropriate row remaining, selecting the one with the lowest from_depth.
    temp <- temp[temp$depth_from == suppressWarnings(min(temp$depth_from)),]
  }
  # Returning the row
  return(temp)
}

# A function that checks for the connector used to extablish the relationship between depth and yield in a pair. It checks whether the connector for
# the present row is the same as the most common. It returns a boolean value
is_common_connector <- function(i, pairs){
  # If there is no single type of connector that is predominant, returning FALSE as then this test is meaningless
  if(all(table(str_squish(pairs[,8])) == 1)){
    return(FALSE)
  }else{
    # Otherwise, checking to see if the connector at the current row is not equal to the most common connector. If it is the same, returning F as we
    # don't need this check. Otherwise returning T.
    return(str_squish(pairs[i,9]) == names(sort(table(str_squish(pairs[,9])), decreasing = T)[1]))
  }
}


