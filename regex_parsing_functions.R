# Author: Saeesh Mangwani
# Date: Fri Sep  4 09:37:26 2020
# Title: Regex Parsing Functions for the GWells Bedrock table
# Desc: Helper functions to assist with the extraction of fracture, yield and frac-yield pair data from the lithology and general comments associated with wells in the gwells table

# libraries
library(tidyverse)
library(stringi)

# A function that applies regular expression matching to return a dataframe of yield-fracture pairs extracted from a given comment
getYieldFracPairs <- function(comment){
  # Looking for yield-value pairs using the paired regex
  pattern <- "(((?:frac\\D*(ft|feet)?)|(?:p\\.l\\.?\\s*)|(?:pumping\\D*))?(?:(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)(\\s*(?:to|-|, |\\s)+\\s*)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)?(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm|\\s*ft|\\s*feet|\\s*')*)|(?:water|trickle|trace|moisture))(\\s*(?:@|-|at|in|, )\\s*)+(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)(\\s*(?:to|-|, |\\s)+\\s*)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)*(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm|\\s*ft|\\s*feet|\\s*')*"
  pairs <- str_match_all(comment, pattern)[[1]]
  
  # If pairs are detected and are not all missing, checking to see the order in which yield depth values are
  # presented, and organizing a resulting dataframe such that this process is preserved
  if( (sum(!is.na(pairs)) > 0) & (nrow(pairs) > 0) ){
    
    # An empty "temp" matrix to store only those pairs that have been cleaned for the phrases "fracture" and
    # "pumping level"
    temp <- pairs[0,]
    
    # Iterating through all the rows of matches to remove inconsistencies 
    for (i in seq(nrow(pairs))){
      # If the words p.l or pumping level are found, skipping this match
      if(str_detect(pairs[i, 1], "(p\\.l\\.?)|(pumping\\D*)")){
        next
        # If the phrase "frac" or related phrases are found in a match, or there is only 1 pair that's
        # been found
      }else if( str_detect(pairs[i, 1], "frac\\D*") | nrow(pairs) == 1){
        # Checking whether the units associated with the values are not the same, and are both present. That
        # is, either there must be a depth-yield or a yield-depth unit pair. In the absence of these, we don't
        # have enough confidence to know whether this is a pair or just a range, so we must skip it
        first_test <- (stri_detect(pairs[i, 8], regex="\\s*ft|\\s*feet|\\s*'") & 
                         stri_detect(pairs[i, 13], regex="\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm"))
        sec_test <- (stri_detect(pairs[i, 8], regex="\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm") & 
                       stri_detect(pairs[i, 13], regex="\\s*ft|\\s*feet|\\s*'"))
        
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
    
    # Next, finding the appropriate units for each of these pairs. If the latter column contains depth units
    # AND the former contains yield units, naming the columns as such by creating a vector of names to add to
    # the table
    if( (sum(str_detect(pairs[,8], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0) & 
        (sum(str_detect(pairs[,13], "ft|feet|'"), na.rm = T) > 0) ){
      yield <- list("num" = 5, "unit" = 8)
      depth <- list("num" = 10, "unit" = 14)
      names <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
      # If either the latter column contains yield units AND the former depth units, assigning them as such
    }else if( (sum(str_detect(pairs[,13], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0) & 
              (sum(str_detect(pairs[,8], "ft|feet|'"), na.rm = T) > 0) ){
      yield <- list("num" = 10, "unit" = 14)
      depth <- list("num" = 5, "unit" = 8)
      names <- c("string", "x2", "x3", "x4","depth", "x6", "depth2", "depth_unit", "x9", "yield", "x11", "yield2", "yield_unit")
      # If none of the units are found, we do not know what the pattern of information is and so the pairs are flagged for review and the table is returned right away.
    }else{ 
      yield <- list("num" = 2, "unit" = 5)
      depth <- list("num" = 8, "unit" = 11)
      names <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
      # Turning the output matrix into a named df and setting the values to allow for review before returning
      # it right away
      pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
      names(pairs) <- names
      pairs <- pairs %>% mutate(yield = "review", depth = "0", depth_unit = "ft")
      return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
    }
    
    # If valid units are found, turning the output matrix into a named df to make it easier to work with and
    # giving it the right names
    pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
    names(pairs) <- names
    
    # Replacing all "trace" indicators with a 0.01, which is a numeric unit chosen to to refer a trickle
    pairs <- pairs %>% 
      mutate(yield = if_else(str_detect(x2, "water|trickle|trace|moisture"), "0.01", yield)) %>% 
      mutate(yield_unit = if_else(str_detect(x2, "water|trickle|trace|moisture"), "gpm", yield_unit))
    
    # For each row in the dataframe, checking for consistency in the storage of yield-depth pairs and filling
    # in empty units
    
    # First creating an empty out-table to store the results from this iteration
    out_table <- pairs[0,]
    
    # iterating
    for (row in rows(pairs)){
      
      # If any units are found in the wrong column, inverting the values and units in that column
      if( str_detect(row$yield_unit, "ft|feet|'") | str_detect(row$depth_unit, "gpm|gph|gpd|ukgpm|usgpm")){
        # Inverting values
        temp <- row$yield
        row$yield <- row$depth
        row$depth <- temp
        # Assigning the right units in the unit columns, and using gpm/ft as defaults in case none are available:
        # First storing the yield unit in a temp variable before reassigning it.
        temp <- row$yield_unit
        # If the depth unit is NA, selecting the valid unit using the helper function
        row$yield_unit <- case_when(is.na(row$depth_unit) ~ select_unit(pairs$yield_unit),
                                    # if the depth unit is also feet, selecting the valid unit using the
                                    # helper function
                                    str_detect(row$depth_unit, "ft|feet|'") ~ select_unit(pairs$yield_unit),
                                    # Otherwise, using the value stored in the depth unit column, as it is a
                                    # valid inversion
                                    TRUE ~ row$depth_unit)
        # Applying the same logic for reassigning the depth unit
        row$depth_unit <- case_when(is.na(temp) ~ select_unit(pairs$depth_unit),
                                    str_detect(temp, "gpm|gph|gpd|ukgpm|usgpm") ~ select_unit(pairs$depth_unit),
                                    TRUE ~ temp)
      }
      
      # If any units are missing, replacing them with the appropriate defaults ones
      row$yield_unit <- if_else(is.na(row$yield_unit), "gpm", row$yield_unit)
      row$depth_unit <- if_else(is.na(row$depth_unit), "ft", row$depth_unit)
      # Replacing "'" with ft in the depth unit
      row$depth_unit <- if_else(row$depth_unit == "'", "ft", row$depth_unit)
      
      # Binding the row to the temporary out_table
      out_table <- bind_rows(out_table, row)
    }
    
    # Overwriting the pairs table with the edited out_table from the previous iteration
    pairs <- out_table
    
    # If either the primary yield or the depth unit is missing, altering the row and flagging it for review
    pairs <- pairs %>% 
      mutate(yield = if_else(is.na(yield), "review", yield)) %>% 
      mutate(yield = if_else(is.na(depth), "review", yield)) %>% 
      mutate(depth = if_else(is.na(depth), "0", depth))
    
    # Returning the dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
  }else{
    # If no matches are found, returning an empty dataframe (it is still named with the default naming pattern
    # for consistency)
    pairs <- suppressMessages(as_tibble(pairs, .name_repair = "unique"))
    names(pairs) <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
    # Returning the empty dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
  }
}


# A function that applies regular expression matching to return a vector of yield values extracted from a given comment
getYieldVals <- function(comment){
  # Looking for yield values using the yield-only regex. First looking for the world "yield" and if found
  # splitting the string on that word to only take the latter half of the comment
  pattern <- "(yield:?\\s*)?(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)+(?:\\s|&|,|-|and|to)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)*(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm)"
  
  # An empty dataframe to store the output of this function
  output <- tibble(yield = NA_character_, yield2 = NA_character_, unit = NA_character_, .rows = 1)
  
  # Using the pattern to extract matches
  yields <- str_match_all(comment, pattern)[[1]]
  
  # If no values are found or the values are all missing, returning an empty dataframe
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
    # Returning the output
    return(output)
    # If the word "yield" isn't found, then looking at how many measures were actually found. If it is exactly
    # one, returning it.
  }else{
    output$yield <- case_when(nrow(yields) == 1 ~ yields[1, 3], 
                              nrow(yields) > 1 ~ "review")
    output$yield2 <- case_when(nrow(yields) == 1 ~ yields[1, 4], 
                               nrow(yields) > 1 ~ "review")
    output$unit <- yields[1, 5]
    return(output)
  }
}

# A function that applies regular expression matching to return a tibble of fracture values extracted from a given comment
getFracVals <- function(comment){
  # Looking for fracture values using the fracture-only regex. This is the first step of a multi part process
  pattern <- "([fg]rac\\w*|moisture|(?:pump(?:\\w|\\s)+)?water)(?:[\\w\\s])*(:|at|@|-|\\s|from)*(((\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)+(\\s|&|,|-|and|to)?(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)*)*(ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?)+"
  fracs <- str_extract_all(comment, pattern)[[1]]
  
  # If the expression above captures anything, then proceeding with the rest of the steps. Otherwise ignoring
  # and proceeding to the return statement
  if ( (sum(!is.na(fracs)) > 0) & (length(fracs) > 0) ){
    
    # Removing any captured expressions that contain the word "pump" or related, as this refers to pumping
    # depth not fracture depth and we don't want that. Creating a temp var to assist with this process
    temp <- c()
    for(i in seq(fracs)){
      # As long as the word pump is not detected, the string is added to the temp table.
      if(!str_detect(fracs[i], "pu?mp")){
        temp <- append(temp, fracs[i])
      }
    }
    # Passing the now filtered expressions back into the fracs variable
    fracs <- temp
    # Collapsing them into one string separated by spaces
    fracs <- paste(fracs, collapse = ' ')
    
    # Now using second regex to extract the actual numeric values from the character strings extracted. Note
    # that it ignores any values followed by yield units
    fractures <- str_match_all(fracs, "((?:\\.*(?:\\d(?:\\d|\\/)*)*\\.{0,1}\\d+)+)\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?(\\s*(?:-|to)\\s*)*((?:\\.*(?:\\d(?:\\d|\\/)*)*\\.{0,1}\\d+)*)\\s*(?:ft|feet|'|gpm|gph|gpd|usgpm|ukgpm|gal)?")[[1]]
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
  return(fractures)
}

# ------------------------------------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------------------------------------

# Function to help extract and iterate through the rows of a dataframe
rows <-  function(df) {
  lapply(seq_len(nrow(df)), function(i) unclass(df[i,,drop=F]))
}

# A helper function to select the right unit when reassigning a yield unit. It checks for whether units have otherwise been reported for other yield values and if so, uses the most common reported unit. Otherwise, it just uses the default unit
select_unit <- function(unit_vector){
  # If there are no units reported, returning gpm. Otherwise returning the unit most frequently reported
  output <- ifelse(is.na(max(unit_vector, na.rm = T)), "gpm", max(unit_vector, na.rm = T))
  return(output)
}
