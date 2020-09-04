# Name: Saeesh Mangwani
# Date: 2020-05-19
# Description: A set of helper functions that assist with table cleaning

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(stringi)

# Function to help extract and iterate through the rows of a dataframe
rows <-  function(df) {
  lapply(seq_len(nrow(df)), function(i) unclass(df[i,,drop=F]))
}

# Function to check whether yield values returned from a dataframe of yield-fracture pair are cumulative or not
check_cumulative <- function(pairs){
  # A variable to store the output of this function. Defaults to true
  output <- TRUE
  # Getting the yield values ordered in ascending order
  temp <- pairs %>% 
    arrange(depth) %>% 
    pull(yield)
  # If there is only 1 yield or no yields reported, just returning false (By definition cannot be cumulative)
  if (length(temp) <= 1){
    return(FALSE)
  }else{
    # If the present value is greater than the next value, breaking the looping and setting the output var to false (if cumulative, the successive value
    # cannot be smaller than the predecessor)
    for(i in 1:(length(temp) - 1)){
      if(temp[i] > temp[i+1]){
        output <- FALSE
      }
    }
  }
  # Returning the output
  return(output)
}

# A function that applies regular expression matching to return a dataframe of yield-fracture pairs extracted from a given comment
getYieldFracPairs <- function(comment){
  # Looking for yield-value pairs using the paired regex
  pattern <- "(((?:frac\\D*(ft|feet)?)|(?:p\\.l\\.?\\s*)|(?:pumping\\D*))?(?:(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)(\\s*(?:to|-|, |\\s)+\\s*)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)?(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm|\\s*ft|\\s*feet)*)|(?:water|trickle|trace|moisture))(\\s*(?:@|-|at|in|, )\\s*)+(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)(\\s*(?:to|-|, |\\s)+\\s*)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/)*)*\\.{0,1}\\d+)*(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm|\\s*ft|\\s*feet)*"
  pairs <- str_match_all(comment, pattern)[[1]]
  
  # If pairs are detected and are not all missing, checking to see the order in which yield depth values are presented, and organizing a resulting dataframe such that this
  # process is preserved
  if( (sum(!is.na(pairs)) > 0) & (nrow(pairs) > 0) ){
    
    # An empty matrix to store only those rows that have been cleaned for the words fracture and pumping level
    temp <- pairs[0,]
    
    # Iterating through all the rows of matches to remove inconsistencies 
    for (i in seq(nrow(pairs))){
      # If the words p.l or pumping level are found, skipping this match
      if(str_detect(pairs[i, 1], "(p\\.l\\.?)|(pumping\\D*)")){
        next
      # Wherever the phrase "frac" or related phrases are found in a match, or there is only 1 pair that's been found
      }else if( str_detect(pairs[i, 1], "frac\\D*") | nrow(pairs) == 1){
        # Checking whether the units associated with the values are not the same, and are both present. That is either there must be a depth-yield or
        # a yield-depth unit pair. In the absence of these, we don't have enough confidence to know whether this is a pair or just a range, so we must
        # skip it
        first_test <- (stri_detect(pairs[i, 8], regex="\\s*ft|\\s*feet") & stri_detect(pairs[i, 13], regex="\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm"))
        sec_test <- (stri_detect(pairs[i, 8], regex="\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm") & stri_detect(pairs[i, 13], regex="\\s*ft|\\s*feet"))
        
        # Breaking it into 2 steps to correct for the behaviour where TRUE and NA equates to NA, while FALSE and NA equates to FALSE. Adding only
        # valid pairs to the temp variable
        if( if_else(is.na(first_test), FALSE, first_test) | if_else(is.na(sec_test), FALSE, sec_test) ){
          temp <- rbind(temp, pairs[i, ])
        }
      # If none of these questionable phrases are found, adding the match result directly to the temp table  
      }else{
        temp <- rbind(temp, pairs[i,])
      }
    }
    # Resetting the pairs variable with the now filtered pairs
    pairs <- temp
    
    #If either the latter column contains depth units or the former
    # contains yield units, assigning the columns as such and creating a vector of names to work with this structure
    if( (sum(str_detect(pairs[,8], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0) & 
        (sum(str_detect(pairs[,13], "ft|feet"), na.rm = T) > 0) ){
      yield <- list("num" = 5, "unit" = 8)
      depth <- list("num" = 10, "unit" = 14)
      names <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
      # If either the latter column contains yield units and the former depth units, assigning them as such
    }else if( (sum(str_detect(pairs[,13], "gpm|gph|gpd|ukgpm|usgpm"), na.rm = T) > 0) & 
              (sum(str_detect(pairs[,8], "ft|feet"), na.rm = T) > 0) ){
      yield <- list("num" = 10, "unit" = 14)
      depth <- list("num" = 5, "unit" = 8)
      names <- c("string", "x2", "x3", "x4","depth", "x6", "depth2", "depth_unit", "x9", "yield", "x11", "yield2", "yield_unit")
      # If none of the units are found assuming a yield first depth after system as per the general norm, flagging the row for for review and
      # returning it
    }else{ 
      yield <- list("num" = 2, "unit" = 5)
      depth <- list("num" = 8, "unit" = 11)
      names <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
      # Turning the output matrix into a named df to make it easier to work with and giving it the right names
      pairs <- as_tibble(pairs, .name_repair = "unique")
      names(pairs) <- names
      pairs <- pairs %>% mutate(yield = "review", depth = "0", depth_unit = "ft")
      return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
    }
    
    # Turning the output matrix into a named df to make it easier to work with and giving it the right names
    pairs <- as_tibble(pairs, .name_repair = "unique")
    names(pairs) <- names
    
    # Refactoring this dataframe to correct for certain error measures and ensure consistency
    pairs <- pairs %>% 
      # Replacing all "trace" indicators with a 0.01, which we assume to imply a trickle
      mutate(yield = if_else(str_detect(x2, "water|trickle|trace|moisture"), "0.01", yield)) %>% 
      mutate(yield_unit = if_else(str_detect(x2, "water|trickle|trace|moisture"), "gpm", yield_unit))
    
    # For each row in the dataframe, checking for consistency in the storage of yield depth pairs and filling in empty units
    for (row in rows(pairs)){
      
      # If any units are missing, replacing them with the appropriate defaults ones
      row$yield_unit <- if_else(is.na(row$yield_unit), "gpm", row$yield_unit)
      row$depth_unit <- if_else(is.na(row$depth_unit), "ft", row$depth_unit)
      
      # If any units are found in the wrong column, inverting the values and units in that column
      if( str_detect(row$yield_unit, "ft|feet") | str_detect(row$depth_unit, "gpm|gph|gpd|ukgpm|usgpm")){
        # Inverting values
        temp <- row$yield
        row$yield <- row$depth
        row$depth <- temp
        # Inverting units
        temp <- row$yield_unit
        row$yield_unit <- row$depth_unit
        row$depth_unit <- temp
      }
      
      # Replacing the row in the dataframe with this cleaned row
      pairs <- pairs %>% 
        filter(string != row$string) %>% 
        bind_rows(row)
    }
    
    # If either the primary yield or the depth unit is missing, altering the row and flagging it for review
    pairs <- pairs %>% 
      mutate(yield = if_else(is.na(yield), "review", yield)) %>% 
      mutate(yield = if_else(is.na(depth), "review", yield)) %>% 
      mutate(depth = if_else(is.na(depth), "0", depth))
    
    # Returning the dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
  }else{
    # If no matches are found, returning an emptydataframe (it is still named with the default naming pattern for consistency)
    pairs <- as_tibble(pairs, .name_repair = "unique")
    names(pairs) <- c("string", "x2", "x3", "x4","yield", "x6", "yield2", "yield_unit", "x9", "depth", "x11", "depth2", "depth_unit")
    # Returning the empty dataframe selecting only the relevant rows
    return(pairs %>% select(string, yield, yield2, yield_unit, depth, depth2, depth_unit))
  }
}

# A function that applies regular expression matching to return a vector of yield values extracted from a given comment
getYieldVals <- function(comment){
  # Looking for yield values using the yield-only regex. First looking for the world "yield" and if found splitting the string on that word to only
  # take the latter half of the comment
  pattern <- "(yield:?\\s*)?(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)+(?:\\s|&|,|-|and|to)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)*(\\s*gpm|\\s*gph|\\s*gpd|\\s*usgpm|\\s*ukgpm)"
  
  # An empty dataframe to store the output of this function
  output <- tibble(yield = NA_character_, yield2 = NA_character_, unit = NA_character_, .rows = 1)
  
  # Using the pattern to extract matches
  yields <- str_match_all(comment, pattern)[[1]]
  
  # If no values are found or the values are all missing, returning an empty dataframe
  if( (sum(!is.na(yields)) == 0) | (nrow(yields) == 0) ){
    return(output)
  # If the second column is filled for any row, it means the word "yield" was detected, and we assume the value associated with it as the cumulative
  # yield of the well.   
  }else if( sum(!is.na(yields[,2])) > 0 ){
    #We then loop through to extract this value and return it
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
  # If the word "yield" isn't found, then looking at how many measures were actually found. If it is exactly one, returning it. If it is more than
  # one, marking it for review. 
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
  # Looking for fracture values using the fracture-only regex. This is a multi part process
  pattern <- "(frac\\w*|moisture|(?:pump(?:\\w|\\s)+)?water)(?:[a-z\\s])+(:|at|@|-|\\s)*(((\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)*(\\s|&|,|-|and|to)*(\\.*(?:\\d\\s{0,1}(?:\\d|\\/|\\-)*)*\\.{0,1}\\d+)*)*(ft|feet|gpm|gph|gpd|usgpm|ukgpm)?)+"
  fracs <- str_extract_all(comment, pattern)[[1]]
    
  # Creating an empty fractures variable to store the results from this expression matching. For now it is set to what was outputted before.
  fractures <- fracs
  
  
  # If the expression above captures anything, then proceeding with the rest of the steps. Otherwise ignoring and proceeding to the return statement
  if ( (sum(!is.na(fracs)) > 0) & (length(fracs) > 0) ){
    
    # Removing any captured expressions that contain the word "pump" or related, as this refers to pumping depth not fracture depth and we don't want
    # that
    for(i in seq(fracs)){
      if(str_detect(fracs[i], "pump")){
        fracs <- fracs[-i]
      }
    }
    
    fracs <- paste(fracs, collapse = ' ')
    # Now using second regex to extract the actual numeric values from the character strings extracted. Note that it ignores any values followed by
    # yield units
    fractures <- str_match_all(fracs, "(\\.*(?:\\d(?:\\d|\\/)*)*\\.{0,1}\\d+)+(?:ft|feet|gpm|gph|gpd|usgpm|ukgpm)?(\\s*(?:-|to)\\s*)*(\\.*(?:\\d(?:\\d|\\/)*)*\\.{0,1}\\d+)*(?:ft|feet|gpm|gph|gpd|usgpm|ukgpm)?")[[1]]
    # Turning this into a named dataframe
    fractures <- as_tibble(fractures,.name_repair = "unique")
    names(fractures) <- c("string", "depth", "to", "depth2")
    # Removing any strings that capture yield values, like gpm, instead of depth values
    fractures <- fractures %>% filter(!str_detect(string, "gpm|gph|gpd|usgpm|ukgpm"))
  }else{
    # If there are no fractures detected, returning an empty named dataframe using the same structure as the one that would be created above
    fractures <- tibble(string = character(), depth = character(), to = character(), depth2 = character(), .rows = 0)
  }
  return(fractures)
}

# # helper function that takes a dataframe and checks whether every successive cumulative fracture yield is in fact greater than the previous one. If
# it is not, it rewrites the cumulative fracture yields as single fracture yields
transform_cumulative <- function(df){
  print(df$borehole[1])
  # Getting the yield values ordered in ascending order
  temp <- df %>% 
    arrange(as.numeric(record_index), as.numeric(depth_from_ft), as.numeric(depth_to_ft)) %>% 
    filter(!is.na(cum_frac_yield)) %>% 
    pull(cum_frac_yield)
  
  # If there exist any cumulative values, checking to see whether or no they are actually increasing 
  if(length(temp) > 1){
    # If the present value is greater than the next value, breaking the looping and editing the dataframe to single values instead of cumulative
    for(i in 1:(length(temp) - 1)){
      if(temp[i] > temp[i+1]){
        df$single_frac_yield <- df$cum_frac_yield
        df$cum_frac_yield <- NA_real_
        break
      }
    }
  }
  # Returning the df 
  return(df)
}

# Function that takes a dataframe and completes its cumulative and single yield columns
fill_yield_vals <- function(df){
  # Arranging the dataframe in the right order and turning the yield columns to numeric
  df <- df %>% arrange(as.numeric(record_index), as.numeric(depth_from_ft), as.numeric(depth_to_ft)) %>% 
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

# A function that extracts yield-fracture pairs using the symmetrical matching of counted yield and pair values. This is a slighly less robust method
# that the pair extractor function above, so it is only used in the later stages of the stepwise tidying process
