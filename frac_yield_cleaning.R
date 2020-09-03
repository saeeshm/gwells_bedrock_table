# !usr/bin/env Rscript

# Author: Saeesh Mangwani
# Date: 2020-09-03
# Description: Extracting and organizing fracture and yield information for the bedrock wells in the gwells lithology table

# ==== Loading libraries ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
