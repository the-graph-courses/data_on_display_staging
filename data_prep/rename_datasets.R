## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Translate datasets to FR
## Author: Sabina Rodriguez
## Date Created: 2024-28-02
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
# Niger Dataset ----

## ~ Load Packages ~
if(!require(pacman)) install.packages("pacman")
pacman :: p_load(tidyverse, here, openxlsx, readxl)

## ~ Load Data ~
df <- read_csv(here("data/clean/nigerm_cases_rgn.csv"))

# Write new column names and get old column names
new_names <- c("annee", "semaine", "region", "cas")
# Make sure both vectors are the same length
old_names <- colnames(df)

# Function to rename and save new dataset
rename_columns_save <- function(df, old_names, new_names, file_name, save_path) {
  # Check if the length of new_names matches the length of old_names
  if(length(new_names) != length(old_names)) {
    stop("The number of new names must match the number of old names provided.")
  }
  
  # Rename columns
  for(i in seq_along(old_names)) {
    colnames(df)[colnames(df) == old_names[i]] <- new_names[i]
  }
  
  # Construct the full file path using here() and the save_path
  full_path <- here(save_path, paste0(file_name, ".csv"))
  
  # Save the dataframe to a CSV file at the specified path
  write_csv(df, full_path)
  
  return(df)
}

# Use function te rename and to save new file
df_renamed <- rename_columns_save(df, old_names, new_names, 
                                  file_name = "niger_cas_rgn", # Write name to save
                                  save_path = "data/clean") # Select folder location

# Double check dataset
View(df_renamed)
