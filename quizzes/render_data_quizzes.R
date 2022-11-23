# Quiz knitter 
## GRAPH Courses team
## 2022-03-10

#' Create unique quiz copies and export them into  an Excel file for upload to Learndash
#' This script may take a while to run. So I recommend using RStudio's "Source as Local Job" functionality to run it.
#' This way you can continue to work while it runs in the background.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(parsermd, rmarkdown, here, fs, writexl, tidyverse)
source(here("global/functions/misc_functions.R"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Enter quiz information ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
course_abbrev <- "idar" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
quiz_folder <- here::here("quizzes/ls01_gg_intro/ls01_gg_intro_data_quiz")
quiz_title <- "Data Quiz | Building plots with {ggplot2}" # User-facing title. 
quiz_type <- "data" # "data" "stratified_theory" or "simple_theory"
n_quizzes <- 2 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.


# course_abbrev <- "idar" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
# quiz_folder <- here::here("quizzes/ch04_ls02_filter_data_quiz")
# quiz_title <- "Data quiz | Filter" # User-facing title.
# quiz_type <- "data" # "data" "stratified_theory" or "simple_theory"
# n_quizzes <- 30 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.
# 
# 
# 
# course_abbrev <- "idar" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
# quiz_folder <- here::here("quizzes/ch04_ls01_select_rename_data_quiz/")
# quiz_title <- "Data quiz | Filter" # User-facing title. 
# quiz_type <- "data" # "data" "stratified_theory" or "simple_theory"
# n_quizzes <- 30 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.
# 
# 
# course_abbrev <- "idar" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
# quiz_folder <- here::here("quizzes/conditional_mutate_data_quiz_2")
# quiz_title <- "Data quiz | Conditional mutate" # User-facing title. 
# quiz_type <- "data" # "data" "stratified_theory" or "simple_theory"
# n_quizzes <- 25 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.
# 
# 
# course_abbrev <- "idar" # abbrevs for our four courses are "idar", "fdar", "err" and "egh"
# quiz_folder <- here::here("quizzes/ch04_ls05_groupby_summarize_data_quiz_2/")
# quiz_title <- "Data quiz | Group by and summarize" # User-facing title. 
# quiz_type <- "data" # "data" "stratified_theory" or "simple_theory"
# n_quizzes <- 30 # Should default to 30. (We want at least 30 unique quiz samples for each quiz.) Using a lower number here for expedience.




# course_abbrev <- "temp"
# quiz_folder <- here("ch99_template/quizzes/ch99_ls01_qz02_data_ebola")
# quiz_title <- "99.1.2 Data quiz: Ebola in Sierra Leone"
# quiz_type <- "data"
# n_quizzes <- 1

# course_abbrev <- "temp"
# quiz_folder <- here("ch99_template/quizzes/ch99_ls01_qz03_simple_theory_filter_select")  
# quiz_title <- "99.1.3 Theory quiz: selecting and filtering" 
# quiz_type <- "simple_theory" 
# n_quizzes <- 3
# questions_per_quiz <- 3


process_quiz(
  course_abbrev = course_abbrev,
  quiz_type = quiz_type,
  quiz_folder = quiz_folder,
  quiz_title = quiz_title,
  n_quizzes = n_quizzes
) 
