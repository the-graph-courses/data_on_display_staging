# Render TGC theory quiz: template
## Joy Vaz, Kene David Nwosu
## 2022-11-23

#' Function to generate xlsx for LearnDash multiple choice quiz import.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set up ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Source gist ----

# From github
pacman::p_load(devtools)
devtools::source_gist("https://gist.github.com/kendavidn/05c7055e487ef22e5a336a4cb489a937")

#source(here("quizzes/process_mcq_function.R"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Enter quiz information ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

quiz_path <- "quizzes/ls06_boxplots/ls06_boxplots_theory_quiz/quiz_fr.Rmd"

process_theory_quiz(quiz_path)

