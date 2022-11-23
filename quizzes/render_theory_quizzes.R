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

### Quiz 00 ----

quiz_path <- "quizzes/ls00_title/ls00_title_theory_quiz/quiz.Rmd"

process_theory_quiz(quiz_path)

### Quiz 99 ----

quiz_path <- "quizzes/ls99_title/ls99_title_theory_quiz/quiz.Rmd"

process_theory_quiz(quiz_path)