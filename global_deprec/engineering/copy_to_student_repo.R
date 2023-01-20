# Copy staging repo to student repo ----
## GRAPH Courses team
## 2021-03-27

#' Copies internal staging repo to the outward-facing student repo. 
#' The outward-facing repo will be made available to students for download.
#' The copy procedure below is aimed at only copying over the files that the students actually need.
#' If you have ideas for simplifying the script, please suggest them! Start a merge request.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, 
               here, 
               fs)

my.file.copy_file <- function(from, to, ...) {
  
  for (i in 1:length(from)){
  
  if (!dir.exists(dirname(to[[i]])))  dir.create(dirname(to[[i]]), recursive = TRUE) 
  file.copy(from = from[[i]],  to = to[[i]], ...)
  
  }
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Establish paths  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here()

to   <- gsub("_staging", "", here()) # the repo you are copying to, "intro-to-data-analysis-with-r" 

# should be located in the same parent directory as the staging repo, from which this script is run

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Remove all folders in target repo ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fs::dir_ls(path = to, type = "directory") %>% 
  fs::dir_delete()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  List top level files and directories  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# list files to be copied
all_files <- list.files(from, full.names = TRUE)

# files to copy
files_to_copy_search_string <- paste(c("global", "lessons", "data", "data_prep", "slides"),
                                     collapse = "|")

# note that this list contains only top level files
files_to_copy <- all_files[str_detect(all_files, files_to_copy_search_string)]


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Copy, then delete extraneous things  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

file.copy(from = files_to_copy, 
          to = to, 
          recursive = TRUE) # `recursive = TRUE` makes sure you catch nested files

# there are a bunch files in the copied folders that we do not want students to have, delete these


# dirs to delete
all_copied_dirs <- fs::dir_ls(to, type = "directory", recurse = T)
dirs_to_delete_search_string <- paste(c("data_prep", "/quizzes", "/recordings", 
                                        "/global/engineering", "global/trash", 
                                        "global/templates", "/old"),
                                     collapse = "|")
dirs_to_delete <- all_copied_dirs[str_ends(all_copied_dirs, dirs_to_delete_search_string)]
unlink(dirs_to_delete, recursive = T)


# files to delete
all_copied_files <- fs::dir_ls(to, type = "file", recurse = T)
files_to_delete_search_string <- paste(c(".docx", "-TEACHER", 
                                         "_TEACHER",
                                         "-GITIGNORE", 
                                         "_blank",
                                        ".html", 
                                        "chXX",
                                        "data_on_display_staging.Rproj",
                                        ".pptx"
                                        ),
                                      collapse = "|")
files_to_delete <- all_copied_files[str_ends(all_copied_files, files_to_delete_search_string)]
unlink(files_to_delete, recursive = T)


# finally delete empty folders with terminal command (as at 2022-03-17, this doesn't work perfectly)
system2(command = "find",
        args    = c(to, "-empty", "-type d", "-delete"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Rearrange and sanitize student repo  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rmds_to_sanitize <- fs::dir_ls(to, 
             regexp = "*.Rmd$",
             recurse = T) %>%
  as_tibble() %>% 
  pull(1)


primary_rmds <-
  rmds_to_sanitize %>% 
  as_tibble() %>% filter(!str_detect(value, "code_along")) %>% pull(1)

#rmds_to_sanitize <- rmds_to_sanitize[1:2]  ## TEMPORARY! REMOVE WHEN LESSON 3 is ready


full_name <- basename(rmds_to_sanitize) %>% tools::file_path_sans_ext()
short_name <- basename(primary_rmds) %>% tools::file_path_sans_ext() %>% str_remove_all("_filled")
full_name_primary_rmds <- basename(primary_rmds) %>% tools::file_path_sans_ext()

# create a folder in the target for each repo
fs::dir_create(path = paste0(to, "/", "data_on_display_", short_name))

# create a Rproj file in each for each repo
fs::file_copy(path = rep(paste0(from, "/data_on_display_staging.Rproj"), length(short_name)) , 
              new_path = paste0(to, "/data_on_display_", short_name, "/data_on_display_",   short_name, ".Rproj"), overwrite = T)

# create a folder for images, functions and data in each target repo
fs::dir_create(path = paste0(to, "/", "data_on_display_", short_name, "/data"))
fs::dir_create(path = paste0(to, "/", "data_on_display_", short_name, "/autograder"))
fs::dir_create(path = paste0(to, "/", "data_on_display_", short_name, "/images"))
# copy global folder into each repo 
fs::dir_copy(rep(paste0(to, "/global"), length(short_name)) , 
             paste0(to, "/", "data_on_display_", short_name, "/global"), overwrite = T)


# rename paths as needed in rmds
for (rmd in rmds_to_sanitize){

  ith_rmd_name <- basename(rmd) %>% tools::file_path_sans_ext() %>% str_remove_all("_video_code_along|_written_code_along|_filled")
  
  xfun::gsub_dir(dir = to, 
               pattern = paste0("lessons/", ith_rmd_name, "_autograder.R"), 
               replacement = paste0("autograder/", ith_rmd_name, "_autograder.R"), 
               ext = c("Rmd", "R"))

}


# copy autograders into target folders
file.copy(from = paste0(to, "/lessons/", short_name, "_autograder.R"),
          to = paste0(to,  "/data_on_display_", short_name, "/autograder/", short_name, "_autograder.R"), 
          overwrite = T)

# copy Rmds into target folders
file.copy(from = paste0(to, "/lessons/", full_name, ".Rmd"),
          to = paste0(to,  "/data_on_display_", 
                      str_remove_all(full_name, "_written_code_along|_video_code_along|_filled"), 
                      "/", full_name, ".Rmd"), 
          overwrite = T)



# copy images used in each Rmd into target folders
for (rmd in rmds_to_sanitize) {
  
  rmd_base_name <- basename(rmd) %>% tools::file_path_sans_ext()
  rmd_repo_name <- rmd_base_name %>% str_remove_all("_video_code_along|_written_code_along|_filled")
  
  all_lines <- read_lines(rmd)
  
  images_used <-
    str_extract(all_lines, "(images/[^)]+)") %>%
    tibble() %>%
    drop_na()
  
  source <- paste0(to, "/lessons/", images_used$.)
  
  target <- paste0(to, "/data_on_display_", rmd_repo_name, "/", images_used$.)
  
  file.copy(source, target, overwrite = T)
  
}

# copy datasets used in each Rmd into target folders
for (rmd in rmds_to_sanitize) {
  
  rmd_base_name <- basename(rmd) %>% tools::file_path_sans_ext()
  short_name <- basename(rmd) %>% tools::file_path_sans_ext() %>% str_remove_all("_filled") %>% 
    str_remove_all("_video_code_along") %>% 
    str_remove_all("_written_code_along")
  
  
  all_lines <- read_lines(rmd)
  
  datasets_used <-
    str_extract(all_lines, "(data/[^)]+)") %>%
    str_remove_all("'") %>% 
    str_remove_all('"') %>% 
    tibble(value = .) %>%
    drop_na()

  source <- paste0(to, "/", datasets_used$value)
  
  target <- paste0(to, "/data_on_display_", short_name, "/", datasets_used$value)
  
  my.file.copy_file(source, target, overwrite = T)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Delete global, data, lessons original files  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fs::dir_delete(paste0(to, "/", c("global", "lessons", "data")))


