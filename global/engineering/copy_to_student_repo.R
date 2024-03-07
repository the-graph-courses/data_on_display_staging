# Copy staging repo to student repo ----
## GRAPH Courses Team

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
               archive,
               fs)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Establish paths  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here()

to   <- gsub("_staging", "", here())

# should be located in the same parent directory as the staging repo, from which this script is run

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Remove all folders in target repo ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fs::dir_ls(path = to, type = "directory") %>% 
  fs::dir_delete()

fs::dir_ls(path = to,  regexp =  'zip') %>% 
  fs::file_delete()


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

# there are a bunch files in the copied folders that we do not want students to have, delete these folders
# dirs to delete
all_copied_dirs <- fs::dir_ls(to, type = "directory", recurse = T)
dirs_to_delete_search_string <- paste(c("data_prep", "/quizzes", "/recordings", 
                                        "/global/engineering", "global/trash", 
                                        "global/templates", "/old"),
                                      collapse = "|")
dirs_to_delete <- all_copied_dirs[str_ends(all_copied_dirs, dirs_to_delete_search_string)]
unlink(dirs_to_delete, recursive = T)


# now delete files (not entire folders)
all_copied_files <- fs::dir_ls(to, type = "file", recurse = T)
files_to_delete_search_string <- paste(c(".docx", "-TEACHER", 
                                         "_TEACHER",
                                         "-GITIGNORE", 
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

selected_lessons <- 
  c(
    "/fr_ls01_gg_intro_VIDEO_CODE_ALONG.Rmd",
    "/fr_ls02_scatter_VIDEO_CODE_ALONG.Rmd",
    "/fr_ls03_line_graphs_VIDEO_CODE_ALONG.Rmd",
    "/fr_ls04_histograms_VIDEO_CODE_ALONG.Rmd",
    "/fr_ls05_boxplots_VIDEO_CODE_ALONG.Rmd",
    "/ls01_gg_intro_VIDEO_CODE_ALONG.Rmd",
    "/ls02_scatter_VIDEO_CODE_ALONG.Rmd",
    "/ls03_line_graphs_VIDEO_CODE_ALONG.Rmd",
    "/ls04_histograms_VIDEO_CODE_ALONG.Rmd",
    "/ls05_boxplots_VIDEO_CODE_ALONG.Rmd"
  ) %>%
  paste0(collapse = "|")




rmds_to_sanitize <- fs::dir_ls(to, 
                               regexp = "*.Rmd$",
                               recurse = T) %>%
  as_tibble() %>% 
  filter(!str_detect(value, "/snippets")) %>% 
  filter(str_detect(value, selected_lessons)) %>% 
  pull(1)




name <- basename(rmds_to_sanitize) %>% tools::file_path_sans_ext() %>% str_remove("_VIDEO_CODE_ALONG")

# create a folder in the target for each repo
fs::dir_create(path = paste0(to, "/", "data_on_display_", name))

# create a Rproj file in each folder 
fs::file_copy(path = rep(paste0(from, "/data_on_display_staging.Rproj"), length(name)) , 
              new_path = paste0(to, "/data_on_display_", name, "/data_on_display_",   name, ".Rproj"), overwrite = T)

# create a folder for images, functions and data in each target repo
fs::dir_create(path = paste0(to, "/", "data_on_display_", name, "/data"))
fs::dir_create(path = paste0(to, "/", "data_on_display_", name, "/data/clean"))
fs::dir_create(path = paste0(to, "/", "data_on_display_", name, "/autograder"))
fs::dir_create(path = paste0(to, "/", "data_on_display_", name, "/images"))
# copy global folder into each repo 
fs::dir_copy(rep(paste0(to, "/global"), length(name)) , 
             paste0(to, "/", "data_on_display_", name, "/global"), overwrite = T)


# rename paths as needed in rmds
for (rmd in rmds_to_sanitize){
  
  ith_rmd_name <- basename(rmd) %>% tools::file_path_sans_ext()
  ith_rmd_name_not_code_along <- ith_rmd_name  %>% str_remove("_VIDEO_CODE_ALONG")
  
  xfun::gsub_dir(dir = to, 
                 pattern = paste0("lessons/", ith_rmd_name_not_code_along, "_autograder.R"), 
                 replacement = paste0("autograder/", ith_rmd_name_not_code_along, "_autograder.R"), 
                 ext = c("Rmd", "R"))
  
}


# copy autograders into target folders
file.copy(from = paste0(to, "/lessons/", name, "_autograder.R"),
          to = paste0(to,  "/data_on_display_", name, "/autograder/", name, "_autograder.R"), 
          overwrite = T)

# copy Rmds into target folders
file.copy(from = paste0(to, 
                        "/lessons/", 
                        name,
                        "_VIDEO_CODE_ALONG",
                        ".Rmd"),
          to = paste0(to,  "/data_on_display_", 
                      name,
                      "/", 
                      name,
                      "_VIDEO_CODE_ALONG",
                      ".Rmd"), 
          overwrite = T)


# copy images used in each Rmd into target folders
for (rmd in rmds_to_sanitize) {
  #rmd = rmds_to_sanitize[1]
  rmd_base_name <- basename(rmd) %>% tools::file_path_sans_ext()
  rmd_base_name_not_code_along <- rmd_base_name  %>% str_remove("_VIDEO_CODE_ALONG")
  
  all_lines <- read_lines(rmd)
  
  images_used <-
    str_extract(all_lines, "(images/[^)]+)") %>%
    tibble() %>%
    drop_na()
  
  source <- paste0(to, "/lessons/", images_used$.)
  
  target <- paste0(to, "/data_on_display_", rmd_base_name_not_code_along, "/", images_used$.)
  
  file.copy(source, target, overwrite = T)
  
}

# copy datasets used in each Rmd into target folders
for (rmd in rmds_to_sanitize) {
  
  rmd_base_name <- basename(rmd) %>% tools::file_path_sans_ext()
  rmd_base_name_not_code_along <- rmd_base_name  %>% str_remove("_VIDEO_CODE_ALONG")
  
  
  all_lines <- read_lines(rmd)
  
  datasets_used <-
    str_extract(all_lines, "(data/[^)]+)") %>%
    str_remove_all("'") %>% 
    str_remove_all('"') %>% 
    tibble(value = .) %>%
    drop_na()
  
  source <- paste0(to, "/", datasets_used$value)
  
  target <- paste0(to, "/data_on_display_", rmd_base_name_not_code_along, "/", datasets_used$value)
  
  file.copy(source, target, overwrite = T)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Delete global, data, lessons original files  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fs::dir_delete(paste0(to, "/", c("global", "lessons", "data")))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Create zip folder ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get the list of directories in "data_on_display"
directories <- list.dirs(path = to, recursive = FALSE)

# Filter out directories that don't start with "data_on_display_ls"
directories <- directories[startsWith(basename(directories), "data_on_display")]


# For each directory
for (dir in directories) {
  
  
  # Define the name of the zip file 
  zip_file_name <- file.path(to, paste0(basename(dir), ".zip"))
  
  # Define the directories to include in the zip file
  dirs <- c(
    "autograder",
    "data",
    "global",
    "images"
  )
  
  dirs <- file.path(dir, dirs)
  
  # Check for existence of each directory and remove those that do not exist
  dirs <- dirs[dir.exists(dirs)]
  
  # Add the Rmd file
  rmd_file <- paste0(to, "/",
                     basename(dir), "/",
                     str_remove(basename(dir), "data_on_display_"),
                     "_VIDEO_CODE_ALONG.Rmd")
  
  files <- c(dirs, rmd_file)
  
  ## Add the Rproj file 
  
  rproj_file <- paste0(to, "/",
                       basename(dir), "/",
                       "data_on_display_",
                       str_remove(basename(dir), "data_on_display_"),
                       ".Rproj")
  
  files <- c(files, rproj_file)
  
  files_cut = basename(files)
  
  # Change the working directory to the current "data_on_display_lsXX" directory
  setwd(dir)
  
  # Create the zip file
  zip::zip(zip_file_name, files = files_cut)
  
  # Change the working directory back to the parent directory
  setwd(here())
  
}


setwd(here())

# --------------------

# Get the list of directories in "data_on_display"
directories <- list.dirs(path = to, recursive = FALSE)

# Filter out directories that don't start with "data_on_display_ls"
directories <- directories[startsWith(basename(directories), "data_on_display")]

# For each directory
for (dir in directories) {
  
  cat("Processing directory:", dir, "\n")
  
  # Define the name of the zip file 
  zip_file_name <- file.path(to, paste0(basename(dir), ".zip"))
  
  cat("Creating zip file:", zip_file_name, "\n")
  
  # Define the directories to include in the zip file
  dirs <- c(
    "autograder",
    "data",
    "global",
    "images"
  )
  
  dirs <- file.path(dir, dirs)
  
  # Check for existence of each directory and remove those that do not exist
  dirs <- dirs[dir.exists(dirs)]
  
  cat("Directories to include in zip:", dirs, "\n")
  
  # Add the Rmd file
  rmd_file <- paste0(dir, "/",
                     str_remove(basename(dir), "data_on_display_"),
                     "_VIDEO_CODE_ALONG.Rmd")
  
  cat("Rmd file:", rmd_file, "\n")
  
  files <- c(dirs, rmd_file)
  
  ## Add the Rproj file 
  
  rproj_file <- paste0(dir, "/",
                       "data_on_display_",
                       str_remove(basename(dir), "data_on_display_"),
                       ".Rproj")
  
  cat("Rproj file:", rproj_file, "\n")
  
  files <- c(files, rproj_file)
  
  cat("Files to include in zip:", files, "\n")
  
  # Change the working directory to the current "data_on_display_lsXX" directory
  setwd(dir)
  
  cat("Current working directory:", getwd(), "\n")
  
  # Create the zip file
  utils::zip(zip_file_name, files = files)
  
  cat("Zip file created.\n")
  
  # Change the working directory back to the parent directory
  setwd(here())
  
}

# Reset working directory
setwd(here())


