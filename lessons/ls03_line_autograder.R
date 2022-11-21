# Ch03 Ls07 5NG Line graphs
## Joy Vaz
## 2022-07-11

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               gapminder,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.gapminder <- gapminder

data(.gapminder, package="gapminder")
.gap_US <- dplyr::filter(.gapminder,
                              country == "United States")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a time series plot of the GPD per capita (`gdpPercap`) recorded in the `gap_US` data frame by using `geom_line()` to create a linegraph.

# [backend]
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(.gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line() 
    
    .autograder <<-
      function(){
        if (!is.ggplot(q1)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q1_test1 <- all_equal(
          target = as_tibble(q1$data), 
          current = as_tibble(.q1_correct$data))
        
        # test 2
        # that learner used geom_line()
        .q1_test2 <- any(stringr::str_detect(capture.output(q1$layers), 
                                             "geom_line"))
        # test 3
        # check the x mapping
        .q1_test3 <- "* `x` -> `year`" %in% capture.output(q1$mapping)
        
        # test 4
        # check the y mapping
        .q1_test4 <- "* `y` -> `gdpPercap`" %in% capture.output(q1$mapping)
        
        if (isTRUE(!(.q1_test1))) return(c(value = 0, message = "Wrong! Check which dataset you are plotting."))
        if (isTRUE(!(.q1_test2))) return(c(value = 0, message = "Wrong! Do not forget to use ggplot2 geometry function geom_line."))
        if (isTRUE(!(.q1_test3 && .q1_test4))) return(c(value = 0, message = "Wrong! Check your mapping arguments for x and y: are you putting the right variables?"))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3 && .q1_test4)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q1 <- function(){
  'First, supply the correct data frame to the data layer of `ggplot()`.
  Then put the variables you want to map on your x and your y axis inside `mapping = aes()`. 
  Then, add the correct geometry function a line graph.
  If all these are correct, look for typos, missing commas, or unclosed brackets.' -> out
  cat(out)
}
# solution of question
.solution_q1 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                        y = gdpPercap)) +
      geom_line()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Building on the code above, visualize the relationship between time the GPD per capita `gap_US` data frame with both points and lines. Change the color of the points as desired.

# [backend]
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(.gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(size = 1.5, 
                color = "grey") +
      geom_point(size = 3, 
                 color = "forestgreen")
    
    # to access color, size of SOLUTION
    .q2_correct_build <- ggplot_build(.q2_correct)
    
    .autograder <<-
      function(){
        if (!is.ggplot(q2)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # to access color, size of ANSWER
        .q2_build <- ggplot_build(q2)
        
        # test 1
        # that learner used geom_line()
        .q2_test1 <- any(stringr::str_detect(capture.output(q2$layers), 
                                             "geom_line"))
        
        # test 2
        # that learner used geom_point()
        .q2_test2 <- any(stringr::str_detect(capture.output(q2$layers), 
                                             "geom_point"))
        
        # test 3
        # check the color argument of geom_line
        .q2_test3 <- .q2_build$data[[1]]["colour"][1,1] == .q2_correct_build$data[[1]]["colour"][1,1]
        
        # test 4
        # check the size argument of geom_line
        .q2_test4 <- .q2_build$data[[1]]["size"][1,1] == .q2_correct_build$data[[1]]["size"][1,1]
        
        # test 5
        # check the color argument of geom_point
        .q2_test5 <- .q2_build$data[[2]]["colour"][1,1] == .q2_correct_build$data[[2]]["colour"][1,1]
        
        # test 6
        # check the size argument of geom_point
        .q2_test6 <- .q2_build$data[[2]]["size"][1,1] == .q2_correct_build$data[[2]]["size"][1,1]
        
        if (isTRUE(!(.q2_test1))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function geom_line"))
        if (isTRUE(!(.q2_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function geom_point"))
        if (isTRUE(!(.q2_test3))) return(c(value = 0, message = "! Check your color argument for geom_line"))
        if (isTRUE(!(.q2_test4))) return(c(value = 0, message = "! Check your size argument for geom_line"))
        if (isTRUE(!(.q2_test5))) return(c(value = 0, message = "! Check your color argument for geom_point"))
        if (isTRUE(!(.q2_test6))) return(c(value = 0, message = "! Check your size argument for geom_point"))
        
        if (isTRUE(.q2_test1 && .q2_test2 && .q2_test3 && .q2_test4 && .q2_test5 && .q2_test6)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.hint_q2 <- function(){
  'We want to plot a line and points: you need to use two different geometry functions.
  Then we want to customize both geometries with their own color and size.' -> out
  cat(out)
}
# solution of question
.solution_q2 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(size = 1.5, 
                color = "grey") +
      geom_point(size = 3, 
                 color = "forestgreen")' -> out
  cat(out)
}

