# Adapted from source: https://gist.github.com/kendavidn/05c7055e487ef22e5a336a4cb489a937
# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, xml2, here, writexl, XML, polite, qdapRegex, tidyverse, blogdown, parsermd, rmarkdown, uuid)

process_theory_quiz <-
  function(rmd_path,
           output_xlsx_path = NULL) {
    
    
    ## Parse title from Rmd----
    quiz_title <- rmarkdown::yaml_front_matter(rmd_path)$title
    if (is.null(quiz_title)) quiz_title <- paste0("learndash_quiz_", str_sub(uuid::UUIDgenerate(), 1, 8))
    
    ## If no output path, given put in same directory as source Rmd----
    
    if(is.null(output_xlsx_path)){
      output_xlsx_path <- paste0(dirname(rmd_path), "/",  tools::file_path_sans_ext(basename(rmd_path)), ".xlsx")
    }
    
    ## Render ----
    dir <- tempdir()
    
    html_path <- rmarkdown::render(input = rmd_path, output_format = "blogdown::html_page", output_dir = dir, encoding = "UTF-8")
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Import html ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    html <- read_html(html_path, encoding = "UTF-8")
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parse intro----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    intro <- 
      html %>% 
      html_elements("#intro") %>% 
      html_children() %>% 
      as.character() %>% 
      as_tibble() %>% 
      filter(row_number() != 1) %>% # drop the header text
      pull()
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Enumerate questions ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    question_ids <-
      html %>%
      html_elements("body") %>%
      html_children() %>%
      html_attrs_dfr(add_text = F) %>%
      as_tibble() %>% 
      filter(str_starts(id, "q-")) %>% 
      pull(id) %>% 
      paste0("#", .)
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Begin question by question loop ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    filled_df <- tibble(data.frame())
    for (question_id in question_ids){
      
      question_nodeset <- 
        html %>% 
        html_elements(question_id) %>% 
        html_children()
      
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Question intro ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      question_intro_row_count <- 
        question_nodeset %>% 
        html_attrs_dfr()  %>% 
        as_tibble() %>% 
        # within question, select elements that are not level 2 or level 1 section headers. These are (likely) components of question intro
        # this is definitely not the most efficient way to do this. 
        filter(class != "section level2" & class != "section level1" | is.na(class)) %>% 
        nrow()
      
      question_intro <- 
        question_nodeset %>% 
        .[2:question_intro_row_count] %>% 
        as.character() %>% 
        paste0(collapse = "\n")
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Question answer ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Needs special treatment because it must be a single letter
      answer <- 
        question_nodeset %>% 
        html_attrs_dfr()  %>% 
        as_tibble() %>% 
        filter(str_starts(id, "answer-") | id == "answer") %>% 
        pull(.text) %>% 
        str_remove_all("\r|\n|Answer|answer") %>% 
        tolower()
      
      answer_numeric <- which(letters == answer)
      
      if(nchar(answer_numeric)!= 1) stop(paste("The answer to the question with id", question_id ,"is not a single numeric digit. (Note that #question corresponds to the first question, question-1 corresponds to the second question, and so on.)"))
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Rest of question ----
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ###  ----
      ids_to_parse <- 
        question_nodeset %>% 
        html_attrs_dfr() %>% 
        as_tibble() %>% 
        filter(!is.na(id)) %>% 
        pull(id)
      
      # 
      col_names_except_intro_and_answer <- c(letters[1:13], "hint", "correct-message", "incorrect-message" )
      df_row <- tibble(data.frame(matrix(nrow = 1, ncol = length(col_names_except_intro_and_answer))))  # Intro and Answer are parsed earlier
      colnames(df_row) <- col_names_except_intro_and_answer
      
      for (id in ids_to_parse){
        
        out <- 
          html %>% 
          html_elements(question_id) %>% 
          html_elements(paste0("#", id)) %>% 
          as.character() %>% 
          qdapRegex::rm_between(left = "\\n<h2>", right = "</h2>\\n", clean = F) %>% 
          str_replace_all("\\n</div>", "</div>") %>%  # remove superfluous spacing
          str_remove("^<div id=\".*\" class=\"section level2\"><p>") %>% # remove div and paragraph marker at start of string. Causes unnecessary spacing
          str_remove("</p></div>$") # remove div and paragraph marker at end of string.
        
        id_stripped <- id %>% str_remove("-\\d+") # changes, for example, "#question-1" to "#question" so that it can be identified properly and slotted into the right column
        
        df_row[1, id_stripped] <- out
        
      }
      
      ## Add in intro and answer_numeric, since those were parsed earlier
      df_row_out <- bind_cols(question_title = question_id, question_intro = question_intro, answer_numeric = answer_numeric, df_row)
      
      filled_df <- 
        filled_df %>% 
        bind_rows(df_row_out)
      
    }
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plug into template df ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    old_names <- c("question_title","question_intro", "answer_numeric", letters[1:13], "hint", "correct-message", "incorrect-message" )
    new_names <- c("Title", "Question Text", "Answer", paste("Answer", 1:13), "Hint", "Message with correct answer", "Message with incorrect answer")
    # create named vector as dictionary
    naming_key <- setNames(object = old_names, nm = new_names)
    
    filled_and_renamed_df <- 
      filled_df %>% 
      rename(any_of(naming_key))
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Add quiz title and quiz content ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    final_output_df <- 
      filled_and_renamed_df %>% 
      mutate(`Quiz Title` = ifelse(row_number() == 1, quiz_title, "")) %>% 
      mutate(`Quiz Content` = ifelse(row_number() == 1, paste0(intro, collapse = ""), "")) %>% 
      mutate(Question = "single") %>% 
      mutate(`Total Point` = 1) %>% 
      select(`Quiz Title`, `Quiz Content`, Title, `Total Point` , Question, `Question Text`, any_of(paste("Answer", 1:13)), Answer,
             `Message with correct answer`, `Message with incorrect answer`, Hint)
    
    
    final_output_df %>% 
      writexl::write_xlsx(path = output_xlsx_path)
    
  }