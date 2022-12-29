#' @title Tutorial grade button
#'
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#'
#' Shiny ui and server logic for the grade computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `grade_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `grade_ui()`, must *not* be included in an R chunk with a `context`.
#'
#' @param id ID matching ui with server
#' @param label Label to appear on the submit grade button
#'
#' @import gt
#'
#' @export
grade_button_ui <- function(id, label = "grade tutorial") {
  ns <- NS(id)
  tagList(
    actionButton( ns("button"), label = label)
  )
}

#' @title Tutorial print grade
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#' @param id ID matching ui with server
#' @param label Label to appear on the button
#' @export
grade_print_ui <- function(id, label = "Download Grade") {
  ns <- NS(id)

  tagList(
    #actionButton( ns("printGrade"), label = "Download Grade"),
    downloadButton(ns("downloadHTML"), label)
  )
}

#' @title Tutorial grade output
#'
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#' @param id ID matching ui with server
#' @export
grade_output_ui <- function(id) {
  ns <- NS(id)

  tagList(
    htmlOutput(ns("pctout")),
    br(),
    htmlOutput(ns("tenout")),
    br(),
    gt_output(ns("tableout"))
  )

}

# Define the server logic for a module to compute grade
#' @title Tutorial grade server
#' @param id ID matching ui with server
#' @param rubric_list A data frame containing a vector of question names and a vector of points_possible for each question
#' @param num_try Number of tries allowed before grade deduction on that question. Default is 3.
#' @param deduction The percent (as a decimal) to be deducted for each additional incorrect attempt after num_try. Default is 0.1.
#' @param display vector of at least one c("percent", "itemize", "scaled"). Default is c("itemize", "scaled").
#'
#' @export
grade_server <- function(id, rubric_list, num_try = 3, deduction = 0.1, display = c("itemize", "scaled") ) {
  moduleServer(
    id,
    function(input, output, session) {
    # View grade
    observeEvent(input$button, {

      ns <- getDefaultReactiveDomain()$ns

      update_grade <- reactiveVal(NULL, label = "grade_recorder")

      # restore past submission
      past_submission_answer <- learnr:::retrieve_question_submission_answer(session, "grade_recorder")
      update_grade(past_submission_answer)

      #to prevent error if clicking without any submissions
      check <- try({grade_tutorial(submissions = update_grade(),
                                   rubric_list = rubric_list) }, silent = TRUE)
      if(class(check) == 'try-error'){
        get_grades <- list(grade_table = NULL, grade_percent = 0)
      }else if(class(check) != 'try-error'){
        get_grades <- grade_tutorial(submissions = update_grade() ,
                                     rubric_list = rubric_list)
      }

      output$tableout <- render_gt({
        if('itemize' %in% display){
          get_grades$grade_table
        }

      })

      output$pctout <- renderText({
        if('percent' %in% display){
          paste0('<span style=\"font-size:30px; font-weight:normal; color:red\">',
                 get_grades$grade_percent, "%")}
      })

      output$tenout <- renderText({
        if('scaled' %in% display){
          paste0('<span style=\"font-size:30px; font-weight:normal; color:red\">',
                 round(get_grades$grade_percent/10, 2), "/10")}
      })

      })
      #Print grade
      # observeEvent(input$printGrade, {
      #
      #   ns <- getDefaultReactiveDomain()$ns
      #
      #   update_grade <- reactiveVal(NULL, label = "grade_recorder")
      #
      #   # restore past submission
      #   past_submission_answer <- learnr:::retrieve_question_submission_answer(session, "grade_recorder")
      #   update_grade(past_submission_answer)
      #
      #   #to prevent error if clicking without any submissions
      #   check <- try({grade_tutorial(submissions = update_grade(),
      #                                rubric_list = rubric_list) }, silent = TRUE)
      #   if(class(check) == 'try-error'){
      #     get_grades <- list(grade_table = NULL, grade_percent = 0)
      #   }else if(class(check) != 'try-error'){
      #     get_grades <- grade_tutorial(submissions = update_grade() ,
      #                                  rubric_list = rubric_list)
      #   }
      #
      #   tab_html <- get_grades$grade_table %>%
      #     as_raw_html()
      #
      #   #tableHTML::write_tableHTML(tableHTML::tableHTML(tab_html), "test.html")
      #   #tableHTML::write_tableHTML(tab_html, "test.html")
      #   tableHTML::write_tableHTML(tableHTML::tableHTML(data.frame(x = c(1))), "test.html")
      # })


      output$downloadHTML <- downloadHandler(
          filename = function() {
            paste("rc-", Sys.Date(), ".html", sep="")
          },
          content = function(file) {
            ns <- getDefaultReactiveDomain()$ns

            update_grade <- reactiveVal(NULL, label = "grade_recorder")

            # restore past submission
            past_submission_answer <- learnr:::retrieve_question_submission_answer(session, "grade_recorder")
            update_grade(past_submission_answer)

            #to prevent error if clicking without any submissions
            check <- try({grade_tutorial(submissions = update_grade(),
                                         rubric_list = rubric_list) }, silent = TRUE)
            if(class(check) == 'try-error'){
              get_grades <- list(grade_table = NULL, grade_percent = 0)
            }else if(class(check) != 'try-error'){
              get_grades <- grade_tutorial(submissions = update_grade() ,
                                           rubric_list = rubric_list)
            }
            tab_html <- get_grades$grade_table %>%
              as.data.frame() %>%
              tableHTML(footer = format(as.POSIXct(Sys.time()),
                                        tz = "America/Chicago",
                                        usetz = TRUE),
                        rownames = FALSE,
                        caption = paste0("Scaled score: ",
                                         round(get_grades$grade_percent/10, 2),
                                         "/10"),
                        second_headers = list(c(4),
                                              c(paste0(get_grades$user_info$rc,
                                                       " - ",
                                                       get_grades$user_info$name))) ) %>%
              tableHTML::add_theme('rshiny-blue')

            tableHTML::write_tableHTML(tab_html,
                                       file)
          },
          contentType = "text/html"
        )

    }
  )
}

#set global variables needed to prevent package warning
utils::globalVariables(c("V1", "x1", "x0", "n", "num_try", ".",
                         "deduction", "points_possible", "score",
                         "num_attempts", "question", "correct"))

#create a table of grades and calculate overall percent
grade_tutorial <- function(submissions, rubric_list,
                           num_try = 3, deduction = 0.1){

  table <- submissions %>%
    data.table::data.table() %>%
    data.table::transpose() %>%
    tidyr::separate_rows(V1, sep = "//") %>%
    data.table::transpose() %>%
    data.table::tstrsplit(split = ",", names = TRUE)

  # issue using dplyr with shiny object
  # so need to manually set as table
  tmpdf <- data.frame(rc = table$V1, id = table$V3, time = table$V4,
                      type = table$V5,
                      question = table$V6,
                      answer = table$V7,
                      correct = table$V8) %>%
    mutate(correct = as.numeric(correct),
           type = as.factor(stringr::str_trim(type) ),
           rc = stringr::str_remove(rc, "\n"))

  # save user info for grade report output
  name <- tmpdf %>%
    filter(question == "Name") %>%
    pull(answer)
  name <- ifelse(is.null(name), "NA", name)

  user_info <- data.frame(rc = tmpdf$rc[1],
                          id = tmpdf$id[1],
                          name = name)

  #fix issue with exercise_result submitting multiple times if student get's kicked out
  #fix issue with exercise_result saving correct every time document is loaded
  split_1 <- tmpdf %>%
    filter(type == "question_submission") %>%
    filter(question != "lock_pressed") %>%
    mutate(correct = as.numeric(correct)) %>%
    distinct(time, type, question, answer, correct, .keep_all = TRUE)

  #find questions with multiple answers and concatenate
  split_1 <- aggregate(data=split_1, answer~.,
                       FUN = paste, collapse=",")

  split_2 <- tmpdf %>%
    filter(type == "exercise_result") %>%
    mutate(correct = as.numeric(correct)) %>%
    distinct(type, question, answer, correct, .keep_all = TRUE)

  newdf <- rbind(split_1, split_2)

  # format question names for merge
  newdf <- newdf %>%
    dplyr::filter(!is.na(question)) %>%
    # remove punctuation for merge
    dplyr::mutate(question = stringr::str_replace_all(question,
                                                      "[[:punct:]]", "")) %>%
    # Remove blank space for merge
    dplyr::mutate(question = gsub(" ", '', question))

  # create data frame of student submission summary
  correct_q <- newdf %>%
    # get counts of attempts
    dplyr::count(question, correct) %>%
    tidyr::pivot_wider(names_from = correct, values_from = n)

  # last submitted answer
  last_answer <- newdf %>%
    group_by(question) %>%
    filter(time == max(time))
  print(last_answer)

  # match student progress with all possible questions
  # rubric_list must be defined by creator
  grade_summary <- dplyr::left_join(rubric_list, correct_q, by = "question") %>%
    janitor::clean_names()
  # grade summary has question, points_possible, num_attempts_score

  # create placeholder column if x1 or x0 has not applied yet
  if(!"x1" %in% colnames(grade_summary)){
    grade_summary$x1 <- rep(0, length(grade_summary$question))
  }
  if(!"x0" %in% colnames(grade_summary)){
    grade_summary$x0 <- rep(0, length(grade_summary$question))
  }

  # replace na values with 0
  grade_summary[is.na(grade_summary$x1),"x1"] <- 0
  grade_summary[is.na(grade_summary$x0),"x0"] <- 0

  # create summary columns for num_attempts and score
  grade_organized <- grade_summary %>%
    dplyr::mutate(num_attempts = rowSums(dplyr::select(.,contains("x")),
                                  na.rm = TRUE)) %>%
    dplyr::mutate(score = ifelse(num_attempts>num_try,
                          points_possible*x1*max(0,(1-deduction*(num_attempts-num_try))),
                          points_possible*x1) ) %>%
    dplyr::select(-x1, -x0)

  print(grade_organized)

  grade_table <- grade_organized %>%
    gt::gt(rowname_col = "question") %>%
    gt::grand_summary_rows(
                  columns = c(points_possible, score),
                  fns = list(
                    total = ~sum(.)
                  ),
                  formatter = fmt_number)

  grade_percent = round(100*sum(grade_organized$score)/sum(grade_organized$points_possible),2)

  return(list(grade_table = grade_table,
              grade_percent = grade_percent,
              user_info = user_info))

}

