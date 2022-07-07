#' Tutorial grade options
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
#' @export
grade_ui <- function(id, label = "grade tutorial") {
  ns <- NS(id)
  tagList(

    actionButton( ns("button"), label = label),
    br(),
    htmlOutput(ns("pctout")),
    br(),
    gt_output(ns("tableout"))

  )
}

# Define the server logic for a module to compute grade
#' Grade tutorial server
#' @param id ID matching ui with server
#' @param rubric_list A data frame containing a vector of question names and a vector of points_possible for each question
#' @param num_try Number of tries allowed before grade deduction on that question. Default is 3.
#' @param deduction The percent (as a decimal) to be deducted for each additional incorrect attempt. Default is 0.1.
#' @param display One of c("both", "itemize", "percent")
#'
#' @export
grade_server <- function(id, rubric_list, num_try = 3, deduction = 0.1, display = "both") {
  moduleServer(
    id,
    function(input, output, session) {

    observeEvent(input$button, {

      ns <- getDefaultReactiveDomain()$ns

      update_grade <- reactiveVal(NULL, label = "grade_recorder")

      # restore past submission
      past_submission_answer <- learnr:::retrieve_question_submission_answer(session, "grade_recorder")
      update_grade(past_submission_answer)

      get_grades <- grade_tutorial(submissions = update_grade() ,
                     rubric_list = rubric_list)

      output$tableout <- render_gt({
        if(display != "percent"){
          get_grades$grade_table
        }

      })

      output$pctout <- renderText({
        if(display != "itemize"){
          paste0('<span style=\"font-size:30px; font-weight:normal; color:red\">',
                 get_grades$grade_percent, "%")
        }

      })

      })

    }
  )
}

#set global variables needed to prevent package warning
utils::globalVariables(c("V1", "x1", "x0", "n", "num_try", ".",
                         "deduction", "points_possible", "score",
                         "num_attempts"))

#create a table of grades and calculate overall percent
grade_tutorial <- function(submissions, rubric_list,
                           num_try = 3, deduction = 0.1){

  #to prevent error if clicking without any submissions
  if(is.null(submissions)){
    return(list(grade_table = NULL, grade_percent = 0))
  }

  table <- submissions %>%
    data.table::data.table() %>%
    data.table::transpose() %>%
    tidyr::separate_rows(V1, sep = "//") %>%
    data.table::transpose() %>%
    data.table::tstrsplit(split = ",", names = TRUE)

  # issue using dplyr with shiny object
  # so need to manually set as table
  newdf <- data.frame(id = table$V3, time = table$V4,
                      type = table$V5,
                      question = table$V6,
                      correct = table$V7)

  # create data frame of student submission summary
  correct_q <- newdf %>%
    dplyr::filter(!is.na(question)) %>%
    # remove punctuation for merge
    dplyr::mutate(question = stringr::str_replace_all(question,
                                      "[[:punct:]]", "")) %>%
    # Remove blank space for merge
    dplyr::mutate(question = gsub(" ", '', question)) %>%
    # get counts of attempts
    dplyr::count(question, correct) %>%
    tidyr::pivot_wider(names_from = correct, values_from = n)

  # match student progress with all possible questions
  # rubric_list must be defined by creator
  grade_summary <- dplyr::left_join(rubric_list, correct_q) %>%
    janitor::clean_names()

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
                          points_possible*x1*(1-deduction*(num_attempts-num_try)),
                          points_possible*x1) ) %>%
    dplyr::select(-x1, -x0)

  grade_table <- grade_organized %>%
    gt::gt(rowname_col = "question") %>%
    gt::grand_summary_rows(
                  columns = c(points_possible, score),
                  fns = list(
                    total = ~sum(.)
                  ),
                  formatter = fmt_number)

  grade_percent = round(100*sum(grade_organized$score)/sum(grade_organized$points_possible),2)

  return(list(grade_table = grade_table, grade_percent = grade_percent))

}

