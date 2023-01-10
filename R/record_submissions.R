#' @title Tutorial grade recorder
#'
#' @description
#' The following function was modified from the learnr package, available at
#' https://github.com/rstudio/learnr. Many thanks to those who
#' developed a fantastic tool for courses that teach R and use the learnr
#' package.
#'
#' @param tutorial_id Tutorial ID obtained from session
#' @param tutorial_version Tutorial version obtained from session
#' @param user_id User ID obtained from session
#' @param event Record events of type question_submission or exercise_result
#' @param data Data from question or exercise with label and whether submission was correct
#'
#' The following isds recorder uses the server functions to store
#' historical user's submissions
#' @export
isds_recorder <- function(tutorial_id, tutorial_version,
                         user_id, event, data) {
  if(event %in% c("question_submission", "exercise_result") &&
     data$label != "grade_recorder" ){

    tmp <- paste0(tutorial_id,"##", tutorial_version, "##",
                  user_id, "##", Sys.time() ,"##",
                  event, "##", data$label, "##",
                  #answer is for question and code is for exercise
                  data$answer, data$code, "##",
                  as.numeric(data$correct),
                  as.numeric(data$feedback$correct),  "##",
                  "1",
                  as.numeric(data$checked), #test this
                  sep = "")

    callModule(
      recorder_module_server,
      "grade_recorder",
      record = tmp
    )
  }
}



recorder_module_server <- function(input, output, session, record) {

  question_state <- reactiveVal()

  observeEvent(
    req(session$userData$learnr_state() == "restored"),
    once = TRUE,
    recorder_module_server_impl(input, output, session, record, question_state)
  )
}


recorder_module_server_impl <- function(
    input, output, session,
    record,
    question_state = NULL
) {

  ns <- getDefaultReactiveDomain()$ns
  # set a seed for each user session for question methods to use
  #record$seed <- learnr:::random_seed()

  # only set when a submit button has been pressed
  # (or reset when try again is hit)
  # (or set when restoring)
  update_grade <- reactiveVal(NULL, label = "grade_recorder")

  is_correct_info <- NULL
  is_done <- NULL

  # restore past submission
  #  If no prior submission, it returns NULL
  past_submission_answer <- learnr:::retrieve_question_submission_answer(session, "grade_recorder")
  # initialize like normal... nothing has been submitted
  #   or
  # initialize with the past answer
  update_grade(past_submission_answer)

  if(!is.null(record)){
      newval <- paste0(update_grade(), record,
                       collapse = "\n", sep = "//")

      update_grade(newval)
  }

  # output$grade_record <- renderText({
  #   update_grade()
  # })

  observe( {
    # submit question to server
    learnr:::event_trigger(
      session = session,
      event   = "question_submission",
      data    = list(
        label    = "grade_recorder",
        question = NULL,
        answer   = update_grade(),
        correct  = NULL
      )
    )

  })

  data = list(
        label    = "grade_recorder",
        question = NULL,
        answer   = update_grade(),
        correct  = NULL
      )

  learnr:::set_tutorial_state("grade_recorder", data)

  return(update_grade() )
}
