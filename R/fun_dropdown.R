#' Dropdown question
#'
#' Creates a dropdown button tutorial quiz question. The student can select only
#' one dropdown before submitting their answer. Note: Multiple correct
#' answers are allowed.
#'
#' @examples
#' question_dropdown(
#'   "Pick the letter B",
#'   learnr:::answer("A"),
#'   learnr:::answer("B", correct = TRUE),
#'   learnr:::answer("C"),
#'   learnr:::answer("D"),
#'   allow_retry = TRUE,
#'   random_answer_order = TRUE
#' )
#'
#' @inheritParams learnr::question
#' @inheritParams drop_question
#' @inheritParams learnr::answer
#' @inheritParams shiny::selectInput
#' @param ... Answers created with [answer()] or extra parameters passed onto
#'   [question()]. Function answers are ignored for radio questions because the
#'   user is required to select a single answer.
#'
#' @return Returns a learnr question of type `"learnr_dropdown"`.
#'
#' @family Interactive Questions
#' @export

question_dropdown <- function(
  text,
  ...,
  #answer,
  type = "learnr_dropdown",
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  allow_retry = FALSE,
  random_answer_order = FALSE
) {
  question <-
    drop_question(
      text = text,
      ...,
      #answer = answer,
      type = "learnr_dropdown",
      correct = correct,
      incorrect = incorrect,
      allow_retry = allow_retry,
      random_answer_order = random_answer_order
    )

  answer_is_fn <- FALSE

  question
}

#' @export
question_ui_initialize.learnr_dropdown <- function(question, value, ...) {

  choice_names <- learnr:::answer_labels(question, exclude_answer_fn = TRUE)
  choice_values <- learnr:::answer_values(question, exclude_answer_fn = TRUE)

  selectInput(
    question$ids$answer,
    label = question$question,
    choices = choice_values,
    #choiceNames = choice_names,
    #choiceValues = choice_values,
    selected = value %||% character(0), # avoid selecting the first item when value is NULL
    width = "50%"
  )
}

#' @export
question_is_correct.learnr_dropdown <- function(question, value, ...) {
  for (ans in question$answers) {
    if (as.character(ans$option) == value) {
      return(mark_as(
        ans$correct,
        ans$message
      ))
    }
  }
  mark_as(FALSE, NULL)
}


#' @export
question_ui_completed.learnr_dropdown <- function(question, value, ...) {
  choice_values <- learnr:::answer_values(question)

  # update select answers to have X or √
  choice_names_final <- lapply(question$answers, function(ans) {
    if (ans$correct) {
      tagClass <- "correct"
    } else {
      tagClass <- "incorrect"
    }
    tags$span(ans$label, class = tagClass)
  })

  finalize_question(
    selectInput(
      question$ids$answer,
      label = question$question,
      choices = choice_values,
      #choiceValues = choice_values,
      #choiceNames = choice_names_final,
      selected = value,
      width = "50%"
    )
  )
}
