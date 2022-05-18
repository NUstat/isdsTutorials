#' Dropdown question
#'
#' @description
#' Creates a dropdown button tutorial quiz question. The student can select only
#' one dropdown before submitting their answer.
#'
#'
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
