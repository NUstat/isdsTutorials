#' Dropdown question
#'
#' @description
#' Creates a dropdown button tutorial quiz question. The student can select only
#' one dropdown before submitting their answer.
#'
#'
#'
#' @param text  Question or option text
#' @param type Type of quiz question. Typically this can be automatically determined
#'   based on the provided answers. Pass \code{"radio"} to indicate that even though
#'   multiple correct answers are specified that inputs which include only one correct
#'   answer are still correct. Pass \code{"checkbox"} to force the use of checkboxes
#'   (as opposed to radio buttons) even though only once correct answer was provided.
#' @param correct For \code{question}, text to print for a correct answer (defaults
#'   to "Correct!"). For \code{answer}, a boolean indicating whether this answer is
#'   correct.
#' @param incorrect Text to print for an incorrect answer (defaults to "Incorrect")
#'   when \code{allow_retry} is \code{FALSE}.
#' @param try_again Text to print for an incorrect answer (defaults to "Incorrect")
#'   when \code{allow_retry} is \code{TRUE}.
#' @param allow_retry Allow retry for incorrect answers. Defaults to \code{FALSE}.
#' @param random_answer_order Display answers in a random order.
#'
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
