#' Text blank question
#'
#' @description
#' Creates a tutorial question asking the student to enter text. The default
#' text input is appropriate for short or single-line text entry. For longer
#' text input, set the `rows` and/or `cols` argument to create a larger text
#' area.
#'
#' When used with [answer()], the student's submission must match the answer
#' exactly, minus whitespace trimming if enabled with `trim = TRUE`. For more
#' complicated submission evaluation, use [answer_fn()] to provide a function
#' that checks the student's submission. For example, you could provide a
#' function that evaluates the user's submission using
#' [regular expressions][base::regex].
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
#'
#' @param rows,cols Defines the size of the text input area in terms of the
#'   number of rows or character columns visible to the user. If either `rows`
#'   or `cols` are provided, the quiz input will use [shiny::textAreaInput()]
#'   for the text input, otherwise the default input element is a single-line
#'   [shiny::textInput()].
#' @param trim Logical to determine if whitespace before and after the answer
#'   should be removed.  Defaults to `TRUE`.
#' @param random_answer_order `r lifecycle::badge('deprecated')` Random answer
#'   order for text questions is automatically disabled to ensure that the
#'   submission is checked against each answer in the order they were provided
#'   by the author.
#' @inheritParams shiny::textInput
#' @param ... Answers created with [answer()] or [answer_fn()], or extra
#'   parameters passed onto [question()]. Answers with custom function checking
#'
#' @return Returns a learnr question of type `"learnr_blank"`.
#'
#' @family Interactive Questions
#' @export
question_blank <- function(
  text,
  ...,
  type = "learnr_blank",
  correct = paste0(icons::ionicons("checkmark-outline") ),
  incorrect = paste0(icons::ionicons("close-outline") ),
  try_again = incorrect,
  allow_retry = FALSE,
  random_answer_order = FALSE,
  placeholder = "Enter answer here...",
  trim = TRUE,
  rows = NULL,
  cols = NULL,
  options = list()
) {
  checkmate::assert_character(placeholder, len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_logical(trim, len = 1, null.ok = FALSE, any.missing = FALSE)

  question <- blank_question(
    text = text,
    ...,
    type = "learnr_blank",
    correct = correct,
    incorrect = incorrect,
    allow_retry = allow_retry,
    random_answer_order = FALSE,
    options = utils::modifyList(
      options,
      list(
        placeholder = placeholder,
        trim = trim,
        rows = rows,
        cols = cols
      )
    )
  )
    question

}

