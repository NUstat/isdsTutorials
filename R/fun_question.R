#' Tutorial quiz questions
#'
#' Add interactive quiz questions to a tutorial.
#' Each quiz question is executed within a shiny runtime to provide more flexibility in the types of questions offered.
#' There are two new types of quiz questions:
#' \describe{
#'   \item{\code{learnr_blank}}{learnr_blank}
#'   \item{\code{learnr_dropdown}}{learnr_dropdown}
#'}
#'
#' @param text Text of quiz question.
#' @param type Type of quiz question.
#' @param correct For \code{question}, text to print for a correct answer (defaults
#'   to "Correct!"). For \code{answer}, a boolean indicating whether this answer is
#'   correct.
#' @param incorrect Text to print for an incorrect answer (defaults to "Incorrect")
#'   when \code{allow_retry} is \code{FALSE}.
#' @param try_again Text to print for an incorrect answer (defaults to "Incorrect")
#'   when \code{allow_retry} is \code{TRUE}.
#' @param message Additional message to display along with correct/incorrect feedback.
#'   This message is always displayed after a question submission.
#' @param post_message Additional message to display along with correct/incorrect feedback.
#'   If \code{allow_retry} is \code{TRUE}, this message will only be displayed after the
#'   correct submission.  If \code{allow_retry} is \code{FALSE}, it will produce a second
#'   message alongside the \code{message} message value.
#' @param loading Loading text to display as a placeholder while the question is loaded
#' @param submit_button Label for the submit button. Defaults to \code{"Submit Answer"}
#' @param try_again_button Label for the try again button. Defaults to \code{"Submit Answer"}
#' @param allow_retry Allow retry for incorrect answers. Defaults to \code{FALSE}.
#' @param random_answer_order Display answers in a random order.
#' @param options Extra options to be stored in the question object.
#' @rdname quiz
#' @import shiny
#' @export
drop_question <- function(
  text,
  ...,
  type = c("learnr_dropdown"),
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  message = NULL,
  post_message = NULL,
  loading = c("**Loading:** ", format(text), "<br/><br/><br/>"),
  submit_button = rlang::missing_arg(),
  try_again_button = rlang::missing_arg(),
  allow_retry = FALSE,
  random_answer_order = FALSE,
  options = list()
) {

  # one time tutor initialization
  #initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  #answers <- list(answer)
  #lapply(answers, function(answer) {
  #  checkmate::assert_class(answer, "tutorial_question_answer")
  #})

  # verify chunk label if necessary
  #verify_tutorial_chunk_label()

  # count total correct answers to decide between radio/checkbox
  #answers_split <- learnr:::answers_split_type(answers)
  #total_correct <- sum(vapply(answers_split[["literal"]], `[[`, logical(1), "correct"))

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- label %||% learnr:::random_question_id()

  # i18nize button labels if default values are used
  submit_button <-
    if (rlang::is_missing(submit_button)) {
      learnr:::i18n_span("button.questionsubmit", "Submit Answer")
    } else {
      learnr:::quiz_text(submit_button)
    }

  try_again_button <-
    if (rlang::is_missing(try_again_button)) {
      learnr:::i18n_span("button.questiontryagain", "Try Again")
    } else {
      learnr:::quiz_text(try_again_button)
    }


  ret <- list(
    type = type,
    label = label,
    question = learnr:::quiz_text(text),
    answers = answers,
    button_labels = list(
      submit = submit_button,
      try_again = try_again_button
    ),
    messages = list(
      correct = learnr:::quiz_text(correct),
      try_again = learnr:::quiz_text(try_again),
      incorrect = learnr:::quiz_text(incorrect),
      message = learnr:::quiz_text(message),
      post_message = learnr:::quiz_text(post_message)
    ),
    ids = list(
      answer = NS(q_id)("answer"),
      question = q_id
    ),
    loading = learnr:::quiz_text(loading),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    # Set a seed for local testing, even though it is overwritten for each shiny session
    seed = learnr:::random_seed(),
    options = options
  )
  class(ret) <- c(type, "notes_question")
  ret
}


#' @rdname quiz
#' @import shiny
#' @export
blank_question <- function(
  text,
  ...,
  type = c("learnr_blank"),
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  message = NULL,
  post_message = NULL,
  loading = c("**Loading:** ", format(text), "<br/><br/><br/>"),
  submit_button = rlang::missing_arg(),
  try_again_button = rlang::missing_arg(),
  allow_retry = FALSE,
  random_answer_order = FALSE,
  options = list()
) {

  # one time tutor initialization
  #initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  #answers <- list(answer)

  #lapply(answers, function(answer) {
  #  checkmate::assert_class(answer, "tutorial_question_answer")
  #})

  # verify chunk label if necessary
  #verify_tutorial_chunk_label()

  # count total correct answers to decide between radio/checkbox
  #answers_split <- learnr:::answers_split_type(answers)
  #total_correct <- sum(vapply(answers_split[["literal"]], `[[`, logical(1), "correct"))

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- label %||% learnr:::random_question_id()

  # i18nize button labels if default values are used
  submit_button <-
    if (rlang::is_missing(submit_button)) {
      learnr:::i18n_span("button.questionsubmit", "Submit Answer")
    } else {
      learnr:::quiz_text(submit_button)
    }

  try_again_button <-
    if (rlang::is_missing(try_again_button)) {
      learnr:::i18n_span("button.questiontryagain", "Try Again")
    } else {
      learnr:::quiz_text(try_again_button)
    }

  ret <- list(
    type = type,
    label = label,
    question = learnr:::quiz_text(text),
    answers = answers,
    button_labels = list(
      submit = submit_button,
      try_again = try_again_button
    ),
    messages = list(
      correct = learnr:::quiz_text(correct),
      try_again = learnr:::quiz_text(try_again),
      incorrect = learnr:::quiz_text(incorrect),
      message = learnr:::quiz_text(message),
      post_message = learnr:::quiz_text(post_message)
    ),
    ids = list(
      answer = NS(q_id)("answer"),
      question = q_id
    ),
    loading = learnr:::quiz_text(loading),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    # Set a seed for local testing, even though it is overwritten for each shiny session
    seed = learnr:::random_seed(),
    options = options
  )
  class(ret) <- c(type, "notes_question")
  ret
}

