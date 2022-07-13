#' Fill in the blank question for learnr tutorials.
#'
#' Add interactive fill in the blank tasks to your `learnr` tutorials.
#'
#'
#' @param rows,cols Defines the size of the text input area in terms of the
#'   number of rows or character columns visible to the user. If either `rows`
#'   or `cols` are provided, the quiz input will use [shiny::textAreaInput()]
#'   for the text input, otherwise the default input element is a single-line
#'   [shiny::textInput()].
#' @param trim Logical to determine if whitespace before and after the answer
#'   should be removed.  Defaults to `TRUE`.
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = blank`.
#'
# @return Returns a learnr question of type `"blank"`.
#'
#' @export
question_blank <- function(
  text,
  ...,
  type = "blank",
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
    type = "blank",
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

#' @export
blank_question <- function(
    text,
    ...,
    type = c("blank"),
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


#' @export
#' @seealso question_blank
question_ui_initialize.blank <- function(question, value, ...) {
  choice_names <- learnr:::answer_labels(question, exclude_answer_fn = TRUE)
  choice_values <- learnr:::answer_values(question, exclude_answer_fn = TRUE)
  # Use textInput() unless one of rows or cols are provided
  textInputFn <-
    if (is.null(question$options$rows) && is.null(question$options$cols)) {
      textInput
    } else {
      function(...) {
        textAreaInput(..., cols = question$options$cols, rows = question$options$rows)
      }
    }

  #split question by blanks
  split <- unlist(stringr::str_split(question$question, pattern = "___") )
  #pos <- NULL
  #for(i in 1:length(split)){
  #  if(i < length(split)){
  #    pos <- c(pos, split[i], "___")
  #  }
  #  else{
  #    pos <- c(pos, split[i])
  #  }
  #}

  bootstrapPage(
    div(style="display:inline-block",split[1]),
    div(style="display:inline-block",
        textInputFn(paste0(question$ids$answer),
                    label = NULL,
                    placeholder = question$options$placeholder,
                    value = value, width = "100%")
    ),
    div(style="display:inline-block",split[2])
  )

}

#' @export
#' @seealso question_blank
question_is_valid.blank <- function(question, value, ...) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (isTRUE(question$options$trim)) {
    return(nchar(stringr::str_trim(value)) > 0)
  } else{
    return(nchar(value) > 0)
  }
}

#' @export
#' @seealso question_blank
question_is_correct.blank <- function(question, value, ...) {
  #how is value being taken in
  if (nchar(value) == 0) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      showNotification("Please enter some text before submitting", type = "error")
    }
    #shiny::validate("Please enter some text")
    value
  }

  if (isTRUE(question$options$trim)) {
    value <- stringr::str_trim(value)
  }

  compare_answer <- function(answer) {
    answer_value <- answer$value
    if (isTRUE(question$options$trim)) {
      answer_value <- stringr::str_trim(answer_value)
    }
    if (isTRUE(all.equal(answer_value, value))) {
      learnr::mark_as(answer$correct, answer$message)
    }
  }

  check_answer <- function(answer) {
    answer_checker <- eval(parse(text = answer$value), envir = rlang::caller_env(2))
    answer_checker(value)
  }

  for (answer in question$answers) {
    ret <- switch(
      answer$type,
      "function" = check_answer(answer),
      compare_answer(answer)
    )
    if (inherits(ret, "learnr_mark_as")) {
      return(ret)
    }
  }

  learnr::mark_as(FALSE, NULL)
}

#' @export
#' @seealso question_blank
question_ui_completed.blank <- function(question, value, ...) {
  choice_names <- learnr:::answer_labels(question, exclude_answer_fn = TRUE)
  choice_values <- learnr:::answer_values(question, exclude_answer_fn = TRUE)
  # Use textInput() unless one of rows or cols are provided
  textInputFn <-
    if (is.null(question$options$rows) && is.null(question$options$cols)) {
      textInput
    } else {
      function(...) {
        textAreaInput(..., cols = question$options$cols, rows = question$options$rows)
      }
    }


  split <- unlist(stringr::str_split(question$question, pattern = "___") )

  bootstrapPage(
    #tagList(
    div(style="display:inline-block",split[1]),
    div(style="display:inline-block",
        learnr::finalize_question(
          textInputFn(paste0(question$ids$answer),
                      label = NULL,
                      placeholder = question$options$placeholder,
                      value = value, width = "100%")
        )
    ),
    div(style="display:inline-block",split[2])
  )

}
