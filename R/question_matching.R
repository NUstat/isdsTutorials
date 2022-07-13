#' Ranking question for learnr tutorials.
#'
#' Thank you to the sortable package for creating functions that allow ranking.
#' This is copied from the function question_rank to allow for matching options.
#'
#' Add interactive ranking tasks to your `learnr` tutorials.  The student can
#' drag-and-drop the answer options into the desired order.
#'
#' Each set of answer options must contain the same set of answer options. When
#' the question is completed, the first correct answer will be displayed.
#'
#' Note that, by default, the answer order is randomized.
#'
#' @param choices a vector of choices that will remain stationary that the answers are sorted with.
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = matching`.
#'
#' @importFrom learnr question_ui_initialize
#' @importFrom learnr question_ui_completed
#' @importFrom learnr question_ui_try_again
#' @importFrom learnr question_is_valid
#' @importFrom learnr question_is_correct
#' @import shiny
#'
#' @export
# @examples
# ## Example of rank problem inside a learnr tutorial
# if (interactive()) {
#   learnr::run_tutorial("question_matching", package = "sortable")
# }
question_matching <- function(
    text,
    ...,
    choices = choices,
    correct = "Correct!",
    incorrect = "Incorrect",
    loading = c("**Loading:** ", text, "<br/><br/><br/>"),
    submit_button = "Submit Answer",
    try_again_button = "Try Again",
    allow_retry = FALSE,
    random_answer_order = TRUE,
    options = sortable::sortable_options()
) {
  matching_question(
    text = text,
    ...,
    choices = choices,
    type = "matching",
    correct = correct,
    incorrect = incorrect,
    loading  = loading,
    submit_button = submit_button,
    try_again_button = try_again_button,
    allow_retry = allow_retry,
    random_answer_order = random_answer_order,
    options = options
  )
}

#' @rdname quiz
#' @export
matching_question <- function(
    text,
    ...,
    choices = choices,
    type = c("matching"),
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
    choices = choices,
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
  class(ret) <- c(type, "tutorial_question")
  ret
}

#' @export
#' @seealso question_matching
question_ui_initialize.matching <- function(question, value, ...) {

  # quickly validate the all possible answers are possible
  answer <- question$answers[[1]]
  possible_answer_vals <- sort(answer$option)
  for (answer in question$answers) {
    if (!identical(
      possible_answer_vals,
      sort(answer$option)
    )) {
      stop(
        "All question_matching answers MUST have the same set of answers. (Order does not matter.) ",
        "\nBad set: ", paste0(answer$option, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # if no label order has been provided
  if (!is.null(value)) {
    labels <- value
  } else {
    labels <- question$answers[[1]]$option

    # if the question is to be displayed in random order, shuffle the options
    if (
      isTRUE(question$random_answer_order) # and we should randomize the order
    ) {
      labels <- sample(labels, length(labels))
    }
  }

  sortable::bucket_list(
    header = question$question,
    sortable::add_rank_list(
      text = NULL,
      labels = question$choices,
      options = sortable::sortable_options(draggable = FALSE)
    ),
    sortable::add_rank_list(
      text = NULL,
      input_id = question$ids$answer,
      labels = labels,
      options = sortable::sortable_options(swap = TRUE)
    ),
    orientation = "horizontal"
  )
  # return the rank_list htmlwidget
  # rank_list(
  #   text = question$question,
  #   input_id = question$ids$answer,
  #   labels = labels,
  #   options = question$options
  # )
}

#' @export
#' @seealso question_matching
question_ui_completed.matching <- function(question, value, ...) {
  # TODO display correct values with X or √ compared to best match
  # TODO DON'T display correct values (listen to an option?)
  learnr::disable_all_tags(
    sortable::bucket_list(
      header = question$question,
      sortable::add_rank_list(
        text = NULL,
        labels = question$choices,
        options = sortable::sortable_options(draggable = FALSE)
      ),
      sortable::add_rank_list(
        text = NULL,
        input_id = question$ids$answer,
        labels = value,
        options = sortable::sortable_options(disabled = TRUE)
      ),
      orientation = "horizontal"
    )
    # rank_list(
    #   text = question$question,
    #   input_id = question$ids$answer,
    #   labels = value,
    #   options = modifyList(
    #     question$options,
    #     sortable::sortable_options(disabled = TRUE)
    #   )
    # )
  )
}

#' @export
#' @seealso question_matching
question_ui_try_again.matching <- function(question, value, ...) {
  learnr::disable_all_tags(
    sortable::bucket_list(
      header = question$question,
      sortable::add_rank_list(
        text = NULL,
        labels = question$choices,
        options = sortable::sortable_options(draggable = FALSE)
      ),
      sortable::add_rank_list(
        text = NULL,
        input_id = question$ids$answer,
        labels = value,
        options = sortable::sortable_options(disabled = TRUE)
      ),
      orientation = "horizontal"
    )

  )
}


#' @export
#' @seealso question_matching
question_is_correct.matching <- function(question, value, ...) {
  # for each possible answer, check if it matches
  for (answer in question$answers) {
    if (identical(answer$option, value)) {
      # if it matches, return the correct-ness and its message
      return(learnr::mark_as(answer$correct, answer$message))
    }
  }
  # no match found. not correct
  learnr::mark_as(FALSE, NULL)
}
