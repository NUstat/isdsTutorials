#' Word bank for learnr tutorials.
#'
#' The following function was modified from the sortable package, available at
#' https://github.com/rstudio/sortable and a solution thread by stefan's on Jun 01, 2022.
#' Many thanks to the sortable author who developed tools to drag and drop objects in rank order.
#' This extends the "question_rank" option so that you can drag any options from the
#' word bank into the answer blanks.
#'
#' Add interactive word bank tasks to your `learnr` tutorials.  The student can
#' drag-and-drop the answer options from the word bank to match the corresponding
#' options.
#'
#' The choices should be a set of options that the word bank choices will
#' match to. The choices should be of equal length to the answers.
#'
#' The answer should contain the set of solutions that match the choices. The first
#' answer much match the first choice and so forth.
#'
#' The word bank should be a set of options for the user to choose from to
#' drag and drop into the blanks. If can be shorter or longer than then number
#' of blanks but must include all unique answer options.
#'
#' @param choices a vector of choices that will remain stationary in the left column.
#' @param wordbank a vector of choices to be placed into the blanks. Can be shorter or longer than the number of blanks.
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = wordbank`.
#'
#' @importFrom learnr question_ui_initialize
#' @importFrom learnr question_ui_completed
#' @importFrom learnr question_ui_try_again
#' @importFrom learnr question_is_valid
#' @importFrom learnr question_is_correct
#' @import learnr
#' @import shiny
#'
#' @examples
#' question_wordbank(
#'   "Drag the numbers to match the corresponding letter's position in the alphabet.",
#'   choices = c("C", "B", "A", "D"),
#'   wordbank = c("1", "2", "3", "4"),
#'   learnr::answer(c("3", "2", "1", "4"), correct = TRUE),
#'   allow_retry = TRUE
#' )
#'
#' @export
question_wordbank <- function(
    text,
    ...,
    choices = choices,
    wordbank = wordbank,
    correct = "Correct!",
    incorrect = "Incorrect",
    loading = c("**Loading:** ", text, "<br/><br/><br/>"),
    submit_button = "Submit Answer",
    try_again_button = "Try Again",
    allow_retry = FALSE,
    random_answer_order = TRUE,
    options = sortable::sortable_options()
) {
  #ISDStutorials:::wordbank_question(
  wordbank_question(
    text = text,
    ...,
    choices = choices,
    wordbank = wordbank,
    type = "wordbank",
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


wordbank_question <- function(
    text,
    ...,
    choices = choices,
    wordbank = wordbank,
    type = c("wordbank"),
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
    wordbank = wordbank,
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
#' @seealso question_wordbank
question_ui_initialize.wordbank <- function(question, value, ...) {
  ns <- NS(question$ids$question)

  # get number of parts
  num <- length(question$choices)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- as.character( unlist(value) )
  } else {
    ans <- rep(NULL, num)
  }
     # need to add randomization to question init
     # or it will randomize everytime
     #question$wordbank <- sample(question$wordbank, length(question$wordbank))
     #order <- sample(1:num)
     #question$choices <- question$choices[order]
     # }

  #if the question is to be displayed in random order, shuffle the options
  if (isTRUE(question$random_answer_order) ) {
      labels <- sample(question$wordbank, length(question$wordbank))
  }else{labels <- question$wordbank}



  # set input and bucket ids
  input_ids = lapply(seq(1,num), function(x) paste0("select", x) )
  css_ids = lapply(seq(1,num), function(x) paste0("drag_to", x) )
  set_bucket <- sortable::sortable_js_capture_bucket_input(input_id = question$ids$answer,
                                                 input_ids = input_ids,
                                                 css_ids = css_ids)




  icons <- function(x) {lapply(x,function(x){tags$div(tags$strong(x))})}

  fluidPage(
    div(class = "panel-heading",
        strong(question$question)  ),

    fixedRow(
      column(
        width = 12,
        div(
          div(
            class = "panel panel-default",
            div(class = "panel-heading", "Drag and drop"),
            lapply(seq(1,num), function(x)
              div(
                class = "panel-body",
                style="display:inline-block",
                id = paste0("drag_from", x),
                icons(c(labels[x]))
              )),
            div(
              class = "panel-body",
              style="display:inline-block",
              id = "sortable_bin",
              icon("trash")
            )
          )
        )
      )
    ),
    div( id = "bucket",
    lapply(seq(1,num), function(x)
      fixedRow(
        column(
          width = 3,
          #div(
            div(
              class = "panel panel-default",
              div(
                class = "panel-body",
                id = css_ids[x], #paste0("drag_to",x)
                icons(ans[x])
              )
            )
          #)
        ),
        column(
          width = 9,
          p(paste0(question$choices[x]) ) )
      ) #ends fixed row
    ) #ends lapply
    ), #ends bucket div group

    # separate columns for each drag
    lapply(seq(1,num), function(x)
      sortable::sortable_js(
        paste0("drag_from", x),
        options = sortable::sortable_options(
          group = list(
            pull = "clone",
            name = "group1",
            put = FALSE
          )
        )
      ) ),
    lapply(seq(1,num), function(x)
      sortable::sortable_js(
        paste0("drag_to",x),
        options = sortable::sortable_options(
          group = list(
            group = "group1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )), #paste0("select",x)
          onLoad = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )) # << solution by stefan on Jun 01, 2022
        ) )
    ),
    sortable::sortable_js(
      "sortable_bin",
      options = sortable::sortable_options(
        group = list(
          group = "sortGroup1",
          put = TRUE,
          pull = FALSE
        ),
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    )
  )

}


#' @export
#' @seealso question_wordbank
question_is_correct.wordbank <- function(question, value, ...) {

  for (ans in question$answers) {

      if (identical(as.character(ans$option), as.character(value) ) ) {
      return(mark_as(
        ans$correct,
        ans$message
      ))
      }

  }
  mark_as(FALSE, NULL)

}



#' @export
#' @seealso question_wordbank
question_ui_completed.wordbank <- function(question, value, ...) {
  # TODO display correct values with X or √ compared to best match
  # TODO DON'T display correct values (listen to an option?)

  ns <- NS(question$ids$question)

  labels <- question$wordbank

  ans <- unlist(value)

  num <- length(question$choices)
  input_ids = lapply(seq(1,num), function(x) paste0("select", x) )
  css_ids = lapply(seq(1,num), function(x) paste0("drag_to", x) )
  set_bucket <- sortable::sortable_js_capture_bucket_input(input_id = question$ids$answer,
                                                 input_ids = input_ids,
                                                 css_ids = css_ids)

  icons <- function(x) {lapply(x,function(x){tags$div(tags$strong(x))})}

  #learnr::disable_all_tags(

  fluidPage(
    div(class = "panel-heading",
        strong(question$question)  ),

    fixedRow(
      column(
        width = 12,
        div(
          div(
            class = "panel panel-default",
            div(class = "panel-heading", "Word Bank"),
            lapply(seq(1,num), function(x)
              div(
                class = "panel-body",
                style="display:inline-block",
                id = paste0("drag_from", x),
                icons(c(labels[x]))
              )),
            div(
              class = "panel-body",
              style="display:inline-block",
              id = "sortable_bin",
              icon("trash")
            )
          )
        )
      )
    ),
    div( id = "bucket",
         lapply(seq(1,num), function(x)
           fixedRow(
             column(
               width = 3,
               #div(
               div(
                 class = "panel panel-default",
                 div(
                   class = "panel-body",
                   id = css_ids[x], #paste0("drag_to",x)
                   icons(ans[x])
                 )
               )
               #)
             ),
             column(
               width = 9,
               p(paste0(question$choices[x]) ) )
           ) #ends fixed row
         ) #ends lapply
    ), #ends bucket div group

    # separate columns for each drag
    lapply(seq(1,num), function(x)
      sortable::sortable_js(
        paste0("drag_from", x),
        options = sortable::sortable_options(
          group = list(
            pull = "clone",
            name = "group1",
            put = FALSE
          )
        )
      ) ),
    lapply(seq(1,num), function(x)
      sortable::sortable_js(
        paste0("drag_to",x),
        options = sortable::sortable_options(
          group = list(
            group = "group1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )), #paste0("select",x)
          onLoad = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )) # << solution by stefan on Jun 01, 2022
        ) )
    ),
    sortable::sortable_js(
      "sortable_bin",
      options = sortable::sortable_options(
        group = list(
          group = "sortGroup1",
          put = TRUE,
          pull = FALSE
        ),
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    )
  )
  #)

}


#' @export
#' @seealso question_wordbank
question_is_valid.wordbank <- function(question, value, ...) {
  if (length(as.character( unlist(value) ) ) != length(question$choices)) {
    return(FALSE)
  }
  return(TRUE)
}
