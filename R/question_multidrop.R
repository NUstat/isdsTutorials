#' Multiple drop-down question
#'
#' @description
#' Creates a multiple drop-down tutorial quiz question.
#' Each drop-down will have the same list of options that correspond to a vector of questions.
#' Each drop-down only allow only one selection.
#'
#'
#' @param choices a vector of choices that will remain stationary in the left column.
#' @param box a number between 1 and 11, inclusive, indicating the width of the drop-down box.
#' The default is 6 which corresponds to 50% of the page width.
#' @param arrange either 'random' or 'ordered'; default is random. Set equal to ordered if
#' you want the drop-down list to appear alphabetically.
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = multidrop`.
#'
#' @family Interactive Questions
#' @export
question_multidrop <- function(
    text,
    choices,
    ...,
    box = 6,
    arrange = "random",
    type = "multidrop",
    correct = "Correct!",
    incorrect = "Incorrect",
    try_again = incorrect,
    allow_retry = FALSE,
    random_answer_order = FALSE
) {
  question <-
    ISDStutorials:::multidrop_question(
      text = text,
      choices = choices,
      ...,
      box = box,
      arrange = arrange,
      type = "multidrop",
      correct = correct,
      incorrect = incorrect,
      allow_retry = allow_retry,
      random_answer_order = random_answer_order
    )

  answer_is_fn <- FALSE

  question
}

multidrop_question <- function(
    text,
    choices,
    ...,
    box = box,
    arrange = arrange,
    type = c("multidrop"),
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
  #ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)

  # ensure box is in 1:11
  if (! box %in% c(1:11)) {
    stop("Box must be a number between 1 to 11, inclusive.")
  }
  # ensure box is in 1:11
  if (! arrange %in% c("random", "ordered")) {
    stop("arrange must be either 'random' or 'ordered' ")
  }

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
    box = box,
    arrange = arrange,
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
  #class(ret) <- c(type, "notes_question")
  class(ret) <- c(type, "tutorial_question")
  ret
}


#' @export
#' @seealso question_multidrop
question_ui_initialize.multidrop <- function(question, value, ...) {

  # get number of parts
  num <- length(question$choices)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- as.character( unlist(value) )
  } else {
    ans <- rep(" ", num)
  }

  #shuffle answer options because must be listed in order
  options <- unlist(learnr:::answer_values(question, exclude_answer_fn = TRUE) )
  if(question$arrange == "ordered"){
    labels <- sort(options)
  }else{
    labels <- sample(options, length(options))
  }


  # pretty close to unique id
  rand = paste0(sample.int(100,1), sample.int(100,1) )
  # set input and bucket ids
  input_ids = lapply(seq(1,num), function(x) paste0("select", rand, x) )
  css_ids = lapply(seq(1,num), function(x) paste0("css", rand, x) )

  fluidPage(
    withMathJax(),
    tags$style(
      ".selectbox {
      width: 100%;
    }"),
    div(class = "panel-heading",
        strong(question$question)  ),
    div(id = paste0("bucket", rand),
         lapply(seq(1,num), function(x)
           fixedRow(
             column(
               width = question$box,
               div(
                 class = "panel panel-default",
                 div(
                   class = "selectbox",
                   id = css_ids[x],
                   #style="display:inline-block",
                   tags$select(id = input_ids[x],
                               class = "selectbox",
                               tagList(
                                 lapply(c(" ",labels), function(y)
                                        if(ans[x] == y){
                                          tags$option(y,selected="selected")}
                                        else{tags$option(y)}),
                               ),#end tagList
                               onclick = htmlwidgets::JS(
                     paste0("Shiny.setInputValue('",question$ids$answer,"',
                     [", toString(lapply(input_ids, function(z)
                       paste0("document.getElementById('",z,"').value")
                       ) ),
                       "] )")
                      )#end JS


                   ) #end tag select
                )#end select div
                )#end bigger div
             ), #end column
             column(
               width = 12-question$box,
               #icons(question$choices[x]) )
               p(paste0(question$choices[x]) ) )
           ) #ends fixed row
         ) #ends lapply
    ) #ends bucket div group
  ) # end fluidpage

}


#' @export
#' @seealso question_multidrop
question_is_valid.multidrop <- function(question, value, ...) {

  if (is.null(value)) {
    return(FALSE)
  }else{
    return(length(value) == length(question$choices))
  }

}

#' @export
#' @seealso question_multidrop
question_is_correct.multidrop <- function(question, value, ...) {
  # for each possible answer, check if it matches
  choice_values <- unlist(learnr:::answer_values(question, exclude_answer_fn = TRUE) )

  record_correct <- c()
  for (i in 1:length(question$choices)){
  # for (answer in question$answers) {
    if (identical(choice_values[i], value[i] )) {
    #if (identical(question$answers[[i]], value[i])) {
      # if it matches, return the correct-ness and its message
      record_correct[i] <- TRUE
    } else{
      # no match found. mark as incorrect
      record_correct[i] <- FALSE
    }
  }

  #if everything is correct mark TRUE
  if(all(record_correct)){
    value_is_correct <- TRUE
  }else{
    value_is_correct <- FALSE
  }

  learnr::mark_as(value_is_correct)

}


#' @export
#' @seealso question_multidrop
question_ui_completed.multidrop <- function(question, value, ...) {

  # get number of parts
  num <- length(question$choices)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- as.character( unlist(value) )
  } else {
    ans <- rep(" ", num)
  }

  #shuffle answer options because must be listed in order
  options <- unlist(learnr:::answer_values(question, exclude_answer_fn = TRUE) )
  if(question$arrange == "ordered"){
    labels <- sort(options)
  }else{
    labels <- sample(options, length(options))
  }

  # pretty close to unique id
  rand = paste0(sample.int(100,1), sample.int(100,1) )
  # set input and bucket ids
  input_ids = lapply(seq(1,num), function(x) paste0("select", rand, x) )
  css_ids = lapply(seq(1,num), function(x) paste0("css", rand, x) )

  learnr::disable_all_tags(

  fluidPage(
    withMathJax(),
    tags$style(
      ".selectbox {
      width: 100%;
    }"),
    div(class = "panel-heading",
        strong(question$question)  ),
    div(id = paste0("bucket", rand),
        lapply(seq(1,num), function(x)
          fixedRow(
            column(
              width = question$box,
              div(
                class = "panel panel-default",
                div(
                  class = "selectbox",
                  id = css_ids[x],
                  #style="display:inline-block",
                  tags$select(id = input_ids[x],
                              class = "selectbox",
                              tagList(
                                lapply(c(" ",labels), function(y)
                                  if(ans[x] == y){
                                    tags$option(y,selected="selected")}
                                  else{tags$option(y)}),
                              ),#end tagList
                              onclick = htmlwidgets::JS(
                                paste0("Shiny.setInputValue('",question$ids$answer,"',
                     [", toString(lapply(input_ids, function(z)
                       paste0("document.getElementById('",z,"').value")
                     ) ),
                     "] )")
                              )#end JS


                  ) #end tag select
                )#end select div
              )#end bigger div
            ), #end column
            column(
              width = 12-question$box,
              #icons(question$choices[x]) )
              p(paste0(question$choices[x]) ) )
          ) #ends fixed row
        ) #ends lapply
    ) #ends bucket div group
  ) # end fluidpage

  ) #end disable all tags


}


