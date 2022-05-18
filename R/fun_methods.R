#' Custom question methods
#'
#' @description
#' There are four methods used to define a custom question.  Each S3 method
#' should correspond to the `type = TYPE` supplied to the question.
#'
#' * `question_ui_initialize.TYPE(question, value, ...)`
#'
#'     -  Determines how the question is initially displayed to the users. This should return a shiny UI object that can be displayed using [shiny::renderUI]. For example, in the case of `question_ui_initialize.radio`, it returns a [shiny::radioButtons] object. This method will be re-executed if the question is attempted again.
#'
#' * `question_ui_completed.TYPE(question, ...)`
#'
#'     - Determines how the question is displayed after a submission.  Just like `question_ui_initialize`, this method should return an shiny UI object that can be displayed using [shiny::renderUI].
#'
#' * `question_is_valid.TYPE(question, value, ...)`
#'
#'      - This method should return a boolean that determines if the input answer is valid.  Depending on the value, this function enables and disables the submission button.
#'
#' * `question_is_correct.TYPE(question, value, ...)`
#'
#'     - This function should return the output of [correct], [incorrect], or [mark_as]. Each method allows for custom messages in addition to the determination of an answer being correct.  See [correct], [incorrect], or [mark_as] for more details.
#'
#' @param question [question] object used
#' @param value user input value
#' @param ... future parameter expansion and custom arguments to be used in dispatched s3 methods.
#'
#' @seealso For more information and question type extension examples, please view the `question_type` tutorial: `learnr::run_tutorial("question_type", "learnr")`.
#'
#' @export
#' @rdname question_methods
question_ui_initialize.learnr_blank <- function(question, value, ...) {
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
  split <- unlist(str_split(question$question, pattern = "___") )
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
#' @rdname question_methods
question_is_valid.learnr_blank <- function(question, value, ...) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (isTRUE(question$options$trim)) {
    return(nchar(str_trim(value)) > 0)
  } else{
    return(nchar(value) > 0)
  }
}

#' @export
#' @rdname question_methods
question_is_correct.learnr_blank <- function(question, value, ...) {
  #how is value being taken in
  if (nchar(value) == 0) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      showNotification("Please enter some text before submitting", type = "error")
    }
    #shiny::validate("Please enter some text")
    value
  }

  if (isTRUE(question$options$trim)) {
    value <- str_trim(value)
  }

  compare_answer <- function(answer) {
    answer_value <- answer$value
    if (isTRUE(question$options$trim)) {
      answer_value <- str_trim(answer_value)
    }
    if (isTRUE(all.equal(answer_value, value))) {
      learnr:::mark_as(answer$correct, answer$message)
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

  mark_as(FALSE, NULL)
}

#' @export
#' @rdname question_methods
question_ui_completed.learnr_blank <- function(question, value, ...) {
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


  split <- unlist(str_split(question$question, pattern = "___") )

  bootstrapPage(
    #tagList(
    div(style="display:inline-block",split[1]),
    div(style="display:inline-block",
        finalize_question(
          textInputFn(paste0(question$ids$answer),
                      label = NULL,
                      placeholder = question$options$placeholder,
                      value = value, width = "100%")
        )
    ),
    div(style="display:inline-block",split[2])
  )

}



#' @export
#' @rdname question_methods
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
#' @rdname question_methods
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
#' @rdname question_methods
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
