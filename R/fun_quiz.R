#' Tutorial notes format
#'
#' Add interactive notes to a tutorial.
#'
#' @param ... One or more questions or answers
#' @param caption Optional quiz caption (defaults to "Quiz")
#' @family Interactive Questions
#' @name quiz
#' @rdname quiz
#' @export
quiz_notes <- function(..., caption = rlang::missing_arg()) {

  # create table rows from questions
  index <- 1
  questions <- lapply(list(...), function(question) {
    if (!is.null(question$label)) {
      label <- paste(question$label, index, sep="-")
      question$label <- label
      question$ids$answer <- NS(label)("answer")
      question$ids$question <- label
      index <<- index + 1
    }
    question
  })

  caption <-
    if (rlang::is_missing(caption)) {
      learnr:::i18n_span("text.quiz", "Quiz")
    } else if (!is.null(caption)) {
      learnr:::quiz_text(caption)
    }

  ret <- list(caption = caption, questions = questions)
  class(ret) <- "notes_quiz"
  ret
}



#' Knitr quiz print methods
#'
#' \code{knitr::\link[knitr]{knit_print}} methods for \code{\link{question}} and \code{\link{quiz}}
#' @inheritParams knitr::knit_print
#' @export
#' @importFrom knitr knit_print
#' @method knit_print notes_question
#' @rdname knit_print
#'
knit_print.notes_question <- function(x, ...) {
  question <- x
  ui <- question_module_ui_notes(question$ids$question)

  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk(
    'server',
    sprintf(
      'notes_prerendered_chunk(%s, session = session)', #format
      learnr:::dput_to_string(question)
    )
  )

  # regular knit print the UI
  knitr::knit_print(ui)
}

#' @method knit_print notes_quiz
#' @export
#' @rdname knit_print
knit_print.notes_quiz <- function(x, ...) {
  quiz <- x

  caption_tag <- if (!is.null(quiz$caption)) {
    list(knitr::knit_print(
      tags$div(style="display:inline-block", class = "panel-default notes-quiz-title", quiz$caption)
      #panel-heading
    ))
  }
  append(
    caption_tag,
    lapply(quiz$questions, knitr::knit_print)
  )
}


#page_module_ui_notes <- function(id) {
#  bootstrapPage(
#    div(caption_tag
#    ),
#    div(
#    )
#  )
#}



question_module_ui_notes <- function(id) {
  ns <- NS(id)
  #  div(   #removed to take out box around each questions
  #    class = "panel panel-default",
  bootstrapPage(
    #tagList(
    div(
      "data-label" = as.character(id),
      style="display:inline-block", #now questions are side by side
      class = "notes-question panel-default",
      uiOutput(ns("answer_container")),
      learnr:::withLearnrMathJax()
    ),
    div(
      "data-label" = as.character(id),
      style="display:inline-block", #now questions are side by side
      class = "notes-question panel-body",
      uiOutput(ns("message_container")),
      uiOutput(ns("action_button_container")), #add/removes submit button
      learnr:::withLearnrMathJax()
    )
  )
  #)
}

question_module_server_notes <- function(
    input, output, session,
    question
) {

  output$answer_container <- renderUI({ div(class="loading", question$loading) })

  # Setup reactive here that will be updated by the question modules
  question_state <- reactiveVal()

  observeEvent(
    req(session$userData$learnr_state() == "restored"),
    once = TRUE,
    learnr:::question_module_server_impl(input, output, session, question, question_state)
  )

  question_state
}



#button labels change to icon and smaller
notes_button_label <- function(question, label_type = "submit", is_valid = TRUE) {
  label_type <- match.arg(label_type, c("submit", "try_again", "correct", "incorrect"))

  if (label_type %in% c("correct", "incorrect")) {
    # No button when answer is correct or incorrect (wrong without try again)
    return(NULL)
  }

  button_label <- question$button_labels[[label_type]]
  is_valid <- isTRUE(is_valid)

  #default_class <- "btn-primary"
  #warning_class <- "btn-warning"
  default_class <- "btn-primary btn-xs"
  warning_class <- "btn-warning btn-xs"

  action_button_id <- NS(question$ids$question)("action_button")

  if (label_type == "submit") {
    button <- actionButton(
      action_button_id, "", icon = icon("play", lib = "glyphicon"),
      class = default_class
    )
    if (!is_valid) {
      button <- disable_all_tags(button)
    }
    button
  } else if (label_type == "try_again") {
    learnr:::mutate_tags(
      actionButton(
        action_button_id, "", icon = icon("refresh", lib = "glyphicon"),
        class = warning_class
      ),
      paste0("#", action_button_id),
      function(ele) {
        ele$attribs$class <- str_remove(ele$attribs$class, "\\s+btn-default")
        ele
      }
    )
  }
}




mutate_tags.shiny.tag <- function(ele, selector, fn, ...) {
  # make sure it's a selector
  selector <- learnr:::as_selector_list(selector)
  # grab the first element
  cur_selector <- selector[[1]]

  is_match <- TRUE
  if (!cur_selector$match_everything) {
    # match on element
    if (is_match && !is.null(cur_selector$element)) {
      is_match <- ele$name == cur_selector$element
    }
    # match on id
    if (is_match && !is.null(cur_selector$id)) {
      is_match <- (ele$attribs$id %||% "") == cur_selector$id
    }
    # match on class values
    if (is_match && !is.null(cur_selector$classes)) {
      is_match <- all(strsplit(ele$attribs$class %||% "", " ")[[1]] %in% cur_selector$classes)
    }

    # if it is a match, drop a selector
    if (is_match) {
      selector <- selector[-1]
    }
  }

  # if there are children and remaining selectors, recurse through
  if (length(selector) > 0 && length(ele$children) > 0) {
    ele$children <- lapply(ele$children, function(x) {
      learnr:::mutate_tags(x, selector, fn, ...)
    })
  }

  # if it was a match
  if (is_match) {
    if (
      # it is a "leaf" match
      length(selector) == 0 ||
      # or should match everything
      cur_selector$match_everything
    ) {
      # update it
      ele <- fn(ele, ...)
    }
  }

  # return the updated element
  ele
}


notes_messages <- function(question, messages, is_correct, is_done) {

  # Always display the incorrect, correct, or try again messages
  default_message <-
    if (is_correct) {
      question$messages$correct
    } else {
      # not correct
      if (is_done) {
        question$messages$incorrect
      } else {
        question$messages$try_again
      }
    }

  if (!is.null(messages)) {
    if (!is.list(messages)) {
      # turn vectors into lists
      messages <- tagList(messages)
    }
  }

  # display the default messages first
  if (!is.null(default_message)) {
    if (!is.null(messages)) {
      messages <- tagList(default_message, messages)
    } else {
      messages <- default_message
    }
  }

  # get regular message
  if (is.null(messages)) {
    message_alert <- NULL
  } else {
    alert_class <- if (is_correct) "alert-success" else "alert-danger"
    if (length(messages) > 1) {
      # add breaks inbetween similar messages
      break_tag <- list(tags$br(), tags$br())
      all_messages <- replicate(length(messages) * 2 - 1, {break_tag}, simplify = FALSE)
      # store in all _odd_ positions
      all_messages[(seq_along(messages) * 2) - 1] <- messages
      messages <- tagList(all_messages)
    }
    message_alert <- tags$div(
      class = paste0("alert ", alert_class, " btn-xs"),
      messages
    )
  }


  if (is.null(question$messages$message)) {
    always_message_alert <- NULL
  } else {
    always_message_alert <- tags$div(
      class = "alert alert-info btn-xs",
      question$messages$message
    )
  }

  # get post question message only if the question is done
  if (isTRUE(is_done) && !is.null(question$messages$post_message)) {
    post_alert <- tags$div(
      class = "alert alert-info btn-xs",
      question$messages$post_message
    )
  } else {
    post_alert <- NULL
  }

  # set UI message
  if (all(
    is.null(message_alert),
    is.null(always_message_alert),
    is.null(post_alert)
  )) {
    NULL
  } else {
    tags$div(message_alert, always_message_alert, post_alert)
  }
}
