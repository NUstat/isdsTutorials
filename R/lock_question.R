#' @title Lock tutorial exam
#'
#' @description
#' Lock all the exam answers for submission.
#' @param id ID matching ui with server
#' @param label Label to appear on the button
#'
#' @export
lock_q_button <- function(id = "Q-1", label = "lock Q1") {

    ns <- NS(id)
    fluidPage(
      useShinyjs(),
      tagList(
        actionButton( ns("lockbutton"), label = label)
      )
    )

}


# Define the server logic for a module to lock questions
#' @title Tutorial lock server
#' @param id ID matching ui with server
#' @export
lock_q_server <- function(id = "Q-1") {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$lockbutton, {

        print("lockpressed")

        cache_q <- as.list(learnr:::get_tutorial_cache(type = c("all")))

        cache_q <- cache_q[["Q-1"]]

        map(cache_q, function(x){
          label <- as.character(x$ids$question)
          #print(label)
          inner_q_lock_server(label)
        })

      }) # close observe event
      }) # close module server

} # close lock server


# Define the server logic for a module to lock questions
#' @title Tutorial inner lock server
#' @param id ID matching ui with server
#' @export
inner_q_lock_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      observe({

        cache1 <- as.list(learnr:::get_tutorial_cache(type = c("all")))
        #print(cache1)
        question <- cache1[[id]]
        question$allow_retry <- FALSE
        #question$messages$incorrect <- "Locked"
        #question$messages$try_again <- "Locked"
        print(question)

        #state1 <- learnr::get_tutorial_state(label = "Q-1")
        #print(state1)
        #answer <- as.character( unlist(state1$answer) )

        #print(answer)

        ns <- NS(question$ids$question)

        learnr:::store_question_cache(question)

        # observe( {
        #   # submit question to server
        #   learnr:::event_trigger(
        #     session = session,
        #     event   = "question_submission",
        #     data    = list(
        #       label    = "Q-1",
        #       question = question,
        #       answer   = answer,
        #       correct  = FALSE
        #     )
        #   )
        #
        # })

        question_state <-
          callModule(
            learnr:::question_module_server,
            id = question$ids$question,
            question = question
          )

        # data = list(
        #   label    = "Q-1",
        #   question = question,
        #   answer   = answer,
        #   correct  = FALSE
        # )

        #learnr:::set_tutorial_state("Q-1", data)
        learnr:::set_tutorial_state(question$label, question_state(), session = session)


        # callModule(
        #   learnr:::question_module_server_impl,
        #   id = question$ids$question,
        #   question = question
        # )

        # observe({
        #   #   #not working!
        #   #   print("fire submit!")
        #     click(id = NS(question$ids$question)("action_button"))
        # })
        # disable_all_tags(NS(question$ids$question)("action_button"))

      })

    }
  )
}

