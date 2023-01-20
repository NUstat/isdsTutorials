#' @title Lock tutorial exam
#'
#' @description
#' Lock all the exam answers for submission.
#' @param id ID matching ui with server
#' @param label Label to appear on the button
#'
#' @export
lock_exam_button2 <- function(id = "lock", label = "lock answers") {

    ns <- NS(id)

    fluidPage(
      tagList(
        actionButton( ns("lockbutton"), label = label)
      )
    )


}


# Define the server logic for a module to lock questions
#' @title Tutorial lock server
#' @param id ID matching ui with server
#' @export
lock_server2 <- function(id = "lock") {
  moduleServer(
    id,
    function(input, output, session) {
      # check if lock was pressed in previous session.
      observeEvent(
        req(session$userData$learnr_state() == "restored"),{

          ns <- getDefaultReactiveDomain()$ns
          store_lock <- reactiveVal(NULL, label = "lock_pressed")
          print("working")

          check_lock_answer <- learnr:::retrieve_question_submission_answer(session, "lock_pressed")

          store_lock(check_lock_answer)

          print(store_lock())

          if(!is.null(store_lock())){
            #force lock click on reload if already been pressed.
            print("lock previously clicked")
            #lock_exam <<- TRUE

          }

        }) # close observe event

      # check if lock was pressed now.
      observeEvent(input$lockbutton, {

        lock_exam <<- TRUE
        learnr::initialize_tutorial()
        # cache_q <- as.list(learnr:::get_tutorial_cache(type = c("question")))
        #
        # map(cache_q, function(x){
        #   label <- as.character(x$ids$question)
        #   #print(label)
        #   inner_lock_server(label)
        # })

        learnr:::event_trigger(
          session = session,
          event   = "question_submission",
          data    = list(
            label    = "lock_pressed",
            question = NULL,
            answer   = TRUE,
            correct  = 1
          )
        )
        learnr:::set_tutorial_state("lock_pressed",
                                    data = list(
          label    = "lock_pressed",
          question = NULL,
          answer   = TRUE,
          correct  = 1
        ))

      })

      }) # close module server

} # close lock server

# Define the server logic for a module to lock questions
# @title Tutorial inner lock server
# @param id ID matching ui with server
# @export
# inner_lock_server <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#
#       observe({
#
#       ns <- NS(id)
#
#       cache1 <- as.list(learnr:::get_tutorial_cache(type = c("question")))
#       question <- cache1[[id]]
#
#       question$allow_retry <- FALSE
#       question$messages$incorrect <- "Locked"
#       question$messages$try_again <- "Locked"
#
#       #print(question)
#
#       #state1 <- learnr::get_tutorial_state(label = "Q-1")
#
#       #print(state1$answer)
#       #answer <- state1$answer
#
#       ns <- NS(question$ids$question)
#
#       learnr:::store_question_cache(question)
#
#       #call module server to communicate with ui
#       #question state with set tutorial state works but try another option
#       question_state <- reactiveVal()
#
#       question_state <-
#       callModule(
#           learnr:::question_module_server,
#           id = question$ids$question,
#           question = question
#       )
#
#       # observe({
#       #   #not working!
#       #   print("fire submit!")
#       #   click(id = NS(question$ids$question)("action_button"))
#       #   click(id = ns("action_button"))
#       # })
#
#       #disable_all_tags(NS(question$ids$question)("action_button"))
#
#
#       #The below two lines work but freezes all buttons and everything?
#       #req(question_state())
#       learnr:::set_tutorial_state(question$label, question_state(), session = session)
#
#       #add on?
#       #tried to simplify to event trigger but not enough to freeze/rerun the UI.
#
#
#       #alt attempt below
#
#
#       })
#
#     }
#   )
# }


