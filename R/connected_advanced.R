# connected_advanced module sets advanced options for displaying ConnectEd results

connected_advanced_ui = function(id) {
  ns = NS(id)

  tabPanel(
    title = 'Advanced Options',
    sidebarLayout(

      sidebarPanel(
        uiOutput(ns('ui_input')), # output$ui_input
        width = 3),

      mainPanel(
        width = 10)
    )
  )
}

connected_advanced_server = function(id) {
  moduleServer(id, function(input, output, session) {

    counter = reactiveVal(0)
    default = c('baseline', 'endline')

    output$ui_input = renderUI({
      ns = session$ns
      checkboxGroupInput(
        inputId = ns('keep_missing'),
        label = strong('Include students'),
        choices = list(
          'missing at baseline' = 'baseline',
          'missing at endline' = 'endline'),
        selected = default)
    })

    # couldn't get this to work with bindEvent
    observeEvent(input$keep_missing, {counter(counter() + 1)})

    keep_missing = reactive({
      # logic is necessary because checkboxGroupInput returns NULL
      # if the input hasn't initialized or if no boxes are checked
      if (counter() == 0) {
        default
      } else {
        input$keep_missing
      }
    })
  })
}
