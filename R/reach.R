# TODO

reach_ui = function(id) {
  ns = NS(id)
  ht = 300

  sidebarLayout(
    sidebarPanel(
      h5('Display Options'),
      uiOutput(ns('ui_input')),
      width = 3
    ),
    mainPanel(
      br(),
      div(dataTableOutput(ns('table_counts')), style = 'font-size:80%')
    )
  )
}

reach_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      students = data_proc()$reach_students

      cols = c('program', 'delivery_model', 'region', 'student_gender')
      choices = lapply(cols, \(col) sort(unique(students[[col]])))
      names(choices) = cols

      tagList(
        pickerInput(
          inputId = ns('program'),
          choices = choices$program,
          selected = choices$program,
          multiple = TRUE,
          options = get_picker_options('Programs')
        ),
        pickerInput(
          inputId = ns('delivery_model'),
          choices = choices$delivery_model,
          selected = choices$delivery_model,
          multiple = TRUE,
          options = get_picker_options('Delivery models')
        ),
        pickerInput(
          inputId = ns('region'),
          choices = choices$region,
          selected = choices$region,
          multiple = TRUE,
          options = get_picker_options('Regions')
        ),
        pickerInput(
          inputId = ns('student_gender'),
          choices = choices$student_gender,
          selected = choices$student_gender,
          multiple = TRUE,
          options = get_picker_options('Student genders')
        ),
        checkboxInput(
          inputId = ns('by_year'),
          label = 'Aggregate by year',
          value = FALSE
        ),
        em(tags$sup('‡'), 'All results based on consented students.')
      )
    })

    students_filt = reactive({
      req(data_proc, input$program, input$delivery_model,
          input$region, input$student_gender)
      filt = CJ(
        program = input$program,
        delivery_model = input$delivery_model,
        region = input$region,
        student_gender = input$student_gender)
      get_data_filtered(data_proc()['reach_students'], filt)[[1L]]
    })

    output$table_counts = renderDataTable({
      req(students_filt, !is.null(input$by_year))
      by_col = if (input$by_year) 'year' else 'year_term_str'
      by_col_new = if (input$by_year) 'Year' else 'Year Term'

      # TODO: filtering will have to come later when calculating cumulative

      counts = students_filt()[, .(
        n_students = .N,
        n_facilitators = uniqueN(.SD, by = c('program', 'facilitator_id_impl')),
        n_schools = uniqueN(.SD, by = c('program', 'school_id', 'school_name'))),
        keyby = by_col]
      setorderv(counts, by_col, -1L)

      old = c(by_col, 'n_students', 'n_facilitators', 'n_schools')
      n_cols = c('Students', 'Facilitators', 'Schools')
      new = c(by_col_new, n_cols)
      setnames(counts, old, new)

      opts = list(pageLength = 25L)
      DT::datatable(counts, rownames = FALSE, options = opts) |>
        formatRound(n_cols, digits = 0L) |>
        formatStyle(colnames(counts), lineHeight = '80%')
    })

  })
}
