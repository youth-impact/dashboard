# TODO

zones_ui = function(id) {
  ns = NS(id)

  ht = 350
  height = glue('{ht}px')

  sidebarLayout(
    sidebarPanel(
      h5('Display Options'),
      uiOutput(ns('ui_input')),
      # uiOutput(ns('ui_counts')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Overview',
          br(),
          # uiOutput(ns('overview_banner')),
          plotlyOutput(ns('plot_kpis'), height = height)
        ),
      ),
      width = 9
    )
  )
}

zones_server = function(id, data_proc) {
  moduleServer(id, function(input, output, session) {

    output$ui_input = renderUI({
      req(data_proc)
      ns = session$ns
      zones = data_proc()$zones_assessments

      cols = c('year_term_str')
      choices = lapply(cols, \(col) sort(unique(zones[[col]])))
      names(choices) = cols

      tagList(
        pickerInput(
          inputId = ns('year_term_str'),
          choices = choices$year_term_str,
          selected = choices$year_term_str,
          multiple = TRUE,
          options = get_picker_options('Years and terms')
        )
      )
    })

    data_filt = reactive({
      req(data_proc, input$year_term_str)
      filt = CJ(year_term_str = input$year_term_str)
      get_data_filtered(data_proc()['zones_assessments'], filt)[[1L]]
    })

    output$plot_kpis = renderPlotly({
      req(data_filt)
      data_now = copy(data_filt())
      data_now[, treatment_id := student_gender]
      data_now[, treatment_wrap := student_gender]

      yaxis = list(title = get_y_title(), titlefont = list(size = 20))

      fig = get_barplot_summary(
        data_now[!is.na(know_hiv_least_10to19)],
        col = 'know_hiv_least_10to19', fills = get_fills('ace'),
        by_treatment = TRUE)
      fig_younger = ggplotly(fig, tooltip = 'text')

      fig = get_barplot_summary(
        data_now[!is.na(know_hiv_riskiest_older)],
        col = 'know_hiv_riskiest_older', fills = get_fills('ace'),
        by_treatment = TRUE)
      fig_older = ggplotly(fig, tooltip = 'text')

      txt_younger = 'Know that 10-to-19-year-olds\nhave lowest prevalence of HIV'
      txt_older = 'Know that older partners\nhave highest risk of\ntransmitting HIV'
      annos = list(
        list(x = 0, y = 1.13, text = txt_younger),
        list(x = 0.69, y = 1.13, text = txt_older))
      annos = lapply(annos, \(z) c(z, anno_base))

      subplot(
        fig_younger, fig_older, nrows = 1L, widths = c(0.65, 0.35),
        margin = 0.04, titleY = TRUE) |>
        layout(annotations = annos, margin = list(t = 100), yaxis = yaxis)
    }) |>
      bindCache(input$year_term_str)

  })
}
