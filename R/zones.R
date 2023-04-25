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
          uiOutput(ns('overview_banner')),
          plotlyOutput(ns('plot_kpis'), height = height)
        ),
        tabPanel(
          title = 'Outcomes as Table',
          br(),
          uiOutput(ns('ui_table')),
          div(dataTableOutput(ns('table_kpis')), style = 'font-size:80%')
        )
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

    output$overview_banner = renderUI({
      req(data_filt)

      metrics = copy(data_filt())[student_gender == 'Female', .(
        pct_younger = 100 * sum(know_hiv_least_10to19, na.rm = TRUE) /
          sum(!is.na(know_hiv_least_10to19)),
        pct_older = 100 * sum(know_hiv_riskiest_older, na.rm = TRUE) /
          sum(!is.na(know_hiv_riskiest_older))),
        keyby = timepoint][, lapply(.SD, diff), .SDcols = !'timepoint']
      metrics = metrics[, lapply(.SD, scales::label_number(accuracy = 1))]

      sty_n = 'font-size:26px;'
      sty_unit = 'font-size:20px;'
      sp = HTML('&nbsp;')

      txt_younger = paste(
        'Increase in female students who know 10-to-19-year-olds',
        'have lowest prevalence of HIV')
      txt_older = paste(
        'Increase in female students who know older partners have',
        'highest risk of transmitting HIV')

      wellPanel(
        fluidRow(
          column(
            width = 6, style = 'background-color:#a6cee3;',
            p(strong(metrics$pct_younger, style = sty_n),
              a(' %-points', br(), txt_younger, style = sty_unit))
          ),
          column(
            width = 6, style = 'background-color:#fb9a99;',
            p(strong(metrics$pct_older, style = sty_n),
              a(' %-points', br(), txt_older, style = sty_unit))
          )
        )
      )
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
        col = 'know_hiv_riskiest_older', fills = get_fills('beginner'),
        by_treatment = TRUE)
      fig_older = ggplotly(fig, tooltip = 'text')

      txt_younger = 'Know 10-to-19-year-olds have\nlowest prevalence of HIV'
      txt_older = 'Know older partners\nhave highest risk\nof transmitting HIV'
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

    output$ui_table = renderUI({
      ns = session$ns
      fluidRow(
        column(
          width = 6,
          checkboxInput(
            inputId = ns('table_show_timepoints'),
            label = 'Show outcomes for baseline and endline',
            value = FALSE,
            width = '100%')),
        column(
          width = 6, align = 'right',
          downloadButton(ns('download_table'), style = 'font-size:80%')
        ),
        br(), br()
      )
    })

    output$table_kpis = renderDataTable({
      req(data_filt, !is.null(input$table_show_timepoints))

      # TODO: options to aggregate by year, year_term_str, school, facilitator
      q_cols = c('know_hiv_least_10to19', 'know_hiv_riskiest_older')
      by_cols = c('year', 'student_gender', 'facilitator_id_impl')

      metrics = get_metrics_zones(data_filt(), q_cols, by_cols)
      cols_num = colnames(metrics)[(length(by_cols) + 1L):ncol(metrics)]

      if (isFALSE(input$table_show_timepoints)) {
        cols_drop = colnames(metrics)[
          grepl('_(base|end)line$', colnames(metrics))]
        metrics[, (cols_drop) := NULL]
        cols_num = setdiff(cols_num, cols_drop)
      }

      opts = list(pageLength = 500L, lengthMenu = c(50, 150, 500))
      DT::datatable(metrics, rownames = FALSE, options = opts) |>
        formatStyle(
          columns = 'know_hiv_least_10to19_diff',
          background = styleColorBar(c(0, 100), get_fills('ace')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(
          columns = 'know_hiv_riskiest_older_diff',
          background = styleColorBar(c(0, 100), get_fills('beginner')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatRound(columns = cols_num, digits = 1L) |>
        formatStyle(colnames(metrics), lineHeight = '80%')
    })

  })
}
