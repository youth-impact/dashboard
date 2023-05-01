# TODO

zones_ui = function(id) {
  ns = NS(id)

  ht = 350
  height = glue('{ht}px')

  sidebarLayout(
    sidebarPanel(
      h5('Display Options'),
      uiOutput(ns('ui_input')),
      uiOutput(ns('ui_counts')),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = 'Overview',
          br(),
          uiOutput(ns('overview_banner')),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(
                ns('plot_kpis_younger'), height = glue('{ht * 1.7}px'))
            ),
            column(
              width = 6,
              plotlyOutput(ns('plot_kpis_older'), height = height)
            )
          ),
          br()
        ),
        tabPanel(
          title = 'Outcomes by School',
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

      cols = c('year_term_str', 'region')
      choices = lapply(cols, \(col) sort(unique(zones[[col]])))
      names(choices) = cols

      tagList(
        pickerInput(
          inputId = ns('year_term_str'),
          choices = choices$year_term_str,
          selected = choices$year_term_str,
          multiple = TRUE,
          options = get_picker_options('Years and terms')
        ),
        pickerInput(
          inputId = ns('region'),
          choices = choices$region,
          selected = choices$region,
          multiple = TRUE,
          options = get_picker_options('Regions')
        )
      )
    })

    data_filt = reactive({
      req(data_proc, input$year_term_str, input$region)
      filt = CJ(
        year_term_str = input$year_term_str,
        region = input$region)
      data_filt = get_data_filtered(
        data_proc()['zones_assessments'], filt)[[1L]]
      data_filt[, treatment_id := student_gender]
      data_filt[, treatment_wrap := student_gender]
    })

    output$ui_counts = renderUI({
      req(data_filt)
      counts = data_filt()[timepoint == 'Baseline', .(
        n_students = uniqueN(student_id)), keyby = student_gender]

      n_total = scales::label_comma()(sum(counts$n_students))
      n_total_txt = glue('{n_total} students in total')

      counts[, n_students := scales::label_comma()(n_students)]
      txt = lapply(glue(
        '{counts$n_students} students ({counts$student_gender})'),
        \(x) list(x, br()))
      em(tags$sup('‡'),
         'Based on students consented at baseline.',
         br(), br(), c(unlist(txt, recursive = FALSE), list(n_total_txt)))
    })

    output$overview_banner = renderUI({
      req(data_filt)
      get_overview_banner(data_filt(), 'zones')
    })

    output$plot_kpis_younger = renderPlotly({
      req(data_filt)
      data_now = data_filt()[!is.na(know_hiv_least_10to19)]

      txt = 'Know 10-to-19-year-olds have\nlowest prevalence of HIV'
      annos = list(c(list(x = 0, y = 1.055, text = txt), anno_base))

      fig = get_barplot_summary(
        data_now, col = 'know_hiv_least_10to19', fills = get_fills('ace'),
        by_treatment = TRUE, ncol = 1L, scales = 'free_x') +
        theme(axis.title.y = element_text(margin = margin(r = 8)),
              panel.spacing = unit(0.8, 'lines'))
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = annos, margin = list(t = 80))
    })

    output$plot_kpis_older = renderPlotly({
      req(data_filt)
      data_now = data_filt()[!is.na(know_hiv_riskiest_older)]

      txt = 'Know older partners have\nhighest risk of transmitting HIV'
      annos = list(c(list(x = 0, y = 1.13, text = txt), anno_base))

      fig = get_barplot_summary(
        data_now, col = 'know_hiv_riskiest_older', fills = get_fills('beginner'),
        by_treatment = TRUE, ncol = 1L, scales = 'free') +
        theme(axis.title.y = element_text(margin = margin(r = 5)))
      ggplotly(fig, tooltip = 'text') |>
        layout(annotations = annos, margin = list(t = 80))
    })

    output$ui_table = renderUI({
      ns = session$ns
      fluidRow(
        column(
          width = 12, align = 'right',
          downloadButton(ns('download_table'), style = 'font-size:80%')
        ),
        br(), br()
      )
    })

    metrics_by_school = reactive({
      req(data_filt)

      q_cols = c('know_hiv_least_10to19', 'know_hiv_riskiest_older')
      by_cols = c('school_id', 'school_name', 'region')
      metrics = get_metrics_zones(data_filt(), q_cols, by_cols)

      cols_old = c(
        'school_name', 'school_id', 'region', 'n_terms',
        'n_females', 'n_males',
        'know_hiv_least_10to19_female_diff',
        'know_hiv_least_10to19_male_diff',
        'know_hiv_riskiest_older_female_diff')
      cols_new = c(
        'School Name', 'School ID', 'Region', 'Number of Terms',
        glue('Number of {g}s at Baseline', g = c('Female', 'Male')),
        glue('Increase in {g}s who know 10-to-19-y.o.s have ',
             'lowest prevalence of HIV (%-points)', g = c('female', 'male')),
        paste('Increase in females who know older partners have',
              'highest risk of transmitting HIV (%-points)'))
      setnames(metrics, cols_old, cols_new)
    })

    output$download_table = downloadHandler(
      filename = \() 'zones_outcomes_by_school.xlsx',
      content = function(file) {
        writexl::write_xlsx(metrics_by_school(), file)}
    )

    output$table_kpis = renderDataTable({
      req(data_filt)#, !is.null(input$table_show_timepoints))

      metrics = copy(metrics_by_school())
      cols_num = colnames(metrics)[(length(by_cols) + 1L):(ncol(metrics) - 3L)]

      # if (isFALSE(input$table_show_timepoints)) {
        cols_drop = colnames(metrics)[
          grepl('_(base|end)line$', colnames(metrics))]
        metrics[, (cols_drop) := NULL]
        cols_num = setdiff(cols_num, cols_drop)
      # }

      opts = list(pageLength = 150L, lengthMenu = c(50, 150, 500))
      DT::datatable(metrics, rownames = FALSE, options = opts) |>
        formatStyle(
          columns = glue(
            'Increase in {g}s who know 10-to-19-y.o.s have ',
            'lowest prevalence of HIV (%-points)', g = c('female', 'male')),
          background = styleColorBar(c(0, 100), get_fills('ace')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatStyle(
          columns = paste(
            'Increase in females who know older partners have',
            'highest risk of transmitting HIV (%-points)'),
          background = styleColorBar(c(0, 100), get_fills('beginner')[1L]),
          backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center') |>
        formatRound(columns = cols_num, digits = 1L) |>
        formatStyle(colnames(metrics), lineHeight = '80%')
    })

  })
}
