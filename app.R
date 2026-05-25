# ============================================================
# app.R
# DSS — Decision Support System
# Previsão & Otimização
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Garante diretório de trabalho correto
if (!file.exists("R/data_helpers.R")) {
  script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
  setwd(script_dir)
}

source("R/data_helpers.R")
source("R/forecast_wrappers.R")
source("R/optimization_wrappers.R")
source("R/legacy_optimization.R")

# ============================================================
# UI
# ============================================================
ui <- navbarPage(
  title = "DSS — Previsão & Otimização",

  header = tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Arial, sans-serif; background: #f5f7fa; }
      .navbar { background-color: #2c3e50 !important; }
      .navbar-brand, .navbar-nav > li > a { color: #ecf0f1 !important; font-weight:500; }
      .navbar-nav > li.active > a { background-color: #1a252f !important; }
      .well { background: #ffffff; border: 1px solid #dce1e7; border-radius: 6px; }
      .btn-primary { background-color: #2980b9; border-color: #2471a3; }
      .btn-primary:hover { background-color: #1a5276; }
      .btn-success { background-color: #27ae60; border-color: #229954; }
      .btn-warning { background-color: #e67e22; border-color: #ca6f1e; color:#fff; }
      .section-title {
        font-size: 15px; font-weight: 600; color: #2c3e50;
        border-bottom: 2px solid #2980b9; padding-bottom: 5px; margin-bottom: 12px;
        margin-top: 6px;
      }
      .metric-box {
        background: #eaf4fb; border-radius: 6px; padding: 10px 14px;
        margin-bottom: 8px; border-left: 4px solid #2980b9;
      }
      .metric-label { font-size: 12px; color: #7f8c8d; }
      .metric-value { font-size: 20px; font-weight: bold; color: #2c3e50; }
      .msg-info    { background: #d6eaf8; border-left: 4px solid #2980b9;
                     padding: 10px; border-radius: 4px; color: #1a5276; margin-bottom:10px; }
      .msg-warning { background: #fef9e7; border-left: 4px solid #f39c12;
                     padding: 10px; border-radius: 4px; color: #7d6608; margin-bottom:10px; }
      .msg-error   { background: #fdedec; border-left: 4px solid #e74c3c;
                     padding: 10px; border-radius: 4px; color: #922b21; margin-bottom:10px; }
      .msg-ok      { background: #d5f5e3; border-left: 4px solid #27ae60;
                     padding: 10px; border-radius: 4px; color: #1e8449; margin-bottom:10px; }
    "))
  ),

  # ===========================================================
  # ABA 1 — PREVISÃO
  # ===========================================================
  tabPanel(
    "📈 Previsão",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "section-title", "Configuração"),

        selectInput("fc_model", "Modelo",
          choices = c(
            "Seasonal Naive" = "snaive",
            "ARIMA"          = "arima",
            "ETS"            = "ets",
            "Random Forest"  = "rf"
          ), selected = "snaive"),

        selectInput("fc_city", "Cidade",
          choices = c(
            "Baltimore"    = "baltimore",
            "Lancaster"    = "lancaster",
            "Philadelphia" = "philadelphia",
            "Richmond"     = "richmond"
          ), selected = "baltimore"),

        selectInput("fc_week", "Semana",
          choices = c(
            "Última semana"    = "ultima",
            "Penúltima semana" = "penultima"
          )),

        checkboxInput("fc_use_index", "Escolher por índice", value = FALSE),
        conditionalPanel(
          condition = "input.fc_use_index == true",
          numericInput("fc_week_idx", "Índice da Semana",
                       value = 82, min = 2, max = 200, step = 1)
        ),

        br(),
        actionButton("fc_run", "▶  Gerar Previsão",
                     class = "btn-primary",
                     style = "width:100%; font-size:14px; padding:10px;"),
        br(), br(),
        div(class = "msg-info",
            tags$b("Nota:"), br(),
            "ARIMA, ETS e RF podem demorar alguns segundos.")
      ),

      mainPanel(
        width = 9,
        uiOutput("fc_status_ui"),
        conditionalPanel(
          condition = "output.fc_has_results",
          fluidRow(column(12,
            div(class = "section-title", "Métricas de Desempenho"),
            uiOutput("fc_metrics_ui")
          )),
          hr(),
          fluidRow(column(12,
            div(class = "section-title", "Valores Reais vs Previstos"),
            plotOutput("fc_plot", height = "350px")
          )),
          hr(),
          fluidRow(column(12,
            div(class = "section-title", "Tabela Detalhada"),
            DT::dataTableOutput("fc_table")
          ))
        )
      )
    )
  ),

  # ===========================================================
  # ABA 2 — OTIMIZAÇÃO O1 / O3
  # ===========================================================
  tabPanel(
    "⚙️ Otimização (O1 / O2 / O3)",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "section-title", "Configuração"),

        selectInput("opt_obj", "Objetivo",
          choices = c(
            "O1 — Maximizar Lucro"         = "O1",
            "O2 — Maximizar Lucro / RH"    = "O2",
            "O3 — Multi-objetivo (Pareto)" = "O3"
          ), selected = "O1"),

        checkboxInput("opt_use_legacy", "Usar scripts originais do relatório", value = FALSE),

        selectInput("opt_city", "Cidade",
          choices = c(
            "Todas as cidades" = "todas",
            "Baltimore"        = "baltimore",
            "Lancaster"        = "lancaster",
            "Philadelphia"     = "philadelphia",
            "Richmond"         = "richmond"
          ), selected = "todas"),

        selectInput("opt_week", "Semana",
          choices = c(
            "Última semana"    = "ultima",
            "Penúltima semana" = "penultima"
          )),

        checkboxInput("opt_use_index", "Escolher por índice", value = FALSE),
        conditionalPanel(
          condition = "input.opt_use_index == true",
          numericInput("opt_week_idx", "Índice", value = 82, min = 2, max = 200, step = 1)
        ),

        hr(),
        div(class = "section-title", "Parâmetros PSO (O1)"),
        sliderInput("opt_particles", "Nº Partículas", min = 10, max = 60, value = 30, step = 5),
        sliderInput("opt_iter",      "Nº Iterações",  min = 20, max = 200, value = 80, step = 10),

        br(),
        actionButton("opt_run", "▶  Executar Otimização",
                     class = "btn-success",
                     style = "width:100%; font-size:14px; padding:10px;"),
        br(), br(),
        div(class = "msg-warning",
            tags$b("Atenção:"), br(),
            "O3 requer o pacote 'mco'.", br(),
          "O1 e O2 podem demorar 10–60 seg.")
      ),

      mainPanel(
        width = 9,
        uiOutput("opt_status_ui"),
        conditionalPanel(
          condition = "output.opt_has_results",
          fluidRow(column(12,
            div(class = "section-title", "Resumo da Solução"),
            tableOutput("opt_summary_table")
          )),
          hr(),
          conditionalPanel(
            condition = "output.opt_show_convergence",
            fluidRow(column(12,
              div(class = "section-title", "Convergência PSO"),
              plotOutput("opt_conv_plot", height = "280px")
            )),
            hr()
          ),
          conditionalPanel(
            condition = "output.opt_show_pareto",
            fluidRow(column(12,
              plotOutput("opt_pareto_plot", height = "350px")
            )),
            hr(),
            fluidRow(column(12,
              div(class = "section-title", "Soluções Pareto"),
              DT::dataTableOutput("opt_pareto_table")
            )),
            hr()
          ),
          fluidRow(column(12,
            div(class = "section-title", "Plano Semanal Detalhado"),
            DT::dataTableOutput("opt_plan_table")
          ))
        )
      )
    )
  ),

  # ===========================================================
  # ABA 4 — SOBRE
  # ===========================================================
  tabPanel(
    "ℹ️ Sobre",
    fluidPage(
      br(),
      wellPanel(
        h3("DSS — Decision Support System"),
        p("Aplicação Shiny para previsão e otimização de recursos humanos e vendas."),
        hr(),
        h4("Estado dos Módulos"),
        tags$table(
          class = "table table-bordered table-hover",
          style = "max-width: 750px;",
          tags$thead(tags$tr(
            tags$th("Módulo"), tags$th("Estado"), tags$th("Notas")
          )),
          tags$tbody(
            tags$tr(tags$td("Seasonal Naive"),   tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("Pacote forecast")),
            tags$tr(tags$td("ARIMA"),             tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("auto.arima, pacote forecast")),
            tags$tr(tags$td("ETS"),               tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("Pacote forecast")),
            tags$tr(tags$td("Random Forest"),     tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("Pacotes randomForest + zoo")),
            tags$tr(tags$td("O1 — Maximizar Lucro"), tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("PSO interno")),
            tags$tr(tags$td("O2 — Maximizar Lucro / RH"), tags$td(tags$span(style="color:green;font-weight:bold;","✔ Implementado")), tags$td("PSO interno")),
            tags$tr(tags$td("O3 — Multi-objetivo"), tags$td(tags$span(style="color:orange;font-weight:bold;","⚠ Requer 'mco'")), tags$td("NSGA-II via pacote mco"))
          )
        ),
        hr(),
        h4("Pacotes necessários"),
        tags$code("install.packages(c('shiny','ggplot2','dplyr','tidyr','DT','forecast','randomForest','zoo','mco'))"),
        hr(),
        h4("Parâmetros do Negócio (ajustáveis em optimization_wrappers.R)"),
        tags$ul(
          tags$li("Preço por unidade: 25"),
          tags$li("Custo Junior/dia: 80 | Custo X/dia: 100 | Custo PR/unidade: 2"),
          tags$li("Capacidade J: 6 unidades | Capacidade X: 7 unidades"),
          tags$li("Boost PR: +10% por unidade de promoção")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # -----------------------------------------------------------
  # Atualizar máximo de semanas dinamicamente
  # -----------------------------------------------------------
  observe({
    tryCatch({
      df <- load_city_data(input$fc_city)
      n  <- floor(nrow(df) / 7)
      updateNumericInput(session, "fc_week_idx", max = n, value = min(n, 82))
    }, error = function(e) {})
  })

  observe({
    tryCatch({
      df <- load_city_data("baltimore")
      n  <- floor(nrow(df) / 7)
      updateNumericInput(session, "opt_week_idx", max = n, value = min(n, 82))
    }, error = function(e) {})
  })

  # ===========================================================
  # ABA PREVISÃO
  # ===========================================================
  fc_result <- reactiveVal(NULL)

  observeEvent(input$fc_run, {
    fc_result(NULL)
    week_sel <- if (input$fc_use_index) as.character(input$fc_week_idx) else input$fc_week
    withProgress(message = "A calcular previsão...", value = 0, {
      setProgress(0.3)
      res <- run_forecast(input$fc_model, input$fc_city, week_sel)
      setProgress(1.0)
    })
    fc_result(res)
  })

  output$fc_has_results <- reactive({
    r <- fc_result(); !is.null(r) && isTRUE(r$status == "ok")
  })
  outputOptions(output, "fc_has_results", suspendWhenHidden = FALSE)

  output$fc_status_ui <- renderUI({
    r <- fc_result()
    if (is.null(r)) return(div(class="msg-info","Configure os parâmetros e clique em 'Gerar Previsão'."))
    if (r$status == "not_implemented") return(div(class="msg-warning", tags$b("⚠ Não implementado: "), r$message))
    if (r$status == "error")           return(div(class="msg-error",   tags$b("✖ Erro: "), r$message))
    div(class="msg-ok",
        tags$b("✔ Previsão gerada — "),
        paste0("Cidade: ", r$city, " | Semana: ", r$week_index, " | Modelo: ", toupper(r$model)))
  })

  output$fc_metrics_ui <- renderUI({
    r <- fc_result(); if (is.null(r) || r$status != "ok") return(NULL)
    m <- r$metrics
    fluidRow(
      column(3, div(class="metric-box", div(class="metric-label","RMSE"), div(class="metric-value", m$RMSE))),
      column(3, div(class="metric-box", div(class="metric-label","MAE"),  div(class="metric-value", m$MAE))),
      column(3, div(class="metric-box", div(class="metric-label","NMAE"), div(class="metric-value", m$NMAE))),
      column(3, div(class="metric-box", div(class="metric-label","R²"),   div(class="metric-value", m$R2)))
    )
  })

  output$fc_plot <- renderPlot({
    r <- fc_result(); if (is.null(r) || r$status != "ok") return(NULL)
    df_plot <- data.frame(Dia = r$dates, Real = r$real, Previsto = r$predicted)
    df_long <- tidyr::pivot_longer(df_plot, c("Real","Previsto"), names_to="Tipo", values_to="Clientes")
    ggplot(df_long, aes(x=Dia, y=Clientes, color=Tipo, group=Tipo)) +
      geom_line(linewidth=1.2) + geom_point(size=3) +
      scale_color_manual(values=c("Real"="#2c3e50","Previsto"="#2980b9")) +
      labs(title=paste("Previsão —", toupper(r$model), "—",
                        tools::toTitleCase(r$city), "| Semana", r$week_index),
           x="Data", y="Nº Clientes", color=NULL) +
      theme_minimal(base_size=13) +
      theme(legend.position="top", plot.title=element_text(face="bold"))
  })

  output$fc_table <- DT::renderDataTable({
    r <- fc_result(); if (is.null(r) || r$status != "ok") return(NULL)
    DT::datatable(r$table, options=list(pageLength=7, dom="t"),
                  rownames=FALSE, class="table-striped table-hover") |>
      DT::formatStyle("Erro",
                       color=DT::styleInterval(c(-0.001,0.001), c("red","black","green")))
  })

  # ===========================================================
  # ABA OTIMIZAÇÃO O1 / O3
  # ===========================================================
  opt_result <- reactiveVal(NULL)

  observeEvent(input$opt_run, {
    opt_result(NULL)
    week_sel <- if (input$opt_use_index) as.character(input$opt_week_idx) else input$opt_week
    withProgress(message="A executar otimização...", value=0, {
      setProgress(0.2)
      # Ensure popsize is valid for NSGA-II (must be multiple of 4)
      popsize_input <- input$opt_particles
      popsize_adj <- ifelse(popsize_input %% 4 == 0, popsize_input, popsize_input + (4 - (popsize_input %% 4)))

      res <- run_optimization(
        objective   = input$opt_obj,
        city        = input$opt_city,
        week_choice = week_sel,
        N_particles = input$opt_particles,
        N_iter      = input$opt_iter,
        use_legacy  = input$opt_use_legacy,
        omega       = 0.7,
        popsize     = popsize_adj,
        generations = input$opt_iter
      )
      setProgress(1.0)
    })
    opt_result(res)
  })

  output$opt_has_results <- reactive({
    r <- opt_result(); !is.null(r) && isTRUE(r$status == "ok")
  })
  outputOptions(output, "opt_has_results", suspendWhenHidden = FALSE)

  output$opt_show_convergence <- reactive({
    r <- opt_result(); !is.null(r) && r$status=="ok" && !is.null(r$history)
  })
  outputOptions(output, "opt_show_convergence", suspendWhenHidden = FALSE)

  output$opt_show_pareto <- reactive({
    r <- opt_result(); !is.null(r) && r$status=="ok" && !is.null(r$pareto_df)
  })
  outputOptions(output, "opt_show_pareto", suspendWhenHidden = FALSE)

  output$opt_status_ui <- renderUI({
    r <- opt_result()
    if (is.null(r))                    return(div(class="msg-info","Configure os parâmetros e clique em 'Executar Otimização'."))
    if (r$status=="not_implemented")   return(div(class="msg-warning", tags$b("⚠ Não implementado: "), r$message))
    if (r$status=="error")             return(div(class="msg-error",   tags$b("✖ Erro: "), r$message))
    div(class="msg-ok", tags$b(paste0("✔ Otimização ", r$objective, " concluída.")))
  })

  output$opt_summary_table <- renderTable({
    r <- opt_result(); if (is.null(r) || r$status!="ok") return(NULL)
    if (!is.null(r$recommended_summary)) r$recommended_summary
    else if (!is.null(r$summary)) r$summary
    else NULL
  }, striped=TRUE, hover=TRUE, bordered=TRUE)

  output$opt_conv_plot <- renderPlot({
    r <- opt_result(); if (is.null(r) || r$status!="ok" || is.null(r$history)) return(NULL)
    df_conv <- data.frame(Iteracao=seq_along(r$history), Score=r$history)
    ggplot(df_conv, aes(x=Iteracao, y=Score)) +
      geom_line(color="#2980b9", linewidth=1.1, na.rm=TRUE) +
      labs(title=paste("Convergência PSO —", r$objective), x="Iteração", y="Melhor Score") +
      theme_minimal(base_size=12) + theme(plot.title=element_text(face="bold"))
  })

  output$opt_pareto_plot <- renderPlot({
    r <- opt_result(); if (is.null(r) || r$status!="ok" || is.null(r$pareto_df)) return(NULL)
    df_p <- r$pareto_df; rec <- r$recommended_id
    # Normalize column names to expected ones
    if (!"Total_HR" %in% names(df_p) && "RH_Total" %in% names(df_p)) df_p$Total_HR <- df_p$RH_Total
    if (!"solution_id" %in% names(df_p) && "Solution_ID" %in% names(df_p)) df_p$solution_id <- df_p$Solution_ID
    if (!"Lucro" %in% names(df_p) && "Lucro" %in% names(df_p)) df_p$Lucro <- df_p$Lucro

    if (!"Total_HR" %in% names(df_p) || !"Lucro" %in% names(df_p)) return(NULL)

    df_p$Tipo <- "Pareto"
    if (!is.null(rec) && rec <= nrow(df_p)) df_p$Tipo[rec] <- "Recomendada"

    ggplot(df_p, aes(x=Total_HR, y=Lucro, color=Tipo, size=Tipo)) +
      geom_line(aes(group=1), color="#95a5a6", linewidth=0.8) +
      geom_point() +
      scale_color_manual(values=c("Pareto"="#2980b9","Recomendada"="#e74c3c")) +
      scale_size_manual( values=c("Pareto"=3,        "Recomendada"=5)) +
      labs(title="Fronteira de Pareto — O3 (NSGA-II)",
           subtitle="Vermelho = solução recomendada (equilíbrio lucro/RH)",
           x="RH Total", y="Lucro", color=NULL, size=NULL) +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"), legend.position="top")
  })

  output$opt_pareto_table <- DT::renderDataTable({
    r <- opt_result(); if (is.null(r) || r$status!="ok" || is.null(r$pareto_df)) return(NULL)
    df_p <- r$pareto_df; rec <- r$recommended_id
    # Normalize columns if needed
    if (!"Total_HR" %in% names(df_p) && "RH_Total" %in% names(df_p)) df_p$Total_HR <- df_p$RH_Total
    if (!"solution_id" %in% names(df_p) && "Solution_ID" %in% names(df_p)) df_p$solution_id <- df_p$Solution_ID

    tbl <- DT::datatable(df_p, options=list(pageLength=10, scrollX=TRUE),
                  rownames=FALSE, class="table-striped table-hover")
    tbl <- tbl |> DT::formatRound("Lucro", digits=2)
    if ("solution_id" %in% names(df_p)) {
      sel_id <- ifelse(!is.null(rec) && rec <= nrow(df_p), df_p$solution_id[rec], -1)
      tbl <- tbl |> DT::formatStyle("solution_id", target="row",
                       backgroundColor=DT::styleEqual(sel_id, "#fdebd0"))
    }
    tbl
  })

  output$opt_plan_table <- DT::renderDataTable({
    r <- opt_result(); if (is.null(r) || r$status!="ok") return(NULL)
    plan <- if (!is.null(r$recommended_plan)) r$recommended_plan
            else if (!is.null(r$plan)) r$plan
            else return(NULL)
    plan$Vendas <- round(plan$Vendas,2)
    plan$Custos <- round(plan$Custos,2)
    plan$Lucro  <- round(plan$Lucro, 2)
    DT::datatable(plan, options=list(pageLength=14, scrollX=TRUE),
                  rownames=FALSE, class="table-striped table-hover") |>
      DT::formatRound(c("Vendas","Custos","Lucro"), digits=2) |>
      DT::formatStyle("Lucro", color=DT::styleInterval(0, c("red","green")))
  })
}

# ============================================================
shinyApp(ui = ui, server = server)
