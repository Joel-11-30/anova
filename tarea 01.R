# app.R
paquetes <- c("shiny", "readxl", "dplyr", "ggplot2", "DescTools", "shinythemes", "report")
instalar_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar_faltantes)) install.packages(instalar_faltantes)
lapply(paquetes, library, character.only = TRUE)

ui <- navbarPage(
  title = "Análisis Estadístico Interactivo",
  theme = shinytheme("slate"),
  tags$head(
    tags$style(HTML("
      pre {
        background-color: #2C3E50 !important;
        color: #ECF0F1 !important;
        border: 1px solid #566573;
        border-radius: 5px;
        padding: 10px;
      }
    "))
  ),
  
  tabPanel("Subir Datos",
           sidebarLayout(
             sidebarPanel(
               fileInput("archivo", "Carga tu archivo (.csv o .xlsx):", accept = c(".csv", ".xlsx")),
               uiOutput("seleccion_vars"),
               actionButton("analizar", "Analizar", class = "btn btn-success")
             ),
             mainPanel(
               h4("Vista previa de los datos"),
               tableOutput("vista_datos")
             )
           )
  ),
  
  tabPanel("Estadísticas & Gráficos",
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Estadísticas Descriptivas"),
                      uiOutput("estadisticas_ui")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Visualización Gráfica"),
                      uiOutput("graficos_ui")
                    )
             )
           )
  ),
  
  tabPanel("Prueba Estadística",
           fluidRow(
             column(12,
                    wellPanel(
                      h4("Resultado de la prueba"),
                      verbatimTextOutput("resultado_prueba"),
                      h4("Interpretación"),
                      verbatimTextOutput("texto_interpretacion")
                    )
             )
           )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    if (ext == "csv") {
      read.csv(input$archivo$datapath)
    } else {
      read_excel(input$archivo$datapath)
    }
  })
  
  output$vista_datos <- renderTable({
    head(datos(), 10)
  })
  
  output$seleccion_vars <- renderUI({
    req(datos())
    selectInput("variables", "Selecciona 2 o más variables:",
                choices = names(datos()), multiple = TRUE)
  })
  
  observeEvent(input$analizar, {
    updateTabsetPanel(session, "navbarPage", selected = "Estadísticas & Gráficos")
  })
  
  # ESTADÍSTICAS DESCRIPTIVAS
  output$estadisticas_ui <- renderUI({
    req(input$variables)
    tablas <- lapply(input$variables, function(var) {
      datos_col <- datos()[[var]]
      tipo <- if (is.numeric(datos_col)) "numérica" else "categórica"
      
      tagList(
        tags$h5(paste("Variable:", var, "-", tipo)),
        tableOutput(paste0("tabla_", var))
      )
    })
    do.call(tagList, tablas)
  })
  
  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("tabla_", v)]] <- renderTable({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            data.frame(
              Media = mean(datos_col, na.rm = TRUE),
              Mediana = median(datos_col, na.rm = TRUE),
              Moda = unique(DescTools::Mode(datos_col))[1],
              Mínimo = min(datos_col, na.rm = TRUE),
              Máximo = max(datos_col, na.rm = TRUE),
              Rango = max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE),
              Desv_Estándar = sd(datos_col, na.rm = TRUE),
              Coef_Variación = sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE)
            )
          } else {
            as.data.frame(table(datos_col))
          }
        })
      })
    }
  })
  
  # GRÁFICOS
  output$graficos_ui <- renderUI({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      plotname <- paste0("plot_", var)
      tagList(h5(paste("Gráfico de", var)), plotOutput(plotname))
    })
    do.call(tagList, plots)
  })
  
  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("plot_", v)]] <- renderPlot({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_histogram(bins = 15, fill = "#5DADE2", color = "white") +
              labs(title = paste("Histograma de", v), x = v, y = "Frecuencia") +
              theme_minimal(base_size = 14) +
              theme(
                plot.background = element_rect(fill = "#2C3E50", color = NA),
                panel.background = element_rect(fill = "#2C3E50", color = NA),
                panel.grid = element_line(color = "#566573"),
                text = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                plot.title = element_text(face = "bold")
              )
            } else {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_bar(fill = "#A569BD") +
              labs(title = paste("Gráfico de barras de", v), x = v, y = "Frecuencia") +
                theme_minimal(base_size = 14) +
                theme(
                  plot.background = element_rect(fill = "#2C3E50", color = NA),
                  panel.background = element_rect(fill = "#2C3E50", color = NA),
                  panel.grid = element_line(color = "#566573"),
                  text = element_text(color = "white"),
                  axis.text = element_text(color = "white"),
                  plot.title = element_text(face = "bold")
                )          }
        })
      })
    }
  })
  
  # PRUEBA ESTADÍSTICA
  prueba_resultado <- reactive({
    req(input$variables)
    vars <- input$variables
    df <- datos()
    
    if (length(vars) == 2) {
      v1 <- df[[vars[1]]]
      v2 <- df[[vars[2]]]
      
      if (is.numeric(v1) && is.numeric(v2)) {
        t.test(v1, v2)
      } else if (!is.numeric(v1) && !is.numeric(v2)) {
        chisq.test(table(v1, v2))
      } else {
        NULL
      }
    } else if (length(vars) > 2) {
      num_vars <- vars[sapply(df[, vars], is.numeric)]
      if (length(num_vars) >= 2) {
        formula <- as.formula(paste(num_vars[1], "~", paste(num_vars[-1], collapse = "+")))
        aov(formula, data = df)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  output$resultado_prueba <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("❌ No se pudo aplicar la prueba. Verifica el tipo y número de variables.")
    } else {
      print(res)
    }
  })
  
  output$texto_interpretacion <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("Sin resultados para interpretar.")
    } else {
      # Tu interpretación personalizada
      if (inherits(res, "htest")) {
        p <- res$p.value
        if (p < 0.05) {
          cat("✅ p =", round(p, 4), "- Existe diferencia significativa.\n")
        } else {
          cat("ℹ p =", round(p, 4), "- No hay diferencia significativa.\n")
        }
      } else if (inherits(res, "aov")) {
        p <- summary(res)[[1]][["Pr(>F)"]][1]
        if (p < 0.05) {
          cat("✅ p =", round(p, 4), "- Al menos un grupo difiere significativamente.\n")
        } else {
          cat("ℹ p =", round(p, 4), "- No hay diferencias significativas entre los grupos.\n")
        }
      }
      
      # Interpretación automática con report (adicional)
      cat("\n📋 Interpretación automática:\n")
      cat(as.character(report::report(res)))
    }
  })
}

shinyApp(ui,server)
