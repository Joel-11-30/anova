paquetes <- c("shiny", "readxl", "dplyr", "ggplot2", "DescTools", "shinythemes", "report")
instalar_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar_faltantes)) install.packages(instalar_faltantes)
lapply(paquetes, library, character.only = TRUE)

ui <- navbarPage(
  title = "Análisis Estadístico Interactivo",
  theme = shinytheme("slate"),
  id = "navbarPage",
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
                      h4("Opciones de Prueba"),
                      selectInput("tipo_prueba", "Selecciona la prueba estadística:",
                                  choices = c("t de Student", "ANOVA", "Wilcoxon", 
                                              "Shapiro-Wilk", "Kolmogorov-Smirnov", "Jarque-Bera",
                                              "Chi-cuadrado", "McNemar", "Correlación", "Límite Central"),
                                  selected = "t de Student"),
                      uiOutput("opciones_extra"),
                      conditionalPanel(
                        condition = "input.tipo_prueba != 'Límite Central'",
                        tagList(
                          h4("Resultado de la prueba"),
                          verbatimTextOutput("resultado_prueba"),
                          h4("Interpretación"),
                          verbatimTextOutput("texto_interpretacion")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.tipo_prueba == 'Límite Central'",
                        tagList(
                          h4("Configuración del Teorema del Límite Central"),
                          selectInput("var_tlc", "Selecciona una variable numérica:", choices = NULL),
                          numericInput("tamano_muestra", "Tamaño de cada muestra (n):", value = 30, min = 5),
                          numericInput("numero_muestras", "Número de muestras a tomar:", value = 1000, min = 100),
                          actionButton("simular_tlc", "Simular", class = "btn btn-primary"),
                          h4("Distribución de las medias muestrales"),
                          plotOutput("grafico_tlc")
                        )
                      )
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
    selectInput("variables", "Selecciona 1 o más variables:",
                choices = names(datos()), multiple = TRUE)
  })
  
  observeEvent(input$analizar, {
    updateTabsetPanel(session, "navbarPage", selected = "Estadísticas & Gráficos")
  })
  
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
              )
          }
        })
      })
    }
  })
  
  output$opciones_extra <- renderUI({
    req(input$tipo_prueba)
    if (input$tipo_prueba == "Límite Central") return(NULL)
    switch(input$tipo_prueba,
           "t de Student" = tagList(
             radioButtons("var_ttest", "Tipo de prueba t:",
                          choices = c("Independientes" = "ind", "Pareadas" = "par"),
                          selected = "ind")
           ),
           "Wilcoxon" = tagList(
             checkboxInput("wilcoxon_pareado", "¿Pareadas?", value = FALSE),
             checkboxInput("wilcoxon_alternativa", "¿Alternativa diferente de dos colas?", value = FALSE)
           ),
           "Correlación" = tagList(
             selectInput("tipo_cor", "Tipo de correlación:",
                         choices = c("Pearson", "Spearman", "Kendall"), selected = "Pearson")
           ),
           NULL
    )
  })
  
  prueba_resultado <- reactive({
    req(input$variables, input$tipo_prueba)
    if (input$tipo_prueba == "Límite Central") return(NULL)
    
    vars <- input$variables
    df <- datos()
    
    if (length(vars) < 2 && !(input$tipo_prueba %in% c("Shapiro-Wilk", "Jarque-Bera"))) {
      return(NULL)
    }
    
    v1 <- df[[vars[1]]]
    v2 <- if (length(vars) > 1) df[[vars[2]]] else NULL
    
    tryCatch({
      switch(input$tipo_prueba,
             "t de Student" = {
               if (is.numeric(v1) && is.numeric(v2)) {
                 paired <- input$var_ttest == "par"
                 t.test(v1, v2, paired = paired)
               } else NULL
             },
             "ANOVA" = {
               num_vars <- vars[sapply(df[, vars], is.numeric)]
               if (length(num_vars) >= 2) {
                 formula <- as.formula(paste(num_vars[1], "~", paste(num_vars[-1], collapse = "+")))
                 aov(formula, data = df)
               } else NULL
             },
             "Wilcoxon" = {
               if (is.numeric(v1) && is.numeric(v2)) {
                 wilcox.test(v1, v2, paired = input$wilcoxon_pareado,
                             alternative = if (input$wilcoxon_alternativa) "greater" else "two.sided")
               } else NULL
             },
             "Correlación" = {
               if (is.numeric(v1) && is.numeric(v2)) {
                 cor.test(v1, v2, method = tolower(input$tipo_cor))
               } else NULL
             },
             "Chi-cuadrado" = {
               if (!is.numeric(v1) && !is.numeric(v2)) chisq.test(table(v1, v2)) else NULL
             },
             "McNemar" = {
               if (!is.numeric(v1) && !is.numeric(v2)) mcnemar.test(table(v1, v2)) else NULL
             },
             "Kolmogorov-Smirnov" = {
               if (is.numeric(v1) && is.numeric(v2)) ks.test(v1, v2) else NULL
             },
             "Shapiro-Wilk" = {
               if (is.numeric(v1)) shapiro.test(v1) else NULL
             },
             "Jarque-Bera" = {
               if (is.numeric(v1)) DescTools::JBTest(v1) else NULL
             },
             NULL
      )
    }, error = function(e) {
      NULL
    })
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
    metodo <- input$tipo_prueba
    
    if (is.null(res)) {
      cat("❌ No se pudo realizar la prueba.\n")
    } else {
      cat("📌 Método utilizado: ", metodo, "\n")
      
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
      
      cat("\n📋 Interpretación automática:\n")
      cat(as.character(report::report(res)))
    }
  })
  
  observe({
    req(datos())
    updateSelectInput(session, "var_tlc", choices = names(datos())[sapply(datos(), is.numeric)])
  })
  
  observeEvent(input$simular_tlc, {
    req(input$var_tlc, input$tamano_muestra, input$numero_muestras)
    df <- datos()
    variable <- df[[input$var_tlc]]
    medias <- replicate(input$numero_muestras, {
      mean(sample(variable, size = input$tamano_muestra, replace = TRUE))
    })
    output$grafico_tlc <- renderPlot({
      ggplot(data.frame(media = medias), aes(x = media)) +
        geom_histogram(bins = 30, fill = "#F39C12", color = "black") +
        labs(title = "Distribución de las Medias Muestrales", x = "Media", y = "Frecuencia") +
        theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "#2C3E50", color = NA),
          panel.background = element_rect(fill = "#2C3E50", color = NA),
          panel.grid = element_line(color = "#566573"),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.title = element_text(face = "bold")
        )
    })
  })
}

shinyApp(ui, server)

  
