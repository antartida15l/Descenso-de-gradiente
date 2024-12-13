# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Descenso de Gradiente Estocástico"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      h3("Configuración"),
      numericInput("learning_rate", "Tasa de aprendizaje (\u03b7):", value = 0.1, step = 0.01),
      numericInput("iterations", "Número de iteraciones:", value = 5, min = 1),
      actionButton("run", "Ejecutar", class = "btn btn-primary"),
      hr(),
      helpText("Ingrese los parámetros y haga clic en 'Ejecutar' para observar los resultados.")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados", 
                 h4("Tabla de Resultados"),
                 tableOutput("resultsTable")),
        tabPanel("Visualización", 
                 h4("Gráfico de Relación"),
                 plotOutput("resultsPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive values to store the results
  results <- reactiveValues(data = NULL)
  
  # Observe the run button
  observeEvent(input$run, {
    # Datos iniciales
    data <- data.frame(
      Estudiante = 1:5,
      x = c(2, 3, 1.5, 4, 3.5),  # Horas de estudio
      y = c(12, 15, 10, 18, 16)   # Rendimiento académico
    )
    
    # Inicialización de parámetros
    w <- 0
    eta <- input$learning_rate
    iterations <- input$iterations
    
    # Almacenar resultados
    results_list <- list()
    
    for (i in 1:iterations) {
      for (j in 1:nrow(data)) {
        x_i <- data$x[j]
        y_i <- data$y[j]
        gradient <- -2 * (y_i - w * x_i) * x_i
        w <- w - eta * gradient
        results_list <- append(results_list, list(data.frame(Iteración = i, Estudiante = j, w = w)))
      }
    }
    
    # Combinar resultados en un solo data frame
    results$data <- do.call(rbind, results_list)
  })
  
  # Render table
  output$resultsTable <- renderTable({
    req(results$data)
    results$data
  })
  
  # Render plot
  output$resultsPlot <- renderPlot({
    req(results$data)
    data <- data.frame(
      x = c(2, 3, 1.5, 4, 3.5),
      y = c(12, 15, 10, 18, 16)
    )
    
    plot(data$x, data$y, pch = 16, col = "blue", xlab = "Horas de estudio", ylab = "Rendimiento académico", 
         main = "Relación entre tiempo de estudio y rendimiento", cex = 1.2)
    abline(a = 0, b = results$data$w[nrow(results$data)], col = "red", lwd = 2)
    legend("topleft", legend = c("Datos", "Línea de ajuste"), 
           col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), bty = "n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
