library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

archivo <- "Rendimientos_decada.xlsx"
datos <- read_excel(archivo, na = c("", "NA", "SD"))

nombres_esperados <- c(
  "Cultivo", "Campaña", "Provincia", "Departamento",
  "Sup. Sembrada", "Sup. Cosechada", "Producción", "Rendimiento"
)

if (!identical(names(datos), nombres_esperados)) {
  stop(
    "Los nombres de las variables no coinciden con los esperados. Se leyeron: ",
    paste(names(datos), collapse = ", ")
  )
}

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Seleccionar cultivo:",
        choices = c("Todos", sort(unique(datos$Cultivo))),
        selected = "Todos"
      )
    ),
    mainPanel(
      plotOutput("grafico", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  datos_grafico <- reactive({
    datos_filtrados <- datos

    if (input$cultivo != "Todos") {
      datos_filtrados <- datos_filtrados |>
        filter(Cultivo == input$cultivo)
    }

    datos_filtrados |>
      group_by(Campaña) |>
      summarise(
        across(
          c(`Sup. Sembrada`, `Sup. Cosechada`),
          ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) |>
      arrange(Campaña) |>
      pivot_longer(
        cols = c(`Sup. Sembrada`, `Sup. Cosechada`),
        names_to = "Superficie",
        values_to = "Hectareas"
      )
  })

  output$grafico <- renderPlot({
    datos_plot <- datos_grafico()

    validate(
      need(
        any(!is.na(datos_plot$Hectareas)),
        "No hay datos de superficie disponibles para la selección."
      )
    )

    ggplot(
      datos_plot,
      aes(
        x = Campaña,
        y = Hectareas,
        color = Superficie,
        group = Superficie
      )
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      labs(
        x = "Campaña",
        y = "Superficie (hectáreas)",
        color = "Variable"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
shiny::runApp("C:/Users/Usuario/Desktop")
