library(shiny)
library(readxl)
library(tidyverse)
library(ggtips)
library(plotly)

#Base code
planos_rta <- read_excel("planos_rta.xlsx")

planos_dist <- planos_rta %>%
    mutate(FAM_code = as_factor(FAM_code),
           FCT_numero = str_remove(FCT_numero, "---"),
           FCT_numero = as_factor(FCT_numero),
           ELTDEC_numero = as_factor(ELTDEC_numero)) %>% 
    group_by(FAM_code, ELTDEC_numero, FCT_numero) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    mutate(cum = cumsum(n),
           cum_perc = cum / sum(n))
planos_test <- planos_dist %>% 
    select(ELTDEC_numero, FCT_numero, n, FAM_code)
################################################################################
ui <- {fluidPage(
    sliderInput(inputId = "n_funciones", label = "Selecciona el número máximo de funciones",
                min = 1, max = 100, value = 30),
    selectInput("perimetro", label = "Selecciona el perímetro", choices = sort((levels(planos_dist$ELTDEC_numero)))),
    #plotOutput("top_func"),
    #uiOutput("myPlot"),
    #uiOutput("top_func1"),
    #plotlyOutput("distPlot"),
    plotlyOutput("funciones_plotly")
)}

server <- function(input, output) {
    bar_ordered <- reactive({planos_test %>%
        filter(ELTDEC_numero == input$perimetro) %>%
        top_n(input$n_funciones, wt = n) %>%
        arrange(FAM_code, desc(n)) %>%
        mutate(order = row_number())})
    #Funciones con mayor cantidad de planos
    output$top_func <- renderPlot({
        bar_ordered() %>%
            ggplot(aes(x = rev(order), y = n, fill = FAM_code)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_x_continuous(breaks = bar_ordered()$order,
                labels = bar_ordered()$FCT_numero,
                expand = c(0, 0)) +
            facet_wrap(~ FAM_code, scales = "free") +
            theme(legend.position = "none") +
            labs(title = paste("Número de planos validados por función en el perímetro",input$perimetro),
                 subtitle = "Nota: Cada gráfica representa un proyecto",
                 x = "Funciones",
                 y = "Número de planos")
    })
    #Prueba barchart con ggtips
    output$top_func1 <- {renderWithTooltips(
        plot = bar_ordered() %>%
            ggplot(aes(x = rev(order), y = n, fill = FAM_code)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_x_continuous(breaks = bar_ordered()$order,
                               labels = bar_ordered()$FCT_numero,
                               expand = c(0, 0)) +
            facet_wrap(~ FAM_code, scales = "free") +
            theme(legend.position = "none") +
            labs(title = paste("Número de planos validados por función en el perímetro",input$perimetro),
                 subtitle = "Nota: Cada gráfica representa un proyecto",
                 x = "Funciones",
                 y = "Número de planos"),
        varDict = list(FCT_numero = "func", n = "planos")
    )}
    #Prueba reactiva con base ggtips
    output$myPlot <- {renderWithTooltips(
        plot = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point(aes(color = Species)) +coord_flip(),
        varDict = list(Sepal.Width = "Width", Sepal.Length = "Length", Species = "Species")
    )}
    #plotly example
    output$distPlot <- renderPlotly({
        ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, fill = Species)) + 
            geom_bar(stat = "identity") +
            facet_wrap(~Species)
    })
    #plotly tryout
    output$funciones_plotly <- renderPlotly({
        bar_ordered() %>%
            ggplot(aes(x = rev(order), y = n, fill = FAM_code)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_x_continuous(breaks = bar_ordered()$order,
                               labels = as.character(bar_ordered()$FCT_numero),
                               expand = c(0, 0)) +
            facet_wrap(~ FAM_code, scales = "free") +
            theme(legend.position = "none") +
            labs(title = paste("Número de planos validados por función en el perímetro",input$perimetro),
                 subtitle = "Nota: Cada gráfica representa un proyecto",
                 x = "Funciones",
                 y = "Número de planos")
    })    
    #Output general WIP
    output$top_planos <- renderPlot({
        planos_dist %>% 
            group_by(FAM_code, ELTDEC_numero) %>% 
            summarize(total_planos = sum(n)) %>% 
            arrange(ELTDEC_numero)
    })
}
# Run the application
shinyApp(ui = ui, server = server)
