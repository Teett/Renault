library(shiny)
library(readxl)
library(tidyverse)
library(plotly)

#Base code
planos_rta <- read_excel("planos_rta.xlsx")

planos_dist <- planos_rta %>%
    mutate(FAM_code = as_factor(FAM_code),
           FCT_numero = str_remove(FCT_numero, "---"),
           FCT_numero = as_factor(FCT_numero),
           ELTDEC_numero = as_factor(ELTDEC_numero)) %>% 
    group_by(FAM_code, ELTDEC_numero, FCT_numero, FCT_designation) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    mutate(cum = cumsum(n),
           cum_perc = cum / sum(n))
planos_test <- planos_dist %>% 
    select(ELTDEC_numero, FCT_designation, FCT_numero, n, FAM_code)
################################################################################
ui <- {fluidPage(
    sliderInput(inputId = "n_funciones", label = "Selecciona el número máximo de funciones",
                min = 1, max = 100, value = 30),
    selectInput("perimetro", label = "Selecciona el perímetro", choices = sort((levels(planos_dist$ELTDEC_numero)))),
    plotlyOutput("funciones_plotly")
)}

server <- function(input, output) {
    #Definiendo el objeto bar_ordered para poder ordenar las barras
    bar_ordered <- reactive({planos_test %>%
        filter(ELTDEC_numero == input$perimetro) %>%
        top_n(input$n_funciones, wt = n) %>%
        arrange(FAM_code, desc(n)) %>%
        mutate(order = row_number())})

    #plotly planos
    output$funciones_plotly <- renderPlotly({
        p <- bar_ordered() %>%
            ggplot(aes(x = rev(order), y = n, fill = FAM_code, label = FCT_designation)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_x_continuous(breaks = bar_ordered()$order,
                               labels = as.character(bar_ordered()$FCT_numero),
                               expand = c(0, 0)) +
            facet_wrap(~ FAM_code, scales = "free") +
            theme(legend.position = "none") +
            labs(title = paste("Número de planos por función en el perímetro",input$perimetro),
                 subtitle = "Nota: Cada gráfica representa un proyecto",
                 x = "Funciones",
                 y = "Número de planos")
        ggplotly(p, tooltip = c("label", "fill", "y"))
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
