library(shiny)
library(readxl)
library(tidyverse)

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
# Define UI for application that draws a histogram
ui <- fluidPage(
    sliderInput(inputId = "n_funciones", label = "Selecciona el número máximo de funciones",
                min = 1, max = 100, value = 30),
    selectInput("perimetro", label = "Selecciona el perímetro", choices = sort((levels(planos_dist$ELTDEC_numero)))),
    plotOutput("top_func")
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    #Gráfica de funciones con mayor cantidad de planos
    output$top_func <- renderPlot({
        bar_ordered <- planos_test %>%
            filter(ELTDEC_numero == input$perimetro) %>%
            top_n(input$n_funciones, wt = n) %>%
            arrange(FAM_code, desc(n)) %>%
            mutate(order = row_number())
        
        bar_ordered %>%
            ggplot(aes(
                x = rev(order),
                y = n,
                fill = FAM_code
            )) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_x_continuous(
                breaks = bar_ordered$order,
                labels = bar_ordered$FCT_numero,
                expand = c(0, 0)
            ) +
            facet_wrap(~ FAM_code, scales = "free") +
            theme(legend.position = "none") +
            labs(title = paste("Número de planos validados por función en el perímetro",input$perimetro),
                 subtitle = "Nota: Cada gráfica representa un proyecto",
                 x = "Funciones",
                 y = "Número de planos")
        
    })
    output$top_planos <- renderPlot({
        planos_dist %>% 
            group_by(FAM_code, ELTDEC_numero) %>% 
            summarize(total_planos = sum(n)) %>% 
            arrange(ELTDEC_numero)
    })
}
# Run the application
shinyApp(ui = ui, server = server)
