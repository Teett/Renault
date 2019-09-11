#Abandoned the shiny app for Tableau RIP 11/9/2019
#Further code will be made on riesgo_planos_data_wrangle.Rmd

library(shiny)
library(tidyverse)
library(plotly)
library(data.table)

#Lectura de datos
{
planos_rta <- fread("planos_rta.csv") %>% as_tibble() %>% 
    mutate(FAM_code = as_factor(FAM_code),
           FCT_numero = str_remove(FCT_numero, "---"),
           FCT_numero = as_factor(FCT_numero),
           ELTDEC_numero = as_factor(ELTDEC_numero))
raw_planos <- fread("consolidado.csv") %>% as_tibble()
}
#Análisis de número de planos
{
planos_dist <- planos_rta %>%
    group_by(FAM_code, ELTDEC_numero, FCT_numero, FCT_designation) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    dplyr::ungroup() %>% 
    mutate(cum = cumsum(n),
           cum_perc = cum / sum(n))

planos_test <- planos_dist %>% 
    dplyr::select(ELTDEC_numero, FCT_designation, FCT_numero, n, FAM_code)
}
#Análisis de riesgo
{pt1_planos <- raw_planos %>%
    #Simplificación evitando estados
    filter(Maturity == "Validated" | Maturity == "Refused") %>%
    #Cómputo por grupo de referencia
    group_by(`Name / Id`, Designation) %>% 
    #Contar frecuencias en Maturity por grupos
    count(Maturity) %>% 
    #Contar totales agrupados
    mutate(total = sum(n),
           riesgo = case_when(
               Maturity == "Validated" & n == total ~ 0, #El riesgo es 0 si siempre ha sido validado
               Maturity == "Refused" ~ (n/total)) #El riesgo es n / total si se ha rechazado
           #Evitar cálculos individuales
    ) %>% dplyr::ungroup() %>%
    #Seleccionando sólo 1 fila por referencia - la correcta
    filter(!is.na(riesgo)) %>% 
    #match names con planos_rta
    dplyr::rename("DOCTECH_numero" = `Name / Id`) %>%
    #unión con planos_rta
    left_join(planos_rta, by = "DOCTECH_numero")
}
################################################################################
ui <- {fluidPage(
    sliderInput(inputId = "n_funciones", label = "Selecciona el número máximo de funciones",
                min = 1, max = 100, value = 30),
    selectInput("perimetro", label = "Selecciona el perímetro", choices = sort((levels(planos_dist$ELTDEC_numero)))),
    sliderInput(inputId = "n_planos", label = "Selecciona cuántos planos deseas ver",
                min = 1, max = 50, value = 20),
    plotlyOutput("funciones_plotly"),
    plotlyOutput("riesgo_planos")
)}

#output del server
server <- function(input, output) {
    #Plots del número de planos
    {
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
    }
    #Plots del riesgo
    {
        risk_order <- reactive({pt1_planos %>%
                filter(ELTDEC_numero == input$perimetro) %>%
                top_n(input$n_planos, wt = riesgo) %>%
                arrange(FAM_code, desc(riesgo)) %>%
                mutate(order = row_number())})
        
        output$riesgo_planos <- renderPlotly({
            p <- risk_order() %>%
                ggplot(aes(x = rev(order), y = riesgo, fill = FAM_code, label = Designation)) +
                geom_col() +
                coord_flip() +
                scale_x_continuous(breaks = risk_order()$order,
                                   labels = as.character(risk_order()$DOCTECH_numero),
                                   expand = c(0, 0)) +
                facet_wrap(~ FAM_code, scales = "free") +
                theme(legend.position = "none") +
                labs(title = paste("Planos más riesgosos del perímetro",input$perimetro),
                     subtitle = "Nota: 0% -> Nunca ha sido rechazado",
                     x = "Planos",
                     y = "Nivel de riesgo")
            ggplotly(p, tooltip = c("label", "y"))
        })
        
    }
}
# Run the application
shinyApp(ui = ui, server = server)
