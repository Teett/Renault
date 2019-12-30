library(data.table)
library(tidyverse)

raw_planos <- fread("consolidado.csv") %>% as_tibble()

pt1_planos <- raw_planos %>%
  #Simplificación evitando estados
  filter(Maturity == "Validated" | Maturity == "Refused") %>%
  #Cómputo por grupo de referencia
  group_by(`Name / Id`) %>% 
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
