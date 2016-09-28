library(shiny)
library(readr)
library(dplyr, warn.conflicts = F)
library(DT)
library(plotly)
library(rcdimple)
library(lubridate)
library(d3plus)

source("doacoes.R")
theme_set(theme_bw())

receitas = readr::read_csv("receitas_prefeitaveis_parcial_2016.csv")

pfs_br = receitas %>% 
    filter(`Tipo receita` == "Verba de pessoas físicas") %>% 
    group_by(`Sigla da UE`, `Nome na urna`, `Nome do doador`) %>% 
    summarise(Recebido = sum(`Valor receita`)) %>% 
    ungroup() %>% 
    rename(Doador = `Nome do doador`) %>% 
    mutate(Quanto = paste0(Doador, " doou R$", Recebido))

recursos_proprios = receitas %>% 
    filter(`Tipo receita` == "Verba própria")

shinyServer(function(input, output, session) {
    cidade_cod = reactive ({
        input$cidade
    })
    
    output$quando = renderDataTable({
        t = receitas %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            mutate(data = dmy_hms(`Data da receita`)) %>% 
            group_by(`Nome na urna`, `Sigla  Partido`) %>% 
            arrange(data) %>% 
            summarise(De = first(data), Ate = last(data)) %>% 
            ungroup()
        t
    }, options = list(paging = F, info = F))
    
    output$arrecadado_abs = renderDimple({
        p = receitas %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            plota_cidade_dimple(desloca_x = 120) 
        p
    })
    
    output$arrecadado_prop = renderDimple({
        p = receitas %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            plota_cidade_dimple(desloca_x = 120) %>% 
            rcdimple::xAxis(type = "addPctAxis") 
        p
    })
    
    output$pessoas_fisicas = renderPlotly({
        dacidade = pfs_br %>% 
            filter(`Sigla da UE` == cidade_cod())
        if (NROW(dacidade) == 0) {
            return(NULL)
        }
        p = dacidade %>%
            ggplot(aes(x = `Nome na urna`, 
                       y = Recebido, 
                       colour = `Nome na urna`, 
                       label = Quanto)) + 
            geom_point(position = position_jitter(width = .2), alpha = 0.5, size = 3) + 
            labs(x = "", y = "Doação", title = "Doações de verba de pessoa física") + 
            theme(legend.position = "none") + 
            scale_y_log10() + 
            coord_flip()
        
        ggplotly(p, tooltip = c("label"))
    })
    
    output$auto_doacoes = renderDataTable({
        t = recursos_proprios %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            mutate(`Nome do doador` = capwords(`Nome do doador`, strict = TRUE)) %>% 
            group_by(`Nome do doador`, `Nome na urna`) %>% 
            summarise(Doado = sum(`Valor receita`)) %>% 
            arrange(-Doado) %>% 
            ungroup()
        t
    }, options = list(paging = F, info = F))
    
    output$auto_doacoes_bubbles = renderD3plus({
        t = recursos_proprios %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            group_by(`Nome do doador`, `Nome na urna`) %>% 
            summarise(Doado = sum(`Valor receita`)) %>% 
            arrange(-Doado) %>% 
            ungroup()
        if (NROW(t) == 0){
            NULL
        } else {
            d3plus("bubbles", 
                   data.frame(name = t$`Nome na urna`, 
                              value = 1, 
                              `Auto-doacao` = t$Doado))
        }
    })
})
