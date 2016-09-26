library(shiny)
library(readr)
library(dplyr, warn.conflicts = F)
library(DT)
library(plotly)
library(rcdimple)

source("doacoes.R")
theme_set(theme_bw())

receitas = readr::read_csv("receitas_prefeitaveis_parcial_2016.csv")

shinyServer(function(input, output, session) {
    cidade_cod = reactive ({
        input$cidade
    })
    
    cidade_nome = reactive ({
        da_cidade = receitas %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            slice(1)
        da_cidade$`Nome da UE`[1]
    }) 
    
    output$arrecadado_abs = renderDimple({
        p = receitas %>% 
            filter(`Sigla da UE` == cidade_cod()) %>% 
            plota_cidade_dimple(cidade_nome(), desloca_x = 220) 
        p
    })
})
