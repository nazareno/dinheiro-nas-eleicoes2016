library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(rcdimple)
library(shinythemes)

cidades = list()
cidades_df = readr::read_csv("receitas_prefeitaveis_parcial_2016.csv") %>% 
    select(`Nome da UE`, UF, `Sigla da UE`) %>% 
    distinct()
for (i in 1:NROW(cidades_df)) {
    sigla = cidades_df[i,]$`Sigla da UE`
    nome = cidades_df[i,]$`Nome da UE`
    uf = cidades_df[i,]$UF
    cidades[[sprintf("%s - %s", nome, uf)]] <- sigla
}


shinyUI(fluidPage(
    theme = shinytheme("journal"),
    # Application title
    titlePanel("De onde vem a verba das campanhas de sua cidade"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                'cidade',
                'Escolha uma cidade',
                choices = cidades,
                selected = cidades[runif(1, 0, 5000)],
                selectize = TRUE
            )
        ),
    
    # Show a plot of the generated distribution
    mainPanel(
        dimpleOutput("arrecadado_abs")
    )
  )
))
