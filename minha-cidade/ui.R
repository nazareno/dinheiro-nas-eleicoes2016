library(shiny)
library(rcdimple)
library(plotly)
library(shinythemes)
library(DT)
library(readr)
library(d3plus)
library(dplyr, warn.conflicts = FALSE)

#################
# Carrega dados # 
#################
cidades_df = read_csv("cidades_e_siglas.csv", 
                      col_types = cols(.default = col_character())) %>% 
    mutate(label = sprintf("%s - %s", `Nome da UE`, UF)) %>% 
    arrange(UF, `Nome da UE`) %>% 
    select(label, `Sigla da UE`)

cidades = split(cidades_df$`Sigla da UE`, cidades_df$`label`)

# Bubbles em Perdizes

###############
# UI          #
###############
shinyUI(
    fluidPage(
        theme = shinytheme("united"),
        # Application title
        titlePanel("De onde vem a verba das campanhas de sua cidade?"),
        
        br(),
        
        mainPanel(
            p("Esse é o primeiro ano em que doações de empresas estão proibidas 
              nas campanhas eleitorais. Isso criou a necessidade de muitos 
              candidatos reinventarem a arrecadação de suas campanhas. De onde 
              está vindo o dinheiro?"),
            p(em("Que candidaturas são construídas sobre que tipo de dinheiro?")),
            br(),
            
            wellPanel(
                selectInput(
                    'cidade',
                    'Escolha uma cidade para analisar',
                    choices = cidades,
                    selected = cidades[runif(1, 0, 5000)],
                    selectize = TRUE
                )
            ),
            hr(),
            br(),
            h3("Verbas arrecadadas por candidato"),
            p("Em termos absolutos, quanto os candidatos arrecadaram de cada maneira? Quem é bancado pelo partido? Pela própria fortuna? Pelos seus eleitores?"),
            fluidRow(dimpleOutput("arrecadado_abs")), 
            p("Em caso de dúvida: na figura acima e nas demais, 500k significa R$500 mil, e 1.1m significa R$1.1 milhão. 
              E se um candidato não aparece, ele não prestou contas da sua arrecadação ao TSE."),
            
            hr(),
            br(),
            h3("Que proporção veio de cada fonte?"),
            p("Que candidato foi mais apoiado por cada tipo de arrecadação?"),
            fluidRow(dimpleOutput("arrecadado_prop")), 
            
            hr(),
            br(),
            h3("Quem são os amigos dos candidatos?"),
            p("Um zoom nas pessoas físicas que doaram dinheiro aos nossos candidatos. (Caso haja. Caso não haja, fica em branco)"),
            fluidRow(column(10, 
                            plotlyOutput("pessoas_fisicas"))), 
            
            hr(),
            br(),
            h3("E quem financia a própria campanha?"),
            p("Quanto os candidatos declararam de dinheiro a partir de recursos próprios?"),
            fluidRow(d3plusOutput("auto_doacoes_bubbles")),
            
            hr(),
            br(),
            h3("Os dados"),
            p("Nos baseamos nos dados dos relatórios financeiros do TSE. 
              O candidatos relataram as despesas nas datas abaixo."),
            fluidRow(dataTableOutput("quando")), 
            
            hr(),
            br(),
            h3("Quem fez"),
            p("Nazareno Andrade, do ", 
              a(href = "http://analytics.ufcg.edu.br", 
                "Laboratório Analytics"), 
              " da Universidade Federal de Campina Grande - PB."), 
            p("Se você quiser dar sugestões, melhorar algo nesse projeto ou começar 
              o seu a partir deste, ", 
              a(href = "https://github.com/nazareno/dinheiro-nas-eleicoes2016", 
                "você é muito bem-vindo no github do projeto."))
        )
    )
)
