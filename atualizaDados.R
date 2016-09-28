#! /usr/bin/Rscript

#' Atualiza os dados que usamos para análises a partir de um arquivo baixado do TSE. 
#' Inicialmente utilizávamos os dados da prestação de contas parcial. Depois percebemos que o 
#' próprio TSE, no DivulgaCandContas utiliza os dados do que eles chamam de relatórios 
#' financeiros, disponíveis em 
#' http://www.tse.jus.br/hotSites/pesquisas-eleitorais/prestacao_contas_anos/2016.html 
library(dplyr, warn.conflicts = FALSE)
source("doacoes.R")

arquivo = "prestacao-20160926/receitas_candidatos_2016_brasil.txt"
receitas = le_receitas(arquivo)

candidatos = readr::read_csv2("../candidatos-20160926/candidatos_tudo.txt", 
                              readr::locale(encoding = "Latin1"), 
                              col_names = FALSE, 
                              col_types = NULL) %>% 
    filter(X10 == "PREFEITO") %>% 
    select("X14", "X15") %>% 
    rename(`CPF do candidato` = X14, `Nome na urna` = X15) %>% 
    mutate(`Nome na urna` = capwords(`Nome na urna`, strict = TRUE))

reclassifica_receitas = function(tipo, especie){
    return(ifelse(tipo == "Recursos de pessoas físicas", 
                  ifelse(especie == "Estimado", 
                         "Serviços/bens de pessoas físicas", 
                         "Verba de pessoas físicas"), 
                  # elseif:
                  ifelse(tipo == "Recursos de partido político", 
                         ifelse(especie == "Estimado", 
                         "Serviços/bens de partido político", 
                         "Verba de partido político"), 
                  #elif
                  ifelse(tipo == "Recursos próprios", 
                         ifelse(especie == "Estimado", 
                                "Serviços/bens próprios", 
                                "Verba própria"), 
                  ifelse(tipo == "Recursos de outros candidatos", 
                         ifelse(especie == "Estimado", 
                                "Serviços/bens de outros candidatos", 
                                "Verba de outros candidatos"), 
                         tipo)))
            )
    )
}

receitas %>% 
    filter(Cargo == "Prefeito") %>%  
    select(-5:-1, -15, -16, -11, -14, -19, -24:-21) %>% 
    mutate(`Tipo receita original` = `Tipo receita`) %>% 
    mutate(`Tipo receita` = reclassifica_receitas(`Tipo receita`, `Especie recurso`)) %>% 
    left_join(info, by = "CPF do candidato") %>%
    write_csv("dados/receitas_prefeitaveis_parcial_2016.csv")

receitas %>% 
    select(`Nome da UE`, UF, `Sigla da UE`) %>% 
    distinct() %>% 
    mutate(`Nome da UE` = capwords(`Nome da UE`, strict = TRUE)) %>% 
    write_csv("dados/cidades_e_siglas.csv")
