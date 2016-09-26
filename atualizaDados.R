#! /usr/bin/Rscript

#' Atualiza os dados que usamos para análises a partir de um arquivo baixado do TSE. 
#' Inicialmente utilizávamos os dados da prestação de contas parcial. Depois percebemos que o 
#' próprio TSE, no DivulgaCandContas utiliza os dados do que eles chamam de relatórios 
#' financeiros, disponíveis em 
#' http://www.tse.jus.br/hotSites/pesquisas-eleitorais/prestacao_contas_anos/2016.html 

source("doacoes.R")
arquivo = "prestacao-20160926/receitas_candidatos_2016_brasil.txt"
receitas = le_receitas(arquivo)
receitas %>% 
    filter(Cargo == "Prefeito") %>%  
    write_csv("dados/receitas_prefeitaveis_parcial_2016.csv")