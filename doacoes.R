library(dplyr)
library(ggplot2)
library(rcdimple)

le_receitas = function(arquivo){
    require(readr)
    readr::read_csv2(arquivo, 
                                readr::locale(encoding = "Latin1"), 
                                col_names = TRUE, 
                                col_types = NULL) %>% 
    return()
}

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

plota_cidade_ggplot = function(as_receitas, a_cidade, o_cargo = "Prefeito"){
    as_receitas %>% 
        filter(`Nome da UE` == a_cidade, Cargo == o_cargo) %>% 
        group_by(`Nome candidato`, `Tipo receita`) %>% 
        summarise(Recebido = sum(`Valor receita`)) %>% 
        ggplot(aes(x = `Nome candidato`, y = Recebido, fill = `Tipo receita`, group = `Tipo receita`)) + 
        geom_bar(stat = "identity", 
                 position = "dodge") + 
        coord_flip() %>% 
        return()
}

plota_cidade_dimple = function(as_receitas, a_cidade, o_cargo = "Prefeito", desloca_x = 100, desloca_y = 30){
    r = as_receitas %>% 
        filter(`Nome da UE` == a_cidade, Cargo == o_cargo) %>% 
        group_by(`Nome candidato`, `Tipo receita`) %>% 
        summarise(Recebido = sum(`Valor receita`)) %>%
        rename(Candidato = `Nome candidato`, Tipo = `Tipo receita`)
    dimple(
        Candidato ~ Recebido,
        groups = "Tipo",
        data = r,
        type = "bar", 
        bounds = list(
            x = desloca_x,
            y = desloca_y,
            width = 350,
            height = 330
        )
    ) %>%
        xAxis(type = "addMeasureAxis") %>%
        yAxis(type = "addCategoryAxis") %>%
        add_legend(
            x = "70%", width = "30%", y = "10%", height = 100 
        ) %>% 
        return()
}

