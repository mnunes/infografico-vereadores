library(tidyverse)
library(readxl)
library(janitor)
library(modeest)
library(lubridate)
library(waffle)

vereadores <- read_excel(path = "dados/vereadores.xlsx")
vereadores <- clean_names(vereadores)

vereadores <- vereadores %>%
  mutate(idade = interval(dmy(data_de_nascimento), today())/years(1))

vereadores %>%
  select(genero, 
         estado_civil, 
         grau_de_instrucao, 
         idade, 
         cor_raca, 
         cidade, 
         uf, 
         recursos_recebidos, 
         patrimonio) %>%
  summarise(genero = mlv(genero), 
            estado_civil = mlv(estado_civil), 
            grau_de_instrucao = mlv(grau_de_instrucao), 
            idade = mean(idade), 
            cor_raca = mlv(cor_raca), 
            cidade = mlv(cidade), 
            uf = mlv(uf), 
            recursos_recebidos = mean(recursos_recebidos), 
            patrimonio = mean(patrimonio)) %>%
  t()

# genero

vereadores %>%
  group_by(genero) %>%
  count() %>%
  arrange(desc(n))

# raca/cor

vereadores %>%
  group_by(cor_raca) %>%
  count() %>%
  arrange(desc(n))

# partido

vereadores %>%
  group_by(partido_atual) %>%
  count() %>%
  arrange(desc(n))

# mudanca partido

vereadores %>%
  mutate(mudanca = ifelse(partido_atual == partido_2016, "Não", "Sim")) %>%
  group_by(mudanca) %>%
  count() %>%
  arrange(desc(n))



