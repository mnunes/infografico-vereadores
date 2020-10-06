## Guia para uso e instalacao dos pacotes: ### Daqui: https://www.listendata.com/2019/06/create-infographics-with-r.html

#pacotes
library(readxl)
library(tidyverse)
library(janitor)
library(modeest)
library(lubridate)
library(waffle)
library(extrafont)
library(echarts4r)
library(echarts4r.assets)
library(ggpol)

##### dados

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

genero <- vereadores %>%
  group_by(genero) %>%
  count() %>%
  arrange(desc(n))

ggplot(genero)+
  geom_parliament(aes(seats = n, fill = genero))+
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = NULL) +
  scale_fill_manual(labels = c("Masculino", "Feminino"), values = c("navyblue", "pink"))

# raca/cor

raca <- vereadores %>%
  group_by(cor_raca) %>%
  count() %>%
  arrange(desc(n))

waffle(
  c("Branca" = raca$n[1], "Parda"  = raca$n[2], "Preta" = raca$n[3]), rows = 5,
  use_glyph = "user",
  title = 'Câmara de Vereadores por raça', legend_pos="bottom"
)

ggplot(raca)+
  geom_parliament(aes(seats = n, fill = cor_raca))+
  coord_fixed() + 
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = NULL) +
  scale_fill_brewer(palette = "Dark2", labels = c("Branco", "Pardo", "Preto"))

# partido

vereadores %>%
  group_by(partido_atual) %>%
  count() %>%
  arrange(desc(n))


partido <- vereadores %>%
  group_by(partido_atual) %>%
  count() %>%
  arrange(desc(n)) %>% 
  filter(n >1) %>% 
  droplevels()


partido %>% 
  e_charts(partido_atual) %>% 
  e_pictorial(n, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(60, 60)) %>% 
  e_theme("westeros") %>%
  e_title(text = "Distribuição por partidos", subtext = "Partidos com pelo menos 2 parlamentares. Partidos com 1 parlamentar são \nAvante, MDB, PCdoB, PL, PP, PROS, PSB, PSD, PSL, PV e Republicanos", textStyle =list(fontSize= 24, fontFamily= 'Arial', fontWeight ='bold'), subtextStyle =list(fontSize= 12, fontFamily= 'Arial', fontWeight ='lighter')) %>%
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE), show = FALSE) %>%
  e_y_axis(splitLine=list(show = FALSE), show = TRUE, inverse = TRUE, axisLine = list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 16, fontWeight ='bold', position = "right", offset=c(10, 0))


###
  
  ### Daqui: https://www.listendata.com/2019/06/create-infographics-with-r.html
