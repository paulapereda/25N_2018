library(tidyverse)
library(janitor)
library(readxl)
library(here)

#Denunciantes

df_denunciantes <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'denunciantes') %>% 
  clean_names() %>% 
  filter(genero != "Total")

ggplot(df_denunciantes, aes(reorder(genero, frecuencia), porcentaje, label = porcentaje)) +
  geom_col() +
  theme_minimal() +
  xlab('Género de las personas víctimas de acoso callejero') +
  geom_text(position = position_dodge(1.4),
            vjust = -0.5, 
            size = 3,
            color = "gray25") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')
  
#Perpetradores

df_perpetradores <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'perpetradores') %>% 
  clean_names() %>% 
  filter(genero != "Total")

ggplot(df_perpetradores, aes(reorder(genero, frecuencia), porcentaje, label = porcentaje)) +
  geom_col() +
  theme_minimal() +
  xlab('Sexo de las personas perpetradoras de acoso callejero') +
  geom_text(position = position_dodge(1.4),
            vjust = -0.5, 
            size = 3,
            color = "gray25") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')
