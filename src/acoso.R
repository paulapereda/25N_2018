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

#Denunciantes según edad

df_den_edad <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'denunciantes_edad') %>% 
  clean_names() %>% 
  filter(edad != "Total") %>% 
  select(-total_denunciantes) %>% 
  gather(-edad, key = sexo, value = porcentaje)

ggplot(df_den_edad, aes(reorder(sexo, porcentaje), porcentaje, label = porcentaje, fill = edad)) +
  geom_col() +
  theme_minimal() +
  xlab('Porcentaje de denunciantes mujeres, varones y total, por tramos de edad') +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')

#Denunciantes según edad y género

df_den_edad_gen <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'denunciante_edad_género') %>% 
  clean_names() %>% 
  filter(edad != "Total") %>% 
  select(-total) %>% 
  gather(-edad, key = genero, value = frecuencia)

ggplot(df_den_edad_gen, aes(reorder(genero, frecuencia), frecuencia, 
                            label = frecuencia, fill = edad)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')

#Denunciantes según tiempo

df_den_tiempo <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'denunciantes_tiempo') %>% 
  clean_names() %>% 
  filter(momento != "Total") %>% 
  select(-total) %>% 
  gather(-momento, key = sexo, value = porcentaje)

ggplot(df_den_tiempo, aes(sexo, porcentaje, fill = momento)) +
  geom_col() +
  theme_minimal() +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')

#Denunciantes según tiempo y tipo

df_tiempo_tipo <- read_xlsx(here('data', 'acoso.xlsx'), sheet = 'tipo_tiempo') %>% 
  clean_names() %>% 
  filter(tipo != "Total") %>% 
  select(-total) %>% 
  gather(-tipo, key = momento, value = porcentaje)

ggplot(df_tiempo_tipo, aes(reorder(tipo, porcentaje), porcentaje, fill = momento)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(caption = 'Fuente: No me halaga, me molesta. Colectivo Catalejo.')
