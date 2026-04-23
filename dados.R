require(tidyverse)
require(dbplyr)
require(tidyr)
require(data.table)
require(readxl)
library (stringr)

###### Datasets
pokemons <- read_excel("pokemon.xlsx", sheet = 1, col_names = TRUE)
moves <- read_excel("pokemon.xlsx", sheet = 2, col_names = TRUE)
evolution <- read_excel("pokemon.xlsx", sheet = 3, col_names = TRUE)
typechart <- read_excel("pokemon.xlsx", sheet = 4, col_names = TRUE)

##### Preparando os datasets
names(pokemons) <- c("Number.Pokedex", "Name", "Type", "Total", "HP", "Attack", "Defense", 
                     "Special.Attack","Special.Defense", "Speed")

## Remove os espacos a esquerda
pokemons <- pokemons %>%
  mutate(Number.Pokedex = str_trim(Number.Pokedex, "left"))

## Remove os espacos a direita
pokemons <- pokemons %>%
  mutate(Number.Pokedex = str_trim(Number.Pokedex, "right"))

pokemons <- pokemons %>%
  mutate(Generation = case_when(
    as.integer(Number.Pokedex) <= 151 ~ 1,
    as.integer(Number.Pokedex) <= 251 ~ 2,
    as.integer(Number.Pokedex) <= 386 ~ 3,
    as.integer(Number.Pokedex) <= 493 ~ 4,
    as.integer(Number.Pokedex) <= 649 ~ 5,
    as.integer(Number.Pokedex) <= 721 ~ 6,
    as.integer(Number.Pokedex) <= 809 ~ 7,
    as.integer(Number.Pokedex) <= 898 ~ 8,
    TRUE ~ 9
  ))

# Adiciona a url da imagem
pokemons <- pokemons %>%
  mutate(ImageURL = paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", 
                           as.integer(Number.Pokedex), ".png"))  # Substitua por URLs reais
# Adiciona coluna Nome-numero
pokemons <- pokemons %>% 
  mutate(nome_numero = paste0(Number.Pokedex,"-",Name))

##### Preparando os dados
pokemon_sem_mega <- pokemons %>% 
  filter(!str_detect(Name, "Mega"))

pokemon_sem_mega <- pokemons %>% 
  filter(grepl("^[A-Za-z]+$", Name))

# Crio um dataframe com os tipos
tipos_pokemons <- pokemons %>% 
  group_by(Type) %>% 
  arrange(Type) %>% 
  distinct(Type)

atributos_pokemons <- c("Total", "HP", "Attack", "Defense", "Special.Attack",
                        "Special.Defense", "Speed")

nomes_pokemons <- pokemon_sem_mega %>% 
  mutate(nome_numero = paste0(Number.Pokedex,"-",Name)) %>% 
  group_by(nome_numero) %>% 
  arrange(Number.Pokedex) %>% 
  distinct(nome_numero)


geracoes_pokemons <- pokemon_sem_mega %>% 
  group_by(Generation) %>% 
  arrange(Generation) %>% 
  select(Generation) %>% 
  distinct(Generation)
