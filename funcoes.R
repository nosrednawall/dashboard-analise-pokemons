library(DT)
library(httr)
library(jsonlite)

#########################################################################
########## Definindo tema
###########################################################################
COR.1="#FFFFFF"  # Fundo principal (branco)
COR.2="#78C2AD"  # Cor principal (verde menta)
COR.3="#333333"  # Texto escuro (preto suave)
COR.4="#5A6268"  # Bordas ou elementos secundários (cinza escuro)
COR.5="#17A2B8"  # Azul para links e interações
COR.6="#FFC107"  # Amarelo para destaques ou alertas


meutema <- function(){
  theme_bw()+
    theme(
      #axis.line = element_line(colour = "black"),  # Adiciona uma linha ao redor do gráfico
      axis.line = element_blank(),
      axis.title = element_text(size=20, family = "Iosevka",colour=COR.2), 
      axis.text = element_text(size=20, family = "Iosevka",colour=COR.1),
      plot.title = element_text(size=25, family = "Iosevka",colour=COR.1,face="bold",hjust=0.5),
      panel.grid = element_blank(), # Remove todas as linhas de grade
      #plot.background = element_rect(fill=NA,colour=NA),
      #panel.background = element_rect(fill=NA,colour=NA),
      panel.background = element_blank(),  # Remove o fundo do painel
      plot.background = element_blank(),    # Remove o fundo do gráfico
      
      strip.background = element_rect(fill=COR.1),
      strip.text=element_text(family="Iosevka",size=15))
}



# Função para buscar a URL da imagem do Pokémon
get_pokemon_image <- function(pokemon_name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_name)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content, flatten = TRUE)
    
    # Retorna a imagem oficial
    return(json_data$sprites$other$"official-artwork"$front_default) 
  } else {
    return(NULL)
  }
}

get_pokemon_image_shiny <- function(pokemon_name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_name)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content, flatten = TRUE)
    
    # Retorna a imagem oficial
    return(json_data$sprites$other$"official-artwork"$front_shiny) 
  } else {
    return(NULL)
  }
}

get_pokemon_small <- function(pokemon_name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_name)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content, flatten = TRUE)
    
    # Retorna a imagem oficial
    return(json_data$sprites$front_default) 
  } else {
    return(NULL)
  }
}

get_pokemon_crie <- function(pokemon_name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_name)
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content, flatten = TRUE)
    
    # Retorna a imagem oficial
    return(json_data$cries$latest) 
  } else {
    return(NULL)
  }
}


# Exemplo de função para determinar a cor com base no tipo do Pokémon
getColorForType <- function(type) {
  type_colors <- list(
    "FIRE" = "red",
    "WATER" = "blue",
    "GRASS" = "green",
    "ELECTRIC" = "yellow",
    "PSYCHIC" = "purple",
    "ICE" = "cyan",
    "DRAGON" = "orange",
    "DARK" = "black",
    "FAIRY" = "pink",
    "NORMAL" = "gray",
    "FIGHTING" = "brown",
    "FLYING" = "skyblue",
    "POISON" = "violet",
    "GROUND" = "gold",
    "ROCK" = "darkgoldenrod",
    "BUG" = "limegreen",
    "GHOST" = "darkslateblue",
    "STEEL" = "silver"
  )
  return(type_colors[[type]] %||% "black") # Cor padrão é preto
}

getTypesPokemon <- function(pokemon){
  
  pkm_selecionado <- pokemon_sem_mega %>% 
    filter(Name == pokemon)
  
  tipo_retorno <- lapply(1:nrow(pkm_selecionado), function(i) {
    paste0(pkm_selecionado$Type[i]," ")
  })
  
  return(tipo_retorno)
}
