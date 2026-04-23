#Shiny
library(shiny)
require(shinybusy)
library(bslib)
library(gridlayout)
library(bsicons)

# Graficos
library(ggplot2)
library(DT)
library(plotly)
library(visNetwork)

# dados
require(tidyverse)
require(dbplyr)
library(dplyr)
require(tidyr)
require(data.table)

source("dados.R")

ui <- page_navbar(
  tags$head(

    
    tags$style(HTML("

.container_hover_image {
  position: relative;
}

.image {
  width: 100%;
  height: auto;
}

.overlay {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(255, 255, 255, 0.9); /* Fundo esbranquiçado transparente */
  display: flex; /* Usado para centralizar o conteúdo */
  justify-content: center; /* Alinha horizontalmente */
  align-items: center; /* Alinha verticalmente */
  transform: scale(0); /* Esconde inicialmente */
  transition: transform 0.3s ease;
}
.container_hover_image:hover .overlay {
  transform: scale(1);
}

.overlay .text {
  color: white;
  font-size: 20px; /* Tamanho padrão da fonte */
  text-align: center;
  transition: font-size 0.3s ease; /* Transição suave no tamanho da fonte */
}

.container_hover_image:hover .overlay .text {
  font-size: 60px; /* Tamanho da fonte ao passar o mouse */
}


################################################
.container_hover_image_pokemon {
  position: relative;
  width: 400px;
  height: 500px;
}

.image_pokemon {
  width: 100%;
  height: auto;
  position: absolute;
  transition: opacity 0.3s ease-in-out;
}

.image_pokemon_shiny {
  width: 100%;
  height: auto;
  position: absolute;
  transition: opacity 0.3s ease-in-out;
}

.overlay_pokemon {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(255, 255, 255, 0.9); /* Fundo esbranquiçado transparente */
  display: flex; /* Usado para centralizar o conteúdo */
  justify-content: center; /* Alinha horizontalmente */
  align-items: center; /* Alinha verticalmente */
  opacity: 0; /* Inicialmente invisível */
  transition: opacity 0.3s ease-in-out;
}

.container_hover_image_pokemon:hover .image_pokemon {
  opacity: 0; /* Torna a imagem original invisível */
}

.container_hover_image_pokemon:hover .overlay_pokemon {
  opacity: 1; /* Torna a imagem de costas visível ao passar o mouse */
}

    "))
  ),
  
  title = "Análise Pokémons",
  selected = "Atributos",
  collapsible = TRUE,
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  nav_panel(
    title = "Atributos",
    grid_container(
    # Detalhes do layout da Aba
    layout = c(
      "configuracoesTipos area92"
    ),
    gap_size = "5px",
    col_sizes = c(
      "250px",
      "1fr"
    ),
    row_sizes = c(
      "1fr"
    ),
    # Configuracoes a esquerda
    grid_card(
      area = "configuracoesTipos",
      card_header("Configurações"),
      card_body(
        selectInput(
          inputId = "tipoGeracao",
          label = "Selecione uma Geração",
          choices = c("Todas", geracoes_pokemons)
        ),
        selectInput(
          inputId = "tipoAtributoX",
          label = "Selecione o Atributo 1",
          choices = c(atributos_pokemons[2], atributos_pokemons[-c(1, 2)])
        ),
        selectInput(
          inputId = "tipoAtributoY",
          label = "Selecione o Atributo 2",
          choices = c(atributos_pokemons[3], atributos_pokemons[2], atributos_pokemons[-c(1, 2, 3)])
        )
      )
    ),
    
    # Area dos graficos
    grid_card(
      area = "area92",
      card_body(
        
        # Detalhes do layout dos graficos
        grid_container(
          layout = c(
            "area31 area31",
            "area34 area33"
          ),
          row_sizes = c(
            "1.00fr",
            "1.00fr"
          ),
          col_sizes = c(
            "0.50fr",
            "0.50fr"
          ),
          gap_size = "5px",
          # Graficos
          grid_card(
            area = "area31",
            card_header("Distribuição dos atributos selecionados"),
            card_body(
              plotlyOutput("atributo_grafico_dispersao")
            )
          ),
          
          grid_card(
            area = "area33",
            card_header("Média dos atributos por tipo e geração"),
            card_body(
              plotlyOutput("atributo_grafico_dispersao_tipo")
            )
            
          ),
          
          grid_card(
            area = "area34",
            card_header("Densidade dos atributos selecionados"),
            card_body(
              plotlyOutput("atributo_grafico_violino_atributo")
            )
          )
        )
        )
      )
    )
  ),
  # nav_panel(
  #   title = "Tipos",
  #   grid_container(
  #     
  #     # Detalhes do layout da Aba
  #     layout = c(
  #       "configuracoesTipos area91"
  #     ),
  #     gap_size = "5px",
  #     col_sizes = c(
  #       "250px",
  #       "1fr"
  #     ),
  #     row_sizes = c(
  #       "1fr"
  #     ),
  #     
  #     # Configuracoes a esquerda
  #     grid_card(
  #       area = "configuracoesTipos",
  #       card_header("Configurações"),
  #       card_body(
  #         selectInput(
  #           inputId = "tipoSelecionado",
  #           label = "Selecione um Tipo",
  #           choices = tipos_pokemons
  #         ),
  #         selectInput(
  #           inputId = "tipoAtributoId",
  #           label = "Selecione um atributo",
  #           choices = atributos_pokemons
  #         )
  #       )
  #     ),
  #     
  #     # Area dos graficos
  #     grid_card(
  #       area = "area91",
  #       card_body(
  #         
  #         # Detalhes do layout dos graficos
  #         grid_container(
  #           layout = c(
  #             "area21 area22 area23",
  #             "area20 area20 area20"
  #           ),
  #           row_sizes = c(
  #             "1.60fr",
  #             "0.8fr"
  #           ),
  #           col_sizes = c(
  #             "0.43fr",
  #             "0.43fr",
  #             "0.43fr"
  #           ),
  #           gap_size = "1px",
  #           
  #           # Graficos
  #           grid_card(
  #             area = "area20",
  #             card_header("Selecionados conforme Type"),
  #             card_body(DTOutput("pokemonDetalhes"))
  #           ),
  #           
  #          grid_card(
  #             area = "area21",
  #             card_header("Pokémon mais Forte, do tipo selecionado com o atributo:"),
  #             card_body(
  #               
  #               uiOutput(outputId = "pokemon_image")
  #             )
  #           ),
  #           
  #           grid_card(
  #             area = "area22",
  #             card_header("Média dos atributos, do tipo selecionado:"),
  #             card_body(
  #               plotOutput("graficoRadar")
  #             )
  #           ),
  #           
  #           grid_card(
  #             area = "area23",
  #             card_body(
  #               fluidRow(
  #                 
  #                 bslib::value_box(
  #                   title = "Total de Pokémons",
  #                   showcase = bsicons::bs_icon("database"),
  #                   value = textOutput("total_pokemons_box")
  #                 ),
  #                 bslib::value_box(
  #                   title = "Total de Golpes",
  #                   showcase = bsicons::bs_icon("database"),
  #                   value = textOutput("total_golpes_box")
  #                 ),
  #                 bslib::value_box(
  #                   title = "Forte Contra",
  #                   showcase = bsicons::bs_icon("database"),
  #                   value = tags$div(style = "font-size: 20px; font-weight: bold;", 
  #                                    uiOutput(outputId = "forte_contra_box"))
  #                 ),
  #                 bslib::value_box(
  #                   title = "Fraco Contra",
  #                   showcase = bsicons::bs_icon("database"),
  #                   value = tags$div(style = "font-size: 20px; font-weight: bold;", 
  #                                    uiOutput(outputId = "fraco_contra_box"))
  #                 ),
  #                 
  #               )
  #               
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  # ),
  nav_panel(
    title = "Pokémon",
    grid_container(
      layout = c(
        "configPokemons pokemons"
      ),
      gap_size = "10px",
      
      col_sizes = c(
        "250px",
        "1fr"
      ),
      row_sizes = c(
        "1fr"
      ),


      # Configuracoes
      grid_card(
        area = "configPokemons",
        card_header("Configurações"),
        card_body(
          selectInput(
            inputId = "pokemonSelecionado",
            label = "Selecione um Pokémon",
            choices = nomes_pokemons
          )
        ),
      
        card_body(
          # textInput('my_url', 'URL:',value="http://www.wavlist.com/humor/001/911d.wav"),
          uiOutput(outputId = 'pokemonCrie')
        )
      ),
      
      # Graficos
      grid_card(
        
        area = "pokemons",
        # Detalhes do layout dos graficos
        card_body(
          grid_container(
            layout = c(
              "area0 area0 area2 area2 area3 area3",
              "area1 area1 area2 area2 area3 area3",
              "area4 area4 area2 area2 area6 area6",
              "area4 area4 area2 area2 area6 area6",
              "area7 area8 area9 area10 area11 area12"
            ),
            row_sizes = c(
              "1.0fr",
              "1.0fr",
              "1.0fr",
              "1.0fr",
              "1.0fr"
            ),
            col_sizes = c(
              "0.43fr",
              "0.43fr",
              "0.43fr",
              "0.43fr",
              "0.43fr",
              "0.43fr"
            ),
            gap_size = "10px",
            
            # Graficos
            grid_card(
              area = "area0",
              card_header("Nome"),
              card_body(
                uiOutput(outputId = "pokemonNome")
              )
              
            ),
            
            grid_card(
              area = "area1",
              card_header("Tipos desse Pokémon"),
              card_body(
                uiOutput(outputId = "pokemonTipo")
              )
            ),
            grid_card(
              area = "area2",
              card_header("Imagem"),
              card_body(
                uiOutput(outputId = "pokemonImagem")
              )
            ),
            grid_card(
              area = "area3",
              card_header("Evolution"),
              card_body(
                visNetworkOutput("evolution_graph", height = "500px")
              )
            ),
            grid_card(
              area = "area4",
              card_header("Atributos:"),
              card_body(
                plotlyOutput("pokemonStatusRadar")
              )
            ),
#            grid_card(
#              area = "area5",
#              card_header("Configurações")
#            ),


            grid_card(
              area = "area6",
              card_header("Atributos Espécie"),
              card_body(
                plotOutput("pokemonStatusEspecie")
              )
            ),
            grid_card(
              area = "area7",
              card_header("Hp"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_hp")
              )
            ),
            grid_card(
              area = "area8",
              card_header("Attack"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_attack")
              )
            ),
            grid_card(
              area = "area9",
              card_header("Defense"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_defense")
              )
            ),
            grid_card(
              area = "area10",
              card_header("Special Attack"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_special_attack")
              )
            ),
            grid_card(
              area = "area11",
              card_header("Special Defense"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_special_defense")
              )
            ),
            
            grid_card(
              area = "area12",
              card_header("Speed"),
              card_body(
                uiOutput(outputId = "pokemon_atributo_speed")
              )
            )
            
            
            )#/grid_container
        )#/grid_card_plot
      )#grid_container
    )
  )#/nav_panel,
)