# Shiny
library(shiny)
require(shinybusy)
library(bslib)

# Dados
require(tidyverse)
require(dbplyr)
require(tidyr)
require(data.table)
library(dplyr)

# Graficos
library(ggplot2)
library(plotly)
library(DT)
library(fmsb)  # Para o gráfico de radar
library(visNetwork) # Grafico evolucao pokemon

# Scripts
source("dados.R")
source("funcoes.R")
source("ui.R")

server <- function(input, output, session) {
#####################################################################
################# Carregamento das variaveis
#####################################################################
  
  # Criar diretório www se não existir
  if(!dir.exists("www")) {
    dir.create("www")
  }
  
  # Recebe a variavel atributo selecionado da aba tipo
  abatipo_tipoAtributo <- debounce(reactive({
    input$tipoAtributoId
  }), millis = 300)
  
  # Recebe a variavel tipo selecionado da aba tipo
  abatipo_tipoSelecionado <- debounce(reactive({
    input$tipoSelecionado
  }), millis = 300)
  
  # Recebe a variavel pokemon selecionado da aba pokemon
  abapokemon_pokemonSelecionado <- debounce(reactive({
    input$pokemonSelecionado
  }), millis = 300)
  
  # Recebe a variavel do tipo de geracao
  aba_atributos_tipoGeracao <- debounce(reactive({
    input$tipoGeracao
  }), millis = 300)
  
  # Recebe a variavel do tipo de tipoAtributoX
  aba_atributos_tipoAtributoX <- debounce(reactive({
    input$tipoAtributoX
  }), millis = 300)
  
  # Recebe a variavel do tipo de tipoAtributoY
  aba_atributos_tipoAtributoY <- debounce(reactive({
    input$tipoAtributoY
  }), millis = 300)
  
#####################################################################
######## Filtragens reativas
#####################################################################
  
  # Lista da aba Tipos
  tipo_listaMaisFortes <- reactive({

    req(abatipo_tipoAtributo(), abatipo_tipoSelecionado())
    
    pokemon_sem_mega %>% 
      filter(Type == toupper(abatipo_tipoSelecionado())) %>% 
      arrange(desc(.data[[abatipo_tipoAtributo()]]))
  })

  # Calcula a qtde de movimentos que um tipo possui
  movimentos_tipo_selecionado <- reactive({
    req(abatipo_tipoSelecionado())
    
    moves %>% 
      filter(Type == abatipo_tipoSelecionado()) %>% 
      summarise(total = n())
  })
  
  # filtra os tipos efetivos, do tipo selecionado
  tipos_efetivos_atack <- reactive({
    req(abatipo_tipoSelecionado())
    
    typechart %>% 
      filter(Attack == abatipo_tipoSelecionado()) %>% 
      group_by(Effectiveness) %>% 
      arrange(desc(Multiplier))
  })
  
  # filtra os tipos ao qual sao fracos ao tipo selecionado
  tipos_efetivos_defesa <- reactive({
    req(abatipo_tipoSelecionado())
    
    typechart %>% 
      filter(Defense == abatipo_tipoSelecionado()) %>% 
      group_by(Effectiveness) %>% 
      arrange(desc(Multiplier))
  })
  
  # Lista da aba Pokemon
  pokemon_selecionado <- reactive({
    
    req(abapokemon_pokemonSelecionado())
    
    pokemon_sem_mega %>% 
      filter(toupper(nome_numero) == toupper(abapokemon_pokemonSelecionado()))
  })
  
  # Filtra o pokemon mais forte do tipo selecionado
  pokemon_selecionado_tipo <- reactive({
    
    req(tipo_listaMaisFortes())
    
    pokemon_sem_mega %>% 
      filter(toupper(nome_numero) == toupper(tipo_listaMaisFortes()$nome_numero[1]))
  })
  
  # Função para encontrar todos os Pokémon em uma cadeia de evoluções
  find_evolution_chain <- function(pokemon_name, evolution_data) {
    chain <- c(pokemon_name)
    added <- TRUE
    
    while (added) {
      added <- FALSE
      related <- evolution_data %>%
        filter(`Evolving from` %in% chain | `Evolving to` %in% chain) %>%
        select(`Evolving from`, `Evolving to`) %>%
        unlist() %>%
        unique()
      
      new_pokemon <- setdiff(related, chain)
      if (length(new_pokemon) > 0) {
        chain <- c(chain, new_pokemon)
        added <- TRUE
      }
    }
    
    chain
  }
  
  # Filtrar dados do Pokémon selecionado
  filtered_evolution <- reactive({
    selected_pokemon <- pokemon_selecionado()$Name[1]
    
    if (is.null(selected_pokemon)) {
      return(NULL)
    }
    
    # Encontrar toda a cadeia de evolução
    full_chain <- find_evolution_chain(selected_pokemon, evolution)
    
    # Filtrar a tabela de evolução apenas para a cadeia identificada
    evolution %>%
      filter(`Evolving from` %in% full_chain | `Evolving to` %in% full_chain)
  })
  
  # Filtra a geracao de pokemon
  pokemons_selecionados_geracao <- reactive({
    
    req(aba_atributos_tipoGeracao())
    if(aba_atributos_tipoGeracao() == "Todas"){
      pokemon_sem_mega
    }else{
      pokemon_sem_mega %>% 
        filter(Generation == aba_atributos_tipoGeracao())
    }

  })
  
  
  output$pokemonCrie <- renderUI({

    # Remove arquivos anteriores do mesmo Pokémon
    arquivos_existentes <- list.files("www", pattern = "cry_", full.names = TRUE)
    if(length(arquivos_existentes) > 0) {
      file.remove(arquivos_existentes)
    }

    selected_pokemon <- pokemon_selecionado()$Name[1]
    url_audio <- get_pokemon_crie(selected_pokemon)
    
    if(is.null(url_audio)) {
      return(tags$div("Som indisponível"))
    
    }

    # Adiciona timestamp na URL para evitar cache do navegador
    timestamp <- as.numeric(Sys.time())
    ext <- tolower(tools::file_ext(url_audio))
    if(ext == "") ext <- "ogg"
    
    # Nome único do arquivo
    filename <- paste0("cry_", selected_pokemon, "_", timestamp, ".", ext)
    filepath <- file.path("www", filename)

    # Download
    tryCatch({
      download.file(url_audio, filepath, mode = "wb", quiet = TRUE)
      
      tags$div(
        h5(paste("Som -", selected_pokemon)),
        tags$audio(
          controls = "controls",
          autoplay = FALSE,
          #preload = "metadata",  # Carrega apenas metadados, não o áudio completo
          tags$source(
            src = filename,  # Caminho relativo
            type = paste0("audio/", ext)
          )
        )
      )
      
    }, error = function(e) {
      tags$div("Erro ao carregar som")
    })
  })

  
  output$pokemonCrieRetorno <- renderUI({
    

    
    selected_pokemon <- pokemon_selecionado()$Name[1]
    url_audio <- get_pokemon_crie(selected_pokemon)
    
    # Reactive para armazenar o timestamp atual
    audio_timestamp <- reactiveVal(0)
    
    # Observer para quando o Pokémon muda
    observeEvent(input$pokemonSelecionado, {
      # Incrementa timestamp para forçar recarregamento
      audio_timestamp(audio_timestamp() + 1)
      

        selected_pokemon <- input$pokemonSelecionado
        url_audio <- get_pokemon_crie(selected_pokemon)
        
        if(is.null(url_audio)) {
          return(tags$div(
            style = "color: orange; padding: 10px;",
            "Som não disponível para este Pokémon"
          ))
        }
    
    
    tryCatch({
      url <- url_audio
      
      # Detectar extensão
      ext <- tolower(tools::file_ext(url))
      if(ext == "") ext <- "ogg"
      
      # Nome do arquivo de saída
      output_file <- paste0("www/audio.", ext)

      if(file.exists(output_file)){
        file.remove(output_file)
      } 
            
      # Download do arquivo
      download.file(url, destfile = output_file, mode = "wb", quiet = TRUE, 
                    overwrite = TRUE)
      
      # Verificar se o arquivo foi baixado
      if(!file.exists(output_file)) {
        stop("Falha no download")
      }
      
      # Determinar o tipo MIME
      mime_type <- switch(ext,
                          "ogg" = "audio/ogg",
                          "wav" = "audio/wav",
                          "mp3" = "audio/mpeg",
                          "audio/mpeg"
      )
      
      #showNotification("Áudio carregado!", type = "success", duration = 2)
      
      tags$div(
        h4("Som Pokémon:"),
        tags$audio(
          id = "audio_player",
          controls = "controls",
          autoplay = FALSE,
          tags$source(
            src = paste0("audio.", ext),
            type = mime_type
          ),
          "Seu navegador não suporta o elemento de áudio."
        ),
      )
      
    }, error = function(e) {
      tags$div(
        style = "color: red; padding: 15px; border: 1px solid red; border-radius: 5px;",
        h4("Erro:"),
        p(e$message),
        p("URL testada: ", url),
        p("Verifique se a URL é acessível e o arquivo existe.")
      )
    })
  })
})      
#####################################################################
################# Gráficos
#####################################################################
  
  ############################################################################
  ## Aba Tipos
  
  # Renderizar a imagem no UI
  output$pokemon_image <- renderUI({
    pkm_selecionado <- tipo_listaMaisFortes() %>% 
      slice(1)
    
    img_url <- get_pokemon_image(tolower(pkm_selecionado$Name))
    
    if (!is.null(img_url)) {
      tagList(
        div(
          class = "container_hover_image",
          h4(pkm_selecionado$Name),
          
          tags$img(
            src = img_url, 
            alt = paste("Imagem de", pkm_selecionado$Name),
            width = "400px",  # Largura desejada
            height = "500px",  # Altura desejada,
            class = "image"
          ),
          
          div(
 
            class = "overlay",
            tags$ul(
              tags$li("Number: ",pkm_selecionado$Number.Pokedex), 
              tags$li("Nome: ",pkm_selecionado$Name), 
              tags$li("Type:", pkm_selecionado$Type), 
              tags$li("HP:", pkm_selecionado$HP),
              tags$li("Attack:", pkm_selecionado$Attack),
              tags$li("Defense:", pkm_selecionado$Defense),
              tags$li("Special.Attack:", pkm_selecionado$Special.Attack),
              tags$li("Special.Defense:", pkm_selecionado$Special.Defense),
              tags$li("Speed:", pkm_selecionado$Speed)
              
            )
          )
        )

      )
    } else {
      h4("Pokémon não encontrado ou erro na API!")
    }
  })
  
  # Tabela interativa
  output$pokemonDetalhes <- renderDT({
    dados <- data.frame(tipo_listaMaisFortes())
  })
  
  # Grafico de barras
  output$pokemonBarraTipo <- renderPlot({
    req(input$tipoSelecionado)
    tipo_listaMaisFortes() %>%
      ggplot(aes(x = Name)) +
      geom_bar() +
      labs(title = paste("Contagem de Pokémon do Tipo" ),
           x = "Pokémon", y = "Contagem") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Grafico de radar
  output$graficoRadar <- renderPlot({
    req(abatipo_tipoSelecionado())
    
    atributos <- tipo_listaMaisFortes() %>%
      #filter(Type == abatipo_tipoSelecionado()) %>%
      summarise(across(c(HP, Attack, Defense, Special.Attack, Special.Defense, Speed), mean))
    
    # Preparando dados para o gráfico de radar
    max_min <- data.frame(
      HP = c(100, 0),
      Attack = c(100, 0),
      Defense = c(100, 0),
      Special.Attack = c(100, 0),
      Special.Defense = c(100, 0),
      Speed = c(100, 0)
    )
    dados_radar <- rbind(max_min, atributos)
    
    op <- par(mar = c(0.005,0.005,0.005,0.005))
    
    radarchart(dados_radar, 
               axistype=1,
               centerzero = 1,
               
               #custom polygon
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=1 , 
               
               #custom the grid
               cglcol="gray48", cglty=2.1, axislabcol="gray48", caxislabels=seq(0,1,5), cglwd=0.9,
               
               #custom labels
               palcex = 1.5,
               calcex = 1.5,
               vlcex=1.5,title=NULL,
               
               cex.main = 1)
    par(op) # this command is used to perform the setup of margin for radar chart
  })

  # Total de Pokémons
  output$total_pokemons_box <- renderText({
    paste0(nrow(tipo_listaMaisFortes()))
  })
  
  # Total de Golpes
  output$total_golpes_box <- renderText({
    paste0(movimentos_tipo_selecionado())
  })
  
  # Tipos forte contra
  output$forte_contra_box <- renderUI({
    
    tipos <- tipos_efetivos_atack() %>% 
      filter(Effectiveness == "Super Effective") %>% 
      select(Defense)
    
    tagList(
      lapply(1:nrow(tipos), function(i) {
        tags$h5(
          tipos$Defense[i],
          style = paste0("color: ", getColorForType(tipos$Defense[i]), ";")
        )
      })
    )
  
  })
  
  # Tipos fraco contra
  output$fraco_contra_box <- renderUI({
    
    tipos <- tipos_efetivos_defesa() %>% 
      filter(Effectiveness == "Super Effective") %>% 
      select(Attack)
    
    tagList(
      lapply(1:nrow(tipos), function(i) {
        tags$h5(
          tipos$Attack[i],
          style = paste0("color: ", getColorForType(tipos$Attack[i]), ";")
        )
      })
    )
    
  })
  
  ############################################################################
  ## Aba Pokemon
  
  # Mostra o nome do pokemon
  output$pokemonNome <- renderUI({
    tagList(
      tags$div(
        style = "display: flex; align-items: center;",
        # Nome do Pokémon à direita
        tags$h1(
          pokemon_selecionado()$Name[1],
          style = "margin: 0;" # Remove margens extras do texto
        )
      )
    )
  })
  
  # Tipos do Pokemon
  output$pokemonTipo <- renderUI({
    pkm_selecionado <- pokemon_selecionado()
    tagList(
      lapply(1:nrow(pkm_selecionado), function(i) {
        tags$h4(
          pkm_selecionado$Type[i],
          style = paste0("color: ", getColorForType(pkm_selecionado$Type[i]), ";")
        )
      })
    )
  })
  
  # atributo hp
  output$pokemon_atributo_hp <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$HP[1])
    )
  })
  
  # atributo attack
  output$pokemon_atributo_attack <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$Attack[1])
    )
  })
  
  # atributo defense
  output$pokemon_atributo_defense <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$Defense[1])
    )
  })
  
  # atributo sp attack
  output$pokemon_atributo_special_attack <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$Special.Attack[1])
    )
  })
  
  # atributo sp defense
  output$pokemon_atributo_special_defense <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$Special.Defense[1])
    )
  })
  
  # atributo speed
  output$pokemon_atributo_speed <- renderUI({
    tags$div(
      style = "text-align: center;",
      h1(pokemon_selecionado()$Speed[1])
    )
  })
  
  # Renderizar o gráfico de evolução
  output$evolution_graph <- renderVisNetwork({
    evo_data <- filtered_evolution()
    
    # Verifica se há evoluções
    if (nrow(evo_data) == 0) {
      # Caso não haja evoluções, mostrar apenas o Pokémon selecionado
      pokemon_name <- pokemon_selecionado()$Name[1]
      
      nodes <- tibble::tibble(
        id = pokemon_name,
        label = pokemon_name,
        image = get_pokemon_small(tolower(pokemon_name)),
        shape = "image"
      )
      
      visNetwork(nodes, edges = NULL) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    } else {
      # Caso haja evoluções, criar grafo completo
      
      # Criar nós
      all_pokemon <- unique(c(evo_data$`Evolving from`, evo_data$`Evolving to`))
      nodes <- tibble::tibble(
        id = all_pokemon,
        label = all_pokemon
      ) %>%
        mutate(
          image = sapply(tolower(id), get_pokemon_small), # Aplicar a função para cada Pokémon
          shape = "image"
        )
      
      # Criar arestas
      edges <- evo_data %>%
        filter(!is.na(`Evolving from`) & !is.na(`Evolving to`)) %>%
        transmute(
          from = `Evolving from`,
          to = `Evolving to`,
          label = paste0(
            ifelse(
              is.na(Level),
              ifelse(is.na(Condition), "Evolution Type: ", "Condition: "),
              "Level: "
            ),
            ifelse(is.na(Level), ifelse(is.na(Condition), `Evolution Type`, Condition), Level)
          )
        )
      
      # Criar grafo com `visNetwork`
      visNetwork(nodes, edges) %>%
        visEdges(
          arrows = "to"
        ) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    }
  })
  
  # Renderiza a imagem do Pokemon
  output$pokemonImagem <- renderUI({
    pkm_selecionado <- pokemon_selecionado()
    
    img_url <- get_pokemon_image(tolower(pkm_selecionado$Name[1]))
    img_url_shiny <- get_pokemon_image_shiny(tolower(pkm_selecionado$Name[1]))
    
    if (!is.null(img_url)) {
      tagList(
        div(
          class = "container_hover_image_pokemon",
          #h4(pkm_selecionado$Name[1]),
          
          tags$img(
            src = img_url, 
            alt = paste("Imagem de", pkm_selecionado$Name[1]),
            width = "400px",  # Largura desejada
            height = "480px",  # Altura desejada,
            class = "image_pokemon"
          ),
          
          div(
            
            class = "overlay_pokemon",
            
              #h4(pkm_selecionado$Name[1], " Shiny"),
              tags$img(
                src = img_url_shiny, 
                alt = paste("Imagem de", pkm_selecionado$Name[1], "Shiny"),
                width = "400px",  # Largura desejada
                height = "500px",  # Altura desejada,
                class = "image_pokemon_shiny"
              )
            )
          )
        )
    } else {
      h4("Pokémon não encontrado ou erro na API!")
    }
  })
  
  # Gráfico de radar interativo
  output$pokemonStatusRadar <- renderPlotly({
    req(pokemon_selecionado())
    
    # Calculando os valores máximos para a espécie
    atributos_media_especie <- pokemon_sem_mega %>%
      filter(Type %in% pokemon_selecionado()$Type) %>%
      summarise(
        HP = max(HP), 
        Attack = max(Attack), 
        Defense = max(Defense),
        Special.Attack = max(Special.Attack),
        Special.Defense = max(Special.Defense),
        Speed = max(Speed)
      )
    
    # Selecionando os atributos do Pokémon
    atributos <- pokemon_selecionado() %>% 
      select(HP, Attack, Defense, Special.Attack, Special.Defense, Speed)
    
    # Preparando os dados máximos e mínimos
    max_min <- data.frame(
      HP = c(atributos_media_especie$HP, 1),
      Attack = c(atributos_media_especie$Attack, 5),
      Defense = c(atributos_media_especie$Defense, 5),
      Special.Attack = c(atributos_media_especie$Special.Attack, 10),
      Special.Defense = c(atributos_media_especie$Special.Defense, 20),
      Speed = c(atributos_media_especie$Speed, 5)
    )
    
    # Incluindo os valores do Pokémon selecionado
    dados_radar <- rbind(max_min, atributos)
    
    # Configurando os dados para o plotly
    radar_data <- as.data.frame(t(dados_radar)) # Transpõe para estruturar por linhas
    colnames(radar_data) <- c("Max", "Min", "Current")
    
    # Adicionar uma coluna `theta` para os rótulos
    radar_data$theta <- rownames(radar_data)
    
    # Duplicar o primeiro valor no final para fechar o gráfico
    radar_data <- rbind(radar_data, radar_data[1, ])
    
    # Preparando o gráfico
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers'
    ) %>%
      add_trace(
        r = radar_data$Max,
        theta = radar_data$theta,
        name = "Máximo",
        line = list(color = 'rgb(0, 123, 255)')
      ) %>%
#      add_trace(
#        r = radar_data$Min,
#        theta = radar_data$theta,
#        name = "Mínimo",
#        line = list(color = 'rgb(255, 99, 132)')
#      ) %>%
      add_trace(
        r = radar_data$Current,
        theta = radar_data$theta,
        name = "Atual",
        fill = 'toself',
        fillcolor = 'rgba(0, 200, 100, 0.2)',
        line = list(color = 'rgb(0, 200, 100)')
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(radar_data$Max)) # Define o alcance do eixo
          )
        ),
        showlegend = TRUE
      )
    
    fig
  })
  
  # Renderizar o gráfico
  output$pokemonStatusEspecie <- renderPlot({
    
    req(pokemon_selecionado())
    
    evo_data <- filtered_evolution()
    
    if (nrow(evo_data) == 0) {
      pokemon_names <- pokemon_selecionado()$Name
    } else{
      # Criar um vetor único com os nomes dos Pokémon
      pokemon_names <- unique(c(evo_data$`Evolving from`, evo_data$`Evolving to`))
      
    }

    # Remover valores NA, se houver
    pokemon_names <- na.omit(pokemon_names)
    
    atributos <- pokemon_sem_mega %>% 
      filter(Name %in% pokemon_names) %>% 
      select(Number.Pokedex, Name, Type, HP, Attack, Defense, Special.Attack, Special.Defense, Speed) %>%
      mutate(Number.Pokedex = as.numeric(as.character(Number.Pokedex))) %>%  # Convertendo para numérico
      arrange(Number.Pokedex)  # Ordenando corretamente
    
    names(atributos) <- c("Number.Pokedex", "Name", "Type", "HP", "Attack", "Defense", 
                          "Sp.Attack","Sp.Defense", "Speed")
    
    atributos_long <- atributos %>%
      pivot_longer(cols = c(HP, Attack, Defense, Sp.Attack, Sp.Defense, Speed), 
                   names_to = "Attribute", 
                   values_to = "Value") %>%
      arrange(Number.Pokedex)  # Ordenar conforme o número da Pokédex
    
    # Criar uma paleta de cores dinâmica
    unique_names <- unique(atributos_long$Name)  # Obter nomes únicos
    color_palette <- scales::hue_pal()(length(unique_names))  # Gerar cores distintas
    names(color_palette) <- unique_names  # Nomear as cores com os Pokémon
    
    
    atributos_long %>%
      ggplot(aes(x = Attribute, y = Value, fill = Name)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      scale_fill_manual(values = color_palette) +  # Paleta dinâmica
      labs(
        title = "",
        x = "",
        y = ""
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),  # Remove o fundo do painel
        plot.background = element_blank(),   # Remove o fundo do gráfico
        panel.grid = element_blank(),        # Remove todas as linhas de grade
        axis.text = element_text(size = 16, colour = COR.3),
        legend.text = element_text(size = 16, colour = COR.3)
      )
      #coord_flip()  # Inverte os eixos
    
  })
  
  
  ############################################################################
  ## Aba Atributos
  
  output$atributo_grafico_dispersao <- renderPlotly({
    
    tipoX <- aba_atributos_tipoAtributoX()
    tipoY <- aba_atributos_tipoAtributoY()
    
    # Adicione uma coluna `customdata` para imagens
    pokemons_local <- pokemons_selecionados_geracao() %>%
      mutate(
        text = paste(
          "<span style='font-size:16px;'><b>", nome_numero, "</b></span><br>",
          "<br><b>Generation:</b> ", Generation,
          "<br><b>Tipo:</b> ", Type,
          "<br><b>",tipoX,":</b> ", .data[[tipoX]],
          "<br><b>",tipoY,":</b> ", .data[[tipoY]],
          sep = ""
        )
      )
    
    # Criar gráfico plotly com customdata para imagem
    plot <- plot_ly(
      pokemons_local,
      x = ~.data[[tipoX]],
      y = ~.data[[tipoY]],
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10
      ),
      text = ~text,
      hoverinfo = "text",
      color = ~Type,
      colors = "Set3"
    ) %>% 
      layout(
        xaxis = list(
          title = tipoX, 
          showgrid = FALSE  # Remover a grid do eixo X
        ),  # Modificar o título do eixo X
        yaxis = list(
          title = tipoY,
          showgrid = FALSE  # Remover a grid do eixo X  
        )   # Modificar o título do eixo Y
      )
    
    # Exibir o gráfico
    plot
  })
  
  output$atributo_grafico_dispersao_tipo <- renderPlotly({
    tipoX <- aba_atributos_tipoAtributoX()
    tipoY <- aba_atributos_tipoAtributoY()
    
    pokemons_local <- pokemons_selecionados_geracao() %>%
      group_by(Type) %>%
      summarise(
        mean_x = mean(.data[[tipoX]], na.rm = TRUE),
        mean_y = mean(.data[[tipoY]], na.rm = TRUE)
      ) %>% 
      mutate(
        text = paste(
          "<span style='font-size:16px;'><b>Tipo: ", Type, "</b></span><br>",
          "<br><b>Média ",tipoX,":</b> ", round(mean_x,2),
          "<br><b>Média ",tipoY,":</b> ", round(mean_y,2),
          sep = ""
        )
      )
    
    # Criar gráfico plotly com customdata para imagem
    plot <- plot_ly(
      pokemons_local,
      x = ~mean_x,
      y = ~mean_y,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10
      ),
      text = ~text,
      hoverinfo = "text",
      color = ~Type,
      colors = "Set3"
    ) %>% 
      layout(
        xaxis = list(
          title = tipoX, 
          showgrid = FALSE  # Remover a grid do eixo X
        ),  # Modificar o título do eixo X
        yaxis = list(
          title = tipoY,
          showgrid = FALSE  # Remover a grid do eixo X  
        )   # Modificar o título do eixo Y
      )
    
    # Exibir o gráfico
    plot
    
  })
  
  output$atributo_grafico_violino_atributo <- renderPlotly({
    tipoX <- aba_atributos_tipoAtributoX()
    tipoY <- aba_atributos_tipoAtributoY()
    
    # Reestruturar os dados para um formato longo
    pokemon_long <- pokemons_selecionados_geracao() %>%
      select(.data[[tipoX]], .data[[tipoY]]) %>%
      pivot_longer(cols = c(.data[[tipoX]], .data[[tipoY]]), names_to = "Atributo", values_to = "Valor")
    
    # Calcular moda para cada atributo
    moda_por_atributo <- pokemon_long %>%
      group_by(Atributo) %>%
      summarize(Moda = Valor[which.max(tabulate(match(Valor, unique(Valor))))],
                text = paste0("Moda: ", round(Moda, 2)))
    
    # Criar o gráfico ggplot
    grafico <- ggplot(pokemon_long, aes(x = Atributo, y = Valor)) +
      geom_violin(aes(fill = Atributo), trim = FALSE, alpha = 0.8, show.legend = FALSE) +  # Violino sem legenda
      #geom_jitter(width = 0.2, alpha = 0.6, color = "darkgray", size = 1.5, show.legend = FALSE) +  # Pontos sem legenda
      stat_summary(
        fun = median, 
        geom = "point", 
        aes(
          color = "Mediana", 
          shape = "Mediana", 
          text = after_stat(paste0("Mediana: ", round(y, 2)))
        ), 
        size = 3
      ) +  # Mediana
      stat_summary(
        fun = mean, 
        geom = "point", 
        aes(
          color = "Média", 
          shape = "Média", 
          text = after_stat(paste0("Média: ", round(y, 2)))
        ), 
        size = 3
      ) +  # Média
      geom_point(
        data = moda_por_atributo,
        aes(x = Atributo, y = Moda, color = "Moda", shape = "Moda", text = text),
        size = 3
      ) +
      scale_fill_brewer(palette = "Set3") +  # Cores dos violinos
      scale_color_manual(
        values = c(
          "Mediana" = "red",
          "Média" = "blue",
          "Moda" = "purple"
        ),
        name = "Estatísticas"  # Título da legenda
      ) +
      scale_shape_manual(
        values = c(
          "Mediana" = 17,
          "Média" = 18,
          "Moda" = 16
        ),
        name = "Estatísticas"  # Título da legenda
      ) +
      labs(
        title = "",
        x = "",
        y = "Valores"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # Legenda à direita
        panel.grid = element_blank(), # Remove todas as linhas de grade
        panel.background = element_blank(),  # Remove o fundo do painel
        plot.background = element_blank(),    # Remove o fundo do gráfico
      )
    
    # Converter para gráfico interativo
    grafico_interativo <- ggplotly(
      grafico,
      tooltip = c("text")  # Apenas exibe o texto personalizado
    )
    
    # Exibir o gráfico interativo
    grafico_interativo
  })
  
}

  

