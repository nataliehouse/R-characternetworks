#written by Natalie House - recreating work from my PhD in R - https://github.com/nataliehouse/

library(jsonlite)
library(tidyverse)
library(networkD3)
library(shiny)
library(igraph)
library(purrr)

#load novel data from json files
novels <- data.frame(
  file = c(
    "data/ttt.json",
    "data/tmaas.json",
    "data/tssr.json",
    "data/asis.json",
    "data/hpatps.json",
    "data/tltwatw.json"
  ),
  title = c(
    "The Time Traders by Andre Norton",
    "The Mysterious Affair at Styles by Agatha Christie",
    "The Stainless Steel Rat by Harry Harrison",
    "A Study in Scarlet by Arthur Conan Doyle",
    "Harry Potter and the Philosopher's Stone by JK Rowling",
    "The Lion, The Witch and The Wardrobe by C.S Lewis"
  ),
  stringsAsFactors = FALSE
)

#user interface
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4ecd8;
        font-family: 'Georgia', serif;
        color: #2f2f2f;
        margin: 0;
      }

      svg {
        display: block;
        margin: auto;
        margin-top: -200px;
      }

      .container-fluid {
        background-color: #f4ecd8;
      }

      .title-panel {
        text-align: center;
        font-weight: 600;
        margin-top: 10px;
        margin-bottom: 5px;
        font-size: 16px;
      }

      #chapter_label {
        text-align: center;
        font-size: 18px;
        margin-bottom: 10px;
        color: #4a3f2f;
      }

      .form-group.shiny-input-container {
        width: 100%;
      }

      input[type='range'] {
        width: 100% !important;
      }

      .irs--shiny .irs-bar {
        background: #8b6f47;
        border-color: #8b6f47;
      }

      .irs--shiny .irs-handle {
        background: #5c4630;
        border: 2px solid #5c4630;
      }

      #network {
        background-color: #f4ecd8;
        border-radius: 10px;
      }

      .irs-bar, .irs-line {
        height: 1px !important;
      }

      .irs-grid-text {
        font-size: 11px;
        color: #4a3f2f;
      }

      .irs-grid-pol {
        background: #8b6f47;
      }
      
      .glyphicon-play {
       color: #000000;
      }
      }
    "))
  ),
  
#app title
  titlePanel(
    div("Character Dialogue Networks", class = "title-panel")
  ),
  
#novel selector
  div(
    style = "width: 100%; padding: 10px 25px 5px 25px;",
    selectInput(
      "novel",
      label = NULL,
      choices = setNames(novels$file, novels$title),
      selected = novels$file[1]
    )
  ),
  
#chapter label
  div(style = "text-align:center;",
      textOutput("chapter_label")
  ),
  
#slider
  div(
    style = "width: 100%; padding: 10px 25px 20px 25px;",
    sliderInput(
      "chapter",
      label = NULL,
      min = 1,
      max = 1,
      value = 1,
      step = 1,
      ticks = TRUE,
      width = "100%",
      animate = animationOptions(interval = 2000, loop = FALSE)
    )
  ),
  
 #network
  forceNetworkOutput("network", height = "850px")
)

#server
server <- function(input, output, session) {
  
#load novel reactively
  novel_data <- reactive({
    
    data <- fromJSON(input$novel, flatten = TRUE)
    
    nodes <- data$characters %>%
      select(id, name)
    
    relationships_clean <- data$relationships[
      lengths(data$relationships) > 0
    ]
    
    edges_time <- map_dfr(seq_along(relationships_clean), function(i) {
      rel <- relationships_clean[[i]]
      bind_rows(rel) %>%
        mutate(
          strength = as.numeric(strength),
          chapter = i
        )
    })
    
    list(
      data = data,
      nodes = nodes,
      edges = edges_time,
      n_chapters = length(relationships_clean)
    )
  })
  
#update chapter slider when novel changes
  observe({
    updateSliderInput(
      session,
      "chapter",
      max = novel_data()$n_chapters,
      value = 1
    )
  })
  
#novel title
  output$novel_title <- renderText({
    novels$title[novels$file == input$novel]
  })
  
  output$chapter_label <- renderText({
    paste0("Chapter ", input$chapter, " of ", novel_data()$n_chapters)
  })
  
#network
  output$network <- renderForceNetwork({
    
    nd <- novel_data()
    
    edges_filt <- nd$edges %>%
      filter(chapter == input$chapter) %>%
      group_by(source, target) %>%
      summarise(strength = sum(strength), .groups = "drop")
    
    active_nodes <- unique(c(edges_filt$source, edges_filt$target))
    
    nodes_sub <- nd$nodes %>%
      filter(id %in% active_nodes)
    
    node_map <- data.frame(
      id = nodes_sub$id,
      index = seq_len(nrow(nodes_sub)) - 1
    )
    
    edges_d3 <- edges_filt %>%
      inner_join(node_map, by = c("source" = "id")) %>%
      rename(source_idx = index) %>%
      inner_join(node_map, by = c("target" = "id")) %>%
      rename(target_idx = index) %>%
      mutate(value = strength) %>%
      select(source = source_idx,
             target = target_idx,
             value)
    
    nodes_d3 <- data.frame(
      name = nodes_sub$name,
      group = seq_len(nrow(nodes_sub))
    )
    
    forceNetwork(
      Links = edges_d3,
      Nodes = nodes_d3,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      Group = "group",
      
      charge = -80,
      linkDistance = 120,
      
      opacity = 0.95,
      fontSize = 14,
      zoom = TRUE,
      bounded = TRUE,
      
      linkColour = "rgba(80,80,80,0.25)",
      opacityNoHover = 1
    )
  })
}

#run the app
shinyApp(ui, server)