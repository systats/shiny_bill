library(shiny)
# devtools::install_github("systats/semanticWidgets")
pacman::p_load(shiny.semantic, semantic.dashboard, dplyr, stringr, semanticWidgets, plotly)

custom_header <- function(...){
  shiny::div(class = paste("ui top attached inverted menu"),
    div(class="item", 
      "Shiny Bill"
    ),
    shiny::tags$a(id = "toggle_menu", class = "item", shiny::tags$i(class = "sidebar icon"), "Menu"),
    shiny::div(class = "right icon menu", ...)
  )
}

source("helper.R")

# Define UI for application that draws a histogram
ui <- shinyUI(
  dashboard_page(
    custom_header(), 
    dashboard_sidebar(
      inverted = T,
      menuItem(tabName = "first", text = "Check", icon = uiicon("list")),
      menuItem(tabName = "second", text = "Analysis", icon = uiicon("chart line"))
    ),
    dashboard_body(
      tabItems(
        tabItem(tabName = "first",
          box(title = "Select", ribbon = F, title_side = "top", color = "blue", width = 16,
            br(),
            div(class = "ui grid", 
              div(class = "row",
                div(class = "three wide column",
                  selectInput("type", label = "", choices = c("proposal", "bill"), selected = "bill")
                ),
                div(class = "three wide column",
                  uiOutput("drop_number")
                )
              )
            )
          ),
          div(class = "seven wide column", 
            div(class = "ui top attached label", "Original PDF"),
            htmlOutput('pdfviewer')
          ),
          div(class = "one wide column", ""),
          div(class = "seven wide column", 
            div(class = "ui top attached label", "Parsed Text"),
            textOutput("text")
          ),
          plotlyOutput("flow")
        ),
        tabItem(tabName = "second",
          ""#new_input()
        )
      )
    )
  )
)


load("dt.Rdata")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  file_list <- reactive({
    dir(paste0("www/data/", input$type)) %>%
      str_replace("\\..*?$", "")
  })
  
  output$drop_number <- renderUI({
    tagList(
      shiny::selectInput(
        "number", 
        label = NULL, 
        choices = file_list(), 
        selected = "None"
      )
    )
  })
  
  output$text <- renderText({
    if(is.null(input$number)) return()
    readLines(file(paste0("www/data/", input$type, "/", input$number, ".txt")))  
  })
  
  output$pdfviewer <- renderText({
    if(is.null(input$number)) return()
    return(paste0('<iframe style="height:600px; width:100%" src="', "data/", input$type, "/", input$number, ".pdf" , '"></iframe>'))  
  })
  
  output$switch <- renderText({
    input$switch
  })
  
  vorgaenge <- reactive({ 
    if(is.null(input$number)) return()
    #vorgaenge$id
    #vorgaenge <- 
    dt %>%
      dplyr::mutate(
        lp = gesetzid %>% 
          as.character %>% 
          str_extract("WP\\d+") %>% 
          str_replace("WP", "")
      ) %>%
      dplyr::mutate(id = gesetzid %>% as.character %>% str_extract("\\d{5,}")) %>%
      dplyr::mutate(id = paste0(lp, id)) %>%
      dplyr::filter(id %in% input$number) %>%
      mutate(issue = title %>% as.character %>% stringr::str_sub(start = 1, end = 50)) %>%
      mutate(institution = ifelse(institution == "BT", 2, 1))
  })
  
  output$flow <- renderPlotly({
    if(is.null(vorgaenge())) return()
    gg1 <- vorgaenge() %>%
      ggplot(aes(as.Date(date1, format = "%d.%m.%Y"), institution)) +
      geom_line(size = 5, alpha = .3) +
      geom_point(aes(colour = as.factor(institution)), size = 5, alpha = .7) +
      ggthemes::theme_few() +
      labs(x = "", y = "") + 
      theme(legend.position = "bottom") +
      ggthemes::scale_colour_fivethirtyeight("", labels = c("BR", "BT"))
    ggplotly(gg1)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)