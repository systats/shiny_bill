library(shiny)
pacman::p_load(shiny.semantic, semantic.dashboard)


# Define UI for application that draws a histogram
ui <- shinyUI(
  dashboard_page(
    dashboard_header("Hi1", inverted = T), 
    semantic.dashboard::dashboard_sidebar(inverted = T),
    dashboard_body(
      br(),
      div(class = "ui segment",
          sliderInput("slider", label = "", min = 1, max = 10, value = 2),
          textOutput("text"),
          br(),
          HTML(
            '<div class="ui card">
              <div class="content">
             <div class="right floated meta">14h</div>
             <img class="ui avatar image" src="https://www.unilu.ch/fileadmin/_processed_/csm_Baechtiger_Andre_Q_e71a2dc222.jpg"> Elliot
             </div>
             <div class="image">
             <img>
             </div>
             <div class="content">
             <span class="right floated">
             <i class="heart outline like icon"></i>
             17 likes
             </span>
             <i class="comment icon"></i>
             3 comments
             </div>
             <div class="extra content">
             <div class="ui large transparent left icon input">
             <i class="heart outline icon"></i>
             <input type="text" placeholder="Add Comment...">
             </div>
             </div>
             </div>')  
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$text <- renderText({input$slider})
}

# Run the application 
shinyApp(ui = ui, server = server)

