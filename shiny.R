## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = '镇铭的osu毕设'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard',tabName='dashboard',icon=icon('dashboard')),
      menuItem('Widgets',tabName = 'widgets',icon=icon('th'))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName='dashboard',
       h2("第一个页面"),
       fluidRow(
         box(plotOutput("plot1", height = 250)),
         box(
           title = "Controls",
           sliderInput("slider", "Number of observations:", 1, 100, 50)
         )
       )
      ),
      tabItem(
        tabName='widgets',
        h2("第二个页面")
      )
    )
    # Boxes need to be put in a row (or column)

    
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)