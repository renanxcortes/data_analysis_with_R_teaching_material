library(shiny)
library(miniUI)

ui <- miniPage(
  miniButtonBlock(
    shiny::flowLayout(
      sliderInput("rsquared", "R^2", 0, 1, c(0, 0.25))
    )
  ),
  miniContentPanel(
    plotOutput("plot", height = "100%")
  )
)

server <- function(input, output) {
  selected_models <- reactive({
    models %>%
      filter(between(rsq, input$rsquared[1], input$rsquared[2])) %>%
      head(20)
  })
  
  rows <- reactive({
    ceiling(nrow(selected_models()) / 2)
  })
  
  output$plot <- renderPlot({
    selected_models() %>%
      semi_join(gapminder, ., by = "country") %>%
      ggplot(aes(year, lifeExp)) +
      geom_line() +
      facet_wrap(~country, ncol = 2) +
      theme(plot.margin = margin(0, 0, 0, 0))  +
      xlab(NULL) +
      ylab(NULL)
  }, height = function(...) rows() * 150, res = 96)
}

runGadget(ui, server, viewer = paneViewer("maximize"))