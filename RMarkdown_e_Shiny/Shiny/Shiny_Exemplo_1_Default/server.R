#
# Esta é a lógica do servidor (server) de um aplicativo Shiny. É possível
# rodar a aplicação clicando no botão "Run App" acima.
#
# Para mais informações sobre aplicativos Shiny, acesse:
# 
#    http://shiny.rstudio.com/
#


library(shiny)

# Definindo a lógica do servidor necessária para desenhar um histograma
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # Gerando o número de classes baseado no onjeto input$bins do arquivo 'ui.R'
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$classes + 1)
    
    # Desenha o histograma com o número de classes especificado
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
