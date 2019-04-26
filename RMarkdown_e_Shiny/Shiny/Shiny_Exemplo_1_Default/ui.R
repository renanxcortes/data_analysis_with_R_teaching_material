#
# Esta é a definição de interface de usuário de um aplicativo Shiny. É possível
# rodar a aplicação clicando no botão "Run App" acima.
#
# Para mais informações sobre aplicativos Shiny, acesse:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Definindo o User Interface (UI) que desenha um histograma
shinyUI(fluidPage(
  
  # Título do Aplicativo
  titlePanel("Dados do Geyser Old Faithful"),
  
  # Barra lateral com um slider com o número de classes do histograma 
  sidebarLayout(
    sidebarPanel(
       sliderInput("classes",
                   "Número de classes:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Mostrar o plot da distribuição gerada
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
