############################################
### Interatividade Gráfica (htmlwidgets) ###
### Disciplina: Análise de Dados com R   ###
###         Renan Xavier Cortes          ###
###       renanxcortes@gmail.com         ###
############################################

library(needs)
needs(tidyverse)
needs(dygraphs)
needs(plotly)
needs(D3plusR) # devtools::install_github('paulofelipe/D3plusR')
needs(highcharter)
needs(leaflet)
needs(collapsibleTree) # devtools::install_github("AdeelK93/collapsibleTree") (https://github.com/AdeelK93/collapsibleTree)


### Referências e links úteis
# https://www.htmlwidgets.org/
# http://gallery.htmlwidgets.org/
# https://rstudio.github.io/dygraphs/index.html
# https://plot.ly/
# https://rstudio.github.io/leaflet/
# https://www.highcharts.com/
# https://github.com/paulofelipe/D3plusR

# "A interatividade de um gráfico já criado é muito importante para uma análise visual mais acurada. A possibilidade de dar 
# zoom, clicar em pontos específicos, remover pontos de um gráfico de dispersão, selecionar períodos de tempo específico em 
# uma série temporal, investigar os metadados de uma observação, arrastar um mapa, aprofundar níveis de um gráfico, etc., 
# são funcionalidades muito interessantes de se adicionar para instigar um usuário quanto ao dado que está sendo 
# apresentado." - Cortes, R. X. (2017). Ensaios em Criminalidade no Rio Grande do Sul. Tese de Doutorado PUCRS.

################################
# Gráficos de Séries Temporais #
################################

#####################
# Usando o dygraphs #
#####################
lungDeaths <- cbind(mdeaths, fdeaths)
class(lungDeaths) # série Temporal Múltipla

# Exemplo 1: Séries simples
dygraph(lungDeaths)

# Exemplo 2: incluindo um cursor para ver somente um intervalo de tempo se necessário (observe que é possível selecionar só com o mouse também)
dygraph(lungDeaths) %>% 
  dyRangeSelector()

# Exemplo 3: área preenchida e com o rótulo da série personalizado
dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)



###################
# Usando o plotly #
###################
# Um gráfico simples
today <- Sys.Date()
tm <- seq(1, 60)
x <- today - tm # Cria um vetor de datas dos últimos 60 dias
y <- rnorm(length(x))
plot_ly(x = ~x, 
        y = ~y, 
        mode = 'lines', 
        text = paste(tm, "dias de hoje"))



# Um gráfico de séries mais elaborado
base_crime <- readRDS("BaseCrime_2016.rds") %>% 
              mutate(Taxa = Qtd / Populacao * 100000)
head(base_crime)
distinct(base_crime, Crime)

crime   <- "Homicídio Doloso"
cidades <- c("Porto Alegre", "Alvorada", "Canoas") 

base_aux <- filter(base_crime, Crime == crime, Mun %in% cidades)

plot_ly(base_aux, 
        x = ~Ano, 
        y = ~Taxa, 
        type = 'scatter', 
        mode = 'lines', 
        color = ~Mun, 
        hoverinfo = "text", # Os tooltips (os balões de informações) serão textos personalizados
        text = ~paste0(Mun, "<br>",
                       "Taxa: ", round(Taxa, 2), "<br>",
                       "Ano: ", Ano)) %>%
        layout(title = crime, 
               yaxis = list(title = "Taxa por 100.000"))

# Excelente link de consulta para os gráficos do plotly (guia de argumentos): https://plot.ly/r/reference
# Exemplo de argumento para retirar algus botões: config(modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'zoom2d', 'toggleSpikelines')


####################
# Usando o D3plusR #
####################
# Exemplo 1 retirado do vignette("D3plusR")
data("bra_inflation")

# Variáveis de datas devem ter esse formato
bra_inflation$Date <- format(bra_inflation$Date, "%Y/%m/%d")

# Datas a serem passada como default no filtro (argumento 'solo')
date_filter <- bra_inflation$Date[bra_inflation$Date > "2013/01/01"]

d3plus(data = bra_inflation, id = "country",
       type = "line",
       percent_var = "Rate",
       height = 400,
       width = "100%") %>% 
  d3plusX(value = "Date", grid = FALSE) %>% 
  d3plusY(value = "Rate") %>% 
  d3plusTime(value = "Date", solo = date_filter) %>% 
  d3plusTooltip(value = "Date") %>% 
  d3plusTitle("Brazilian Inflation (IPCA)")


# Exemplo 2 da base de crimes
d3plus(data = base_aux, id = "Mun",
       type = "line",
       height = 400,
       width = "100%") %>% 
  d3plusX(value = "Ano", grid = FALSE) %>% 
  d3plusY(value = "Taxa") %>% 
  d3plusTime(value = "Ano") %>%
  d3plusTooltip(value = "Ano") %>% 
  d3plusTitle(paste0("Série temporal usando o D3plusR de ", crime))







################################
# Gráficos de Dispersão/Bolhas #
################################

###################
# Usando o plotly #
###################
# Exemplo 1: gráfico de dispersão simples
plot_ly(data = iris, 
        x = ~Sepal.Length, 
        y = ~Petal.Length,
        type = "scatter")


# Exemplo 2: gráfico de bolhas elaborado usando a base de crimes
ano <- 2015
crime_x <- "Homicídio Doloso"
crime_y <- "Roubo"

base_scatter_aux <- base_crime %>%
                    filter(Ano == ano,
                           Crime %in% c(crime_x, crime_y)) %>%
                    select(Mun, Crime, Qtd, Populacao) %>%
                    spread(Crime, Qtd)

names(base_scatter_aux)[3:4] <- c("X", "Y")

x_attr <- list(title = crime_x, showgrid = TRUE)
y_attr <- list(title = crime_y, showgrid = TRUE)

t <- list(size = 10, color = "black")

plot_ly(base_scatter_aux, x = ~X, y = ~Y, type = 'scatter', mode = 'markers', size = ~base_scatter_aux$Populacao,
        hoverinfo = "text", 
        text = paste0(base_scatter_aux$Mun, "<br>", 
                      "População: ", base_scatter_aux$Populacao, "<br>",
                      crime_x, ": ", base_scatter_aux$X, "<br>",
                      crime_y, ": ", base_scatter_aux$Y),
        marker = list(opacity = 0.5, sizemode = 'diameter'), showlegend = FALSE) %>%
  layout(title = 'Gráfico de Bolhas',
         xaxis = x_attr,
         yaxis = y_attr)


########################
# Usando o highcharter #
########################
# Exemplo simples em https://www.htmlwidgets.org/showcase_highcharts.html
highchart() %>% 
  hc_title(text = "Gráfico de bolhas com tamanho e cor usando o highcharter") %>% 
  hc_add_series_scatter(x = mtcars$wt, 
                        y = mtcars$mpg,
                        z = mtcars$drat, 
                        color = mtcars$hp)





#####################
# Mapas Interativos #
#####################

#####################################################
# Usando o leaflet - Choropleth com escala de cores #
#####################################################
mapa_rs <- readRDS("MapaRS.rds")

df_aux_pre <- filter(base_crime, Ano == 2015 & Crime == "Furto")
df_mapa <- merge(mapa_rs, df_aux_pre, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)

head(df_mapa@data) # Com os dados incluídos no mapa
class(df_mapa)

gradiente = colorNumeric(c("#359800", "#FDFE65", "#FEA527", "#E7400B", "#6A1103"), domain = df_mapa$Taxa)

# Nota: é possível dar cores por nomes como, por exemplo, c("lightgrey", "yellow", "orange", "Red").

leaflet(data = df_mapa) %>% 
  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>% # Uma ista de tiles possíveis: http://leaflet-extras.github.io/leaflet-providers/preview/
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% # Tiles da Nasa :)
  addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Taxa), # Weight é a grossura das bordas
              color = "grey", fillOpacity = 0.5, # Color é a cor das bordas e o fillopacity é a tranparencia
              smoothFactor = 0.25,
              popup = paste0(df_mapa$Nome_Munic, "<br>",
                             "Pop.: ", df_mapa$Populacao, "<br>",
                             "Qtd.: ", df_mapa$Qtd, "<br>",
                             "Tx.: ", round(df_mapa$Taxa,2))) %>% 
  addLegend(position = "bottomright", pal = gradiente, values = ~Taxa)



#####################################################
# Usando o leaflet - Choropleth com grupos de cores #
#####################################################
classes <- c(0, 1000, 1500, 2000, 4000, 6000)
pal_cor <- colorBin(c("#359800", "#FDFE65", "#FEA527", "#E7400B", "#6A1103"), domain = df_mapa$Taxa, bins = classes)

leaflet(data = df_mapa) %>% 
  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
  addPolygons(weight = 0.5, fillColor = ~pal_cor(Taxa), # Weight e a grossura das bordas
              color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
              smoothFactor = 0.25,
              popup = paste0(df_mapa$Nome_Munic, "<br>",
                             "Índice: ", round(df_mapa$Taxa, 3))) %>% 
  addLegend(position = "bottomright", pal = pal_cor,values = ~Taxa)




########################################
# Usando o leaflet - Mapas de círculos #
########################################
long_lat <- coordinates(df_mapa) %>% data.frame() # Pega as latitudes e longitudes dos centróides de cada polígono de um mapa
names(long_lat) <- c("Long", "Lat")

sensibilidade_raio <- 1.9

leaflet(data = df_mapa) %>% addTiles() %>%
  addCircles(lng = ~long_lat$Long, lat = ~long_lat$Lat, weight = 1, color = "IndianRed",
             radius = ~df_mapa$Populacao ^ (1 / sensibilidade_raio) * 30, 
             popup = paste0(df_mapa$Nome_Munic, "<br>",
                            "Pop.: ", df_mapa$Populacao, "<br>",
                            "Freq.: ", df_mapa$Qtd, "<br>",
                            "Rate: ", round(df_mapa$Taxa,2)))




########################
# Treemaps Interativos #
########################

####################
# Usando o D3plusR #
####################

# Treemap simples

base_populacao <- base_crime %>%
                  filter(Ano == 2016, Crime == "Roubo") %>% # Note que poderia ser escolhido qwualquer crime, pois a população do município é a mesma
                  select(Mun, Populacao)

d3plus(data = base_populacao,
       type = "tree_map",
       id = c('Mun'),
       width = "100%",
       locale = "pt_BR") %>%
d3plusSize("Populacao")

# Treemap hierárquico (utilizando dados de Produto Interno Bruto (PIB))

# Explicação dos níveis do PIB
pib_mun_struct <- data.frame(H1 = rep("PIB", 5), 
                             H2 = c(rep("Valor Adicionado Bruto", 4), "Impostos"),
                             H3 = c("Agropecuária", "Indústria", "Serviços", "Serviços", NA),
                             H4 = c(NA, NA, "Administração Pública", "Outros Serviços", NA))

collapsibleTree(
  pib_mun_struct,
  hierarchy = c("H2", "H3", "H4"),
  root = "PIB",
  fontSize = 15
)

base_pib_mun <- readRDS("base_pib_mun.rds") %>%
                mutate(VAB_OutSer = VAB_Serv - VAB_Apub) # Criando a variável de VAB de 'Outros Serviços'

ano_escolhido <- 2014


# Filtrando um ano específico e somando para todo o RS
df_aux <- base_pib_mun %>% 
          filter(Ano == ano_escolhido) %>%
          summarize(VAB_Agro = sum(VAB_Agro, na.rm = T),
                    VAB_Ind = sum(VAB_Ind, na.rm = T),
                    VAB_Apub = sum(VAB_Apub, na.rm = T),
                    VAB_OutSer = sum(VAB_OutSer, na.rm = T),
                    Imp = sum(Imp, na.rm = T))


pib_struct_aux <- cbind(pib_mun_struct, Valor = c(df_aux$VAB_Agro,
                                                  df_aux$VAB_Ind,
                                                  df_aux$VAB_Apub,
                                                  df_aux$VAB_OutSer,
                                                  df_aux$Imp))

d3plus(data = pib_struct_aux,
       type = "tree_map",
       id = c("H1","H2","H3","H4")) %>%
  d3plusSize(value = "Valor") %>%
  d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
  d3plusDepth(0) %>%
  d3plusLabels(valign = "top")





##############
# Exercícios #
##############
# 1) Usando o plotly e a base_crime, faça um gráfico de série temporal do município de Santa Maria comparando a taxa de Furto com a de Roubo. 
# Qual a taxa (por 100.000 habitantes) de Furto de Santa Maria em 2016?

# 2) Usando o plotly, faça um gráfico de bolhas dos municípios (o tamanho do círculo proporcional à população), 
# relacionando as taxas de Furto (eixo vertical) e Roubo (eixo horizontal) para o ano de 2010. 
# Qual é o município que tem a maior taxa de furto (ou seja, o círculo mais acima do gráfico)? 
# Qual a população, taxa de roubo e de furto desse município neste ano?

# 3) Usando o leaflet, faça um mapa das taxas por 100.000 dos Roubos de Veículos em 2015 
# com o seguinte gradiente de cores: c("lightgrey", "yellow", "orange", "Red"). 
# Qual região ficou mais avermelhada no mapa?

# 4) Usando o D3plusR, faça um treemap hierárquico da estrutura setorial do PIB do município de Porto Alegre.
# Quantos % o setor de Serviços representa no Valor Adicionado Bruto nesta capital?








