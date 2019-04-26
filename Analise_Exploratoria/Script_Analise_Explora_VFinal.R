##########################################
###   Análise Exploratória de Dados    ###
### Disciplina: Análise de Dados com R ###
###         Renan Xavier Cortes        ###
###       renanxcortes@gmail.com       ###
##########################################

library(needs)
needs(tidyverse)
needs(data.table)
needs(gapminder) # Base de dados do gapminder
needs(gridExtra) # Função grid.arrange

############
# Prelúdio #
############
# Neste código usaremos funções do dplyr e, novamente, o conceito de 'pipe' (%>%) do pacote magrittr

# Lembrando, o pipe pode ser lido como "então", pois é a entrada de uma função subsequente.
# Leia esta frase: "Pegue isto, então faça isto, então faça isto agora, então faça isto, por último."
# Usando pipes:
# Pegue isto %>% 
#  faça isto %>% 
#  faça isto, agora %>% 
#  faça isto, por último

# A %>% B indica o valor A como entrada da função B
iris %>% head

# pode-se ajustar apenas os parâmetros da função
iris %>% head(10)

# criando dois vetores e calculando a distância euclidiana entre eles
x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))

# piping
(x1-x2)^2 %>% sum() %>% sqrt()

# Parece confuso no início, mas acreditem em mim: é bem melhor e mais intuitivo deste jeito :)

###############################################################
### Estatística Descritiva ou Análise Exploratória de Dados ###
###############################################################

# Referência desta parte: https://www.datacamp.com/courses/exploratory-data-analysis

# Explorando Dados Categóricos
link <- "https://assets.datacamp.com/production/course_1796/datasets/comics.csv"


comics <- tbl_df(fread(link)) %>%
          mutate_at(.vars = c("name", "id", "align", "eye", "hair", "gender", "gsm", "alive", "first_appear", "publisher"), as.factor)

# Personagens da Marvel e da DC

# Primeiras linhas dos dados
comics

# Níveis do alinhamento/orientação
levels(comics$align)

# Níveis de Gênero
levels(comics$gender)

# Tabela de frequência de duas variáveis
table(comics$align, comics$gender)

# É possível alocar um objeto para essa tabela
tab <- table(comics$align, comics$gender)
tab

# Remover um nível de alinhamento/orientação
comics <- comics %>%
          filter(align != "Reformed Criminals") %>%
          droplevels()


# Gráfico de barras lado-a-lado por orientação de gênero e orientação
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = "dodge")

# Invertendo as variáveis
ggplot(comics, aes(x = gender, fill = align)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))


# Dentre personagens com orientação Neutra, homens são mais frequentes.
# Em geral, exite uma associaçaõ entre gênero e alinhamento/orientação.
# Existem mais personagens homens do que mulheres neste banco de dados. 

# Mudando a ordem das classes de orientação
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# Criando um plot de orientação
ggplot(comics, aes(x = align)) + 
  geom_bar()

# Quebrando por gênero
ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~ gender)



# Explorando dados numéricos.
link <- "https://assets.datacamp.com/production/course_1796/datasets/cars04.csv"

cars <- tbl_df(fread(link))
  
# Estrutura dos Dados
str(cars)

# Criar um histograma por níveis da variável se é esportivo ou não
ggplot(cars, aes(x = city_mpg)) +
  geom_histogram() +
  facet_wrap(~ sports_car)
  

# Filtrar cafrros com 4, 6 ou 8 cilindros
common_cyl <- filter(cars, ncyl %in% c(4,6,8))

# BOx-plot de milhas por galão que cada carro faz na cidade pelos número de cilindros
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Densidade não-paramétrica para os mesmos dados (https://pt.wikipedia.org/wiki/Estimativa_de_densidade_kernel)
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

# Interpretando o gráfico: 
# - Os carros que tem mais alto rendimento são os que tem 4 cilindros  
# - O típico carro de 4 cilindros é melhor que o típico carro de 6 que, por sua vez, também é melhor que o típico carro de 8 cilindros.
# - A maioria dos carros de 4 cilindros performa melhor até mesmo do que os carros de 8 cilindros mais eficientes.
  
  

# Histograma de cavalos de potência
cars %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  ggtitle("Histograma de Cavalos de Potência")

# Histograma de cavalos de potência para carros com preço abaixo de 25.000 dólares
cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle("Title")


# Existe relação entre preço e potência?
plot(cars$horsepwr, cars$msrp)

# Usando o ggplot (gráfico default mais bonito)
ggplot(cars, aes(x = horsepwr, y = msrp)) + 
  geom_point()



# Identificando outliers com boxplots:
# Construir boxplot dos preços
cars %>%
  ggplot(aes(x = 1, y = msrp)) +
  geom_boxplot()

# Exclui outliers dos dados
cars_no_out <- cars %>%
  filter(msrp < 100000)

# Construct box plot of msrp using the reduced dataset
cars_no_out %>%
  ggplot(aes(x = 1, y = msrp)) +
  geom_boxplot()

# IMPORTANTE: boxplot não é bom para distribuições multimodais (que possuem vários modas ou máximos locais)  

# O primeiro exemplo é mais apropriado um boxplot, enquanto que o segundo uma densidade
# Plot do city_mpg
cars %>%
  ggplot(aes(x=1,y=city_mpg)) +
  geom_boxplot()

# Plot do width
cars %>% 
  ggplot(aes(x=width)) +
  geom_density()

# Histograma separado por diferentes níveis de duas variáveis
# Quebrando por número de cilindros e se é SUV ou não
common_cyl %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv) +
  ggtitle("Título Geral do Gráfico")








## Resumos numéricos ##

# Principais medidas de tendência central: média, mediana e moda.

# Média: Valor "típico" é o valor médio (o centro de gravidade da distribuição). Ela é sensivel a valores atípicos.
# Mediana: o valor que divide a distribuição na metade. Ela não é necessariamente sensível a valores atípicos.
# Moda: o valor mais frequente da distribução.

x <- rchisq(1000, df = 1)
y <- rnorm(1000, mean = 2)
dados <- tibble(Valor = c(x,y), Dist = rep(c("Qui-Quadrado", "Normal"), c(length(x), length(y))))
ggplot(dados, aes(x = Valor, fill = as.factor(Dist))) +
  geom_density(alpha = .3)


# Devido à assimetria, na distribuição qui-quadrado, a mediana é mais apropriada como medida de tendência central. Por outro lado, na distribuição normal, a média é mais apropriada.

mean(x)
median(x)

mean(y)
median(y)


# Analisando os dados do gapminder
# Criando a base de dados do ano de 2007
gap2007 <- filter(gapminder, year == 2007)

# Computando as médias e medianas das expectativas de vida
gap2007 %>%
  group_by(continent) %>%
  summarize(mean(lifeExp),
            median(lifeExp))

# Criando um boxplot de expectativa de vida para cada continente
gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()

# A linha preta de cada caixa representa a mediana de cada distribuição.

# Calculando a moda de uma distribuição discreta:
x <- rbinom(100, size = 8, prob = 0.5)
table(x)
names(table(x)[which.max(table(x))])



# Medidas de variabilidade/dispersão

# Principais medidas de variabilidade: variância, desvio padrão, desvio interquartílico e amplitude.

# A variância é sensível a valores atípicos e não está na mesma unidade de medida original dos dados. 
# O desvio padrão é sensível a valores atípicos, mas está na mesma unidade de medida original dos dados.
# Desvio interquartílico é robusto a outliers.
# Amplitude: É a diferença entre o máximo e o mínimo dos dados (também sensível a outliers).

# Suponha os seguintes valores de expectativa de vida (em anos)
x     <- c(76,78,75,74,76,72,74,73,73,75,74)
x_new <- c(76,97,75,74,76,72,74,73,73,75,74) # Introduzindo um outlier para verificar o seu efeito nas medidas de variabilidade 

base <- data.frame(Valores = c(x, x_new), Tipo = rep(c("x (Sem outlier)", "x_new (Com outlier)"), c(length(x), length(x_new))))

ggplot(base, aes(x = Valores)) + 
  geom_dotplot() + 
  facet_wrap(~Tipo)

sd(x)          # Desvio padrão (em anos)
var(x)         # Variância (em anos AO QUADRADO)
IQR(x)         # Desvio inter-quartílico
diff(range(x)) # Amplitude

summary(x)     # Maneira alternativa

sd(x_new)          # Era 1.69
var(x_new)         # Era 2.87
IQR(x_new)         # Não mudou
diff(range(x_new)) # Era 6

# Computando medidas de variabilidade por continente
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

# Gerando gráficos de densidades sobrepostos
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)

# Analisando com mais profundidade as Américas
americas2007 <- gap2007 %>% filter(continent == "Americas")

p1 <- ggplot(americas2007, aes(x = lifeExp)) + geom_density() + ggtitle("Expectativa de Vida nas Américas")
p2 <- ggplot(americas2007, aes(x = pop)) + geom_density() + ggtitle("População a nível de país")

grid.arrange(p1, p2, nrow = 1)

# Quais seriam as medidas mais apropriadas (tendência central e variabilidade) para cada uma destas variáveis?

# Para expectativa de vida, seria a média e o desvio padrão
gap2007 %>%
  filter(continent == "Americas") %>%
  summarize(mean(lifeExp),
            sd(lifeExp))

# Para a população, seria a mediana e o desvio inter-quartílico
gap2007 %>%
  summarize(median(pop),
            IQR(pop))



# Formas de uma distribuição
# Uma distribuição de probabilidade pode ser classificada de diversas maneiras como, por exemplo, quanto a assimetria (à esquerda, à direita ou simétrica) e número de modas (ela pode ser multimodal).

# Observe as quatro distribuições abaixo:

A <- 20 - rchisq(1000, df = 3)
B <- rnorm(1000)
C <- rchisq(1000, df = 3)
D <- rnorm(1000, mean = rep(c(2,10), c(500,500)), sd = rep(c(1.5,1.5), c(500,500)))

base <- data.frame(Valores = c(A,B,C,D), Dist = rep(c("A", "B", "C", "D"), each = 1000))

ggplot(base, aes(x = Valores)) +
  geom_density() +
  facet_wrap(~Dist, scales = "free")


# A: unimodal e assimétrica à esquerda
# B: unimodal e simétrica
# C: unimodal e assimétrica à direita
# D: bimodal e simétrica



# Transformações
# Muitas vezes transformar uma variável aplicando alguma função a fim de "suavizar" a variabilidade e mitigar o efeito dos valores atípicos pode ser útil.

# Aplicando o logaritmo na população e vendo o efeito desta transformação:
# Densidade da variável bruta
gap2007 %>%
  ggplot(aes(x = pop)) +
  geom_density()

# Transforma a variável populacional assimétrica
gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))

# Cria a densidade desta nova variável
gap2007 %>%
  ggplot(aes(x = log_pop)) +
  geom_density()



# Outliers
# Analisando dados sem outliers
ggplot(gap2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_wrap(~continent)

# Na Ásia temos um país outlier que tem 974 de PIB per capita e expectativa de vida de 43 anos. Que país é esse?

gap2007 %>% filter(lifeExp < 45, continent == "Asia") # Afeganistão

gap2007_Asia <- gap2007 %>%
                filter(continent == "Asia") %>%
                mutate(is_outlier = lifeExp < 45)

# Plotando somente a Ásia sem o país outlier
gap2007_Asia %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point()




# Exercícios:

# Com o banco de dados 'iris' realize os exercícios abaixo.

# 1) Calcule a média, mediana, variância, desvio padrão, desvio interquartílico e amplitude cada uma das variáveis globalmente.

# 2) Calcule as mesmas medidas do exercício anterior individualmente para cada espécie (setosa, versicolor e virginica).

# 3) Realize 4 gráficos para cada uma das variáveis. Cada gráfico deve ter 3 densidades de cada uma das espécies de flores. Recomenda-se, utilizar o argumento alpha = 0.3 para melhorar o aspecto do gráfico gerado.

# 4) Classifique qual o tipo de distribuição tem o formato da variável Sepal.Width do exercício anterior (simétrico, assimétrico, unimodal, bimodal, etc.).

