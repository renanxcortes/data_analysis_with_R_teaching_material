###############################################################
### Importando e Manipulando dados: Introdução do tidyverse ###
###            Disciplina: Análise de Dados com R           ###
###                    Renan Xavier Cortes                  ###
###                  renanxcortes@gmail.com                 ###
###############################################################

# Uma parte deste código tem como inspiração um material construído por Filipe Zabala (obrigado!).

### Referências
# http://r4ds.had.co.nz/
# http://datacamp-community.s3.amazonaws.com/076cc4b0-6e77-4c75-b369-e60d1434817c

library(needs)
needs(tidyverse)
needs(data.table)     # Função fread (maneira rápida de ler arquivos)
needs(readxl)         # Funções para leitura de arquivos excel como excel_sheets, read_excel, etc.
needs(gdata)          # Função read.xls
needs(DBI)            # Conexão com bancos de dados
needs(RMySQL)         # Conexão com MySQL
needs(nycflights13)   # Base de dados 'flights'
needs(microbenchmark) # Função 'microbenchmark' para verificar desempenho

#########################
# Importando Dados no R #
#########################

# Uma maneira não muito reprodutível de especificar as pastas para leitura de arquivos:
# setwd("caminho_específico_que_contém_o_arquivo")

# Importando um arquivo .csv
potatoes  <- read_csv("potatoes.csv")   # Separado por vírgula  
potatoes2 <- read_csv2("potatoes2.csv") # Separado por ponto-e-vígula

# Tendo um pouco mais de versatilidade usando read_tsv ou read_delim 
properties <- c("area", "temp", "size", "storage", "method","texture", "flavor", "moistness")
potatoes <- read_tsv("potatoes.txt", col_names=properties)
potatoes <- read_delim("potatoes.txt", delim="\t", col_names = properties)

# Se os arquivos são grandes (tipo um milhão de linhas) é possível ler pedaços dele somente com ‘skip’. No entanto, temos que especificar o nome das colunas e ‘n_max’.
potatoes_fragment <- read_tsv("potatoes.txt", skip = 6, n_max = 5, col_names = properties)


# Importando a base 'hotdogs' sem os tipos das colunas
hotdogs <- read_tsv("hotdogs.txt", col_names = c("type", "calories", "sodium"))
head(hotdogs); summary(hotdogs)

# Os coletores que serão usados para importar os dados
fac <- col_factor(levels = c("Beef", "Meat", "Poultry"))
int <- col_integer()

# Editando o argument do tipo de coluna para importar os dados corretamente: hotdogs_factor
hotdogs_factor <- read_tsv("hotdogs.txt",
                           col_names = c("type", "calories", "sodium"),
                           col_types = list(fac, int, int))
head(hotdogs_factor); summary(hotdogs_factor)


# Maneira alternativa de leitura usando a função 'fread' ('Fast and friendly file finagler': apropriada para grandes bases)
hotdogs <- tbl_df(fread("hotdogs.txt", stringsAsFactors = T))

# O pacote data.table tem como propósito velocidade, manipulação e uma maneira poderosíssima de importar que é a função fread.
# Ela é absolutamente esperta na medida em que ela pode inferir se as colunas tem ou não nomes, qual o tipo de separados, tipo de dados, etc.
# A classe que retorna é data.table.

# Suponha que você tem um dataset que tem 5 variáveis e você quer manter a primeira e a quinta variável (sendo que elas se chamam "a", "b", "c", "d" e "e"). 
# As seguintes opções farão o desejado:

fread("path/to/file.txt", drop = 2:4)
fread("path/to/file.txt", select = c(1, 5))
fread("path/to/file.txt", drop = c("b", "c", "d"))
fread("path/to/file.txt", select = c("a", "e"))


# Para maiores informações de todos os argumentos ?read_csv e ?fread


# Lendo arquivos Excel

# Ler aquivos excel, podendo ser de várias planilhas, tanto do .xls quanto .xlsx.
# Nomes das planilhas de um arquivo .xlsx
excel_sheets("urbanpop.xlsx")

# Lendo várias planilhas uma por uma:
pop_1 <- read_excel("urbanpop.xlsx", sheet = 1)
pop_2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop_3 <- read_excel("urbanpop.xlsx", sheet = 3)

# Colocando pop_1, pop_2 and pop_3 em uma lista: pop_list
pop_list <- list(pop_1, pop_2, pop_3)

# Mostrando a estrutura da pop_list
str(pop_list)

# Maneira alternativa usando o lapply:
pop_list <- lapply(excel_sheets("urbanpop.xlsx"), read_excel, path = "urbanpop.xlsx")



# Lendo arquivos .xls (versão mais antiga do Excel) com a função 'read.xls' do pacote 'gdata':
columns <- c("country", paste0("year_", 1967:1974))
urban_pop <- read.xls("urbanpop.xls", 
                      sheet = 2,
                      skip = 50, 
                      header = F, 
                      stringsAsFactors = F,
                      col.names = columns)
head(urban_pop, 10)

# Conexão com MySQL
con <- dbConnect(RMySQL::MySQL(), # DBI driver
                 dbname = "nome_do_database",
                 host = "nome_do_host",
                 port = numero_da_porta,
                 user = "nome_do_usuario",
                 password = "seha_do_usuario")

tables <- dbListTables(con)                  # Lista das tabelas presentes na base de dados "nome_do_database".
dt <- dbReadTable(con, "nome_de_uma_tabela") # Cria o objeto 'dt' (data.frame) com uma base de dados da conexão criada.

# Nota: para importar dados de outros softwares como SAS, Stata, SPSS, etc. se utiliza o pacote 'foreign' ou 'haven'.











################################################################
### 1 Manipulando dados com dplyr and tidyr (Data Wrangling) ###
################################################################

###
## dplyr
#

# tibble, o data frame moderno
vignette('tibble')


# A seguir, seguem algumas vantagens de se usar tibble ao invés de um data.frame:


# desempenho
l <- replicate(26, sample(100), simplify = FALSE)
names(l) <- letters

# https://pt.wikipedia.org/wiki/Micro
microbenchmark::microbenchmark(
  as_tibble(l),
  as.data.frame(l)
)


# imprimindo na tela (printing) (tela melhor aproveitada)
head(volcano)
as_tibble(volcano)
tbl_df(volcano)    # equivale a as_tibble


# obtendo subgrupos (subsetting)
df1 <- data.frame(x = 1:3, y = 3:1)
class(df1[, 1:2])
class(df1[, 1])

# mantém a classe
df2 <- as_tibble(df1)
class(df2[, 1:2])
class(df2[, 1])

class(df2[[1]])
class(df2$x)


# tibbles não fazem captura parcial (comportamento indesejado)
df <- data.frame(abc = 1)
df$a

df2 <- tibble(abc = 1)
df2$a


# referência a variáveis recém criadas
data.frame(
  x1 = 1:5, 
  y1 = 1, 
  z1 = x1^2 + y1
)

tibble(
  x2 = 1:5, 
  y2 = 1, 
  z2 = x2^2 + y2
)


# tibbles permitem nomes de variáveis não válidos no R
df <- data.frame(
  `:)` = 'smile', 
  ` ` = 'space',
  `2000` = 'number'
)
df

tb <- tibble(
  `:)` = 'smile', 
  ` ` = 'space',
  `2000` = 'number'
)
tb



###
## magrittr forward-pipe operator: %>%
#

# O pipe pode ser lido como "então", pois é a entrada de uma função subsequente.
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

#########
# dplyr #
#########

# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# dplyr fornece uma função para cada verbo na manipulação de dados

# filter(): filtra a base de dados nas linhas de acordo com alguma condição de uma (ou mais) variável (variáveis)
# slice(): filtra a base de dados nas linhas de acordo com índices estabelecidos
# arrange(): ordena uma base de dados de acordo com uma (ou mais) variável (variáveis)
# select(): seleciona um conjunto de variáveis
# rename(): renomeia uma variável
# distinct(): pega somente valores distintos
# mutate(): cria uma nova coluna sendo oriunda de operações de outras colunas já existentes
# transmute(): cria uma nova variável e 'dropa' todas as outras variáveis da base
# group_by(): cria grupos na base de dados para realizar operações de resumo destes grupos
# summarise(): realiza estatísticas de resumo de acordo com grupos criados
# sample_n(): extrai uma amostra de tamanho 'n' da base de dados 
# sample_frac(): retira uma fração de x% da base de dados


flights # Do pacote 'nycflights13'

# filter()
filter(flights, month == 1, day == 1)
flights %>%
  filter(month == 1, day == 1)

table(filter(flights, month == 1 | month == 2)$month)



# slice()
slice(flights, 1:11)
flights %>% 
  print(n = 11)
flights %>% 
  print(n = 11, width = Inf)



# arrange(): ordena de maneira ascendente e deve-se usar a função desc() se quiser de maneira descendente
arrange(flights, year, month, day)
flights %>% arrange(year, month, day)
arrange(flights, desc(arr_delay))




# select() seleciona colunas de maneira esperta
select(flights, year, month, day)
select(flights, year:day)         # note year:day
select(flights, -(year:day))

# "select helpers"
select(flights, starts_with('a'))
select(flights, ends_with('e'))
select(flights, contains('el'))
select(flights, contains('tail'))

# select com expressões regulares
flights %>%
  select(matches('rr|ar')) %>%
  head(2)



# abordagem dplyr
flights %>%
  select(carrier, dep_delay) %>%
  arrange(dep_delay)

# use 'desc' para descendente conjuntamente com pipes
flights %>%
  select(carrier, dep_delay) %>%
  arrange(desc(dep_delay))

# método aninhado para selecionar as colunas carrier e dep_delay e filtrar por atrasos maiores que 60 minutos
filter(select(flights, carrier, dep_delay), dep_delay > 60)

# com pipes (bem melhor em termos de organização de código e clareza de comandos):
flights %>%
  select(carrier, dep_delay) %>%
  filter(dep_delay > 60)


# rename() para renomear colunas
flights
rename(flights, tail_num = tailnum) # Observe que a variável original tá depois do sinal de igual
select(flights, contains('tail'))


# distinct() encontra valores únicos na tabela
distinct(flights, tailnum)

# base::unique() (maneira alternativa)
length(unique(flights$tailnum))


# mutate() para adicionar colunas, permitindo utilizar as colunas recém criadas
flights %>% 
  mutate(gain = arr_delay - dep_delay,
         gain_per_hour = gain / (air_time / 60),
         speed = distance / air_time * 60)


# transmute() mantém apenas as variáveis criadas
flights %>%
transmute(gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60),
          speed = distance / air_time * 60
)


# sumarize(): cria estatísticas de resumo
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))


# sample_n() retira uma amostra para um número fixo
sample_n(flights, 10)                  # sem reposição
sample_n(flights, 10, replace = TRUE)  # com reposisção

# sample_frac() retira uma amostra para uma fração fixa
sample_frac(flights, 0.01)


# group_by(): agrupa a base de acordo com alguma variável categórica
by_tailnum <- group_by(flights, tailnum) # Simplesmente criou um grupo (a rigor, ele não mudou muito)
#by_tailnum <- group_by(flights, tailnum, carrier) # Para múltiplos agrupamentos

# algumas estatísticas por número do avião (frequência, distância  e atraso de chegada)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay

# Outro exemplo:
# abordagem dplyr: crie uma tabela agrupada por dest, e resuma cada grupo calculando a média por arr_delay
flights %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))


# para cada companhia, calculando os atrasos mínimos e máximos nas chegadas e partidas (sumarizando em várias colunas)
flights %>%
  group_by(carrier) %>%
  summarise_at(vars(matches('delay')), .funs = funs(min(., na.rm=TRUE), max(., na.rm=TRUE))) # Só nas colunas que tiver a palavra 'delay'


# para cada dia do ano, contando o total de voos e ordenando de forma descrescente por flight_count
flights %>%
  group_by(month, day) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# mais simples com a função 'tally' (conta observações por grupo e já ordena de maneira descendente)
flights %>%
  group_by(month, day) %>%
  tally(sort = TRUE)


# para cada destino, contando o total de voos e o número de avioões diferentes
flights %>%
  group_by(dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(tailnum))

# para cada destino, mostrando o número de voos por mês
flights %>%
  group_by(dest) %>%
  select(month, dest) %>%
  table() %>%
  head()



# para cada mês, calculando o número de voos e a mudança do mês anterior
flights %>%
  group_by(month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

# mais simples com a função 'tally'
flights %>%
  group_by(month) %>%
  tally() %>%
  mutate(change = n - lag(n))



##################
# A família join #
##################

###
## combinando conjuntos de dados
# https://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html

a <- tibble(
  x1 = LETTERS[1:3], 
  x2 = 1:3
)

b <- tibble(
  x1 = LETTERS[c(1,2,4)], 
  x3 = c(T,F,T)
)

##
# mutating joins

# left_join(), une fazendo correspondência das linhas de b para a
left_join(a, b, by = 'x1')

# right_join(), une fazendo correspondência das linhas de a para b
right_join(a, b, by = 'x1')

# inner_join(), une os dados mantendo apenas as linhas comuns aos dois conjuntos
inner_join(a, b, by = 'x1')

# full_join(), une os dados mantendo todos os valores, todas as linhas
full_join(a, b, by = 'x1')


##
# filtering joins

# semi_join(), todas as linhas em a que tem correspondência com b
semi_join(a, b, by = 'x1')

# anti_join(), todas as linhas em a que não tem correspondência com b
anti_join(a, b, by = 'x1')


# Nota: é possível realziar joins com mais mais de uma variáveis: by = c("Var_A1" = "Var_B1", "Var_A2" = "Var_B2")


##
# operações com conjuntos

y <- tibble(
  x1 = LETTERS[1:3], 
  x2 = 1:3
)

z <- tibble(
  x1 = LETTERS[2:4], 
  x2 = 2:4
)

# intersect(), linhas que aparecem simultaneamente em y e z
intersect(y,z)

# union(), linhas que aparecem em pelo menos um dos conjuntos y e z
union(y, z)

# setdiff(y, z), linhas que aparecem em y mas não em z
setdiff(y, z)


##
# binding

# bind_rows(), acrescenta z a y como novas linhas
bind_rows(y, z)

# bind_cols(y, z), acrescenta z a y como novas colunas
bind_cols(y, z)


###
## tidyr
#

# gather(): reune múltiplas colunas em poucas colunas
# spread(): espalha poucas colunas em múltiplas colunas
# separate(): separa uma variável de acordo com um (ou mais) separador (separadores)
# unite(): une variáveis em uma variável

# http://stackoverflow.com/questions/1181060
# Suponha o comportamento de três ações: X, Y e Z
stocks <- data_frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

# gather(), transforma colunas em linhas
# https://rstudio-pubs-static.s3.amazonaws.com/58498_dd3b603ba4fb4b469bb1c57b5a951c39.html#gather-function
gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time) %>% print(n=30)


# spread(), transforma linhas em colunas
table2
spread(table2, country, count)
spread(table2, year, count)
spread(table2, type, count)



# separate(), separa uma coluna em múltiplas colunas
(sep_time <- separate(stocks, time, c('t1', 't2', 't3')))



# unite(), une múltiplas colunas em uma
unite(sep_time, time2, t1:t3, sep = '-')


##
# para saber mais
browseVignettes(package = c('dplyr', 'tidyr'))



##############
# Exercícios #
##############

# Exercícios baseados em https://github.com/rstudio/master-the-tidyverse

# 1) Instale e carregue o pacote 'babynames' para ter acesso a base de dados 'babynames'. Realize os seguintes exercícios com esta base:

# i) Faça um select que mostre somente a coluna 'n'.
# ii) Faça um filtro em que a proporção (prop) seja maior ou igual a 0,08.
# iii) Faça um filtro de todas as crianças que se chamam "Sea".
# iv) Ordene a base de acordo com as variáveis 'n' e 'prop', nesta ordem, de maneira crescente. Qual a menor frequência (n) que tem na tabela? Cite o nome de um bebê com esta frequência.
# v) Ordene de maneira descrescente a base de acordo com a variável'n'. Cite um nome muito frequente.
# vi) Use group_by(), summarise() e arrange() para mostrar os 10 nomes mais populares. Compute a popularidade como o número total de crianças de um gênero específico de acordo com os nomes.


# 2) Use gather() para reorganizar a tabela table4a (do pacote tidyverse) em três colunas: country, year e cases.

# 3) Use spread() para reorganizar a tabela table2 (do pacote tidyverse) em quatro colunas: country, year, cases e population.

# Usando a base de dados 'who' do tidyverse faça os dois exercícios seguintes:

# i) Agrupe usando a função gather as colunas de 5 a 60 em duas colunas: o primeiro argumento é a coluna chamada "codes" e a segunda coluna chamada "n". Então, selecione somente as variáveis country, year, codes e n.
# ii) Rode o seguinte código abaixo:

#who %>%
#  gather("codes", "n", 5:60) %>%
#  select(-iso2, -iso3) %>%
#  separate(codes, c("new", "type", "sexage"), sep = "_") %>%
#  select(-new) %>%
#  separate(sexage, into = c("sex", "age"), sep = 1)

# Descreva, com suas palavras, o resultado gerado. 

# 4) Usando uma função do pacote 'readr' (do tidyverse), importe a base de dados 'nimbus.csv' para o R.






