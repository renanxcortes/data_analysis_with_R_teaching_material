##########################################
###         Fundamentos de R           ###
### Disciplina: Análise de Dados com R ###
###         Renan Xavier Cortes        ###
###       renanxcortes@gmail.com       ###
##########################################

# Este código tem como base um material construído por Filipe Zabala.
# Obrigado pela gentileza de permitir a utilização destes códigos!

### Referências
# http://r4ds.had.co.nz/
# https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
# https://www.rstudio.com/wp-content/uploads/2016/10/r-cheat-sheet-3.pdf

# ## Links para a instalação do R e do RStudio
# https://cloud.r-project.org/
# https://www.rstudio.com/products/rstudio/download/preview/

# Recomenda-se sempre ter a versão mais atualizada do R.



########################
### Fundamentos de R ###
########################
  
# Instalando o pacote 'needs' (rodar uma vez)
install.packages('needs', dep = T)
library(needs)

# Instalando e atualizando pacotes (rodar uma vez)
needs(MASS)         # Para a função fractions()
needs(pracma)       # Função nthroot(x,n)
needs(rgl)          # Pacote para 'demo(rgl)'
needs(tidyverse)    # carrega os pacotes ggplot2, tibble, tidyr, readr, purrr e dplyr, dentre outros
needs(nycflights13) # dados de voos
needs(gapminder)    # dados de desenvolvimento mundial
needs(Lahman)       # dados de baseball
needs(sp)           # mapas
needs(lubridate)    # datas
# update.packages(ask = F)


# Demonstração de algumas funcionalidades
demo()              # Lista todos as demonstrações disponíveis, mesmo em pacotes não carregados
demo(persp)         # Gráficos 3D
demo(rgl)           # {rgl} Gráficos 3D mais flexíveis
demo(colors)        # Cores por nome
demo(Hershey)       # Tabelas de caracteres
demo(plotmath)      # Notação matemática


# Detalhes da distribuição
licence()
RShowDoc('COPYING') # GNU Version 2, June 1991
RShowDoc('GPL-3')   # GNU Version 3, June 2007


# Contribuidores
contributors()


# Citação
citation()          # R
citation('dplyr')   # pacotes


# Ajuda e documentação
help()              # Ajuda
help.start()        # Ajuda em html
help(mean)          # Chama a ajuda para a função 'mean'
?mean               # Equivalente a 'help(mean)'
help.search('mean') # Busca por tópicos contendo 'mean'
??mean              # Equivalente a 'help.search('mean')'
example(mean)       # Roda os exemplos da documentação de 'mean'
apropos('mean')     # Encontra funções que contenham 'mean'


## Comandos básicos ##

# Operações
2+5             # Adição
sum(2,5)        # Adição através da função 'sum'
2-5             # Subtração
2*5             # Multiplicação
2/5             # Divisão
2^5             # Potenciação (ou poderia ser 2**5)
sqrt(16)        # Raiz quadrada (ou ainda 16^(1/2))
16^(1/4)        # Raiz quarta de 16

# Alternativo
pracma::nthroot(16,2)   # {pracma} Raiz n-ésima (n=2)
pracma::nthroot(16,4)   # {pracma} Raiz n-ésima (n=4)

# Prioridade das operações: os parênteses possuem prioridade (calcule mentalmente antes de rodar)
# Potenciação/Radiciação > Multiplicação/Divisão > Soma/Subtração
1+2*5 
(1+2)*5
1/2*5^3
1/(2*5)^3



# Sequências regulares
1:100
seq(1, 100, by = 1)
seq(1, 100, by = 2)
100:1
-100:1
-(100:1)

rep(1:4, times = 2)         # repete a sequência 1:4 duas vezes
rep(1:4, length.out = 10)   # limita o numero de observacoes
rep(1:4, each = 2)          # repete cada elemento duas vezes

rep(c(1,5,3), c(5,2,10))    # Repete cada elemento do primeiro vetor de acordo com as frequências do segundo vetor 

# A função 'c' será vista mais detalhes adiante.


# Atribuições
(x <- 2)
(3 -> z)
y = 4
4 = y # Erro
x*y+z

# Funções matemáticas
pi
cos(c(0,30,45,60)*pi/180)
MASS::fractions(cos(c(0,30,45,60)*pi/180)) # Dá a fração aproximada


# Arredondamento
round(153.456789,3) # Arredonda para 3 decimais
for(i in 6:-2){ print(round(153.456789,i)) }
options(digits = 9) # Ajusta apresentação para 9 dígitos (padrão é 7)
for(i in 6:-2){ print(round(153.456789,i)) }

# Looping

# for
for(i in 1:10) print(i)
for(i in seq(1,10,2)) print(i)

# celsius para farenheit
for(celsius in 20:30){
  print( c(celsius, round(1.8 * celsius + 32, 0)) )
}
  
# while
x <- 0
while(x < 5) {x <- x + 1; print(x)}

x <- 0
while(x < 5) {x <- x + 1; if (x == 3) break; print(x)}  # pára quando x == 3

x <- 0
while(x < 5) {x <- x + 1; if (x == 3) next; print(x)}   #  omite x == 3


# Alternativo (primeiro exemplo colocando a atualização do indexador no final da sentença):
x <- 1
while(x <= 5) {print(x)
               x <- x + 1}


# Valores lógicos
2 < 5            # TRUE ou T
2 > 5            # FALSE ou F
1 == 1           # Igualdade simbolizada por ==
1 != 1           # Diferença simbolizada por !=
(1==1) & (1==2)  # Intersecção/E simbolizado por & ou &&
(1==1) | (1==2)  # União/OU simbolizado por |
!(1==1)          # Negação simbolizada por !
as.numeric(c(T,F,F,T,T,T,T,F,T))  # conversão para numérico
sum(c(T,F,F,T,T,T,T,F,T))
?Logic  # ajuda de 'Logical Operators'


# Fatores
factor(letters[1:10])                           # os fatores são categorias

as.numeric(factor(101:103))                     # atenção! ('The R Inferno', pg 82)
as.numeric(as.character(factor(101:103)))       # Possível solução

ff <- factor(c('AA', 'BA', 'CA'))   # ff, fator de 3 níveis
ff
ff[1:2]             # mesmo com o filtro o nível 'CA' ainda está lá
ff[1:2, drop=TRUE]  # solução 1, usando drop
factor(ff[1:2])     # solução 2, transformando f[1:2] em um novo fator


# Valores faltantes (NA/missing values)
(z1 <- c(1:3, NA))    # NA é um símbolo especial
(z2 <- c(1:3, NB))    # erro, pois 'NB' não é símbolo especial
is.na(z1)
mean(z1)
mean(z1, na.rm = T)


# Diretório de trabalho (working directory)
getwd()        # Apresenta o diretório corrente
setwd('H:/')   # Ajusta o diretório corrente
dir()          # Apresenta o conteúdo do diretório corrente

# Manipulando objetos
x <- 2
class(x)   # Apresenta a classe de 'x'
rm(x)      # Remove o objeto
objects()  # Listando objetos
save.image(file = 'aula1.RData') # Salvando área de trabalho (todos os objetos atuais)

# Arquivos .rds (objetos genéricos que facilitam a leitura posterior)
saveRDS(x, "Objeto_x.rds")
x_lido <- readRDS("Objeto_x.rds")

mapa <- readRDS("MapaRS.rds") # Arquivo .rds criado previamente a partir de um arquivo .shp
class(mapa)

plot(mapa)


# Vetores
(v <- c(2,0,1,2,4,2))   # Atribui o vetor (2,0,1,2,4,2) a 'v' e apresenta 'v'
2*v
v^2
v[3]
v[-3]
v[c(2,5)]
length(v)

(x <- 1:10)           # gera uma sequência regular de 1 a 10
class(x)              # 'integer'
1/x                   # novo vetor, não guardado na memória
class(1/x)            # ponto flutuante
(y <- x^2)            # gerando outras variáveis de exemplo
(z <- 2*x+y+1)

(v1 <- c(x,y,z))      # 'c' concatena os vetores 'x', 'y' e 'z'
length(v1)            # utiliza-se 'length' para vetores e listas

e1 <- vector()        # atribuindo um objeto 'vazio'
e2 <- numeric()       # equivalente a 'vector()'
e1[3] <- 17; e1       # o objeto assume a dimensão da posição fornecida
e2[10] <- 10; e2


# Matrizes
(m1 <- cbind(x,y,z))    # Cola os vetores como colunas, transformando em uma matriz 10x3
dim(m1)                 # Utiliza-se 'dim' para matrizes e data frames
(m2 <- rbind(x,y,z))    # Cola os vetores como linhas, transformando em uma matriz 3x10
dim(m2)

t(m2)                 # transposta
all.equal(m1,t(m2))   # compara os objetos
identical(m1,t(m2))   # mais restritiva que 'all.equal'

# Outro Exemplo
identical(as.double(8), as.integer(8)) # FALSE
all.equal(as.double(8), as.integer(8)) # TRUE
as.double(8) == as.integer(8) # TRUE

class(m1)               # 'cbind' e 'rbind' geram objetos da classe 'matrix'
class(m2)

rownames(m1) <- paste0('Linha ', 1:10)    # ?paste
m1
colnames(m2) <- paste0(LETTERS[1:10])     # ?letters
m2

(m3 <- matrix(1:16, nrow=4, ncol=4))            # distribui os valores por coluna
is.matrix(m3)
(m4 <- matrix(1:20, nrow=4, ncol=5, byrow=T))   # distribui os valores por linha
(m5 <- matrix(0, nrow=5, ncol=7))               # repete o valor fornecido em todas as celulas

m1 %*% m2             # o produto de matrizes é feito com %*%
crossprod(t(m1),m2)   # t(x) %*% y, mais rápido que %*%
m2 %*% m1
crossprod(m1,t(m2))


# Matriz identidade de n-ésima ordem: eye(n)
eye(5) # Matriz identidade 5x5



# Array
(a1 <- array(data = 1:30, dim = c(2,5,3)))
dim(a1)
a1[1,3,2]
a1[,,1]
a1[,1,]
a1[1,,]


# Listas
l1 <- list(matriz1 = m1, matriz2 = m2, char = c('aa', 'bb'), array1 = a1)
l1
is.list(l1)
l1$matriz2    # $ para chamar pelo nome
l1[[2]]       # [[ ]] para chamar pela posicao
class(l1$matriz2)
diag(l1$matriz2)
class(l1$char)
l1$char[1]
names(l1)


# Data frame
data(iris) # 50 medidas de pétalas e sépalas de 3 tipos de flores: help(iris)
dim(iris)
head(iris)
summary(iris)
str(iris)
dplyr::glimpse(iris)

class(iris)
iris.matrix <- as.matrix(iris)
class(iris.matrix)

iris.matrix # Note que para uma matriz, todos os elementos devem ser da mesma classe
as.matrix(iris[,-5]) # Matriz numérica

# Para digitar diretamente um data.frame no R:
dados <- data.frame(Nome = c("João", "Maria", "José"), 
                    Nota = c(7.5, 8.0, 9.3))


# Funções úteis

(x <- 1:10)
rev(x)          # reverte o vetor
cumsum(x)       # soma acumulada
cumsum(rev(x))  # soma acumulada inversa

(x <- c(3:5, 11:8, 8 + 0:5))
(ux <- unique(x))           # apresenta uma lista de valores distintos
length(x)                   # 13 elementos, contando as repeticoes
length(unique(x))           # 9 elementos únicos
duplicated(x)               # indica as posicoes onde há duplicações

(tab <- table(duplicated(x)))     # cria uma tabela
prop.table(tab)                   # percentuais

match(1:10, c(1,9,8,4))     # retorna um vetor das posições do primeiro objeto que corresponde com o segundo
1:10 %in% c(1,9,8,4)        # retorna TRUE onde há correspondência e FALSE caso contrário

(r1 <- rank(x1 <- c(3,1,4,15,92)))    # indica a ordem de cada elemento do vetor
x2 <- c(3,1,4,1,5,9,2,6,5,3,5)
names(x2) <- letters[1:length(x2)]
(r2 <- rank(x2))                          # ties.method = c('average', 'first', 'last', 'random', 'max', 'min')

sample(1:60, size=6)                      # amostra pseudo-aleatoria para jogar na mega-sena
set.seed(20); sample(1:60, size=6)        # ajustando a semente pseudo-aleatoria
set.seed(20); sort(sample(1:60, size=6),decreasing=T)  # ordenando, para facilitar a leitura

letters                       # letras minusculas
LETTERS                       # letras maiusculas
LETTERS[1]
noquote(letters)              # sem aspas

substr('abcdef', 2, 4)        # apresenta da segunda ate a quarta posição

x <- c('Tche', 'Churrasco', 'Chimarrao', 'Farrapo', 'Gaita', 'Bah')
strsplit(x, 'a')              # retira a letra 'a'

tolower(x)    # minusculas
toupper(x)    # maiusculas

texto <- 'Rio Grande do Sul'
nchar(texto)    # conta o número de caracteres, contando os espaços
stringi::stri_stats_latex(texto)  # relatório mais completo

x <- c(2,4,-2,6,7,8)
diff(x)           # tira a diferenca, note que restam n-1 elementos
range(x)          # apresenta o mínimo e o máximo
diff(range(x))    # tira a diferenca entre o mínimo e o máximo (amplitude)


# Lidando com Datas no R
as.Date('11/3/1981', format = '%d/%m/%Y')   # coloca no formato de data...
as.Date('3:11:1981', format = "%m:%d:%Y")   # ... não importa o separador
(data <- as.Date('11/3/1981', format='%d/%m/%Y'))
class(data)       # classe Date
months(data)      # mês
weekdays(data)    # dia da semana
quarters(data)    # trimestre
Sys.Date() - data # número de dias entre hoje e data

# Maneira mais moderna de se lidar com data no R (pacote lubridate):
data1 <- c("12-4-2011", "15-12-2010")
data2 <- c("4/12/2011", "12/15/2010")
data3 <- c("12_2011_4", "15_2010_12")

dmy(data1)
mdy(data2)
dym(data3)

(m <- cbind(x = 1:10, y = (-4:5)^2))    
split(m, col(m))      # divide uma matriz em uma lista pelas colunas, gerando uma lista
split(1:20, 1:2)      # divide a sequencia de 1 a 10 de dois em dois valores (pares/ímpares)

(x <- rep(1:3,3))
car::recode(x, " c(1,2) = 'A'; else = 'B' ")  # recodifica


###################
# Criando funções #
###################

mediaEdesvio <- function(x = rnorm(10)) {   # Note o nome da função e os argumentos que ela requer!
  media = mean(x)
  desvio = sd(x)
  c("Média" = media, "Desvio Padrão" = desvio) # Valores que a função irá retornar!
}

mediaEdesvio()
vetor <- c(3,2,5,4,6,4,7,5,6,8,11)
mediaEdesvio(vetor)

funcao <- function(x=2, y=3) { 
  x^2 + sqrt(y) 
}

funcao()
funcao(x=3, y=8)
funcao(x=0, y=25)
funcao(0, 0)
funcao(1, 2)

funcao <- function(x=2, y=3) { # Note que esta função não retornar?á nada!
  z <- x^3 + sin(y)
}

funcao(3,1)
valor <- funcao(6,5)
valor # Agora sim.

funcao <- function(x=2, y=3) { 
  return(x^3 + sin(y)) # Agora sim com este comando.
}

funcao(x=3, y=1)
funcao(y=3, x=1)



# Exercícios:
# 1) Calcule no R a seguinte soma: 1 + 1/2 + 1/3 + 1/4 + ... + 1/100

# 2) Construa um loop que mostre na tela os 10 primeiros elementos do exercício anterior.

# 3) Calcule a média de cada uma das 4 colunas numéricas do banco de dados iris.

# 4) Construa dois vetores: 
# i) O primeiro deve conter as palavras "Grêmio", "Inter", "Juventude" e "Caxias", sendo TODAS as letras minúsculas. 
# ii) O segundo vetor deve ser igual ao primeiro, mas com TODAS as letras maiúsculas.

# 5) Crie uma função que tenha 3 valores de entrada e que retorne a multiplicação dos dois primeiros valores e depois subtraia o terceiro valor.
# Em outras palavras: f(x,y,z) = x * y - z
# Calcule a função para os valores, respectivamente, 36, 74 e 14.

