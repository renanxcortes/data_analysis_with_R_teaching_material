################################################
###  Probabilidade e Inferência Estatística  ###
###    Disciplina: Análise de Dados com R    ###
###           Renan Xavier Cortes            ###
###          renanxcortes@gmail.com          ###
################################################

library(needs)
needs(tidyverse)
needs(infer)     # Função rep_sample_n
needs(nortest)   # Testes de aderência à normalidade
needs(car)       # Teste de Levene para homogeneidade de variâncias na ANOVA
needs(broom)     # Função tidy
needs(agricolae) # Função HSD.test


#setwd("") # Caminho de arquivos

# Probabilidade e Inferência Estatística


################################
# Fundamentos de Probabilidade #
################################

# Funções base (exemplificando com a distribuição normal)
# rnorm() - simulação
# dnorm() - valor de densidade
# pnorm() - probabilidade à esquerda
# qnorm() - valor do quantil da distribuição


# A probabilidade é um ramo da estatística importante pois permite que se estude os possíveis processos geradores de dados da vida real.

#########################
# Distribuição binomial #
#########################

rbinom(1, 1, 0.5) # Experimento de jogar uma moeda honesta (este é o caso particular da distribuição Bernoulli)

# Vamos estabelecer que 1 é cara e 0 é coroa.

rbinom(10, 1, 0.5) # "Jogando" a moeda 10 vezes.
rbinom(10, 10, 0.5) # Jogando 10 vezes, 10 moedas e contando quantas vezes deu "cara" em cada um dos 10 experimentos (Binomial(10, 0.5))
rbinom(10, 10, 0.8) # Fazendo a mesma coisa anterior, porém com uma moeda viesada para a face "cara"

# Densidade e Densidade Cumulativa
# Se X ~ Binomial(10, 0.5), qual P(X = 5)?
amostra <- rbinom(10000, 10, 0.5)
mean(amostra == 5) # Valor estimado pela simulação
dbinom(5, 10, 0.5) # Valor Exato

# E P(X <= 4)?
mean(amostra <= 4) # Valor estimado pela simulação
pbinom(4, 10, 0.5) # Valor Exato

barplot(table(amostra)) # Gráfico das simulação de uma Binomial(10, 0.5)


# Valor Esperado e Variância
# Suponha X ~ Binomial(n, p). Então, E(X) = np e Var(X) = np(1-p)
sample <- rbinom(10000, 20, 0.7)
mean(sample)
var(sample)

20 * 0.7             # Valor esperado exato
20 * 0.7 * (1 - 0.7) # Variância Exata


#########################
# A distribuição normal #
#########################

# A distribuição binomial pode ser aproximada pela distribuição Normal, principalmente quando n é grande.
# Se X ~ Binomial(n, p), então a distribuição Y ~ Normal(mu, sigma) pode ser aproximada por mu = np e sigma = sqrt(np(1-p))

n <- 1000
p <- 0.5

binomial <- rbinom(100000, n, p)

valor_esperado <- n * p
variancia      <- n * p * (1 - p)
desvio_pad     <- sqrt(variancia)

normal <- rnorm(100000, valor_esperado, desvio_pad)

# Histograma da binomial com a densidade da normal teórica

# Base R
hist(binomial, prob = T, ylim = c(0, 0.03))
curve(dnorm(x, valor_esperado, desvio_pad), add = TRUE, col = "red", lwd = 3)

# ggplot2
df <- data.frame(Valor = binomial)
ggplot(df, aes(x = Valor)) +
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, 
                args = list(mean = valor_esperado, 
                            sd = desvio_pad), 
                colour = 'red', size = 1)


# Comparando os histogramas simulados

compare_histograms <- function(variable1, variable2) {
  x <- data.frame(value = variable1, variable = "Variable 1")
  y <- data.frame(value = variable2, variable = "Variable 2")
  ggplot(rbind(x, y), aes(value)) +
    geom_histogram() +
    facet_wrap(~ variable, nrow = 2)
}

compare_histograms(binomial, normal)



# Comparando as probabilidades simuladas e teóricas

# Simulações de uma distribuição normal e binomial
binom_sample <- rbinom(100000, 1000, .2)
normal_sample <- rnorm(100000, 200, sqrt(160))

# Qual a probabilidade de ocorrer 190 caras ou menos utilizando os valores simulados da binomial?
mean(binom_sample <= 190)

# Qual a probabilidade de ocorrer 190 caras ou menos utilizando os valores simulados da normal?
mean(normal_sample <= 190)

# Qual a probabilidade de ocorrer 190 caras ou menos utilizando o valor exato da binomial?
pbinom(190, 1000, 0.2)

# Qual a probabilidade de ocorrer 190 caras ou menos utilizando o valor exato da normal?
pnorm(190, 200, sqrt(160))




# Importante: esta comparação não é boa quando n é "pequeno" (por exemplo, n = 10):
binom_sample <- rbinom(100000, 10, 0.2)
normal_sample <- rnorm(100000, mean = 10 * 0.2, sd = sqrt(10 * 0.2 * 0.8))
compare_histograms(binom_sample, normal_sample)




#############################
# A distribuição de Poisson #
#############################

# A distribuição de Poisson pode ser vista como a contagem discreta sobre um intervalo contínuo. Por exemplo, o número de carros que passa em um pedágio da free-way em um intervalo de uma hora.
# Ademais, ela pode ser vista como um caso limite de uma binomial em que n vai para infinito e p vai para zero.

# Olhe esta distribuição:
binomial <- rbinom(100000, 1000, 1/1000)
hist(binomial) # Ela não possui formato de "sino" e nem é simétrica.

# Simulando a Poisson equivalente. Se X ~ Poisson(lambda), então E(X) = lambda e Var(X) = lambda
poisson <- rpois(100000, 1)
hist(poisson)

# Poisson com diferentes formatos:

m <- 150000

lambda_01 <- rpois(m, 0.1)
lambda_1  <- rpois(m, 1)
lambda_3  <- rpois(m, 3)
lambda_10 <- rpois(m, 10)

# Detalhe: estas simulações poderiam ser otimizadas com a função map2 do pacote 'purrr'.

df <- data.frame(Valor = c(lambda_01, lambda_1, lambda_3, lambda_10), Dist = rep(c("1) lambda = 0.1", "2) lambda = 1", "3) lambda = 3", "4) lambda = 10"), each = m))
ggplot(df, aes(x = Valor)) +
  geom_histogram(binwidth = 0.3) +
  facet_wrap(~Dist)



# Comparando as formas de diferentes distribuições

# Normal
ggplot(data.frame(x = c(-4, 10)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='red') +
  stat_function(fun = dnorm, args = list(mean = 1, sd = .5), col='blue') +
  stat_function(fun = dnorm, args = list(mean = 3, sd = 3), col='green')

# Student-t
ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dt, args = list(df = 5), col='red') +
  stat_function(fun = dt, args = list(df = 30), col='blue') +
  stat_function(fun = dt, args = list(df = 100), col='green')

# Qui-Quadrado
ggplot(data.frame(x = c(0, 100)), aes(x)) + 
  stat_function(fun = dchisq, args = list(df = 5), col='red') +
  stat_function(fun = dchisq, args = list(df = 10), col='blue') +
  stat_function(fun = dchisq, args = list(df = 30), col='green')

# Binomial

m <- 150000
binom_1 <- rbinom(m, 5, 0.1)
binom_2 <- rbinom(m, 10, 0.2)
binom_3 <- rbinom(m, 30, 0.7)

# Detalhe: poderiam ser otimizados com o paocte 'purrr': pmap(list(rep(150000, 3), c(5, 10, 30), c(0.1, 0.2, 0.7)), rbinom)

df <- data.frame(Valor = c(binom_1, binom_2, binom_3), Dist = rep(c("n = 5 e p = 0.1", "n = 10 e p = 0.2", "n = 30 e p = 0.7"), each = m))
ggplot(df, aes(x = Valor)) +
  geom_histogram(binwidth = 0.3) +
  facet_wrap(~Dist)















#############################
# Fundamentos de Inferência #
#############################

# Referência desta parte: https://www.datacamp.com/courses/foundations-of-inference

# Exemplo: discriminação de gênero no mercado de trabalho.
# Paper: Influence of sex roles stereotypes on personnel decisions. Journal of Applied Psychology. Rosen, B. and Jeredee, T. H. (1974)
# 48 currículos IGUAIS foram randomizados para 48 supervisores de instituições financeiras.
# 24 deles estavam com nomes de homens e 24 com nomes de mulheres.
# Pergunta para cada um deles: Este currículo merece ser promovido para gerente de agência?
# Hipótese: existe discriminação de gênero?
# Hipótese nula (H0): não há diferença de proporções entre homens e mulheres.

disc <- data.frame(promote = rep(c("promoted", "not promoted"), c(35, 13)),
                   sex = rep(c("male", "female", "male", "female"), c(21, 14, 3, 10)))

disc %>%
  group_by(sex) %>%
  summarize(promoted_prop = mean(promote == "promoted"))

# Essa diferença de proporções é plausível em um cenário em que homens e mulheres são igualmente qualificados para a promoção?


# Criando um data.frame com as diferenças das promoções
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted"))  %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female


# Histograma de diferenças permutadas. 
# O valor observado (linha vermelha) está localizado em um valor extremo da distribuição que supõe ausência de relação entre sexo e promoções?
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), col = "red")

# Ou é possível fazer um gráfico de pontos
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_dotplot(dotsize = 0.25) +
  geom_vline(aes(xintercept = diff_orig), col = "red")


# O gráfico anterior pode se rvisto como a distribuição de probabilidade da diferença entre as proporções.
# Podemos comparar o valor estimado com os quantis das diferenças sob a hipótese nula.
# Quantis à direita
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.9),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))

# Quantis à esquerda
disc_perm %>% 
  summarize(q.01 = quantile(diff_perm, p = 0.01),
            q.05 = quantile(diff_perm, p = 0.05),
            q.10 = quantile(diff_perm, p = 0.1))

# Usualmente, se utiliza um valor crítico de 5% que pode ser visto como um grau "ceticismo" do analista.
# Este valor é chamado de Nível de Significância.



# Valor-p: o grau de quanto a amostra observada difere da hipótese nula.
# DEFINIÇÃO: É A PROBABILIDADE DE SE OBSERVAR DADOS TÃO OU MAIS EXTREMOS DO QUE NÓS REALMENTE COLETAMOS SUPONDO A HIPÓTESE NULA VERDADEIRA.

# Neste exemplo: é a probabilidade de se observar uma diferença de 0,2917 ou maior, supondo que as taxas de promoções sejam as mesmas.

# Calculando o valor-p dos dados (Hipótese alternativa de que homens tem maior probabilidade de ser promovidos)
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))


# Valor-p bilateral (Hipótese alternativa de que homens tem probabilidade diferente de mulheres)
disc_perm %>%
  summarize(2 * mean(diff_orig <= diff_perm))




# Intervalos de Confiança e Bootstrapping

################
# Preliminares #
################

# Importante saber o conceito do Teorema Central do Limite
# Olhar o código TCL_no_R.R

########################
# Fim das Preliminares #
########################



# Estimadores por Intervalos de Confiança dão uma noção de variabilidade de uma estimativa pontual.
# Neste caso, ter uma ideia do erro padrão da estimativa é imprescindível e, nestes casos, o bootstrap pode auxiliar na estimativa de erro padrão.

all_polls <- readRDS("all_polls.rds")

# Esta base de dados é de pesquisas de opinião com relação a disposição a votar em um candidato X em uma eleição.

# Neste exemplo, a base de dados é populacional, mas vamos supor que a nossa amostra é, somente, a primeira pesquisa.


# Selecionando uma pesquisa para fazer o bootstrap: one_poll
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)

# Retirando 1000 amostras da one_poll com mesmo tamanho da amostra original (n = 30): one_poll_boot_30
one_poll_boot_30 <- one_poll %>%
  rep_sample_n(30, replace = T, reps = 1000) # Amostragem com reposição!

# Computar o p estimado de cada submostra do primeiro caso
ex30_props <- one_poll_boot_30 %>% 
  summarize(prop_yes = mean(vote == 1))


# Estimativa da variabilidade de proporção via bootstrap
ex30_props %>% summarize(sd(prop_yes))

# Pela distribuição Bernoulli/Binomial, a variância da proporção é sqrt(p*(1-p)/n).

# Importante: o tamanho da subamostra deve ser igual ao da amostra original.



# Construindo um intervalo de confiança via bootstrap para a proporção

# Computando o p estimado
p_hat <- mean(one_poll$vote)

# Bootstrap para encontrar o erro padrão de p_hat: one_poll_boot
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote == 1))

# Criando um intervalo de valores plausíveis
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot)) # O valor 2 é um valor aproximado que permite que ele tenha alta confiança de conter o verdadeiro valor de proporção da população.


# Comparando o intervalo anterior com o intervalo obtido via os quantis da distribuição
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Interpretação: estamos 95% de confiança que a verdadeira proporção de pessoas que pretendem votar no candidato X está entre o limite inferior e o limite superior. 









###################################
# Inferência para dados numéricos #
###################################

base_idese <- readRDS("base_idese_2018.rds")
corresp    <- readRDS("Corresp_Mun_PopRS.rds") %>%
  mutate(CodIBGE = as.character(CodIBGE))

# Códigos IBGE de municípios da Região Metropolitana de Porto Alegre (RMPA)
cods_rmpa <- c(4300604,4300877,4301107,4303103,4303905,4304606,4304689,4305355,4306403,4306767,4307609,4307708,4309050,4309209,4309308,4310108,4310801,4312401,4313060,4313375,4313409,4314050,4314803,4314902,4316006,4317608,4318408,4318705,4319505,4319901,4320008,4321204,4322004,4323002)


# Índice de Desenvolvimento Sócio Econômico (Idese) com todas regionalizações e série história até 2015

# Data Wrangling
# Pegando somente um tipo de regionalização e de um ano específico: filter
# Organizando a base: spread
# Incluindo uma coluna nova: mutate
# Adicionando informações de regionalização por município: join

idese_mun_rec <- base_idese %>%
  filter(TIPO_UNID == "Municípios",
         ANO == 2015,
         CATEGORIA %in% c("Idese", "Bloco Educação", "Bloco Renda", "Bloco Saúde")) %>%
  spread(CATEGORIA, VALOR) %>%
  mutate(RMPA = as.factor(ifelse(COD %in% cods_rmpa, "Pertence", "Não Pertence"))) %>%
  inner_join(corresp, by = c("COD" = "CodIBGE"))

head(idese_mun_rec) # Primeiras Linhas
dim(idese_mun_rec)  # Dimensões

# Intervalo de Confiança de 95% para o valor do Idese dos Municípios do RS em 2015
t.test(idese_mun_rec$Idese, conf.level = 0.95) # Ignore o valor do teste, neste momento

# O mesmo Intervalo feito "à mão":
mu     <- mean(idese_mun_rec$Idese)
ep     <- sd(idese_mun_rec$Idese) / sqrt((length(idese_mun_rec$Idese) - 1))
t_crit <- qt(0.975, df = (length(idese_mun_rec$Idese) - 1))

lim_inf <- mu - t_crit * ep
lim_sup <- mu + t_crit * ep


# Testes de Hipóteses para dados numéricos

# H0 (hipótese nula):        o valor médio do Idese dos municípios em 2017 é igual 0,74.
# H1 (hipótese alternativa): o valor médio do Idese dos municípios em 2017 é diferente de 0,74.

# Variância desconhecida: teste-t

t.test(idese_mun_rec$Idese, mu = 0.74, conf.level = 0.95)

# Valor-p > 5%, logo aceita-se H0!

# Em regra geral, em um teste de hipótese para uma média, testar qualquer valor dentro do intervalo de confiança resultará em aceitar H0.

# Hipótese a ser testada para validar o teste: os dados do Idese são "normais"?
# H0: os dados são normais
# H1: os dados não são normais

shapiro.test(idese_mun_rec$Idese) # Teste de Shapiro-Wilk

ad.test(idese_mun_rec$Idese)      # Teste de Anderson-Darling
cvm.test(idese_mun_rec$Idese)     # Teste de Cramer Von-Mises
lillie.test(idese_mun_rec$Idese)  # Teste de Lilliefors (Kolmogorov-Smirnov)
sf.test(idese_mun_rec$Idese)      # Teste de Shapiro-Francia


# Teste-t para duas médias: existem diferenças entre os Ideses de municípios da RMPA e dos que não são?
idese_mun_rec %>%
  group_by(RMPA) %>%
  summarize(Media =  mean(Idese),
            Mediana = quantile(Idese, 0.5),
            Desvio = sd(Idese),
            IQR = IQR(Idese),
            Minimo = min(Idese),
            Maximo = max(Idese),
            n = n())

ggplot(idese_mun_rec, aes(x = Idese, fill = RMPA)) +
  geom_density(alpha = 0.3)

ggplot(idese_mun_rec, aes(x = RMPA, y = Idese)) +
  geom_boxplot()

# Supondo heterogeneidade de variâncias
t.test(Idese ~ RMPA, data = idese_mun_rec) # Maneira alternativa t.test(x, y)


# Hipótese preliminar do teste: as variâncias são iguais?
# H0: as variâncias são iguais
# H1: as variâncias não são iguais
var.test(Idese ~ RMPA, data = idese_mun_rec) # Teste F!

bartlett.test(Idese ~ RMPA, data = idese_mun_rec) # Método Alternativo 1 (Teste de Bartlett)
fligner.test(Idese ~ RMPA, data = idese_mun_rec)  # Método Alternativo 2 (Teste de Fligner)


# Supondo homogeneidade de variâncias
t.test(Idese ~ RMPA, data = idese_mun_rec, var.equal = TRUE)


# Teste não-paramétrico equivalente ao teste t (teste para mediana): Wilcox/U-Mann-Whitney
wilcox.test(Idese ~ RMPA, data = idese_mun_rec)



# Análise de Variância (ANOVA) para comparação de múltiplas médias

# H0: as médias dos grupos são iguais
# H1: pelo menos dois grupos diferem em média

ggplot(idese_mun_rec, aes(x = Idese, fill = Meso)) +
  geom_density(alpha = 0.3)

ggplot(idese_mun_rec, aes(x = Meso, y = Idese)) +
  geom_boxplot()

ggplot(idese_mun_rec, aes(x = Meso, y = Idese)) +
  geom_jitter(width = 0.15) # Gráfico mais apropriado para ver que o Noroeste Rio-Grandense é mais denso em quantidade de municípios

idese_mun_rec %>%
  group_by(Meso) %>%
  summarize(Media =  mean(Idese),
            Mediana = quantile(Idese, 0.5),
            Desvio = sd(Idese),
            IQR = IQR(Idese),
            Minimo = min(Idese),
            Maximo = max(Idese),
            n = n())

# Gráfico alternativo de médias: também
# plot.design(idese_mun_rec$Idese ~ idese_mun_rec$Meso, main = "Médias das Regiões", cex = 0.5)

modelo <- aov(Idese ~ Meso, data = idese_mun_rec) # Estima o modelo diretamente usando a função 'aov'
summary(modelo) # Resumo o modelo estimado (tabela de ANOVA)
plot(modelo)


# Comparações múltiplas post-hoc (quando se realiza múltiplos testes, o Erro Tipo I (alpha) é inflado, então uma correção nos valores-p devem ser realizada).
pairwise.t.test(idese_mun_rec$Idese, idese_mun_rec$Meso) %>% tidy()
TukeyHSD(modelo)

# Uma maneira de não ajustar o valor-p é o argumento p.adjust = "none", mas neste caso, devemos analisar a significância global divida pelo número de combinações de pares. Ou seja, alpha/C(7,2).

# Note que o número de combinações é Combinação de Sete dois-a-dois: choose(7, 2) = 21

# Plotando o teste de Tukey
post_hoc <- HSD.test(modelo, trt = "Meso")
plot(post_hoc, cex.names = 0.45, main = "Agrupamentos gerados para cada região")



# Exercícios:
# 1) Qual o valor da densidade do ponto X = 0, em uma Normal com média 1 e desvio padrão 5?

# 2) Qual a probabilidade de X > 1.6 em um Normal com média 0 e desvio padrão 0.7?

# 3) Faça 3 histogramas em gráficos separados simulando 10.000 valores com as seguintes especificações:
# - Simulando uma Normal com média 10 e desvio 1
# - Simulando uma Binomial com n = 100 e p = 0.1
# - Simulando uma Poisson com parâmetro lambda = 10

# 4) Realize um teste-t (e interprete o resultado para significância de 5%) para duas amostras 
# supondo homogeneidade de variâncias para a variável "Bloco Renda" do Idese comparando municípios 
# que estão na RMPA (Grupo 1) com os que não estão (Grupo 2).
















