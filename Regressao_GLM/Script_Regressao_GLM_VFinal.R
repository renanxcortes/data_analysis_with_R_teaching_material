################################################
### Modelagem Estatística -  Regressão e GLM ###
###    Disciplina: Análise de Dados com R    ###
###           Renan Xavier Cortes            ###
###          renanxcortes@gmail.com          ###
################################################

library(needs)
needs(tidyverse)
needs(magrittr)  # 'pipe' de atribuição %<>%
needs(corrplot)  # Função corrplot
needs(plotly)    # Função plot_ly (gráficos interativos)
needs(GGally)    # Função ggpairs
needs(lmtest)    # Função coeftest
needs(sandwich)  # Estimador sandwich de erros robustos
needs(gapminder) # Base de dados do gapminder

#setwd("") # Caminho de arquivos

############################
# Regressão Linear Simples #
############################
help(airquality)
data(airquality)

head(airquality)

plot(airquality[,-c(5,6)])

corrMatrix <- cor(na.omit(airquality[,-c(5,6)]))
corrMatrix
corrplot(corrMatrix, method = "ellipse")


ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point() # Note a tendência decrescente

# Estabelecendo uma regressão linear simples da temperatura sendo explicada pelo vento:
modelo_linear <- lm(Temp ~ Wind, data = airquality)
summary(modelo_linear)

# O que está guardado neste objeto? Vamos ver a sua estrutura.
str(modelo_linear)

modelo_linear$fitted.values # Valores ajustados
modelo_linear$residuals # Resíduos

par(mfrow=c(2,2)) # Dividindo a tela gráfica em 2x2
plot(modelo_linear) # Checagem dos resíduos

influence.measures(modelo_linear) # Observe as estatísticas de diagnóstico (Ex.: matriz de alavancagem H)

par(mfrow=c(1,1))

# Plotando a reta estimada
# ggplot2
ggplot(airquality, aes(x = Wind, y = Temp)) +
  geom_point() +
  geom_smooth(method = "lm") # "se = FALSE" para retirar o intervalo de confiança da média.

# Base R
plot(airquality$Wind, airquality$Temp)
abline(a = modelo_linear$coefficient[1], b = modelo_linear$coefficient[2], col = "blue")


# Predições
predict(modelo_linear, interval = "confidence") # Intervalo de 95% (Ajustado, Inferior e Superior)

auxiliar <- data.frame(Wind = seq(min(airquality$Wind), max(airquality$Wind), 0.01))

# Intervalo de confiança para resposta média
lim_inf <- predict(modelo_linear, auxiliar, interval = "confidence")[,2]
lim_sup <- predict(modelo_linear, auxiliar, interval = "confidence")[,3]

matlines(auxiliar,cbind(lim_inf, lim_sup),lty=c(2,2), col=c(2,2))

# Intervalo de confiança para previsão
lim_inf <- predict(modelo_linear, auxiliar, interval = "prediction")[,2]
lim_sup <- predict(modelo_linear, auxiliar, interval = "prediction")[,3]

matlines(auxiliar,cbind(lim_inf, lim_sup),lty=c(3,3), col=c(3,3))
legend("topright", title="Linhas:", c("Confiança","Valor Predito","Predição"), lty = c(2,1,3), col = c(2,1,3), bty = "n") # bty: border type



# Observação: para um modelo sem intercepto faça: modelo <- lm(y ~ x - 1)




########################
# Regressão Polinomial #
########################
plot(airquality[,-c(5, 6)])

ggplot(airquality, aes(x = Temp, y = Ozone)) +
  geom_point() # Observe o comportamento não-linear!

base_limpa <- airquality %>%
              select(Temp, Ozone) %>%
              na.exclude() # Retira linhas que tem valores faltantes para estimar a regressão

# Definindo os modelos:

linear <-     lm(Ozone ~ Temp,                         data = base_limpa)  # Linear
quadratico <- lm(Ozone ~ Temp + I(Temp^2),             data = base_limpa)  # Quadrático
cubico <-     lm(Ozone ~ Temp + I(Temp^2) + I(Temp^3), data = base_limpa)  # Cúbico

# Plotando os modelos pelo ggplot2:
ggplot(base_limpa, aes(x = Temp, y = Ozone)) +
  geom_point() +
  stat_smooth(method = "lm", col = "darkgray", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), col = "blue", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), col = "red", se = F)

anova(linear, quadratico, cubico) # Testando qual o melhor grau dentre os três!

# Conclusão: A adição do termo de grau 2 se justifica, no entanto de grau 3 não se justifica!






######################
# Regressão Múltipla #
######################
help(USArrests) # Índice de Crimes por estado Norte-Americano
data(USArrests) 
plot(USArrests)

corMatrix_Arrests <- cor(USArrests)
corrplot(corMatrix_Arrests, method = "ellipse")

# Note que a correlação linear entre assault (assaltos com violência) é o mais correlacionado linearmente com murder (homicídios)

# Homicídios sendo explicado pelos assaltos:
modelo1 <- lm(Murder ~ Assault, data = USArrests)
summary(modelo1)
plot(modelo1) # Diagnóstico

# Ok! Beleza, modelo estimado... mas e se eu quiser incrementar o meu modelo e adicionar mais
# variáveis para explicar os homicídios como, por exemplo, estupro?

modelo2 <- lm(Murder ~ Assault + Rape, data = USArrests)

summary(modelo2)
plot(modelo2) # Diagnóstico

# Gráfico de superfície

x_grid <- unique(round(seq(min(USArrests$Assault), max(USArrests$Assault), l = 3000), 0))
y_grid <- unique(round(seq(min(USArrests$Rape),    max(USArrests$Rape),    l = 3000), 0))
grid   <- expand.grid(x = x_grid, y = y_grid)
z_grid <- coef(modelo2)[1] + coef(modelo2)[2]*grid$x + coef(modelo2)[3]*grid$y

z_grid_aux <- matrix(z_grid, ncol = length(x_grid), byrow = T)

est <- matrix(NA, nrow = max(y_grid), ncol = max(x_grid))
est[y_grid, x_grid] <- z_grid_aux

# Gráfico interativo (na matriz, colunas são os índices do eixo x, e as linhas do eixo y)
USArrests %>%
  plot_ly(x = ~Assault, y = ~Rape, z = ~Murder) %>% 
  add_surface(x = NULL, y = NULL, z = ~est) %>%
  add_markers()
  

# Comparando os modelos:
anova(modelo1, modelo2) # Método de teste de modelos aninhados (H0: modelo "menor" vs. H1: modelo "completo")

# Conclusão: a inclusão da variável de estupro náo melhora substancialmente o poder preditivo do modelo univariado



# Adicionando covariáveis categóricas na regressão
USArrests %<>% # 'pipe' atribucional
  mutate(Tipo = ifelse(UrbanPop > 80, "Urbana", "Rural"))

ggplot(USArrests, aes(x = Murder, y = Rape, col = Tipo)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = F) # Note a diferença entre as regressão nos diferentes tipos de cidade



modelo1  <- lm(Rape ~ Murder, data = USArrests)
modelo2 <-  lm(Rape ~ Tipo, data = USArrests)
modelo_completo <- lm(Rape ~ Murder + Tipo, data = USArrests)

summary(modelo1)
summary(modelo2)         # Ser Urbano aumenta a média do índice de estupro em aprox. 4.5 unidades de medida
summary(modelo_completo) # O valor do coeficiente do Tipo não se alterou muito em magnitude
anova(modelo1, modelo_completo) # Comparando os dois modelos

# A inclusão desta covariável não é significativa a 5% de significância.


# Exemplos de modelos:
attach(USArrests)
lm(UrbanPop ~ Rape)
lm(UrbanPop ~ Rape - 1)                     # Modelo sem o intercepto
lm(UrbanPop ~ Rape + Assault)
lm(UrbanPop ~ Rape + Assault + Rape:Assault)
lm(UrbanPop ~ Rape*Assault)                 # Igual ao anterior
lm(UrbanPop ~ Rape*Assault*Murder)          # Com todas interações de menor grau
lm(UrbanPop ~ Rape*Assault + Murder)
lm(UrbanPop ~ Rape*Assault + Murder + I(Murder^2))
detach(USArrests)




#############################
# GLM - Regressão Logística #
#############################

################
# Preliminares #
################

base_preliminar <- tibble(var_dicotomica = rep(c(0,1), c(50, 50)),
                          covariavel     = c(rnorm(50, 3), rnorm(50, 5)))

# Plotando os dados com uma linha de tendência e uma regressão linear (não apropriado)
ggplot(data = base_preliminar, aes(x = covariavel, y = var_dicotomica)) + 
  geom_jitter(height = 0.05, width = 0.1) +
  stat_smooth(fill = 'pink', color = 'red') +
  geom_smooth(method = "lm")

# Plotando os dados com um modelo logístico
ggplot(data = base_preliminar, aes(x = covariavel, y = var_dicotomica)) + 
  geom_jitter(height = 0.05, width = 0.1) +
  stat_smooth(method = 'glm',
              method.args = list(family = "binomial"))

###########################
# Fim das Preliminares :) #
###########################


data("SwissLabor", package = "AER")
help(SwissLabor)
head(SwissLabor)

ggpairs(SwissLabor)  # Rápida Análise Descritiva de toda a base
ggpairs(SwissLabor, ggplot2::aes(colour = participation))  # Colorindo pela variável dependente

# Vamos tentar relacionar o impacto de algumas variáveis na participação do mercado de trabalho
modelo <- glm(participation ~ education + age + foreign, family = binomial(link = "logit"), data = SwissLabor)
summary(modelo)

# ?family para ver as distribuições do glm

# IMPORTANTE: O R ASSUME QUE "NO" É CATEGORIA DE REFERÊNCIA (DENOMINADOR DENTRO DA FUNÇÃO DE LIGAÇÃO), ENTÃO ELE ESTÁ REALMENTE FAZENDO P(Y = 1) ou P(ser participante)
# Caso você queira checar, crie a variável aux <- ifelse(SwissLabor$participation == "yes", 1, 0) e verifique que os resultados são equivalentes.

# Interprete os coeficientes.

confint(modelo)                               # 95% CI for the coefficients
exp(coef(modelo))                             # Exponencial dos coeficientes
exp(confint(modelo))                          # Intervalo de confiança de 95% para o exponencial dos coeficientes
predict(modelo, type="response")              # Valores preditos (Probabilidade de sucesso)
exp(predict(modelo))/(1+exp(predict(modelo))) # Maneira alternativa pela fórmula do logit
residuals(modelo, type="deviance")            # Resíduos

# Comparando com modelos mais parcimoniosos
modelo_1 <- glm(participation ~ age + foreign, family = binomial(link = "logit"), data = SwissLabor)
modelo_2 <- glm(participation ~ education + foreign, family = binomial(link = "logit"), data = SwissLabor)
modelo_3 <- glm(participation ~ education + age, family = binomial(link = "logit"), data = SwissLabor)

anova(modelo_1, modelo, test="Chisq") # (H0: modelo "menor" vs. H1: modelo "completo")
anova(modelo_2, modelo, test="Chisq")
anova(modelo_3, modelo, test="Chisq")

summary(modelo_1) # Superior

# Interprete os coeficientes.



#######################
# GLM - Modelo Probit #
#######################
modelo_logit <- glm(participation ~ education + age + foreign, family = binomial(link = "logit"), data = SwissLabor)
modelo_probit <- glm(participation ~ education + age + foreign, family = binomial(link = "probit"), data = SwissLabor)

# Observe as diferenças
summary(modelo_logit)
summary(modelo_probit)
plot(predict(modelo_logit, type="response"), predict(modelo_probit, type="response"), xlab = "Logit", ylab= "Probit", main = "Probabilidades Estimadas")


par(mfrow=c(2,1))
plot(sort(predict(modelo_probit, type="response")), ylab="Probabilidade")
plot(sort(predict(modelo_probit, type="response")), cex=0.2, ylim=c(-0.1, 1.1), ylab="Probabilidade")
abline(h=c(0,1), lty=2, col="red")

# Modelo teórico
windows()
x <- seq(-3, 3, 0.01)
plot(x, pnorm(x), cex=0.2)


# Os efeitos marginais não são mais diretos em termos de razão de chances.
# A interpretação do efeito é a variação do eixo horizontal na distribuição acumulada da normal.





##############################
# GLM - Regressão de Poisson #
##############################
# Referência: arquivo countreg.pdf (Regression Models for Count Data in R)
# Disponível também em https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
# Descrição das variáveis na página 9

# Objetivo: modelar o número de visitas ao consultório médico (ofp)

load("DebTrivedi.rda")
head(DebTrivedi)

dt <- DebTrivedi %>% select(ofp, hosp, health, numchron, gender, school, privins)

# ofp      - número de visitas ao consultório médico
# hosp     - número de internações hospitalares
# health   - grau de auto-percepção de status de saúde
# numchron - número de condições crônicas
# gender   - sexo
# school   - anos de estudo
# privins  - se a pessoa tem seguro privado de saúde ou não

ggplot(dt, aes(x = ofp)) + geom_bar() # A variação é substancial e a quantidade de zeros é bem grande também

ggpairs(dt)

# Gráficos contra os regressores mais "personalizados":
ggplot(dt, aes(x = ofp, y = hosp)) + geom_jitter(width = 0.1, height = 0.15)
ggplot(dt, aes(x = hosp, fill = health)) + geom_bar(position = "dodge") # Ou ggplot(dt, aes(x = hosp)) + geom_bar() + facet_wrap(~health)
ggplot(dt, aes(x = numchron, y = hosp)) + geom_jitter(width = 0.2)
ggplot(dt, aes(x = hosp, fill = gender)) + geom_bar(position = "dodge") # Ou ggplot(dt, aes(x = hosp)) + geom_bar() + facet_wrap(~gender)
ggplot(dt, aes(x = school, y = hosp)) + geom_jitter(width = 0.2) + geom_smooth()
ggplot(dt, aes(x = hosp, fill = privins)) + geom_bar(position = "dodge")

# Regressão de Poisson contra todas as variáveis:
fm_pois <- glm(ofp ~ ., data = dt, family = poisson)
summary(fm_pois)

# Note que a categoria "average" é a referência para a variável "health".

# Interpretações: 
# i)  Todos os coeficientes são altamente significativos.
# ii) O teste realizado para cada coeficiente é o teste de Wald, que pode ser "otimista", pois existe evidência de sobredispersão dos dados.

# Refazendo os testes dos coeficientes usando erros robustos mais apropriados:
coeftest(fm_pois, vcov = sandwich)

# Resultado: as significância permanecem, mas com menor intensidade.
# Neste caso, os erros padrões são mais apropriados.



###################################
# GLM - Regressão de Quasipoisson #
###################################
# Hipótese da distribuição de Poisson: se X ~ Poisson(lambda), então E(X) = lambda e Var(X) = lambda
# Parâmetro de dispersão pode ser visto como o grau de discrepância entre a média e a variância estimada: https://stats.stackexchange.com/questions/62006/definition-of-dispersion-parameter-for-quasipoisson-family

mean(dt$ofp)
var(dt$ofp)

# A regressão de Quasipoisson permite a inclusão da estimativa deste termo de sobredispersão (e com excesso de zero "Zero-Inflated"):
fm_qpois <- glm(ofp ~ ., data = dt, family = quasipoisson)
summary(fm_qpois)

# A estimativa do parâmetro de sobredispersão é 6,7 (muito maior que 1) indicando presença de sobredispersão nos dados.
# As conclusões dos coeficientes são análogas às anteriores (note que eles são os mesmos).



########################
# Múltiplas Regressões #
########################

# Referência: "Hadley Wickham: Managing many models with R" (https://www.youtube.com/watch?v=rz3_FDVt9eg)
# Código baseado em https://gist.github.com/hadley/056cf4074acedc164161d6abb751cb35
# Combinando o pacote broom (tidy models) com o pacote purrr (functional programming)

# Gráfico motivador
ggplot(gapminder, aes(x = year, y = lifeExp, group = country)) + geom_line()

# Tendência ascendente em linhas gerais, mas alguns países possuem tendência decrescente e mais erráticos.

# Nested Data: tidyr
# Functional Programming: purrr (alternativa a "for" loops)
# Models: tidy data (broom)


gapminder
gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# Nested data -------------------------------------------------------------

by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

by_country
str(by_country)      # Não muito útil
by_country$data[[1]] # Todo o subconjunto de dados do Afeganistão
by_country[1, ]
by_country$data[[2]] # Todo o subconjunto de dados da Albânia

# Fit models --------------------------------------------------------------

country_model <- function(df) {
  lm(lifeExp ~ year1950, data = df)
}

models <- by_country %>%
  mutate(
    model  = data %>% map(country_model)
  )

models
models %>% filter(continent == "Africa")

# Broom -------------------------------------------------------------------

models <- models %>%
  mutate(
    glance  = model %>% map(broom::glance),
    rsq     = glance %>% map_dbl("r.squared"),
    tidy    = model %>% map(broom::tidy),
    augment = model %>% map(broom::augment)
  )
models

models %>% arrange(desc(rsq))
models %>% filter(continent == "Africa")

models %>%
  ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point(aes(colour = continent))

source("gapminder-shiny.R") # Exemplo de aplicativo que mostra os países individualmente para níveis de R quadrado.

# Unnest ------------------------------------------------------------------

models
unnest(models, data)                 # Base Original
unnest(models, glance, .drop = TRUE) # Metadados de cada modelo (o argumento .drop = T retira as colunas dos subconjuntos de dados)
unnest(models, tidy)                 # Dados dos coeficientes das regressões

# Plotando os interceptos e os coeficientes angulares de todos os modelos conjuntamente
models %>%
  unnest(tidy) %>%
  select(continent, country, term, estimate, rsq) %>%
  spread(term, estimate) %>%
  ggplot(aes(`(Intercept)`, year1950)) +
  geom_point(aes(colour = continent, size = rsq)) +
  geom_smooth(se = FALSE) +
  xlab("Life Expectancy (1950)") +
  ylab("Yearly improvement") +
  scale_size_area()
# Note que os pequenos pontos que representam coeficientes angulares negativos possuem ajustes "ruins" (R-quadrado baixos).
# Efeito "catching-up" de países que começaram com expectativa de vida menor no início do período, pois estão aumentando mais rápido.

# Dados de cada ponto da regressão (resíduos, valores ajustados, etc.)
unnest(models, augment)

# Plotando todos os resíduos e separando por continente
models %>%
  unnest(augment) %>%
  ggplot(aes(year1950, .resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_hline(yintercept = 0, colour = 'white', size = 1.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(~continent)
# Os ajustes da África não parecem muito apropriados...


# Plot de resumo geral dos modelos ----------------------------------------

summary <- models %>%
  transmute(
    continent,
    country,
    slope = model %>% map(coef) %>% map_dbl(2),
    rsq = glance %>% map_dbl("r.squared")
  )

summary %>%
  ggplot(aes(rsq, slope)) +
  geom_point(aes(colour = continent)) +
  xlab(quote(R ^ 2)) +
  ylab("Estimated yearly increase in life expectancy") +
  theme(legend.position = "top", aspect.ratio = 1)







# Exercícios
# 1) Realize regressões polinomiais entre as variáveis Wind (y) e Solar.R (x) da base airquality até o polinômio de grau 3. 
# Qual é o polinômio mais apropriado? Adicionalmente, plote o gráfico com os 3 polinômios estimados usando o ggplot2.

# 2) Crie duas regressões múltiplas usando a base airquality:
# Modelo 1) Temp ~ Wind + Solar.R
# Modelo 2) Temp ~ Wind + Solar.R + Solar.R^2
# A adição do termo quadrático do modelo 2 foi significativa? Se sim, plote em um gráfico 3D usando o plotly. Qual o efeito visual deste termo na superfície gerada?

# 3) Faça um gráfico com todas as séries temporais da base gapminder com o PIB per capita (variável gdpPercap). Qual é o país que possui uma evolução que destoa completamente dos demais?

# 4) Faça um modelo linear para cada país entre ano (x = year) explicando o PIB per capita (y = gdpPercap). Qual o país que melhor se ajusta aos dados em termos de R-quadrado?

