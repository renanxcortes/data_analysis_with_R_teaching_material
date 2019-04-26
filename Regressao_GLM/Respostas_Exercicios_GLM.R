# Resposta Ex. 1 #
data(base_limpa)

base_limpa <- base_limpa %>%
  select(Wind, Solar.R) %>%
  na.exclude() # Retira linhas que tem valores faltantes para estimar a regressão

# Definindo os modelos:

linear <-     lm(Wind ~ Solar.R,                         data = base_limpa)  # Linear
quadratico <- lm(Wind ~ Solar.R + I(Solar.R^2),             data = base_limpa)  # Quadrático
cubico <-     lm(Wind ~ Solar.R + I(Solar.R^2) + I(Solar.R^3), data = base_limpa)  # Cúbico

ggplot(base_limpa, aes(x = Solar.R, y = Wind)) +
  geom_point() +
  stat_smooth(method = "lm", col = "darkgray", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), col = "blue", se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), col = "red", se = F)







# Resposta Ex. 2 #

base_limpa <- airquality %>%
              select(Temp, Wind, Solar.R) %>%
              na.exclude() 

modelo1 <- lm(Temp ~ Wind + Solar.R, data = base_limpa)
modelo2 <- lm(Temp ~ Wind + Solar.R + I(Solar.R^2), data = base_limpa)

anova(modelo1, modelo2)

x_grid <- unique(round(seq(min(base_limpa$Wind), max(base_limpa$Wind), l = 3000), 0))
y_grid <- unique(round(seq(min(base_limpa$Solar.R),    max(base_limpa$Solar.R),    l = 3000), 0))
grid   <- expand.grid(x = x_grid, y = y_grid)
z_grid <- coef(modelo2)[1] + coef(modelo2)[2]*grid$x + coef(modelo2)[3]*grid$y + coef(modelo2)[4]*grid$y^2

z_grid_aux <- matrix(z_grid, ncol = length(x_grid), byrow = T)

est <- matrix(NA, nrow = max(y_grid), ncol = max(x_grid))
est[y_grid, x_grid] <- z_grid_aux

# Gráfico interativo (na matriz, colunas são os índices do eixo x, e as linhas do eixo y)
base_limpa %>%
  plot_ly(x = ~Wind, y = ~Solar.R, z = ~Temp) %>% 
  add_surface(x = NULL, y = NULL, z = ~est) %>%
  add_markers()




# Resposta Ex. 3 #
ggplot(gapminder, aes(x = year, y = gdpPercap, group = country)) + geom_line()
# Kuwait






# Resposta Ex. 4 #

gapminder
gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# Nested data -------------------------------------------------------------

by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

# Fit models --------------------------------------------------------------

country_model <- function(df) {
  lm(gdpPercap ~ year1950, data = df)
}

models <- by_country %>%
  mutate(
    model  = data %>% map(country_model)
  )

# Broom -------------------------------------------------------------------

models <- models %>%
  mutate(
    glance  = model %>% map(broom::glance),
    rsq     = glance %>% map_dbl("r.squared"),
    tidy    = model %>% map(broom::tidy),
    augment = model %>% map(broom::augment)
  )

models %>% arrange(desc(rsq))
# França