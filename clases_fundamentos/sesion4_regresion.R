## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(wooldridge)
library(broom)
library(modelsummary)
library(knitr)
library(plotly)
theme_set(theme_minimal(base_size = 13))
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"
data("wage1")
m1 <- lm(wage ~ educ, data = wage1)
m2 <- lm(wage ~ educ + exper + tenure, data = wage1)
d6 <- tibble(educ = c(8, 10, 12, 12, 14, 16),
             salario = c(4, 5, 6, 7, 8, 9)) %>%
  mutate(ajuste = -1.3 + 0.65 * educ, residuo = salario - ajuste)
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}


## -----------------------------------------------------------------------------
#| label: plot-ancla
#| echo: false
#| fig-height: 2.3
p <- ggplot(wage1, aes(x = educ, y = wage)) +
  geom_jitter(width = 0.25, alpha = 0.35, color = celeste, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, color = rojo, linewidth = 1.1) +
  labs(x = "Educación (años)", y = "Salario por hora (USD)",
       title = "526 trabajadores: la recta MCO resume la nube con pendiente 0.54")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-scatter
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/scatter_regresion.png")


## -----------------------------------------------------------------------------
#| label: plot-condmean
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
set.seed(123)
sim <- tibble(x = rep(seq(8, 16, 2), each = 50)) %>%
  mutate(y = 1 + 0.5 * x + rnorm(n(), 0, 1.1))
medias_c <- sim %>% group_by(x) %>% summarise(y = mean(y))
p <- ggplot(sim, aes(x, y)) +
  geom_jitter(width = 0.25, alpha = 0.25, color = celeste) +
  geom_abline(intercept = 1, slope = 0.5, color = azul, linewidth = 1) +
  geom_point(data = medias_c, color = rojo, size = 3.2) +
  labs(x = "x (años de educación)", y = "y",
       title = "Las medias de y (rojo) caen sobre la recta")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-lm-mano
d6 <- tibble(
  educ = c(8, 10, 12, 12, 14, 16),
  salario = c(4, 5, 6, 7, 8, 9))
coef(lm(salario ~ educ, data = d6))


## -----------------------------------------------------------------------------
#| label: plot-mano
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
p <- ggplot(d6 %>% mutate(ajuste = -1.3 + 0.65 * educ),
            aes(x = educ, y = salario)) +
  geom_segment(aes(xend = educ, yend = ajuste),
               color = naranja, linewidth = 0.9) +
  geom_abline(intercept = -1.3, slope = 0.65,
              color = rojo, linewidth = 1.1) +
  geom_point(size = 3, color = azul) +
  labs(x = "Educación (años)", y = "Salario por hora (USD)",
       title = "Recta MCO y residuos (segmentos verticales)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-geometria
#| echo: false
#| fig-height: 3.2
geo <- bind_rows(
  d6 %>% mutate(yhat = -1.3 + 0.65 * educ, recta = "Recta MCO: SSR = 0.60"),
  d6 %>% mutate(yhat = 1 + 0.40 * educ, recta = "Otra recta (1 + 0.40x): SSR = 6.04")) %>%
  mutate(recta = factor(recta, levels = c("Recta MCO: SSR = 0.60",
                                          "Otra recta (1 + 0.40x): SSR = 6.04")))
p <- ggplot(geo, aes(x = educ, y = salario)) +
  geom_segment(aes(xend = educ, yend = yhat), color = naranja, linewidth = 0.8) +
  geom_line(aes(y = yhat), color = rojo, linewidth = 1) +
  geom_point(size = 2.6, color = azul) +
  facet_wrap(~recta) +
  labs(x = "Educación (años)", y = "Salario por hora (USD)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: lab1-pdf
#| echo: false
#| fig-height: 2.7
d8 <- tibble(x = c(8, 9, 10, 11, 12, 13, 14, 16),
             y = c(4.5, 4.0, 6.0, 5.2, 6.8, 6.4, 8.2, 8.6))
m8 <- lm(y ~ x, data = d8)
d8 <- d8 %>% mutate(yhat = fitted(m8))
p <- ggplot(d8, aes(x, y)) +
  geom_segment(aes(xend = x, yend = yhat), color = naranja, linewidth = 0.9) +
  geom_abline(intercept = coef(m8)[1], slope = coef(m8)[2], color = rojo, linewidth = 1.1) +
  geom_point(color = azul, size = 2.8) +
  labs(x = "Educación (años)", y = "Salario por hora (USD)",
       title = "La recta ganadora: -0.46 + 0.57x, con SSR = 2.49")
p


## -----------------------------------------------------------------------------
#| label: img-residuos
#| echo: false
#| out.width: "96%"
knitr::include_graphics("figuras/residuos_ols.png")


## -----------------------------------------------------------------------------
#| label: img-r2
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/r_cuadrado.png")


## -----------------------------------------------------------------------------
#| label: ej-r2-mano
round(summary(lm(salario ~ educ, d6))$r.squared, 4)


## -----------------------------------------------------------------------------
#| label: plot-descomp
#| echo: false
#| fig-width: 5
#| fig-height: 3.2
#| out.width: "100%"
p <- tibble(
  comp = factor(c("SST (total)", "SSE (explicada)", "SSR (residual)"),
                levels = c("SSR (residual)", "SSE (explicada)", "SST (total)")),
  valor = c(17.5, 16.9, 0.6)) %>%
  ggplot(aes(x = valor, y = comp, fill = comp)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = valor), hjust = -0.2, size = 4.5, color = azul) +
  scale_fill_manual(values = c(naranja, verde, celeste)) +
  scale_x_continuous(limits = c(0, 21)) +
  labs(x = "Suma de cuadrados", y = NULL,
       title = "17.5 = 16.9 + 0.6")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: lab2-pdf
#| echo: false
#| fig-height: 2.7
set.seed(7)
sim2 <- purrr::map_dfr(c(1, 6), function(s) {
  x <- runif(80, 0, 10)
  y <- 2 + 0.5 * x + rnorm(80, 0, s)
  r2 <- summary(lm(y ~ x))$r.squared
  tibble(x, y, panel = sprintf("DE del error = %.0f:  R2 = %.2f", s, r2))
})
p <- ggplot(sim2, aes(x, y)) +
  geom_point(color = celeste, alpha = 0.55, size = 1.5) +
  geom_abline(intercept = 2, slope = 0.5, color = azul, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = rojo, linewidth = 1) +
  facet_wrap(~panel) +
  labs(x = "x", y = "y")
p


## -----------------------------------------------------------------------------
#| label: plot-tdist
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
tt <- tibble(x = seq(-4, 4, 0.02), dens = dt(x, df = 524))
p <- ggplot(tt, aes(x, dens)) +
  geom_area(data = filter(tt, x <= -1.9645), fill = rojo, alpha = 0.55) +
  geom_area(data = filter(tt, x >= 1.9645), fill = rojo, alpha = 0.55) +
  geom_line(color = azul, linewidth = 1) +
  annotate("text", x = -3, y = 0.09, label = "2.5%", color = rojo, size = 4.5) +
  annotate("text", x = 3, y = 0.09, label = "2.5%", color = rojo, size = 4.5) +
  annotate("text", x = 2.6, y = 0.3, label = "t observado = 10.2\n(muy fuera de escala)",
           color = azul, size = 3.6) +
  labs(x = "t", y = "Densidad",
       title = "t con 524 g.l.: rechazo si |t| > 1.96")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-ic-wage
tidy(m1, conf.int = TRUE) %>% filter(term == "educ") %>%
  select(term, estimate, std.error, statistic, conf.low, conf.high) %>% kable(digits = 4)


## -----------------------------------------------------------------------------
#| label: tabla-modelos
#| echo: false
m3 <- lm(wage ~ educ + exper + tenure + female, data = wage1)
modelsummary(list("(1) Salario" = m1, "(2) Salario" = m2, "(3) Salario" = m3),
             output = "markdown", fmt = 3,
             stars = c("+" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             estimate = "{estimate}{stars}",
             coef_rename = c(educ = "Educación (años)", exper = "Experiencia (años)",
                             tenure = "Antigüedad (años)", female = "Mujer (dummy)",
                             "(Intercept)" = "Constante"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))


## -----------------------------------------------------------------------------
#| label: plot-coefplot
#| echo: false
#| fig-width: 5
#| fig-height: 3.3
#| out.width: "100%"
cc <- bind_rows(
  tidy(m1, conf.int = TRUE) %>% filter(term == "educ") %>% mutate(modelo = "(1) corto"),
  tidy(m2, conf.int = TRUE) %>% filter(term == "educ") %>% mutate(modelo = "(2) largo"))
p <- ggplot(cc, aes(x = estimate, y = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = modelo),
               color = celeste, linewidth = 1.2) +
  geom_point(size = 3.4, color = azul) +
  scale_x_continuous(limits = c(0, 0.75)) +
  labs(x = "Coeficiente de educación (USD/hora por año), IC 95%",
       y = NULL, title = "El retorno estimado sube de 0.54 a 0.60")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-dummy
round(coef(lm(wage ~ female, wage1)), 2)


## -----------------------------------------------------------------------------
#| label: plot-dummy
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
dsex <- wage1 %>%
  mutate(sexo = if_else(female == 1, "Mujeres", "Hombres"))
medias_s <- dsex %>% group_by(sexo) %>%
  summarise(wage = mean(wage))
p <- ggplot(dsex, aes(x = sexo, y = wage)) +
  geom_jitter(width = 0.18, alpha = 0.25, color = celeste, size = 1.4) +
  geom_point(data = medias_s, color = rojo, size = 4) +
  annotate("text", x = 1, y = 9.6, label = "media = 7.10", color = rojo, size = 4.2) +
  annotate("text", x = 2, y = 7.1, label = "media = 4.59", color = rojo, size = 4.2) +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = NULL, y = "Salario por hora (USD)",
       title = "La dummy estima la brecha de medias: -2.51")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-log
tidy(lm(log(wage) ~ educ, wage1)) %>%
  select(term, estimate, std.error) %>%
  kable(digits = 4)


## -----------------------------------------------------------------------------
#| label: plot-logfit
#| echo: false
#| fig-width: 5
#| fig-height: 3.5
#| out.width: "100%"
p <- ggplot(wage1, aes(x = educ, y = log(wage))) +
  geom_jitter(width = 0.25, alpha = 0.3, color = celeste, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = rojo, linewidth = 1.1) +
  labs(x = "Educación (años)", y = "log(salario por hora)",
       title = "En logs, la pendiente es un cambio porcentual")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-supuestos
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/supuestos_ols.png")

