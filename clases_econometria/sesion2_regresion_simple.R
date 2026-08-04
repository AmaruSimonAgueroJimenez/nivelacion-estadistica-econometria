## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(wooldridge)
library(gapminder)
library(knitr)
library(broom)
library(lmtest)
library(sandwich)
library(modelsummary)
library(plotly)
theme_set(theme_minimal(base_size = 13))
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"; morado <- "#8E44AD"
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}

data("wage1", package = "wooldridge")
modelo <- lm(wage ~ educ, data = wage1)
modelo_log <- lm(lwage ~ educ, data = wage1)
gap2007 <- gapminder %>% filter(year == 2007)


## -----------------------------------------------------------------------------
#| label: img-flujo
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/flujo_econometria.png")


## -----------------------------------------------------------------------------
#| label: img-modelo
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/scatter_regresion.png")


## -----------------------------------------------------------------------------
#| label: datos-wage
data("wage1", package = "wooldridge")
wage1 %>% select(wage, educ, exper) %>% glimpse()


## -----------------------------------------------------------------------------
#| label: code-scatter
#| eval: false
# ggplot(wage1,
#        aes(x = educ, y = wage)) +
#   geom_jitter(width = .2,
#               alpha = .35,
#               color = celeste) +
#   geom_smooth(method = "lm",
#               se = FALSE,
#               color = rojo) +
#   labs(x = "Educación (años)",
#        y = "Salario (USD/hora)")


## -----------------------------------------------------------------------------
#| label: plot-scatter
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(wage1, aes(x = educ, y = wage)) +
    geom_jitter(width = .2, alpha = .35, color = celeste) +
    geom_smooth(method = "lm", se = FALSE, color = rojo) +
    labs(x = "Educación (años)", y = "Salario (USD/hora)")
)


## -----------------------------------------------------------------------------
#| label: mco-manual
b1 <- cov(wage1$educ, wage1$wage) / var(wage1$educ)
b0 <- mean(wage1$wage) - b1 * mean(wage1$educ)
round(c(b0_manual = b0, b1_manual = b1), 4)
round(coef(lm(wage ~ educ, data = wage1)), 4)


## -----------------------------------------------------------------------------
#| label: plot-anatomia
#| echo: false
#| fig-height: 3.2
set.seed(21)
sub <- wage1 %>% slice_sample(n = 70) %>%
  mutate(ajuste = predict(modelo, newdata = .),
         educ_j = educ + runif(n(), -.18, .18))
p <- ggplot(sub) +
  geom_segment(aes(x = educ_j, xend = educ_j, y = wage, yend = ajuste),
               color = rojo, alpha = .6) +
  geom_point(aes(x = educ_j, y = wage), color = celeste, size = 2, alpha = .85) +
  geom_abline(intercept = coef(modelo)[1], slope = coef(modelo)[2],
              color = azul, linewidth = 1) +
  annotate("text", x = 3, y = 20, label = "segmentos rojos = residuos",
           hjust = 0, color = rojo, size = 4.2) +
  annotate("text", x = 15, y = 13.5, label = "recta MCO: valores ajustados", color = azul, size = 4.2) +
  labs(x = "Educación (años)", y = "Salario (USD/hora)",
       title = "Submuestra de 70 trabajadores: observado, ajustado y residuo")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: broom-salida
tidy(modelo) %>% kable(digits = 3)
glance(modelo) %>%
  select(r.squared, sigma, statistic, p.value, nobs) %>%
  kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: code-logwage
#| eval: false
# modelo_log <- lm(lwage ~ educ,
#                  data = wage1)
# round(coef(modelo_log), 3)
# 
# ggplot(wage1,
#        aes(x = educ, y = lwage)) +
#   geom_jitter(width = .2,
#               alpha = .35) +
#   geom_smooth(method = "lm")


## -----------------------------------------------------------------------------
#| label: plot-logwage
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(wage1, aes(x = educ, y = lwage)) +
    geom_jitter(width = .2, alpha = .35, color = celeste) +
    geom_smooth(method = "lm", se = FALSE, color = rojo) +
    labs(x = "Educación (años)", y = "log(salario)")
)


## -----------------------------------------------------------------------------
#| label: plot-formas-gap
#| echo: false
#| fig-height: 3.0
dfor <- bind_rows(
  gap2007 %>% transmute(x = gdpPercap, lifeExp,
                        forma = "Nivel-nivel: vida ~ PIB (R2 = 0.46)"),
  gap2007 %>% transmute(x = log(gdpPercap), lifeExp,
                        forma = "Nivel-log: vida ~ ln(PIB) (R2 = 0.65)")
) %>% mutate(forma = factor(forma, levels = unique(forma)))
p <- ggplot(dfor, aes(x = x, y = lifeExp)) +
  geom_point(alpha = .5, color = celeste, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, color = rojo) +
  facet_wrap(~forma, scales = "free_x") +
  labs(x = "PIB per cápita (USD / log USD)", y = "Esperanza de vida (años)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-r2
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/r_cuadrado.png")


## -----------------------------------------------------------------------------
#| label: plot-descomposicion
#| echo: false
#| fig-height: 3.0
set.seed(5)
xd <- seq(1, 10, length.out = 14)
yd <- 3 + 1.1 * xd + rnorm(14, 0, 2.2)
md <- lm(yd ~ xd)
base <- tibble(x = xd, y = yd, ajuste = fitted(md))
d3 <- bind_rows(
  base %>% transmute(x, y, ystart = mean(y), yend = y, comp = "Total (SST)"),
  base %>% transmute(x, y, ystart = mean(y), yend = ajuste, comp = "Explicada (SSE)"),
  base %>% transmute(x, y, ystart = ajuste, yend = y, comp = "Residual (SSR)")
) %>% mutate(comp = factor(comp, levels = c("Total (SST)", "Explicada (SSE)", "Residual (SSR)")))
p <- ggplot(d3) +
  geom_segment(aes(x = x, xend = x, y = ystart, yend = yend, color = comp),
               linewidth = 1, show.legend = FALSE) +
  geom_point(aes(x = x, y = y), color = "grey35", size = 1.7) +
  geom_abline(intercept = coef(md)[1], slope = coef(md)[2], color = azul, linewidth = .7) +
  geom_hline(yintercept = mean(yd), linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(morado, verde, rojo)) +
  facet_wrap(~comp) +
  labs(x = "x", y = "y")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-se-sim
#| echo: false
#| fig-width: 5.4
#| fig-height: 3.6
#| out.width: "100%"
set.seed(99)
sim_se <- map_dfr(1:40, function(r) {
  map_dfr(c(25, 200), function(n) {
    xs <- runif(n, 0, 10)
    ys <- 2 + 0.5 * xs + rnorm(n, 0, 2)
    f <- lm(ys ~ xs)
    tibble(rep = r, tamano = paste0("n = ", n),
           x = c(0, 10), y = predict(f, tibble(xs = c(0, 10))))
  })
})
sim_se <- sim_se %>% mutate(tamano = factor(tamano, levels = c("n = 25", "n = 200")))
p <- ggplot(sim_se, aes(x = x, y = y, group = rep)) +
  geom_line(alpha = .25, color = celeste) +
  geom_abline(intercept = 2, slope = 0.5, color = rojo, linewidth = 1) +
  facet_wrap(~tamano) +
  labs(x = "x", y = "y", title = "40 rectas MCO, una por muestra")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: infer-t
tidy(modelo, conf.int = TRUE) %>%
  select(term, estimate, std.error, statistic, conf.low, conf.high) %>%
  kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: plot-hetero
#| echo: false
#| fig-height: 3.0
set.seed(8)
xh <- runif(280, 0, 10)
dh <- bind_rows(
  tibble(x = xh, y = 2 + 1.5 * xh + rnorm(280, 0, 2),
         tipo = "Homocedasticidad: Var(u|x) constante"),
  tibble(x = xh, y = 2 + 1.5 * xh + rnorm(280, 0, 0.45 * xh),
         tipo = "Heterocedasticidad: Var(u|x) crece con x")
) %>% mutate(tipo = factor(tipo, levels = unique(tipo)))
p <- ggplot(dh, aes(x = x, y = y)) +
  geom_point(alpha = .35, color = celeste, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, color = rojo) +
  facet_wrap(~tipo) +
  labs(x = "x", y = "y")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: robust-se
modelsummary(list("SE clásicos" = modelo, "SE robustos (HC1)" = modelo),
             vcov = c("classical", "HC1"), fmt = 3,
             output = "markdown", gof_map = c("nobs", "r.squared"))


## -----------------------------------------------------------------------------
#| label: img-diagnostico
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/supuestos_ols.png")


## -----------------------------------------------------------------------------
#| label: plot-anscombe
#| echo: false
#| fig-height: 3.2
ans <- datasets::anscombe %>%
  pivot_longer(everything(), names_to = c(".value", "conjunto"),
               names_pattern = "(.)(.)") %>%
  mutate(conjunto = paste("Conjunto", conjunto))
p <- ggplot(ans, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE,
              color = rojo, linewidth = .8) +
  geom_point(color = celeste, size = 2) +
  facet_wrap(~conjunto, nrow = 1) +
  labs(x = "x", y = "y",
       title = "Cuatro conjuntos, una misma recta MCO: y = 3 + 0.5x")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-bandas
#| echo: false
#| fig-height: 3.1
grid <- tibble(educ = seq(0, 18, by = .5))
b_conf <- as_tibble(predict(modelo, grid, interval = "confidence"))
b_pred <- as_tibble(predict(modelo, grid, interval = "prediction"))
bandas <- grid %>%
  mutate(ajuste = b_conf$fit, conf_lo = b_conf$lwr, conf_hi = b_conf$upr,
         pred_lo = b_pred$lwr, pred_hi = b_pred$upr)
p <- ggplot(bandas, aes(x = educ)) +
  geom_ribbon(aes(ymin = pred_lo, ymax = pred_hi, fill = "Predicción (individuo)"), alpha = .25) +
  geom_ribbon(aes(ymin = conf_lo, ymax = conf_hi, fill = "Confianza (media)"), alpha = .65) +
  geom_line(aes(y = ajuste), color = azul, linewidth = 1) +
  geom_jitter(data = wage1, aes(x = educ, y = wage), width = .15,
              alpha = .12, size = 1, color = "grey30") +
  scale_fill_manual(values = c("Confianza (media)" = naranja,
                               "Predicción (individuo)" = celeste)) +
  labs(x = "Educación (años)", y = "Salario (USD/hora)", fill = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: pred-tabla
nuevos <- tibble(educ = c(8, 12, 16))
bind_cols(
  nuevos,
  as_tibble(predict(modelo, nuevos, interval = "confidence")) %>%
    rename(ajuste = fit, conf_inf = lwr, conf_sup = upr),
  as_tibble(predict(modelo, nuevos, interval = "prediction")) %>%
    select(lwr, upr) %>% rename(pred_inf = lwr, pred_sup = upr)
) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: plot-extrapolacion
#| echo: false
#| fig-height: 3.1
set.seed(31)
x_obs <- runif(60, 1, 6)
y_obs <- 4 + 9 * x_obs - 0.9 * x_obs^2 + rnorm(60, 0, 1.6)
f_lin <- lm(y_obs ~ x_obs)
curvas <- tibble(x = seq(0.5, 12, .2)) %>%
  mutate(`Relación verdadera` = 4 + 9 * x - 0.9 * x^2,
         `Recta MCO extrapolada` = predict(f_lin, tibble(x_obs = x))) %>%
  pivot_longer(-x, names_to = "curva", values_to = "y")
p <- ggplot() +
  annotate("rect", xmin = 6, xmax = 12, ymin = -22, ymax = 42,
           fill = rojo, alpha = .07) +
  geom_point(data = tibble(x = x_obs, y = y_obs), aes(x = x, y = y),
             alpha = .5, color = celeste, size = 1.6) +
  geom_line(data = curvas, aes(x = x, y = y, color = curva, linetype = curva),
            linewidth = 1) +
  scale_color_manual(values = c("Recta MCO extrapolada" = rojo,
                                "Relación verdadera" = azul)) +
  scale_linetype_manual(values = c("Recta MCO extrapolada" = "solid",
                                   "Relación verdadera" = "dashed")) +
  annotate("text", x = 3.3, y = 38, label = "rango observado", color = "grey30", size = 4.2) +
  annotate("text", x = 9.6, y = 38, label = "extrapolación", color = rojo, size = 4.2) +
  labs(x = "Gasto del programa (escala arbitraria)", y = "Impacto", color = NULL, linetype = NULL) +
  theme(legend.position = "top")
interactivo(p)

