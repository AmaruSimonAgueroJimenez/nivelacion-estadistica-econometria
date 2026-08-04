## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(knitr)
library(broom)
library(plotly)
library(car)
library(lmtest)
library(sandwich)
library(modelsummary)
theme_set(theme_minimal(base_size = 13))
options(knitr.kable.NA = "")
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"; morado <- "#8E44AD"
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}

# Datos simulados: mercado laboral (educacion, experiencia, genero)
# DGP conocido -> permite evaluar cada especificacion contra la "verdad"
set.seed(42)
n <- 200
datos <- tibble(
  educacion = pmax(rnorm(n, mean = 13, sd = 3), 8),
  experiencia = pmax(rnorm(n, mean = 15, sd = 10), 0),
  mujer = rbinom(n, size = 1, prob = 0.5),
  log_ingreso = 9 + 0.15 * educacion + 0.08 * experiencia +
    0.25 * mujer - 0.04 * educacion * mujer + rnorm(n, sd = 0.3),
  ingreso = exp(log_ingreso),
  ingreso_miles = ingreso / 1000,
  genero = factor(mujer, levels = c(0, 1), labels = c("Hombre", "Mujer"))
)


## -----------------------------------------------------------------------------
#| label: tabla-datos
#| echo: false
datos %>%
  select(educacion, experiencia, genero, log_ingreso, ingreso) %>%
  head(4) %>%
  mutate(ingreso = round(ingreso)) %>%
  kable(digits = 1)


## -----------------------------------------------------------------------------
#| label: img-ovb-dag
#| echo: false
#| out.width: "96%"
knitr::include_graphics("figuras/sesgo_variable_omitida.png")


## -----------------------------------------------------------------------------
#| label: sim-ovb
set.seed(123); n_s <- 300
sim <- tibble(
  habilidad = rnorm(n_s),
  educ = 12 + 1.5 * habilidad + rnorm(n_s, 0, 1.5),
  log_sal = 8 + 0.08 * educ + 0.35 * habilidad + rnorm(n_s, 0, 0.25))
corto <- lm(log_sal ~ educ, data = sim)
largo <- lm(log_sal ~ educ + habilidad, data = sim)


## -----------------------------------------------------------------------------
#| label: tabla-ovb
#| echo: false
tibble(
  Modelo = c("Verdadero (DGP)", "Corto: omite habilidad", "Largo: controla habilidad"),
  `Coef. educación` = c(0.080, coef(corto)["educ"], coef(largo)["educ"])
) %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: code-ovb-scatter
#| eval: false
# b <- coef(largo)
# ggplot(sim,
#        aes(educ, log_sal)) +
#   geom_point(aes(color = habilidad),
#              alpha = .8) +
#   geom_smooth(method = "lm",
#               se = FALSE,
#               color = rojo) +
#   geom_abline(intercept = b[1],
#               slope = b[2],
#               color = verde,
#               linewidth = 1.1) +
#   scale_color_gradient(
#     low = "#F9E79F", high = azul)


## -----------------------------------------------------------------------------
#| label: plot-ovb-scatter
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
b <- coef(largo)
interactivo(
  ggplot(sim, aes(educ, log_sal)) +
    geom_point(aes(color = habilidad), alpha = .8, size = 1.6) +
    geom_smooth(method = "lm", se = FALSE, color = rojo, linewidth = 1.1) +
    geom_abline(intercept = b[1], slope = b[2],
                color = verde, linewidth = 1.1) +
    scale_color_gradient(low = "#F9E79F", high = azul) +
    labs(x = "Años de educación", y = "log(salario)",
         color = "Habilidad")
)


## -----------------------------------------------------------------------------
#| label: img-dag
#| echo: false
#| out.width: "80%"
knitr::include_graphics("figuras/dag_tres_tipos.png")


## -----------------------------------------------------------------------------
#| label: plot-berkson
#| echo: false
#| fig-width: 5.4
#| fig-height: 3.5
#| out.width: "100%"
set.seed(7)
beca <- tibble(
  merito = rnorm(400),
  vulnerabilidad = rnorm(400),
  estado = ifelse(merito + vulnerabilidad + rnorm(400, 0, .4) > 1,
                  "Becado", "No becado"))
p <- ggplot(beca, aes(merito, vulnerabilidad, color = estado)) +
  geom_point(alpha = .6, size = 1.5) +
  geom_smooth(data = filter(beca, estado == "Becado"),
              method = "lm", se = FALSE, color = rojo, linewidth = 1.1) +
  scale_color_manual(values = c("Becado" = naranja, "No becado" = "grey65")) +
  labs(x = "Mérito académico (z)", y = "Vulnerabilidad (z)",
       color = NULL, title = "Analizar solo becados inventa una correlación") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-dummy
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/variables_dummy.png")


## -----------------------------------------------------------------------------
#| label: mod-dummy
m3 <- lm(log_ingreso ~ educacion + experiencia + mujer, data = datos)
tidy(m3) %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: img-interaccion
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/interaccion.png")


## -----------------------------------------------------------------------------
#| label: mod-interaccion
m4 <- lm(log_ingreso ~ educacion * mujer + experiencia, data = datos)
tidy(m4) %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: code-rectas
#| eval: false
# grid <- expand_grid(
#   educacion = seq(8, 20, .5),
#   mujer = c(0, 1),
#   experiencia = 15)
# pred <- bind_rows(
#   mutate(grid,
#     m = "Solo dummy",
#     y = predict(m3, grid)),
#   mutate(grid,
#     m = "Con interacción",
#     y = predict(m4, grid)))
# ggplot(pred,
#        aes(educacion, y,
#            color = factor(mujer))) +
#   geom_line(linewidth = 1.1) +
#   facet_wrap(~ m)


## -----------------------------------------------------------------------------
#| label: plot-rectas
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
grid <- expand_grid(educacion = seq(8, 20, .5), mujer = c(0, 1),
                    experiencia = 15)
pred <- bind_rows(
  mutate(grid, m = "Solo dummy: paralelas", y = predict(m3, grid)),
  mutate(grid, m = "Con interacción: divergen", y = predict(m4, grid))) %>%
  mutate(m = factor(m, levels = c("Solo dummy: paralelas",
                                  "Con interacción: divergen")))
interactivo(
  ggplot(pred, aes(educacion, y, color = factor(mujer, labels = c("Hombre", "Mujer")))) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ m) +
    scale_x_continuous(breaks = seq(8, 20, 4)) +
    scale_color_manual(values = c(Hombre = celeste, Mujer = rojo)) +
    labs(x = "Años de educación", y = "log(ingreso) predicho", color = NULL) +
    theme(legend.position = "top")
)


## -----------------------------------------------------------------------------
#| label: mod-anidados
m1 <- lm(log_ingreso ~ educacion, data = datos)
m2 <- lm(log_ingreso ~ educacion + experiencia, data = datos)
list(M1 = m1, M2 = m2, M3 = m3, M4 = m4) %>%
  map_dfr(glance, .id = "Modelo") %>%
  select(Modelo, r.squared, adj.r.squared, AIC) %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: test-f
anova(m3, m4) %>% tidy() %>%
  mutate(Modelo = c("M3 (restringido)", "M4 (+ interacción)")) %>%
  select(Modelo, df.residual, rss, statistic, p.value) %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: img-anidados
#| echo: false
#| out.width: "58%"
knitr::include_graphics("figuras/modelos_anidados.png")


## -----------------------------------------------------------------------------
#| label: code-coefs
#| eval: false
# coefs <- list(M1 = m1, M2 = m2,
#               M3 = m3, M4 = m4) %>%
#   map_dfr(
#     ~ tidy(.x, conf.int = TRUE),
#     .id = "modelo") %>%
#   filter(term == "educacion")
# ggplot(coefs,
#        aes(modelo, estimate)) +
#   geom_pointrange(
#     aes(ymin = conf.low,
#         ymax = conf.high),
#     color = azul) +
#   geom_hline(yintercept = 0.15,
#              linetype = "dashed",
#              color = rojo)


## -----------------------------------------------------------------------------
#| label: plot-coefs
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
coefs <- list(M1 = m1, M2 = m2, M3 = m3, M4 = m4) %>%
  map_dfr(~ tidy(.x, conf.int = TRUE), .id = "modelo") %>%
  filter(term == "educacion")
interactivo(
  ggplot(coefs, aes(modelo, estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                    color = azul, linewidth = .9, size = .55) +
    geom_hline(yintercept = 0.15, linetype = "dashed", color = rojo) +
    labs(x = NULL, y = "Coeficiente de educación (IC 95%)")
)


## -----------------------------------------------------------------------------
#| label: tabla-modelsummary
#| echo: false
modelsummary(
  list("M1" = m1, "M3" = m3, "M4" = m4),
  output = "markdown", stars = TRUE,
  coef_map = c(educacion = "Educación", experiencia = "Experiencia",
               mujer = "Mujer", "educacion:mujer" = "Educación × Mujer"),
  gof_map = c("nobs", "r.squared", "adj.r.squared"))


## -----------------------------------------------------------------------------
#| label: vif-calc
vif(m3)
set.seed(99)
datos2 <- datos %>% mutate(edad = 6 + educacion + experiencia + rnorm(n, 0, 2))
vif(lm(log_ingreso ~ educacion + experiencia + edad + mujer, data = datos2))


## -----------------------------------------------------------------------------
#| label: img-vif
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/vif_multicolinealidad.png")


## -----------------------------------------------------------------------------
#| label: plot-diagnosticos
#| echo: false
#| fig-height: 3.2
std <- rstandard(m3)
ddf <- bind_rows(
  tibble(x = fitted(m3), y = residuals(m3), panel = "1 · Residuos vs ajustados"),
  tibble(x = qnorm(ppoints(length(std))), y = sort(std), panel = "2 · Q-Q normal"),
  tibble(x = fitted(m3), y = sqrt(abs(std)), panel = "3 · Escala-localización"))
refs_h <- tibble(panel = "1 · Residuos vs ajustados", yint = 0)
refs_qq <- tibble(panel = "2 · Q-Q normal", x = c(-2.8, 2.8), y = c(-2.8, 2.8))
p <- ggplot(ddf, aes(x, y)) +
  geom_point(alpha = .5, color = celeste, size = 1.2) +
  geom_hline(data = refs_h, aes(yintercept = yint),
             color = rojo, linetype = "dashed") +
  geom_line(data = refs_qq, aes(x, y), color = rojo, linetype = "dashed") +
  facet_wrap(~ panel, scales = "free") +
  labs(x = NULL, y = NULL)
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-heteroced
#| echo: false
#| fig-height: 3.2
m_niv <- lm(ingreso_miles ~ educacion + experiencia + mujer, data = datos)
het <- bind_rows(
  tibble(ajustados = fitted(m_niv), residuos = residuals(m_niv),
         modelo = "Ingreso en niveles: abanico"),
  tibble(ajustados = fitted(m3), residuos = residuals(m3),
         modelo = "log(ingreso): varianza estable")) %>%
  mutate(modelo = factor(modelo, levels = c("Ingreso en niveles: abanico",
                                            "log(ingreso): varianza estable")))
p <- ggplot(het, aes(ajustados, residuos)) +
  geom_point(alpha = .5, color = celeste, size = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = rojo) +
  facet_wrap(~ modelo, scales = "free") +
  labs(x = "Valores ajustados", y = "Residuos")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: bp-robusto
m_niv <- lm(ingreso_miles ~ educacion + experiencia + mujer, data = datos)
bptest(m_niv)


## -----------------------------------------------------------------------------
#| label: tabla-robusto
#| echo: false
tibble(
  Término = names(coef(m_niv)),
  Estimación = coef(m_niv),
  `EE clásico` = sqrt(diag(vcov(m_niv))),
  `EE robusto (HC1)` = sqrt(diag(vcovHC(m_niv, type = "HC1")))
) %>% filter(Término != "(Intercept)") %>% kable(digits = 1)

