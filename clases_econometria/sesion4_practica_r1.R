## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(knitr)
library(broom)
library(lmtest)
library(modelsummary)
library(plotly)
theme_set(theme_minimal(base_size = 13))
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}


## -----------------------------------------------------------------------------
#| label: img-flujo-mapa
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/flujo_econometria.png")


## -----------------------------------------------------------------------------
#| label: code-carga
#| eval: false
# # CSV (readr, parte del tidyverse)
# encuesta <- read_csv("encuesta_hogares.csv")
# 
# # Excel
# library(readxl)
# encuesta <- read_excel("encuesta.xlsx", sheet = "datos")
# 
# # Stata / SPSS (archivos de encuestas oficiales)
# library(haven)
# encuesta <- read_dta("casen.dta")
# 
# # Directamente desde una URL
# encuesta <- read_csv("https://ejemplo.org/datos.csv")


## -----------------------------------------------------------------------------
#| label: crea-encuesta
set.seed(2026)
n <- 150
encuesta <- tibble(
  educacion = round(pmin(pmax(rnorm(n, 13, 3), 4), 21)),
  genero    = sample(c("Hombre", "Mujer"), n, replace = TRUE, prob = c(0.52, 0.48)),
  region    = sample(c("Norte", "Centro", "Sur"), n, replace = TRUE,
                     prob = c(0.30, 0.45, 0.25)),
  empleo    = sample(c("Empleado", "Desempleado"), n, replace = TRUE,
                     prob = c(0.85, 0.15)),
  ingresos  = round(exp(6.2 + 0.09 * educacion + 0.15 * (genero == "Hombre") -
                        0.35 * (empleo == "Desempleado") + rnorm(n, 0, 0.30)))
)
encuesta$ingresos[c(17, 84)] <- NA   # dos hogares no reportan ingreso
encuesta$educacion[131] <- 99        # un error de digitacion


## -----------------------------------------------------------------------------
#| label: inspeccion-glimpse
glimpse(encuesta)


## -----------------------------------------------------------------------------
#| label: limpieza-na
colSums(is.na(encuesta))       # ¿cuantos NA por variable?
range(encuesta$educacion)      # el maximo 99 es imposible
encuesta <- encuesta %>%
  filter(educacion <= 22) %>%  # elimina el error de digitacion
  drop_na()                    # elimina los ingresos no reportados
nrow(encuesta)


## -----------------------------------------------------------------------------
#| label: tabla-descriptiva
encuesta %>% group_by(genero) %>%
  summarise(n = n(), media = mean(ingresos), mediana = median(ingresos),
            de = sd(ingresos), educ_media = mean(educacion)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  kable()


## -----------------------------------------------------------------------------
#| label: code-hist-ing
#| eval: false
# media_i   <- mean(encuesta$ingresos)
# mediana_i <- median(encuesta$ingresos)
# 
# ggplot(encuesta, aes(x = ingresos)) +
#   geom_histogram(bins = 22,
#                  fill = celeste,
#                  color = "white") +
#   geom_vline(xintercept = media_i,
#              color = rojo) +
#   geom_vline(xintercept = mediana_i,
#              color = verde) +
#   labs(x = "Ingreso mensual (USD)",
#        y = "N° de hogares")


## -----------------------------------------------------------------------------
#| label: plot-hist-ing
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
media_i   <- mean(encuesta$ingresos)
mediana_i <- median(encuesta$ingresos)
interactivo(
  ggplot(encuesta, aes(x = ingresos)) +
    geom_histogram(bins = 22, fill = celeste, color = "white") +
    geom_vline(xintercept = media_i, color = rojo, linewidth = 1) +
    geom_vline(xintercept = mediana_i, color = verde, linewidth = 1) +
    labs(x = "Ingreso mensual (USD)", y = "N° de hogares")
)


## -----------------------------------------------------------------------------
#| label: code-scatter-edu
#| eval: false
# ggplot(encuesta,
#        aes(x = educacion,
#            y = ingresos)) +
#   geom_jitter(width = 0.2,
#               alpha = 0.5,
#               color = azul) +
#   geom_smooth(method = "lm",
#               color = rojo) +
#   labs(x = "Años de educación",
#        y = "Ingreso mensual (USD)")


## -----------------------------------------------------------------------------
#| label: plot-scatter-edu
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(encuesta, aes(x = educacion, y = ingresos)) +
    geom_jitter(width = 0.2, alpha = 0.5, color = azul) +
    geom_smooth(method = "lm", color = rojo) +
    labs(x = "Años de educación", y = "Ingreso mensual (USD)")
)


## -----------------------------------------------------------------------------
#| label: ttest-genero
t_gen <- t.test(ingresos ~ genero, data = encuesta)
tidy(t_gen) %>%
  select(estimate1, estimate2, statistic, p.value, conf.low, conf.high) %>%
  kable(digits = 3, col.names = c("media H", "media M", "t", "valor-p",
                                  "IC inf", "IC sup"))


## -----------------------------------------------------------------------------
#| label: code-box-gen
#| eval: false
# ggplot(encuesta,
#        aes(x = genero, y = ingresos,
#            fill = genero)) +
#   geom_boxplot(alpha = .6,
#                outlier.color = rojo) +
#   geom_jitter(width = .15,
#               alpha = .3) +
#   labs(x = NULL,
#        y = "Ingreso mensual (USD)") +
#   theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: plot-box-gen
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(encuesta, aes(x = genero, y = ingresos, fill = genero)) +
    geom_boxplot(alpha = .6, outlier.color = rojo) +
    geom_jitter(width = .15, alpha = .3) +
    labs(x = NULL, y = "Ingreso mensual (USD)") +
    theme(legend.position = "none")
)


## -----------------------------------------------------------------------------
#| label: prop-desempleo
tab_empleo <- table(encuesta$empleo)
tab_empleo
prop.test(tab_empleo["Desempleado"], sum(tab_empleo), p = 0.10) %>%
  tidy() %>%
  select(estimate, statistic, p.value, conf.low, conf.high) %>%
  kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: chi-tabla
tab_er <- table(encuesta$empleo, encuesta$region)
kable(tab_er)


## -----------------------------------------------------------------------------
#| label: chi-prueba
chisq.test(tab_er) %>% tidy() %>% kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: reg-niveles
m1 <- lm(ingresos ~ educacion, data = encuesta)
tidy(m1) %>%
  mutate(across(c(estimate, std.error, statistic), ~ round(.x, 1)),
         p.value = format.pval(p.value, digits = 2, eps = 0.001)) %>%
  kable()


## -----------------------------------------------------------------------------
#| label: reg-log
m2 <- lm(log(ingresos) ~ educacion, data = encuesta)
tidy(m2) %>%
  mutate(across(c(estimate, std.error, statistic), ~ round(.x, 3)),
         p.value = format.pval(p.value, digits = 2, eps = 0.001)) %>%
  kable()


## -----------------------------------------------------------------------------
#| label: ajuste-glance
glance(m1) %>%
  select(r.squared, adj.r.squared, sigma, statistic, nobs) %>%
  kable(digits = 3, col.names = c("R2", "R2 ajustado", "sigma", "F", "n"))


## -----------------------------------------------------------------------------
#| label: inferencia-beta1
tidy(m1, conf.int = TRUE) %>%
  filter(term == "educacion") %>%
  select(term, estimate, std.error, statistic, conf.low, conf.high) %>%
  kable(digits = 1)


## -----------------------------------------------------------------------------
#| label: prediccion-ic
nuevo <- tibble(educacion = 16)
predict(m1, nuevo, interval = "confidence")
predict(m1, nuevo, interval = "prediction")


## -----------------------------------------------------------------------------
#| label: code-resid-m1
#| eval: false
# diag1 <- tibble(
#   ajustado = fitted(m1),
#   residuo  = resid(m1))
# 
# ggplot(diag1,
#        aes(ajustado, residuo)) +
#   geom_point(alpha = .5,
#              color = azul) +
#   geom_hline(yintercept = 0,
#              linetype = "dashed") +
#   geom_smooth(se = FALSE,
#               color = naranja) +
#   labs(x = "Valores ajustados",
#        y = "Residuos")


## -----------------------------------------------------------------------------
#| label: plot-resid-m1
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
diag1 <- tibble(ajustado = fitted(m1), residuo = resid(m1))
interactivo(
  ggplot(diag1, aes(ajustado, residuo)) +
    geom_point(alpha = .5, color = azul) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(se = FALSE, color = naranja) +
    labs(x = "Valores ajustados", y = "Residuos")
)


## -----------------------------------------------------------------------------
#| label: pruebas-formales
list("Shapiro (niveles)" = shapiro.test(resid(m1)),
     "Shapiro (log)"     = shapiro.test(resid(m2)),
     "Breusch-Pagan (niveles)" = bptest(m1),
     "Breusch-Pagan (log)"     = bptest(m2)) %>%
  map_df(tidy, .id = "prueba") %>%
  select(prueba, statistic, p.value) %>%
  mutate(statistic = round(statistic, 2), p.value = signif(p.value, 2)) %>%
  kable()


## -----------------------------------------------------------------------------
#| label: code-ggplotly
#| eval: false
# library(plotly)
# 
# p <- ggplot(encuesta,
#        aes(educacion, ingresos,
#            color = empleo)) +
#   geom_jitter(width = .2,
#               alpha = .6) +
#   labs(x = "Años de educación",
#        y = "Ingreso (USD)")
# 
# ggplotly(p)   # eso es todo


## -----------------------------------------------------------------------------
#| label: plot-ggplotly
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(encuesta, aes(educacion, ingresos, color = empleo)) +
    geom_jitter(width = .2, alpha = .6) +
    scale_color_manual(values = c(Empleado = celeste, Desempleado = rojo)) +
    labs(x = "Años de educación", y = "Ingreso (USD)", color = NULL) +
    theme(legend.position = "bottom")
)


## -----------------------------------------------------------------------------
#| label: informe-modelsummary
modelsummary(list("Ingresos (USD)" = m1, "log(Ingresos)" = m2),
             output = "markdown", fmt = 2,
             stars = c("*" = .05, "**" = .01, "***" = .001),
             gof_map = c("nobs", "r.squared"))

