## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(knitr)
library(scales)
library(broom)
library(car)
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


## -----------------------------------------------------------------------------
#| label: crear-datos
set.seed(42)
n <- 250
datos <- tibble(
  genero = sample(c("Mujer", "Hombre"), n, replace = TRUE,
                  prob = c(.45, .55)),
  region = sample(c("Sur", "Centro", "Norte"), n, replace = TRUE,
                  prob = c(.35, .35, .30)),
  sector = sample(c("Servicios", "Industria", "Tecnología"), n,
                  replace = TRUE, prob = c(.40, .35, .25)),
  edad = round(rnorm(n, mean = 40, sd = 10)),
  educacion = round(rnorm(n, mean = 13, sd = 3), 1),
  experiencia = round(pmax(edad - educacion - 6 + rnorm(n, 0, 4), 0), 1),
  salario = round(pmax(800,
    2200 + 320 * educacion + 125 * experiencia +
      200 * (genero == "Hombre") +
      150 * (region == "Centro") + 280 * (region == "Norte") +
      950 * (genero == "Hombre") * (educacion - 13) / 3 +
      rnorm(n, mean = 0, sd = 450)))
)


## -----------------------------------------------------------------------------
#| label: img-flujo
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/flujo_econometria.png")


## -----------------------------------------------------------------------------
#| label: tabla-grupos
bind_rows(
  datos %>% group_by(grupo = genero) %>%
    summarise(n = n(), media = mean(salario),
              mediana = median(salario), de = sd(salario)),
  datos %>% group_by(grupo = region) %>%
    summarise(n = n(), media = mean(salario),
              mediana = median(salario), de = sd(salario))
) %>% kable(digits = 0)


## -----------------------------------------------------------------------------
#| label: code-scatter-edu
#| eval: false
# ggplot(datos,
#        aes(x = educacion,
#            y = salario)) +
#   geom_point(alpha = .5,
#              color = celeste) +
#   geom_smooth(method = "lm",
#               color = rojo) +
#   scale_y_continuous(
#     labels = scales::comma) +
#   labs(x = "Años de educación",
#        y = "Salario (miles de $)")


## -----------------------------------------------------------------------------
#| label: plot-scatter-edu
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = educacion, y = salario)) +
    geom_point(alpha = .5, color = celeste) +
    geom_smooth(method = "lm", color = rojo) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Años de educación", y = "Salario (miles de $)")
)


## -----------------------------------------------------------------------------
#| label: mod-simple
m1 <- lm(salario ~ educacion, data = datos)
tidy(m1) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: ovb-demo
corto <- lm(salario ~ educacion, data = datos)
largo <- lm(salario ~ educacion + experiencia, data = datos)
delta <- coef(lm(experiencia ~ educacion, data = datos))["educacion"]
round(c(corto = unname(coef(corto)["educacion"]),
        largo = unname(coef(largo)["educacion"]),
        sesgo_formula = unname(coef(largo)["experiencia"] * delta)), 1)


## -----------------------------------------------------------------------------
#| label: mod-multiple
m2 <- lm(salario ~ educacion + experiencia + edad, data = datos)
tidy(m2) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: dummies-relevel
datos <- datos %>%
  mutate(genero = factor(genero, levels = c("Mujer", "Hombre")),
         region = factor(region, levels = c("Sur", "Centro", "Norte")))
contrasts(datos$region)


## -----------------------------------------------------------------------------
#| label: mod-dummies
m3 <- lm(salario ~ educacion + experiencia + edad + genero + region,
         data = datos)
tidy(m3) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: mod-interaccion
m4 <- lm(salario ~ educacion * genero + experiencia + edad + region,
         data = datos)
tidy(m4) %>% filter(str_detect(term, "educacion|genero")) %>%
  kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: code-plot-inter
#| eval: false
# ggplot(datos,
#        aes(x = educacion,
#            y = salario,
#            color = genero)) +
#   geom_point(alpha = .45) +
#   geom_smooth(method = "lm",
#               se = FALSE,
#               linewidth = 1.1) +
#   labs(x = "Años de educación",
#        y = "Salario (miles de $)",
#        color = NULL)


## -----------------------------------------------------------------------------
#| label: plot-inter
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = educacion, y = salario, color = genero)) +
    geom_point(alpha = .45) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
    scale_color_manual(values = c(Mujer = naranja, Hombre = celeste)) +
    labs(x = "Años de educación", y = "Salario (miles de $)",
         color = NULL) +
    theme(legend.position = "top")
)


## -----------------------------------------------------------------------------
#| label: efectos-marginales
b <- coef(m4); V <- vcov(m4)
int <- "educacion:generoHombre"
ret_h <- b["educacion"] + b[int]
se_h <- sqrt(V["educacion", "educacion"] + V[int, int] +
               2 * V["educacion", int])
tibble(grupo = c("Mujeres", "Hombres"),
       retorno = c(b["educacion"], ret_h),
       ee = c(sqrt(V["educacion", "educacion"]), se_h)) %>%
  mutate(ic_95 = paste0("[", round(retorno - 1.96 * ee), "; ",
                        round(retorno + 1.96 * ee), "]")) %>%
  kable(digits = 1)


## -----------------------------------------------------------------------------
#| label: f-anidada
print(anova(m3, m4), signif.stars = FALSE)


## -----------------------------------------------------------------------------
#| label: vif-m3
vif(m3) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: img-diagnosticos
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/diagnosticos_r.png")


## -----------------------------------------------------------------------------
#| label: code-resid-fit
#| eval: false
# diag_m4 <- augment(m4)
# 
# ggplot(diag_m4,
#        aes(x = .fitted,
#            y = .resid)) +
#   geom_point(alpha = .5,
#              color = celeste) +
#   geom_hline(yintercept = 0,
#              linetype = "dashed") +
#   geom_smooth(se = FALSE,
#               color = rojo) +
#   labs(x = "Valores ajustados",
#        y = "Residuos")


## -----------------------------------------------------------------------------
#| label: plot-resid-fit
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
diag_m4 <- augment(m4)
interactivo(
  ggplot(diag_m4, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = .5, color = celeste) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(se = FALSE, color = rojo) +
    labs(x = "Valores ajustados", y = "Residuos")
)


## -----------------------------------------------------------------------------
#| label: code-qq
#| eval: false
# qq <- tibble(
#   teorico =
#     qnorm(ppoints(nobs(m4))),
#   muestral = sort(resid(m4)))
# 
# ggplot(qq,
#        aes(x = teorico,
#            y = muestral)) +
#   geom_point(alpha = .5,
#              color = celeste) +
#   geom_line(aes(y = sd(muestral) *
#                   teorico),
#             color = rojo) +
#   labs(x = "Cuantiles teóricos",
#        y = "Cuantiles muestrales")


## -----------------------------------------------------------------------------
#| label: plot-qq
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
qq <- tibble(teorico = qnorm(ppoints(nobs(m4))),
             muestral = sort(resid(m4)))
interactivo(
  ggplot(qq, aes(x = teorico, y = muestral)) +
    geom_point(alpha = .5, color = celeste) +
    geom_line(aes(y = sd(muestral) * teorico), color = rojo,
              linewidth = 1) +
    labs(x = "Cuantiles teóricos", y = "Cuantiles muestrales")
)


## -----------------------------------------------------------------------------
#| label: bp-test
bptest(m4)


## -----------------------------------------------------------------------------
#| label: robustos-coeftest
ct <- coeftest(m4, vcov = vcovHC(m4, type = "HC1"))
ct[c("educacion", "educacion:generoHombre"), ]


## -----------------------------------------------------------------------------
#| label: robustos-tabla
tibble(termino = names(coef(m4)), beta = coef(m4),
       se_mco = sqrt(diag(vcov(m4))),
       se_hc1 = sqrt(diag(vcovHC(m4, type = "HC1")))) %>%
  mutate(razon = se_hc1 / se_mco) %>%
  filter(str_detect(termino, "educacion|genero|experiencia")) %>%
  kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: tabla-modelos
modelsummary(list("(1) Simple" = m1, "(2) Controles" = m3,
                  "(3) Interacción" = m4),
             coef_omit = "Intercept|edad|region",
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             stars = TRUE)


## -----------------------------------------------------------------------------
#| label: code-coefplot
#| eval: false
# coefs <- tidy(m4,
#               conf.int = TRUE) %>%
#   filter(!term %in%
#     c("(Intercept)",
#       "generoHombre"))
# 
# ggplot(coefs,
#        aes(x = estimate,
#            y = reorder(term,
#                        estimate))) +
#   geom_vline(xintercept = 0,
#              linetype = "dashed") +
#   geom_pointrange(
#     aes(xmin = conf.low,
#         xmax = conf.high),
#     color = azul) +
#   labs(x = "Coeficiente (IC 95%)",
#        y = NULL)


## -----------------------------------------------------------------------------
#| label: plot-coefplot
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
coefs <- tidy(m4, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)", "generoHombre"))
interactivo(
  ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    color = azul) +
    labs(x = "Coeficiente (IC 95%)", y = NULL)
)


## -----------------------------------------------------------------------------
#| label: escenarios-pred
esc <- crossing(genero = c("Mujer", "Hombre"),
                educacion = c(12, 16)) %>%
  mutate(region = "Sur", experiencia = mean(datos$experiencia),
         edad = mean(datos$edad))
esc %>% select(genero, educacion) %>%
  bind_cols(as_tibble(predict(m4, esc, interval = "confidence"))) %>%
  kable(digits = 0)

