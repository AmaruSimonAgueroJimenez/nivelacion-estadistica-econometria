## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(gapminder)
library(moments)
library(knitr)
library(plotly)
theme_set(theme_minimal(base_size = 13))
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"
set.seed(2026)
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}


## -----------------------------------------------------------------------------
#| label: mapa-flujo
#| echo: false
#| out.width: "58%"
knitr::include_graphics("figuras/flujo_analisis.png")


## -----------------------------------------------------------------------------
#| label: paso-carga
#| output-location: column
library(gapminder)
glimpse(gapminder)


## -----------------------------------------------------------------------------
#| label: paso-calidad
c(filas = nrow(gapminder),
  paises = n_distinct(gapminder$country),
  anios = n_distinct(gapminder$year),
  na_total = sum(is.na(gapminder)))


## -----------------------------------------------------------------------------
#| label: paso-calidad-rango
range(gapminder$year)
summary(gapminder$lifeExp)[c(1, 6)]


## -----------------------------------------------------------------------------
#| label: paso-limpieza
datos <- gapminder %>%
  filter(year == 2007) %>%
  mutate(log_pib = log(gdpPercap),
         grupo_pib = if_else(
           gdpPercap >= median(gdpPercap),
           "PIB alto", "PIB bajo"))
datos %>% count(grupo_pib)


## -----------------------------------------------------------------------------
#| label: paso-descriptivos
datos %>% group_by(continent) %>%
  summarise(n = n(), media = mean(lifeExp), de = sd(lifeExp),
            mediana = median(lifeExp), iqr = IQR(lifeExp)) %>%
  mutate(across(where(is.numeric), ~round(.x, 1))) %>% kable()


## -----------------------------------------------------------------------------
#| label: code-hist5
#| eval: false
# p <- ggplot(datos, aes(x = lifeExp)) +
#   geom_histogram(bins = 18,
#                  fill = celeste,
#                  color = "white") +
#   geom_vline(
#     xintercept = mean(datos$lifeExp),
#     color = rojo, linewidth = 1,
#     linetype = "dashed") +
#   labs(x = "Esperanza de vida (años)",
#        y = "N° de países")
# ggplotly(p)  # interactivo en HTML


## -----------------------------------------------------------------------------
#| label: plot-hist5
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = lifeExp)) +
    geom_histogram(bins = 18, fill = celeste, color = "white") +
    geom_vline(xintercept = mean(datos$lifeExp), color = rojo,
               linewidth = 1, linetype = "dashed") +
    labs(x = "Esperanza de vida (años)", y = "N° de países")
)


## -----------------------------------------------------------------------------
#| label: code-box5
#| eval: false
# ggplot(datos,
#        aes(x = reorder(continent,
#                        lifeExp, median),
#            y = lifeExp,
#            fill = continent)) +
#   geom_boxplot(alpha = .7,
#                outlier.color = rojo) +
#   geom_jitter(width = .12,
#               alpha = .3, size = 1) +
#   labs(x = NULL,
#        y = "Esperanza de vida (años)") +
#   theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: plot-box5
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = reorder(continent, lifeExp, median),
                    y = lifeExp, fill = continent)) +
    geom_boxplot(alpha = .7, outlier.color = rojo) +
    geom_jitter(width = .12, alpha = .3, size = 1) +
    labs(x = NULL, y = "Esperanza de vida (años)") +
    theme(legend.position = "none")
)


## -----------------------------------------------------------------------------
#| label: code-scatter5
#| eval: false
# ggplot(datos,
#        aes(x = log_pib, y = lifeExp)) +
#   geom_point(aes(color = grupo_pib),
#              alpha = .7, size = 2) +
#   geom_smooth(method = "lm",
#               color = azul) +
#   scale_color_manual(
#     values = c(celeste, naranja)) +
#   labs(x = "log(PIB per cápita)",
#        y = "Esperanza de vida (años)",
#        color = NULL) +
#   theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: plot-scatter5
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = log_pib, y = lifeExp)) +
    geom_point(aes(color = grupo_pib, text = country),
               alpha = .7, size = 2) +
    geom_smooth(method = "lm", color = azul) +
    scale_color_manual(values = c(celeste, naranja)) +
    labs(x = "log(PIB per cápita)",
         y = "Esperanza de vida (años)", color = NULL) +
    theme(legend.position = "bottom")
)


## -----------------------------------------------------------------------------
#| label: code-dens5
#| eval: false
# ggplot(datos,
#        aes(x = lifeExp,
#            fill = grupo_pib)) +
#   geom_density(alpha = .55,
#                color = NA) +
#   scale_fill_manual(
#     values = c(celeste, naranja)) +
#   labs(x = "Esperanza de vida (años)",
#        y = "Densidad", fill = NULL) +
#   theme(legend.position = "top")


## -----------------------------------------------------------------------------
#| label: plot-dens5
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = lifeExp, fill = grupo_pib)) +
    geom_density(alpha = .55, color = NA) +
    scale_fill_manual(values = c(celeste, naranja)) +
    labs(x = "Esperanza de vida (años)",
         y = "Densidad", fill = NULL) +
    theme(legend.position = "top")
)


## -----------------------------------------------------------------------------
#| label: paso-outliers
Q1 <- quantile(datos$gdpPercap, .25)
Q3 <- quantile(datos$gdpPercap, .75)
lim <- Q3 + 1.5 * IQR(datos$gdpPercap)
datos %>% filter(gdpPercap > lim) %>%
  select(country, gdpPercap) %>%
  arrange(desc(gdpPercap)) %>%
  mutate(gdpPercap = round(gdpPercap))


## -----------------------------------------------------------------------------
#| label: code-qq5
#| eval: false
# ggplot(datos,
#        aes(sample = lifeExp)) +
#   stat_qq(color = celeste,
#           size = 2, alpha = .7) +
#   stat_qq_line(color = rojo,
#                linewidth = 1) +
#   labs(
#     x = "Cuantiles teóricos N(0,1)",
#     y = "Cuantiles muestrales")


## -----------------------------------------------------------------------------
#| label: plot-qq5
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
ggplot(datos, aes(sample = lifeExp)) +
  stat_qq(color = celeste, size = 2, alpha = .7) +
  stat_qq_line(color = rojo, linewidth = 1) +
  labs(x = "Cuantiles teóricos N(0,1)",
       y = "Cuantiles muestrales")


## -----------------------------------------------------------------------------
#| label: paso-shapiro
sw <- shapiro.test(datos$lifeExp)
round(c(W = unname(sw$statistic), p = sw$p.value), 4)


## -----------------------------------------------------------------------------
#| label: paso-shapiro-grupo
datos %>% group_by(grupo_pib) %>%
  summarise(p_shapiro = shapiro.test(lifeExp)$p.value)


## -----------------------------------------------------------------------------
#| label: paso-ic
ic <- t.test(datos$lifeExp, conf.level = 0.95)
round(c(media = unname(ic$estimate),
        li = ic$conf.int[1], ls = ic$conf.int[2]), 2)


## -----------------------------------------------------------------------------
#| label: paso-ic-manual
n <- nrow(datos)
ee <- sd(datos$lifeExp) / sqrt(n)
round(mean(datos$lifeExp) + qt(c(.025, .975), n - 1) * ee, 2)


## -----------------------------------------------------------------------------
#| label: code-icgrupo
#| eval: false
# ic_g <- datos %>%
#   group_by(continent) %>%
#   summarise(
#     m = mean(lifeExp),
#     ee = sd(lifeExp) / sqrt(n()),
#     tq = qt(.975, n() - 1))
# ggplot(ic_g,
#        aes(x = m,
#            y = reorder(continent, m))) +
#   geom_pointrange(
#     aes(xmin = m - tq * ee,
#         xmax = m + tq * ee),
#     color = azul, linewidth = .9) +
#   labs(x = "Media e IC 95% (años)",
#        y = NULL)


## -----------------------------------------------------------------------------
#| label: plot-icgrupo
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo({
  ic_g <- datos %>% group_by(continent) %>%
    summarise(m = mean(lifeExp), ee = sd(lifeExp) / sqrt(n()),
              tq = qt(.975, n() - 1))
  ggplot(ic_g, aes(x = m, y = reorder(continent, m))) +
    geom_pointrange(aes(xmin = m - tq * ee, xmax = m + tq * ee),
                    color = azul, linewidth = .9) +
    labs(x = "Media e IC 95% (años)", y = NULL)
})


## -----------------------------------------------------------------------------
#| label: paso-t-una
t1 <- t.test(datos$lifeExp, mu = 70)
t1


## -----------------------------------------------------------------------------
#| label: paso-t-dos-medias
datos %>% group_by(grupo_pib) %>%
  summarise(n = n(), media = round(mean(lifeExp), 1),
            de = round(sd(lifeExp), 1))


## -----------------------------------------------------------------------------
#| label: paso-t-dos
t2 <- t.test(lifeExp ~ grupo_pib, data = datos)
round(c(dif = -diff(unname(t2$estimate)), li = t2$conf.int[1],
        ls = t2$conf.int[2], t = unname(t2$statistic)), 2)


## -----------------------------------------------------------------------------
#| label: paso-efecto
g <- datos %>% group_by(grupo_pib) %>%
  summarise(n = n(), m = mean(lifeExp), s = sd(lifeExp))
sp <- sqrt(((g$n[1] - 1) * g$s[1]^2 + (g$n[2] - 1) * g$s[2]^2) /
             (sum(g$n) - 2))
d <- (g$m[1] - g$m[2]) / sp
round(c(dif_medias = g$m[1] - g$m[2], sd_conjunta = sp, cohen_d = d), 2)


## -----------------------------------------------------------------------------
#| label: paso-tabla-final
tibble(
  Resultado = c("Media mundial 2007",
                "Prueba t vs meta de 70 años",
                "Brecha PIB alto - PIB bajo",
                "Tamaño del efecto"),
  Valor = c(
    sprintf("%.1f años [%.1f; %.1f]", ic$estimate,
            ic$conf.int[1], ic$conf.int[2]),
    sprintf("t = %.2f; p = %.3f", t1$statistic, t1$p.value),
    sprintf("%.1f años [%.1f; %.1f]; p < 0.001",
            -diff(unname(t2$estimate)),
            t2$conf.int[1], t2$conf.int[2]),
    sprintf("d = %.2f (muy grande)", d))) %>% kable()


## -----------------------------------------------------------------------------
#| label: paso-repro
set.seed(2026)
R.version.string

