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
datos <- gapminder %>% filter(year == 2007)
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}


## -----------------------------------------------------------------------------
#| label: arbol-tipos
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/tipos_variables.png")


## -----------------------------------------------------------------------------
#| label: datos-glimpse
#| output-location: column
glimpse(datos)


## -----------------------------------------------------------------------------
#| label: centro
c(media = mean(datos$lifeExp), mediana = median(datos$lifeExp),
  media_recortada_10 = mean(datos$lifeExp, trim = 0.10))


## -----------------------------------------------------------------------------
#| label: plot-outlier-media
#| echo: false
#| fig-height: 3.1
set.seed(11)
sueldos <- c(rnorm(29, 800, 120), 8000)
df <- tibble(sueldo = sueldos)
p <- ggplot(df, aes(x = sueldo, y = 0)) +
  geom_jitter(height = 0.02, size = 3, alpha = 0.6, color = celeste) +
  geom_vline(xintercept = mean(sueldos), color = rojo, linewidth = 1.2) +
  geom_vline(xintercept = median(sueldos), color = verde, linewidth = 1.2) +
  annotate("text", x = mean(sueldos) + 900, y = 0.09,
           label = paste0("Media = ", round(mean(sueldos))), color = rojo, size = 4.5) +
  annotate("text", x = median(sueldos) + 950, y = -0.09,
           label = paste0("Mediana = ", round(median(sueldos))), color = verde, size = 4.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Sueldo (miles de $)", y = NULL,
       title = "30 sueldos: 29 cercanos a 800 y un gerente con 8.000") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
interactivo(p)


## -----------------------------------------------------------------------------
#| label: media-mediana
media <- mean(datos$lifeExp)
mediana <- median(datos$lifeExp)
round(c(media = media, mediana = mediana,
        diferencia = media - mediana), 2)


## -----------------------------------------------------------------------------
#| label: plot-asimetria
#| echo: false
#| fig-height: 3.6
#| fig-width: 5
#| out.width: "100%"
p <- ggplot(datos, aes(x = lifeExp)) +
  geom_histogram(bins = 22, fill = celeste, color = "white") +
  geom_vline(xintercept = media, color = rojo, linewidth = 1) +
  geom_vline(xintercept = mediana, color = verde, linewidth = 1) +
  annotate("text", x = media - 6, y = 16, label = "media", color = rojo, size = 4.5) +
  annotate("text", x = mediana + 4.5, y = 18, label = "mediana", color = verde, size = 4.5) +
  labs(x = "Esperanza de vida (años)", y = "Frecuencia",
       title = "Cola izquierda: media < mediana")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: dispersion
round(c(sd = sd(datos$gdpPercap), IQR = IQR(datos$gdpPercap),
        rango = diff(range(datos$gdpPercap))), 0)


## -----------------------------------------------------------------------------
#| label: plot-misma-media
#| echo: false
#| fig-height: 3.3
set.seed(42)
sim <- tibble(
  A = rnorm(4000, 500, 30),
  B = rnorm(4000, 500, 120)
) %>% pivot_longer(everything(), names_to = "comuna", values_to = "ingreso")
p <- ggplot(sim, aes(x = ingreso, fill = comuna)) +
  geom_density(alpha = 0.55, color = NA) +
  geom_vline(xintercept = 500, linetype = "dashed", color = azul, linewidth = 1) +
  scale_fill_manual(values = c(A = celeste, B = naranja),
                    labels = c("Comuna A (DE = 30)", "Comuna B (DE = 120)")) +
  labs(x = "Ingreso del hogar (miles de $)", y = "Densidad", fill = NULL,
       title = "Ambas comunas tienen media 500") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-anatomia-boxplot
#| echo: false
#| fig-height: 3.3
q <- quantile(datos$gdpPercap, c(.25, .5, .75))
iqr <- q[3] - q[1]
lim_sup <- q[3] + 1.5 * iqr
p <- ggplot(datos, aes(y = gdpPercap, x = "")) +
  geom_boxplot(fill = celeste, alpha = 0.5, width = 0.25, outlier.color = rojo, outlier.size = 2.5) +
  annotate("text", x = 1.28, y = q[1], label = paste0("Q1 = ", round(q[1])), hjust = 0, size = 4.2, color = azul) +
  annotate("text", x = 1.28, y = q[2], label = paste0("Mediana = ", round(q[2])), hjust = 0, size = 4.2, color = azul) +
  annotate("text", x = 1.28, y = q[3], label = paste0("Q3 = ", round(q[3])), hjust = 0, size = 4.2, color = azul) +
  annotate("text", x = 1.28, y = lim_sup, label = paste0("Q3 + 1.5·IQR = ", round(lim_sup)), hjust = 0, size = 4.2, color = rojo) +
  annotate("text", x = 0.78, y = 46000, label = "outliers", color = rojo, size = 4.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(expand = expansion(add = c(0.4, 1.4))) +
  labs(x = NULL, y = "PIB per cápita (USD)", title = "PIB per cápita mundial, 2007") +
  theme(axis.text.x = element_blank())
interactivo(p)


## -----------------------------------------------------------------------------
#| label: cv
cv <- function(x) sd(x) / mean(x) * 100
round(c(cv_pib = cv(datos$gdpPercap),
        cv_vida = cv(datos$lifeExp)), 1)


## -----------------------------------------------------------------------------
#| label: plot-cv
#| echo: false
#| fig-height: 3.2
#| fig-width: 5
#| out.width: "100%"
p <- tibble(variable = c("PIB per cápita", "Esperanza de vida"),
            cv = c(cv(datos$gdpPercap), cv(datos$lifeExp))) %>%
  ggplot(aes(x = cv, y = reorder(variable, cv), fill = variable)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(cv, 1), "%")), hjust = -0.15, size = 5, color = azul) +
  scale_fill_manual(values = c(celeste, naranja)) +
  scale_x_continuous(limits = c(0, 135)) +
  labs(y = NULL, x = "Coeficiente de variación (%)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: zscores
datos <- datos %>% mutate(z_vida = (lifeExp - mean(lifeExp)) / sd(lifeExp))
datos %>% filter(abs(z_vida) > 2) %>%
  select(country, continent, lifeExp, z_vida) %>%
  arrange(z_vida) %>% mutate(z_vida = round(z_vida, 2))


## -----------------------------------------------------------------------------
#| label: outliers-iqr
Q1 <- quantile(datos$gdpPercap, .25)
Q3 <- quantile(datos$gdpPercap, .75)
lim <- Q3 + 1.5 * IQR(datos$gdpPercap)
datos %>% filter(gdpPercap > lim) %>%
  select(country, gdpPercap) %>%
  arrange(desc(gdpPercap)) %>% head(5)


## -----------------------------------------------------------------------------
#| label: code-hist
#| eval: false
# ggplot(datos, aes(x = lifeExp)) +
#   geom_histogram(bins = 20,
#                  fill = celeste,
#                  color = "white") +
#   geom_vline(xintercept = media,
#              color = rojo,
#              linetype = "dashed",
#              linewidth = 1) +
#   labs(x = "Esperanza de vida (años)",
#        y = "N° de países")


## -----------------------------------------------------------------------------
#| label: plot-hist
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = lifeExp)) +
    geom_histogram(bins = 20, fill = celeste, color = "white") +
    geom_vline(xintercept = media, color = rojo,
               linetype = "dashed", linewidth = 1) +
    labs(x = "Esperanza de vida (años)", y = "N° de países")
)


## -----------------------------------------------------------------------------
#| label: code-box
#| eval: false
# ggplot(datos,
#        aes(x = continent, y = gdpPercap,
#            fill = continent)) +
#   geom_boxplot(alpha = .7,
#                outlier.color = rojo) +
#   geom_jitter(width = .15, alpha = .3,
#               size = 1) +
#   scale_y_log10(labels = scales::comma) +
#   labs(x = NULL, y = "PIB pc (USD, log)") +
#   theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: plot-box
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = continent, y = gdpPercap, fill = continent)) +
    geom_boxplot(alpha = .7, outlier.color = rojo) +
    geom_jitter(width = .15, alpha = .3, size = 1) +
    scale_y_log10(labels = scales::comma) +
    labs(x = NULL, y = "PIB pc (USD, log)") +
    theme(legend.position = "none")
)


## -----------------------------------------------------------------------------
#| label: code-scatter
#| eval: false
# ggplot(datos,
#        aes(x = gdpPercap, y = lifeExp,
#            size = pop, color = continent)) +
#   geom_point(alpha = .7) +
#   scale_x_log10(labels = scales::comma) +
#   scale_size(range = c(1, 12),
#              guide = "none") +
#   labs(x = "PIB pc (USD, log)",
#        y = "Esperanza de vida (años)",
#        color = NULL) +
#   theme(legend.position = "bottom")


## -----------------------------------------------------------------------------
#| label: plot-scatter
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = gdpPercap, y = lifeExp, size = pop,
                    color = continent, text = country)) +
    geom_point(alpha = .7) +
    scale_x_log10(labels = scales::comma) +
    scale_size(range = c(1, 12), guide = "none") +
    labs(x = "PIB pc (USD, log)", y = "Esperanza de vida (años)",
         color = NULL) +
    theme(legend.position = "bottom")
)


## -----------------------------------------------------------------------------
#| label: code-violin
#| eval: false
# ggplot(datos,
#        aes(x = continent, y = lifeExp,
#            fill = continent)) +
#   geom_violin(alpha = .5, color = NA) +
#   geom_boxplot(width = .15,
#                fill = "white", alpha = .8) +
#   labs(x = NULL,
#        y = "Esperanza de vida (años)") +
#   theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| label: plot-violin
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
interactivo(
  ggplot(datos, aes(x = continent, y = lifeExp, fill = continent)) +
    geom_violin(alpha = .5, color = NA) +
    geom_boxplot(width = .15, fill = "white", alpha = .8) +
    labs(x = NULL, y = "Esperanza de vida (años)") +
    theme(legend.position = "none")
)


## -----------------------------------------------------------------------------
#| label: tabla-resumen
datos %>% group_by(continent) %>%
  summarise(n = n(), media = mean(lifeExp), mediana = median(lifeExp),
            de = sd(lifeExp), iqr = IQR(lifeExp), cv = sd(lifeExp)/mean(lifeExp)*100) %>%
  mutate(across(where(is.numeric), ~round(.x, 1))) %>%
  kable()


## -----------------------------------------------------------------------------
#| label: forma
round(c(sesgo_pib = skewness(datos$gdpPercap),
        curtosis_pib = kurtosis(datos$gdpPercap),
        sesgo_vida = skewness(datos$lifeExp)), 2)


## -----------------------------------------------------------------------------
#| label: plot-formas
#| echo: false
#| fig-height: 3.6
#| fig-width: 5.4
#| out.width: "100%"
set.seed(7)
formas <- tibble(
  `Sesgo positivo` = rlnorm(5000, 0, 0.6),
  `Simétrica` = rnorm(5000, 2, 0.6),
  `Sesgo negativo` = 4 - rlnorm(5000, 0, 0.6)
) %>% pivot_longer(everything())
p <- ggplot(formas, aes(value, fill = name)) +
  geom_density(alpha = .6, color = NA) +
  scale_fill_manual(values = c(rojo, naranja, celeste)) +
  coord_cartesian(xlim = c(-1, 6)) +
  labs(x = NULL, y = "Densidad", fill = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-log
#| echo: false
#| fig-height: 3.1
dlog <- bind_rows(
  tibble(valor = datos$gdpPercap,
         escala = paste0("PIB en USD (sesgo = ", round(skewness(datos$gdpPercap), 1), ")")),
  tibble(valor = log(datos$gdpPercap),
         escala = paste0("log(PIB) (sesgo = ", round(skewness(log(datos$gdpPercap)), 1), ")"))
)
p <- ggplot(dlog, aes(valor, fill = escala)) +
  geom_histogram(bins = 25, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c(verde, naranja)) +
  facet_wrap(~escala, scales = "free") +
  labs(x = NULL, y = "N° países")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: correlacion
round(c(
  pearson = cor(datos$gdpPercap, datos$lifeExp),
  pearson_log = cor(log(datos$gdpPercap),
                    datos$lifeExp),
  spearman = cor(datos$gdpPercap, datos$lifeExp,
                 method = "spearman")), 3)


## -----------------------------------------------------------------------------
#| label: brechas
datos %>% group_by(continent) %>%
  summarise(vida = mean(lifeExp), pib = mean(gdpPercap)) %>%
  mutate(brecha_vida = round(vida - max(vida), 1),
         brecha_pib = round(pib - max(pib), 0),
         across(c(vida, pib), ~round(.x, 1))) %>% kable()

