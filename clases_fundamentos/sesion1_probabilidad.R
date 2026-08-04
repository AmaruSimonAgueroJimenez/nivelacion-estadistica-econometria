## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(knitr)
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
#| label: plot-frecuencia
#| echo: false
#| fig-height: 2.6
set.seed(123)
sim <- tibble(n = 1:1000, exito = rbinom(1000, 1, 0.42)) %>%
  mutate(frec = cummean(exito))
p <- ggplot(sim, aes(x = n, y = frec)) +
  geom_line(color = celeste, linewidth = 0.7) +
  geom_hline(yintercept = 0.42, color = rojo, linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 880, y = 0.48, label = "P(E) = 0.42", color = rojo, size = 4.5) +
  coord_cartesian(ylim = c(0.2, 0.7)) +
  labs(x = "Número de hogares observados (n)", y = "Frecuencia relativa",
       title = "Proporción acumulada de hogares con empleo (simulación)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: venn-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/diagrama_venn.png")


## -----------------------------------------------------------------------------
#| label: plot-condicional
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
df <- tibble(
  grupo = factor(c("Participantes", "No participantes", "Marginal"),
                 levels = c("Participantes", "No participantes", "Marginal")),
  prob = c(0.60, 0.30, 0.42))
p <- ggplot(df, aes(x = grupo, y = prob, fill = grupo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.4, size = 5, color = azul) +
  scale_fill_manual(values = c(celeste, naranja, "grey60")) +
  scale_y_continuous(limits = c(0, 0.75)) +
  labs(x = NULL, y = "P(consigue empleo)",
       title = "Probabilidad de empleo según participación")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-total
#| echo: false
#| fig-width: 5
#| fig-height: 3.5
#| out.width: "100%"
df <- tibble(
  zona = factor(c("Norte", "Centro", "Sur", "Total"),
                levels = c("Norte", "Centro", "Sur", "Total")),
  aporte = c(0.020, 0.054, 0.060, 0.134),
  tipo = c("Aporte", "Aporte", "Aporte", "Total"))
p <- ggplot(df, aes(x = zona, y = aporte, fill = tipo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", aporte)), vjust = -0.4, size = 4.5, color = azul) +
  scale_fill_manual(values = c(Aporte = celeste, Total = azul)) +
  scale_y_continuous(limits = c(0, 0.16)) +
  labs(x = NULL, y = "Aporte a P(D)",
       title = "P(D) = suma de aportes por zona")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-bayes-verifica
round(0.90 * 0.10 / (0.90 * 0.10 + 0.15 * 0.90), 3)


## -----------------------------------------------------------------------------
#| label: plot-priorpost
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
df <- tibble(
  etapa = factor(c("Prior P(V)", "Posterior P(V | +)"),
                 levels = c("Prior P(V)", "Posterior P(V | +)")),
  prob = c(0.10, 0.40))
p <- ggplot(df, aes(x = etapa, y = prob, fill = etapa)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.4, size = 5.5, color = azul) +
  scale_fill_manual(values = c(celeste, rojo)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = NULL, y = "Probabilidad",
       title = "Focalización: prior vs posterior")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: bayes-png
#| echo: false
#| out.width: "92%"
knitr::include_graphics("figuras/bayes_ejemplo.png")


## -----------------------------------------------------------------------------
#| label: plot-prevalencia
#| echo: false
#| fig-height: 3.1
df <- tibble(prev = seq(0.005, 0.60, by = 0.005)) %>%
  mutate(post = 0.90 * prev / (0.90 * prev + 0.15 * (1 - prev)))
pts <- tibble(prev = c(0.01, 0.10, 0.30), post = c(0.057, 0.400, 0.720))
p <- ggplot(df, aes(x = prev, y = post)) +
  geom_line(color = azul, linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
  geom_point(data = pts, color = rojo, size = 3) +
  annotate("text", x = 0.10, y = 0.50, label = "prev 0.10 -> post 0.40", color = rojo, size = 4.2) +
  annotate("text", x = 0.34, y = 0.63, label = "prev 0.30 -> post 0.72", color = rojo, size = 4.2) +
  labs(x = "Prevalencia P(V)", y = "Posterior P(V | +)",
       title = "Mismo instrumento (sens 0.90, espec 0.85), distinta poblacion")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-indep-verifica
round(c(P_TyE = 120/500, P_T_x_P_E = (200/500) * (210/500),
        P_E_dado_T = 120/200, P_E = 210/500), 3)


## -----------------------------------------------------------------------------
#| label: tipos-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/tipos_variables.png")


## -----------------------------------------------------------------------------
#| label: plot-pmf
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
df <- tibble(x = 0:3, p = c(0.35, 0.30, 0.20, 0.15),
             zona = c("X <= 2", "X <= 2", "X <= 2", "X = 3"))
p <- ggplot(df, aes(x = x, y = p, fill = zona)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.2f", p)), vjust = -0.4, size = 4.8, color = azul) +
  scale_fill_manual(values = c(celeste, "grey70")) +
  scale_y_continuous(limits = c(0, 0.42)) +
  labs(x = "x = postulaciones aceptadas", y = "p(x)", fill = NULL,
       title = "PMF: P(X <= 2) = suma de barras azules") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-pmf-verifica
p <- c(0.35, 0.30, 0.20, 0.15)
c(suma = sum(p), P_le_2 = sum(p[1:3]), P_gt_1 = sum(p[3:4]))


## -----------------------------------------------------------------------------
#| label: code-cdf
#| eval: false
# esc <- tibble(
#   x = c(-1, 0, 1, 2, 3, 4),
#   Fx = c(0, .35, .65, .85, 1, 1))
# 
# ggplot(esc, aes(x, Fx)) +
#   geom_step(direction = "hv",
#             color = azul,
#             linewidth = 1) +
#   geom_point(
#     data = filter(esc, x %in% 0:3),
#     color = rojo, size = 2.5) +
#   labs(x = "x", y = "F(x)")


## -----------------------------------------------------------------------------
#| label: plot-cdf
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
esc <- tibble(
  x = c(-1, 0, 1, 2, 3, 4),
  Fx = c(0, .35, .65, .85, 1, 1))
interactivo(
  ggplot(esc, aes(x, Fx)) +
    geom_step(direction = "hv", color = azul, linewidth = 1) +
    geom_point(data = filter(esc, x %in% 0:3),
               color = rojo, size = 2.5) +
    labs(x = "x", y = "F(x)",
         title = "CDF de las postulaciones aceptadas")
)


## -----------------------------------------------------------------------------
#| label: plot-area
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
df <- tibble(x = seq(-0.5, 10.5, 0.01),
             f = if_else(x >= 0 & x <= 10, 0.1, 0))
sombra <- filter(df, x >= 2, x <= 5)
p <- ggplot(df, aes(x, f)) +
  geom_area(data = sombra, fill = celeste, alpha = 0.7) +
  geom_line(color = azul, linewidth = 1.1) +
  annotate("text", x = 3.5, y = 0.05, label = "Area = 0.30",
           color = azul, size = 5) +
  coord_cartesian(ylim = c(0, 0.15)) +
  labs(x = "x = minutos de espera", y = "f(x)",
       title = "P(2 <= X <= 5) es el area bajo f")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-unif-verifica
round(punif(5, 0, 10) - punif(2, 0, 10), 2)

