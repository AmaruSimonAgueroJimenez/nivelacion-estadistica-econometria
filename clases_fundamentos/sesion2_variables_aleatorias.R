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
#| label: plot-pmf-img
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/pmf_ejemplo.png")


## -----------------------------------------------------------------------------
#| label: plot-esperanza
#| echo: false
#| fig-height: 3.0
d_ex <- tibble(x = c(-4, 5, 12), p = c(0.2, 0.5, 0.3))
mu_ex <- sum(d_ex$x * d_ex$p)
p <- ggplot(d_ex, aes(x = x, y = p)) +
  geom_segment(aes(xend = x, y = 0, yend = p), color = celeste, linewidth = 1.4) +
  geom_point(aes(size = p), color = azul, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_segment(x = mu_ex, xend = mu_ex, y = 0.4, yend = 0.02,
               color = rojo, linewidth = 1, arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", x = mu_ex + 0.4, y = 0.45,
           label = paste0("E[X] = ", mu_ex, ": punto de equilibrio"),
           color = rojo, size = 4.5, hjust = 0) +
  scale_size(range = c(3, 7)) +
  scale_x_continuous(breaks = c(-4, 0, 5, 12)) +
  labs(x = "Beneficio neto x (miles de USD)", y = "Masa de probabilidad p(x)",
       title = "Masas p(x) sobre una barra: E[X] la equilibra")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-esperanza
x <- c(12, 5, -4); p <- c(0.3, 0.5, 0.2)
sum(x * p)


## -----------------------------------------------------------------------------
#| label: ej-linealidad
c(directo = sum((0.9 * x - 1) * p), por_linealidad = 0.9 * 5.3 - 1)  # Y = 0.9X - 1


## -----------------------------------------------------------------------------
#| label: ej-varprop
# Continuando: Var(X) = 30.81  =>  Var(0.9X - 1) = 0.81 * 30.81
c(directo = sum((0.9 * x - 1 - 3.77)^2 * p), por_propiedad = 0.9^2 * 30.81)


## -----------------------------------------------------------------------------
#| label: ej-varianza
c(def = sum((x - 5.3)^2 * p), alternativa = sum(x^2 * p) - 5.3^2, de = sqrt(30.81)) |> round(3)


## -----------------------------------------------------------------------------
#| label: plot-cuadrantes
#| echo: false
#| fig-height: 3.3
set.seed(21)
n <- 120
inv <- rnorm(n, 100, 25)
asis <- 70 + 0.15 * inv + rnorm(n, 0, 4)
dq <- tibble(inv, asis) %>%
  mutate(signo = if_else((inv - mean(inv)) * (asis - mean(asis)) > 0,
                         "producto (+)", "producto (-)"))
p <- ggplot(dq, aes(inv, asis, color = signo)) +
  geom_point(alpha = 0.75, size = 2) +
  geom_vline(xintercept = mean(dq$inv), color = azul, linetype = "dashed") +
  geom_hline(yintercept = mean(dq$asis), color = azul, linetype = "dashed") +
  annotate("text", x = 145, y = 92, label = "(+)(+) suma", color = verde, size = 4.2) +
  annotate("text", x = 48, y = 74, label = "(-)(-) suma", color = verde, size = 4.2) +
  annotate("text", x = 48, y = 92, label = "(-)(+) resta", color = rojo, size = 4.2) +
  annotate("text", x = 145, y = 74, label = "(+)(-) resta", color = rojo, size = 4.2) +
  scale_color_manual(values = c(`producto (+)` = verde, `producto (-)` = rojo)) +
  labs(x = "Inversión social per cápita (miles de $)", y = "Asistencia escolar (%)",
       color = NULL, title = "Comunas: las medias dividen el plano en 4 cuadrantes") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-correlacion
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
set.seed(33)
sim_rho <- function(r, n = 150) {
  x <- rnorm(n)
  tibble(x = x, y = r * x + sqrt(1 - r^2) * rnorm(n),
         panel = paste0("rho = ", r))
}
d_rho <- bind_rows(sim_rho(-0.9), sim_rho(0.3), sim_rho(0.95)) %>%
  mutate(panel = factor(panel, levels = c("rho = -0.9", "rho = 0.3", "rho = 0.95")))
p <- ggplot(d_rho, aes(x, y)) +
  geom_point(alpha = .5, size = 1.1, color = celeste) +
  facet_wrap(~panel, nrow = 2, scales = "free") +
  labs(x = NULL, y = NULL, title = "Tres intensidades de asociación lineal")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: ej-bivariada
pxy <- c(p00 = .30, p01 = .20, p10 = .15, p11 = .35)
c(EXY = unname(pxy["p11"]), cov = .35 - .5 * .55, rho = .075 / sqrt(.25 * .2475)) |> round(3)


## -----------------------------------------------------------------------------
#| label: plot-venn-bivariado
#| echo: false
#| fig-width: 5.4
#| fig-height: 3.1
#| out.width: "100%"
venn_df <- tibble(x0 = c(-0.55, 0.55), y0 = 0, r = 1.05,
                  evento = c("A", "B"))
ggplot() +
  ggforce::geom_circle(data = venn_df,
                       aes(x0 = x0, y0 = y0, r = r, fill = evento),
                       alpha = 0.35, color = "grey35", linewidth = 0.4,
                       show.legend = FALSE) +
  scale_fill_manual(values = c(A = celeste, B = naranja)) +
  annotate("text", x = -1.05, y = 0, label = "0.15", size = 5, color = azul) +
  annotate("text", x = 0, y = 0, label = "0.35", size = 5.5,
           fontface = "bold", color = azul) +
  annotate("text", x = 1.05, y = 0, label = "0.20", size = 5, color = azul) +
  annotate("text", x = -1.15, y = 1.35, label = "A: se capacitó (0.50)",
           size = 4.3, color = azul) +
  annotate("text", x = 1.15, y = 1.35, label = "B: empleo formal (0.55)",
           size = 4.3, color = naranja) +
  annotate("text", x = 1.9, y = -1.3, label = "ninguno: 0.30",
           size = 4.1, color = "grey40") +
  coord_fixed(xlim = c(-2.5, 2.7), ylim = c(-1.55, 1.6)) +
  theme_void()


## -----------------------------------------------------------------------------
#| label: ej-varsuma
s <- c(0, 1, 1, 2)  # S = X + Y en el ejemplo bivariado
c(directa = sum(s^2 * pxy) - sum(s * pxy)^2, formula = .25 + .2475 + 2 * .075)


## -----------------------------------------------------------------------------
#| label: plot-binpois-img
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/binomial_poisson.png")


## -----------------------------------------------------------------------------
#| label: code-binom
#| eval: false
# # 10 licitaciones auditadas;
# # p = 0.3 de irregularidad
# d <- tibble(k = 0:10,
#             pk = dbinom(0:10, 10, .3))
# ggplot(d, aes(k, pk,
#        fill = k >= 5)) +
#   geom_col(show.legend = FALSE) +
#   scale_fill_manual(
#     values = c(celeste, rojo)) +
#   scale_x_continuous(
#     breaks = 0:10) +
#   labs(x = "k irregularidades",
#        y = "P(X = k)")
# 
# 1 - pbinom(4, 10, .3)  # P(X >= 5)


## -----------------------------------------------------------------------------
#| label: plot-binom
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
d_b <- tibble(k = 0:10, pk = dbinom(0:10, 10, .3))
interactivo(
  ggplot(d_b, aes(k, pk, fill = k >= 5)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c(`FALSE` = celeste, `TRUE` = rojo)) +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "k irregularidades", y = "P(X = k)",
         title = "X ~ Bin(10, 0.3): E[X] = 3, DE = 1.45")
)


## -----------------------------------------------------------------------------
#| label: pdf-lab-binom
#| echo: false
#| fig-height: 2.6
#| fig-width: 8
#| out.width: "92%"
d_lab3 <- bind_rows(
  tibble(k = 0:30, pk = dbinom(0:30, 10, .3), panel = "n = 10, p = 0.3"),
  tibble(k = 0:30, pk = dbinom(0:30, 30, .3), panel = "n = 30, p = 0.3"),
  tibble(k = 0:30, pk = dbinom(0:30, 30, .7), panel = "n = 30, p = 0.7"))
d_mu3 <- tibble(panel = unique(d_lab3$panel), np = c(3, 9, 21))
ggplot(d_lab3, aes(k, pk)) +
  geom_col(fill = celeste, width = .85) +
  geom_vline(data = d_mu3, aes(xintercept = np),
             color = rojo, linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~panel, nrow = 1) +
  labs(x = "k éxitos", y = "P(X = k)")


## -----------------------------------------------------------------------------
#| label: plot-normal
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
xx <- seq(-4, 4, length.out = 300)
dn <- tibble(x = xx, f = dnorm(xx))
p <- ggplot(dn, aes(x, f)) +
  geom_line(color = azul, linewidth = 1.1) +
  geom_point(data = filter(dn, abs(abs(x) - 1) < .015),
             color = rojo, size = 3) +
  geom_vline(xintercept = c(-1, 1), linetype = "dotted", color = rojo) +
  annotate("text", x = 2.4, y = 0.26,
           label = "inflexión en mu ± sigma", color = rojo, size = 4.2) +
  labs(x = "x (en unidades de sigma desde mu)", y = "f(x)",
       title = "Densidad N(0, 1)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: pdf-lab-normal
#| echo: false
#| fig-height: 2.7
#| fig-width: 7.5
#| out.width: "80%"
xx_l1 <- seq(-8, 8, length.out = 400)
d_lab1 <- bind_rows(
  tibble(x = xx_l1, f = dnorm(xx_l1, 0, 1),   curva = "N(0, 1)"),
  tibble(x = xx_l1, f = dnorm(xx_l1, 1.5, 1), curva = "N(1.5, 1)"),
  tibble(x = xx_l1, f = dnorm(xx_l1, 0, 2),   curva = "N(0, 2²)"))
d_inf1 <- tibble(x = c(-1, 1, 0.5, 2.5, -2, 2),
                 f = c(dnorm(-1), dnorm(1), dnorm(-1), dnorm(1),
                       dnorm(-2, 0, 2), dnorm(2, 0, 2)))
ggplot(d_lab1, aes(x, f, color = curva)) +
  geom_line(linewidth = 1) +
  geom_point(data = d_inf1, aes(x, f), inherit.aes = FALSE,
             color = rojo, size = 2) +
  scale_color_manual(values = c(`N(0, 1)` = azul, `N(1.5, 1)` = naranja,
                                `N(0, 2²)` = celeste)) +
  labs(x = "x", y = "f(x)", color = NULL) +
  theme(legend.position = "top")


## -----------------------------------------------------------------------------
#| label: plot-regla-img
#| echo: false
#| out.width: "46%"
knitr::include_graphics("figuras/distribucion_normal.png")


## -----------------------------------------------------------------------------
#| label: ej-normal1
c(elegible = pnorm(440, 520, 80), franja = pnorm(600, 520, 80) - pnorm(440, 520, 80)) |> round(4)


## -----------------------------------------------------------------------------
#| label: plot-ej1
#| echo: false
#| fig-height: 3.1
xx <- seq(240, 800, length.out = 400)
d1 <- tibble(x = xx, f = dnorm(xx, 520, 80))
p <- ggplot(d1, aes(x, f)) +
  geom_area(data = filter(d1, x <= 440), fill = rojo, alpha = .65) +
  geom_area(data = filter(d1, x >= 440, x <= 600), fill = celeste, alpha = .5) +
  geom_line(color = azul, linewidth = 1) +
  geom_vline(xintercept = c(440, 600), linetype = "dashed", color = "grey40") +
  annotate("text", x = 360, y = 0.0006, label = "0.1587", color = rojo, size = 4.6) +
  annotate("text", x = 520, y = 0.0019, label = "0.6827", color = azul, size = 4.6) +
  annotate("text", x = 680, y = 0.00085, label = "0.1587", color = "grey40", size = 4.6) +
  labs(x = "Puntaje de focalización", y = "Densidad",
       title = "X ~ N(520, 80²): elegibles (rojo) y franja 440-600 (celeste)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: pdf-lab-intervalo
#| echo: false
#| fig-height: 2.6
#| fig-width: 8
#| out.width: "88%"
xx_l2 <- seq(150, 900, length.out = 500)
d_lab2 <- bind_rows(
  tibble(x = xx_l2, f = dnorm(xx_l2, 520, 80), a = 440,
         panel = "a = 440: P(X < a) = 0.159"),
  tibble(x = xx_l2, f = dnorm(xx_l2, 520, 80), a = 520,
         panel = "a = 520: P(X < a) = 0.500"))
ggplot(d_lab2, aes(x, f)) +
  geom_area(data = filter(d_lab2, x <= a), fill = rojo, alpha = .55) +
  geom_line(color = azul, linewidth = 0.9) +
  geom_vline(aes(xintercept = a), linetype = "dashed", color = naranja) +
  facet_wrap(~panel, nrow = 1) +
  labs(x = "Puntaje de focalización", y = "Densidad")


## -----------------------------------------------------------------------------
#| label: ej-normal2
c(pobreza = pnorm(240, 450, 150), vulnerable = pnorm(450, 450, 150) - pnorm(240, 450, 150)) |> round(4)


## -----------------------------------------------------------------------------
#| label: code-ej2
#| eval: false
# xx <- seq(0, 900, 2)
# d <- tibble(x = xx,
#             f = dnorm(xx, 450, 150))
# ggplot(d, aes(x, f)) +
#   geom_area(
#     data = filter(d, x <= 240),
#     fill = rojo, alpha = .65) +
#   geom_line(color = azul,
#             linewidth = 1) +
#   geom_vline(xintercept = 240,
#              linetype = "dashed") +
#   labs(x = "Ingreso (miles de $)",
#        y = "Densidad")
# 
# pnorm(240, 450, 150)
# #> 0.0808


## -----------------------------------------------------------------------------
#| label: plot-ej2
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
xx2 <- seq(0, 900, 2)
d2 <- tibble(x = xx2, f = dnorm(xx2, 450, 150))
interactivo(
  ggplot(d2, aes(x, f)) +
    geom_area(data = filter(d2, x <= 240), fill = rojo, alpha = .65) +
    geom_line(color = azul, linewidth = 1) +
    geom_vline(xintercept = 240, linetype = "dashed", color = "grey40") +
    annotate("text", x = 155, y = 0.00045, label = "0.0808",
             color = rojo, size = 4.6) +
    annotate("text", x = 245, y = 0.00265, label = "L = 240",
             color = "grey30", size = 4.2, hjust = 0) +
    labs(x = "Ingreso per cápita (miles de $)", y = "Densidad",
         title = "P(X < 240) con X ~ N(450, 150²)")
)


## -----------------------------------------------------------------------------
#| label: ej-qnorm
round(qnorm(0.20, 520, 80), 1)


## -----------------------------------------------------------------------------
#| label: plot-qnorm
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
u <- qnorm(0.20, 520, 80)
xx3 <- seq(240, 800, length.out = 400)
d3 <- tibble(x = xx3, f = dnorm(xx3, 520, 80))
p <- ggplot(d3, aes(x, f)) +
  geom_area(data = filter(d3, x <= u), fill = naranja, alpha = .7) +
  geom_line(color = azul, linewidth = 1) +
  geom_vline(xintercept = u, linetype = "dashed", color = naranja) +
  annotate("text", x = 395, y = 0.0008, label = "área = 0.20",
           color = "grey20", size = 4.4) +
  annotate("text", x = 660, y = 0.0035, label = "u = 452.7",
           color = naranja, size = 4.6) +
  labs(x = "Puntaje de focalización", y = "Densidad",
       title = "Cuantil 0.20 de N(520, 80²)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-programas
#| echo: false
#| fig-height: 3.0
xx4 <- seq(-15, 35, length.out = 500)
d4 <- bind_rows(
  tibble(x = xx4, f = dnorm(xx4, 10, 3), programa = "A: N(10, 3²)"),
  tibble(x = xx4, f = dnorm(xx4, 10, 8), programa = "B: N(10, 8²)")
)
p <- ggplot(d4, aes(x, f, color = programa)) +
  geom_area(data = filter(d4, programa == "B: N(10, 8²)", x <= 0),
            aes(x, f), inherit.aes = FALSE, fill = rojo, alpha = .75) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(celeste, naranja)) +
  annotate("text", x = -7, y = 0.033, label = "P(B < 0) = 0.106", color = rojo, size = 4.4) +
  labs(x = "Efecto sobre el aprendizaje (puntos)", y = "Densidad", color = NULL,
       title = "Dos programas con E = 10: la varianza es el riesgo") +
  theme(legend.position = "top")
interactivo(p)

