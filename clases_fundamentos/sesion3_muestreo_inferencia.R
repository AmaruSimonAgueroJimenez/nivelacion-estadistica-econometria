## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(knitr)
library(broom)
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
#| label: plot-dist-muestral-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribucion_muestral.png")


## -----------------------------------------------------------------------------
#| label: plot-sim-muestral
#| echo: false
#| fig-height: 3.2
set.seed(123)
pobl <- tibble(valor = rexp(20000, rate = 1/5),
               dist = "Población: dias de tramite, Exp(1/5), media 5")
medias <- tibble(valor = replicate(5000, mean(rexp(30, rate = 1/5))),
                 dist = "5.000 medias muestrales con n = 30")
df_sim <- bind_rows(pobl, medias) %>%
  mutate(dist = factor(dist, levels = c(
    "Población: dias de tramite, Exp(1/5), media 5",
    "5.000 medias muestrales con n = 30")))
p <- ggplot(df_sim, aes(x = valor)) +
  geom_histogram(bins = 45, fill = celeste, color = "white") +
  geom_vline(xintercept = 5, color = rojo, linetype = "dashed", linewidth = 0.9) +
  facet_wrap(~dist, scales = "free") +
  labs(x = "Dias de tramitacion", y = "Frecuencia")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-lgn-trayectorias
#| echo: false
#| fig-height: 3.2
set.seed(2026)
p_verdadero <- 0.6
tray <- map_dfr(1:5, function(k) {
  x <- rbinom(1000, 1, p_verdadero)
  tibble(encuestador = paste("Muestra", k), n = 1:1000, media = cummean(x))
})
p <- ggplot(tray, aes(n, media, color = encuestador)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_hline(yintercept = p_verdadero, linetype = "dashed", color = azul, linewidth = 1) +
  annotate("text", x = 930, y = 0.635, label = "p = 0.60", color = azul, size = 4.2) +
  scale_color_manual(values = c(celeste, naranja, verde, rojo, "#8E7CC3")) +
  coord_cartesian(ylim = c(0.3, 0.9)) +
  labs(x = "Tamaño muestral acumulado n", y = "Proporción muestral acumulada",
       color = NULL, title = "Cinco encuestas independientes: apoyo verdadero p = 0.60") +
  theme(legend.position = "none")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-tlc-sim
#| echo: false
#| fig-height: 3.0
set.seed(456)
ns <- c(2, 10, 30, 100)
tlc <- map_dfr(ns, function(n) {
  tibble(n = n, media = replicate(3000, mean(rexp(n, rate = 1/5))))
}) %>% mutate(fac = factor(paste0("n = ", n), levels = paste0("n = ", ns)))
curvas <- map_dfr(ns, function(n) {
  s <- 5 / sqrt(n)
  tibble(fac = factor(paste0("n = ", n), levels = paste0("n = ", ns)),
         x = seq(5 - 4 * s, 5 + 4 * s, length.out = 200),
         d = dnorm(seq(5 - 4 * s, 5 + 4 * s, length.out = 200), 5, s))
})
p <- ggplot(tlc, aes(x = media)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = celeste, color = "white") +
  geom_line(data = curvas, aes(x = x, y = d), color = rojo, linewidth = 0.8) +
  facet_wrap(~fac, nrow = 1, scales = "free") +
  labs(x = "Media muestral de Exp(1/5)", y = "Densidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-tlc-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/tlc_simulacion.png")


## -----------------------------------------------------------------------------
#| label: plot-insesgadez
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
xs <- seq(40, 68, length.out = 400)
dfi <- bind_rows(
  tibble(x = xs, d = dnorm(xs, 50, 3), tipo = "Insesgado: E = theta"),
  tibble(x = xs, d = dnorm(xs, 57, 3), tipo = "Sesgado: E = theta + 7"))
p <- ggplot(dfi, aes(x, d, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = azul) +
  annotate("text", x = 50, y = 0.145, label = "theta", color = azul, size = 4.5) +
  scale_color_manual(values = c(verde, rojo)) +
  labs(x = "Valores del estimador", y = "Densidad", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: sim-varianza
set.seed(2026)
sims <- replicate(20000, {
  x <- rnorm(5, mean = 50, sd = 10)          # sigma^2 = 100, n = 5
  c(divide_n = sum((x - mean(x))^2) / 5, divide_n1 = var(x))
})
round(rowMeans(sims), 1)


## -----------------------------------------------------------------------------
#| label: plot-eficiencia
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
se_m <- 10 / sqrt(30); se_md <- sqrt(pi / 2) * se_m
xs2 <- seq(43, 57, length.out = 400)
dfe <- bind_rows(
  tibble(x = xs2, d = dnorm(xs2, 50, se_m), est = "Media (eficiente)"),
  tibble(x = xs2, d = dnorm(xs2, 50, se_md), est = "Mediana (Var 1.57x)"))
p <- ggplot(dfe, aes(x, d, color = est)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = azul) +
  scale_color_manual(values = c(verde, naranja)) +
  labs(x = "Valores del estimador (theta = 50)", y = "Densidad", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-consistencia
#| echo: false
#| fig-width: 5.4
#| fig-height: 3.4
#| out.width: "100%"
xs3 <- seq(42, 58, length.out = 500)
dfc <- map_dfr(c(10, 50, 500), function(n) {
  tibble(n_lab = factor(paste0("n = ", n), levels = paste0("n = ", c(10, 50, 500))),
         x = xs3, d = dnorm(xs3, 50, 15 / sqrt(n)))
})
p <- ggplot(dfc, aes(x, d, color = n_lab)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = azul) +
  scale_color_manual(values = c(naranja, celeste, verde)) +
  labs(x = "Distribucion de la media muestral (theta = 50)",
       y = "Densidad", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-dianas
#| echo: false
#| fig-height: 2.9
set.seed(99)
ang <- seq(0, 2 * pi, length.out = 120)
casos <- tribble(
  ~caso, ~sx, ~sy, ~disp,
  "Insesgado y preciso", 0, 0, 0.35,
  "Insesgado, impreciso", 0, 0, 1.05,
  "Sesgado, preciso", 1.6, 1.3, 0.35,
  "Sesgado e impreciso", 1.6, 1.3, 1.05)
orden <- casos$caso
tiros <- casos %>% rowwise() %>%
  reframe(caso = caso, x = rnorm(15, sx, disp), y = rnorm(15, sy, disp))
circulos <- map_dfr(orden, function(cs) {
  map_dfr(c(1, 2, 3), function(r) {
    tibble(caso = cs, r = r, cx = r * cos(ang), cy = r * sin(ang))
  })
})
tiros <- tiros %>% mutate(caso = factor(caso, levels = orden))
circulos <- circulos %>% mutate(caso = factor(caso, levels = orden))
p <- ggplot() +
  geom_path(data = circulos, aes(cx, cy, group = r), color = "grey55", linewidth = 0.4) +
  geom_point(data = tiros, aes(x, y), color = rojo, size = 1.6, alpha = 0.8) +
  annotate("point", x = 0, y = 0, shape = 3, size = 2.5, color = azul, stroke = 1) +
  facet_wrap(~caso, nrow = 1) +
  coord_fixed(xlim = c(-3.3, 3.3), ylim = c(-3.3, 3.3)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid = element_blank())
p


## -----------------------------------------------------------------------------
#| label: plot-t-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribuciones_t.png")


## -----------------------------------------------------------------------------
#| label: code-ic-verifica
set.seed(123)
ingresos <- as.numeric(scale(rnorm(36))) * 90 + 520   # muestra con xbar = 520, s = 90 exactos
c(media = mean(ingresos), de = sd(ingresos), se = sd(ingresos) / sqrt(36))


## -----------------------------------------------------------------------------
#| label: code-ic-tidy
tidy(t.test(ingresos)) %>%
  select(estimate, conf.low, conf.high) %>% kable(digits = 2)


## -----------------------------------------------------------------------------
#| label: code-cobertura
#| eval: false
# set.seed(1)
# ics <- map_dfr(1:100, function(i) {
#   x <- rnorm(30, mean = 50, sd = 10)
#   ci <- t.test(x)$conf.int
#   tibble(id = i, li = ci[1],
#          ls = ci[2])
# })
# ics <- ics %>%
#   mutate(cubre = li <= 50 & ls >= 50)
# sum(ics$cubre)   # 95 de 100


## -----------------------------------------------------------------------------
#| label: plot-cobertura
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
set.seed(1)
ics <- map_dfr(1:100, function(i) {
  x <- rnorm(30, mean = 50, sd = 10)
  ci <- t.test(x)$conf.int
  tibble(id = i, li = ci[1], ls = ci[2])
}) %>% mutate(cubre = ifelse(li <= 50 & ls >= 50,
                             "Contiene a mu", "No contiene"))
interactivo(
  ggplot(ics) +
    geom_segment(aes(x = id, xend = id, y = li, yend = ls, color = cubre),
                 linewidth = 0.7) +
    geom_hline(yintercept = 50, color = azul, linewidth = 0.7) +
    scale_color_manual(values = c("Contiene a mu" = verde, "No contiene" = rojo)) +
    labs(x = "Muestra", y = "IC 95%", color = NULL) +
    theme(legend.position = "top")
)


## -----------------------------------------------------------------------------
#| label: plot-ancho-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/ancho_ic.png")


## -----------------------------------------------------------------------------
#| label: code-test-verifica
set.seed(42)
puntajes <- as.numeric(scale(rnorm(25))) * 24 + 268   # xbar = 268, s = 24 exactos
tidy(t.test(puntajes, mu = 256)) %>%
  select(estimate, statistic, p.value, conf.low, conf.high) %>% kable(digits = 4)


## -----------------------------------------------------------------------------
#| label: plot-matriz-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/matriz_decision.png")


## -----------------------------------------------------------------------------
#| label: plot-errores
#| echo: false
#| fig-height: 3.2
xs4 <- seq(-4, 6.5, length.out = 600)
h0 <- tibble(x = xs4, d = dnorm(xs4, 0, 1))
h1 <- tibble(x = xs4, d = dnorm(xs4, 2.5, 1))
p <- ggplot() +
  geom_area(data = filter(h1, x <= 1.96), aes(x, d), fill = naranja, alpha = 0.55) +
  geom_area(data = filter(h1, x > 1.96), aes(x, d), fill = verde, alpha = 0.45) +
  geom_area(data = filter(h0, x >= 1.96), aes(x, d), fill = rojo, alpha = 0.65) +
  geom_line(data = h0, aes(x, d), color = azul, linewidth = 0.9) +
  geom_line(data = h1, aes(x, d), color = verde, linewidth = 0.9) +
  geom_vline(xintercept = 1.96, linetype = "dashed", color = "grey30") +
  annotate("text", x = -1.5, y = 0.33, label = "H0: N(0,1)", color = azul, size = 4.2) +
  annotate("segment", x = 3.9, y = 0.055, xend = 2.3, yend = 0.008,
           color = rojo, linewidth = 0.4) +
  annotate("text", x = 4.55, y = 0.075, label = "alfa/2 = 0.025", color = rojo, size = 3.8) +
  annotate("segment", x = -1.35, y = 0.125, xend = 0.85, yend = 0.04,
           color = naranja, linewidth = 0.4) +
  annotate("text", x = -1.85, y = 0.145, label = "beta = 0.295", color = naranja, size = 3.8) +
  annotate("text", x = 3.5, y = 0.15, label = "potencia = 0.705", color = "grey20", size = 3.8) +
  annotate("text", x = 5.35, y = 0.30, label = "H1: N(2.5,1)", color = verde, size = 4.2) +
  labs(x = "Estadistico de contraste (en unidades de SE)", y = "Densidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-pvalor
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
xs5 <- seq(-4, 4, length.out = 500)
dt24 <- tibble(x = xs5, d = dt(xs5, 24))
p <- ggplot(dt24, aes(x, d)) +
  geom_line(color = azul, linewidth = 0.9) +
  geom_area(data = filter(dt24, x >= 2.5), fill = rojo, alpha = 0.7) +
  geom_area(data = filter(dt24, x <= -2.5), fill = rojo, alpha = 0.7) +
  geom_vline(xintercept = c(-2.5, 2.5), linetype = "dashed", color = rojo) +
  annotate("text", x = 3.15, y = 0.075, label = "area total =", color = rojo, size = 3.8) +
  annotate("text", x = 3.15, y = 0.052, label = "valor-p = 0.0197", color = rojo, size = 3.8) +
  labs(x = "T ~ t con 24 gl, t observado = 2.5", y = "Densidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-potencia-png
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/potencia_estadistica.png")


## -----------------------------------------------------------------------------
#| label: code-potencia
#| eval: false
# d <- seq(2, 20, by = 0.5)
# pot <- map_dbl(d, function(delta)
#   power.t.test(n = 25, delta = delta,
#     sd = 24, sig.level = 0.05,
#     type = "one.sample")$power)


## -----------------------------------------------------------------------------
#| label: plot-potencia-curva
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
d <- seq(2, 20, by = 0.5)
curva <- map_dfr(c(25, 73), function(n) {
  tibble(n_lab = paste0("n = ", n), delta = d,
         potencia = map_dbl(d, function(x)
           power.t.test(n = n, delta = x, sd = 24, sig.level = 0.05,
                        type = "one.sample")$power))
})
interactivo(
  ggplot(curva, aes(delta, potencia, color = n_lab)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey30") +
    geom_vline(xintercept = 8, linetype = "dotted", color = azul) +
    scale_color_manual(values = c(celeste, verde)) +
    labs(x = "Tamano del efecto delta (puntos de prueba)",
         y = "Potencia (1 - beta)", color = NULL) +
    theme(legend.position = "top")
)

