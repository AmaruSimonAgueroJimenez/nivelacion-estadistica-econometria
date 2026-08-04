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
#| label: plot-pmf-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/pmf_ejemplo.png")


## -----------------------------------------------------------------------------
#| label: plot-pdf-cdf
#| echo: false
#| fig-height: 3.0
xs <- seq(200, 1400, 2)
vpdf <- "PDF: probabilidad = area"
vcdf <- "CDF: probabilidad = altura"
lv <- c(vpdf, vcdf)
d <- bind_rows(
  tibble(x = xs, y = dnorm(xs, 800, 200), vista = vpdf),
  tibble(x = xs, y = pnorm(xs, 800, 200), vista = vcdf)) %>%
  mutate(vista = factor(vista, lv))
seg <- tibble(x = c(600, 200), xend = c(600, 600), y = c(0, 0.159),
              yend = c(0.159, 0.159), vista = factor(vcdf, lv))
pt <- tibble(x = 600, y = 0.159, vista = factor(vcdf, lv))
txt <- tibble(x = c(440, 400), y = c(0.00042, 0.28),
              lab = c("area = 0.16", "F(600) = 0.16"),
              vista = factor(lv, lv))
p <- ggplot(d, aes(x, y)) +
  geom_area(data = filter(d, vista == vpdf & x <= 600),
            fill = celeste, alpha = 0.6) +
  geom_line(color = azul, linewidth = 1) +
  geom_segment(data = seg, aes(x = x, xend = xend, y = y, yend = yend),
               linetype = "dashed", color = rojo) +
  geom_point(data = pt, aes(x, y), color = rojo, size = 2.5) +
  geom_text(data = filter(txt, vista == vpdf), aes(x, y, label = lab),
            color = azul, size = 4) +
  geom_text(data = filter(txt, vista == vcdf), aes(x, y, label = lab),
            color = rojo, size = 4) +
  facet_wrap(~ vista, scales = "free_y") +
  labs(x = "Ingreso (miles de $)", y = NULL)
interactivo(p)


## -----------------------------------------------------------------------------
#| label: esperanza
x <- 0:3                              # atenciones anuales por beneficiario
p <- c(0.20, 0.35, 0.30, 0.15)        # sus probabilidades
mu <- sum(x * p)
round(c(esperanza = mu, varianza = sum((x - mu)^2 * p),
        de = sqrt(sum((x - mu)^2 * p))), 2)


## -----------------------------------------------------------------------------
#| label: plot-var-bernoulli
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
p <- tibble(p = seq(0, 1, 0.01), v = p * (1 - p)) %>%
  ggplot(aes(p, v)) +
  geom_line(color = azul, linewidth = 1.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = rojo) +
  annotate("text", x = 0.5, y = 0.27, label = "maximo en p = 0.5",
           color = rojo, size = 4.2) +
  coord_cartesian(ylim = c(0, 0.29)) +
  labs(x = "p", y = "Var(X) = p(1-p)",
       title = "La incertidumbre es maxima en p = 0.5")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: binom-momentos
n <- 30; p <- 0.7    # 30 hogares; 70% con acceso a internet
round(c(esperanza = n * p, de = sqrt(n * p * (1 - p)),
        p_igual_20 = dbinom(20, n, p),
        p_25_o_mas = 1 - pbinom(24, n, p)), 3)


## -----------------------------------------------------------------------------
#| label: code-binom
#| eval: false
# n <- 30; p <- 0.7
# df <- tibble(k = 0:n,
#              prob = dbinom(k, n, p))
# ggplot(df, aes(k, prob)) +
#   geom_col(fill = celeste,
#            color = "white") +
#   geom_vline(xintercept = n * p,
#              color = rojo,
#              linetype = "dashed",
#              linewidth = 1) +
#   labs(x = "Hogares con internet (k)",
#        y = "P(X = k)")


## -----------------------------------------------------------------------------
#| label: plot-binom
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
n <- 30; p <- 0.7
df <- tibble(k = 0:n, prob = dbinom(k, n, p))
interactivo(
  ggplot(df, aes(k, prob)) +
    geom_col(fill = celeste, color = "white") +
    geom_vline(xintercept = n * p, color = rojo,
               linetype = "dashed", linewidth = 1) +
    labs(x = "Hogares con internet (k)", y = "P(X = k)")
)


## -----------------------------------------------------------------------------
#| label: plot-binom-facetas
#| echo: false
#| fig-height: 3.3
bin <- crossing(n = c(10, 50), p = c(0.2, 0.5, 0.8)) %>%
  mutate(datos = map2(n, p, \(n, p) tibble(k = 0:n, prob = dbinom(0:n, n, p)))) %>%
  unnest(datos) %>%
  mutate(fila = fct_inorder(paste0("n = ", n)),
         col = fct_inorder(paste0("p = ", p)))
p <- ggplot(bin, aes(k, prob)) +
  geom_col(fill = celeste, width = 0.9) +
  facet_grid(fila ~ col, scales = "free") +
  labs(x = "Numero de exitos (k)", y = "P(X = k)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-binpois-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/binomial_poisson.png")


## -----------------------------------------------------------------------------
#| label: code-pois
lambda <- 3   # delitos por semana
round(c(p_0 = dpois(0, lambda),
        p_3 = dpois(3, lambda),
        p_6_o_mas =
          1 - ppois(5, lambda)), 3)


## -----------------------------------------------------------------------------
#| label: plot-pois-cola
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
d <- tibble(k = 0:12, prob = dpois(k, 3))
p <- ggplot(d, aes(k, prob)) +
  geom_col(data = filter(d, k < 6), fill = celeste, color = "white") +
  geom_col(data = filter(d, k >= 6), fill = rojo, color = "white") +
  annotate("text", x = 8.7, y = 0.07,
           label = "P(X >= 6) = 0.084", color = rojo, size = 4.4) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  labs(x = "Delitos en la semana (k)", y = "P(X = k)",
       title = "Poisson(3): la cola de semanas malas")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-pois-facetas
#| echo: false
#| fig-height: 3.0
pois <- crossing(lambda = c(1, 4, 10), k = 0:22) %>%
  mutate(prob = dpois(k, lambda),
         etq = fct_inorder(paste0("lambda = ", lambda)))
p <- ggplot(pois, aes(k, prob)) +
  geom_col(fill = celeste, width = 0.9) +
  facet_wrap(~ etq, scales = "free_y") +
  labs(x = "Numero de eventos (k)", y = "P(X = k)")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-aprox-poisson
#| echo: false
#| fig-height: 3.0
k <- 0:14
aprox <- bind_rows(
  tibble(k, prob = dbinom(k, 1000, 0.005), dist = "Binomial(1000, 0.005)"),
  tibble(k, prob = dpois(k, 5), dist = "Poisson(5)"))
p <- ggplot(aprox, aes(k, prob, fill = dist)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(values = c(azul, naranja)) +
  labs(x = "Numero de casos (k)", y = "P(X = k)", fill = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-normales
#| echo: false
#| fig-width: 5
#| fig-height: 3.6
#| out.width: "100%"
xs <- seq(0, 1500, 2)
nn <- bind_rows(
  tibble(x = xs, dens = dnorm(xs, 800, 200), par = "N(800, 200)"),
  tibble(x = xs, dens = dnorm(xs, 800, 100), par = "N(800, 100)"),
  tibble(x = xs, dens = dnorm(xs, 600, 200), par = "N(600, 200)"))
p <- ggplot(nn, aes(x, dens, color = par)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(naranja, celeste, azul)) +
  labs(x = "Ingreso (miles de $)", y = "Densidad", color = NULL,
       title = "mu desplaza, sigma ensancha") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-normal-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribucion_normal.png")


## -----------------------------------------------------------------------------
#| label: zscore
# Puntaje municipal ~ N(500, 100); un municipio obtiene 620
round(c(z = (620 - 500) / 100,
        via_original = pnorm(620, mean = 500, sd = 100),
        via_z = pnorm(1.2)), 3)


## -----------------------------------------------------------------------------
#| label: code-pnorm
mu <- 800; s <- 200
round(c(
  bajo_600 = pnorm(600, mu, s),
  sobre_1000 =
    1 - pnorm(1000, mu, s),
  entre = pnorm(1000, mu, s) -
    pnorm(600, mu, s)), 3)


## -----------------------------------------------------------------------------
#| label: plot-pnorm-areas
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
xs <- seq(200, 1400, 1)
zonas <- tibble(x = xs, dens = dnorm(xs, 800, 200))
p <- ggplot(zonas, aes(x, dens)) +
  geom_area(data = filter(zonas, x <= 600), fill = rojo, alpha = 0.75) +
  geom_area(data = filter(zonas, x >= 600 & x <= 1000),
            fill = celeste, alpha = 0.75) +
  geom_area(data = filter(zonas, x >= 1000), fill = naranja, alpha = 0.75) +
  geom_line(color = azul, linewidth = 1) +
  annotate("text", x = 490, y = 0.00030, label = "16%", color = "white", size = 4.5) +
  annotate("text", x = 800, y = 0.00080, label = "68%", color = "white", size = 5.5) +
  annotate("text", x = 1110, y = 0.00030, label = "16%", color = "white", size = 4.5) +
  labs(x = "Ingreso (miles de $)", y = "Densidad",
       title = "Ingresos ~ N(800, 200): tres areas, tres pnorm()")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: dpqr
set.seed(42)
round(c(d = dnorm(0), p = pnorm(1.96), q = qnorm(0.975), r = rnorm(1)), 3)


## -----------------------------------------------------------------------------
#| label: code-qnorm
mu <- 800; s <- 200
# Umbral del 20% mas pobre
q20 <- qnorm(0.20, mu, s)
# pnorm invierte a qnorm
round(c(umbral = q20,
        check = pnorm(q20, mu, s)),
      3)


## -----------------------------------------------------------------------------
#| label: plot-qnorm-foco
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
q20 <- qnorm(0.20, 800, 200)
dfoc <- tibble(x = seq(200, 1400, 1), dens = dnorm(x, 800, 200))
p <- ggplot(dfoc, aes(x, dens)) +
  geom_area(data = filter(dfoc, x <= q20), fill = verde, alpha = 0.65) +
  geom_line(color = azul, linewidth = 1) +
  geom_vline(xintercept = q20, color = rojo, linetype = "dashed", linewidth = 1) +
  annotate("text", x = 470, y = 0.00030, label = "20%", color = verde, size = 5) +
  annotate("text", x = q20 + 260, y = 0.0018,
           label = "umbral = qnorm(0.20) = 632", color = rojo, size = 4.2) +
  labs(x = "Ingreso (miles de $)", y = "Densidad",
       title = "Focalizar = cortar en un cuantil")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-t-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribuciones_t.png")


## -----------------------------------------------------------------------------
#| label: plot-t-normal
#| echo: false
#| fig-height: 2.5
xs <- seq(-4.5, 4.5, 0.01)
tt <- bind_rows(
  tibble(x = xs, dens = dnorm(xs), dist = "Normal(0,1)"),
  tibble(x = xs, dens = dt(xs, 3), dist = "t (df = 3)"),
  tibble(x = xs, dens = dt(xs, 10), dist = "t (df = 10)"))
p <- ggplot(tt, aes(x, dens, color = dist)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Normal(0,1)" = azul, "t (df = 3)" = rojo,
                                "t (df = 10)" = naranja)) +
  labs(x = "x", y = "Densidad", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: qt-criticos
round(c(t_df3 = qt(.975, 3), t_df10 = qt(.975, 10),
        t_df30 = qt(.975, 30), normal = qnorm(.975)), 2)


## -----------------------------------------------------------------------------
#| label: plot-tlc-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/tlc_simulacion.png")


## -----------------------------------------------------------------------------
#| label: code-tlc
#| eval: false
# set.seed(123)
# sim <- crossing(n = c(2, 10, 50),
#                 rep = 1:3000) %>%
#   mutate(
#     media = map_dbl(n, \(k)
#       mean(rexp(k, rate = 1/5))),
#     etq = fct_inorder(
#       paste0("n = ", n)))
# ggplot(sim, aes(media)) +
#   geom_histogram(bins = 40,
#                  fill = celeste,
#                  color = "white") +
#   facet_wrap(~ etq,
#              scales = "free") +
#   labs(x = "Media muestral",
#        y = "Frecuencia")


## -----------------------------------------------------------------------------
#| label: plot-tlc
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
set.seed(123)
sim <- crossing(n = c(2, 10, 50), rep = 1:3000) %>%
  mutate(media = map_dbl(n, \(k) mean(rexp(k, rate = 1/5))),
         etq = fct_inorder(paste0("n = ", n)))
interactivo(
  ggplot(sim, aes(media)) +
    geom_histogram(bins = 40, fill = celeste, color = "white") +
    facet_wrap(~ etq, scales = "free") +
    labs(x = "Media muestral", y = "Frecuencia")
)


## -----------------------------------------------------------------------------
#| label: plot-muestral-estatica
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribucion_muestral.png")

