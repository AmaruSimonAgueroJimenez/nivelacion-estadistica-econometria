## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(gapminder)
library(moments)
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
# Escenario de trabajo: evaluacion de un programa de empleo
set.seed(123)
poblacion <- rlnorm(50000, meanlog = 6.2, sdlog = 0.5)  # ingresos (miles de $)
mu_pob <- mean(poblacion); sigma_pob <- sd(poblacion)
set.seed(42)
encuesta <- sample(poblacion, 200)                      # encuesta a beneficiarios
set.seed(7)
programa <- tibble(
  grupo = rep(c("Control", "Tratado"), each = 120),
  ingreso = c(rnorm(120, 520, 90), rnorm(120, 565, 90))
)
set.seed(21)
gestion <- tibble(municipio = 1:35, antes = rnorm(35, 62, 8)) %>%
  mutate(despues = antes + rnorm(35, 3, 4))


## -----------------------------------------------------------------------------
#| label: png-dist-muestral
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/distribucion_muestral.png")


## -----------------------------------------------------------------------------
#| label: png-tlc
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/tlc_simulacion.png")


## -----------------------------------------------------------------------------
#| label: png-ancho-ic
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/ancho_ic.png")


## -----------------------------------------------------------------------------
#| label: plot-sim-muestral
#| echo: false
#| fig-height: 3.2
set.seed(99)
medias_sim <- replicate(3000, mean(sample(poblacion, 100)))
dsim <- bind_rows(
  tibble(valor = poblacion, panel = "Población: 50.000 ingresos (sesgo = 1.7)"),
  tibble(valor = medias_sim, panel = "3.000 medias muestrales (n = 100)")
) %>%
  mutate(panel = factor(panel, levels = c(
    "Población: 50.000 ingresos (sesgo = 1.7)",
    "3.000 medias muestrales (n = 100)")))
p <- ggplot(dsim, aes(valor, fill = panel)) +
  geom_histogram(bins = 60, color = "white", show.legend = FALSE) +
  geom_vline(xintercept = mu_pob, color = rojo, linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = c(naranja, celeste)) +
  facet_wrap(~panel, scales = "free") +
  labs(x = "Ingreso mensual (miles de $)", y = NULL)
interactivo(p)


## -----------------------------------------------------------------------------
#| label: png-ic
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/intervalo_confianza.png")


## -----------------------------------------------------------------------------
#| label: plot-z-vs-t
#| echo: false
#| fig-width: 5
#| fig-height: 3.4
#| out.width: "100%"
xg <- seq(-4, 4, 0.01)
df_zt <- bind_rows(
  tibble(x = xg, dens = dnorm(xg), dist = "Normal (z)"),
  tibble(x = xg, dens = dt(xg, 4), dist = "t con 4 gl"),
  tibble(x = xg, dens = dt(xg, 29), dist = "t con 29 gl")
) %>%
  mutate(dist = factor(dist, levels = c("Normal (z)", "t con 4 gl", "t con 29 gl")))
p <- ggplot(df_zt, aes(x, dens, color = dist)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(azul, rojo, naranja)) +
  labs(x = "Estadístico estandarizado", y = "Densidad", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-ic-media
n <- length(encuesta)                       # 200 beneficiarios encuestados
media <- mean(encuesta); s <- sd(encuesta)
t_crit <- qt(0.975, df = n - 1)
round(c(media = media, SE = s / sqrt(n), t_critico = t_crit), 2)
round(media + c(-1, 1) * t_crit * s / sqrt(n), 1)   # IC 95% manual
round(as.numeric(t.test(encuesta)$conf.int), 1)     # IC 95% con t.test()


## -----------------------------------------------------------------------------
#| label: plot-cobertura
#| echo: false
#| fig-height: 3.2
set.seed(2026)
sims_cob <- map_dfr(1:100, function(i) {
  m <- sample(poblacion, 50)
  ic <- t.test(m)$conf.int
  tibble(id = i, inf = ic[1], sup = ic[2], media = mean(m))
}) %>%
  mutate(estado = factor(inf <= mu_pob & mu_pob <= sup,
                         levels = c(TRUE, FALSE),
                         labels = c("Contiene la media poblacional", "No la contiene")))
p <- ggplot(sims_cob, aes(x = id)) +
  geom_segment(aes(xend = id, y = inf, yend = sup, color = estado), linewidth = 0.8) +
  geom_point(aes(y = media, color = estado), size = 0.7) +
  geom_hline(yintercept = mu_pob, color = azul, linetype = "dashed", linewidth = 0.9) +
  scale_color_manual(values = c("Contiene la media poblacional" = celeste,
                                "No la contiene" = rojo)) +
  labs(x = "Muestra (n = 50 cada una)", y = "Ingreso (miles de $)", color = NULL,
       title = "Cada segmento es el IC 95% de una muestra distinta") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-ancho
#| eval: false
# g <- crossing(
#     n = seq(10, 500, 5),
#     conf = c(.90, .95, .99)) %>%
#   mutate(
#     t_c = qt(1 - (1 - conf)/2,
#              df = n - 1),
#     ancho = 2 * t_c *
#       sigma_pob / sqrt(n))
# ggplot(g, aes(n, ancho,
#        color = factor(conf))) +
#   geom_line(linewidth = 1) +
#   labs(x = "Tamaño muestral n",
#        y = "Ancho del IC",
#        color = "Confianza")


## -----------------------------------------------------------------------------
#| label: plot-ancho
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
g <- crossing(n = seq(10, 500, 5), conf = c(.90, .95, .99)) %>%
  mutate(t_c = qt(1 - (1 - conf)/2, df = n - 1),
         ancho = 2 * t_c * sigma_pob / sqrt(n))
interactivo(
  ggplot(g, aes(n, ancho, color = factor(conf))) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c(verde, celeste, rojo),
                       labels = c("90%", "95%", "99%")) +
    labs(x = "Tamaño muestral n", y = "Ancho del IC (miles de $)",
         color = "Confianza")
)


## -----------------------------------------------------------------------------
#| label: code-ic-prop
x <- 280; n <- 500                      # 280 de 500 apoyan la reforma
p_hat <- x / n
ee <- sqrt(p_hat * (1 - p_hat) / n)
round(c(p_hat = p_hat, SE = ee, margen_95 = 1.96 * ee), 3)
round(as.numeric(prop.test(x, n)$conf.int), 3)   # IC de Wilson


## -----------------------------------------------------------------------------
#| label: plot-valor-p
#| echo: false
#| fig-height: 3.0
xg <- seq(-4, 4, 0.01)
dens_t <- tibble(x = xg, d = dt(xg, 199))
p <- ggplot(dens_t, aes(x, d)) +
  geom_area(data = filter(dens_t, x >= 2.2), fill = rojo, alpha = .6) +
  geom_area(data = filter(dens_t, x <= -2.2), fill = rojo, alpha = .6) +
  geom_line(color = azul, linewidth = 1) +
  geom_vline(xintercept = c(-2.2, 2.2), linetype = "dashed", color = rojo) +
  annotate("text", x = 2.95, y = 0.055, label = "área = p/2", color = rojo, size = 4.3) +
  annotate("text", x = -2.95, y = 0.055, label = "área = p/2", color = rojo, size = 4.3) +
  annotate("text", x = 0, y = 0.16, label = "Distribución de t\nsi H0 es cierta", color = azul, size = 4.3) +
  annotate("text", x = 2.3, y = 0.38, label = "t observado = 2.2", color = rojo, size = 4.1, hjust = 0) +
  labs(x = "Estadístico t", y = "Densidad bajo H0")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-t-una
tt1 <- t.test(encuesta, mu = 500)
tt1


## -----------------------------------------------------------------------------
#| label: code-t-dos
tt2 <- t.test(ingreso ~ grupo, data = programa)   # Welch por defecto
tidy(tt2) %>%
  transmute(dif = estimate, t = statistic, gl = parameter,
            p = p.value, ic_inf = conf.low, ic_sup = conf.high) %>%
  kable(digits = 3)


## -----------------------------------------------------------------------------
#| label: plot-dos-grupos
#| echo: false
#| fig-height: 3.3
niveles_panel <- c("Ingreso por grupo", "Diferencia e IC 95%")
dif_df <- tidy(tt2) %>%
  transmute(grupo = "Tratado - Control", dif = -estimate,
            inf = -conf.high, sup = -conf.low,
            panel = factor("Diferencia e IC 95%", levels = niveles_panel))
prog_p <- programa %>%
  mutate(panel = factor("Ingreso por grupo", levels = niveles_panel))
p <- ggplot() +
  geom_jitter(data = prog_p, aes(grupo, ingreso, color = grupo),
              width = .12, alpha = .3, size = 1, show.legend = FALSE) +
  geom_boxplot(data = prog_p, aes(grupo, ingreso, fill = grupo),
               alpha = .6, width = .45, outlier.shape = NA, show.legend = FALSE) +
  geom_hline(data = dif_df, aes(yintercept = 0), linetype = "dashed", color = rojo) +
  geom_errorbar(data = dif_df, aes(x = grupo, ymin = inf, ymax = sup),
                width = .12, color = azul, linewidth = 1) +
  geom_point(data = dif_df, aes(grupo, dif), color = azul, size = 3) +
  facet_wrap(~panel, scales = "free") +
  scale_fill_manual(values = c(Control = naranja, Tratado = celeste)) +
  scale_color_manual(values = c(Control = naranja, Tratado = celeste)) +
  labs(x = NULL, y = "Miles de $")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-pareada
tp <- t.test(gestion$despues,
             gestion$antes,
             paired = TRUE)
round(c(dif = unname(tp$estimate),
        as.numeric(tp$conf.int)), 2)
signif(tp$p.value, 2)


## -----------------------------------------------------------------------------
#| label: plot-pareada
#| echo: false
#| fig-width: 5
#| fig-height: 3.5
#| out.width: "100%"
glargo <- gestion %>%
  pivot_longer(c(antes, despues), names_to = "momento", values_to = "indice") %>%
  mutate(momento = factor(momento, levels = c("antes", "despues"),
                          labels = c("Antes", "Después")))
p <- ggplot(glargo, aes(momento, indice, group = municipio)) +
  geom_line(alpha = 0.35, color = celeste) +
  geom_point(size = 1.4, color = azul, alpha = 0.6) +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               color = rojo, linewidth = 1.2) +
  labs(x = NULL, y = "Índice de gestión (0-100)",
       title = "35 municipios; línea roja = promedio")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: plot-errores
#| echo: false
#| fig-height: 3.2
xg <- seq(-4, 6.5, 0.01)
crit <- qnorm(0.95)
curvas_h <- bind_rows(
  tibble(x = xg, dens = dnorm(xg), hip = "H0"),
  tibble(x = xg, dens = dnorm(xg, 2.5), hip = "H1"))
p <- ggplot(curvas_h, aes(x, dens, group = hip)) +
  geom_area(data = filter(curvas_h, hip == "H1", x >= crit),
            fill = verde, alpha = .35) +
  geom_area(data = filter(curvas_h, hip == "H1", x < crit),
            fill = naranja, alpha = .55) +
  geom_area(data = filter(curvas_h, hip == "H0", x >= crit),
            fill = rojo, alpha = .75) +
  geom_line(aes(color = hip), linewidth = 1, show.legend = FALSE) +
  geom_vline(xintercept = crit, linetype = "dashed", color = azul) +
  scale_color_manual(values = c(H0 = azul, H1 = verde)) +
  annotate("text", x = -1.7, y = 0.35, label = "H0: sin efecto", color = azul, size = 4.4) +
  annotate("text", x = 4.35, y = 0.35, label = "H1: efecto real", color = verde, size = 4.4) +
  annotate("text", x = 1.68, y = 0.44, label = "valor crítico", color = azul, size = 3.8, hjust = 0) +
  annotate("text", x = 3.55, y = 0.30, label = "rojo: alfa = 0.05 (error I)", color = rojo, size = 4, hjust = 0) +
  annotate("text", x = 3.55, y = 0.26, label = "naranja: beta = 0.20 (error II)", color = naranja, size = 4, hjust = 0) +
  annotate("text", x = 3.55, y = 0.22, label = "verde: potencia = 0.80", color = verde, size = 4, hjust = 0) +
  coord_cartesian(ylim = c(0, 0.46)) +
  labs(x = "Estadístico de prueba", y = "Densidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-potencia
#| eval: false
# curvas <- crossing(
#     d = seq(0.1, 1.2, 0.02),
#     n = c(30, 64, 120)) %>%
#   mutate(pow = map2_dbl(d, n,
#     ~power.t.test(n = .y,
#                   delta = .x,
#                   sd = 1)$power))
# ggplot(curvas, aes(d, pow,
#        color = factor(n))) +
#   geom_line(linewidth = 1) +
#   geom_hline(yintercept = 0.8,
#              linetype = "dashed") +
#   labs(x = "Efecto (d de Cohen)",
#        y = "Potencia",
#        color = "n por grupo")


## -----------------------------------------------------------------------------
#| label: plot-potencia
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
curvas <- crossing(d = seq(0.1, 1.2, 0.02), n = c(30, 64, 120)) %>%
  mutate(pow = map2_dbl(d, n,
    ~power.t.test(n = .y, delta = .x, sd = 1)$power))
interactivo(
  ggplot(curvas, aes(d, pow, color = factor(n))) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    scale_color_manual(values = c(naranja, celeste, azul)) +
    labs(x = "Efecto (d de Cohen)", y = "Potencia",
         color = "n por grupo")
)


## -----------------------------------------------------------------------------
#| label: code-cohen
resumen <- programa %>% group_by(grupo) %>%
  summarise(m = mean(ingreso), s = sd(ingreso))
s_pool <- sqrt(mean(resumen$s^2))
round(c(dif = diff(resumen$m), s_comb = s_pool,
        d_cohen = diff(resumen$m) / s_pool), 2)


## -----------------------------------------------------------------------------
#| label: plot-p-vs-n
#| echo: false
#| fig-height: 3.0
df_pn <- crossing(n = seq(20, 4000, 20), d = c(0.05, 0.2, 0.5)) %>%
  mutate(t = d * sqrt(n / 2), p = 2 * pt(-t, df = 2 * n - 2),
         efecto = paste0("d = ", d))
p <- ggplot(df_pn, aes(n, p, color = efecto)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = rojo) +
  annotate("text", x = 3700, y = 0.09, label = "p = 0.05", color = rojo, size = 4) +
  scale_y_log10(breaks = c(1e-6, 1e-4, 0.01, 1),
                labels = c("0.000001", "0.0001", "0.01", "1")) +
  coord_cartesian(ylim = c(1e-6, 1)) +
  scale_color_manual(values = c(rojo, naranja, celeste)) +
  labs(x = "n por grupo", y = "Valor-p (escala log)", color = NULL,
       title = "Valor-p si la diferencia observada igualara a la verdadera") +
  theme(legend.position = "top")
interactivo(p)

