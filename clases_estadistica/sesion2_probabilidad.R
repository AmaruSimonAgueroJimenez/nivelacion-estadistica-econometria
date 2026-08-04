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
#| label: plot-venn
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/diagrama_venn.png")


## -----------------------------------------------------------------------------
#| label: venn-operaciones
#| echo: false
#| dev: png
#| fig-width: 10
#| fig-height: 2.7
#| out-width: "100%"
library(ggforce)
niveles <- c("Unión: A o B (o ambos)", "Intersección: A y B",
             "Complemento: no ocurre A", "Disjuntos: no coexisten")
circ <- bind_rows(
  tibble(panel = niveles[1], x0 = c(-0.5, 0.5), y0 = 0, r = 1,
         fill = celeste, al = 0.45),
  tibble(panel = niveles[2], x0 = c(-0.5, 0.5), y0 = 0, r = 1,
         fill = NA_character_, al = 1),
  tibble(panel = niveles[3], x0 = -0.2, y0 = 0, r = 1,
         fill = "white", al = 1),
  tibble(panel = niveles[4], x0 = c(-1.05, 1.05), y0 = 0, r = 0.9,
         fill = c(celeste, naranja), al = 0.45)
) %>% mutate(panel = factor(panel, levels = niveles))
t1 <- seq(-pi/3, pi/3, length.out = 60)
t2 <- seq(2*pi/3, 4*pi/3, length.out = 60)
lente <- tibble(panel = factor(niveles[2], levels = niveles),
                x = c(-0.5 + cos(t1), 0.5 + cos(t2)),
                y = c(sin(t1), sin(t2)))
fondo <- tibble(panel = factor(niveles[3], levels = niveles))
etiq <- bind_rows(
  tibble(panel = niveles[1], x = c(-0.95, 0.95), y = 1.25, lab = c("A", "B")),
  tibble(panel = niveles[2], x = c(-0.95, 0.95), y = 1.25, lab = c("A", "B")),
  tibble(panel = niveles[3], x = c(-0.2, 1.55), y = c(1.25, -1.25),
         lab = c("A", "A^c")),
  tibble(panel = niveles[4], x = c(-1.05, 1.05), y = 1.15, lab = c("A", "B"))
) %>% mutate(panel = factor(panel, levels = niveles))
ggplot() +
  geom_rect(data = fondo,
            aes(xmin = -2.05, xmax = 2.05, ymin = -1.45, ymax = 1.45),
            fill = alpha(celeste, 0.45), color = NA) +
  geom_polygon(data = lente, aes(x, y), fill = alpha(rojo, 0.65), color = NA) +
  geom_circle(data = circ,
              aes(x0 = x0, y0 = y0, r = r, fill = fill, alpha = al),
              color = azul, linewidth = 0.6) +
  scale_fill_identity() + scale_alpha_identity() +
  annotate("rect", xmin = -2.05, xmax = 2.05, ymin = -1.45, ymax = 1.45,
           fill = NA, color = "gray40", linewidth = 0.5) +
  geom_text(data = etiq, aes(x, y, label = lab), color = azul, parse = TRUE,
            fontface = "bold.italic", size = 4.2) +
  facet_wrap(~panel, nrow = 1) +
  coord_fixed(xlim = c(-2.1, 2.1), ylim = c(-1.5, 1.5), expand = FALSE) +
  theme_void(base_size = 12) +
  theme(strip.text = element_text(size = 10.5, face = "bold", color = azul,
                                  margin = margin(b = 4)))


## -----------------------------------------------------------------------------
#| label: code-adicion
pA <- 0.30; pB <- 0.25; pAB <- 0.10
c(al_menos_uno = pA + pB - pAB,
  solo_vivienda = pA - pAB,
  ninguno = 1 - (pA + pB - pAB))


## -----------------------------------------------------------------------------
#| label: code-lln
#| eval: false
# set.seed(2026)
# n <- 10000
# dado <- sample(1:6, n,
#                replace = TRUE)
# frec <- cumsum(dado == 6) / (1:n)
# tibble(n = 1:n, frec = frec) %>%
#   ggplot(aes(n, frec)) +
#   geom_line(color = celeste) +
#   geom_hline(yintercept = 1/6,
#              color = rojo,
#              linetype = "dashed") +
#   scale_x_log10(
#     labels = scales::comma) +
#   labs(x = "N° lanzamientos (log)",
#        y = "Frecuencia de 'sale 6'")


## -----------------------------------------------------------------------------
#| label: plot-lln
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
set.seed(2026)
n <- 10000
dado <- sample(1:6, n, replace = TRUE)
frec <- cumsum(dado == 6) / (1:n)
interactivo(
  tibble(n = 1:n, frec = frec) %>%
    ggplot(aes(n, frec)) +
    geom_line(color = celeste) +
    geom_hline(yintercept = 1/6, color = rojo,
               linetype = "dashed") +
    scale_x_log10(labels = scales::comma) +
    labs(x = "N° lanzamientos (log)",
         y = "Frecuencia de 'sale 6'")
)


## -----------------------------------------------------------------------------
#| label: tabla-voto
#| echo: false
tibble(
  ` ` = c("Vota", "No vota", "Total"),
  `Educación superior` = c(140, 180, 320),
  `Sin educación superior` = c(70, 110, 180),
  `Total` = c(210, 290, 500)
) %>% kable()


## -----------------------------------------------------------------------------
#| label: plot-heatmap
#| echo: false
#| fig-height: 3.0
conj <- tibble(
  voto = rep(c("Vota", "No vota"), each = 2),
  educ = rep(c("Ed. superior", "Sin ed. superior"), 2),
  n = c(140, 70, 180, 110)
) %>% mutate(p = n / 500)
p <- ggplot(conj, aes(educ, voto, fill = p)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = paste0(scales::percent(p, accuracy = 0.1), " (n = ", n, ")")),
            color = "white", size = 4.6, fontface = "bold") +
  scale_fill_gradient(low = "#5DADE2", high = azul, guide = "none") +
  labs(x = NULL, y = NULL, title = "Probabilidades conjuntas: cada celda / 500") +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 12))
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-cond
round(c(P_V = 210/500,
        P_V_dado_E = 140/320,
        P_V_dado_noE = 70/180), 3)


## -----------------------------------------------------------------------------
#| label: plot-cond
#| echo: false
#| fig-width: 5
#| fig-height: 3.3
#| out.width: "100%"
et2 <- c("P(V)", "P(V | E)", "P(V | no E)")
p <- tibble(g = factor(et2, levels = et2),
            p = c(210/500, 140/320, 70/180)) %>%
  ggplot(aes(g, p, fill = g)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(p, accuracy = 0.1)),
            vjust = -0.4, color = azul, size = 4.6) +
  scale_fill_manual(values = c(celeste, verde, naranja)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.55)) +
  labs(x = NULL, y = "Probabilidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: venn-condicional
#| echo: false
#| dev: png
#| fig-width: 7.5
#| fig-height: 3.0
#| out-width: "70%"
niveles3 <- c("Antes: el universo es Ω", "Al saber B: el universo es B")
c3 <- bind_rows(
  tibble(panel = niveles3[1], x0 = c(-0.5, 0.5), y0 = 0, r = 1,
         fill = c(alpha(celeste, 0.45), alpha(naranja, 0.45)),
         col = c(azul, naranja)),
  tibble(panel = niveles3[2], x0 = c(-0.5, 0.5), y0 = 0, r = 1,
         fill = c(NA_character_, alpha(naranja, 0.5)),
         col = c("gray70", naranja))
) %>% mutate(panel = factor(panel, levels = niveles3))
t1 <- seq(-pi/3, pi/3, length.out = 60)
t2 <- seq(2*pi/3, 4*pi/3, length.out = 60)
lente3 <- tibble(panel = factor(niveles3[2], levels = niveles3),
                 x = c(-0.5 + cos(t1), 0.5 + cos(t2)),
                 y = c(sin(t1), sin(t2)))
etiq3 <- bind_rows(
  tibble(panel = niveles3[1], x = c(-1.0, 1.0), y = 1.25, lab = c("A", "B"),
         col = c(azul, naranja)),
  tibble(panel = niveles3[2], x = c(-1.0, 1.0), y = 1.25, lab = c("A", "B"),
         col = c("gray60", naranja))
) %>% mutate(panel = factor(panel, levels = niveles3))
ggplot() +
  annotate("rect", xmin = -2.05, xmax = 2.05, ymin = -1.45, ymax = 1.45,
           fill = NA, color = "gray45", linewidth = 0.5) +
  geom_circle(data = c3, aes(x0 = x0, y0 = y0, r = r, fill = fill, color = col),
              linewidth = 0.7) +
  geom_polygon(data = lente3, aes(x, y), fill = alpha(rojo, 0.6), color = NA) +
  scale_fill_identity() + scale_color_identity() +
  geom_text(data = etiq3, aes(x, y, label = lab, color = col),
            fontface = "bold.italic", size = 4.6) +
  geom_text(data = tibble(panel = factor(niveles3[2], levels = niveles3),
                          x = 0, y = 0),
            aes(x, y), label = "A∩B", color = "white",
            fontface = "bold", size = 3.6) +
  facet_wrap(~panel, nrow = 1) +
  coord_fixed(xlim = c(-2.1, 2.1), ylim = c(-1.5, 1.5), expand = FALSE) +
  theme_void(base_size = 12) +
  theme(strip.text = element_text(size = 11, face = "bold", color = azul,
                                  margin = margin(b = 4)))


## -----------------------------------------------------------------------------
#| label: code-indep
tabla <- matrix(c(140, 70, 180, 110),
                nrow = 2, byrow = TRUE)
round(chisq.test(tabla)$p.value, 3)


## -----------------------------------------------------------------------------
#| label: code-producto
round(c(dos_irregulares = (4/20) * (3/19),
        al_menos_uno = 1 - (16/20) * (15/19)), 3)


## -----------------------------------------------------------------------------
#| label: venn-particion
#| echo: false
#| dev: png
#| fig-width: 7
#| fig-height: 2.9
#| out-width: "62%"
bandas <- tibble(xmin = c(-2.1, -0.7, 0.7), xmax = c(-0.7, 0.7, 2.1),
                 fill = c(alpha(celeste, 0.25), alpha(verde, 0.22),
                          alpha(naranja, 0.25)))
ggplot() +
  geom_rect(data = bandas,
            aes(xmin = xmin, xmax = xmax, ymin = -1.3, ymax = 1.3, fill = fill),
            color = "gray55", linewidth = 0.4) +
  scale_fill_identity() +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = -0.15, a = 1.45, b = 0.62, angle = 0),
                        fill = alpha(rojo, 0.35), color = rojo,
                        linewidth = 0.8) +
  geom_vline(xintercept = c(-0.7, 0.7), color = "gray35", linewidth = 0.5) +
  annotate("text", x = c(-1.4, 0, 1.4), y = 1.08,
           label = c("B[1]", "B[2]", "B[3]"), parse = TRUE,
           color = azul, fontface = "bold", size = 4.6) +
  annotate("text", x = 0, y = -0.15, label = "A", color = rojo,
           fontface = "bold.italic", size = 5) +
  annotate("text", x = c(-1.05, 0, 1.05), y = -0.95,
           label = c("A*'∩'*B[1]", "A*'∩'*B[2]", "A*'∩'*B[3]"), parse = TRUE,
           color = "gray25", size = 3.4) +
  annotate("segment", x = c(-1.05, 0, 1.05), xend = c(-1.05, 0, 1.05),
           y = -0.83, yend = c(-0.52, -0.72, -0.52),
           color = "gray45", linewidth = 0.4) +
  annotate("text", x = -1.95, y = 1.18, label = "Omega", parse = TRUE,
           color = "gray30", size = 4.6) +
  coord_fixed(xlim = c(-2.15, 2.15), ylim = c(-1.35, 1.35), expand = FALSE) +
  theme_void()


## -----------------------------------------------------------------------------
#| label: plot-arbol
#| echo: false
#| fig-height: 3.0
#| out.width: "78%"
seg <- tibble(
  x0 = c(0.10, 0.10, 1.18, 1.18, 1.18, 1.18),
  y0 = c(0.50, 0.50, 0.78, 0.78, 0.22, 0.22),
  x1 = c(0.84, 0.84, 1.92, 1.92, 1.92, 1.92),
  y1 = c(0.78, 0.22, 0.95, 0.61, 0.39, 0.05))
ggplot() +
  geom_segment(data = seg, aes(x = x0, y = y0, xend = x1, yend = y1),
               color = "gray55", linewidth = 0.7) +
  annotate("label", x = 0, y = 0.50, label = "Población", size = 4.4,
           fill = "#EAF2F8", color = azul, fontface = "bold") +
  annotate("label", x = 1.0, y = 0.78, label = "Enferma", size = 4,
           fill = rojo, color = "white", fontface = "bold") +
  annotate("label", x = 1.0, y = 0.22, label = "Sana", size = 4,
           fill = celeste, color = "white", fontface = "bold") +
  annotate("text", x = 0.42, y = 0.71, label = "0.01", size = 3.8, color = "gray30") +
  annotate("text", x = 0.42, y = 0.29, label = "0.99", size = 3.8, color = "gray30") +
  annotate("text", x = 1.42, y = 0.92, label = "sens. 0.95", size = 3.6, color = "gray30") +
  annotate("text", x = 1.46, y = 0.64, label = "0.05", size = 3.6, color = "gray30") +
  annotate("text", x = 1.42, y = 0.36, label = "1 - esp. 0.10", size = 3.6, color = "gray30") +
  annotate("text", x = 1.46, y = 0.08, label = "esp. 0.90", size = 3.6, color = "gray30") +
  annotate("text", x = 2.0, y = 0.95, label = "Test +:  0.01 · 0.95 = 0.0095",
           hjust = 0, size = 4.2, color = rojo, fontface = "bold") +
  annotate("text", x = 2.0, y = 0.61, label = "Test -:  0.01 · 0.05 = 0.0005",
           hjust = 0, size = 4.2, color = "gray45") +
  annotate("text", x = 2.0, y = 0.39, label = "Test +:  0.99 · 0.10 = 0.0990",
           hjust = 0, size = 4.2, color = naranja, fontface = "bold") +
  annotate("text", x = 2.0, y = 0.05, label = "Test -:  0.99 · 0.90 = 0.8910",
           hjust = 0, size = 4.2, color = "gray45") +
  coord_cartesian(xlim = c(-0.18, 3.55), ylim = c(-0.03, 1.03)) +
  theme_void()


## -----------------------------------------------------------------------------
#| label: code-bayes-calc
prev <- 0.01; sens <- 0.95; esp <- 0.90
p_pos <- sens * prev + (1 - esp) * (1 - prev)
round(c(P_positivo = p_pos,
        P_enf_dado_pos = sens * prev / p_pos), 4)


## -----------------------------------------------------------------------------
#| label: tabla-frecnat
#| echo: false
tibble(
  ` ` = c("Enfermas (100)", "Sanas (9.900)", "Total"),
  `Test +` = c("95", "990", "1.085"),
  `Test -` = c("5", "8.910", "8.915"),
  `Total` = c("100", "9.900", "10.000")
) %>% kable(align = "lrrr")


## -----------------------------------------------------------------------------
#| label: tabla-lab-bayes
#| echo: false
prevs <- c(0.001, 0.01, 0.05, 0.20)
posts <- 0.95 * prevs / (0.95 * prevs + 0.10 * (1 - prevs))
tibble(
  `Prevalencia (prior)` = scales::percent(prevs, accuracy = 0.1),
  `P(Enf | +) (posterior)` = scales::percent(posts, accuracy = 0.1)
) %>% kable(align = "rr")


## -----------------------------------------------------------------------------
#| label: plot-bayespng
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/bayes_ejemplo.png")


## -----------------------------------------------------------------------------
#| label: plot-prevalencia
#| echo: false
#| fig-height: 3.2
sens <- 0.95
grid <- crossing(prev = seq(0.001, 0.5, 0.001),
                 fp = c(0.10, 0.05, 0.01)) %>%
  mutate(post = sens * prev / (sens * prev + fp * (1 - prev)),
         tasa = factor(paste0("Falsos positivos = ", scales::percent(fp)),
                       levels = paste0("Falsos positivos = ",
                                       scales::percent(c(0.10, 0.05, 0.01)))))
p <- ggplot(grid, aes(prev, post, color = tasa)) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = 0.01, linetype = "dashed", color = "gray40") +
  annotate("text", x = 0.035, y = 0.97, label = "prev. = 1%", color = "gray30", size = 4) +
  annotate("text", x = 0.42, y = 0.36, label = "posterior = prior", color = "gray45", size = 3.8) +
  scale_color_manual(values = c(naranja, celeste, verde)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Prevalencia (prior)", y = "P(Enf | +) (posterior)", color = NULL) +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-fraude
#| eval: false
# prev <- 0.02; sens <- 0.90
# fp <- c(0.05, 0.02, 0.01, 0.005)
# post <- sens * prev /
#   (sens * prev + fp * (1 - prev))
# et <- scales::percent(fp, 0.1)
# tibble(
#   tasa = factor(et, levels = et),
#   post = post) %>%
#   ggplot(aes(tasa, post)) +
#   geom_col(fill = celeste,
#            width = 0.6) +
#   geom_text(aes(label =
#     scales::percent(post, 1)),
#     vjust = -0.4, color = azul) +
#   scale_y_continuous(
#     labels = scales::percent,
#     limits = c(0, 0.95)) +
#   labs(x = "Tasa falsos positivos",
#        y = "P(fraude | alerta)")


## -----------------------------------------------------------------------------
#| label: plot-fraude
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
prev <- 0.02; sens <- 0.90
fp <- c(0.05, 0.02, 0.01, 0.005)
post <- sens * prev / (sens * prev + fp * (1 - prev))
et <- scales::percent(fp, 0.1)
interactivo(
  tibble(tasa = factor(et, levels = et), post = post) %>%
    ggplot(aes(tasa, post)) +
    geom_col(fill = celeste, width = 0.6) +
    geom_text(aes(label = scales::percent(post, 1)),
              vjust = -0.4, color = azul) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 0.95)) +
    labs(x = "Tasa falsos positivos",
         y = "P(fraude | alerta)")
)


## -----------------------------------------------------------------------------
#| label: code-montecarlo
set.seed(123)
n <- 100000
enfermo <- rbinom(n, 1, 0.01)
p_test <- ifelse(enfermo == 1, 0.95, 0.10)
positivo <- rbinom(n, 1, p_test)
round(c(P_pos_simulada = mean(positivo),
        P_enf_dado_pos_simulada = mean(enfermo[positivo == 1]),
        teorico = 0.0876), 4)


## -----------------------------------------------------------------------------
#| label: plot-pmfpng
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/pmf_ejemplo.png")

