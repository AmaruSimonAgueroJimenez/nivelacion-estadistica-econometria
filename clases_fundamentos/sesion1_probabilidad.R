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
#| label: venn-operaciones
#| echo: false
#| fig-width: 7.5
#| fig-height: 3.0
#| out.width: "72%"
library(ggforce)
# Helpers de diagramas de Venn (se reutilizan en las laminas siguientes)
circ_pts <- function(cx, cy, r, n = 240) {
  t <- seq(0, 2 * pi, length.out = n)
  list(x = cx + r * cos(t), y = cy + r * sin(t))
}
elip_pts <- function(cx, cy, a, b, n = 240) {
  t <- seq(0, 2 * pi, length.out = n)
  list(x = cx + a * cos(t), y = cy + b * sin(t))
}
rect_pts <- function(x0, y0, x1, y1) list(x = c(x0, x1, x1, x0), y = c(y0, y0, y1, y1))
clip_df <- function(A, B, op, etiqueta) {
  res <- polyclip::polyclip(A, B, op)
  bind_rows(lapply(seq_along(res), function(i)
    tibble(x = res[[i]]$x, y = res[[i]]$y, pieza = paste(etiqueta, i), op = etiqueta)))
}

cA <- circ_pts(-0.6, 0, 1); cB <- circ_pts(0.6, 0, 1)
om <- rect_pts(-2.2, -1.35, 2.2, 1.35)
ops <- c('A*" "*symbol("\\310")*" "*B', 'A*" "*symbol("\\307")*" "*B', 'A^c', 'A - B')
zonas <- bind_rows(
  clip_df(cA, cB, "union", ops[1]),
  clip_df(cA, cB, "intersection", ops[2]),
  clip_df(om, cA, "minus", ops[3]),
  clip_df(cA, cB, "minus", ops[4])
) %>% mutate(op = factor(op, levels = ops))
marcos <- tibble(op = factor(ops, levels = ops),
                 xmin = -2.2, xmax = 2.2, ymin = -1.35, ymax = 1.35)
circulos <- crossing(tibble(op = factor(ops, levels = ops)),
                     tibble(x0 = c(-0.6, 0.6), y0 = 0, r = 1))
letras <- crossing(tibble(op = factor(ops, levels = ops)),
                   tibble(x = c(-1.35, 1.35), y = 1.05, lab = c("A", "B")))
ggplot() +
  geom_rect(data = marcos, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey96", color = "grey40", linewidth = 0.4) +
  geom_polygon(data = zonas, aes(x = x, y = y, group = op, subgroup = pieza, fill = op),
               alpha = 0.65, rule = "evenodd", show.legend = FALSE) +
  geom_circle(data = circulos, aes(x0 = x0, y0 = y0, r = r),
              color = azul, linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = letras, aes(x = x, y = y, label = lab),
            color = azul, size = 4, fontface = "bold") +
  scale_fill_manual(values = setNames(c(celeste, naranja, rojo, verde), ops)) +
  facet_wrap(~op, nrow = 2, labeller = label_parsed) +
  coord_fixed() + theme_void() +
  theme(strip.text = element_text(size = 12, color = azul, face = "bold"))


## -----------------------------------------------------------------------------
#| label: venn-condicional
#| echo: false
#| fig-width: 6.5
#| fig-height: 2.9
#| out.width: "58%"
cA2 <- circ_pts(-0.55, 0, 0.95); cB2 <- circ_pts(0.65, 0, 0.95)
lente <- clip_df(cA2, cB2, "intersection", "lente")
soloB <- clip_df(cB2, cA2, "minus", "soloB")
ggplot() +
  geom_rect(aes(xmin = -2.3, xmax = 2.3, ymin = -1.3, ymax = 1.3),
            fill = "grey92", color = "grey55", linewidth = 0.4) +
  geom_circle(aes(x0 = -0.55, y0 = 0, r = 0.95), fill = "grey80", alpha = 0.55,
              color = "grey55", linewidth = 0.5) +
  geom_polygon(data = soloB, aes(x = x, y = y, group = pieza), fill = celeste, alpha = 0.55) +
  geom_polygon(data = lente, aes(x = x, y = y, group = pieza), fill = naranja, alpha = 0.85) +
  geom_circle(aes(x0 = 0.65, y0 = 0, r = 0.95), fill = NA, color = azul, linewidth = 1.1) +
  annotate("text", x = -1.35, y = 0.55, label = "A", color = "grey45", size = 5.5, fontface = "bold") +
  annotate("text", x = 1.25, y = 0.55, label = "B", color = azul, size = 6, fontface = "bold") +
  annotate("text", x = 0.05, y = 0, label = 'A*symbol("\\307")*B', parse = TRUE,
           color = "grey10", size = 4.2) +
  annotate("text", x = -1.72, y = -1.05, label = "Omega~(atenuado)", parse = TRUE,
           color = "grey50", size = 3.8) +
  annotate("text", x = 1.45, y = -1.05, label = "nuevo universo", color = azul, size = 3.8) +
  coord_fixed() + theme_void()


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
#| label: venn-particion
#| echo: false
#| fig-width: 7.5
#| fig-height: 2.9
#| out.width: "68%"
elB <- elip_pts(3.1, 1.1, 2.5, 0.72)
franjas <- tibble(x0 = c(0, 2.0, 4.2), x1 = c(2.0, 4.2, 6.2),
                  ev = c("A[1]", "A[2]", "A[3]"),
                  col = c(celeste, verde, naranja))
inter <- bind_rows(lapply(1:3, function(i)
  clip_df(elB, rect_pts(franjas$x0[i], 0, franjas$x1[i], 2.2), "intersection",
          franjas$ev[i])))
ggplot() +
  geom_rect(data = franjas, aes(xmin = x0, xmax = x1, ymin = 0, ymax = 2.2, fill = ev),
            alpha = 0.25, color = "grey40", linewidth = 0.3, show.legend = FALSE) +
  geom_polygon(data = inter, aes(x = x, y = y, group = pieza, fill = op),
               alpha = 0.8, show.legend = FALSE) +
  geom_ellipse(aes(x0 = 3.1, y0 = 1.1, a = 2.5, b = 0.72, angle = 0),
               color = azul, linewidth = 0.7, inherit.aes = FALSE) +
  geom_text(data = franjas, aes(x = (x0 + x1) / 2, y = 2.0, label = ev),
            parse = TRUE, color = azul, size = 5, fontface = "bold") +
  annotate("text", x = 5.45, y = 0.55, label = "B", color = azul, size = 6, fontface = "bold") +
  geom_text(data = tibble(x = c(1.35, 3.1, 4.9), y = 1.1,
                          lab = c('B*symbol("\\307")*A[1]', 'B*symbol("\\307")*A[2]',
                                  'B*symbol("\\307")*A[3]')),
            aes(x = x, y = y, label = lab), parse = TRUE, color = "grey20", size = 3.6) +
  scale_fill_manual(values = setNames(franjas$col, franjas$ev)) +
  coord_fixed() + theme_void()


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
#| label: venn-exclusion
#| echo: false
#| fig-width: 6.0
#| fig-height: 2.6
#| out.width: "52%"
ggplot() +
  geom_rect(aes(xmin = -2.6, xmax = 2.6, ymin = -1.25, ymax = 1.25),
            fill = "grey96", color = "grey40", linewidth = 0.4) +
  geom_circle(aes(x0 = -1.25, y0 = 0, r = 0.85), fill = celeste, alpha = 0.5,
              color = azul, linewidth = 0.6) +
  geom_circle(aes(x0 = 1.25, y0 = 0, r = 0.85), fill = naranja, alpha = 0.5,
              color = azul, linewidth = 0.6) +
  annotate("text", x = -1.25, y = 0, label = "A", color = azul, size = 6, fontface = "bold") +
  annotate("text", x = 1.25, y = 0, label = "B", color = azul, size = 6, fontface = "bold") +
  annotate("text", x = 0, y = -1.02, label = 'A*" "*symbol("\\307")*" "*B*" = "*symbol("\\306")',
           parse = TRUE, color = rojo, size = 5) +
  annotate("text", x = 2.35, y = 1.06, label = "Omega", parse = TRUE, color = "grey40", size = 4.5) +
  coord_fixed() + theme_void()


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

