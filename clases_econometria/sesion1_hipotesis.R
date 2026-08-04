## -----------------------------------------------------------------------------
#| label: setup
#| echo: false
library(tidyverse)
library(gapminder)
library(knitr)
library(broom)
library(plotly)
theme_set(theme_minimal(base_size = 13))
azul <- "#1F4E79"; celeste <- "#2E86C1"; rojo <- "#E74C3C"; verde <- "#27AE60"; naranja <- "#F39C12"; morado <- "#8E44AD"
datos <- gapminder %>% filter(year == 2007)
# En HTML (revealjs) los graficos se vuelven interactivos con plotly;
# en Beamer (PDF) se mantienen estaticos.
es_html <- knitr::is_html_output()
interactivo <- function(p) {
  if (es_html) plotly::config(plotly::ggplotly(p), displayModeBar = FALSE) else p
}

# Datos simulados: programa de empleo juvenil (dos muestras independientes)
set.seed(123)
empleo <- tibble(
  grupo = rep(c("Control", "Tratamiento"), c(120, 110)),
  salario = c(rnorm(120, mean = 520, sd = 90),
              rnorm(110, mean = 555, sd = 130))
)
m_c <- mean(empleo$salario[empleo$grupo == "Control"])
m_t <- mean(empleo$salario[empleo$grupo == "Tratamiento"])
s_c <- sd(empleo$salario[empleo$grupo == "Control"])
s_t <- sd(empleo$salario[empleo$grupo == "Tratamiento"])

# Datos simulados: capacitacion (muestras pareadas)
set.seed(456)
antes <- rnorm(80, mean = 480, sd = 70)
despues <- antes + rnorm(80, mean = 25, sd = 40)

# Tabla de contingencia: educacion y voto
tabla <- matrix(c(180, 150, 90, 20, 50, 110), ncol = 2,
                dimnames = list(Educacion = c("Superior", "Secundaria", "Primaria"),
                                Voto = c("Voto", "No voto")))


## -----------------------------------------------------------------------------
#| label: img-flujo
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/flujo_econometria.png")


## -----------------------------------------------------------------------------
#| label: img-dist-t
#| echo: false
#| out.width: "92%"
knitr::include_graphics("figuras/distribucion_prueba_t.png")


## -----------------------------------------------------------------------------
#| label: plot-pvalor
#| echo: false
#| fig-height: 3.0
t_obs <- 2.3; gl <- 24
p_bilateral <- 2 * pt(-abs(t_obs), gl)
curva <- tibble(x = seq(-4.5, 4.5, 0.01), d = dt(seq(-4.5, 4.5, 0.01), gl))
p <- ggplot(curva, aes(x, d)) +
  geom_area(data = filter(curva, x >= t_obs), fill = rojo, alpha = 0.65) +
  geom_area(data = filter(curva, x <= -t_obs), fill = rojo, alpha = 0.65) +
  geom_line(color = azul, linewidth = 1.1) +
  geom_vline(xintercept = t_obs, color = rojo, linewidth = 1) +
  annotate("text", x = t_obs, y = 0.33, label = "t observado = 2.3", color = rojo, size = 4.2) +
  annotate("text", x = 3.1, y = 0.045, label = "p/2", color = rojo, size = 4.5) +
  annotate("text", x = -3.1, y = 0.045, label = "p/2", color = rojo, size = 4.5) +
  labs(x = "Estadístico t (distribución nula, 24 gl)", y = "Densidad",
       title = paste0("Área sombreada = valor-p = ", round(p_bilateral, 3)))
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-pvalor-escenarios
#| echo: false
#| out.width: "60%"
knitr::include_graphics("figuras/h0_h1_pvalor.png")


## -----------------------------------------------------------------------------
#| label: img-errores
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/errores_tipo.png")


## -----------------------------------------------------------------------------
#| label: plot-potencia-anatomia
#| echo: false
#| fig-height: 3.2
crit <- qnorm(0.975)
malla <- tibble(x = seq(-4, 7, 0.01),
                h0 = dnorm(seq(-4, 7, 0.01), 0, 1),
                h1 = dnorm(seq(-4, 7, 0.01), 2.8, 1))
p <- ggplot(malla, aes(x)) +
  geom_area(data = filter(malla, x >= crit), aes(y = h1), fill = verde, alpha = 0.4) +
  geom_area(data = filter(malla, x <= crit), aes(y = h1), fill = naranja, alpha = 0.55) +
  geom_area(data = filter(malla, x >= crit), aes(y = h0), fill = rojo, alpha = 0.7) +
  geom_line(aes(y = h0), color = azul, linewidth = 1.1) +
  geom_line(aes(y = h1), color = rojo, linewidth = 1.1) +
  geom_vline(xintercept = crit, linetype = "dashed", color = "grey30") +
  annotate("text", x = -1.6, y = 0.36, label = "Bajo H0", color = azul, size = 4.5) +
  annotate("text", x = 4.6, y = 0.36, label = "Bajo H1 (efecto real)", color = rojo, size = 4.5) +
  annotate("text", x = 2.45, y = 0.085, label = "alfa", color = rojo, size = 4) +
  annotate("text", x = 1.0, y = 0.05, label = "beta", color = "#A04000", size = 4) +
  annotate("text", x = 4.7, y = 0.16, label = "Potencia = 1 - beta", color = verde, size = 4.5) +
  annotate("text", x = 2.6, y = 0.42, label = "valor crítico", color = "grey30", size = 3.8) +
  labs(x = "Valor del estadístico", y = "Densidad")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-potencia-n
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/potencia_estadistica.png")


## -----------------------------------------------------------------------------
#| label: code-power
# ¿Que n necesito? (d = 0.5)
p1 <- power.t.test(delta = 0.5,
                   sd = 1,
                   sig.level = 0.05,
                   power = 0.80)
ceiling(p1$n)   # n por grupo

# ¿Que potencia logro con n = 30?
p2 <- power.t.test(n = 30,
                   delta = 0.5,
                   sd = 1,
                   sig.level = 0.05)
round(p2$power, 2)


## -----------------------------------------------------------------------------
#| label: plot-curvas-potencia
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
malla_pot <- expand_grid(n = seq(10, 200, 5), d = c(0.2, 0.5, 0.8)) %>%
  mutate(potencia = map2_dbl(n, d, ~ power.t.test(n = .x, delta = .y,
                                                  sd = 1, sig.level = 0.05)$power),
         efecto = factor(d, labels = c("d = 0.2 (pequeño)", "d = 0.5 (mediano)",
                                       "d = 0.8 (grande)")))
p <- ggplot(malla_pot, aes(n, potencia, color = efecto)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  annotate("text", x = 175, y = 0.86, label = "objetivo 0.80", color = "grey40", size = 4) +
  scale_color_manual(values = c(celeste, verde, rojo)) +
  labs(x = "n por grupo", y = "Potencia (1 - beta)", color = NULL,
       title = "Curvas de potencia con power.t.test") +
  theme(legend.position = "bottom")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-t-una
t1 <- t.test(datos$lifeExp, mu = 70)
tidy(t1) %>%
  select(estimate, statistic, p.value, conf.low, conf.high) %>%
  mutate(across(everything(), ~ round(.x, 3)))


## -----------------------------------------------------------------------------
#| label: code-t-dos
welch  <- t.test(salario ~ grupo, data = empleo)
pooled <- t.test(salario ~ grupo, data = empleo, var.equal = TRUE)
tibble(version = c("Welch", "Pooled"),
       t = c(welch$statistic, pooled$statistic),
       gl = c(welch$parameter, pooled$parameter),
       p = c(welch$p.value, pooled$p.value)) %>%
  mutate(across(-version, ~ round(.x, 3)))


## -----------------------------------------------------------------------------
#| label: code-t-pareada
res <- t.test(despues, antes,
              paired = TRUE)
# diferencia media
round(as.numeric(res$estimate), 1)
# IC 95% de la diferencia
round(as.numeric(res$conf.int), 1)
# valor-p
signif(res$p.value, 2)


## -----------------------------------------------------------------------------
#| label: plot-pareada
#| echo: false
#| fig-width: 5.6
#| fig-height: 3.9
#| out.width: "100%"
dif <- despues - antes
p <- ggplot(tibble(dif), aes(dif)) +
  geom_histogram(bins = 18, fill = celeste, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.9) +
  geom_vline(xintercept = mean(dif), color = rojo, linewidth = 1.1) +
  annotate("text", x = mean(dif) + 22, y = 11.5,
           label = paste0("media = ", round(mean(dif), 1)), color = rojo, size = 4.3) +
  labs(x = "Diferencia individual (después - antes)", y = "N° de trabajadores",
       title = "Distribución de las diferencias pareadas")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-chi
#| echo: false
#| out.width: "94%"
knitr::include_graphics("figuras/chi_cuadrado.png")


## -----------------------------------------------------------------------------
#| label: tabla-contingencia
#| echo: false
tab_m <- addmargins(tabla)
dimnames(tab_m)[[1]][4] <- "Total"
dimnames(tab_m)[[2]][3] <- "Total"
kable(tab_m)


## -----------------------------------------------------------------------------
#| label: code-chi
chisq.test(tabla)


## -----------------------------------------------------------------------------
#| label: plot-obs-esp
#| echo: false
#| fig-height: 3.1
chi <- chisq.test(tabla)
obs <- as.data.frame(as.table(chi$observed)) %>% mutate(tipo = "Observado")
esp <- as.data.frame(as.table(chi$expected)) %>% mutate(tipo = "Esperado bajo H0")
df_oe <- bind_rows(obs, esp) %>%
  mutate(Educacion = factor(Educacion, levels = c("Superior", "Secundaria", "Primaria")),
         tipo = factor(tipo, levels = c("Observado", "Esperado bajo H0")))
p <- ggplot(df_oe, aes(Educacion, Freq, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  facet_wrap(~ Voto) +
  scale_fill_manual(values = c(celeste, naranja)) +
  labs(x = NULL, y = "Frecuencia", fill = NULL,
       title = "Frecuencias observadas vs esperadas bajo independencia") +
  theme(legend.position = "top")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-prop-una
# 132 de 200 comunas elegibles
# implementaron el programa
una <- prop.test(x = 132, n = 200,
                 p = 0.5)
round(as.numeric(una$estimate), 3)
signif(una$p.value, 3)


## -----------------------------------------------------------------------------
#| label: code-prop-dos
# empleo a 12 meses:
# 96/160 tratados, 62/158 control
dos <- prop.test(x = c(96, 62),
                 n = c(160, 158))
round(as.numeric(dos$estimate), 3)
signif(dos$p.value, 3)


## -----------------------------------------------------------------------------
#| label: plot-relevancia
#| echo: false
#| fig-height: 2.9
df_ic <- tibble(
  estudio = c("Piloto RCT (n = 40)", "Registro administrativo (n = 250.000)"),
  est = c(8, 0.4), lo = c(-1.2, 0.1), hi = c(17.2, 0.7),
  etiqueta = c("p = 0.086", "p < 0.001"))
p <- ggplot(df_ic, aes(x = est, y = estudio)) +
  annotate("rect", xmin = -2, xmax = 2, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = lo, xmax = hi), color = azul, linewidth = 1.1, size = 0.7) +
  geom_text(aes(label = etiqueta), vjust = -1.3, color = rojo, size = 4.3) +
  annotate("text", x = 9, y = 0.55, label = "zona sombreada: irrelevancia práctica (menos de 2 pp)",
           color = "grey35", size = 3.6) +
  coord_cartesian(xlim = c(-4, 19), ylim = c(0.4, 2.5)) +
  labs(x = "Efecto sobre la tasa de empleo (puntos porcentuales)", y = NULL,
       title = "Mismo programa, dos estudios")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: code-cohen
sal_c <- empleo$salario[empleo$grupo == "Control"]
sal_t <- empleo$salario[empleo$grupo == "Tratamiento"]
sp <- sqrt(((length(sal_c) - 1) * var(sal_c) +
            (length(sal_t) - 1) * var(sal_t)) / (nrow(empleo) - 2))
round((mean(sal_t) - mean(sal_c)) / sp, 2)


## -----------------------------------------------------------------------------
#| label: plot-phacking
#| echo: false
#| fig-height: 3.2
set.seed(2026)
p_unico <- replicate(5000, t.test(rnorm(30), rnorm(30))$p.value)
p_min5 <- replicate(2000, min(replicate(5, t.test(rnorm(30), rnorm(30))$p.value)))
sim_ph <- bind_rows(
  tibble(p = p_unico, escenario = "Una prueba por estudio"),
  tibble(p = p_min5, escenario = "Se reporta el mínimo de 5 pruebas")
) %>%
  mutate(escenario = factor(escenario, levels = unique(escenario)))
p <- ggplot(sim_ph, aes(p, fill = p < 0.05)) +
  geom_histogram(breaks = seq(0, 1, 0.05), color = "white") +
  scale_fill_manual(values = c("FALSE" = celeste, "TRUE" = rojo), guide = "none") +
  facet_wrap(~ escenario, scales = "free_y") +
  labs(x = "valor-p reportado", y = "Frecuencia")
interactivo(p)


## -----------------------------------------------------------------------------
#| label: img-matriz
#| echo: false
#| out.width: "100%"
knitr::include_graphics("figuras/matriz_decision.png")

