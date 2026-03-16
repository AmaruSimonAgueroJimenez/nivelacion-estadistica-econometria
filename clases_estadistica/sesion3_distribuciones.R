# ==============================================================================
# Sesión 3: Distribuciones de Probabilidad
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================

# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2"))
library(tidyverse)
library(ggplot2)

# ==============================================================================
# 1. DISTRIBUCIÓN BERNOULLI
# ==============================================================================

cat("=== DISTRIBUCIÓN BERNOULLI ===\n")

# Ejemplo: encuesta — ¿apoya una política pública? (p = 0.6)
p <- 0.6
cat("Bernoulli con p =", p, "\n")
cat("E(X) =", p, "\n")
cat("Var(X) =", p * (1 - p), "\n")

# Simular 20 respuestas
set.seed(42)
respuestas <- rbinom(20, size = 1, prob = p)
cat("Simulación (20 personas):", respuestas, "\n")
cat("Proporción de 'Sí':", mean(respuestas), "\n")

# ==============================================================================
# 2. DISTRIBUCIÓN BINOMIAL
# ==============================================================================

cat("\n=== DISTRIBUCIÓN BINOMIAL ===\n")

# Ejemplo: De 30 hogares encuestados, ¿cuántos tienen acceso a internet?
# Si p = 0.7 (70% de penetración)
n <- 30
p <- 0.7

cat("Binomial(n =", n, ", p =", p, ")\n")
cat("E(X) = n*p =", n * p, "\n")
cat("Var(X) = n*p*(1-p) =", n * p * (1 - p), "\n")
cat("DE(X) =", round(sqrt(n * p * (1 - p)), 2), "\n")

# Probabilidades puntuales
cat("\nP(X = 20) =", round(dbinom(20, n, p), 4), "\n")
cat("P(X = 21) =", round(dbinom(21, n, p), 4), "\n")
cat("P(X >= 25) =", round(1 - pbinom(24, n, p), 4), "\n")
cat("P(X <= 15) =", round(pbinom(15, n, p), 4), "\n")

# Visualización de la PMF
x_binom <- 0:n
df_binom <- data.frame(
  x = x_binom,
  prob = dbinom(x_binom, n, p)
)

p1 <- ggplot(df_binom, aes(x = x, y = prob)) +
  geom_col(fill = "#2E86C1", alpha = 0.7, width = 0.8) +
  geom_vline(xintercept = n * p, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = n * p + 2, y = max(df_binom$prob),
           label = paste("E(X) =", n * p), color = "red") +
  labs(title = paste0("Distribución Binomial (n = ", n, ", p = ", p, ")"),
       subtitle = "Hogares con acceso a internet",
       x = "Número de hogares con acceso", y = "Probabilidad") +
  theme_minimal()
print(p1)

# ==============================================================================
# 3. DISTRIBUCIÓN DE POISSON
# ==============================================================================

cat("\n=== DISTRIBUCIÓN DE POISSON ===\n")

# Ejemplo: número de llamadas a emergencias por hora (lambda = 5)
lambda <- 5
cat("Poisson(lambda =", lambda, ")\n")
cat("E(X) = Var(X) =", lambda, "\n")

# Probabilidades
cat("\nP(X = 0) =", round(dpois(0, lambda), 4), "\n")
cat("P(X = 5) =", round(dpois(5, lambda), 4), "\n")
cat("P(X >= 10) =", round(1 - ppois(9, lambda), 4), "\n")

# Comparar Binomial vs Poisson
# Cuando n es grande y p es pequeño, Binomial ≈ Poisson
n_grande <- 1000
p_peq <- 0.005  # lambda = n*p = 5
x_vals <- 0:15

df_comp <- data.frame(
  x = rep(x_vals, 2),
  prob = c(dbinom(x_vals, n_grande, p_peq), dpois(x_vals, lambda)),
  dist = rep(c("Binomial(1000, 0.005)", "Poisson(5)"), each = length(x_vals))
)

p2 <- ggplot(df_comp, aes(x = x, y = prob, fill = dist)) +
  geom_col(position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("#1F4E79", "#E74C3C")) +
  labs(title = "Binomial vs Poisson (Aproximación)",
       subtitle = "Cuando n grande y p pequeño, Binomial ≈ Poisson",
       x = "k", y = "P(X = k)", fill = "Distribución") +
  theme_minimal()
print(p2)

# ==============================================================================
# 4. DISTRIBUCIÓN NORMAL
# ==============================================================================

cat("\n=== DISTRIBUCIÓN NORMAL ===\n")

# Ejemplo: ingresos mensuales con media = 800 y DE = 200 (en miles CLP)
mu <- 800
sigma <- 200

cat("Normal(mu =", mu, ", sigma =", sigma, ")\n")

# Probabilidades
cat("\nP(X < 600) =", round(pnorm(600, mu, sigma), 4), "\n")
cat("P(X > 1000) =", round(1 - pnorm(1000, mu, sigma), 4), "\n")
cat("P(600 < X < 1000) =",
    round(pnorm(1000, mu, sigma) - pnorm(600, mu, sigma), 4), "\n")

# Regla empírica 68-95-99.7
cat("\nRegla empírica:\n")
cat("P(μ ± 1σ) =", round(pnorm(mu + sigma, mu, sigma) -
                            pnorm(mu - sigma, mu, sigma), 4), "\n")
cat("P(μ ± 2σ) =", round(pnorm(mu + 2*sigma, mu, sigma) -
                            pnorm(mu - 2*sigma, mu, sigma), 4), "\n")
cat("P(μ ± 3σ) =", round(pnorm(mu + 3*sigma, mu, sigma) -
                            pnorm(mu - 3*sigma, mu, sigma), 4), "\n")

# Quantiles
cat("\nPercentil 25:", round(qnorm(0.25, mu, sigma), 1), "\n")
cat("Percentil 50 (mediana):", round(qnorm(0.50, mu, sigma), 1), "\n")
cat("Percentil 75:", round(qnorm(0.75, mu, sigma), 1), "\n")
cat("Percentil 95:", round(qnorm(0.95, mu, sigma), 1), "\n")

# Visualización de diferentes normales
x_rng <- seq(0, 1600, by = 1)
df_norm <- data.frame(
  x = rep(x_rng, 3),
  densidad = c(
    dnorm(x_rng, 800, 200),
    dnorm(x_rng, 800, 100),
    dnorm(x_rng, 600, 200)
  ),
  params = rep(c("N(800, 200)", "N(800, 100)", "N(600, 200)"), each = length(x_rng))
)

p3 <- ggplot(df_norm, aes(x = x, y = densidad, color = params)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#1F4E79", "#2E86C1", "#E74C3C")) +
  labs(title = "Distribuciones Normales con Diferentes Parámetros",
       x = "Ingreso (miles CLP)", y = "Densidad", color = "Parámetros") +
  theme_minimal()
print(p3)

# ==============================================================================
# 5. TEOREMA DEL LÍMITE CENTRAL (TLC)
# ==============================================================================

cat("\n=== TEOREMA DEL LÍMITE CENTRAL ===\n")

# Población: distribución uniforme (NO normal)
set.seed(123)
n_muestras <- 10000

# Diferentes tamaños de muestra
tamanos <- c(5, 15, 30, 100)

par(mfrow = c(2, 2))  # Para base R plots

resultados_tlc <- list()
for (n_muestra in tamanos) {
  medias <- replicate(n_muestras, {
    muestra <- runif(n_muestra, min = 0, max = 10)
    mean(muestra)
  })
  resultados_tlc[[as.character(n_muestra)]] <- medias
}

# Visualización con ggplot2
df_tlc <- bind_rows(
  lapply(names(resultados_tlc), function(n) {
    data.frame(
      media = resultados_tlc[[n]],
      n = paste("n =", n)
    )
  })
)
df_tlc$n <- factor(df_tlc$n, levels = paste("n =", tamanos))

p4 <- ggplot(df_tlc, aes(x = media)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#2E86C1", alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = 5, sd = sqrt(100/12) / sqrt(5)),
                color = "red", size = 0.8,
                data = data.frame(x = c(2, 8), n = factor("n = 5",
                                  levels = levels(df_tlc$n)))) +
  facet_wrap(~ n, scales = "free_y") +
  labs(title = "Teorema del Límite Central",
       subtitle = "Distribución de medias muestrales (población Uniforme[0,10])",
       x = "Media muestral", y = "Densidad") +
  theme_minimal()
print(p4)

# Verificar convergencia
cat("\nPoblación: Uniforme[0, 10]\n")
cat("Media teórica: 5, DE teórica:", round(sqrt(100/12), 3), "\n\n")
for (n_muestra in tamanos) {
  medias <- resultados_tlc[[as.character(n_muestra)]]
  cat(sprintf("n = %3d: Media = %.3f, DE = %.3f, DE teórica = %.3f\n",
              n_muestra, mean(medias), sd(medias),
              sqrt(100/12) / sqrt(n_muestra)))
}

# ==============================================================================
# 6. DISTRIBUCIÓN t DE STUDENT
# ==============================================================================

cat("\n=== DISTRIBUCIÓN t DE STUDENT ===\n")

# Comparar t con distintos grados de libertad vs Normal
x_t <- seq(-5, 5, by = 0.01)
df_t <- data.frame(
  x = rep(x_t, 4),
  densidad = c(
    dt(x_t, df = 3),
    dt(x_t, df = 10),
    dt(x_t, df = 30),
    dnorm(x_t)
  ),
  dist = rep(c("t(df=3)", "t(df=10)", "t(df=30)", "Normal"), each = length(x_t))
)

p5 <- ggplot(df_t, aes(x = x, y = densidad, color = dist)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("#1F4E79", "#2E86C1", "#E74C3C", "black")) +
  labs(title = "Distribución t de Student vs Normal",
       subtitle = "Conforme df → ∞, t → Normal",
       x = "x", y = "Densidad", color = "Distribución") +
  theme_minimal()
print(p5)

# Valores críticos
cat("Valores críticos (α = 0.05, dos colas):\n")
for (gl in c(3, 10, 30, Inf)) {
  t_crit <- if (is.infinite(gl)) qnorm(0.975) else qt(0.975, gl)
  cat(sprintf("  df = %3s: t* = %.3f\n",
              ifelse(is.infinite(gl), "∞", as.character(gl)), t_crit))
}

# ==============================================================================
# 7. DISTRIBUCIÓN CHI-CUADRADO
# ==============================================================================

cat("\n=== DISTRIBUCIÓN CHI-CUADRADO ===\n")

x_chi <- seq(0.01, 40, by = 0.1)
df_chi <- data.frame(
  x = rep(x_chi, 4),
  densidad = c(
    dchisq(x_chi, df = 2),
    dchisq(x_chi, df = 5),
    dchisq(x_chi, df = 10),
    dchisq(x_chi, df = 20)
  ),
  dist = rep(c("χ²(df=2)", "χ²(df=5)", "χ²(df=10)", "χ²(df=20)"),
             each = length(x_chi))
)

p6 <- ggplot(df_chi, aes(x = x, y = densidad, color = dist)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("#1F4E79", "#2E86C1", "#E74C3C", "#27AE60")) +
  labs(title = "Distribución Chi-Cuadrado",
       subtitle = "Conforme df crece, se parece más a una normal",
       x = "x", y = "Densidad", color = "Distribución") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal()
print(p6)

# ==============================================================================
# 8. DISTRIBUCIÓN F
# ==============================================================================

cat("\n=== DISTRIBUCIÓN F ===\n")

x_f <- seq(0.01, 6, by = 0.01)
df_f <- data.frame(
  x = rep(x_f, 3),
  densidad = c(
    df(x_f, df1 = 5, df2 = 10),
    df(x_f, df1 = 10, df2 = 30),
    df(x_f, df1 = 50, df2 = 50)
  ),
  dist = rep(c("F(5, 10)", "F(10, 30)", "F(50, 50)"), each = length(x_f))
)

p7 <- ggplot(df_f, aes(x = x, y = densidad, color = dist)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("#1F4E79", "#E74C3C", "#27AE60")) +
  labs(title = "Distribución F de Fisher",
       subtitle = "Usada en ANOVA y tests de varianza",
       x = "x", y = "Densidad", color = "Distribución") +
  theme_minimal()
print(p7)

# ==============================================================================
# 9. GENERACIÓN DE VARIABLES ALEATORIAS EN R
# ==============================================================================

cat("\n=== Funciones r*, d*, p*, q* en R ===\n")

cat("\nPatrón: r (random), d (density), p (probability), q (quantile)\n\n")

# Ejemplo completo con la normal
cat("dnorm(0) =", dnorm(0), "       (densidad en x=0)\n")
cat("pnorm(1.96) =", pnorm(1.96), " (P(Z ≤ 1.96))\n")
cat("qnorm(0.975) =", qnorm(0.975), " (z tal que P(Z ≤ z) = 0.975)\n")
cat("rnorm(5) =", round(rnorm(5), 2), "(5 valores aleatorios)\n")

# Tabla resumen de distribuciones
cat("\n--- Resumen de distribuciones ---\n")
cat("Distribución     | Tipo     | Parámetros        | E(X)     | Var(X)\n")
cat("-----------------|----------|-------------------|----------|--------\n")
cat("Bernoulli        | Discreta | p                 | p        | p(1-p)\n")
cat("Binomial         | Discreta | n, p              | np       | np(1-p)\n")
cat("Poisson          | Discreta | λ                 | λ        | λ\n")
cat("Normal           | Continua | μ, σ              | μ        | σ²\n")
cat("t de Student     | Continua | df                | 0        | df/(df-2)\n")
cat("Chi-cuadrado     | Continua | df                | df       | 2*df\n")
cat("F de Fisher      | Continua | df1, df2          | complejo | complejo\n")

cat("\n✓ Fin de Sesión 3\n")
