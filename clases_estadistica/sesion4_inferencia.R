# ==============================================================================
# Sesión 4: Inferencia Estadística — Estimación
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================

# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2", "MASS"))
library(tidyverse)
library(ggplot2)
library(MASS)  # Dataset Boston (precios de viviendas)

# ==============================================================================
# 1. POBLACIÓN VS MUESTRA
# ==============================================================================

cat("=== POBLACIÓN VS MUESTRA ===\n")

# Usamos el dataset Boston de MASS
# medv = valor mediano de viviendas (en miles USD)
data(Boston)
cat("Dataset Boston: precios de viviendas en suburbios de Boston\n")
cat("N (población simulada):", nrow(Boston), "observaciones\n")
str(Boston[, c("medv", "rm", "lstat", "crim")])

# Parámetros poblacionales (tratamos Boston como la "población")
mu_pop <- mean(Boston$medv)
sigma_pop <- sd(Boston$medv)
cat("\nParámetros poblacionales:\n")
cat("  μ (media) =", round(mu_pop, 2), "mil USD\n")
cat("  σ (desv. est.) =", round(sigma_pop, 2), "mil USD\n")

# Tomar UNA muestra aleatoria
set.seed(42)
n <- 30
muestra <- sample(Boston$medv, n)
cat("\nEstadísticos muestrales (n =", n, "):\n")
cat("  x̄ (media) =", round(mean(muestra), 2), "\n")
cat("  s (desv. est.) =", round(sd(muestra), 2), "\n")
cat("  Diferencia |x̄ - μ| =", round(abs(mean(muestra) - mu_pop), 2), "\n")

# ==============================================================================
# 2. DISTRIBUCIÓN MUESTRAL DE LA MEDIA
# ==============================================================================

cat("\n=== DISTRIBUCIÓN MUESTRAL DE LA MEDIA ===\n")

set.seed(123)
n_muestras <- 5000
n <- 30

medias_muestrales <- replicate(n_muestras, mean(sample(Boston$medv, n)))

cat("E(X̄) teórica =", round(mu_pop, 2), "\n")
cat("E(X̄) simulada =", round(mean(medias_muestrales), 2), "\n")
cat("SE teórico = σ/√n =", round(sigma_pop / sqrt(n), 2), "\n")
cat("SE simulado =", round(sd(medias_muestrales), 2), "\n")

# Visualización
df_dist_muestral <- data.frame(media = medias_muestrales)

p1 <- ggplot(df_dist_muestral, aes(x = media)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#2E86C1", alpha = 0.6, color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mu_pop, sd = sigma_pop / sqrt(n)),
                color = "red", size = 1.2) +
  geom_vline(xintercept = mu_pop, color = "#E74C3C",
             linetype = "dashed", size = 1) +
  labs(title = "Distribución Muestral de la Media (n = 30)",
       subtitle = paste("5,000 muestras de Boston$medv | μ =",
                        round(mu_pop, 1), ", SE =",
                        round(sigma_pop / sqrt(n), 2)),
       x = "Media muestral (miles USD)", y = "Densidad") +
  theme_minimal()
print(p1)

# ==============================================================================
# 3. PROPIEDADES DE LOS ESTIMADORES
# ==============================================================================

cat("\n=== PROPIEDADES DE LOS ESTIMADORES ===\n")

# Insesgadez: comparar distintos tamaños de muestra
tamanos <- c(5, 10, 30, 50, 100, 200)
resultados <- data.frame()

set.seed(456)
for (n_size in tamanos) {
  medias <- replicate(5000, mean(sample(Boston$medv, n_size)))
  resultados <- rbind(resultados, data.frame(
    n = n_size,
    media_de_medias = mean(medias),
    se_estimado = sd(medias),
    se_teorico = sigma_pop / sqrt(n_size),
    sesgo = mean(medias) - mu_pop
  ))
}

cat("\n  n     E(X̄)     SE_est   SE_teor  Sesgo\n")
cat("------  ------   ------   ------   ------\n")
for (i in 1:nrow(resultados)) {
  cat(sprintf("  %3d   %6.2f   %6.2f   %6.2f   %6.3f\n",
              resultados$n[i], resultados$media_de_medias[i],
              resultados$se_estimado[i], resultados$se_teorico[i],
              resultados$sesgo[i]))
}
cat("\n→ El sesgo es ~0 para todos los n (estimador insesgado)\n")
cat("→ El SE decrece con √n (estimador consistente)\n")

# Eficiencia: comparar media vs mediana como estimadores de μ
set.seed(789)
n <- 30
medias_est <- replicate(5000, mean(sample(Boston$medv, n)))
medianas_est <- replicate(5000, median(sample(Boston$medv, n)))

cat("\nEficiencia (estimando μ):\n")
cat("  Var(media):   ", round(var(medias_est), 3), "\n")
cat("  Var(mediana): ", round(var(medianas_est), 3), "\n")
cat("  Eficiencia relativa:", round(var(medias_est) / var(medianas_est), 3), "\n")
cat("  → La media es más eficiente (menor varianza)\n")

# ==============================================================================
# 4. INTERVALOS DE CONFIANZA PARA LA MEDIA
# ==============================================================================

cat("\n=== INTERVALOS DE CONFIANZA ===\n")

# IC para la media con σ conocido (Z)
set.seed(42)
n <- 30
muestra <- sample(Boston$medv, n)
x_bar <- mean(muestra)
s <- sd(muestra)

# IC al 95% usando t (σ desconocido — caso real)
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = n - 1)
se <- s / sqrt(n)
ic_lower <- x_bar - t_crit * se
ic_upper <- x_bar + t_crit * se

cat("Muestra: n =", n, ", x̄ =", round(x_bar, 2),
    ", s =", round(s, 2), "\n")
cat("IC 95%: [", round(ic_lower, 2), ",", round(ic_upper, 2), "]\n")
cat("t* (df =", n-1, ") =", round(t_crit, 3), "\n")
cat("¿Contiene μ =", round(mu_pop, 2), "?",
    ifelse(mu_pop >= ic_lower & mu_pop <= ic_upper, "SÍ", "NO"), "\n")

# Función t.test() — la forma estándar en R
cat("\nUsando t.test():\n")
print(t.test(muestra, conf.level = 0.95))

# ==============================================================================
# 5. COBERTURA DEL IC (Simulación)
# ==============================================================================

cat("\n=== SIMULACIÓN DE COBERTURA DEL IC ===\n")

set.seed(2026)
n <- 30
n_sim <- 1000
alpha <- 0.05

# Generar n_sim intervalos de confianza
ics <- data.frame(
  id = 1:n_sim,
  lower = numeric(n_sim),
  upper = numeric(n_sim),
  x_bar = numeric(n_sim),
  contiene_mu = logical(n_sim)
)

for (i in 1:n_sim) {
  muestra_i <- sample(Boston$medv, n)
  test <- t.test(muestra_i, conf.level = 1 - alpha)
  ics$lower[i] <- test$conf.int[1]
  ics$upper[i] <- test$conf.int[2]
  ics$x_bar[i] <- test$estimate
  ics$contiene_mu[i] <- mu_pop >= test$conf.int[1] & mu_pop <= test$conf.int[2]
}

cobertura <- mean(ics$contiene_mu) * 100
cat("Cobertura empírica (", n_sim, "ICs al 95%):", cobertura, "%\n")
cat("Cobertura nominal: 95%\n")

# Visualizar 100 ICs
ics_plot <- ics[1:100, ]
ics_plot$id <- 1:100

p2 <- ggplot(ics_plot, aes(x = id, y = x_bar, color = contiene_mu)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, size = 0.4) +
  geom_hline(yintercept = mu_pop, color = "red",
             linetype = "dashed", size = 0.8) +
  scale_color_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#2E86C1"),
                     labels = c("No contiene μ", "Contiene μ")) +
  coord_flip() +
  labs(title = "100 Intervalos de Confianza al 95%",
       subtitle = paste("Línea roja = μ real =", round(mu_pop, 1),
                        "| Cobertura =", round(cobertura, 1), "%"),
       x = "Muestra", y = "Valor mediano de vivienda (miles USD)",
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)

# ==============================================================================
# 6. EFECTO DEL TAMAÑO DE MUESTRA EN EL IC
# ==============================================================================

cat("\n=== EFECTO DEL TAMAÑO DE MUESTRA ===\n")

set.seed(100)
tamanos_n <- c(10, 20, 30, 50, 100, 200, 500)
anchos_ic <- numeric(length(tamanos_n))

for (i in seq_along(tamanos_n)) {
  muestra_i <- sample(Boston$medv, tamanos_n[i])
  test <- t.test(muestra_i, conf.level = 0.95)
  anchos_ic[i] <- diff(test$conf.int)
}

df_ancho <- data.frame(n = tamanos_n, ancho = anchos_ic)

cat("\n  n     Ancho del IC 95%\n")
cat("------  ----------------\n")
for (i in 1:nrow(df_ancho)) {
  cat(sprintf("  %3d   %8.2f\n", df_ancho$n[i], df_ancho$ancho[i]))
}

p3 <- ggplot(df_ancho, aes(x = n, y = ancho)) +
  geom_line(color = "#1F4E79", size = 1.2) +
  geom_point(color = "#E74C3C", size = 3) +
  labs(title = "Ancho del IC al 95% según Tamaño de Muestra",
       subtitle = "El IC se reduce proporcional a 1/√n",
       x = "Tamaño de muestra (n)", y = "Ancho del IC (miles USD)") +
  theme_minimal()
print(p3)

# ==============================================================================
# 7. IC PARA DIFERENTES NIVELES DE CONFIANZA
# ==============================================================================

cat("\n=== IC PARA DIFERENTES NIVELES DE CONFIANZA ===\n")

set.seed(42)
muestra <- sample(Boston$medv, 50)
niveles <- c(0.90, 0.95, 0.99)

cat("\nMuestra: n = 50, x̄ =", round(mean(muestra), 2), "\n\n")
for (nivel in niveles) {
  test <- t.test(muestra, conf.level = nivel)
  cat(sprintf("IC al %d%%: [%.2f, %.2f] — Ancho: %.2f\n",
              nivel * 100, test$conf.int[1], test$conf.int[2],
              diff(test$conf.int)))
}
cat("\n→ Mayor confianza = IC más ancho (trade-off)\n")

# ==============================================================================
# 8. IC PARA PROPORCIONES
# ==============================================================================

cat("\n=== IC PARA PROPORCIONES ===\n")

# Ejemplo: proporción de viviendas con valor > 30 mil USD
umbral <- 30
exitos <- sum(Boston$medv > umbral)
n_total <- nrow(Boston)
p_hat <- exitos / n_total

cat("¿Proporción de viviendas con valor > $30K?\n")
cat("p̂ =", round(p_hat, 3), "(", exitos, "de", n_total, ")\n")

# IC para proporción (Wald)
se_p <- sqrt(p_hat * (1 - p_hat) / n_total)
z_crit <- qnorm(0.975)
ic_p_lower <- p_hat - z_crit * se_p
ic_p_upper <- p_hat + z_crit * se_p
cat("IC 95% (Wald): [", round(ic_p_lower, 3), ",",
    round(ic_p_upper, 3), "]\n")

# Usando prop.test()
cat("\nUsando prop.test():\n")
print(prop.test(exitos, n_total, conf.level = 0.95))

# ==============================================================================
# 9. IC PARA DIFERENCIA DE MEDIAS
# ==============================================================================

cat("\n=== IC PARA DIFERENCIA DE MEDIAS ===\n")

# Comparar precios: zonas con alta vs baja criminalidad
Boston <- Boston %>%
  mutate(alta_crim = ifelse(crim > median(crim), "Alta", "Baja"))

cat("Comparar valor de viviendas según nivel de criminalidad:\n")
Boston %>%
  group_by(alta_crim) %>%
  summarise(
    n = n(),
    media = round(mean(medv), 2),
    desv_est = round(sd(medv), 2),
    .groups = "drop"
  ) %>%
  print()

# Test t para dos muestras
test_2m <- t.test(medv ~ alta_crim, data = Boston, conf.level = 0.95)
cat("\nt.test para diferencia de medias:\n")
print(test_2m)

# Visualización
p4 <- ggplot(Boston, aes(x = alta_crim, y = medv, fill = alta_crim)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  scale_fill_manual(values = c("#E74C3C", "#2E86C1")) +
  labs(title = "Valor de Vivienda según Nivel de Criminalidad",
       subtitle = paste("Diferencia de medias:",
                        round(diff(test_2m$estimate), 2),
                        "| IC 95%: [",
                        round(test_2m$conf.int[1], 2), ",",
                        round(test_2m$conf.int[2], 2), "]"),
       x = "Nivel de criminalidad", y = "Valor mediano (miles USD)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)

# ==============================================================================
# 10. EJERCICIO PRÁCTICO: ANÁLISIS COMPLETO
# ==============================================================================

cat("\n=== EJERCICIO PRÁCTICO ===\n")

# Variable: número de habitaciones (rm)
set.seed(2026)
n <- 40
muestra_rm <- sample(Boston$rm, n)

cat("\nAnálisis de número de habitaciones (n =", n, "):\n")
cat("x̄ =", round(mean(muestra_rm), 3), "\n")
cat("s =", round(sd(muestra_rm), 3), "\n")
cat("SE =", round(sd(muestra_rm)/sqrt(n), 3), "\n")

test_rm <- t.test(muestra_rm, conf.level = 0.95)
cat("IC 95%: [", round(test_rm$conf.int[1], 3), ",",
    round(test_rm$conf.int[2], 3), "]\n")
cat("μ real =", round(mean(Boston$rm), 3), "\n")
cat("¿Contiene μ?",
    ifelse(mean(Boston$rm) >= test_rm$conf.int[1] &
           mean(Boston$rm) <= test_rm$conf.int[2], "SÍ", "NO"), "\n")

cat("\n✓ Fin de Sesión 4\n")
