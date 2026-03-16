# ==============================================================================
# Sesión 2: Fundamentos de Probabilidad
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================

# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2"))
library(tidyverse)
library(ggplot2)

# ==============================================================================
# 1. CONCEPTOS BÁSICOS DE PROBABILIDAD
# ==============================================================================

# Espacio muestral: lanzar un dado
espacio_muestral <- 1:6
cat("Espacio muestral (dado):", espacio_muestral, "\n")

# Evento: obtener un número par
evento_par <- espacio_muestral[espacio_muestral %% 2 == 0]
cat("Evento (par):", evento_par, "\n")
cat("P(par) =", length(evento_par) / length(espacio_muestral), "\n")

# Simulación: verificar la probabilidad empírica
set.seed(42)
n_lanzamientos <- 100000
lanzamientos <- sample(1:6, n_lanzamientos, replace = TRUE)
prob_empirica <- mean(lanzamientos %% 2 == 0)
cat("P(par) empírica (", n_lanzamientos, "lanzamientos):", prob_empirica, "\n")

# ==============================================================================
# 2. REGLAS DE PROBABILIDAD
# ==============================================================================

# Ejemplo: encuesta de preferencias (datos simulados realistas)
set.seed(123)
n <- 1000

# Simular: género y preferencia política
genero <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.48, 0.52))
pref <- character(n)
for (i in 1:n) {
  if (genero[i] == "M") {
    pref[i] <- sample(c("A", "B", "C"), 1, prob = c(0.4, 0.35, 0.25))
  } else {
    pref[i] <- sample(c("A", "B", "C"), 1, prob = c(0.3, 0.4, 0.3))
  }
}
encuesta <- data.frame(genero, pref)

# Tabla de contingencia
tabla <- table(encuesta$genero, encuesta$pref)
cat("\n--- Tabla de contingencia ---\n")
print(tabla)
cat("\nProporciones:\n")
print(round(prop.table(tabla), 3))

# P(A) — probabilidad marginal
P_A <- mean(encuesta$pref == "A")
cat("\nP(preferencia A):", round(P_A, 3), "\n")

# P(A | M) — probabilidad condicional
P_A_dado_M <- mean(encuesta$pref[encuesta$genero == "M"] == "A")
cat("P(A | Masculino):", round(P_A_dado_M, 3), "\n")

# P(A | F) — probabilidad condicional
P_A_dado_F <- mean(encuesta$pref[encuesta$genero == "F"] == "A")
cat("P(A | Femenino):", round(P_A_dado_F, 3), "\n")

# ¿Son independientes género y preferencia?
cat("\nSi independientes: P(A|M) ≈ P(A)\n")
cat("P(A) =", round(P_A, 3), " vs P(A|M) =", round(P_A_dado_M, 3), "\n")
cat("Test chi-cuadrado:\n")
print(chisq.test(tabla))

# ==============================================================================
# 3. REGLA DE LA SUMA (Inclusión-Exclusión)
# ==============================================================================

# P(A ∪ B) = P(A) + P(B) - P(A ∩ B)
# Ejemplo: cartas
cat("\n--- Regla de la Suma: Baraja de cartas ---\n")
P_roja <- 26/52     # P(carta roja)
P_figura <- 12/52   # P(figura: J, Q, K)
P_roja_y_fig <- 6/52 # P(roja Y figura)
P_roja_o_fig <- P_roja + P_figura - P_roja_y_fig
cat("P(roja) =", round(P_roja, 3), "\n")
cat("P(figura) =", round(P_figura, 3), "\n")
cat("P(roja ∩ figura) =", round(P_roja_y_fig, 3), "\n")
cat("P(roja ∪ figura) =", round(P_roja_o_fig, 3), "\n")

# Verificación por simulación
set.seed(456)
baraja <- expand.grid(
  numero = c(2:10, "J", "Q", "K", "A"),
  palo = c("Corazones", "Diamantes", "Picas", "Treboles")
)
baraja$roja <- baraja$palo %in% c("Corazones", "Diamantes")
baraja$figura <- baraja$numero %in% c("J", "Q", "K")

P_sim <- mean(baraja$roja | baraja$figura)
cat("P(roja ∪ figura) simulada:", round(P_sim, 3), "\n")

# ==============================================================================
# 4. TEOREMA DE BAYES
# ==============================================================================

cat("\n--- Teorema de Bayes: Prueba diagnóstica ---\n")

# Parámetros
prevalencia <- 0.01     # P(Enfermo) = 1%
sensibilidad <- 0.95    # P(+|Enfermo) = 95%
falso_positivo <- 0.10  # P(+|Sano) = 10%

# Aplicar Bayes: P(Enfermo|+)
P_positivo <- sensibilidad * prevalencia +
              falso_positivo * (1 - prevalencia)
P_enfermo_dado_pos <- (sensibilidad * prevalencia) / P_positivo

cat("Prevalencia:       ", prevalencia, "\n")
cat("Sensibilidad:      ", sensibilidad, "\n")
cat("Tasa falso +:      ", falso_positivo, "\n")
cat("P(+):              ", round(P_positivo, 4), "\n")
cat("P(Enfermo | +):    ", round(P_enfermo_dado_pos, 4), "\n")
cat("\n→ A pesar del test positivo, solo hay",
    round(P_enfermo_dado_pos * 100, 1),
    "% de probabilidad de estar enfermo\n")
cat("  Esto se debe a la BAJA prevalencia (1%)\n")

# Simulación del Teorema de Bayes
set.seed(123)
n_sim <- 100000

enfermo <- rbinom(n_sim, 1, p = prevalencia)
resultado_test <- numeric(n_sim)

for (i in 1:n_sim) {
  if (enfermo[i] == 1) {
    resultado_test[i] <- rbinom(1, 1, p = sensibilidad)
  } else {
    resultado_test[i] <- rbinom(1, 1, p = falso_positivo)
  }
}

# P(Enfermo | Positivo) empírica
positivos <- resultado_test == 1
P_bayes_sim <- mean(enfermo[positivos])
cat("\nP(Enfermo | +) simulada:", round(P_bayes_sim, 4), "\n")
cat("P(Enfermo | +) teórica:", round(P_enfermo_dado_pos, 4), "\n")

# Visualizar resultados
resultados <- data.frame(
  estado = factor(c("Verdadero +", "Falso +", "Verdadero -", "Falso -")),
  conteo = c(
    sum(enfermo == 1 & resultado_test == 1),
    sum(enfermo == 0 & resultado_test == 1),
    sum(enfermo == 0 & resultado_test == 0),
    sum(enfermo == 1 & resultado_test == 0)
  )
)
resultados$proporcion <- resultados$conteo / n_sim

p_bayes <- ggplot(resultados, aes(x = estado, y = conteo, fill = estado)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(round(proporcion * 100, 2), "%")),
            vjust = -0.5) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#2ECC71", "#3498DB")) +
  labs(title = "Simulación de Prueba Diagnóstica (n = 100,000)",
       subtitle = paste("P(Enfermo|+) =",
                        round(P_bayes_sim * 100, 1), "%"),
       x = "", y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_bayes)

# ==============================================================================
# 5. PROBABILIDAD CONDICIONAL: VISUALIZACIÓN
# ==============================================================================

# Efecto de la prevalencia en P(Enfermo|+)
prevalencias <- seq(0.001, 0.5, by = 0.001)
P_enfermo_pos <- (sensibilidad * prevalencias) /
  (sensibilidad * prevalencias + falso_positivo * (1 - prevalencias))

df_prev <- data.frame(prevalencia = prevalencias, P_enfermo_pos)

p_prev <- ggplot(df_prev, aes(x = prevalencia, y = P_enfermo_pos)) +
  geom_line(color = "#1F4E79", size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.01, linetype = "dotted", color = "gray50") +
  annotate("text", x = 0.05, y = 0.15,
           label = "Prevalencia = 1%", color = "gray40") +
  labs(title = "P(Enfermo | Test +) según Prevalencia",
       subtitle = "Sensibilidad = 95%, Tasa falso positivo = 10%",
       x = "Prevalencia", y = "P(Enfermo | +)") +
  theme_minimal()
print(p_prev)

# ==============================================================================
# 6. VARIABLES ALEATORIAS DISCRETAS
# ==============================================================================

cat("\n--- Variables aleatorias discretas ---\n")

# PMF de un dado justo
x <- 1:6
pmf <- rep(1/6, 6)
cat("E(X) dado justo:", sum(x * pmf), "\n")
cat("Var(X) dado justo:", sum((x - sum(x * pmf))^2 * pmf), "\n")

# PMF de un dado cargado
pmf_cargado <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
cat("\nE(X) dado cargado:", sum(x * pmf_cargado), "\n")

# Visualizar ambas PMF
df_dado <- data.frame(
  x = rep(x, 2),
  prob = c(pmf, pmf_cargado),
  tipo = rep(c("Justo", "Cargado"), each = 6)
)

p_pmf <- ggplot(df_dado, aes(x = factor(x), y = prob, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("#E74C3C", "#2E86C1")) +
  labs(title = "PMF: Dado Justo vs Dado Cargado",
       x = "Resultado", y = "Probabilidad", fill = "Tipo") +
  theme_minimal()
print(p_pmf)

# ==============================================================================
# 7. VARIABLES ALEATORIAS CONTINUAS
# ==============================================================================

cat("\n--- Variables aleatorias continuas ---\n")

# Densidad normal estándar
x_norm <- seq(-4, 4, by = 0.01)
df_norm <- data.frame(x = x_norm, densidad = dnorm(x_norm))

p_norm <- ggplot(df_norm, aes(x = x, y = densidad)) +
  geom_line(color = "#1F4E79", size = 1) +
  geom_area(data = df_norm %>% filter(x >= -1.96 & x <= 1.96),
            aes(x = x, y = densidad), fill = "#2E86C1", alpha = 0.3) +
  annotate("text", x = 0, y = 0.15, label = "95%", size = 5, color = "#1F4E79") +
  labs(title = "Distribución Normal Estándar N(0,1)",
       subtitle = "Área sombreada = 95% central (±1.96σ)",
       x = "z", y = "Densidad") +
  theme_minimal()
print(p_norm)

# Probabilidades con la normal
cat("P(Z < 1.96):", round(pnorm(1.96), 4), "\n")
cat("P(-1.96 < Z < 1.96):", round(pnorm(1.96) - pnorm(-1.96), 4), "\n")
cat("P(Z > 2):", round(1 - pnorm(2), 4), "\n")
cat("Valor z para P = 0.975:", round(qnorm(0.975), 4), "\n")

# ==============================================================================
# 8. CONVERGENCIA DE FRECUENCIA RELATIVA
# ==============================================================================

# Ley de los grandes números
set.seed(789)
n_max <- 10000
moneda <- sample(c(0, 1), n_max, replace = TRUE)
freq_acumulada <- cumsum(moneda) / (1:n_max)

df_conv <- data.frame(
  n = 1:n_max,
  frecuencia = freq_acumulada
)

p_conv <- ggplot(df_conv, aes(x = n, y = frecuencia)) +
  geom_line(color = "#2E86C1", alpha = 0.7) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Convergencia de Frecuencia Relativa (Moneda Justa)",
       subtitle = "La frecuencia de 'cara' converge a 0.5",
       x = "Número de lanzamientos (escala log)", y = "Frecuencia relativa") +
  theme_minimal()
print(p_conv)

cat("\n✓ Fin de Sesión 2\n")
