# ==============================================================================
# Sesión 1: Pruebas de Hipótesis
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================

# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2", "dplyr", "gapminder"))
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gapminder)

# ==============================================================================
# 1. LÓGICA Y FUNDAMENTOS DE LAS PRUEBAS DE HIPÓTESIS
# ==============================================================================

# Cargar datos y filtrar año 2007
data("gapminder")
datos <- gapminder %>% filter(year == 2007)

cat("Dataset gapminder - Sesión 2007:\n")
cat("Número de países:", nrow(datos), "\n")
cat("Variables disponibles:\n")
print(colnames(datos))
cat("\nPrimeras observaciones:\n")
print(head(datos))

# ==============================================================================
# 2. HIPÓTESIS NULA (H₀) E HIPÓTESIS ALTERNATIVA (H₁)
# ==============================================================================

# Ejemplo 1: Prueba bilateral sobre la esperanza de vida global
cat("\n=== Ejemplo 1: Esperanza de vida global ===\n")
cat("H₀: μ = 70 años (la esperanza de vida promedio global es 70)\n")
cat("H₁: μ ≠ 70 años (la esperanza de vida promedio difiere de 70) [bilateral]\n\n")

media_global <- mean(datos$lifeExp)
cat("Media observada en la muestra:", round(media_global, 2), "años\n")
cat("Diferencia respecto a H₀:", round(media_global - 70, 2), "años\n\n")

# Ejemplo 2: Prueba unilateral sobre ingresos
cat("=== Ejemplo 2: Ingresos por región ===\n")
cat("Comparar esperanza de vida entre Asia y Europa\n")
cat("H₀: μ_Asia = μ_Europa (esperanza de vida igual entre regiones)\n")
cat("H₁: μ_Asia < μ_Europa (esperanza menor en Asia) [unilateral]\n\n")

life_asia <- datos %>% filter(continent == "Asia") %>% pull(lifeExp) %>% mean()
life_europe <- datos %>% filter(continent == "Europe") %>% pull(lifeExp) %>% mean()
cat("Asia media:", round(life_asia, 2), "años\n")
cat("Europa media:", round(life_europe, 2), "años\n")
cat("Diferencia:", round(life_asia - life_europe, 2), "años\n")

# ==============================================================================
# 3. VALOR-P, ERRORES TIPO I Y II, POTENCIA
# ==============================================================================

# Simulación: distribución t bajo H₀
set.seed(123)
n <- 50
mu_0 <- 70  # Valor bajo H₀

# Generar muchas muestras bajo H₀ (simulación)
simul_t <- replicate(10000, {
  muestra <- rnorm(n, mean = mu_0, sd = sd(datos$lifeExp))
  (mean(muestra) - mu_0) / (sd(muestra) / sqrt(n))
})

# Graficar distribución de t bajo H₀
df_simul <- data.frame(t_stat = simul_t)

p_h0 <- ggplot(df_simul, aes(x = t_stat)) +
  geom_histogram(bins = 50, fill = "#2E86C1", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = 1.96), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = -1.96), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 3.5, y = 700, label = "Zona de rechazo (α/2 = 0.025)",
           size = 3, color = "red") +
  annotate("text", x = -3.5, y = 700, label = "Zona de rechazo (α/2 = 0.025)",
           size = 3, color = "red") +
  labs(title = "Distribución del estadístico t bajo H₀ (bilateral, α = 0.05)",
       x = "Estadístico t", y = "Frecuencia") +
  theme_minimal()

print(p_h0)

cat("\nCrítica de rechazo: |t| > 1.96 (para α = 0.05, bilateral)\n")
cat("Proporción de valores |t| > 1.96 en la simulación:",
    round(mean(abs(simul_t) > 1.96), 4), "\n")

# ==============================================================================
# 4. PRUEBA t DE UNA MEDIA
# ==============================================================================

cat("\n=== Prueba t de una muestra: Esperanza de vida ===\n")
cat("H₀: μ = 70 años\n")
cat("H₁: μ ≠ 70 años\n")
cat("α = 0.05\n\n")

# Realizar prueba t
resultado_t1 <- t.test(datos$lifeExp, mu = 70, alternative = "two.sided")
print(resultado_t1)

# Extraer elementos clave
cat("\n--- Interpretación ---\n")
cat("Estadístico t:", round(resultado_t1$statistic, 4), "\n")
cat("Grados de libertad:", resultado_t1$parameter, "\n")
cat("Valor-p:", round(resultado_t1$p.value, 4), "\n")
cat("Media muestral:", round(resultado_t1$estimate, 2), "años\n")
cat("IC 95%:", round(resultado_t1$conf.int[1], 2), "a", round(resultado_t1$conf.int[2], 2), "\n\n")

if (resultado_t1$p.value < 0.05) {
  cat("✓ Decisión: RECHAZAMOS H₀ (p < 0.05)\n")
  cat("  Conclusión: Hay evidencia significativa de que la esperanza de vida\n")
  cat("  promedio difiere de 70 años.\n")
} else {
  cat("✗ Decisión: NO RECHAZAMOS H₀ (p ≥ 0.05)\n")
  cat("  Conclusión: No hay evidencia suficiente para afirmar que la\n")
  cat("  esperanza de vida promedio difiere de 70 años.\n")
}

# ==============================================================================
# 5. PRUEBA t DE DIFERENCIA DE MEDIAS
# ==============================================================================

cat("\n=== Prueba t de dos muestras: Asia vs Europa ===\n")
cat("H₀: μ_Asia = μ_Europa (esperanza de vida igual)\n")
cat("H₁: μ_Asia ≠ μ_Europa (esperanza de vida difiere) [bilateral]\n")
cat("α = 0.05\n\n")

# Preparar datos
datos_asia <- datos %>% filter(continent == "Asia")
datos_europa <- datos %>% filter(continent == "Europe")

# Estadísticas descriptivas
cat("--- Estadísticas descriptivas ---\n")
cat("Asia: n =", nrow(datos_asia),
    ", media =", round(mean(datos_asia$lifeExp), 2),
    ", DE =", round(sd(datos_asia$lifeExp), 2), "\n")
cat("Europa: n =", nrow(datos_europa),
    ", media =", round(mean(datos_europa$lifeExp), 2),
    ", DE =", round(sd(datos_europa$lifeExp), 2), "\n\n")

# Prueba t con varianzas iguales
resultado_t2 <- t.test(datos_asia$lifeExp, datos_europa$lifeExp,
                        var.equal = TRUE, alternative = "two.sided")
print(resultado_t2)

# Interpretación
cat("\n--- Interpretación ---\n")
cat("Diferencia de medias:", round(mean(datos_asia$lifeExp) - mean(datos_europa$lifeExp), 2), "años\n")
cat("Valor-p:", round(resultado_t2$p.value, 4), "\n\n")

if (resultado_t2$p.value < 0.05) {
  cat("✓ Decisión: RECHAZAMOS H₀ (p < 0.05)\n")
  cat("  Conclusión: Hay evidencia significativa de que la esperanza de vida\n")
  cat("  promedio difiere entre Asia y Europa.\n")
} else {
  cat("✗ Decisión: NO RECHAZAMOS H₀ (p ≥ 0.05)\n")
  cat("  Conclusión: No hay evidencia suficiente para afirmar diferencia\n")
  cat("  en la esperanza de vida entre regiones.\n")
}

# Visualización
p_t2 <- ggplot(datos %>% filter(continent %in% c("Asia", "Europe")),
               aes(x = continent, y = lifeExp, fill = continent)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  labs(title = "Esperanza de Vida: Asia vs Europa (2007)",
       x = "Continente", y = "Esperanza de vida (años)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_t2)

# ==============================================================================
# 6. PRUEBAS DE PROPORCIONES
# ==============================================================================

# Definir "alta esperanza de vida" como > 72 años (media global)
datos$alta_esperanza <- ifelse(datos$lifeExp > 72, 1, 0)

cat("\n=== Prueba de proporciones: Alta esperanza de vida (>72 años) ===\n")
cat("H₀: p = 0.5 (50% de países tienen alta esperanza)\n")
cat("H₁: p ≠ 0.5 (la proporción difiere de 50%)\n")
cat("α = 0.05\n\n")

n_total <- nrow(datos)
n_alta <- sum(datos$alta_esperanza)
prop_observada <- n_alta / n_total

cat("Total de países:", n_total, "\n")
cat("Países con alta esperanza (>72):", n_alta, "\n")
cat("Proporción observada:", round(prop_observada, 3), "\n\n")

# Prueba de proporción
resultado_prop <- prop.test(n_alta, n_total, p = 0.5, alternative = "two.sided")
print(resultado_prop)

cat("\n--- Interpretación ---\n")
cat("Valor-p:", round(resultado_prop$p.value, 4), "\n")

if (resultado_prop$p.value < 0.05) {
  cat("✓ RECHAZAMOS H₀: la proporción difiere significativamente de 0.5\n")
} else {
  cat("✗ NO RECHAZAMOS H₀: la proporción no difiere significativamente de 0.5\n")
}

# ==============================================================================
# 7. PRUEBA CHI-CUADRADO DE INDEPENDENCIA
# ==============================================================================

cat("\n=== Prueba chi-cuadrado: Continente vs Alta Esperanza ===\n")
cat("H₀: Continente y esperanza de vida son independientes\n")
cat("H₁: Existe asociación entre ambas variables\n")
cat("α = 0.05\n\n")

# Crear tabla de contingencia
tabla <- table(datos$continent, datos$alta_esperanza)
colnames(tabla) <- c("Baja esperanza", "Alta esperanza")

cat("--- Tabla de contingencia ---\n")
print(tabla)
cat("\n")

# Prueba chi-cuadrado
resultado_chi2 <- chisq.test(tabla)
print(resultado_chi2)

cat("\n--- Interpretación ---\n")
cat("Valor-p:", round(resultado_chi2$p.value, 4), "\n")

if (resultado_chi2$p.value < 0.05) {
  cat("✓ RECHAZAMOS H₀: Existe asociación significativa\n")
  cat("  entre continente y nivel de esperanza de vida.\n")
} else {
  cat("✗ NO RECHAZAMOS H₀: No hay asociación significativa.\n")
}

# Visualización de proporciones
datos_cont <- datos %>%
  group_by(continent) %>%
  summarise(
    total = n(),
    alta = sum(alta_esperanza),
    prop_alta = alta / total,
    .groups = "drop"
  )

p_chi2 <- ggplot(datos_cont, aes(x = continent, y = prop_alta, fill = continent)) +
  geom_col(alpha = 0.7) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Proporción de Países con Alta Esperanza (>72 años) por Continente",
       x = "Continente", y = "Proporción") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_chi2)

# ==============================================================================
# 8. SIMULACIÓN: DISTRIBUCIÓN DEL ESTADÍSTICO t BAJO H₀ Y H₁
# ==============================================================================

cat("\n=== Simulación: Error Tipo I, Tipo II y Potencia ===\n\n")

set.seed(42)
alpha <- 0.05
n_sim <- 5000
n_muestra <- 50
mu_0 <- 70  # H₀
mu_1 <- 73  # H₁ (efecto real)

# Bajo H₀: generar muestras donde μ = 70
t_h0 <- replicate(n_sim, {
  muestra <- rnorm(n_muestra, mean = mu_0, sd = 8)
  (mean(muestra) - mu_0) / (sd(muestra) / sqrt(n_muestra))
})

# Bajo H₁: generar muestras donde μ = 73
t_h1 <- replicate(n_sim, {
  muestra <- rnorm(n_muestra, mean = mu_1, sd = 8)
  (mean(muestra) - mu_0) / (sd(muestra) / sqrt(n_muestra))
})

# Valores críticos para α = 0.05 bilateral
t_crit <- qt(1 - alpha/2, df = n_muestra - 1)

# Calcular errores
tipo1 <- mean(abs(t_h0) > t_crit)  # Rechazar H₀ cuando es verdadera
tipo2 <- mean(abs(t_h1) <= t_crit) # No rechazar H₀ cuando es falsa
potencia <- 1 - tipo2

cat("Parámetros de simulación:\n")
cat("  n por muestra:", n_muestra, "\n")
cat("  Número de replicaciones:", n_sim, "\n")
cat("  H₀: μ = ", mu_0, "\n")
cat("  H₁: μ = ", mu_1, "(diferencia real = ", mu_1 - mu_0, " unidades)\n\n")

cat("Resultados:\n")
cat("  Error Tipo I (α):", round(tipo1, 4), "(esperado ≈ 0.05)\n")
cat("  Error Tipo II (β):", round(tipo2, 4), "\n")
cat("  Potencia (1-β):", round(potencia, 4), "\n\n")

# Graficar ambas distribuciones
df_sim <- rbind(
  data.frame(t_stat = t_h0, hipotesis = "Bajo H₀ (μ=70)"),
  data.frame(t_stat = t_h1, hipotesis = "Bajo H₁ (μ=73)")
)

p_sim <- ggplot(df_sim, aes(x = t_stat, fill = hipotesis)) +
  geom_histogram(bins = 40, alpha = 0.6, color = "white", position = "identity") +
  geom_vline(aes(xintercept = t_crit), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = -t_crit), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 3.8, y = 600, label = "Zona rechazo H₀",
           size = 3, color = "red", fontface = "bold") +
  labs(title = "Errores Tipo I y II: Distribución de t bajo H₀ y H₁",
       subtitle = paste("Potencia =", round(potencia, 3)),
       x = "Estadístico t", y = "Frecuencia", fill = "Supuesto") +
  theme_minimal()
print(p_sim)

# ==============================================================================
# 9. CASO PRÁCTICO: COMPARACIÓN SOCIOECONÓMICA
# ==============================================================================

cat("\n=== CASO PRÁCTICO: Indicadores de Desarrollo por Región ===\n\n")

# Crear variables binarias útiles
datos_practico <- datos %>%
  mutate(
    alta_esperanza = ifelse(lifeExp > median(lifeExp), 1, 0),
    alto_pib = ifelse(gdpPercap > median(gdpPercap), 1, 0),
    es_desarrollado = ifelse(continent %in% c("Europe", "Oceania"), 1, 0)
  )

# 1. ¿Difiere PIB entre desarrollados y en desarrollo?
cat("1. Prueba t: PIB per cápita (Desarrollados vs Otros)\n")
cat("   H₀: μ_desarrollados = μ_otros\n")
resultado_pib <- t.test(
  datos_practico %>% filter(es_desarrollado == 1) %>% pull(gdpPercap),
  datos_practico %>% filter(es_desarrollado == 0) %>% pull(gdpPercap),
  var.equal = TRUE
)
cat("   p-valor:", round(resultado_pib$p.value, 4), "\n")
cat("   Conclusión:", ifelse(resultado_pib$p.value < 0.05,
                              "SÍ hay diferencia significativa ✓",
                              "NO hay diferencia significativa ✗"), "\n\n")

# 2. ¿Existe asociación entre desarrollo y alta esperanza?
cat("2. Prueba chi-cuadrado: Desarrollo vs Alta Esperanza de Vida\n")
cat("   H₀: Las variables son independientes\n")
tabla_asoc <- table(datos_practico$es_desarrollado, datos_practico$alta_esperanza)
resultado_asoc <- chisq.test(tabla_asoc)
cat("   p-valor:", round(resultado_asoc$p.value, 4), "\n")
cat("   Conclusión:", ifelse(resultado_asoc$p.value < 0.05,
                              "SÍ hay asociación significativa ✓",
                              "NO hay asociación significativa ✗"), "\n\n")

# 3. ¿La esperanza de vida promedio en Africa es < media global?
cat("3. Prueba t unilateral: Africa vs Media Global\n")
cat("   H₀: μ_Africa = ", round(mean(datos$lifeExp), 1), "\n")
cat("   H₁: μ_Africa < ", round(mean(datos$lifeExp), 1), " [unilateral]\n")
africa_lifeexp <- datos %>% filter(continent == "Africa") %>% pull(lifeExp)
resultado_africa <- t.test(africa_lifeexp, mu = mean(datos$lifeExp), alternative = "less")
cat("   Media Africa:", round(mean(africa_lifeexp), 2), "\n")
cat("   p-valor:", round(resultado_africa$p.value, 4), "\n")
cat("   Conclusión:", ifelse(resultado_africa$p.value < 0.05,
                              "SÍ es significativamente menor ✓",
                              "NO hay diferencia significativa ✗"), "\n\n")

# 4. Tabla resumen
cat("4. Resumen estadístico por continente\n")
resumen <- datos_practico %>%
  group_by(continent) %>%
  summarise(
    n = n(),
    media_esperanza = round(mean(lifeExp), 1),
    media_pib = round(mean(gdpPercap), 0),
    prop_alta_esperanza = round(mean(alta_esperanza), 2),
    .groups = "drop"
  )
print(resumen)

# ==============================================================================
# 10. VISUALIZACIONES FINALES
# ==============================================================================

# Comparación de esperanza de vida por continente con estadísticas
p_final <- ggplot(datos, aes(x = reorder(continent, lifeExp, median),
                              y = lifeExp, fill = continent)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  geom_point(alpha = 0.3, size = 1.5, position = position_jitter(width = 0.1)) +
  labs(title = "Distribución de Esperanza de Vida por Continente (2007)",
       x = "Continente", y = "Esperanza de vida (años)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_final)

cat("\n✓ Fin de Sesión 1\n")
