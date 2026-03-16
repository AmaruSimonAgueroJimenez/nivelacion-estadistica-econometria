# ==============================================================================
# Sesión 1: Fundamentos y Estadística Descriptiva
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================

# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2", "dplyr", "gapminder", "moments"))
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gapminder)  # Dataset de países (esperanza de vida, PIB, población)
library(moments)    # Sesgo y curtosis

# ==============================================================================
# 1. TIPOS DE DATOS Y ESCALAS DE MEDICIÓN
# ==============================================================================

# Usamos el dataset gapminder: datos de 142 países, 1952-2007
data("gapminder")
head(gapminder)
str(gapminder)

# Identificar tipos de variables
cat("\n--- Tipos de variables en gapminder ---\n")
cat("country:   Cualitativa nominal\n")
cat("continent: Cualitativa nominal\n")
cat("year:      Cuantitativa discreta (intervalo)\n")
cat("lifeExp:   Cuantitativa continua (razón)\n")
cat("pop:       Cuantitativa discreta (razón)\n")
cat("gdpPercap: Cuantitativa continua (razón)\n")

# Filtrar datos más recientes
datos <- gapminder %>% filter(year == 2007)
cat("\nDimensiones (2007):", dim(datos), "\n")

# ==============================================================================
# 2. MEDIDAS DE TENDENCIA CENTRAL
# ==============================================================================

cat("\n--- Medidas de tendencia central: Esperanza de vida (2007) ---\n")
cat("Media:   ", mean(datos$lifeExp), "\n")
cat("Mediana: ", median(datos$lifeExp), "\n")

# Moda (no hay función base en R)
moda <- function(x) {
  tab <- table(round(x, 0))
  as.numeric(names(tab)[which.max(tab)])
}
cat("Moda (redondeada): ", moda(datos$lifeExp), "\n")

# Comparar media vs mediana — ¿hay sesgo?
cat("\nDiferencia media - mediana:",
    round(mean(datos$lifeExp) - median(datos$lifeExp), 2), "\n")
cat("Si media > mediana → sesgo positivo (cola derecha)\n")
cat("Si media < mediana → sesgo negativo (cola izquierda)\n")

# Media recortada (trimmed mean) — robusta a outliers
cat("\nMedia recortada (10%):", mean(datos$lifeExp, trim = 0.10), "\n")

# ==============================================================================
# 3. MEDIDAS DE DISPERSIÓN
# ==============================================================================

cat("\n--- Medidas de dispersión: PIB per cápita (2007) ---\n")
cat("Varianza:           ", var(datos$gdpPercap), "\n")
cat("Desv. estándar:     ", sd(datos$gdpPercap), "\n")
cat("Rango:              ", diff(range(datos$gdpPercap)), "\n")
cat("Rango intercuartil: ", IQR(datos$gdpPercap), "\n")
cat("Mínimo:             ", min(datos$gdpPercap), "\n")
cat("Máximo:             ", max(datos$gdpPercap), "\n")

# Cuartiles
cat("\nCuartiles:\n")
print(quantile(datos$gdpPercap))

# Coeficiente de variación
cv_gdp <- sd(datos$gdpPercap) / mean(datos$gdpPercap) * 100
cv_life <- sd(datos$lifeExp) / mean(datos$lifeExp) * 100
cat("\nCoeficiente de Variación:\n")
cat("  PIB per cápita: ", round(cv_gdp, 1), "%\n")
cat("  Esperanza vida:  ", round(cv_life, 1), "%\n")
cat("  → PIB tiene MUCHA más variabilidad relativa\n")

# ==============================================================================
# 4. Z-SCORES (ESTANDARIZACIÓN)
# ==============================================================================

datos <- datos %>%
  mutate(
    z_lifeExp = (lifeExp - mean(lifeExp)) / sd(lifeExp),
    z_gdpPercap = (gdpPercap - mean(gdpPercap)) / sd(gdpPercap)
  )

cat("\n--- Países con esperanza de vida extrema (|z| > 2) ---\n")
extremos <- datos %>%
  filter(abs(z_lifeExp) > 2) %>%
  select(country, lifeExp, z_lifeExp) %>%
  arrange(z_lifeExp)
print(extremos)

# ==============================================================================
# 5. DETECCIÓN DE OUTLIERS (Método IQR)
# ==============================================================================

Q1 <- quantile(datos$gdpPercap, 0.25)
Q3 <- quantile(datos$gdpPercap, 0.75)
IQR_val <- IQR(datos$gdpPercap)
limite_inf <- Q1 - 1.5 * IQR_val
limite_sup <- Q3 + 1.5 * IQR_val

cat("\n--- Detección de outliers en PIB per cápita ---\n")
cat("Q1:", Q1, " Q3:", Q3, " IQR:", IQR_val, "\n")
cat("Límite inferior:", limite_inf, "\n")
cat("Límite superior:", limite_sup, "\n")

outliers <- datos %>%
  filter(gdpPercap < limite_inf | gdpPercap > limite_sup) %>%
  select(country, continent, gdpPercap)
cat("\nOutliers encontrados:", nrow(outliers), "\n")
print(outliers)

# ==============================================================================
# 6. VISUALIZACIÓN CON ggplot2
# ==============================================================================

# 6a. Histograma de esperanza de vida
p1 <- ggplot(datos, aes(x = lifeExp)) +
  geom_histogram(bins = 20, fill = "#2E86C1", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(lifeExp)), color = "red",
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(lifeExp)), color = "darkgreen",
             linetype = "dashed", size = 1) +
  labs(title = "Distribución de Esperanza de Vida (2007)",
       subtitle = "Rojo = media, Verde = mediana",
       x = "Esperanza de vida (años)", y = "Frecuencia") +
  theme_minimal()
print(p1)

# 6b. Boxplot comparativo por continente
p2 <- ggplot(datos, aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_y_log10(labels = scales::comma) +  # Escala log por la dispersión
  labs(title = "PIB per Cápita por Continente (2007)",
       x = "Continente", y = "PIB per cápita (USD, escala log)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

# 6c. Scatter: relación PIB vs esperanza de vida
p3 <- ggplot(datos, aes(x = gdpPercap, y = lifeExp,
                         size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_size_continuous(range = c(1, 12), guide = "none") +
  labs(title = "Esperanza de Vida vs PIB per Cápita (2007)",
       x = "PIB per cápita (USD, escala log)",
       y = "Esperanza de vida (años)",
       color = "Continente") +
  theme_minimal()
print(p3)

# 6d. Violin + boxplot por continente
p4 <- ggplot(datos, aes(x = continent, y = lifeExp, fill = continent)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  labs(title = "Distribución de Esperanza de Vida por Continente",
       x = "Continente", y = "Esperanza de vida (años)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)

# ==============================================================================
# 7. TABLA RESUMEN POR CONTINENTE
# ==============================================================================

resumen <- datos %>%
  group_by(continent) %>%
  summarise(
    n = n(),
    media = round(mean(lifeExp), 1),
    mediana = round(median(lifeExp), 1),
    desv_est = round(sd(lifeExp), 1),
    min = round(min(lifeExp), 1),
    max = round(max(lifeExp), 1),
    cv = round(sd(lifeExp) / mean(lifeExp) * 100, 1),
    .groups = "drop"
  )

cat("\n--- Resumen de Esperanza de Vida por Continente (2007) ---\n")
print(as.data.frame(resumen))

# ==============================================================================
# 8. SESGO Y CURTOSIS
# ==============================================================================

cat("\n--- Forma de la distribución: PIB per cápita ---\n")
cat("Sesgo (skewness):  ", round(skewness(datos$gdpPercap), 2), "\n")
cat("Curtosis (kurtosis):", round(kurtosis(datos$gdpPercap), 2), "\n")
cat("  Sesgo > 0 → cola derecha más larga\n")
cat("  Curtosis > 3 → colas más pesadas que normal\n")

# ==============================================================================
# 9. CASO PRÁCTICO: BRECHA POR CONTINENTE
# ==============================================================================

# Análisis de brecha de esperanza de vida
brecha <- datos %>%
  group_by(continent) %>%
  summarise(
    media_vida = mean(lifeExp),
    media_pib = mean(gdpPercap),
    .groups = "drop"
  ) %>%
  mutate(
    brecha_vida = media_vida - max(media_vida),
    brecha_pib = media_pib - max(media_pib)
  )

cat("\n--- Brecha respecto al continente con mayor esperanza de vida ---\n")
print(as.data.frame(brecha))

# Correlación
cat("\nCorrelación PIB-Esperanza de vida:",
    round(cor(datos$gdpPercap, datos$lifeExp), 3), "\n")
cat("Correlación log(PIB)-Esperanza de vida:",
    round(cor(log(datos$gdpPercap), datos$lifeExp), 3), "\n")

cat("\n✓ Fin de Sesión 1\n")
