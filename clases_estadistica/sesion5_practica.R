# ==============================================================================
# Sesión 5: Práctica Integradora en R
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Profesor: Amaru Agüero (a.agueroj@udd.cl)
# ==============================================================================
#
# Este script integra TODAS las sesiones anteriores en un análisis completo.
# Usamos el dataset "iris" (base R) y "mtcars" (base R) para que no
# necesites instalar paquetes adicionales más allá del tidyverse.
#
# --- Paquetes necesarios ---
# install.packages(c("tidyverse", "ggplot2", "broom"))
library(tidyverse)
library(ggplot2)
library(broom)  # Para tidy() de modelos

# ==============================================================================
# PARTE 1: CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 1: Exploración de Datos\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Usamos mtcars: rendimiento de 32 automóviles
data(mtcars)

# Agregar nombres de autos como columna y crear factores
datos <- mtcars %>%
  rownames_to_column("modelo") %>%
  mutate(
    cilindros = factor(cyl, labels = c("4 cil", "6 cil", "8 cil")),
    transmision = factor(am, labels = c("Automático", "Manual")),
    forma_motor = factor(vs, labels = c("V", "Recto"))
  )

# Exploración inicial
cat("--- Estructura del dataset ---\n")
cat("Dimensiones:", dim(datos), "\n")
cat("Variables:", ncol(datos), "\n\n")

cat("Primeras filas:\n")
print(head(datos[, c("modelo", "mpg", "hp", "wt", "cilindros", "transmision")]))

cat("\nResumen estadístico de variables clave:\n")
datos %>%
  select(mpg, hp, wt, qsec, disp) %>%
  summary() %>%
  print()

# Valores faltantes
cat("\nValores faltantes por variable:\n")
print(colSums(is.na(datos)))
cat("→ No hay valores faltantes en mtcars\n")

# ==============================================================================
# PARTE 2: ESTADÍSTICA DESCRIPTIVA (Sesión 1)
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 2: Estadística Descriptiva\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Tabla resumen completa
resumen <- datos %>%
  group_by(cilindros) %>%
  summarise(
    n = n(),
    media_mpg = round(mean(mpg), 1),
    mediana_mpg = round(median(mpg), 1),
    sd_mpg = round(sd(mpg), 1),
    min_mpg = min(mpg),
    max_mpg = max(mpg),
    iqr_mpg = round(IQR(mpg), 1),
    cv_mpg = round(sd(mpg) / mean(mpg) * 100, 1),
    media_hp = round(mean(hp), 0),
    media_wt = round(mean(wt), 2),
    .groups = "drop"
  )

cat("--- Resumen por número de cilindros ---\n")
print(as.data.frame(resumen))

# Detección de outliers
cat("\n--- Detección de outliers en MPG ---\n")
Q1 <- quantile(datos$mpg, 0.25)
Q3 <- quantile(datos$mpg, 0.75)
IQR_val <- IQR(datos$mpg)
cat("Q1:", Q1, " Q3:", Q3, " IQR:", IQR_val, "\n")
cat("Límites: [", Q1 - 1.5 * IQR_val, ",", Q3 + 1.5 * IQR_val, "]\n")

outliers <- datos %>%
  filter(mpg < Q1 - 1.5 * IQR_val | mpg > Q3 + 1.5 * IQR_val) %>%
  select(modelo, mpg, cilindros)
cat("Outliers:", nrow(outliers), "\n")
if (nrow(outliers) > 0) print(outliers)

# Visualizaciones descriptivas
# 2a. Histograma con densidad
p1 <- ggplot(datos, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 12,
                 fill = "#2E86C1", alpha = 0.6, color = "white") +
  geom_density(color = "#E74C3C", size = 1) +
  geom_vline(aes(xintercept = mean(mpg)), color = "red",
             linetype = "dashed") +
  geom_vline(aes(xintercept = median(mpg)), color = "darkgreen",
             linetype = "dashed") +
  labs(title = "Distribución de Rendimiento (MPG)",
       subtitle = "Rojo = media, Verde = mediana",
       x = "Millas por galón", y = "Densidad") +
  theme_minimal()
print(p1)

# 2b. Boxplot por cilindros
p2 <- ggplot(datos, aes(x = cilindros, y = mpg, fill = cilindros)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c("#2E86C1", "#E74C3C", "#1F4E79")) +
  labs(title = "Rendimiento por Número de Cilindros",
       x = "Cilindros", y = "MPG") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)

# 2c. Scatter: peso vs rendimiento
p3 <- ggplot(datos, aes(x = wt, y = mpg, color = cilindros, shape = transmision)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("#2E86C1", "#E74C3C", "#1F4E79")) +
  labs(title = "Peso vs Rendimiento",
       x = "Peso (1000 lbs)", y = "MPG",
       color = "Cilindros", shape = "Transmisión") +
  theme_minimal()
print(p3)

# 2d. Matriz de correlaciones
vars_num <- datos %>% select(mpg, hp, wt, disp, qsec)
cor_matrix <- cor(vars_num)
cat("\n--- Matriz de correlaciones ---\n")
print(round(cor_matrix, 2))

# ==============================================================================
# PARTE 3: PROBABILIDAD Y DISTRIBUCIONES (Sesiones 2-3)
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 3: Probabilidades y Distribuciones\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# ¿Sigue MPG una distribución normal?
cat("--- Test de normalidad para MPG ---\n")
shapiro_test <- shapiro.test(datos$mpg)
cat("Shapiro-Wilk: W =", round(shapiro_test$statistic, 4),
    ", p-valor =", round(shapiro_test$p.value, 4), "\n")
cat("→", ifelse(shapiro_test$p.value > 0.05,
                "No rechazamos normalidad (p > 0.05)",
                "Rechazamos normalidad (p ≤ 0.05)"), "\n")

# QQ-Plot
p4 <- ggplot(datos, aes(sample = mpg)) +
  stat_qq(color = "#2E86C1", size = 2) +
  stat_qq_line(color = "#E74C3C", size = 1) +
  labs(title = "QQ-Plot de MPG",
       subtitle = paste("Shapiro-Wilk p =", round(shapiro_test$p.value, 3)),
       x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
  theme_minimal()
print(p4)

# Normalidad por grupo
cat("\nTest de normalidad por grupo de cilindros:\n")
datos %>%
  group_by(cilindros) %>%
  summarise(
    n = n(),
    W = round(shapiro.test(mpg)$statistic, 4),
    p_valor = round(shapiro.test(mpg)$p.value, 4),
    normal = ifelse(shapiro.test(mpg)$p.value > 0.05, "Sí", "No"),
    .groups = "drop"
  ) %>%
  print()

# Probabilidades con la distribución empírica
cat("\n--- Probabilidades empíricas ---\n")
cat("P(MPG > 25):", round(mean(datos$mpg > 25), 3), "\n")
cat("P(MPG < 15):", round(mean(datos$mpg < 15), 3), "\n")
cat("P(15 < MPG < 25):", round(mean(datos$mpg > 15 & datos$mpg < 25), 3), "\n")

# Si asumimos normalidad
mu_mpg <- mean(datos$mpg)
sd_mpg <- sd(datos$mpg)
cat("\nSi asumimos MPG ~ N(", round(mu_mpg, 1), ",", round(sd_mpg, 1), "):\n")
cat("P(MPG > 25):", round(1 - pnorm(25, mu_mpg, sd_mpg), 3), "\n")
cat("P(MPG < 15):", round(pnorm(15, mu_mpg, sd_mpg), 3), "\n")

# ==============================================================================
# PARTE 4: INFERENCIA ESTADÍSTICA (Sesión 4)
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 4: Inferencia Estadística\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# 4a. IC para la media de MPG
cat("--- IC para la media de MPG ---\n")
test_mpg <- t.test(datos$mpg, conf.level = 0.95)
cat("x̄ =", round(test_mpg$estimate, 2), "\n")
cat("IC 95%: [", round(test_mpg$conf.int[1], 2), ",",
    round(test_mpg$conf.int[2], 2), "]\n")

# 4b. Test de dos muestras: Manual vs Automático
cat("\n--- Comparación: Manual vs Automático ---\n")
test_trans <- t.test(mpg ~ transmision, data = datos)
cat("Media Automático:", round(test_trans$estimate[1], 2), "\n")
cat("Media Manual:", round(test_trans$estimate[2], 2), "\n")
cat("Diferencia:", round(diff(test_trans$estimate), 2), "\n")
cat("IC 95% para diferencia: [", round(test_trans$conf.int[1], 2), ",",
    round(test_trans$conf.int[2], 2), "]\n")
cat("p-valor:", format(test_trans$p.value, digits = 4), "\n")
cat("→", ifelse(test_trans$p.value < 0.05,
                "Diferencia significativa (p < 0.05)",
                "Diferencia no significativa (p ≥ 0.05)"), "\n")

p5 <- ggplot(datos, aes(x = transmision, y = mpg, fill = transmision)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  scale_fill_manual(values = c("#E74C3C", "#2E86C1")) +
  labs(title = "MPG: Automático vs Manual",
       subtitle = paste("p-valor =", format(test_trans$p.value, digits = 3)),
       x = "Transmisión", y = "Millas por galón") +
  theme_minimal() +
  theme(legend.position = "none")
print(p5)

# 4c. ANOVA: MPG según cilindros
cat("\n--- ANOVA: MPG según cilindros ---\n")
anova_cil <- aov(mpg ~ cilindros, data = datos)
cat("Tabla ANOVA:\n")
print(summary(anova_cil))

cat("\nComparaciones múltiples (Tukey HSD):\n")
tukey <- TukeyHSD(anova_cil)
print(tukey)

# ==============================================================================
# PARTE 5: REGRESIÓN LINEAL SIMPLE
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 5: Regresión Lineal Simple\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Modelo: MPG ~ Peso
modelo_simple <- lm(mpg ~ wt, data = datos)

cat("--- Modelo: mpg ~ wt ---\n")
print(summary(modelo_simple))

# IC para coeficientes
cat("\nIC 95% para coeficientes:\n")
print(confint(modelo_simple))

# Interpretación
cat("\nInterpretación:\n")
cat("  Intercepto:", round(coef(modelo_simple)[1], 2),
    "→ MPG estimado cuando peso = 0 (extrapolación)\n")
cat("  Pendiente (wt):", round(coef(modelo_simple)[2], 2),
    "→ Por cada 1000 lbs adicionales, MPG baja en",
    round(abs(coef(modelo_simple)[2]), 2), "\n")
cat("  R²:", round(summary(modelo_simple)$r.squared, 3),
    "→ El peso explica", round(summary(modelo_simple)$r.squared * 100, 1),
    "% de la variación en MPG\n")

# Predicciones
nuevos_pesos <- data.frame(wt = c(2.0, 3.0, 4.0, 5.0))
pred <- predict(modelo_simple, nuevos_pesos, interval = "confidence", level = 0.95)
cat("\nPredicciones con IC 95%:\n")
print(cbind(nuevos_pesos, round(pred, 2)))

# Gráfico de regresión
p6 <- ggplot(datos, aes(x = wt, y = mpg)) +
  geom_point(aes(color = cilindros), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#1F4E79",
              fill = "#2E86C1", alpha = 0.2) +
  scale_color_manual(values = c("#2E86C1", "#E74C3C", "#1F4E79")) +
  labs(title = "Regresión: MPG ~ Peso",
       subtitle = paste("R² =", round(summary(modelo_simple)$r.squared, 3),
                        "| Pendiente =", round(coef(modelo_simple)[2], 2)),
       x = "Peso (1000 lbs)", y = "Millas por galón",
       color = "Cilindros") +
  theme_minimal()
print(p6)

# Diagnósticos del modelo
cat("\n--- Diagnósticos del modelo ---\n")

# Residuos
datos$residuos <- residuals(modelo_simple)
datos$predichos <- fitted(modelo_simple)

# Test de normalidad de residuos
shapiro_res <- shapiro.test(datos$residuos)
cat("Normalidad de residuos (Shapiro-Wilk): p =",
    round(shapiro_res$p.value, 4), "\n")

# Gráficos de diagnóstico
p7 <- ggplot(datos, aes(x = predichos, y = residuos)) +
  geom_point(color = "#2E86C1", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#E74C3C", size = 0.8) +
  labs(title = "Residuos vs Valores Predichos",
       subtitle = "Buscar patrones → indica problemas de especificación",
       x = "Valores predichos", y = "Residuos") +
  theme_minimal()
print(p7)

# ==============================================================================
# PARTE 6: REGRESIÓN MÚLTIPLE
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("PARTE 6: Regresión Múltiple\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Modelo múltiple
modelo_multi <- lm(mpg ~ wt + hp + factor(cyl), data = datos)

cat("--- Modelo: mpg ~ wt + hp + cilindros ---\n")
print(summary(modelo_multi))

cat("\nIC 95% para coeficientes:\n")
print(confint(modelo_multi))

# Comparar modelos
cat("\n--- Comparación de modelos ---\n")
modelo_1 <- lm(mpg ~ wt, data = datos)
modelo_2 <- lm(mpg ~ wt + hp, data = datos)
modelo_3 <- lm(mpg ~ wt + hp + factor(cyl), data = datos)

comparacion <- data.frame(
  modelo = c("mpg ~ wt", "mpg ~ wt + hp", "mpg ~ wt + hp + cyl"),
  R2 = c(summary(modelo_1)$r.squared,
         summary(modelo_2)$r.squared,
         summary(modelo_3)$r.squared),
  R2_adj = c(summary(modelo_1)$adj.r.squared,
             summary(modelo_2)$adj.r.squared,
             summary(modelo_3)$adj.r.squared),
  AIC = c(AIC(modelo_1), AIC(modelo_2), AIC(modelo_3)),
  BIC = c(BIC(modelo_1), BIC(modelo_2), BIC(modelo_3))
)
comparacion[, 2:5] <- round(comparacion[, 2:5], 3)
print(comparacion)

# Test F para comparar modelos anidados
cat("\nTest F: Modelo 2 vs Modelo 3 (¿cyl agrega información?):\n")
print(anova(modelo_2, modelo_3))

# ==============================================================================
# PARTE 7: RESUMEN Y CONCLUSIONES
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("RESUMEN DEL ANÁLISIS\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("Dataset: mtcars (32 automóviles)\n")
cat("Variable dependiente: MPG (rendimiento)\n\n")

cat("Hallazgos principales:\n")
cat("1. El MPG promedio es", round(mean(datos$mpg), 1),
    "(DE =", round(sd(datos$mpg), 1), ")\n")
cat("2. Los autos manuales tienen mejor rendimiento que los automáticos\n")
cat("   (diferencia:", round(abs(diff(test_trans$estimate)), 1), "MPG, p =",
    format(test_trans$p.value, digits = 3), ")\n")
cat("3. Hay diferencias significativas de MPG entre cilindros (ANOVA p < 0.001)\n")
cat("4. El peso es el mejor predictor individual de MPG (R² =",
    round(summary(modelo_1)$r.squared, 3), ")\n")
cat("5. El modelo múltiple (wt + hp + cyl) explica",
    round(summary(modelo_3)$r.squared * 100, 1), "% de la variación\n")

cat("\n✓ Fin de Sesión 5 — Práctica Integradora\n")
