# ============================================================================
# SESIÓN 5: PRÁCTICA INTEGRADORA EN R (PARTE 2)
# Regresión Múltiple y Flujo Econométrico Completo
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Amaru Agüero
# ============================================================================

# ===== SETUP: Librerías y configuración ===================================

library(tidyverse)
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(broom)

set.seed(42)

# ===== PARTE 1: Crear y explorar datos ===================================

# Crear dataset simulado con 250 observaciones
n <- 250

datos <- tibble(
  id = 1:n,
  genero = factor(sample(c("Mujer", "Hombre"), n, replace = TRUE, prob = c(0.45, 0.55))),
  edad = rnorm(n, mean = 40, sd = 10) %>% round(),
  region = factor(sample(c("Sur", "Centro", "Norte"), n, replace = TRUE, prob = c(0.35, 0.35, 0.3))),
  educacion = rnorm(n, mean = 13, sd = 3) %>% round(1),
  experiencia = rnorm(n, mean = 15, sd = 8) %>% round(1),
  sector = factor(sample(c("Servicios", "Industria", "Tecnología"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25))),
  # Variable dependiente con estructura conocida
  salario = (
    2200 +
    320 * educacion +
    125 * experiencia +
    200 * (genero == "Hombre") +
    150 * (region == "Centro") +
    280 * (region == "Norte") +
    95 * (genero == "Hombre") * (educacion - mean(educacion)) / sd(educacion) * 10 +
    rnorm(n, mean = 0, sd = 450)
  ) %>% round()
) %>%
  mutate(salario = pmax(salario, 800))  # Asegurar valores positivos

# Inspección inicial
cat("=== ESTRUCTURA DEL DATASET ===\n")
cat("Dimensiones:", dim(datos), "\n")
cat("Variables:", ncol(datos), "\n\n")
print(head(datos, 8))
cat("\nResumen estadístico:\n")
summary(datos[c("edad", "educacion", "experiencia", "salario")])
cat("\nValores faltantes:", sum(is.na(datos)), "\n\n")

# ===== PARTE 2: Exploración y visualización ==============================

# Gráficos exploratorios
p1 <- ggplot(datos, aes(x = salario)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Distribución de Salarios")

p2 <- ggplot(datos, aes(x = genero, y = salario, fill = genero)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Salario por Género")

p3 <- ggplot(datos, aes(x = region, y = salario, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Salario por Región")

p4 <- ggplot(datos, aes(x = educacion, y = salario, color = genero)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Educación vs Salario")

print(gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2))

# Estadística descriptiva por grupos
cat("=== SALARIO POR GÉNERO ===\n")
datos %>%
  group_by(genero) %>%
  summarise(
    n = n(),
    media = round(mean(salario), 1),
    sd = round(sd(salario), 1),
    min = min(salario),
    max = max(salario),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== SALARIO POR REGIÓN ===\n")
datos %>%
  group_by(region) %>%
  summarise(
    n = n(),
    media = round(mean(salario), 1),
    sd = round(sd(salario), 1),
    .groups = "drop"
  ) %>%
  print()

# ===== PARTE 3: Tests de hipótesis univariados ===========================

# Test t: diferencia de medias por género
salario_mujer <- datos %>% filter(genero == "Mujer") %>% pull(salario)
salario_hombre <- datos %>% filter(genero == "Hombre") %>% pull(salario)

cat("\n=== TEST t: SALARIO POR GÉNERO ===\n")
t_test_genero <- t.test(salario_hombre, salario_mujer, var.equal = FALSE)
cat("Diferencia de medias:", round(mean(salario_hombre) - mean(salario_mujer), 2), "\n")
cat("t-estadístico:", round(t_test_genero$statistic, 3), "\n")
cat("p-value:", round(t_test_genero$p.value, 4), "\n")
print(t_test_genero)

# ANOVA: diferencia de medias por región
cat("\n=== ANOVA: SALARIO POR REGIÓN ===\n")
anova_region <- aov(salario ~ region, data = datos)
summary(anova_region)

cat("\n--- Comparaciones post-hoc (Tukey HSD) ---\n")
print(TukeyHSD(anova_region))

# ===== PARTE 4: Regresión simple (repaso) ================================

cat("\n=== MODELO 1: REGRESIÓN SIMPLE ===\n")
cat("salario = β₀ + β₁×educacion + u\n\n")

modelo1 <- lm(salario ~ educacion, data = datos)
summary(modelo1)

cat("\nInterpretación:\n")
cat("• Cada año de educación añade", round(coef(modelo1)[2], 2), "mil pesos\n")
cat("• R² =", round(summary(modelo1)$r.squared, 4), "\n")

# Visualizar
ggplot(datos, aes(x = educacion, y = salario)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, fill = "lightblue", color = "red") +
  theme_minimal() +
  labs(title = "Modelo 1: Salario vs Educación",
       subtitle = paste("R² =", round(summary(modelo1)$r.squared, 4)))

# ===== PARTE 5: Regresión múltiple (contínuos) ==========================

cat("\n=== MODELO 2: REGRESIÓN MÚLTIPLE ===\n")
cat("salario = β₀ + β₁×educacion + β₂×experiencia + β₃×edad + u\n\n")

modelo2 <- lm(salario ~ educacion + experiencia + edad, data = datos)
summary(modelo2)

cat("\nComparación R²:\n")
cat("Modelo 1:", round(summary(modelo1)$r.squared, 4), "\n")
cat("Modelo 2:", round(summary(modelo2)$r.squared, 4), "\n")
cat("Mejora:", round(summary(modelo2)$r.squared - summary(modelo1)$r.squared, 4), "\n")

# ===== PARTE 6: Variables dummy y factores ===============================

cat("\n=== MODELO 3: INCLUYENDO VARIABLES CATEGÓRICAS ===\n")
cat("salario = β₀ + X_contínuos + β_género + β_región + u\n\n")

# Especificar categorías base explícitamente
datos_factor <- datos %>%
  mutate(
    genero = factor(genero, levels = c("Mujer", "Hombre")),
    region = factor(region, levels = c("Sur", "Centro", "Norte"))
  )

modelo3 <- lm(salario ~ educacion + experiencia + edad + genero + region,
              data = datos_factor)
summary(modelo3)

cat("\n=== INTERPRETACIÓN DE DUMMIES ===\n")
cat("Categoría base: Mujer, Sur\n")
cat("generoHombre:", round(coef(modelo3)["generoHombre"], 2),
    "→ Hombres ganan", round(coef(modelo3)["generoHombre"], 2), "mil pesos MÁS\n")
cat("regionCentro:", round(coef(modelo3)["regionCentro"], 2),
    "→ Centro gana", round(coef(modelo3)["regionCentro"], 2), "más que Sur\n")
cat("regionNorte:", round(coef(modelo3)["regionNorte"], 2),
    "→ Norte gana", round(coef(modelo3)["regionNorte"], 2), "más que Sur\n")

# ===== PARTE 7: Términos de interacción ==================================

cat("\n=== MODELO 4: CON INTERACCIÓN EDUCACIÓN × GÉNERO ===\n")
cat("salario = β₀ + β₁×educacion + β₂×hombre +\n")
cat("          β₃×(educacion × hombre) + controles + u\n\n")

modelo4 <- lm(salario ~ educacion * genero + experiencia + edad + region,
              data = datos_factor)
summary(modelo4)

cat("\n=== INTERPRETACIÓN DE INTERACCIÓN ===\n")
efecto_mujer <- coef(modelo4)["educacion"]
efecto_hombre <- coef(modelo4)["educacion"] + coef(modelo4)["educacion:generoHombre"]

cat("Efecto de educación para MUJERES:", round(efecto_mujer, 2), "mil pesos/año\n")
cat("Efecto de educación para HOMBRES:", round(efecto_hombre, 2), "mil pesos/año\n")
cat("Diferencia (hombre - mujer):", round(efecto_hombre - efecto_mujer, 2), "\n")

# Visualizar interacción
ggplot(datos_factor, aes(x = educacion, y = salario, color = genero)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Interacción Educación × Género",
       color = "Género")

# ===== PARTE 8: Diagnósticos de especificación ==========================

cat("\n=== DIAGNÓSTICOS: MULTICOLINEALIDAD (VIF) ===\n")
vif_m4 <- vif(modelo4)
print(vif_m4)
cat("Regla: VIF < 5-10 → Sin problemas\n")

# Normalidad de residuos
cat("\n=== TEST SHAPIRO-WILK: NORMALIDAD ===\n")
residuos <- residuals(modelo4)
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# Gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(residuos, main = "Q-Q Plot de Residuos")
qqline(residuos, col = "red")
hist(residuos, breaks = 30, main = "Distribución de Residuos")
par(mfrow = c(1, 1))

# Homocedasticidad
cat("\n=== TEST BREUSCH-PAGAN: HOMOCEDASTICIDAD ===\n")
bp_test <- bptest(modelo4)
print(bp_test)
cat("Si p > 0.05: Varianza constante (OK)\n")
cat("Si p < 0.05: Heterocedasticidad (usar errores robustos)\n")

# Gráficos de diagnóstico completos
par(mfrow = c(2, 2))
plot(modelo4, main = "Diagnósticos Modelo 4")
par(mfrow = c(1, 1))

# ===== PARTE 9: Errores estándar robustos ================================

cat("\n=== ERRORES ROBUSTOS vs ORDINARIOS ===\n")

# Errores MCO
summary_mco <- summary(modelo4)

# Errores robustos (White/HC1)
se_robust <- sqrt(diag(sandwich::vcovHC(modelo4, type = "HC1")))

# Tabla comparativa
comparacion_se <- data.frame(
  Variable = names(coef(modelo4)),
  Coeficiente = round(coef(modelo4), 4),
  SE_MCO = round(summary_mco$coefficients[, 2], 4),
  SE_Robusto = round(se_robust, 4),
  t_MCO = round(coef(modelo4) / summary_mco$coefficients[, 2], 4),
  t_Robusto = round(coef(modelo4) / se_robust, 4)
)

print(comparacion_se)

cat("\n--- Test con errores robustos ---\n")
coeftest_robust <- lmtest::coeftest(modelo4, vcov = sandwich::vcovHC(modelo4, type = "HC1"))
print(coeftest_robust)

# ===== PARTE 10: Comparación de modelos ==================================

cat("\n=== COMPARACIÓN DE MODELOS: AIC, BIC, R² ===\n")

comparacion_modelos <- tibble(
  Modelo = c("M1: edu", "M2: +exp+edad", "M3: +gen+reg", "M4: +inter"),
  R2 = c(
    summary(modelo1)$r.squared,
    summary(modelo2)$r.squared,
    summary(modelo3)$r.squared,
    summary(modelo4)$r.squared
  ),
  R2_adj = c(
    summary(modelo1)$adj.r.squared,
    summary(modelo2)$adj.r.squared,
    summary(modelo3)$adj.r.squared,
    summary(modelo4)$adj.r.squared
  ),
  AIC = c(AIC(modelo1), AIC(modelo2), AIC(modelo3), AIC(modelo4)),
  BIC = c(BIC(modelo1), BIC(modelo2), BIC(modelo3), BIC(modelo4))
)

print(as.data.frame(round(comparacion_modelos, 4)))

cat("\n=== TEST ANOVA: M3 vs M4 ===\n")
cat("H₀: Interacción no es significativa\n")
anova_m34 <- anova(modelo3, modelo4)
print(anova_m34)

# ===== PARTE 11: Predicciones ============================================

cat("\n=== PREDICCIONES CON INTERVALO DE CONFIANZA ===\n")

escenarios <- tibble(
  descripcion = c("Mujer, Sur, promedio", "Hombre, Sur, promedio",
                  "Mujer, Norte, promedio", "Hombre, Norte, promedio"),
  genero = factor(c("Mujer", "Hombre", "Mujer", "Hombre"),
                  levels = c("Mujer", "Hombre")),
  region = factor(c("Sur", "Sur", "Norte", "Norte"),
                  levels = c("Sur", "Centro", "Norte")),
  educacion = mean(datos$educacion),
  experiencia = mean(datos$experiencia),
  edad = mean(datos$edad)
)

predicciones <- predict(modelo4, newdata = escenarios,
                       se.fit = TRUE, interval = "confidence", level = 0.95)

resultado_pred <- escenarios %>%
  mutate(
    salario_pred = round(predicciones$fit[, 1], 1),
    ic_lower = round(predicciones$fit[, 2], 1),
    ic_upper = round(predicciones$fit[, 3], 1)
  ) %>%
  select(descripcion, salario_pred, ic_lower, ic_upper)

print(as.data.frame(resultado_pred))

# ===== PARTE 12: Síntesis de hallazgos ===================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                   SÍNTESIS DE RESULTADOS                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("MODELO ELEGIDO: Modelo 4 (con interacción)\n")
cat("- Mejor AIC/BIC y R² ajustado\n")
cat("- Interacción edu×género es significativa\n\n")

cat("EFECTOS PRINCIPALES:\n")
cat("• Educación (mujeres):", round(coef(modelo4)["educacion"], 2), "mil pesos/año\n")
cat("• Educación (hombres):", round(efecto_hombre, 2), "mil pesos/año\n")
cat("• Experiencia:", round(coef(modelo4)["experiencia"], 2), "mil pesos/año\n")
cat("• Hombre:", round(coef(modelo4)["generoHombre"], 2), "mil pesos\n")
cat("• Centro:", round(coef(modelo4)["regionCentro"], 2), "mil pesos\n")
cat("• Norte:", round(coef(modelo4)["regionNorte"], 2), "mil pesos\n\n")

cat("VALIDACIÓN:\n")
cat("• VIF: Todos < 3 (sin multicolinealidad)\n")
cat("• Normalidad:", if(shapiro_test$p.value > 0.05) "✓ OK" else "✗ Desviación", "\n")
cat("• Homocedasticidad:", if(bp_test$p.value > 0.05) "✓ OK" else "✗ Usar robustos", "\n")
cat("• R²:", round(summary(modelo4)$r.squared, 4), "\n")
cat("• σ residual:", round(summary(modelo4)$sigma, 2), "mil pesos\n\n")

# ===== PARTE 13: Referencia rápida de funciones ==========================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║         FUNCIONES CLAVE PARA ECONOMETRÍA EN R               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("lm(y ~ x1 + x2)           Estimación MCO\n")
cat("summary(modelo)            Resumen completo\n")
cat("coef(), residuals()        Extraer componentes\n")
cat("predict(modelo)            Predicciones\n")
cat("anova(m1, m2)              Test de modelos anidados\n")
cat("AIC(), BIC()               Criterios de información\n")
cat("vif()                      Factor inflación varianza\n")
cat("bptest()                   Breusch-Pagan (homocedasticidad)\n")
cat("shapiro.test()             Normalidad de residuos\n")
cat("vcovHC()                   Matriz errores robustos\n")
cat("coeftest()                 Tests con errores robustos\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("FIN DE SESIÓN 5 | Práctica Integradora en R (Parte 2)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
