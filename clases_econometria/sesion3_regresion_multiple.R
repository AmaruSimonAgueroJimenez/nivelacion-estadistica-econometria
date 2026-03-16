# ============================================================================
# SESIÓN 3: REGRESIÓN LINEAL MÚLTIPLE
# Econometría - Doctorado en Ciencias de la Complejidad Social, UDD
# ============================================================================

# ============================================================================
# 1. CONFIGURACIÓN Y DATOS
# ============================================================================

# Limpiar entorno
rm(list = ls())

# Librerías
library(tidyverse)
library(car)      # Para VIF
library(broom)    # Para tablas limpias

# Generar datos de ejemplo
set.seed(42)
n <- 200

datos <- tibble(
  id = 1:n,
  # Años de educación (8-20 años)
  educacion = rnorm(n, mean = 13, sd = 3) %>% pmax(8),

  # Años de experiencia (0-40 años)
  experiencia = rnorm(n, mean = 15, sd = 10) %>% pmax(0),

  # Género (1 = mujer, 0 = hombre)
  mujer = rbinom(n, size = 1, prob = 0.5),

  # Log-ingreso mensual (miles CLP)
  # Modelo verdadero: log(ingreso) = 9 + 0.15*educacion + 0.08*experiencia - 0.25*mujer + error
  log_ingreso = 9 + 0.15 * educacion + 0.08 * experiencia - 0.25 * mujer + rnorm(n, sd = 0.3),
  ingreso = exp(log_ingreso)
)

# Vista previa
head(datos, 10)
str(datos)


# ============================================================================
# 2. SESGO DE VARIABLE OMITIDA (OVB)
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("2. SESGO DE VARIABLE OMITIDA (OVB)\n")
cat("==============================================================\n")

# Modelo 1: Solo educación (modelo sesgado - omite experiencia y género)
m1 <- lm(log_ingreso ~ educacion, data = datos)

# Modelo 2: Educación + Experiencia
m2 <- lm(log_ingreso ~ educacion + experiencia, data = datos)

# Modelo 3: Completo (educación + experiencia + género)
m3 <- lm(log_ingreso ~ educacion + experiencia + mujer, data = datos)

# Comparar coeficientes de educación
cat("Coeficiente de educación:\n")
cat("  M1 (solo educación):           ", round(coef(m1)["educacion"], 4), "\n")
cat("  M2 (+ experiencia):            ", round(coef(m2)["educacion"], 4), "\n")
cat("  M3 (+ experiencia + género):   ", round(coef(m3)["educacion"], 4), "\n")

# Cambio porcentual
cambio_12 <- (coef(m2)["educacion"] - coef(m1)["educacion"]) / abs(coef(m1)["educacion"]) * 100
cambio_23 <- (coef(m3)["educacion"] - coef(m2)["educacion"]) / abs(coef(m2)["educacion"]) * 100

cat("\n  Cambio M1→M2: ", round(cambio_12, 2), "%\n")
cat("  Cambio M2→M3: ", round(cambio_23, 2), "%\n")

# Correlaciones que explican el OVB
cat("\nCorrelaciones (indicio de confusión):\n")
cat("  Cor(educación, experiencia):", round(cor(datos$educacion, datos$experiencia), 3), "\n")
cat("  Cor(educación, mujer):      ", round(cor(datos$educacion, datos$mujer), 3), "\n")


# ============================================================================
# 3. VARIABLES CATEGÓRICAS (DUMMY VARIABLES)
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("3. VARIABLES CATEGÓRICAS (DUMMY VARIABLES)\n")
cat("==============================================================\n")

# Crear variable de factor para género
datos <- datos %>%
  mutate(genero = factor(mujer, levels = c(0, 1), labels = c("Hombre", "Mujer")))

cat("Niveles de la variable genero:\n")
print(levels(datos$genero))

# Modelo con dummy variable
m_dummy <- lm(log_ingreso ~ educacion + experiencia + genero, data = datos)

cat("\nResumen del modelo con dummy:\n")
print(summary(m_dummy))

# Interpretación: coeficiente de generoMujer
beta_mujer <- coef(m_dummy)["generoMujer"]
cat("\nInterpretación de generoMujer =", round(beta_mujer, 4), ":\n")
cat("  Las mujeres ganan", round((exp(beta_mujer) - 1) * 100, 2),
    "% MENOS que los hombres\n")
cat("  (controlando por educación y experiencia)\n")


# ============================================================================
# 4. CAMBIO DE CATEGORÍA DE REFERENCIA CON relevel()
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("4. CAMBIO DE CATEGORÍA DE REFERENCIA\n")
cat("==============================================================\n")

# Cambiar categoría de referencia a "Mujer"
datos <- datos %>%
  mutate(genero = relevel(genero, ref = "Mujer"))

m_dummy_rev <- lm(log_ingreso ~ educacion + experiencia + genero, data = datos)

beta_hombre <- coef(m_dummy_rev)["generoHombre"]
cat("Coeficiente de generoHombre:", round(beta_hombre, 4), "\n")
cat("Interpretación:\n")
cat("  Los hombres ganan", round((exp(beta_hombre) - 1) * 100, 2),
    "% MÁS que las mujeres\n")
cat("  (es el mismo efecto, pero interpretado desde otra base)\n")

# Revertir a referencia "Hombre" para consistencia
datos <- datos %>%
  mutate(genero = relevel(genero, ref = "Hombre"))


# ============================================================================
# 5. TÉRMINOS DE INTERACCIÓN: continua × dummy
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("5. TÉRMINOS DE INTERACCIÓN (EDUCACIÓN × GÉNERO)\n")
cat("==============================================================\n")

# Modelo sin interacción
m_sin_int <- lm(log_ingreso ~ educacion + genero + experiencia, data = datos)

# Modelo con interacción
m_con_int <- lm(log_ingreso ~ educacion * genero + experiencia, data = datos)

cat("Comparación de coeficientes:\n")
cat("\nSin interacción:\n")
print(coef(m_sin_int))

cat("\nCon interacción (educacion × genero):\n")
print(coef(m_con_int))

# Interpretación de retornos
beta <- coef(m_con_int)
retorno_hombres <- beta["educacion"]
retorno_mujeres <- beta["educacion"] + beta["educacion:generoMujer"]

cat("\nInterpretación de retornos a educación:\n")
cat("  Para hombres: ", round(retorno_hombres, 4), "(aumento % en ingreso por año edu.)\n")
cat("  Para mujeres: ", round(retorno_mujeres, 4), "(aumento % en ingreso por año edu.)\n")
cat("  Diferencia:   ", round(beta["educacion:generoMujer"], 4), "\n")

# Test F para la interacción
test_int <- anova(m_sin_int, m_con_int)
cat("\nTest F para interacción:\n")
print(test_int)


# ============================================================================
# 6. TÉRMINOS DE INTERACCIÓN: continua × continua
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("6. INTERACCIÓN (EDUCACIÓN × EXPERIENCIA)\n")
cat("==============================================================\n")

# Modelo con interacción educación × experiencia
m_cont <- lm(log_ingreso ~ educacion * experiencia + genero, data = datos)

cat("Resumen del modelo:\n")
print(summary(m_cont))

# Efectos parciales
beta_cont <- coef(m_cont)
cat("\nEfecto parcial de educación:\n")
cat("  ∂Y/∂Educ = ", round(beta_cont["educacion"], 4), " + ",
    round(beta_cont["educacion:experiencia"], 6), " × Experiencia\n", sep = "")

cat("\nRetorno a educación a diferentes niveles de experiencia:\n")
for (exp in c(10, 15, 20, 25)) {
  retorno <- beta_cont["educacion"] + beta_cont["educacion:experiencia"] * exp
  cat("  A", exp, "años experiencia:", round(retorno, 4), "\n")
}


# ============================================================================
# 7. PRUEBAS DE SIGNIFICANCIA: TEST t INDIVIDUAL
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("7. TEST t INDIVIDUAL\n")
cat("==============================================================\n")

cat("Resumen del modelo M3:\n")
print(summary(m3))

# Usar broom para tabla limpia
resumen_m3 <- tidy(m3)
cat("\nTabla de coeficientes (con broom):\n")
print(resumen_m3)


# ============================================================================
# 8. PRUEBAS DE SIGNIFICANCIA: TEST F GLOBAL Y ANIDADOS
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("8. TEST F: GLOBAL Y PARA MODELOS ANIDADOS\n")
cat("==============================================================\n")

# F global (modelo vs nulo)
glance_m3 <- glance(m3)
cat("F-statistic global (M3 vs modelo nulo):\n")
cat("  F =", round(glance_m3$statistic, 3), "\n")
cat("  p-value < 0.001 (muy significativo)\n")

# Test F para M1 vs M2 (¿es experiencia significativa?)
cat("\n\nTest F: M1 vs M2 (¿agregamos experiencia?)\n")
anova_12 <- anova(m1, m2)
print(anova_12)
cat("Conclusión: Experiencia es significativa (p <", round(anova_12$`Pr(>F)`[2], 4), ")\n")

# Test F para M2 vs M3 (¿es género significativo?)
cat("\n\nTest F: M2 vs M3 (¿agregamos género?)\n")
anova_23 <- anova(m2, m3)
print(anova_23)
cat("Conclusión: Género es significativo (p <", round(anova_23$`Pr(>F)`[2], 4), ")\n")

# Test F para M3 vs M_con_int (¿es la interacción significativa?)
cat("\n\nTest F: M3 vs M_con_int (¿agregamos interacción edu×mujer?)\n")
anova_34 <- anova(m3, m_con_int)
print(anova_34)


# ============================================================================
# 9. MODELOS ANIDADOS: COMPARACIÓN SISTEMÁTICA
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("9. COMPARACIÓN DE MODELOS ANIDADOS\n")
cat("==============================================================\n")

# Crear tabla comparativa
comparacion <- tibble(
  Modelo = c("M1", "M2", "M3", "M_int"),
  Especificacion = c(
    "log_ingreso ~ educacion",
    "log_ingreso ~ educacion + experiencia",
    "log_ingreso ~ educacion + experiencia + genero",
    "log_ingreso ~ educacion * genero + experiencia"
  ),
  R2 = c(
    round(glance(m1)$r.squared, 4),
    round(glance(m2)$r.squared, 4),
    round(glance(m3)$r.squared, 4),
    round(glance(m_con_int)$r.squared, 4)
  ),
  R2_adj = c(
    round(glance(m1)$adj.r.squared, 4),
    round(glance(m2)$adj.r.squared, 4),
    round(glance(m3)$adj.r.squared, 4),
    round(glance(m_con_int)$adj.r.squared, 4)
  ),
  AIC = c(
    round(AIC(m1), 2),
    round(AIC(m2), 2),
    round(AIC(m3), 2),
    round(AIC(m_con_int), 2)
  ),
  BIC = c(
    round(BIC(m1), 2),
    round(BIC(m2), 2),
    round(BIC(m3), 2),
    round(BIC(m_con_int), 2)
  )
)

cat("Tabla de comparación de modelos:\n")
print(comparacion)

cat("\nInterpretación:\n")
cat("  R²: Mayor es mejor\n")
cat("  R² ajustado: Penaliza por número de variables\n")
cat("  AIC/BIC: Menor es mejor (balance ajuste vs complejidad)\n")


# ============================================================================
# 10. MULTICOLINEALIDAD: VIF
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("10. DETECCIÓN DE MULTICOLINEALIDAD (VIF)\n")
cat("==============================================================\n")

# Calcular VIF para M3
vif_m3 <- vif(m3)
cat("Variance Inflation Factor (VIF) para M3:\n")
print(vif_m3)

cat("\nCriterios de interpretación:\n")
cat("  VIF < 5: Sin problema\n")
cat("  VIF 5-10: Potencial problema\n")
cat("  VIF > 10: Problema grave\n\n")

for (var in names(vif_m3)) {
  status <- if (vif_m3[var] < 5) "OK" else if (vif_m3[var] < 10) "ALERTA" else "PROBLEMA"
  cat("  ", var, ":", round(vif_m3[var], 2), "(", status, ")\n")
}


# ============================================================================
# 11. ANÁLISIS DE RESIDUOS Y DIAGNÓSTICOS
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("11. ANÁLISIS DE RESIDUOS\n")
cat("==============================================================\n")

# Residuos del modelo M3
residuos_m3 <- residuals(m3)
fitted_m3 <- fitted(m3)
std_residuos <- rstandard(m3)

# Normalidad de residuos: test Shapiro-Wilk
test_norm <- shapiro.test(residuos_m3)
cat("Test de Shapiro-Wilk (normalidad):\n")
cat("  W =", round(test_norm$statistic, 4), "\n")
cat("  p-value =", round(test_norm$p.value, 4), "\n")
cat("  Conclusión:", if (test_norm$p.value > 0.05) "Residuos ~ Normal" else "Residuos NO normales", "\n")

# Homocedasticidad: test de Breusch-Pagan
cat("\nPrueba de heterocedasticidad (Breusch-Pagan):\n")
bp_test <- lmtest::bptest(m3)
cat("  Estadístico =", round(bp_test$statistic, 4), "\n")
cat("  p-value =", round(bp_test$p.value, 4), "\n")
cat("  Conclusión:", if (bp_test$p.value > 0.05) "Homocedasticidad OK" else "Heterocedasticidad presente", "\n")

# Autocorrelación: Durbin-Watson
cat("\nPrueba de autocorrelación (Durbin-Watson):\n")
dw_test <- lmtest::dwtest(m3)
cat("  DW =", round(dw_test$statistic, 4), "\n")
cat("  p-value =", round(dw_test$p.value, 4), "\n")


# ============================================================================
# 12. PREDICCIONES Y MÁRGENES
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("12. PREDICCIONES\n")
cat("==============================================================\n")

# Crear casos hipotéticos
casos <- tibble(
  educacion = c(12, 16, 16),
  experiencia = c(10, 10, 15),
  genero = factor(c("Hombre", "Hombre", "Mujer"), levels = levels(datos$genero))
)

pred <- predict(m3, newdata = casos, interval = "confidence", level = 0.95)

resultado <- cbind(casos, pred) %>%
  mutate(
    ingreso_pred = exp(fit),
    ingreso_lower = exp(lwr),
    ingreso_upper = exp(upr)
  )

cat("Predicciones para casos hipotéticos:\n")
print(resultado)


# ============================================================================
# 13. RESUMEN Y RECOMENDACIONES
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("13. RESUMEN: ESTRATEGIA DE SELECCIÓN DE VARIABLES\n")
cat("==============================================================\n")

cat("
¿CUÁNDO INCLUIR UNA VARIABLE?

1. CONFUSORA (X ← Z → Y):
   - Causa tanto al regresor como al resultado
   - Genera asociación espuria
   - ACCIÓN: INCLUIR en el modelo

2. COLISIONADORA (X → Z ← Y):
   - Causada tanto por X como por Y
   - Controlar introduce sesgo (Berkson)
   - ACCIÓN: NO INCLUIR

3. MEDIADORA (X → Z → Y):
   - Está en el camino causal
   - Absorbe parte del efecto
   - ACCIÓN: NO INCLUIR (si efecto total es de interés)

4. MODIFICADORA (Z modifica X → Y):
   - El efecto de X en Y depende de Z
   - Se detecta con interacción significativa
   - ACCIÓN: INCLUIR INTERACCIÓN X × Z

CHECKLIST PRÁCTICO:
✓ Dibujar el DAG antes de estimar
✓ Reportar modelos anidados
✓ Verificar cambios > 20% en coeficientes (señal OVB)
✓ Revisar significancia con F-tests
✓ Monitorear VIF (< 5 es OK)
✓ Examinar residuos: normalidad, homocedasticidad, autocorr.
✓ Usar múltiples criterios: R², AIC, BIC
✓ Documentar hipótesis causales
")


# ============================================================================
# 14. TABLA FINAL CON stargazer (opcional)
# ============================================================================

cat("\n")
cat("==============================================================\n")
cat("14. TABLA PROFESIONAL DE REGRESIONES\n")
cat("==============================================================\n")

# Instalar si es necesario: install.packages("stargazer")
if (require(stargazer, quietly = TRUE)) {
  cat("Tabla de regresiones con stargazer:\n\n")
  stargazer(m1, m2, m3, m_con_int,
            type = "text",
            title = "Modelos de Regresión Anidados",
            column.labels = c("Simple", "+ Control", "+ Dummy", "Interacción"),
            dep.var.labels = "Log(Ingreso)",
            covariate.labels = c("Educación", "Experiencia", "Mujer",
                                 "Edu × Mujer"),
            omit.stat = c("f", "ser"),
            digits = 4,
            notes = "Modelos estimados por MCO. Variable dependiente: log(ingreso mensual)"
  )
} else {
  cat("stargazer no está instalado. Instalar con: install.packages('stargazer')\n")
}

cat("\n")
cat("==============================================================\n")
cat("FIN DE LA SESIÓN 3\n")
cat("==============================================================\n")
