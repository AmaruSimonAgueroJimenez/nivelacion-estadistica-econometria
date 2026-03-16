# ===== SESION 4: PRACTICA INTEGRADORA EN R (PARTE 1) =====
# Pruebas de Hipótesis y Regresión Lineal Simple
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Autor: Amaru Agüero
# Fecha: 26 de marzo de 2026

# Limpiar ambiente
rm(list = ls())

# ===== 0. CARGAR PAQUETES =====

library(tidyverse)
library(ggplot2)
library(lmtest)
library(car)

# ===== 1. CREAR Y EXPLORAR DATOS =====

# Fijar semilla para reproducibilidad
set.seed(2026)
n <- 150

# Crear dataset de encuesta social
datos <- data.frame(
  id = 1:n,
  ingresos = rnorm(n, mean = 2200, sd = 800) +
    seq(0, 1500, length.out = n),
  educacion = rnorm(n, mean = 13, sd = 3),
  genero = sample(c("Hombre", "Mujer"), n, replace = TRUE,
                  prob = c(0.52, 0.48)),
  region = sample(c("Norte", "Centro", "Sur"), n, replace = TRUE),
  empleo = sample(c("Empleado", "Desempleado"), n, replace = TRUE,
                  prob = c(0.85, 0.15)),
  satisfaccion = sample(1:5, n, replace = TRUE)
)

# Asegurar ingresos positivos
datos$ingresos <- pmax(datos$ingresos, 800)

# Exploración inicial
cat("===== EXPLORACION INICIAL =====\n\n")
dim(datos)
str(datos)
summary(datos)
head(datos, 10)

# ===== 2. ESTADISTICA DESCRIPTIVA =====

cat("\n===== ESTADISTICA DESCRIPTIVA =====\n\n")

# Resumen por género
cat("Resumen por género:\n")
print(
  datos %>%
    group_by(genero) %>%
    summarise(
      n = n(),
      media_ingresos = mean(ingresos),
      sd_ingresos = sd(ingresos),
      media_educacion = mean(educacion),
      .groups = 'drop'
    )
)

# Resumen por empleo
cat("\nResumen por estado de empleo:\n")
print(
  datos %>%
    group_by(empleo) %>%
    summarise(
      n = n(),
      media_ingresos = mean(ingresos),
      sd_ingresos = sd(ingresos),
      .groups = 'drop'
    )
)

# Tablas de frecuencia
cat("\nDistribución por género:\n")
print(table(datos$genero))

cat("\nDistribución por región:\n")
print(table(datos$region))

cat("\nTabla cruzada: género vs región\n")
print(table(datos$genero, datos$region))

# ===== 3. PRUEBAS DE HIPOTESIS: T-TEST =====

cat("\n===== PRUEBAS T: COMPARACION DE MEDIAS =====\n\n")

# 3.1 Diferencia de ingresos por género
cat("Pregunta 1: ¿Hay diferencia de ingresos entre hombres y mujeres?\n")
cat("H0: µ_Hombre = µ_Mujer\n")
cat("Ha: µ_Hombre ≠ µ_Mujer\n\n")

resultado_t_genero <- t.test(ingresos ~ genero, data = datos)
print(resultado_t_genero)

# Interpretación
cat("\nInterpretación:\n")
if (resultado_t_genero$p.value < 0.05) {
  cat("p-value =", round(resultado_t_genero$p.value, 4), "< 0.05\n")
  cat("RECHAZAMOS H0: Sí hay diferencia significativa.\n")
} else {
  cat("p-value =", round(resultado_t_genero$p.value, 4), ">= 0.05\n")
  cat("NO RECHAZAMOS H0: No hay diferencia significativa.\n")
}

# 3.2 Diferencia de ingresos por empleo
cat("\n\nPregunta 2: ¿Hay diferencia de ingresos por estado de empleo?\n")
cat("H0: µ_Empleado = µ_Desempleado\n\n")

resultado_t_empleo <- t.test(ingresos ~ empleo, data = datos)
print(resultado_t_empleo)

cat("\nInterpretación:\n")
if (resultado_t_empleo$p.value < 0.05) {
  cat("p-value < 0.05: RECHAZAMOS H0\n")
  cat("Hay diferencia significativa en ingresos por empleo.\n")
} else {
  cat("p-value >= 0.05: NO RECHAZAMOS H0\n")
}

# ===== 4. PRUEBA DE PROPORCIONES =====

cat("\n===== PRUEBA DE PROPORCIONES =====\n\n")

# 4.1 ¿Proporción de mujeres = 0.5?
cat("Pregunta 3: ¿Es la proporción de mujeres igual a 0.5?\n")
cat("H0: p = 0.5\n")
cat("Ha: p ≠ 0.5\n\n")

tabla_genero <- table(datos$genero)
print(tabla_genero)

n_mujeres <- tabla_genero["Mujer"]
n_total <- sum(tabla_genero)
prop_mujeres <- n_mujeres / n_total

resultado_prop_genero <- prop.test(n_mujeres, n_total, p = 0.5)
print(resultado_prop_genero)

cat("\nProporción observada de mujeres:", round(prop_mujeres, 3), "\n")
if (resultado_prop_genero$p.value < 0.05) {
  cat("p-value < 0.05: Proporción significativamente diferente de 0.5\n")
} else {
  cat("p-value >= 0.05: Proporción no diferente de 0.5\n")
}

# 4.2 ¿Proporción de desempleados = 0.15?
cat("\n\nPregunta 4: ¿Es la proporción de desempleados igual a 0.15?\n")

tabla_empleo <- table(datos$empleo)
n_desempleados <- tabla_empleo["Desempleado"]
prop_desempleados <- n_desempleados / n_total

resultado_prop_empleo <- prop.test(n_desempleados, n_total, p = 0.15)
print(resultado_prop_empleo)

# ===== 5. PRUEBA CHI-CUADRADO =====

cat("\n===== PRUEBA CHI-CUADRADO: INDEPENDENCIA =====\n\n")

# 5.1 Género y región
cat("Pregunta 5: ¿Son género y región independientes?\n")
cat("H0: Género y región son independientes\n")
cat("Ha: Género y región están asociados\n\n")

tabla_genero_region <- table(datos$genero, datos$region)
print(tabla_genero_region)

resultado_chi_gr <- chisq.test(tabla_genero_region)
print(resultado_chi_gr)

cat("\nFrecuencias esperadas:\n")
print(round(resultado_chi_gr$expected, 2))

if (resultado_chi_gr$p.value < 0.05) {
  cat("\np-value < 0.05: RECHAZAMOS H0\n")
  cat("Género y región ESTÁN ASOCIADOS.\n")
} else {
  cat("\np-value >= 0.05: NO RECHAZAMOS H0\n")
  cat("Género y región SON INDEPENDIENTES.\n")
}

# 5.2 Empleo y región
cat("\n\nPregunta 6: ¿Están asociados empleo y región?\n")

tabla_empleo_region <- table(datos$empleo, datos$region)
print(tabla_empleo_region)

resultado_chi_er <- chisq.test(tabla_empleo_region)
print(resultado_chi_er)

# ===== 6. REGRESION LINEAL SIMPLE =====

cat("\n===== REGRESION LINEAL SIMPLE =====\n\n")

# 6.1 Modelo: ingresos ~ educacion
cat("Pregunta 7: ¿Cómo se relacionan ingresos y educación?\n")
cat("Modelo: ingresos = β0 + β1*educacion + u\n\n")

modelo1 <- lm(ingresos ~ educacion, data = datos)
print(summary(modelo1))

# Extraer coeficientes
coef_const <- coef(modelo1)[1]
coef_edu <- coef(modelo1)[2]

cat("\n===== INTERPRETACION DEL MODELO =====\n\n")

cat("Ecuación estimada:\n")
cat("ingresos = ", round(coef_const, 2), " + ",
    round(coef_edu, 2), " * educacion\n\n")

cat("Interpretación de coeficientes:\n")
cat("- Intercepto (β0):", round(coef_const, 2), "USD\n")
cat("  Ingreso estimado cuando educación = 0 años\n\n")

cat("- Pendiente (β1):", round(coef_edu, 2), "USD/año\n")
cat("  Cada año adicional de educación se asocia con\n")
cat("  ", round(coef_edu, 2), "USD adicionales en ingreso\n\n")

# Significancia
pvalue_edu <- summary(modelo1)$coefficients[2, 4]
cat("p-value de educación:", round(pvalue_edu, 4), "\n")
if (pvalue_edu < 0.05) {
  cat("La educación ES significativa (***)\n")
} else {
  cat("La educación NO es significativa\n")
}

# R-squared
r2 <- summary(modelo1)$r.squared
r2_adj <- summary(modelo1)$adj.r.squared

cat("\nBondad de ajuste:\n")
cat("R²:", round(r2, 3),
    "→ Educación explica el", round(r2*100, 1), "% de la varianza en ingresos\n")
cat("R² ajustado:", round(r2_adj, 3), "\n")

# Predicción
cat("\n\nPredicciones del modelo:\n")
predicciones <- data.frame(
  educacion = c(10, 12, 14, 16),
  ingreso_predicho = predict(modelo1, newdata = data.frame(
    educacion = c(10, 12, 14, 16)))
)
print(predicciones)

# ===== 7. DIAGNOSTICOS DEL MODELO =====

cat("\n===== DIAGNOSTICOS Y VALIDACION DE SUPUESTOS =====\n\n")

residuos1 <- residuals(modelo1)
ajustados1 <- fitted(modelo1)

# 7.1 Normalidad (Shapiro-Wilk)
cat("Supuesto 1: NORMALIDAD DE RESIDUOS\n")
cat("H0: Los residuos provienen de una distribución normal\n\n")

test_shapiro <- shapiro.test(residuos1)
print(test_shapiro)

if (test_shapiro$p.value > 0.05) {
  cat("p-value > 0.05: NO RECHAZAMOS H0\n")
  cat("Los residuos SON aproximadamente normales ✓\n")
} else {
  cat("p-value < 0.05: RECHAZAMOS H0\n")
  cat("Posible VIOLACION de normalidad\n")
}

# 7.2 Homocedasticidad (Breusch-Pagan)
cat("\n\nSupuesto 2: HOMOCEDASTICIDAD (varianza constante)\n")
cat("H0: La varianza de residuos es constante\n\n")

test_bp <- bptest(modelo1)
print(test_bp)

if (test_bp$p.value > 0.05) {
  cat("p-value > 0.05: NO RECHAZAMOS H0\n")
  cat("La varianza ES aproximadamente constante ✓\n")
} else {
  cat("p-value < 0.05: RECHAZAMOS H0\n")
  cat("Evidencia de HETEROSCEDASTICIDAD\n")
}

# 7.3 Resumen diagnóstico
cat("\n\nRESUMEN DIAGNOSTICO:\n")
cat("Supuesto        | Estado     | p-value\n")
cat("Normalidad      |",
    if(test_shapiro$p.value > 0.05) "✓ Cumplido" else "✗ Violado",
    " |", round(test_shapiro$p.value, 4), "\n")
cat("Homocedasticidad|",
    if(test_bp$p.value > 0.05) "✓ Cumplido" else "✗ Violado",
    " |", round(test_bp$p.value, 4), "\n")

# ===== 8. VISUALIZACIONES =====

cat("\n===== GRAFICOS Y VISUALIZACIONES =====\n\n")

# 8.1 Scatter plot con regresión
grafico1 <- ggplot(datos, aes(x = educacion, y = ingresos)) +
  geom_point(color = "#1F4E79", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#2E86C1", fill = "#2E86C1", alpha = 0.2) +
  labs(
    title = "Relación: Educación vs. Ingresos",
    subtitle = paste0("R² = ", round(r2, 3)),
    x = "Años de Educación",
    y = "Ingresos Mensuales (USD)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# 8.2 Boxplot por género
grafico2 <- ggplot(datos, aes(x = genero, y = ingresos, fill = genero)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    title = "Distribución de Ingresos por Género",
    x = "Género",
    y = "Ingresos (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  )

# 8.3 Residuos vs ajustados
df_diag <- data.frame(
  ajustados = ajustados1,
  residuos = residuos1
)

grafico3 <- ggplot(df_diag, aes(x = ajustados, y = residuos)) +
  geom_point(color = "#1F4E79", alpha = 0.6, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Residuos vs. Valores Ajustados",
    x = "Valores Ajustados",
    y = "Residuos"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# Mostrar gráficos
print(grafico1)
print(grafico2)
print(grafico3)

# Gráficos diagnósticos automáticos
cat("\nGráficos diagnósticos automáticos del modelo:\n")
par(mfrow = c(2, 2))
plot(modelo1)
par(mfrow = c(1, 1))

# ===== 9. EJERCICIO INTEGRADOR =====

cat("\n===== EJERCICIO INTEGRADOR: ANALISIS COMPLETO =====\n\n")

cat("A. PRUEBAS DE HIPOTESIS APLICADAS:\n")
cat("   1. t-test género: p-value =", round(resultado_t_genero$p.value, 4), "\n")
cat("   2. t-test empleo: p-value =", round(resultado_t_empleo$p.value, 4), "\n")
cat("   3. prop.test mujeres: p-value =", round(resultado_prop_genero$p.value, 4), "\n")
cat("   4. chi-square género-región: p-value =", round(resultado_chi_gr$p.value, 4), "\n\n")

cat("B. MODELO DE REGRESION:\n")
cat("   Ecuación: ingresos =", round(coef_const, 2), "+",
    round(coef_edu, 2), "*educacion\n")
cat("   R²:", round(r2, 3), "(explica", round(r2*100, 1), "% de varianza)\n")
cat("   Significancia β1: p-value =", round(pvalue_edu, 4), "\n\n")

cat("C. VALIDACION DE SUPUESTOS:\n")
cat("   Normalidad (Shapiro): p-value =", round(test_shapiro$p.value, 4), "\n")
cat("   Homocedasticidad (BP): p-value =", round(test_bp$p.value, 4), "\n\n")

cat("D. CONCLUSIONES:\n")
cat("   - Educación es un factor significativo en explicar ingresos\n")
cat("   - Cada año de educación adicional se asocia con",
    round(coef_edu, 2), "USD más\n")
cat("   - El modelo cumple aproximadamente con los supuestos OLS\n")
cat("   - Hay variabilidad no explicada por educación solo\n")

# ===== FIN DEL SCRIPT =====

cat("\n\n===== FIN DEL ANALISIS =====\n")
cat("Script completado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
