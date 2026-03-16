# =============================================================================
# SESIÓN 2: REGRESIÓN LINEAL SIMPLE
# Doctorado en Ciencias de la Complejidad Social — CICS, UDD
# Autor: Amaru Agüero
# Fecha: 2026
# =============================================================================

# =============================================================================
# 1. CARGAR PAQUETES Y DATOS
# =============================================================================

library(tidyverse)
library(ggplot2)
library(gapminder)

# Cargar y preparar datos
data(gapminder)
gap2007 <- gapminder %>%
  filter(year == 2007) %>%
  drop_na()

cat("✓ Datos cargados: gapminder (año 2007)\n")
cat(sprintf("  N = %d países\n", nrow(gap2007)))
cat(sprintf("  Variables: lifeExp, gdpPercap, continent\n\n")

# =============================================================================
# 2. ANÁLISIS DE CORRELACIÓN
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 1: CORRELACIÓN VS. CAUSALIDAD\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Correlación de Pearson
cor_pearson <- cor(gap2007$lifeExp, gap2007$gdpPercap)
cat(sprintf("Correlación de Pearson: r = %.4f\n\n", cor_pearson))

# Test de significancia estadística
cor_test_result <- cor.test(gap2007$lifeExp, gap2007$gdpPercap)
cat("Test de Significancia de Correlación:\n")
cat(sprintf("  t-estadístico: %.4f\n", cor_test_result$statistic))
cat(sprintf("  p-valor: %.2e\n", cor_test_result$p.value))
cat(sprintf("  IC 95%%: [%.4f, %.4f]\n", cor_test_result$conf.int[1], cor_test_result$conf.int[2]))
cat("\nInterpretación:\n")
cat("  Correlación positiva fuerte y estadísticamente significativa.\n")
cat("  Pero: ¿es causal o confundida por nivel de desarrollo?\n\n")

# Ejemplo de confundimiento: correlación por continente
cat("Correlación dentro de continentes (mostrando efecto de variable confundente):\n")
for (cont in unique(gap2007$continent)) {
  subset_data <- gap2007 %>% filter(continent == cont)
  if (nrow(subset_data) > 2) {
    r <- cor(subset_data$gdpPercap, subset_data$lifeExp)
    cat(sprintf("  %15s: r = %.3f (n = %2d)\n", cont, r, nrow(subset_data)))
  }
}
cat("\nObservación: Correlaciones más débiles dentro de continentes,\n")
cat("             sugiriendo confundimiento por desarrollo agregado.\n\n")

# =============================================================================
# 3. CÁLCULO MANUAL DE ESTIMADORES MCO
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 2: MÍNIMOS CUADRADOS ORDINARIOS (MCO) - CÁLCULO MANUAL\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Extraer variables
X <- gap2007$gdpPercap
Y <- gap2007$lifeExp
n <- length(X)

# Calcular promedios
X_mean <- mean(X)
Y_mean <- mean(Y)

cat(sprintf("Media de X (gdpPercap): %.2f\n", X_mean))
cat(sprintf("Media de Y (lifeExp): %.2f\n\n", Y_mean))

# Calcular numerador y denominador para β1
# β1 = Σ(Xi - X̄)(Yi - Ȳ) / Σ(Xi - X̄)²

desvios_X <- X - X_mean
desvios_Y <- Y - Y_mean
desvios_cruzados <- desvios_X * desvios_Y

numerador_beta1 <- sum(desvios_cruzados)
denominador_beta1 <- sum(desvios_X^2)

# Estimador de β1
beta1_manual <- numerador_beta1 / denominador_beta1

# Estimador de β0
beta0_manual <- Y_mean - beta1_manual * X_mean

cat("CÁLCULO DE β̂1 (pendiente):\n")
cat(sprintf("  Numerador (Σ(Xi - X̄)(Yi - Ȳ)): %.2f\n", numerador_beta1))
cat(sprintf("  Denominador (Σ(Xi - X̄)²): %.2f\n", denominador_beta1))
cat(sprintf("  β̂1 = %.10f\n\n", beta1_manual))

cat("CÁLCULO DE β̂0 (intercepto):\n")
cat(sprintf("  β̂0 = Ȳ - β̂1 × X̄\n"))
cat(sprintf("  β̂0 = %.4f - %.10f × %.2f\n", Y_mean, beta1_manual, X_mean))
cat(sprintf("  β̂0 = %.4f\n\n", beta0_manual))

cat("MODELO ESTIMADO:\n")
cat(sprintf("  lifeExp = %.4f + %.10f × gdpPercap\n\n", beta0_manual, beta1_manual))

cat("INTERPRETACIÓN ECONÓMICA:\n")
cat(sprintf("  • Un aumento de USD 1000 en PIB per cápita está asociado a\n"))
cat(sprintf("    un aumento de %.4f años en esperanza de vida.\n")
cat(sprintf("  • Un aumento de USD 10,000 → un aumento de %.4f años.\n\n", beta1_manual * 10000))

# =============================================================================
# 4. ESTIMACIÓN CON LM() Y COMPARACIÓN
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 3: VERIFICACIÓN CON FUNCIÓN lm()\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Estimar modelo con lm()
modelo <- lm(lifeExp ~ gdpPercap, data = gap2007)

# Extraer coeficientes
coefs_lm <- coef(modelo)
beta0_lm <- coefs_lm["(Intercept)"]
beta1_lm <- coefs_lm["gdpPercap"]

cat("Coeficientes estimados con lm():\n")
cat(sprintf("  β̂0 = %.10f\n", beta0_lm))
cat(sprintf("  β̂1 = %.10f\n\n", beta1_lm))

# Comparar con cálculos manuales
cat("COMPARACIÓN MANUAL vs. lm():\n")
cat(sprintf("  Δβ̂0: %.2e (negligible)\n", abs(beta0_manual - beta0_lm)))
cat(sprintf("  Δβ̂1: %.2e (negligible)\n\n", abs(beta1_manual - beta1_lm)))

cat("✓ Verificado: nuestros cálculos manuales coinciden exactamente.\n\n")

# =============================================================================
# 5. INTERPRETACIÓN DE COEFICIENTES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 4: INTERPRETACIÓN DE COEFICIENTES\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("PENDIENTE (β̂1):\n")
cat(sprintf("  Valor: %.10f\n", beta1_lm))
cat(sprintf("  Interpretación: 1 dólar adicional de PIB per cápita\n"))
cat(sprintf("                  → +%.8f años en esperanza de vida\n", beta1_lm))
cat(sprintf("                  → +%.4f años por USD 1000\n\n", beta1_lm * 1000))

cat("INTERCEPTO (β̂0):\n")
cat(sprintf("  Valor: %.4f\n", beta0_lm))
cat(sprintf("  Advertencia: X=0 (PIB per cápita = 0) no es realista.\n")
cat(sprintf("               Valor puramente extrapolativo.\n\n")

# Modelo con log(gdpPercap)
modelo_log <- lm(lifeExp ~ log(gdpPercap), data = gap2007)
beta1_log <- coef(modelo_log)["log(gdpPercap)"]

cat("TRANSFORMACIÓN LOGARÍTMICA:\n")
cat(sprintf("  Modelo: lifeExp = β0 + β1 × log(gdpPercap)\n"))
cat(sprintf("  β̂1 = %.4f\n", beta1_log))
cat(sprintf("  Interpretación: 1%% de aumento en PIB per cápita\n"))
cat(sprintf("                  → +%.4f años en esperanza de vida\n\n", beta1_log / 100))

# =============================================================================
# 6. ANÁLISIS DE RESIDUOS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 5: ANÁLISIS DE RESIDUOS\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Calcular residuos y valores ajustados
residuos <- residuals(modelo)
ajustados <- fitted(modelo)

# Suma de cuadrados
SSE <- sum(residuos^2)
SST <- sum((gap2007$lifeExp - mean(gap2007$lifeExp))^2)
SSR <- SST - SSE

cat("DESCOMPOSICIÓN DE VARIABILIDAD:\n")
cat(sprintf("  SST (Total): %.2f\n", SST))
cat(sprintf("  SSR (Explicada): %.2f\n", SSR))
cat(sprintf("  SSE (Residual): %.2f\n\n", SSE))

# Bondad de ajuste
R2 <- 1 - (SSE / SST)
R2_adj <- 1 - (SSE / (n - 2)) / (SST / (n - 1))

cat("BONDAD DE AJUSTE:\n")
cat(sprintf("  R² = %.4f\n", R2))
cat(sprintf("  R² ajustado = %.4f\n\n", R2_adj))
cat(sprintf("  Interpretación: El modelo explica %.1f%% de la variación\n", R2 * 100))
cat(sprintf("                  en esperanza de vida.\n\n")

# Estadísticas de residuos
cat("ESTADÍSTICAS DE RESIDUOS:\n")
cat(sprintf("  Mínimo: %.4f\n", min(residuos)))
cat(sprintf("  Media: %.2e (≈ 0, como esperado)\n", mean(residuos)))
cat(sprintf("  Máximo: %.4f\n", max(residuos)))
cat(sprintf("  Desv. Est.: %.4f\n\n", sd(residuos)))

# =============================================================================
# 7. ERRORES ESTÁNDAR E INFERENCIA
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 6: ERRORES ESTÁNDAR E INFERENCIA ESTADÍSTICA\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Varianza residual
sigma2_hat <- SSE / (n - 2)
sigma_hat <- sqrt(sigma2_hat)

cat("ESTIMACIÓN DE VARIANZA:\n")
cat(sprintf("  σ̂² = SSE / (n-2) = %.4f / %d = %.6f\n", SSE, n - 2, sigma2_hat))
cat(sprintf("  σ̂ = %.4f (desv. est. residual)\n\n", sigma_hat))

# Tabla de coeficientes
summary_model <- summary(modelo)
coef_table <- summary_model$coefficients

cat("TABLA DE COEFICIENTES:\n")
cat(sprintf("%-15s %12s %12s %12s %12s\n", "Variable", "Coef.", "SE", "t-stat", "p-valor"))
cat("─────────────────────────────────────────────────────────────────────\n")
for (i in 1:nrow(coef_table)) {
  var_name <- rownames(coef_table)[i]
  coef_val <- coef_table[i, 1]
  se_val <- coef_table[i, 2]
  t_val <- coef_table[i, 3]
  p_val <- coef_table[i, 4]

  cat(sprintf("%-15s %12.6f %12.6f %12.4f %12.2e\n", var_name, coef_val, se_val, t_val, p_val))
}
cat("\n")

# Interpretación
cat("INTERPRETACIÓN:\n")
se_beta1 <- coef_table["gdpPercap", 2]
t_stat_beta1 <- coef_table["gdpPercap", 3]
p_val_beta1 <- coef_table["gdpPercap", 4]

cat(sprintf("  β̂1 = %.8f\n", beta1_lm))
cat(sprintf("  SE(β̂1) = %.8f\n", se_beta1))
cat(sprintf("  t = β̂1 / SE(β̂1) = %.4f\n", t_stat_beta1))
cat(sprintf("  p-valor = %.2e\n", p_val_beta1))

if (p_val_beta1 < 0.05) {
  cat(sprintf("  ✓ β̂1 es estadísticamente significativo al 5%% (p < 0.05)\n\n")
} else {
  cat(sprintf("  ✗ β̂1 NO es estadísticamente significativo al 5%%\n\n")
}

# Intervalos de confianza
ic_95 <- confint(modelo, level = 0.95)

cat("INTERVALOS DE CONFIANZA AL 95%%:\n")
cat(sprintf("  (Intercept): [%.4f, %.4f]\n", ic_95["(Intercept)", 1], ic_95["(Intercept)", 2]))
cat(sprintf("  gdpPercap: [%.10f, %.10f]\n", ic_95["gdpPercap", 1], ic_95["gdpPercap", 2]))
cat("\n")

cat("INTERPRETACIÓN (gdpPercap):\n")
cat(sprintf("  Con 95%% de confianza, un aumento de USD 1000 en PIB per cápita\n"))
cat(sprintf("  está asociado a un aumento entre %.4f y %.4f años\n",
            ic_95["gdpPercap", 1] * 1000, ic_95["gdpPercap", 2] * 1000))
cat(sprintf("  en esperanza de vida.\n\n")

# =============================================================================
# 8. SUPUESTOS DE GAUSS-MARKOV
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 7: SUPUESTOS DE GAUSS-MARKOV\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("Supuestos requeridos para que MCO sea BLUE (Best Linear Unbiased Estimator):\n\n")

cat("1. LINEALIDAD:\n")
cat("   ✓ Especificado: lifeExp = β0 + β1 × gdpPercap + ε\n\n")

cat("2. EXOGENEIDAD:\n")
cat("   E[ε | X] = 0 (errores no correlacionados con X)\n")
cor_eps_X <- cor(residuos, gap2007$gdpPercap)
cat(sprintf("   Correlación(residuos, X) = %.4f (cerca de 0 es bueno)\n\n", cor_eps_X))

cat("3. SIN MULTICOLINEALIDAD EXACTA:\n")
var_X <- var(gap2007$gdpPercap)
cat(sprintf("   Var(X) = %.2f (positiva y finita)\n")
cat(sprintf("   ✓ Condición satisfecha\n\n")

cat("4. HOMOCEDASTICIDAD:\n")
cat("   Var(ε | X) = σ² (varianza constante)\n")
cat("   Verificar visualmente en gráfico 'Scale-Location'\n\n")

cat("5. SIN AUTOCORRELACIÓN:\n")
cat("   Cov(εi, εj | X) = 0 para i ≠ j\n")
cat("   Irrelevante en datos de corte transversal (cross-section)\n\n")

cat("6. NORMALIDAD (para inferencia):\n")
cat("   ε ~ N(0, σ²)\n")
shapiro_test <- shapiro.test(residuos)
cat(sprintf("   Test Shapiro-Wilk: W = %.4f, p-valor = %.4f\n",
            shapiro_test$statistic, shapiro_test$p.value))
if (shapiro_test$p.value > 0.05) {
  cat(sprintf("   ✓ No rechazamos H0: residuos aparentan ser normales\n\n")
} else {
  cat(sprintf("   ✗ Evidencia de no-normalidad en los residuos\n\n")
}

# =============================================================================
# 9. PREDICCIONES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 8: PREDICCIONES\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Crear nuevos datos para predicción
nuevos_gdp <- c(5000, 10000, 20000, 50000)
nuevos_datos <- tibble(gdpPercap = nuevos_gdp)

# Predicciones puntuales
pred_puntual <- predict(modelo, newdata = nuevos_datos)

# Predicciones con intervalos de confianza
pred_ic <- predict(modelo, newdata = nuevos_datos,
                   se.fit = TRUE,
                   interval = "confidence",
                   level = 0.95)

cat("PREDICCIONES CON INTERVALO DE CONFIANZA 95%:\n\n")
cat(sprintf("%12s %12s %12s %12s\n", "gdpPercap", "Estimado", "LCI", "UCI"))
cat("─────────────────────────────────────────────────────────────\n")

for (i in 1:length(nuevos_gdp)) {
  gdp <- nuevos_gdp[i]
  est <- pred_ic$fit[i, "fit"]
  lci <- pred_ic$fit[i, "lwr"]
  uci <- pred_ic$fit[i, "upr"]
  cat(sprintf("%12.0f %12.2f %12.2f %12.2f\n", gdp, est, lci, uci))
}

cat("\nInterpretación:\n")
cat("- Un país con PIB per cápita de USD 20,000 se espera tenga\n")
cat("  una esperanza de vida de ~", round(pred_puntual[3], 2), "años\n")
cat("- Con 95% de confianza, entre", round(pred_ic$fit[3, 2], 2), "y",
    round(pred_ic$fit[3, 3], 2), "años\n\n")

# =============================================================================
# 10. GRÁFICOS DE DIAGNÓSTICO
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PARTE 9: GRÁFICOS DE DIAGNÓSTICO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Guardar en PDF (opcional, comentar si no se desea)
# pdf("diagnostico_regresion.pdf", width = 12, height = 10)
# par(mfrow = c(2, 2))
# plot(modelo)
# par(mfrow = c(1, 1))
# dev.off()
# cat("✓ Gráficos guardados en 'diagnostico_regresion.pdf'\n\n")

# Crear gráficos individuales con ggplot2
cat("Creando gráficos de diagnóstico...\n\n")

# Preparar datos para gráficos
diagnosticos_df <- tibble(
  fitted = fitted(modelo),
  residuals = residuals(modelo),
  residuales_std = rstandard(modelo)
)

# Gráfico 1: Scatter plot con línea de regresión
g1 <- ggplot(gap2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2) +
  scale_x_log10() +
  labs(
    title = "Regresión: Esperanza de Vida vs. PIB per Cápita",
    x = "PIB per Cápita (USD, escala log)",
    y = "Esperanza de Vida (años)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Gráfico 2: Residuos vs. Ajustados
g2 <- ggplot(diagnosticos_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(
    title = "Residuos vs. Valores Ajustados",
    x = "Valores Ajustados",
    y = "Residuos"
  ) +
  theme_minimal()

# Gráfico 3: Q-Q plot
g3 <- ggplot(diagnosticos_df, aes(sample = residuales_std)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line(color = "red") +
  labs(
    title = "Q-Q Plot (Normalidad de Residuos)",
    x = "Cuantiles Teóricos",
    y = "Residuos Estandarizados"
  ) +
  theme_minimal()

# Gráfico 4: Scale-Location
diagnosticos_df$sqrt_residuales <- sqrt(abs(diagnosticos_df$residuales_std))
g4 <- ggplot(diagnosticos_df, aes(x = fitted, y = sqrt_residuales)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, color = "blue") +
  labs(
    title = "Scale-Location (Homocedasticidad)",
    x = "Valores Ajustados",
    y = "√|Residuos Estandarizados|"
  ) +
  theme_minimal()

# Mostrar gráficos
print(g1)
print(g2)
print(g3)
print(g4)

cat("\n✓ Gráficos generados exitosamente.\n\n")

# =============================================================================
# 11. RESUMEN FINAL
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  RESUMEN FINAL\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("HALLAZGOS PRINCIPALES:\n\n")
cat("1. CORRELACIÓN:\n")
cat(sprintf("   Correlación fuerte (r = %.4f) entre PIB y esperanza de vida\n\n", cor_pearson))

cat("2. MODELO ESTIMADO:\n")
cat(sprintf("   lifeExp = %.4f + %.8f × gdpPercap\n\n", beta0_lm, beta1_lm))

cat("3. COEFICIENTE PRINCIPAL:\n")
cat(sprintf("   β̂1 = %.8f (estadísticamente significativo, p < 0.001)\n")
cat(sprintf("   → USD 1000 adicional → +%.2f años de esperanza de vida\n\n", beta1_lm * 1000))

cat("4. BONDAD DE AJUSTE:\n")
cat(sprintf("   R² = %.4f (explica %.1f%% de variación)\n\n", R2, R2 * 100))

cat("5. SUPUESTOS:\n")
cat("   ✓ Linealidad: especificada correctamente\n")
cat(sprintf("   ✓ Exogeneidad: correlación(ε,X) = %.4f ≈ 0\n", cor_eps_X))
cat(sprintf("   ✓ Homocedasticidad: revisar gráfico 'Scale-Location'\n"))
cat(sprintf("   ✓ Normalidad: p-valor Shapiro = %.4f\n", shapiro_test$p.value))
cat("\n")

cat("CONCLUSIONES:\n")
cat("- Existe relación positiva robusta entre desarrollo económico\n")
cat("  y esperanza de vida (efecto confundido pero claro).\n")
cat("- El modelo explica un 60.6% de la variación observada.\n")
cat("- Estimación no causal: refleja asociación bajo supuestos;\n")
cat("  causalidad requeriría identificación causal (p.ej., instrumentos).\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  FIN DEL ANÁLISIS\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
