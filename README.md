# Nivelación Estadística y Econometría — UDD (2026)

Material docente para los cursos de nivelación en **estadística y probabilidades** y **econometría** del **Doctorado en Políticas Públicas** y del **Doctorado en Ciencias de la Complejidad Social** (CICS), Universidad del Desarrollo.

Cada sesión se genera desde un único archivo Quarto (`.qmd`) que produce dos salidas:

- **Slides interactivas** (`.html`, reveal.js): figuras interactivas con plotly (tooltips, zoom), autocontenidas — se abren en cualquier navegador sin instalación.
- **Presentación PDF** (`.pdf`, Beamer 16:9): misma secuencia de slides con figuras estáticas, lista para imprimir o proyectar.

---

## Curso: Fundamentos de Probabilidad y Estadística

Nivelación intensiva de 2 días (4 horas por día, 2 bloques diarios) previa a la asignatura **Métodos Cuantitativos**, para el Doctorado en Ciencias de la Complejidad Social y el Doctorado en Políticas Públicas. Énfasis en formalidad matemática con demostraciones y ejemplos numéricos resueltos paso a paso.

| Sesión | Tema | Slides interactivas | PDF | Fuente | Script R |
|:------:|------|:---:|:---:|:---:|:---:|
| 1 (Día 1) | Fundamentos de Probabilidad | [HTML](clases_fundamentos/sesion1_probabilidad.html) | [PDF](clases_fundamentos/sesion1_probabilidad.pdf) | [QMD](clases_fundamentos/sesion1_probabilidad.qmd) | [R](clases_fundamentos/sesion1_probabilidad.R) |
| 2 (Día 1) | Variables Aleatorias y Distribuciones | [HTML](clases_fundamentos/sesion2_variables_aleatorias.html) | [PDF](clases_fundamentos/sesion2_variables_aleatorias.pdf) | [QMD](clases_fundamentos/sesion2_variables_aleatorias.qmd) | [R](clases_fundamentos/sesion2_variables_aleatorias.R) |
| 3 (Día 2) | Muestreo e Inferencia Estadística | [HTML](clases_fundamentos/sesion3_muestreo_inferencia.html) | [PDF](clases_fundamentos/sesion3_muestreo_inferencia.pdf) | [QMD](clases_fundamentos/sesion3_muestreo_inferencia.qmd) | [R](clases_fundamentos/sesion3_muestreo_inferencia.R) |
| 4 (Día 2) | Regresión Lineal y Lectura de Resultados | [HTML](clases_fundamentos/sesion4_regresion.html) | [PDF](clases_fundamentos/sesion4_regresion.pdf) | [QMD](clases_fundamentos/sesion4_regresion.qmd) | [R](clases_fundamentos/sesion4_regresion.R) |

**Contenidos:** axiomas de Kolmogorov y propiedades demostradas, probabilidad marginal/conjunta/condicional, ley de probabilidad total y Teorema de Bayes derivado paso a paso, variables aleatorias (PMF, CDF, densidad), esperanza/varianza/covarianza con derivaciones, distribución normal y cálculo de probabilidades de intervalos, distribuciones muestrales (demostraciones de E[X̄]=μ y Var(X̄)=σ²/n), TCL, propiedades de estimadores (sesgo, eficiencia, consistencia), intervalos de confianza derivados, contraste de hipótesis (errores I/II, valor-p, potencia), derivación completa de MCO con ejemplo a mano, y lectura guiada de tablas de regresión.

---

## Curso de Estadística y Probabilidades

| Sesión | Tema | Slides interactivas | PDF | Fuente | Script R |
|:------:|------|:---:|:---:|:---:|:---:|
| 1 | Estadística Descriptiva | [HTML](clases_estadistica/sesion1_descriptiva.html) | [PDF](clases_estadistica/sesion1_descriptiva.pdf) | [QMD](clases_estadistica/sesion1_descriptiva.qmd) | [R](clases_estadistica/sesion1_descriptiva.R) |
| 2 | Probabilidad y Teorema de Bayes | [HTML](clases_estadistica/sesion2_probabilidad.html) | [PDF](clases_estadistica/sesion2_probabilidad.pdf) | [QMD](clases_estadistica/sesion2_probabilidad.qmd) | [R](clases_estadistica/sesion2_probabilidad.R) |
| 3 | Distribuciones de Probabilidad | [HTML](clases_estadistica/sesion3_distribuciones.html) | [PDF](clases_estadistica/sesion3_distribuciones.pdf) | [QMD](clases_estadistica/sesion3_distribuciones.qmd) | [R](clases_estadistica/sesion3_distribuciones.R) |
| 4 | Inferencia Estadística | [HTML](clases_estadistica/sesion4_inferencia.html) | [PDF](clases_estadistica/sesion4_inferencia.pdf) | [QMD](clases_estadistica/sesion4_inferencia.qmd) | [R](clases_estadistica/sesion4_inferencia.R) |
| 5 | Práctica Integradora en R | [HTML](clases_estadistica/sesion5_practica.html) | [PDF](clases_estadistica/sesion5_practica.pdf) | [QMD](clases_estadistica/sesion5_practica.qmd) | [R](clases_estadistica/sesion5_practica.R) |

**Contenidos:** tipos de variables y escalas de medición, medidas de tendencia central y dispersión, outliers y z-scores, visualización con ggplot2, probabilidad condicional y Teorema de Bayes, distribuciones discretas (Bernoulli, Binomial, Poisson) y continuas (Normal, t de Student), Teorema del Límite Central, intervalos de confianza, pruebas de hipótesis, tamaño del efecto, y una práctica completa en R de principio a fin.

---

## Curso de Econometría

| Sesión | Tema | Slides interactivas | PDF | Fuente | Script R |
|:------:|------|:---:|:---:|:---:|:---:|
| 1 | Pruebas de Hipótesis | [HTML](clases_econometria/sesion1_hipotesis.html) | [PDF](clases_econometria/sesion1_hipotesis.pdf) | [QMD](clases_econometria/sesion1_hipotesis.qmd) | [R](clases_econometria/sesion1_hipotesis.R) |
| 2 | Regresión Lineal Simple | [HTML](clases_econometria/sesion2_regresion_simple.html) | [PDF](clases_econometria/sesion2_regresion_simple.pdf) | [QMD](clases_econometria/sesion2_regresion_simple.qmd) | [R](clases_econometria/sesion2_regresion_simple.R) |
| 3 | Regresión Lineal Múltiple | [HTML](clases_econometria/sesion3_regresion_multiple.html) | [PDF](clases_econometria/sesion3_regresion_multiple.pdf) | [QMD](clases_econometria/sesion3_regresion_multiple.qmd) | [R](clases_econometria/sesion3_regresion_multiple.R) |
| 4 | Práctica Integradora en R (Parte 1) | [HTML](clases_econometria/sesion4_practica_r1.html) | [PDF](clases_econometria/sesion4_practica_r1.pdf) | [QMD](clases_econometria/sesion4_practica_r1.qmd) | [R](clases_econometria/sesion4_practica_r1.R) |
| 5 | Práctica Integradora en R (Parte 2) | [HTML](clases_econometria/sesion5_practica_r2.html) | [PDF](clases_econometria/sesion5_practica_r2.pdf) | [QMD](clases_econometria/sesion5_practica_r2.qmd) | [R](clases_econometria/sesion5_practica_r2.R) |

**Contenidos:** valor-p y sus malinterpretaciones, errores Tipo I/II, potencia estadística, pruebas t / chi-cuadrado / proporciones, p-hacking y correcciones múltiples, MCO y supuestos de Gauss-Markov, formas funcionales (log), R², sesgo de variable omitida, estructuras causales con DAGs (confusora, collider, mediadora), variables dummy e interacciones, prueba F y modelos anidados, multicolinealidad (VIF), diagnósticos de residuos, Breusch-Pagan y errores estándar robustos, tablas de modelos con modelsummary, y dos prácticas integradoras completas.

---

## Documentos adicionales

**Fundamentos de Probabilidad y Estadística (Doctorados en Ciencias de la Complejidad Social y en Políticas Públicas)**

| Documento | PDF | Fuente |
|-----------|:---:|:---:|
| Syllabus Fundamentos | [PDF](Syllabus_Fundamentos_2026.pdf) | [QMD](Syllabus_Fundamentos_2026.qmd) |
| Prueba Diagnóstica Fundamentos | [PDF](Prueba_Diagnostica_Fundamentos_2026.pdf) | [QMD](Prueba_Diagnostica_Fundamentos_2026.qmd) |
| Pauta de Corrección Fundamentos | [PDF](Pauta_Correccion_Fundamentos_2026.pdf) | [QMD](Pauta_Correccion_Fundamentos_2026.qmd) |

**Nivelación Estadística y Econometría (CICS)**

| Documento | PDF | Fuente |
|-----------|:---:|:---:|
| Syllabus Estadística | [PDF](Syllabus_Nivelacion_Estadistica_2026.pdf) | [QMD](Syllabus_Nivelacion_Estadistica_2026.qmd) |
| Syllabus Econometría | [PDF](Syllabus_Nivelacion_Econometria_2026.pdf) | [QMD](Syllabus_Nivelacion_Econometria_2026.qmd) |
| Prueba Diagnóstica | [PDF](Prueba_Diagnostica_Nivelacion_2026.pdf) | [QMD](Prueba_Diagnostica_Nivelacion_2026.qmd) |
| Pauta de Corrección | [PDF](Pauta_Correccion_Diagnostica_2026.pdf) | [QMD](Pauta_Correccion_Diagnostica_2026.qmd) |

---

## Cómo regenerar el material

Requisitos: [Quarto](https://quarto.org) ≥ 1.9, R ≥ 4.5 con `tidyverse`, `plotly`, `gapminder`, `wooldridge`, `broom`, `car`, `lmtest`, `sandwich`, `modelsummary`, `moments`, `scales`, `knitr`, y TinyTeX (o LaTeX equivalente) para el PDF.

```bash
# Renderiza ambos formatos (HTML interactivo + PDF Beamer) de una sesión:
cd clases_estadistica
quarto render sesion1_descriptiva.qmd

# Regenera el script R desde el QMD:
Rscript -e 'knitr::purl("sesion1_descriptiva.qmd", output = "sesion1_descriptiva.R", documentation = 1)'
```

Las figuras estáticas de apoyo (diagramas conceptuales) se generan con Python (`numpy`, `matplotlib`, `scipy`) en PDF y PNG:

```bash
cd clases_estadistica && python3 generar_figuras.py   # 15 figuras
cd clases_econometria && python3 generar_figuras.py   # 18 figuras
```

**Notas de arquitectura:**

- Un solo `.qmd` por sesión es la fuente de la verdad; los antiguos `.tex` de Beamer fueron reemplazados por este pipeline (siguen disponibles en el historial de git).
- El helper `interactivo()` definido en cada QMD envuelve los ggplots con `plotly::ggplotly()` solo en la salida HTML; en el PDF quedan estáticos.
- La hoja de estilo de las slides es `estilo_slides.scss` (una copia por curso).

---

## Autor

**Amaru Agüero Jiménez** (a.agueroj@udd.cl)
