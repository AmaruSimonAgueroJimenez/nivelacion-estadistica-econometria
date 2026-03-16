# Nivelacion Estadistica y Econometria — CICS, UDD (2026)

Material docente para los cursos de nivelacion del **Doctorado en Ciencias de la Complejidad Social** del Centro de Investigacion en Complejidad Social (CICS), Universidad del Desarrollo.

---

## Curso de Estadistica

| Sesion | Tema | Presentacion | Quarto | Script R |
|:------:|------|:------------:|:------:|:--------:|
| 1 | Estadistica Descriptiva | [PDF](clases_estadistica/sesion1_descriptiva.pdf) | [QMD](clases_estadistica/sesion1_descriptiva.qmd) | [R](clases_estadistica/sesion1_descriptiva.R) |
| 2 | Probabilidad y Teorema de Bayes | [PDF](clases_estadistica/sesion2_probabilidad.pdf) | [QMD](clases_estadistica/sesion2_probabilidad.qmd) | [R](clases_estadistica/sesion2_probabilidad.R) |
| 3 | Distribuciones de Probabilidad | [PDF](clases_estadistica/sesion3_distribuciones.pdf) | [QMD](clases_estadistica/sesion3_distribuciones.qmd) | [R](clases_estadistica/sesion3_distribuciones.R) |
| 4 | Inferencia Estadistica | [PDF](clases_estadistica/sesion4_inferencia.pdf) | [QMD](clases_estadistica/sesion4_inferencia.qmd) | [R](clases_estadistica/sesion4_inferencia.R) |
| 5 | Practica Integradora en R | [PDF](clases_estadistica/sesion5_practica.pdf) | [QMD](clases_estadistica/sesion5_practica.qmd) | [R](clases_estadistica/sesion5_practica.R) |

**Contenidos:** Medidas de tendencia central y dispersion, tipos de variables, visualizacion, probabilidad condicional, Teorema de Bayes, distribuciones discretas (Binomial, Poisson) y continuas (Normal, t-Student), Teorema del Limite Central, intervalos de confianza, pruebas de hipotesis, practica completa en R.

---

## Curso de Econometria

| Sesion | Tema | Presentacion |
|:------:|------|:------------:|
| 1 | Pruebas de Hipotesis | [PDF](clases_econometria/sesion1_hipotesis.pdf) |
| 2 | Regresion Lineal Simple | [PDF](clases_econometria/sesion2_regresion_simple.pdf) |
| 3 | Regresion Lineal Multiple | [PDF](clases_econometria/sesion3_regresion_multiple.pdf) |
| 4 | Practica Integradora en R (Parte 1) | [PDF](clases_econometria/sesion4_practica_r1.pdf) |
| 5 | Practica Integradora en R (Parte 2) | [PDF](clases_econometria/sesion5_practica_r2.pdf) |

**Contenidos:** Pruebas t, chi-cuadrado, valor-p, errores Tipo I/II, potencia estadistica, MCO, R-cuadrado, supuestos Gauss-Markov, sesgo de variable omitida, estructuras causales (confusora, collider, modificadora de efecto), modelos anidados, variables dummy, interacciones, multicolinealidad (VIF), pruebas F, diagnosticos de residuos, errores estandar robustos.

---

## Documentos adicionales

| Documento | PDF | Word |
|-----------|:---:|:----:|
| Syllabus Estadistica | [PDF](Syllabus_Nivelacion_Estadistica_2026.pdf) | [DOCX](Syllabus_Nivelacion_Estadistica_2026.docx) |
| Syllabus Econometria | [PDF](Syllabus_Nivelacion_Econometria_2026.pdf) | [DOCX](Syllabus_Nivelacion_Econometria_2026.docx) |
| Prueba Diagnostica | [PDF](Prueba_Diagnostica_Nivelacion_2026_v2.pdf) | [DOCX](Prueba_Diagnostica_Nivelacion_2026.docx) |
| Pauta de Correccion | [PDF](Pauta_Correccion_Diagnostica_2026_v2.pdf) | — |

---

## Generacion de figuras

Las figuras se generan con Python (`numpy`, `matplotlib`, `scipy`) y se guardan como PDF vectorial para Beamer.

```bash
cd clases_estadistica && python3 generar_figuras.py   # 15 figuras
cd clases_econometria && python3 generar_figuras.py   # 18 figuras
```

---

## Autor

**Amaru Aguero**
Doctorado en Ciencias de la Complejidad Social — CICS, Universidad del Desarrollo
