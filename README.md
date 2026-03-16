# Nivelacion Estadistica y Econometria — CICS, UDD (2026)

Material docente para los cursos de nivelacion del **Doctorado en Ciencias de la Complejidad Social** del Centro de Investigacion en Complejidad Social (CICS), Universidad del Desarrollo.

Presentaciones en LaTeX Beamer con figuras generadas programaticamente en Python y scripts de practica en R.

---

## Estructura del repositorio

```
Nivelaciones/
|-- clases_estadistica/          # Curso de Estadistica (5 sesiones)
|   |-- sesion1_descriptiva.tex
|   |-- sesion2_probabilidad.tex
|   |-- sesion3_distribuciones.tex
|   |-- sesion4_inferencia.tex
|   |-- sesion5_practica.tex
|   |-- sesion1_descriptiva.R    # Scripts de practica en R
|   |-- sesion2_probabilidad.R
|   |-- sesion3_distribuciones.R
|   |-- sesion4_inferencia.R
|   |-- sesion5_practica.R
|   |-- generar_figuras.py       # Genera las 15 figuras del curso
|   +-- figuras/                 # PDFs generados automaticamente
|
|-- clases_econometria/          # Curso de Econometria (5 sesiones)
|   |-- sesion1_hipotesis.tex
|   |-- sesion2_regresion_simple.tex
|   |-- sesion3_regresion_multiple.tex
|   |-- sesion4_practica_r1.tex
|   |-- sesion5_practica_r2.tex
|   |-- generar_figuras.py       # Genera las 18 figuras del curso
|   +-- figuras/                 # PDFs generados automaticamente
|
|-- Syllabus_Nivelacion_Estadistica_2026.docx
|-- Syllabus_Nivelacion_Econometria_2026.docx
|-- Prueba_Diagnostica_Nivelacion_2026.pdf
+-- Pauta_Correccion_Diagnostica_2026.pdf
```

---

## Curso de Estadistica

| Sesion | Tema | Archivo | Slides |
|:------:|------|---------|:------:|
| 1 | Estadistica Descriptiva | [`sesion1_descriptiva.tex`](clases_estadistica/sesion1_descriptiva.tex) | 30 |
| 2 | Probabilidad y Teorema de Bayes | [`sesion2_probabilidad.tex`](clases_estadistica/sesion2_probabilidad.tex) | 25 |
| 3 | Distribuciones de Probabilidad | [`sesion3_distribuciones.tex`](clases_estadistica/sesion3_distribuciones.tex) | 25 |
| 4 | Inferencia Estadistica | [`sesion4_inferencia.tex`](clases_estadistica/sesion4_inferencia.tex) | 20 |
| 5 | Practica Integradora en R | [`sesion5_practica.tex`](clases_estadistica/sesion5_practica.tex) | 18 |

**Contenidos:** Medidas de tendencia central y dispersion, tipos de variables, visualizacion (histogramas, boxplots), probabilidad condicional, Teorema de Bayes, distribuciones discretas (Binomial, Poisson) y continuas (Normal, t-Student), Teorema del Limite Central, intervalos de confianza, pruebas de hipotesis basicas, practica completa en R.

**Scripts R:** Cada sesion incluye un script `.R` con ejemplos reproducibles y ejercicios.

---

## Curso de Econometria

| Sesion | Tema | Archivo | Slides |
|:------:|------|---------|:------:|
| 1 | Pruebas de Hipotesis | [`sesion1_hipotesis.tex`](clases_econometria/sesion1_hipotesis.tex) | 27 |
| 2 | Regresion Lineal Simple | [`sesion2_regresion_simple.tex`](clases_econometria/sesion2_regresion_simple.tex) | 20 |
| 3 | Regresion Lineal Multiple | [`sesion3_regresion_multiple.tex`](clases_econometria/sesion3_regresion_multiple.tex) | 26 |
| 4 | Practica Integradora en R (Parte 1) | [`sesion4_practica_r1.tex`](clases_econometria/sesion4_practica_r1.tex) | 18 |
| 5 | Practica Integradora en R (Parte 2) | [`sesion5_practica_r2.tex`](clases_econometria/sesion5_practica_r2.tex) | 18 |

**Contenidos:** Pruebas t (una y dos muestras), chi-cuadrado, valor-p, errores Tipo I/II (matriz de decision), potencia estadistica, MCO, R-cuadrado, supuestos Gauss-Markov, sesgo de variable omitida, estructuras causales (confusora, collider, modificadora de efecto), modelos anidados, variables dummy, interacciones, multicolinealidad (VIF), pruebas F, diagnosticos de residuos, errores estandar robustos, `stargazer`.

---

## Compilacion

### Requisitos

- **LaTeX:** `pdflatex` con paquetes `beamer`, `amsmath`, `tikz`, `listings`, `booktabs`
- **Python 3:** `numpy`, `matplotlib`, `scipy` (para generar figuras)
- **R:** Base R con paquetes `car`, `pwr`, `stargazer` (para sesiones practicas)

### Generar figuras

```bash
# Estadistica (15 figuras)
cd clases_estadistica && python3 generar_figuras.py

# Econometria (18 figuras)
cd clases_econometria && python3 generar_figuras.py
```

### Compilar presentaciones

```bash
cd clases_estadistica && pdflatex sesion1_descriptiva.tex
cd clases_econometria && pdflatex sesion1_hipotesis.tex
```

---

## Formato de las presentaciones

Todas las presentaciones usan un formato Beamer consistente: tema Madrid con esquema de colores whale, aspect ratio 16:9, branding institucional UDD/CICS, y figuras vectoriales en PDF generadas programaticamente.

---

## Documentos adicionales

- [Syllabus Estadistica](Syllabus_Nivelacion_Estadistica_2026.docx)
- [Syllabus Econometria](Syllabus_Nivelacion_Econometria_2026.docx)
- [Prueba Diagnostica](Prueba_Diagnostica_Nivelacion_2026.pdf)
- [Pauta de Correccion](Pauta_Correccion_Diagnostica_2026.pdf)

---

## Autor

**Amaru Aguero**
Doctorado en Ciencias de la Complejidad Social — CICS, Universidad del Desarrollo

---

## Licencia

Material academico de uso interno. Todos los derechos reservados.
