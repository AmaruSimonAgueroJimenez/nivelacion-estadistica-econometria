================================================================================
FIGURAS GENERADAS PARA SESIONES DE ESTADÍSTICA EN BEAMER
================================================================================

Script: generar_figuras.py
Fecha de generación: 2026-03-13
Ubicación: /sessions/sweet-adoring-franklin/mnt/Nivelaciones/clases_estadistica/figuras/

ESPECIFICACIONES:
- Formato: PDF (optimizado para Beamer)
- Dimensiones: 5 x 3.5 pulgadas, 150 DPI
- Estilo: seaborn-v0_8-whitegrid
- Colores: #1F4E79 (primario), #2E86C1 (secundario), #E74C3C (acento)
- Idioma: Español
- Reproducibilidad: numpy.random.seed(42)

================================================================================
SESIÓN 1: ESTADÍSTICA DESCRIPTIVA
================================================================================

1. histograma_ingresos.pdf
   - Histograma de ingresos (distribución lognormal)
   - Líneas de media y mediana etiquetadas
   - Tamaño: 22K

2. boxplot_comparativo.pdf
   - Gráficos de caja lado a lado
   - Comparación de ingresos por 3 regiones: Norte, Centro, Sur
   - Tamaño: 21K

3. scatter_educacion_ingreso.pdf
   - Scatter plot: años de educación vs ingreso
   - Incluye línea de tendencia y coeficiente de correlación
   - Tamaño: 27K

4. tipos_variables.pdf
   - Diagrama de taxonomía de variables
   - División: Cualitativas (Nominal, Ordinal) y Cuantitativas (Discreta, Continua)
   - Ejemplos para cada categoría
   - Tamaño: 27K

================================================================================
SESIÓN 2: PROBABILIDAD
================================================================================

5. diagrama_venn.pdf
   - Diagrama de Venn con dos conjuntos A y B
   - Muestra la fórmula: P(A∪B) = P(A) + P(B) - P(A∩B)
   - Regiones etiquetadas y coloreadas
   - Tamaño: 12K

6. bayes_ejemplo.pdf
   - Gráfico de barras apiladas: probabilidades prior vs posterior
   - Ejemplo de test médico (enfermo/sano antes y después)
   - Demostración del Teorema de Bayes
   - Tamaño: 25K

7. pmf_ejemplo.pdf
   - Función de masa de probabilidad (PMF)
   - Variable aleatoria discreta: distribución Binomial (n=10, p=0.6)
   - Gráfico de barras con valores de probabilidad
   - Tamaño: 22K

================================================================================
SESIÓN 3: DISTRIBUCIONES
================================================================================

8. distribucion_normal.pdf
   - Curva normal con áreas sombreadas
   - Regla 68-95-99.7 ilustrada
   - Etiquetados: ±1σ (68%), ±2σ (95%)
   - Tamaño: 33K

9. tlc_simulacion.pdf
   - Simulación del Teorema del Límite Central en 4 paneles
   - Panel 1: Distribución original (uniforme)
   - Paneles 2-4: Distribuciones de medias para n=5, 30, 100
   - Tamaño: 25K

10. distribuciones_t.pdf
    - Overlay de distribuciones t con df=2, 5, 30
    - Comparación con distribución normal
    - Muestra convergencia hacia normal con más grados de libertad
    - Tamaño: 24K

11. binomial_poisson.pdf
    - Dos paneles lado a lado
    - Izquierda: PMF Binomial (n=20, p=0.3)
    - Derecha: PMF Poisson (λ=5)
    - Tamaño: 20K

================================================================================
SESIÓN 4: INFERENCIA ESTADÍSTICA
================================================================================

12. distribucion_muestral.pdf
    - Tres paneles mostrando distribución de la población → muestras → distribución muestral
    - Población: normal (μ=100, σ=15)
    - Distribución muestral del promedio (n=30)
    - Tamaño: 33K

13. intervalo_confianza.pdf
    - 20 intervalos de confianza al 95% como segmentos horizontales
    - Línea vertical indica la media verdadera (μ)
    - Intervalos que contienen μ en color secundario
    - Intervalos que NO contienen μ en color acento (rojo)
    - Tamaño: 28K

14. ancho_ic.pdf
    - Dos curvas normales superpuestas
    - IC estrecho (n=100) vs IC amplio (n=10)
    - Demuestra influencia del tamaño de muestra en precisión
    - Tamaño: 52K

================================================================================
SESIÓN 5: PRÁCTICA INTEGRADORA
================================================================================

15. flujo_analisis.pdf
    - Diagrama de flujo del análisis estadístico
    - Pipeline: Datos → Exploración → Descriptiva → Probabilidad → 
                Distribuciones → Inferencia → Análisis → Conclusiones
    - Cajas coloreadas conectadas con flechas
    - Tamaño: 23K

================================================================================
INSTRUCCIONES DE USO EN BEAMER
================================================================================

Para incluir las figuras en presentaciones Beamer:

\usepackage{graphicx}

\begin{frame}
  \frametitle{Título del Slide}
  \begin{center}
    \includegraphics[width=0.9\textwidth]{figuras/nombre_figura.pdf}
  \end{center}
\end{frame}

Nota: Los PDFs generados están optimizados para esta dimensión específica,
por lo que se recomienda mantener width alrededor de 0.85-0.95\textwidth.

================================================================================
REQUISITOS
================================================================================

Python 3.7+
numpy
matplotlib
seaborn
scipy

Instalar: pip install numpy matplotlib seaborn scipy

================================================================================
REPRODUCIBILIDAD
================================================================================

Todas las figuras fueron generadas con numpy.random.seed(42), lo que asegura
que se pueden reproducir exactamente los mismos datos y visualizaciones.
Para regenerar las figuras, ejecute:

python generar_figuras.py

================================================================================
