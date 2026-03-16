#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script para generar figuras de 5 sesiones de Estadística en Beamer.
Autor: Script de generación automática
Fecha: 2026-03-13

Las figuras se generan en formato PDF optimizado para presentaciones Beamer.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch, Circle, Rectangle
from matplotlib.patches import Polygon
from matplotlib import ticker
import seaborn as sns
from scipy import stats
from scipy.special import comb, factorial
import os
import warnings
from math import factorial as math_factorial

warnings.filterwarnings('ignore')

# Configuración global
np.random.seed(42)
plt.style.use('seaborn-v0_8-whitegrid')

# Colores profesionales
COLOR_PRIMARIO = '#1F4E79'
COLOR_SECUNDARIO = '#2E86C1'
COLOR_ACENTO = '#E74C3C'
COLOR_VERDE = '#27AE60'
COLOR_NARANJA = '#F39C12'

# Parámetros de figura para Beamer
FIG_WIDTH = 5
FIG_HEIGHT = 3.5
DPI = 150

# Crear directorio de figuras si no existe
OUTPUT_DIR = '/sessions/sweet-adoring-franklin/mnt/Nivelaciones/clases_estadistica/figuras'
os.makedirs(OUTPUT_DIR, exist_ok=True)

def save_figure(filename):
    """Decorator para guardar figuras con estilo consistente."""
    def decorator(func):
        def wrapper(*args, **kwargs):
            fig = func(*args, **kwargs)
            filepath = os.path.join(OUTPUT_DIR, filename)
            fig.savefig(filepath, format='pdf', bbox_inches='tight', dpi=DPI)
            print(f"✓ Guardado: {filename}")
            plt.close(fig)
        return wrapper
    return decorator


def format_thousands(x, pos):
    """Formato de miles con separador de punto."""
    if x >= 1000:
        return f'{x/1000:.0f}k'
    return f'{x:.0f}'


# ============================================================================
# SESIÓN 1: ESTADÍSTICA DESCRIPTIVA
# ============================================================================

@save_figure('histograma_ingresos.pdf')
def figura_histograma_ingresos():
    """Histograma de ingresos con líneas de media y mediana."""
    ingresos = np.random.lognormal(mean=10.5, sigma=0.8, size=1000)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.hist(ingresos, bins=40, color=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=0.5)

    media = np.mean(ingresos)
    mediana = np.median(ingresos)

    ax.axvline(media, color=COLOR_PRIMARIO, linestyle='--', linewidth=2.5, label=f'Media: {media/1000:.1f}k')
    ax.axvline(mediana, color=COLOR_ACENTO, linestyle='--', linewidth=2.5, label=f'Mediana: {mediana/1000:.1f}k')

    ax.xaxis.set_major_formatter(ticker.FuncFormatter(format_thousands))
    ax.set_xlabel('Ingreso Anual ($)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Frecuencia', fontsize=11, fontweight='bold')
    ax.set_title('Distribución de Ingresos Anuales', fontsize=12, fontweight='bold')
    ax.legend(fontsize=10, loc='upper right')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('boxplot_comparativo.pdf')
def figura_boxplot_comparativo():
    """Boxplots comparativos de ingresos por región."""
    norte = np.random.lognormal(mean=10.7, sigma=0.7, size=150)
    centro = np.random.lognormal(mean=10.5, sigma=0.8, size=150)
    sur = np.random.lognormal(mean=10.2, sigma=0.9, size=150)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    datos = [norte, centro, sur]
    regiones = ['Norte', 'Centro', 'Sur']

    bp = ax.boxplot(datos, labels=regiones, patch_artist=True,
                     boxprops=dict(facecolor=COLOR_SECUNDARIO, alpha=0.7),
                     medianprops=dict(color=COLOR_ACENTO, linewidth=2),
                     whiskerprops=dict(color=COLOR_PRIMARIO, linewidth=1.5),
                     capprops=dict(color=COLOR_PRIMARIO, linewidth=1.5))

    ax.yaxis.set_major_formatter(ticker.FuncFormatter(format_thousands))
    ax.set_ylabel('Ingreso Anual ($)', fontsize=11, fontweight='bold')
    ax.set_title('Comparación de Ingresos por Región', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3, axis='y')

    fig.tight_layout()
    return fig


@save_figure('scatter_educacion_ingreso.pdf')
def figura_scatter_educacion_ingreso():
    """Scatter plot de años de educación vs ingreso con línea de tendencia."""
    años_educacion = np.random.uniform(8, 20, 200)
    ingreso = 25000 + 15000 * años_educacion + np.random.normal(0, 30000, 200)
    ingreso = np.maximum(ingreso, 20000)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.scatter(años_educacion, ingreso, color=COLOR_SECUNDARIO, alpha=0.6, s=50, edgecolors='black', linewidth=0.3)

    z = np.polyfit(años_educacion, ingreso, 1)
    p = np.poly1d(z)
    x_line = np.linspace(años_educacion.min(), años_educacion.max(), 100)
    ax.plot(x_line, p(x_line), color=COLOR_ACENTO, linewidth=2.5, label='Línea de Tendencia')

    corr = np.corrcoef(años_educacion, ingreso)[0, 1]

    ax.yaxis.set_major_formatter(ticker.FuncFormatter(format_thousands))
    ax.set_xlabel('Años de Educación', fontsize=11, fontweight='bold')
    ax.set_ylabel('Ingreso Anual ($)', fontsize=11, fontweight='bold')
    ax.set_title(f'Educación vs Ingreso (r = {corr:.2f})', fontsize=12, fontweight='bold')
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('tipos_variables.pdf')
def figura_tipos_variables():
    """Diagrama de tipos de variables estadísticas con flechas claras."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')

    # Título principal
    ax.text(5, 9.2, 'Tipos de Variables Estadísticas',
            fontsize=13, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.5', facecolor=COLOR_PRIMARIO, alpha=0.8, edgecolor='black', linewidth=1.5),
            color='white')

    # Variables (división principal)
    ax.text(2.5, 7.2, 'CUALITATIVAS', fontsize=11, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.4', facecolor=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=1),
            color='white')
    ax.text(7.5, 7.2, 'CUANTITATIVAS', fontsize=11, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.4', facecolor=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=1),
            color='white')

    # Flechas con FancyArrowPatch (mucho mejor que ax.arrow)
    arrow_props = dict(arrowstyle='->', mutation_scale=15, linewidth=2, color='#555555')
    ax.add_patch(FancyArrowPatch((4.0, 8.7), (2.8, 7.7), **arrow_props))
    ax.add_patch(FancyArrowPatch((6.0, 8.7), (7.2, 7.7), **arrow_props))

    # Flechas a subcategorías
    ax.add_patch(FancyArrowPatch((1.8, 6.7), (1.2, 6.2), **arrow_props))
    ax.add_patch(FancyArrowPatch((3.2, 6.7), (3.8, 6.2), **arrow_props))
    ax.add_patch(FancyArrowPatch((6.8, 6.7), (6.2, 6.2), **arrow_props))
    ax.add_patch(FancyArrowPatch((8.2, 6.7), (8.8, 6.2), **arrow_props))

    # Subcategorías cualitativas
    ax.text(1.2, 5.8, 'Nominal', fontsize=10, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#AED6F1', alpha=0.8, edgecolor='black'))
    ax.text(3.8, 5.8, 'Ordinal', fontsize=10, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#AED6F1', alpha=0.8, edgecolor='black'))

    # Ejemplos cualitativos
    ax.text(1.2, 4.6, 'Color, Sexo,\nProfesión', fontsize=8, ha='center', style='italic')
    ax.text(3.8, 4.6, 'Calificación,\nNivel educativo', fontsize=8, ha='center', style='italic')

    # Subcategorías cuantitativas
    ax.text(6.2, 5.8, 'Discreta', fontsize=10, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#F5B041', alpha=0.8, edgecolor='black'))
    ax.text(8.8, 5.8, 'Continua', fontsize=10, fontweight='bold', ha='center',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='#F5B041', alpha=0.8, edgecolor='black'))

    # Ejemplos cuantitativos
    ax.text(6.2, 4.6, 'Número de hijos,\ndefectos', fontsize=8, ha='center', style='italic')
    ax.text(8.8, 4.6, 'Edad, Peso,\nIngreso', fontsize=8, ha='center', style='italic')

    fig.tight_layout()
    return fig


# ============================================================================
# SESIÓN 2: PROBABILIDAD
# ============================================================================

@save_figure('diagrama_venn.pdf')
def figura_diagrama_venn():
    """Diagrama de Venn mostrando P(A∪B) = P(A) + P(B) - P(A∩B)."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)
    ax.set_xlim(-0.5, 5.5)
    ax.set_ylim(-0.5, 4.5)
    ax.set_aspect('equal')
    ax.axis('off')

    # Rectángulo para espacio muestral
    rect = Rectangle((-0.3, -0.3), 5.6, 4.6, fill=False, edgecolor='black', linewidth=2)
    ax.add_patch(rect)
    ax.text(5.0, 4.0, 'Ω', fontsize=14, fontweight='bold')

    # Círculos A y B con mayor superposición (centros más cercanos)
    circle_a = Circle((1.8, 2.0), 1.2, color=COLOR_SECUNDARIO, alpha=0.4, edgecolor=COLOR_PRIMARIO, linewidth=2.5)
    ax.add_patch(circle_a)
    ax.text(1.0, 2.5, 'A', fontsize=14, fontweight='bold', color=COLOR_PRIMARIO)

    circle_b = Circle((3.0, 2.0), 1.2, color=COLOR_ACENTO, alpha=0.4, edgecolor=COLOR_ACENTO, linewidth=2.5)
    ax.add_patch(circle_b)
    ax.text(3.8, 2.5, 'B', fontsize=14, fontweight='bold', color='#8B0000')

    # Etiqueta intersección
    ax.text(2.4, 2.0, 'A∩B', fontsize=11, fontweight='bold', ha='center', va='center')

    # Fórmula
    ax.text(2.75, 0.1, r'$P(A \cup B) = P(A) + P(B) - P(A \cap B)$',
            fontsize=11, ha='center', fontweight='bold',
            bbox=dict(boxstyle='round,pad=0.5', facecolor='lightyellow', edgecolor='black', linewidth=1.5))

    fig.tight_layout()
    return fig


@save_figure('bayes_ejemplo.pdf')
def figura_bayes_ejemplo():
    """Gráfico de probabilidades prior vs posterior en test médico."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    categorias = ['Prior', 'Posterior\n(Test +)']
    enfermo = [0.01, 0.166]
    sano = [0.99, 0.834]

    x = np.arange(len(categorias))
    ancho = 0.35

    ax.bar(x, enfermo, ancho, label='Enfermo', color=COLOR_ACENTO, edgecolor='black', linewidth=1)
    ax.bar(x, sano, ancho, bottom=enfermo, label='Sano', color=COLOR_SECUNDARIO, edgecolor='black', linewidth=1)

    ax.text(0, 0.005, '1%', ha='center', va='bottom', fontweight='bold', fontsize=10)
    ax.text(0, 0.55, '99%', ha='center', va='center', fontweight='bold', fontsize=10, color='white')

    ax.text(1, 0.083, '16.6%', ha='center', va='bottom', fontweight='bold', fontsize=10)
    ax.text(1, 0.5, '83.4%', ha='center', va='center', fontweight='bold', fontsize=10, color='white')

    ax.set_ylabel('Probabilidad', fontsize=11, fontweight='bold')
    ax.set_title('Teorema de Bayes: Test Médico', fontsize=12, fontweight='bold')
    ax.set_xticks(x)
    ax.set_xticklabels(categorias, fontsize=10)
    ax.set_ylim(0, 1.1)
    ax.legend(fontsize=10, loc='upper right')
    ax.grid(True, alpha=0.3, axis='y')

    fig.tight_layout()
    return fig


@save_figure('pmf_ejemplo.pdf')
def figura_pmf_ejemplo():
    """PMF de variable aleatoria discreta (Binomial)."""
    n, p = 10, 0.6
    x = np.arange(0, n + 1)
    pmf = [comb(n, k, exact=True) * (p ** k) * ((1 - p) ** (n - k)) for k in x]

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.bar(x, pmf, color=COLOR_SECUNDARIO, edgecolor=COLOR_PRIMARIO, linewidth=1.5, alpha=0.8)

    ax.set_xlabel('Número de Éxitos (X)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Probabilidad P(X=k)', fontsize=11, fontweight='bold')
    ax.set_title('Función de Masa de Probabilidad (PMF)\nDistribución Binomial (n=10, p=0.6)',
                 fontsize=11, fontweight='bold')
    ax.set_xticks(x)
    ax.grid(True, alpha=0.3, axis='y')

    fig.tight_layout()
    return fig


# ============================================================================
# SESIÓN 3: DISTRIBUCIONES
# ============================================================================

@save_figure('distribucion_normal.pdf')
def figura_distribucion_normal():
    """Curva normal con áreas sombreadas para regla 68-95-99.7."""
    x = np.linspace(-4, 4, 1000)
    y = stats.norm.pdf(x, 0, 1)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    # Área 2σ primero (detrás)
    x2 = x[(x >= -2) & (x <= 2)]
    ax.fill_between(x2, 0, stats.norm.pdf(x2), alpha=0.3, color=COLOR_ACENTO, label='95% (±2σ)')

    # Área 1σ encima
    x1 = x[(x >= -1) & (x <= 1)]
    ax.fill_between(x1, 0, stats.norm.pdf(x1), alpha=0.5, color=COLOR_SECUNDARIO, label='68% (±1σ)')

    # Línea principal
    ax.plot(x, y, color=COLOR_PRIMARIO, linewidth=2.5)

    # Líneas verticales
    for sigma in [-1, 1, -2, 2]:
        ax.axvline(sigma, color='gray', linestyle=':', alpha=0.5, linewidth=1)

    # Etiquetas
    ax.text(0, 0.22, '68%', fontsize=11, fontweight='bold', ha='center', color=COLOR_PRIMARIO)
    ax.text(1.5, 0.04, '95%', fontsize=10, fontweight='bold', ha='center', color=COLOR_ACENTO)

    ax.set_xlabel('z (Desviaciones Estándar)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad de Probabilidad', fontsize=11, fontweight='bold')
    ax.set_title('Distribución Normal: Regla 68-95-99.7', fontsize=12, fontweight='bold')
    ax.legend(fontsize=10, loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(0, 0.45)

    fig.tight_layout()
    return fig


@save_figure('tlc_simulacion.pdf')
def figura_tlc_simulacion():
    """Simulación del Teorema del Límite Central con 4 paneles."""
    np.random.seed(42)

    datos_originales = np.random.uniform(0, 1, 10000)
    medias_n5 = [np.mean(np.random.uniform(0, 1, 5)) for _ in range(1000)]
    medias_n30 = [np.mean(np.random.uniform(0, 1, 30)) for _ in range(1000)]
    medias_n100 = [np.mean(np.random.uniform(0, 1, 100)) for _ in range(1000)]

    fig, axes = plt.subplots(2, 2, figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT + 0.3), dpi=DPI)
    fig.suptitle('Teorema del Límite Central\n(Distribución Original: Uniforme)',
                 fontsize=11, fontweight='bold', y=1.02)

    # Panel 1: Original
    axes[0, 0].hist(datos_originales, bins=30, color=COLOR_PRIMARIO, alpha=0.7, edgecolor='black', linewidth=0.5)
    axes[0, 0].set_title('Población (Uniforme)', fontsize=9, fontweight='bold')
    axes[0, 0].set_ylabel('Frecuencia', fontsize=8)
    axes[0, 0].tick_params(labelsize=7)
    axes[0, 0].grid(True, alpha=0.3, axis='y')

    # Panel 2: n=5
    axes[0, 1].hist(medias_n5, bins=30, color=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=0.5)
    axes[0, 1].set_title('Medias (n=5)', fontsize=9, fontweight='bold')
    axes[0, 1].tick_params(labelsize=7)
    axes[0, 1].grid(True, alpha=0.3, axis='y')

    # Panel 3: n=30
    axes[1, 0].hist(medias_n30, bins=30, color=COLOR_VERDE, alpha=0.7, edgecolor='black', linewidth=0.5)
    axes[1, 0].set_title('Medias (n=30)', fontsize=9, fontweight='bold')
    axes[1, 0].set_ylabel('Frecuencia', fontsize=8)
    axes[1, 0].tick_params(labelsize=7)
    axes[1, 0].grid(True, alpha=0.3, axis='y')

    # Panel 4: n=100
    axes[1, 1].hist(medias_n100, bins=30, color=COLOR_ACENTO, alpha=0.7, edgecolor='black', linewidth=0.5)
    axes[1, 1].set_title('Medias (n=100)', fontsize=9, fontweight='bold')
    axes[1, 1].tick_params(labelsize=7)
    axes[1, 1].grid(True, alpha=0.3, axis='y')

    fig.tight_layout()
    return fig


@save_figure('distribuciones_t.pdf')
def figura_distribuciones_t():
    """Overlay de distribuciones t con diferentes grados de libertad."""
    x = np.linspace(-4, 4, 1000)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    y_t2 = stats.t.pdf(x, df=2)
    y_t5 = stats.t.pdf(x, df=5)
    y_t30 = stats.t.pdf(x, df=30)
    y_normal = stats.norm.pdf(x)

    ax.plot(x, y_t2, label='t (df=2)', color=COLOR_PRIMARIO, linewidth=2)
    ax.plot(x, y_t5, label='t (df=5)', color=COLOR_SECUNDARIO, linewidth=2)
    ax.plot(x, y_t30, label='t (df=30)', color=COLOR_NARANJA, linewidth=2)
    ax.plot(x, y_normal, label='Normal', color=COLOR_ACENTO, linewidth=2, linestyle='--')

    ax.set_xlabel('t', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad de Probabilidad', fontsize=11, fontweight='bold')
    ax.set_title('Distribución t de Student vs Normal', fontsize=12, fontweight='bold')
    ax.legend(fontsize=10, loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(0, 0.45)

    fig.tight_layout()
    return fig


@save_figure('binomial_poisson.pdf')
def figura_binomial_poisson():
    """Dos paneles: PMF Binomial y Poisson con etiquetas claras."""
    # Binomial (n=20, p=0.3)
    n, p = 20, 0.3
    x_binom = np.arange(0, n + 1)
    pmf_binom = [comb(n, k, exact=True) * (p ** k) * ((1 - p) ** (n - k)) for k in x_binom]

    # Poisson (λ=5)
    lambda_param = 5
    x_poisson = np.arange(0, 15)
    pmf_poisson = [np.exp(-lambda_param) * (lambda_param ** k) / math_factorial(int(k)) for k in x_poisson]

    fig, axes = plt.subplots(1, 2, figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT), dpi=DPI)

    # Binomial
    axes[0].bar(x_binom, pmf_binom, color=COLOR_SECUNDARIO, edgecolor=COLOR_PRIMARIO, linewidth=0.8, alpha=0.8)
    axes[0].set_xlabel('k', fontsize=10, fontweight='bold')
    axes[0].set_ylabel('P(X=k)', fontsize=10, fontweight='bold')
    axes[0].set_title('Binomial (n=20, p=0.3)', fontsize=10, fontweight='bold')
    axes[0].grid(True, alpha=0.3, axis='y')
    axes[0].set_xticks(np.arange(0, n + 1, 4))  # cada 4 para no encimar
    axes[0].tick_params(labelsize=8)

    # Poisson
    axes[1].bar(x_poisson, pmf_poisson, color=COLOR_ACENTO, edgecolor=COLOR_PRIMARIO, linewidth=0.8, alpha=0.8)
    axes[1].set_xlabel('k', fontsize=10, fontweight='bold')
    axes[1].set_ylabel('P(X=k)', fontsize=10, fontweight='bold')
    axes[1].set_title('Poisson (λ=5)', fontsize=10, fontweight='bold')
    axes[1].grid(True, alpha=0.3, axis='y')
    axes[1].set_xticks(np.arange(0, 15, 2))  # cada 2 para no encimar
    axes[1].tick_params(labelsize=8)

    fig.tight_layout()
    return fig


# ============================================================================
# SESIÓN 4: INFERENCIA ESTADÍSTICA
# ============================================================================

@save_figure('distribucion_muestral.pdf')
def figura_distribucion_muestral():
    """Distribución muestral del promedio - layout limpio con 3 filas."""
    np.random.seed(42)

    fig = plt.figure(figsize=(FIG_WIDTH, FIG_HEIGHT + 1.2), dpi=DPI)
    gs = fig.add_gridspec(3, 1, height_ratios=[1, 0.15, 1], hspace=0.3)

    ax_top = fig.add_subplot(gs[0])
    ax_mid = fig.add_subplot(gs[1])
    ax_bot = fig.add_subplot(gs[2])

    # Panel superior: Población
    poblacion = np.random.normal(loc=100, scale=15, size=10000)
    ax_top.hist(poblacion, bins=50, color=COLOR_PRIMARIO, alpha=0.7, edgecolor='black', linewidth=0.3)
    ax_top.axvline(np.mean(poblacion), color=COLOR_ACENTO, linestyle='--', linewidth=2,
                    label=f'μ = {np.mean(poblacion):.1f}')
    ax_top.set_ylabel('Frecuencia', fontsize=9, fontweight='bold')
    ax_top.set_title('Población Original (μ=100, σ=15)', fontsize=10, fontweight='bold')
    ax_top.legend(fontsize=8, loc='upper right')
    ax_top.grid(True, alpha=0.3, axis='y')
    ax_top.tick_params(labelsize=8)

    # Panel medio: anotación
    ax_mid.axis('off')
    ax_mid.text(0.5, 0.5, '↓  Muestras aleatorias (n=30)  ↓', ha='center', va='center',
                fontsize=9, fontweight='bold', color='#555555', transform=ax_mid.transAxes,
                bbox=dict(boxstyle='round,pad=0.3', facecolor='#F0F0F0', edgecolor='#999999'))

    # Panel inferior: Distribución muestral
    medias_muestrales = [np.mean(np.random.normal(100, 15, 30)) for _ in range(1000)]
    ax_bot.hist(medias_muestrales, bins=40, color=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=0.3)
    ax_bot.axvline(np.mean(medias_muestrales), color=COLOR_ACENTO, linestyle='--', linewidth=2,
                    label=f'Media de $\\bar{{X}}$ = {np.mean(medias_muestrales):.1f}')
    ax_bot.set_xlabel('$\\bar{X}$ (Promedio Muestral)', fontsize=9, fontweight='bold')
    ax_bot.set_ylabel('Frecuencia', fontsize=9, fontweight='bold')
    ax_bot.set_title('Distribución Muestral del Promedio (n=30)', fontsize=10, fontweight='bold')
    ax_bot.legend(fontsize=8, loc='upper right')
    ax_bot.grid(True, alpha=0.3, axis='y')
    ax_bot.tick_params(labelsize=8)

    return fig


@save_figure('intervalo_confianza.pdf')
def figura_intervalo_confianza():
    """20 intervalos de confianza con ~95% conteniendo la media real."""
    np.random.seed(42)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    mu_verdadera = 100
    n_intervalos = 20

    intervalos = []
    contiene = []

    for i in range(n_intervalos):
        muestra = np.random.normal(mu_verdadera, 15, 30)
        media_muestra = np.mean(muestra)
        sem = np.std(muestra, ddof=1) / np.sqrt(30)
        margen = 1.96 * sem
        ic_inferior = media_muestra - margen
        ic_superior = media_muestra + margen
        intervalos.append((ic_inferior, ic_superior))
        contiene.append(ic_inferior <= mu_verdadera <= ic_superior)

    for i, (inf, sup) in enumerate(intervalos):
        color = COLOR_SECUNDARIO if contiene[i] else COLOR_ACENTO
        lw = 2 if contiene[i] else 2.5
        ax.plot([inf, sup], [i, i], color=color, linewidth=lw)
        ax.plot(inf, i, 'o', color=color, markersize=5)
        ax.plot(sup, i, 'o', color=color, markersize=5)

    ax.axvline(mu_verdadera, color=COLOR_PRIMARIO, linewidth=2.5, linestyle='--', label=f'μ verdadera = {mu_verdadera}')

    n_contiene = sum(contiene)
    ax.set_xlabel('Valor', fontsize=11, fontweight='bold')
    ax.set_ylabel('Número de Intervalo', fontsize=11, fontweight='bold')
    ax.set_title(f'Intervalos de Confianza 95% ({n_contiene}/{n_intervalos} contienen μ)',
                fontsize=11, fontweight='bold')
    ax.set_yticks(range(n_intervalos))
    ax.set_yticklabels([str(i+1) for i in range(n_intervalos)], fontsize=8)
    ax.set_ylim(-1, n_intervalos)
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3, axis='x')

    fig.tight_layout()
    return fig


@save_figure('ancho_ic.pdf')
def figura_ancho_ic():
    """Dos curvas normales mostrando IC estrecho (n grande) vs IC amplio (n pequeño)."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    # Rango centrado en la media
    x = np.linspace(85, 115, 1000)

    se_narrow = 15 / np.sqrt(100)  # n=100
    se_wide = 15 / np.sqrt(10)     # n=10

    # IC amplio primero (detrás)
    y2 = stats.norm.pdf(x, 100, se_wide)
    ax.plot(x, y2, color=COLOR_ACENTO, linewidth=2.5, label='IC Amplio (n=10)')

    margin_wide = 1.96 * se_wide
    x_wide = x[(x >= 100 - margin_wide) & (x <= 100 + margin_wide)]
    ax.fill_between(x_wide, 0, stats.norm.pdf(x_wide, 100, se_wide),
                    alpha=0.25, color=COLOR_ACENTO)

    # IC estrecho encima
    y1 = stats.norm.pdf(x, 100, se_narrow)
    ax.plot(x, y1, color=COLOR_SECUNDARIO, linewidth=2.5, label='IC Estrecho (n=100)')

    margin_narrow = 1.96 * se_narrow
    x_narrow = x[(x >= 100 - margin_narrow) & (x <= 100 + margin_narrow)]
    ax.fill_between(x_narrow, 0, stats.norm.pdf(x_narrow, 100, se_narrow),
                    alpha=0.4, color=COLOR_SECUNDARIO)

    # Línea vertical en media
    ax.axvline(100, color=COLOR_PRIMARIO, linestyle='--', linewidth=2, label='Media μ')

    ax.set_xlabel('Valor', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad de Probabilidad', fontsize=11, fontweight='bold')
    ax.set_title('Influencia del Tamaño de Muestra en Ancho del IC', fontsize=11, fontweight='bold')
    ax.legend(fontsize=9, loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=0)

    fig.tight_layout()
    return fig


# ============================================================================
# SESIÓN 5: PRÁCTICA INTEGRADORA
# ============================================================================

@save_figure('flujo_analisis.pdf')
def figura_flujo_analisis():
    """Diagrama de flujo del análisis estadístico - texto sin cortar."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH + 1.5, FIG_HEIGHT + 0.5), dpi=DPI)
    ax.set_xlim(-0.5, 11.5)
    ax.set_ylim(2.5, 10.5)
    ax.axis('off')

    # Título
    ax.text(5.5, 10.0, 'Flujo de Análisis Estadístico', fontsize=13, fontweight='bold',
            ha='center', bbox=dict(boxstyle='round,pad=0.5', facecolor='lightyellow',
                                  edgecolor='black', linewidth=1.5))

    # Parámetros de las cajas
    box_w = 1.8
    box_h = 0.9

    # Posiciones de los nodos (más espaciados)
    pasos = [
        (1.5, 8.0, 'Datos', COLOR_PRIMARIO),
        (4.0, 8.0, 'Exploración', COLOR_SECUNDARIO),
        (7.0, 8.0, 'Descriptiva', COLOR_ACENTO),
        (10.0, 8.0, 'Probabilidad', COLOR_SECUNDARIO),
        (1.5, 5.5, 'Distribuciones', COLOR_PRIMARIO),
        (4.0, 5.5, 'Inferencia', COLOR_SECUNDARIO),
        (7.0, 5.5, 'Análisis', COLOR_ACENTO),
        (10.0, 5.5, 'Conclusiones', COLOR_PRIMARIO),
    ]

    # Dibujar cajas
    for x, y, texto, color in pasos:
        box = FancyBboxPatch((x - box_w/2, y - box_h/2), box_w, box_h,
                            boxstyle="round,pad=0.15", edgecolor='black', facecolor=color,
                            alpha=0.8, linewidth=1.5)
        ax.add_patch(box)
        ax.text(x, y, texto, ha='center', va='center', fontsize=9, fontweight='bold', color='white')

    # Flechas horizontales (fila superior)
    arrow_props = dict(arrowstyle='->', mutation_scale=18, linewidth=2, color='#444444')
    for i in range(3):
        x1 = pasos[i][0] + box_w/2 + 0.05
        x2 = pasos[i+1][0] - box_w/2 - 0.05
        y = pasos[i][1]
        ax.add_patch(FancyArrowPatch((x1, y), (x2, y), **arrow_props))

    # Flechas verticales (conectar filas)
    arrow_props_v = dict(arrowstyle='->', mutation_scale=18, linewidth=1.5, color='gray', linestyle='dashed')
    for i in [0, 1, 2, 3]:
        x = pasos[i][0]
        y1 = pasos[i][1] - box_h/2 - 0.05
        y2 = pasos[i+4][1] + box_h/2 + 0.05
        ax.add_patch(FancyArrowPatch((x, y1), (x, y2), **arrow_props_v))

    # Flechas horizontales (fila inferior)
    for i in range(4, 7):
        x1 = pasos[i][0] + box_w/2 + 0.05
        x2 = pasos[i+1][0] - box_w/2 - 0.05
        y = pasos[i][1]
        ax.add_patch(FancyArrowPatch((x1, y), (x2, y), **arrow_props))

    # Leyenda
    ax.text(5.5, 3.5, 'Cada fase depende de las anteriores para obtener conclusiones robustas',
           fontsize=9, ha='center', style='italic', color='gray')

    fig.tight_layout()
    return fig


# ============================================================================
# FUNCIÓN PRINCIPAL DE GENERACIÓN
# ============================================================================

def generar_todas_las_figuras():
    """Genera todas las figuras para las 5 sesiones de estadística."""

    print("\n" + "="*70)
    print("GENERANDO FIGURAS PARA SESIONES DE ESTADÍSTICA EN BEAMER")
    print("="*70 + "\n")

    # SESIÓN 1
    print("SESIÓN 1: ESTADÍSTICA DESCRIPTIVA")
    print("-" * 70)
    figura_histograma_ingresos()
    figura_boxplot_comparativo()
    figura_scatter_educacion_ingreso()
    figura_tipos_variables()

    # SESIÓN 2
    print("\nSESIÓN 2: PROBABILIDAD")
    print("-" * 70)
    figura_diagrama_venn()
    figura_bayes_ejemplo()
    figura_pmf_ejemplo()

    # SESIÓN 3
    print("\nSESIÓN 3: DISTRIBUCIONES")
    print("-" * 70)
    figura_distribucion_normal()
    figura_tlc_simulacion()
    figura_distribuciones_t()
    figura_binomial_poisson()

    # SESIÓN 4
    print("\nSESIÓN 4: INFERENCIA ESTADÍSTICA")
    print("-" * 70)
    figura_distribucion_muestral()
    figura_intervalo_confianza()
    figura_ancho_ic()

    # SESIÓN 5
    print("\nSESIÓN 5: PRÁCTICA INTEGRADORA")
    print("-" * 70)
    figura_flujo_analisis()

    print("\n" + "="*70)
    print("¡GENERACIÓN COMPLETADA!")
    print("="*70)
    print(f"\nTodas las figuras se han guardado en:")
    print(f"  {OUTPUT_DIR}")
    print("\nFiguras generadas:")
    figuras = sorted(os.listdir(OUTPUT_DIR))
    for i, fig in enumerate(figuras, 1):
        if fig.endswith('.pdf'):
            print(f"  {i:2d}. {fig}")
    total_pdf = len([f for f in figuras if f.endswith('.pdf')])
    print(f"\nTotal: {total_pdf} figuras")
    print("="*70 + "\n")


if __name__ == '__main__':
    generar_todas_las_figuras()
