#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script para generar figuras de 5 sesiones de Econometría en Beamer.
Las figuras se generan en formato PDF optimizado para presentaciones Beamer.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
from matplotlib import ticker
from scipy import stats
import os
import warnings

warnings.filterwarnings('ignore')

np.random.seed(42)
plt.style.use('seaborn-v0_8-whitegrid')

# Colores profesionales
COLOR_PRIMARIO = '#1F4E79'
COLOR_SECUNDARIO = '#2E86C1'
COLOR_ACENTO = '#E74C3C'
COLOR_VERDE = '#27AE60'
COLOR_NARANJA = '#F39C12'

FIG_WIDTH = 5
FIG_HEIGHT = 3.5
DPI = 150

OUTPUT_DIR = '/sessions/sweet-adoring-franklin/mnt/Nivelaciones/clases_econometria/figuras'
os.makedirs(OUTPUT_DIR, exist_ok=True)

def save_figure(filename):
    def decorator(func):
        def wrapper(*args, **kwargs):
            fig = func(*args, **kwargs)
            filepath = os.path.join(OUTPUT_DIR, filename)
            fig.savefig(filepath, format='pdf', bbox_inches='tight', dpi=DPI)
            print(f"  Guardado: {filename}")
            plt.close(fig)
        return wrapper
    return decorator


# ============================================================================
# SESION 1: PRUEBAS DE HIPOTESIS
# ============================================================================

@save_figure('distribucion_prueba_t.pdf')
def figura_distribucion_prueba_t():
    """Distribucion t con regiones de rechazo bilateral."""
    x = np.linspace(-4, 4, 1000)
    y = stats.t.pdf(x, df=25)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.plot(x, y, color=COLOR_PRIMARIO, linewidth=2.5)

    # Region de rechazo bilateral (alpha=0.05)
    t_crit = stats.t.ppf(0.975, df=25)
    x_left = x[x <= -t_crit]
    x_right = x[x >= t_crit]
    ax.fill_between(x_left, 0, stats.t.pdf(x_left, df=25), alpha=0.5, color=COLOR_ACENTO, label='Region de rechazo (alpha=0.05)')
    ax.fill_between(x_right, 0, stats.t.pdf(x_right, df=25), alpha=0.5, color=COLOR_ACENTO)

    # Region de no rechazo
    x_mid = x[(x > -t_crit) & (x < t_crit)]
    ax.fill_between(x_mid, 0, stats.t.pdf(x_mid, df=25), alpha=0.2, color=COLOR_SECUNDARIO, label='No rechazar H0')

    ax.axvline(-t_crit, color=COLOR_ACENTO, linestyle='--', linewidth=1.5)
    ax.axvline(t_crit, color=COLOR_ACENTO, linestyle='--', linewidth=1.5)

    ax.text(-t_crit, -0.02, f'-t*={-t_crit:.2f}', ha='center', fontsize=8, color=COLOR_ACENTO)
    ax.text(t_crit, -0.02, f't*={t_crit:.2f}', ha='center', fontsize=8, color=COLOR_ACENTO)

    ax.set_xlabel('Estadistico t', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad', fontsize=11, fontweight='bold')
    ax.set_title('Prueba t Bilateral (df=25, alpha=0.05)', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=-0.04)

    fig.tight_layout()
    return fig


@save_figure('errores_tipo.pdf')
def figura_errores_tipo():
    """Diagrama de errores Tipo I y Tipo II con dos distribuciones."""
    x = np.linspace(-4, 8, 1000)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    # H0: mu=0
    y_h0 = stats.norm.pdf(x, 0, 1)
    # H1: mu=3
    y_h1 = stats.norm.pdf(x, 3, 1)

    ax.plot(x, y_h0, color=COLOR_SECUNDARIO, linewidth=2.5, label='Bajo H0 (mu=0)')
    ax.plot(x, y_h1, color=COLOR_ACENTO, linewidth=2.5, label='Bajo H1 (mu=3)')

    # Valor critico
    z_crit = 1.96

    # Error Tipo I (alpha): area de H0 mas alla del critico
    x_alpha = x[x >= z_crit]
    ax.fill_between(x_alpha, 0, stats.norm.pdf(x_alpha, 0, 1), alpha=0.4, color=COLOR_NARANJA, label='Error Tipo I (alpha)')

    # Error Tipo II (beta): area de H1 por debajo del critico
    x_beta = x[(x <= z_crit) & (x >= -1)]
    ax.fill_between(x_beta, 0, stats.norm.pdf(x_beta, 3, 1), alpha=0.3, color='#9B59B6', label='Error Tipo II (beta)')

    ax.axvline(z_crit, color='black', linestyle='--', linewidth=1.5, alpha=0.7)
    ax.text(z_crit + 0.1, 0.35, f'Valor critico\n(z={z_crit})', fontsize=8, fontweight='bold')

    ax.set_xlabel('Valor del estadistico', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad', fontsize=11, fontweight='bold')
    ax.set_title('Errores Tipo I y Tipo II', fontsize=12, fontweight='bold')
    ax.legend(fontsize=8, loc='upper left')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=0)

    fig.tight_layout()
    return fig


@save_figure('matriz_decision.pdf')
def figura_matriz_decision():
    """Matriz de decision 2x2 para pruebas de hipotesis (visual profesional)."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT + 0.2), dpi=DPI)
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 8)
    ax.axis('off')

    # Titulo
    ax.text(5, 7.5, 'Matriz de Decision en Pruebas de Hipotesis',
            fontsize=13, fontweight='bold', ha='center', va='center', color=COLOR_PRIMARIO)

    # Headers columna
    ax.text(5.5, 6.5, 'H0 es Verdadera', fontsize=10, fontweight='bold', ha='center', va='center', color='white',
            bbox=dict(boxstyle='round,pad=0.4', facecolor=COLOR_PRIMARIO, edgecolor='black', linewidth=1.5))
    ax.text(8.5, 6.5, 'H0 es Falsa', fontsize=10, fontweight='bold', ha='center', va='center', color='white',
            bbox=dict(boxstyle='round,pad=0.4', facecolor=COLOR_PRIMARIO, edgecolor='black', linewidth=1.5))

    # Headers fila
    ax.text(2, 5, 'Rechazar\nH0', fontsize=10, fontweight='bold', ha='center', va='center', color='white',
            bbox=dict(boxstyle='round,pad=0.5', facecolor=COLOR_SECUNDARIO, edgecolor='black', linewidth=1.5))
    ax.text(2, 2.5, 'No rechazar\nH0', fontsize=10, fontweight='bold', ha='center', va='center', color='white',
            bbox=dict(boxstyle='round,pad=0.5', facecolor=COLOR_SECUNDARIO, edgecolor='black', linewidth=1.5))

    # Celda (Rechazar H0, H0 Verdadera) = Error Tipo I
    rect1 = patches.FancyBboxPatch((4, 4), 3, 2, boxstyle="round,pad=0.1",
                                     facecolor='#FADBD8', edgecolor=COLOR_ACENTO, linewidth=2)
    ax.add_patch(rect1)
    ax.text(5.5, 5.3, 'ERROR TIPO I', fontsize=10, fontweight='bold', ha='center', va='center', color=COLOR_ACENTO)
    ax.text(5.5, 4.7, '(Falso Positivo)', fontsize=8, ha='center', va='center', color=COLOR_ACENTO)
    ax.text(5.5, 4.2, 'Prob = alpha', fontsize=9, ha='center', va='center', color='gray', style='italic')

    # Celda (Rechazar H0, H0 Falsa) = Correcto (Potencia)
    rect2 = patches.FancyBboxPatch((7, 4), 3, 2, boxstyle="round,pad=0.1",
                                     facecolor='#D5F5E3', edgecolor=COLOR_VERDE, linewidth=2)
    ax.add_patch(rect2)
    ax.text(8.5, 5.3, 'CORRECTO', fontsize=10, fontweight='bold', ha='center', va='center', color=COLOR_VERDE)
    ax.text(8.5, 4.7, '(Potencia)', fontsize=8, ha='center', va='center', color=COLOR_VERDE)
    ax.text(8.5, 4.2, 'Prob = 1 - beta', fontsize=9, ha='center', va='center', color='gray', style='italic')

    # Celda (No rechazar H0, H0 Verdadera) = Correcto
    rect3 = patches.FancyBboxPatch((4, 1.5), 3, 2, boxstyle="round,pad=0.1",
                                     facecolor='#D5F5E3', edgecolor=COLOR_VERDE, linewidth=2)
    ax.add_patch(rect3)
    ax.text(5.5, 2.8, 'CORRECTO', fontsize=10, fontweight='bold', ha='center', va='center', color=COLOR_VERDE)
    ax.text(5.5, 2.2, '(Verdadero Negativo)', fontsize=8, ha='center', va='center', color=COLOR_VERDE)
    ax.text(5.5, 1.7, 'Prob = 1 - alpha', fontsize=9, ha='center', va='center', color='gray', style='italic')

    # Celda (No rechazar H0, H0 Falsa) = Error Tipo II
    rect4 = patches.FancyBboxPatch((7, 1.5), 3, 2, boxstyle="round,pad=0.1",
                                     facecolor='#FADBD8', edgecolor=COLOR_ACENTO, linewidth=2)
    ax.add_patch(rect4)
    ax.text(8.5, 2.8, 'ERROR TIPO II', fontsize=10, fontweight='bold', ha='center', va='center', color=COLOR_ACENTO)
    ax.text(8.5, 2.2, '(Falso Negativo)', fontsize=8, ha='center', va='center', color=COLOR_ACENTO)
    ax.text(8.5, 1.7, 'Prob = beta', fontsize=9, ha='center', va='center', color='gray', style='italic')

    # Nota inferior
    ax.text(5, 0.5, 'alpha = nivel de significancia (tipicamente 0.05)   |   beta = probabilidad de Error Tipo II',
            fontsize=8, ha='center', va='center', color='gray', style='italic')

    fig.tight_layout()
    return fig


@save_figure('h0_h1_pvalor.pdf')
def figura_h0_h1_pvalor():
    """Distribucion bajo H0 con valor observado y p-valor sombreado."""
    x = np.linspace(-4, 4, 1000)
    y = stats.norm.pdf(x, 0, 1)

    fig, axes = plt.subplots(1, 2, figsize=(FIG_WIDTH + 1.5, FIG_HEIGHT), dpi=DPI)

    # --- Panel izquierdo: No rechazar H0 (t_obs = 1.2, p = 0.23) ---
    ax = axes[0]
    t_obs_nr = 1.2
    p_val_nr = 2 * (1 - stats.norm.cdf(abs(t_obs_nr)))

    ax.plot(x, y, color=COLOR_PRIMARIO, linewidth=2.5)
    ax.fill_between(x, 0, y, alpha=0.08, color=COLOR_SECUNDARIO)

    # p-valor (dos colas)
    x_right = x[x >= t_obs_nr]
    x_left = x[x <= -t_obs_nr]
    ax.fill_between(x_right, 0, stats.norm.pdf(x_right, 0, 1), alpha=0.5, color=COLOR_SECUNDARIO, label=f'p-valor = {p_val_nr:.3f}')
    ax.fill_between(x_left, 0, stats.norm.pdf(x_left, 0, 1), alpha=0.5, color=COLOR_SECUNDARIO)

    ax.axvline(t_obs_nr, color=COLOR_SECUNDARIO, linewidth=2, linestyle='-')
    ax.axvline(-t_obs_nr, color=COLOR_SECUNDARIO, linewidth=2, linestyle='-')
    ax.text(t_obs_nr + 0.1, 0.28, f't = {t_obs_nr}', fontsize=9, fontweight='bold', color=COLOR_SECUNDARIO)

    # Valores criticos
    ax.axvline(1.96, color='gray', linewidth=1, linestyle='--', alpha=0.5)
    ax.axvline(-1.96, color='gray', linewidth=1, linestyle='--', alpha=0.5)

    ax.set_title(f'No rechazar H0\n(p = {p_val_nr:.3f} > 0.05)', fontsize=10, fontweight='bold', color=COLOR_SECUNDARIO)
    ax.set_xlabel('Estadistico t', fontsize=9, fontweight='bold')
    ax.set_ylabel('Densidad', fontsize=9, fontweight='bold')
    ax.legend(fontsize=8, loc='upper left')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=-0.02)
    ax.tick_params(labelsize=8)

    # --- Panel derecho: Rechazar H0 (t_obs = 2.8, p = 0.005) ---
    ax = axes[1]
    t_obs_r = 2.8
    p_val_r = 2 * (1 - stats.norm.cdf(abs(t_obs_r)))

    ax.plot(x, y, color=COLOR_PRIMARIO, linewidth=2.5)
    ax.fill_between(x, 0, y, alpha=0.08, color=COLOR_ACENTO)

    # p-valor (dos colas)
    x_right = x[x >= t_obs_r]
    x_left = x[x <= -t_obs_r]
    ax.fill_between(x_right, 0, stats.norm.pdf(x_right, 0, 1), alpha=0.6, color=COLOR_ACENTO, label=f'p-valor = {p_val_r:.4f}')
    ax.fill_between(x_left, 0, stats.norm.pdf(x_left, 0, 1), alpha=0.6, color=COLOR_ACENTO)

    ax.axvline(t_obs_r, color=COLOR_ACENTO, linewidth=2, linestyle='-')
    ax.axvline(-t_obs_r, color=COLOR_ACENTO, linewidth=2, linestyle='-')
    ax.text(t_obs_r + 0.1, 0.28, f't = {t_obs_r}', fontsize=9, fontweight='bold', color=COLOR_ACENTO)

    # Valores criticos
    ax.axvline(1.96, color='gray', linewidth=1, linestyle='--', alpha=0.5)
    ax.axvline(-1.96, color='gray', linewidth=1, linestyle='--', alpha=0.5)

    ax.set_title(f'Rechazar H0\n(p = {p_val_r:.4f} < 0.05)', fontsize=10, fontweight='bold', color=COLOR_ACENTO)
    ax.set_xlabel('Estadistico t', fontsize=9, fontweight='bold')
    ax.set_ylabel('Densidad', fontsize=9, fontweight='bold')
    ax.legend(fontsize=8, loc='upper left')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=-0.02)
    ax.tick_params(labelsize=8)

    fig.suptitle('Distribucion bajo H0: Interpretacion del p-valor', fontsize=12, fontweight='bold',
                 color=COLOR_PRIMARIO, y=1.02)
    fig.tight_layout()
    return fig


@save_figure('potencia_estadistica.pdf')
def figura_potencia_estadistica():
    """Potencia vs tamano de muestra para diferentes tamanos de efecto."""
    ns = np.arange(10, 201, 5)
    alpha = 0.05
    z_alpha = stats.norm.ppf(1 - alpha/2)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    for d, color, label in [(0.2, COLOR_SECUNDARIO, 'Efecto pequeno (d=0.2)'),
                             (0.5, COLOR_VERDE, 'Efecto mediano (d=0.5)'),
                             (0.8, COLOR_ACENTO, 'Efecto grande (d=0.8)')]:
        power = []
        for n in ns:
            se = 1 / np.sqrt(n)
            z_beta = d / se - z_alpha
            pow_val = stats.norm.cdf(z_beta)
            power.append(pow_val)
        ax.plot(ns, power, color=color, linewidth=2.5, label=label)

    ax.axhline(0.8, color='gray', linestyle='--', linewidth=1, alpha=0.7)
    ax.text(185, 0.82, 'Potencia=0.80', fontsize=8, color='gray')

    ax.set_xlabel('Tamano de muestra (n)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Potencia (1-beta)', fontsize=11, fontweight='bold')
    ax.set_title('Potencia Estadistica vs Tamano de Muestra', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='lower right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(0, 1.05)

    fig.tight_layout()
    return fig


@save_figure('chi_cuadrado.pdf')
def figura_chi_cuadrado():
    """Distribucion chi-cuadrado con region de rechazo."""
    x = np.linspace(0.01, 20, 1000)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    for df, color, label in [(3, COLOR_SECUNDARIO, 'df=3'),
                              (5, COLOR_VERDE, 'df=5'),
                              (8, COLOR_NARANJA, 'df=8')]:
        y = stats.chi2.pdf(x, df)
        ax.plot(x, y, color=color, linewidth=2.5, label=label)

    # Region de rechazo para df=5
    chi_crit = stats.chi2.ppf(0.95, df=5)
    x_rej = x[x >= chi_crit]
    ax.fill_between(x_rej, 0, stats.chi2.pdf(x_rej, df=5), alpha=0.4, color=COLOR_ACENTO, label=f'Rechazo (df=5, chi2*={chi_crit:.1f})')

    ax.set_xlabel('chi2', fontsize=11, fontweight='bold')
    ax.set_ylabel('Densidad', fontsize=11, fontweight='bold')
    ax.set_title('Distribucion Chi-Cuadrado', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_ylim(bottom=0)

    fig.tight_layout()
    return fig


# ============================================================================
# SESION 2: REGRESION LINEAL SIMPLE
# ============================================================================

@save_figure('scatter_regresion.pdf')
def figura_scatter_regresion():
    """Scatter con linea de regresion y residuos marcados."""
    np.random.seed(42)
    n = 50
    x = np.random.uniform(5, 25, n)
    y = 10 + 3 * x + np.random.normal(0, 8, n)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.scatter(x, y, color=COLOR_SECUNDARIO, alpha=0.7, s=60, edgecolors='black', linewidth=0.5, label='Datos', zorder=3)

    # Linea de regresion
    z = np.polyfit(x, y, 1)
    p = np.poly1d(z)
    x_line = np.linspace(x.min(), x.max(), 100)
    ax.plot(x_line, p(x_line), color=COLOR_ACENTO, linewidth=2.5, label=f'MCO: Y = {z[1]:.1f} + {z[0]:.2f}X')

    # Mostrar algunos residuos
    for i in [5, 15, 25, 35, 45]:
        y_hat = p(x[i])
        ax.plot([x[i], x[i]], [y[i], y_hat], color=COLOR_VERDE, linewidth=1.5, alpha=0.7)

    ax.set_xlabel('Variable Independiente (X)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Variable Dependiente (Y)', fontsize=11, fontweight='bold')
    ax.set_title('Regresion Lineal Simple por MCO', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper left')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('residuos_ols.pdf')
def figura_residuos_ols():
    """Residuos vs valores ajustados."""
    np.random.seed(42)
    n = 80
    x = np.random.uniform(5, 25, n)
    y = 10 + 3 * x + np.random.normal(0, 8, n)
    z = np.polyfit(x, y, 1)
    p = np.poly1d(z)
    y_hat = p(x)
    residuos = y - y_hat

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    ax.scatter(y_hat, residuos, color=COLOR_SECUNDARIO, alpha=0.7, s=50, edgecolors='black', linewidth=0.5)
    ax.axhline(0, color=COLOR_ACENTO, linewidth=2, linestyle='--')

    # Banda de confianza
    ax.axhline(2*np.std(residuos), color='gray', linewidth=1, linestyle=':', alpha=0.5)
    ax.axhline(-2*np.std(residuos), color='gray', linewidth=1, linestyle=':', alpha=0.5)

    ax.set_xlabel('Valores Ajustados (Y hat)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Residuos (e)', fontsize=11, fontweight='bold')
    ax.set_title('Residuos vs Valores Ajustados', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('r_cuadrado.pdf')
def figura_r_cuadrado():
    """Comparacion visual R2 bajo vs alto."""
    np.random.seed(42)

    fig, axes = plt.subplots(1, 2, figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT), dpi=DPI)

    # R2 bajo
    x1 = np.random.uniform(0, 10, 50)
    y1 = 2 + 0.5 * x1 + np.random.normal(0, 4, 50)
    axes[0].scatter(x1, y1, color=COLOR_SECUNDARIO, alpha=0.6, s=40, edgecolors='black', linewidth=0.3)
    z1 = np.polyfit(x1, y1, 1)
    axes[0].plot(np.sort(x1), np.poly1d(z1)(np.sort(x1)), color=COLOR_ACENTO, linewidth=2)
    ss_res1 = np.sum((y1 - np.poly1d(z1)(x1))**2)
    ss_tot1 = np.sum((y1 - np.mean(y1))**2)
    r2_1 = 1 - ss_res1/ss_tot1
    axes[0].set_title(f'R2 = {r2_1:.2f} (Bajo)', fontsize=10, fontweight='bold')
    axes[0].set_xlabel('X', fontsize=9)
    axes[0].set_ylabel('Y', fontsize=9)
    axes[0].grid(True, alpha=0.3)
    axes[0].tick_params(labelsize=8)

    # R2 alto
    x2 = np.random.uniform(0, 10, 50)
    y2 = 2 + 3 * x2 + np.random.normal(0, 2, 50)
    axes[1].scatter(x2, y2, color=COLOR_SECUNDARIO, alpha=0.6, s=40, edgecolors='black', linewidth=0.3)
    z2 = np.polyfit(x2, y2, 1)
    axes[1].plot(np.sort(x2), np.poly1d(z2)(np.sort(x2)), color=COLOR_ACENTO, linewidth=2)
    ss_res2 = np.sum((y2 - np.poly1d(z2)(x2))**2)
    ss_tot2 = np.sum((y2 - np.mean(y2))**2)
    r2_2 = 1 - ss_res2/ss_tot2
    axes[1].set_title(f'R2 = {r2_2:.2f} (Alto)', fontsize=10, fontweight='bold')
    axes[1].set_xlabel('X', fontsize=9)
    axes[1].set_ylabel('Y', fontsize=9)
    axes[1].grid(True, alpha=0.3)
    axes[1].tick_params(labelsize=8)

    fig.tight_layout()
    return fig


@save_figure('supuestos_ols.pdf')
def figura_supuestos_ols():
    """Panel 2x2 de diagnosticos de regresion."""
    np.random.seed(42)
    n = 100
    x = np.random.uniform(1, 20, n)
    y = 5 + 2.5 * x + np.random.normal(0, 5, n)
    z = np.polyfit(x, y, 1)
    p_func = np.poly1d(z)
    y_hat = p_func(x)
    residuos = y - y_hat
    residuos_std = (residuos - np.mean(residuos)) / np.std(residuos)

    fig, axes = plt.subplots(2, 2, figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT + 0.3), dpi=DPI)

    # 1. Residuos vs Ajustados
    axes[0, 0].scatter(y_hat, residuos, color=COLOR_SECUNDARIO, alpha=0.5, s=25, edgecolors='black', linewidth=0.3)
    axes[0, 0].axhline(0, color=COLOR_ACENTO, linewidth=1.5, linestyle='--')
    axes[0, 0].set_title('Residuos vs Ajustados', fontsize=9, fontweight='bold')
    axes[0, 0].set_xlabel('Y hat', fontsize=8)
    axes[0, 0].set_ylabel('Residuos', fontsize=8)
    axes[0, 0].tick_params(labelsize=7)
    axes[0, 0].grid(True, alpha=0.3)

    # 2. QQ Plot
    stats.probplot(residuos_std, plot=axes[0, 1])
    axes[0, 1].get_lines()[0].set(color=COLOR_SECUNDARIO, markersize=3)
    axes[0, 1].get_lines()[1].set(color=COLOR_ACENTO, linewidth=1.5)
    axes[0, 1].set_title('Q-Q Normal', fontsize=9, fontweight='bold')
    axes[0, 1].set_xlabel('Cuantiles teoricos', fontsize=8)
    axes[0, 1].set_ylabel('Cuantiles muestrales', fontsize=8)
    axes[0, 1].tick_params(labelsize=7)

    # 3. Scale-Location
    axes[1, 0].scatter(y_hat, np.sqrt(np.abs(residuos_std)), color=COLOR_SECUNDARIO, alpha=0.5, s=25, edgecolors='black', linewidth=0.3)
    axes[1, 0].set_title('Scale-Location', fontsize=9, fontweight='bold')
    axes[1, 0].set_xlabel('Y hat', fontsize=8)
    axes[1, 0].set_ylabel('sqrt(|Res. Std.|)', fontsize=8)
    axes[1, 0].tick_params(labelsize=7)
    axes[1, 0].grid(True, alpha=0.3)

    # 4. Histograma de residuos
    axes[1, 1].hist(residuos_std, bins=20, color=COLOR_SECUNDARIO, alpha=0.7, edgecolor='black', linewidth=0.5)
    x_norm = np.linspace(-3, 3, 100)
    axes[1, 1].plot(x_norm, stats.norm.pdf(x_norm) * n * 0.3, color=COLOR_ACENTO, linewidth=2)
    axes[1, 1].set_title('Histograma Residuos', fontsize=9, fontweight='bold')
    axes[1, 1].set_xlabel('Residuos estandarizados', fontsize=8)
    axes[1, 1].set_ylabel('Frecuencia', fontsize=8)
    axes[1, 1].tick_params(labelsize=7)
    axes[1, 1].grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


# ============================================================================
# SESION 3: REGRESION MULTIPLE
# ============================================================================

@save_figure('sesgo_variable_omitida.pdf')
def figura_sesgo_variable_omitida():
    """Ejemplo de sesgo de variable omitida con diagrama."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)
    ax.set_xlim(0, 10)
    ax.set_ylim(1, 9)
    ax.axis('off')

    # Cajas
    box_props = dict(boxstyle='round,pad=0.4', linewidth=2)

    ax.text(1.5, 7, 'Educacion\n(X)', fontsize=11, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, edgecolor='black', alpha=0.8), color='white')
    ax.text(8.5, 7, 'Ingreso\n(Y)', fontsize=11, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_ACENTO, edgecolor='black', alpha=0.8), color='white')
    ax.text(5, 3, 'Habilidad\n(Z, omitida)', fontsize=11, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_NARANJA, edgecolor='black', alpha=0.8))

    # Flechas
    arrow_props = dict(arrowstyle='->', mutation_scale=20, linewidth=2.5)
    ax.add_patch(FancyArrowPatch((3, 7), (7, 7), color=COLOR_PRIMARIO, **arrow_props))
    ax.text(5, 7.5, 'Efecto causal?', fontsize=9, ha='center', fontweight='bold', color=COLOR_PRIMARIO)

    ax.add_patch(FancyArrowPatch((5, 3.8), (2.5, 6.2), color=COLOR_NARANJA, **arrow_props))
    ax.add_patch(FancyArrowPatch((5, 3.8), (7.5, 6.2), color=COLOR_NARANJA, **arrow_props))

    ax.text(3, 4.5, 'Correlacion', fontsize=8, ha='center', color=COLOR_NARANJA, rotation=35)
    ax.text(7, 4.5, 'Efecto', fontsize=8, ha='center', color=COLOR_NARANJA, rotation=-35)

    ax.text(5, 1.5, 'Omitir Z sesga la estimacion de X -> Y',
            fontsize=10, ha='center', style='italic', color='gray',
            bbox=dict(boxstyle='round,pad=0.3', facecolor='lightyellow', edgecolor='gray'))

    fig.tight_layout()
    return fig


@save_figure('dag_tres_tipos.pdf')
def figura_dag_tres_tipos():
    """DAGs: Confusora, Collider, Modificadora de efecto - panel 1x3."""
    fig, axes = plt.subplots(1, 3, figsize=(FIG_WIDTH + 2.5, FIG_HEIGHT + 0.3), dpi=DPI)

    box_props = dict(boxstyle='round,pad=0.35', linewidth=2, edgecolor='black')
    arrow_props = dict(arrowstyle='->', mutation_scale=18, linewidth=2.5)

    # --- Panel 1: CONFUSORA (Z -> X, Z -> Y) ---
    ax = axes[0]
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')
    ax.set_title('CONFUSORA', fontsize=11, fontweight='bold', color=COLOR_ACENTO, pad=8)

    ax.text(1.5, 3, 'X', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(8.5, 3, 'Y', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(5, 8, 'Z', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_ACENTO, alpha=0.85), color='white')

    ax.add_patch(FancyArrowPatch((3, 3), (7, 3), color=COLOR_PRIMARIO, **arrow_props))
    ax.add_patch(FancyArrowPatch((4.2, 7.3), (2.3, 3.8), color=COLOR_ACENTO, **arrow_props))
    ax.add_patch(FancyArrowPatch((5.8, 7.3), (7.7, 3.8), color=COLOR_ACENTO, **arrow_props))

    ax.text(5, 1, 'Z causa X e Y\nControlar por Z', fontsize=7.5, ha='center', va='center',
            color='gray', style='italic',
            bbox=dict(boxstyle='round,pad=0.2', facecolor='lightyellow', edgecolor='gray', alpha=0.8))

    # --- Panel 2: COLLIDER (X -> Z, Y -> Z) ---
    ax = axes[1]
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')
    ax.set_title('COLLIDER', fontsize=11, fontweight='bold', color=COLOR_NARANJA, pad=8)

    ax.text(1.5, 8, 'X', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(8.5, 8, 'Y', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(5, 3, 'Z', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_NARANJA, alpha=0.85), color='white')

    ax.add_patch(FancyArrowPatch((2.3, 7.2), (4.2, 3.8), color=COLOR_NARANJA, **arrow_props))
    ax.add_patch(FancyArrowPatch((7.7, 7.2), (5.8, 3.8), color=COLOR_NARANJA, **arrow_props))

    ax.text(5, 1, 'X e Y causan Z\nNO controlar por Z', fontsize=7.5, ha='center', va='center',
            color='gray', style='italic',
            bbox=dict(boxstyle='round,pad=0.2', facecolor='#FFF3E0', edgecolor='gray', alpha=0.8))

    # --- Panel 3: MODIFICADORA DE EFECTO (Z modifica X -> Y) ---
    ax = axes[2]
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')
    ax.set_title('MODIFICADORA\nDE EFECTO', fontsize=10, fontweight='bold', color=COLOR_VERDE, pad=4)

    ax.text(1.5, 5, 'X', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(8.5, 5, 'Y', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_SECUNDARIO, alpha=0.85), color='white')
    ax.text(5, 8.5, 'Z', fontsize=14, fontweight='bold', ha='center', va='center',
            bbox=dict(**box_props, facecolor=COLOR_VERDE, alpha=0.85), color='white')

    ax.add_patch(FancyArrowPatch((3, 5), (7, 5), color=COLOR_PRIMARIO, **arrow_props))
    # Arrow from Z to the middle of X->Y (modifier)
    ax.add_patch(FancyArrowPatch((5, 7.7), (5, 5.5), color=COLOR_VERDE, **arrow_props,
                                  linestyle='dashed'))

    ax.text(5, 1, 'Z cambia la magnitud\ndel efecto X->Y', fontsize=7.5, ha='center', va='center',
            color='gray', style='italic',
            bbox=dict(boxstyle='round,pad=0.2', facecolor='#E8F5E9', edgecolor='gray', alpha=0.8))

    fig.tight_layout()
    return fig


@save_figure('modelos_anidados.pdf')
def figura_modelos_anidados():
    """Tabla visual de modelos anidados mostrando cambio en coeficientes."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH + 1.5, FIG_HEIGHT + 0.5), dpi=DPI)
    ax.axis('off')

    # Datos de la tabla (sin colLabels separado, todo en data)
    col_labels = ['Modelo 1\n(Simple)', 'Modelo 2\n(+ Control)', 'Modelo 3\n(+ Dummy)', 'Modelo 4\n(Completo)']
    row_labels = ['Educacion', 'Experiencia', 'Mujer', 'Educ x Mujer', 'R2', 'R2 ajust.', 'N']
    data = [
        ['5.200***', '3.800***', '3.650***', '4.100***'],
        ['---', '0.950***', '0.920***', '0.890***'],
        ['---', '---', '-2.100**', '-4.500*'],
        ['---', '---', '---', '0.350*'],
        ['0.32', '0.48', '0.51', '0.53'],
        ['0.31', '0.47', '0.50', '0.52'],
        ['500', '500', '500', '500'],
    ]

    # Crear tabla
    table = ax.table(cellText=data, rowLabels=row_labels, colLabels=col_labels,
                      cellLoc='center', loc='center', colColours=['#D6EAF8']*4)

    table.auto_set_font_size(False)
    table.set_fontsize(8.5)
    table.scale(1.0, 1.55)

    # Colorear celdas
    for i in range(len(row_labels)):
        for j in range(-1, len(col_labels)):
            try:
                cell = table[i+1, j]
            except KeyError:
                continue
            if j == -1:  # row labels
                cell.set_text_props(fontweight='bold', fontsize=8)
                cell.set_facecolor('#EBF5FB')
            elif i == 0:  # Educacion row - highlight changes
                if j == 0:
                    cell.set_facecolor('#FADBD8')  # red - biased
                else:
                    cell.set_facecolor('#D5F5E3')  # green - corrected
            elif data[i][j] == '---':
                cell.set_facecolor('#F8F9F9')
                cell.set_text_props(color='#CCCCCC')

    # Header styling
    for j in range(-1, len(col_labels)):
        try:
            cell = table[0, j]
            cell.set_text_props(fontweight='bold', fontsize=8, color='white')
            cell.set_facecolor(COLOR_PRIMARIO)
        except KeyError:
            continue

    # Title and annotation
    ax.set_title('Modelos Anidados: Detectando Sesgo por Variable Omitida',
                 fontsize=12, fontweight='bold', color=COLOR_PRIMARIO, pad=15)

    ax.text(0.5, -0.02, 'Educacion cambia de 5.2 a 3.8 al agregar Experiencia = evidencia de OVB\n'
            '*** p<0.001, ** p<0.01, * p<0.05',
            fontsize=8, ha='center', va='top', transform=ax.transAxes,
            color='gray', style='italic')

    fig.tight_layout()
    return fig


@save_figure('variables_dummy.pdf')
def figura_variables_dummy():
    """Regresion con variable dummy: dos lineas paralelas."""
    np.random.seed(42)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    # Grupo 0 (control)
    x0 = np.random.uniform(5, 20, 30)
    y0 = 20 + 2 * x0 + np.random.normal(0, 4, 30)

    # Grupo 1 (tratamiento)
    x1 = np.random.uniform(5, 20, 30)
    y1 = 35 + 2 * x1 + np.random.normal(0, 4, 30)

    ax.scatter(x0, y0, color=COLOR_SECUNDARIO, alpha=0.7, s=50, edgecolors='black', linewidth=0.3, label='D=0 (Sin programa)')
    ax.scatter(x1, y1, color=COLOR_ACENTO, alpha=0.7, s=50, edgecolors='black', linewidth=0.3, marker='s', label='D=1 (Con programa)')

    x_line = np.linspace(5, 20, 100)
    ax.plot(x_line, 20 + 2 * x_line, color=COLOR_SECUNDARIO, linewidth=2.5, linestyle='--')
    ax.plot(x_line, 35 + 2 * x_line, color=COLOR_ACENTO, linewidth=2.5, linestyle='--')

    # Anotar diferencia
    ax.annotate('', xy=(12, 35 + 2*12), xytext=(12, 20 + 2*12),
                arrowprops=dict(arrowstyle='<->', color=COLOR_VERDE, linewidth=2))
    ax.text(12.5, 20 + 2*12 + 7.5, 'beta_D = 15', fontsize=10, fontweight='bold', color=COLOR_VERDE)

    ax.set_xlabel('Experiencia (anos)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Salario (miles)', fontsize=11, fontweight='bold')
    ax.set_title('Variable Dummy: Efecto de Programa', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper left')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('interaccion.pdf')
def figura_interaccion():
    """Efecto de interaccion: pendientes diferentes por grupo."""
    np.random.seed(42)

    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    x_line = np.linspace(0, 15, 100)

    # Sin interaccion (solo dummy)
    # Grupo A: Y = 10 + 3X
    # Grupo B: Y = 10 + 1.5X (pendiente diferente = interaccion)
    ax.plot(x_line, 10 + 3 * x_line, color=COLOR_SECUNDARIO, linewidth=2.5, label='Grupo A: Y = 10 + 3X')
    ax.plot(x_line, 10 + 1.5 * x_line, color=COLOR_ACENTO, linewidth=2.5, linestyle='--', label='Grupo B: Y = 10 + 1.5X')

    # Datos simulados
    x_a = np.random.uniform(1, 14, 25)
    y_a = 10 + 3 * x_a + np.random.normal(0, 3, 25)
    x_b = np.random.uniform(1, 14, 25)
    y_b = 10 + 1.5 * x_b + np.random.normal(0, 3, 25)

    ax.scatter(x_a, y_a, color=COLOR_SECUNDARIO, alpha=0.5, s=30, edgecolors='black', linewidth=0.3)
    ax.scatter(x_b, y_b, color=COLOR_ACENTO, alpha=0.5, s=30, marker='s', edgecolors='black', linewidth=0.3)

    ax.set_xlabel('X (anos de experiencia)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Y (ingreso)', fontsize=11, fontweight='bold')
    ax.set_title('Efecto de Interaccion: Pendientes Diferentes', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper left')
    ax.grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


@save_figure('vif_multicolinealidad.pdf')
def figura_vif_multicolinealidad():
    """Barplot de VIF para diagnostico de multicolinealidad."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH, FIG_HEIGHT), dpi=DPI)

    variables = ['Educacion', 'Experiencia', 'Edad', 'Horas\nsemanales', 'Antiguedad', 'Ingreso\nfamiliar']
    vif_values = [1.8, 2.3, 8.5, 1.4, 7.2, 1.6]

    colors = [COLOR_ACENTO if v > 5 else COLOR_SECUNDARIO for v in vif_values]

    bars = ax.bar(variables, vif_values, color=colors, edgecolor='black', linewidth=0.8, alpha=0.8)

    ax.axhline(5, color=COLOR_ACENTO, linewidth=2, linestyle='--', label='Umbral VIF=5')
    ax.axhline(10, color='darkred', linewidth=1.5, linestyle=':', label='Umbral VIF=10')

    for bar, val in zip(bars, vif_values):
        ax.text(bar.get_x() + bar.get_width()/2, val + 0.2, f'{val:.1f}',
                ha='center', fontsize=9, fontweight='bold')

    ax.set_ylabel('Factor de Inflacion de Varianza (VIF)', fontsize=10, fontweight='bold')
    ax.set_title('Diagnostico de Multicolinealidad', fontsize=12, fontweight='bold')
    ax.legend(fontsize=9, loc='upper left')
    ax.grid(True, alpha=0.3, axis='y')
    ax.tick_params(axis='x', labelsize=8)

    fig.tight_layout()
    return fig


# ============================================================================
# SESION 4-5: PRACTICA
# ============================================================================

@save_figure('flujo_econometria.pdf')
def figura_flujo_econometria():
    """Flujo de analisis econometrico."""
    fig, ax = plt.subplots(figsize=(FIG_WIDTH + 1, FIG_HEIGHT + 0.3), dpi=DPI)
    ax.set_xlim(-0.5, 11)
    ax.set_ylim(1, 9)
    ax.axis('off')

    pasos = [
        (1.5, 7.5, 'Pregunta\nde Investigacion', COLOR_PRIMARIO),
        (5, 7.5, 'Datos y\nExploracion', COLOR_SECUNDARIO),
        (8.5, 7.5, 'Especificacion\ndel Modelo', COLOR_ACENTO),
        (1.5, 4, 'Estimacion\n(MCO)', COLOR_VERDE),
        (5, 4, 'Diagnosticos\ny Validacion', COLOR_NARANJA),
        (8.5, 4, 'Interpretacion\ny Reporte', COLOR_PRIMARIO),
    ]

    box_w, box_h = 2.2, 1.2

    for x, y, texto, color in pasos:
        box = FancyBboxPatch((x - box_w/2, y - box_h/2), box_w, box_h,
                            boxstyle="round,pad=0.15", edgecolor='black', facecolor=color,
                            alpha=0.8, linewidth=1.5)
        ax.add_patch(box)
        ax.text(x, y, texto, ha='center', va='center', fontsize=9, fontweight='bold', color='white')

    arrow_props = dict(arrowstyle='->', mutation_scale=18, linewidth=2, color='#444444')

    # Flechas horizontales superiores
    ax.add_patch(FancyArrowPatch((2.65, 7.5), (3.85, 7.5), **arrow_props))
    ax.add_patch(FancyArrowPatch((6.15, 7.5), (7.35, 7.5), **arrow_props))

    # Flecha bajando
    ax.add_patch(FancyArrowPatch((8.5, 6.85), (8.5, 4.65), connectionstyle="arc3,rad=0", **arrow_props))

    # Flechas horizontales inferiores (derecha a izquierda? no, izquierda a derecha)
    # Realmente va: Especificacion -> Estimacion (baja y va a la izq)
    ax.add_patch(FancyArrowPatch((7.35, 4), (6.15, 4), **arrow_props))  # Diagnosticos <- Interpretacion (reversed)

    # Correccion: flujo es Especificacion -> Estimacion -> Diagnosticos -> Interpretacion
    # Pero la fila inferior va al reves espacialmente. Vamos de derecha a izquierda en fila sup, luego baja
    # y va izquierda a derecha en fila inf
    # Actually let me fix: top row left to right, then down, bottom row left to right
    # Estimacion(1.5,4) -> Diagnosticos(5,4) -> Interpretacion(8.5,4)
    ax.add_patch(FancyArrowPatch((2.65, 4), (3.85, 4), **arrow_props))
    ax.add_patch(FancyArrowPatch((6.15, 4), (7.35, 4), **arrow_props))

    # Flecha de Especificacion bajando a Estimacion
    ax.add_patch(FancyArrowPatch((8.5, 6.85), (1.5, 4.65), connectionstyle="arc3,rad=-0.3",
                                  arrowstyle='->', mutation_scale=18, linewidth=1.5, color='gray', linestyle='dashed'))

    ax.text(5.5, 2.2, 'Flujo iterativo: los diagnosticos pueden llevar a re-especificar',
           fontsize=9, ha='center', style='italic', color='gray')

    fig.tight_layout()
    return fig


@save_figure('diagnosticos_r.pdf')
def figura_diagnosticos_r():
    """Panel 2x2 simulando output de plot(lm()) en R."""
    np.random.seed(123)
    n = 80
    x = np.random.uniform(2, 20, n)
    y = 5 + 2 * x + np.random.normal(0, 3, n)

    # Agregar outlier
    x = np.append(x, [18])
    y = np.append(y, [60])
    n += 1

    z = np.polyfit(x, y, 1)
    p_func = np.poly1d(z)
    y_hat = p_func(x)
    residuos = y - y_hat
    residuos_std = (residuos - np.mean(residuos)) / np.std(residuos)

    fig, axes = plt.subplots(2, 2, figsize=(FIG_WIDTH + 0.5, FIG_HEIGHT + 0.3), dpi=DPI)

    # Residuos vs Fitted
    axes[0, 0].scatter(y_hat, residuos, color=COLOR_SECUNDARIO, alpha=0.6, s=20, edgecolors='black', linewidth=0.3)
    axes[0, 0].axhline(0, color=COLOR_ACENTO, linewidth=1.5, linestyle='--')
    axes[0, 0].set_title('Residuals vs Fitted', fontsize=9, fontweight='bold')
    axes[0, 0].tick_params(labelsize=7)
    axes[0, 0].grid(True, alpha=0.3)

    # Normal Q-Q
    stats.probplot(residuos_std, plot=axes[0, 1])
    axes[0, 1].get_lines()[0].set(color=COLOR_SECUNDARIO, markersize=3)
    axes[0, 1].get_lines()[1].set(color=COLOR_ACENTO, linewidth=1.5)
    axes[0, 1].set_title('Normal Q-Q', fontsize=9, fontweight='bold')
    axes[0, 1].tick_params(labelsize=7)

    # Scale-Location
    axes[1, 0].scatter(y_hat, np.sqrt(np.abs(residuos_std)), color=COLOR_SECUNDARIO, alpha=0.6, s=20, edgecolors='black', linewidth=0.3)
    axes[1, 0].set_title('Scale-Location', fontsize=9, fontweight='bold')
    axes[1, 0].tick_params(labelsize=7)
    axes[1, 0].grid(True, alpha=0.3)

    # Cook's Distance
    leverage = (x - np.mean(x))**2 / np.sum((x - np.mean(x))**2) + 1/n
    cooks_d = residuos_std**2 * leverage / (2 * (1 - leverage)**2)
    cooks_d = np.clip(cooks_d, 0, 2)

    markerline, stemlines, baseline = axes[1, 1].stem(range(n), cooks_d, linefmt='-', markerfmt='o', basefmt='k-')
    markerline.set_color(COLOR_SECUNDARIO)
    markerline.set_markersize(3)
    stemlines.set_color(COLOR_SECUNDARIO)
    axes[1, 1].axhline(4/n, color=COLOR_ACENTO, linewidth=1.5, linestyle='--')
    axes[1, 1].set_title("Cook's Distance", fontsize=9, fontweight='bold')
    axes[1, 1].tick_params(labelsize=7)
    axes[1, 1].grid(True, alpha=0.3)

    fig.tight_layout()
    return fig


# ============================================================================
# PRINCIPAL
# ============================================================================

def generar_todas():
    print("\n" + "="*60)
    print("GENERANDO FIGURAS - ECONOMETRIA")
    print("="*60)

    print("\nSesion 1: Pruebas de Hipotesis")
    print("-"*40)
    figura_distribucion_prueba_t()
    figura_errores_tipo()
    figura_matriz_decision()
    figura_h0_h1_pvalor()
    figura_potencia_estadistica()
    figura_chi_cuadrado()

    print("\nSesion 2: Regresion Simple")
    print("-"*40)
    figura_scatter_regresion()
    figura_residuos_ols()
    figura_r_cuadrado()
    figura_supuestos_ols()

    print("\nSesion 3: Regresion Multiple")
    print("-"*40)
    figura_sesgo_variable_omitida()
    figura_dag_tres_tipos()
    figura_modelos_anidados()
    figura_variables_dummy()
    figura_interaccion()
    figura_vif_multicolinealidad()

    print("\nSesiones 4-5: Practica")
    print("-"*40)
    figura_flujo_econometria()
    figura_diagnosticos_r()

    total = len([f for f in os.listdir(OUTPUT_DIR) if f.endswith('.pdf')])
    print(f"\nTotal: {total} figuras en {OUTPUT_DIR}")
    print("="*60 + "\n")

if __name__ == '__main__':
    generar_todas()
