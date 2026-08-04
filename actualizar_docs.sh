#!/bin/bash
# Copia las slides interactivas (HTML autocontenidos) renderizadas en las
# carpetas de clases hacia docs/, que es la carpeta publicada por GitHub Pages.
# Ejecutar despues de `quarto render` en cualquier sesion.
set -e
cd "$(dirname "$0")"
n=0
for f in clases_estadistica/sesion*.html clases_econometria/sesion*.html clases_fundamentos/sesion*.html; do
  [ -e "$f" ] || continue
  mv "$f" "docs/$(basename "$f")"
  echo "docs/ <- $f"
  n=$((n+1))
done
echo "$n slides actualizadas en docs/"
