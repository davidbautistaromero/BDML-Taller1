## BDML Taller 1 — Predicción de ingresos (GEIH 2018 Bogotá)

Proyecto del curso "Big Data y Machine Learning para Economía Aplicada". Este taller construye una canalización completa: obtención de datos (web scraping), limpieza, exploración, modelación econométrica, evaluación de modelos predictivos y generación de artefactos reproducibles.

### Objetivos
- Obtener y consolidar la muestra GEIH 2018 (Bogotá) desde la página del curso.
- Limpiar y preparar datos, con manejo de faltantes y atípicos.
- Explorar estadísticas descriptivas y correlaciones.
- Estimar el perfil edad–ingreso y hallar la edad pico (con intervalos vía bootstrap).
- Analizar la brecha salarial de género con controles (FWL) y bootstrap.
- Entrenar y evaluar modelos predictivos (Validation Set y LOOCV) para el salario.

### Fuente de datos
- GEIH 2018 (Bogotá), tomada del sitio del curso: [GEIH 2018 sample](https://ignaciomsarmiento.github.io/GEIH2018_sample/).

### Estructura del repositorio
- `scripts/`
  - `00_config.R`: configuración base; carga/instala paquetes; define rutas y utilidades de almacenamiento.
  - `01_Web_Scrapping.R`: scraping de la página del curso; detecta y descarga las tablas; consolida en `stores/GEIH2018_consolidada.{csv,rds}`.
  - `02_limpieza.R`: filtra Bogotá, ≥18 y ocupados; selección/renombrado de variables; imputación (regresión/mediana) para ingresos; winsorización; creación de transformaciones (logs) y dummies (`Mujer`, `Edad2`); construcción de ocupaciones (`ocupacion_o`, `ocupacion_directiva`); exporta `stores/base_final.rds`.
  - `02.01_estadisticas_descriptivas.R`: descriptivas (stargazer), matriz de correlaciones y densidades; guarda salidas en `views/` (tabla y figuras).
  - `03_edad_que_maximiza_ingreso.R`: regresiones cuadráticas de edad–ingreso, edad pico analítica y bootstrap; figuras del perfil con bandas.
  - `04_brecha_ingreso_genero.R`: estimación de la brecha de género (OLS completo, FWL manual y FWL con bootstrap); tabla comparativa (LaTeX) y perfiles edad–ingreso por género; exporta `stores/edad_pico.rds` y gráficos.
  - `05_predictions.R`: partición train/test (70/30); entrena 7 especificaciones; compara RMSE en test; LOOCV (caret y fórmula analítica); exporta tablas LaTeX y `stores/{base_predictions,train_set,test_set}.rds`.
- `stores/` (generado): datasets intermedios/finales (`*.rds`, `*.csv`).
- `views/` (generado): tablas (`*.tex`, `*.txt`) y gráficas (`*.png`).
- `BDML-Taller1.Rproj`: proyecto de RStudio (UTF-8, Sweave, pdfLaTeX).

### Requisitos
- R (>= 4.0 recomendado) y RStudio.
- Paquetes R (cargados con `pacman` en `00_config.R`): `rio`, `tidyverse`, `skimr`, `visdat`, `corrplot`, `stargazer`, `gridExtra`, `MASS`, `rvest`, `naniar`, `gt`, `gtsummary`, `caret`, `dplyr`, `ggplot2`, `data.table`, `here`, `boot`, `xtable`.
  - Adicionales usados en los scripts: `patchwork` y `rstudioapi`.

Instalación rápida de paquetes:

```r
install.packages("pacman")
pacman::p_load(
  rio, tidyverse, skimr, visdat, corrplot, stargazer, gridExtra, MASS, rvest,
  naniar, gt, gtsummary, caret, dplyr, ggplot2, data.table, here, boot, xtable,
  patchwork
)
```

### Ejecución (reproducible)
1) Abrir `BDML-Taller1.Rproj` en RStudio.
2) Ejecutar los scripts en orden (crean `stores/` y `views/` si no existen):

```r
source(here::here("scripts", "01_Web_Scrapping.R"))
source(here::here("scripts", "02_limpieza.R"))
source(here::here("scripts", "02.01_estadisticas_descriptivas.R"))
source(here::here("scripts", "03_edad_que_maximiza_ingreso.R"))
source(here::here("scripts", "04_brecha_ingreso_genero.R"))
source(here::here("scripts", "05_predictions.R"))
```

Notas de ejecución:
- `01_Web_Scrapping.R` abre el sitio en el navegador (`browseURL`) y luego raspa los enlaces.
- `00_config.R` usa `rstudioapi::getSourceEditorContext()` para ubicar rutas; si corres fuera de RStudio, define manualmente el directorio de trabajo del proyecto o ajusta `stores_path`.

### Principales resultados
- Descriptivas (resumen `views/tabla_estadisticas.txt`):
  - Salario por hora (winsorizado): media ≈ 7,643.8; N=9,892.
  - Edad (winsorizada): media ≈ 39.4; N=16,542.
  - Horas trabajadas (winsorizadas): media ≈ 47.3; N=16,542.
  - Otras variables: ver archivo para detalle.
- Perfil edad–ingreso:
  - Modelos cuadráticos por diferentes definiciones de ingreso; se calcula edad pico analítica y con bootstrap.
  - Ejemplo de edades pico usadas para graficar (script 04): Hombres ≈ 61.19; Mujeres ≈ 57.15.
- Brecha salarial de género:
  - Comparación entre OLS completo, FWL manual y FWL con bootstrap; se genera tabla comparativa (LaTeX) y figura de perfiles por género.
- Evaluación predictiva (RMSE):
  - Validation Set (test): mejor desempeño en `Modelo 4` (RMSE ≈ 0.11246); `Modelo 5` ≈ 0.11254.
  - LOOCV (caret): `Modelo 4` ≈ 0.10717; `Modelo 5` ≈ 0.10716.
  - Ver `views/rmse_validation_set.tex` y `views/RMSE_LOOCV.tex`.

### Artefactos generados (salida)
- `stores/`
  - `GEIH2018_consolidada.{csv,rds}` (scraping).
  - `base_final.rds` (datos limpios y enriquecidos).
  - `edad_pico.rds`, `base_predictions.rds`, `train_set.rds`, `test_set.rds`.
- `views/`
  - Tablas: `tabla_estadisticas.txt`, `rmse_validation_set.tex`, `RMSE_LOOCV.tex`.
  - Gráficas: densidades, correlaciones y perfiles (`*.png`), p. ej. `densidades.png`, `matriz_correlaciones.png`, `graf_edad_pico_intervalos.png`, `distribucion_error_pred.png`.

### Decisiones de preparación de datos (clave)
- Filtro: Bogotá, personas ≥18 y ocupadas.
- Imputaciones: ingresos por regresión (preferida) y mediana (explorada); eliminación de filas no imputables para ingresos por hora.
- Winsorización: extremos (p99 o bilateral) para ingresos y covariables claves.
- Transformaciones: `log_*` de ingresos; creación de `Mujer` y `Edad2`.
- Ocupaciones: agrupación por frecuencia (`ocupacion_o`) y dummy `ocupacion_directiva`.

### Reproducibilidad
- Semilla: `set.seed(10101)` en módulos de bootstrap, partición y validación.
- Salidas deterministas dado el flujo y la semilla, sujeto a versiones de paquetes.



