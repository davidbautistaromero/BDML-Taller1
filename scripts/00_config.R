# ============================================================
# 00_Config.R - Configuración base para todo el proyecto
# ============================================================

# Limpiar entorno
rm(list = ls())

# Lista de paquetes requeridos para el análisis completo
# instalar pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio,        # Importación/exportación datos
       tidyverse,  # Herramientas de manipulación de datos y visualización
       skimr,      # Resumen de los datos
       visdat,     # Visualizar datos faltantes
       corrplot,   # Gráficas de correlación
       stargazer,  # Tablas y salida en formato TEX
       gridExtra,  # Organiza gráficos o tablas en una misma disposición
       MASS,       # Funciones estadisticas
       rvest,      # Librería de web scraping
       naniar,     # Visualizar y manipular missing values
       gt,         # Tablas descriptivas
       gtsummary,  # Tablas resumidas de estadísticas y modelos
       caret,      # Entrenamiento y validación de modelos
       dplyr,      # Manipulación de datos
       ggplot2,    # Visualización de datos
       data.table, # Manipulación de datos
       here,       # Gestionar rutas
       boot        # Bootstrap  
)

# Definir rutas 
# Identificamos la ruta del script actual
script_path <- rstudioapi::getSourceEditorContext()$path
script_dir  <- dirname(script_path)

# Creamos carpeta stores (si no existe) en la raíz del proyecto
stores_path <- file.path(dirname(script_dir), "stores")
if (!dir.exists(stores_path)) {
  dir.create(stores_path, recursive = TRUE)
}

# Función de ayuda para construir rutas hacia stores
store_file <- function(filename) {
  file.path(stores_path, filename)
}

