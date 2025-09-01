# =============================================================================
# Taller Big Data y Machine Learning 
# Problem Set 1 - Predicting Income
# =============================================================================

# =============================================================================
# CONFIGURACIÓN DEL AMBIENTE DE TRABAJO
# =============================================================================

# Limpiar el entorno
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
       gt,         # Tablas descriptivas
       gtsummary,  # Tablas resumidas de estadísticas y modelos
       caret,      # Entrenamiento y validación de modelos
       dplyr,      # Manipulación de datos
       ggplot2,    # Visualización de datos
       data.table  # Manipulación de datos
)


# =============================================================================
# SECCIÓN 1: Web Scraping
# =============================================================================
# Objetivo:
# - Obtener los datos de Bogotá del Informe de Medición de Pobreza Monetaria y
#   Desigualdad de 2018 utilizando información del GEIH.
#
# Fuente principal:
# - https://ignaciomsarmiento.github.io/GEIH2018_sample/
# =============================================================================

# URL base para la extracción de información
url_taller <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# Acceder al robots.txt de la página para verificar restricciones
browseURL(url_taller)

# Leer el html de la página
html_taller = read_html(url_taller)

# Validar la clase del objeto
class(html_taller) 

# Si usamos la función html_table nos topamos con una lista vacía, dado que la
# función solo obtiene el HTML inicial sin ejecutar el JavaScript que carga los
# datos, se debe detectar y replicar las peticiones de red que la página realiza
# para obtener el enlace desde donde se alimenta la tabla.
html_taller %>%
  html_table()

# Al inspeccionar la página vemos que cada chunk de información esta en un 
# elemento <li>, en el nodo <a y el atributo href
links <- html_taller %>%
  html_elements("li")%>%
  html_nodes("a")%>%
  html_attr("href")
print(links)

# El scraping también devuelve un enlace extra ("index") que no necesitamos.
# Filtramos solo los que contienen "page". Además, notamos que los enlaces
# están incompletos (solo traen el sufijo final).
chunks <- links[grepl("page", links)] 

# Construimos las URL completas concatenando la URL base con cada chunk
chunks_urls <- paste0(url_taller, chunks)
print(chunks_urls)

# Extraemos las tablas de cada chunk y las unimos
# Inicializamos el data.frame vacío
base_final <- data.frame()  

# Iteramos cada chunk
for (chunk_url in chunks_urls) {
  page <- read_html(chunk_url)

# Extramos la referencia donde esta la tabla  
  referencia <- page %>%
    html_element("div.col-md-9") %>%
    html_element("div") %>%
    html_attr("w3-include-html")

# Construimos la URL completa de la tabla concatenando la base y la referencia
  url_tabla <- paste0(url_taller, referencia)

# Extraemos las tablas dentro de ese HTML    
  tabla <- read_html(url_tabla) %>%
    html_elements("table") %>%
    html_table()

  tabla_final <- as.data.frame(tabla)

# Verificamos en la consola la cantidad de columnas de cada tabla y unimos
  print(paste("Columnas en esta tabla:", ncol(tabla_final)))  
  base_final <- rbind(base_final, tabla_final)
}

# Validamos las dimensiones y las primeras observaciones de la base final 
dim(base_final)
head(base_final)

# Guardamos la base

## Identificamos la ruta donde está guardado el script (automatización del guardado)
script_path <- rstudioapi::getSourceEditorContext()$path

##Obtenemos el directorio del Script
script_dir = dirname(script_path)

##Creamos la carpeta stores para almacenar las bases
stores_path = file.path(dirname(script_dir),"stores")

# Creamos la carpeta si no existe
if (!dir.exists(stores_path)) {
  dir.create(stores_path, recursive = TRUE)
}

write.csv(base_final, file.path(stores_path, "GEIH2018_consolidada.csv"),row.names = FALSE)
saveRDS(base_final, file.path(stores_path, "GEIH2018_consolidada.rds"))

