# =============================================================================
# Taller Big Data y Machine Learning 
# Problem Set 1 - Predicting Income
# =============================================================================

# =============================================================================
# Script_01: Web Scraping
# =============================================================================
# Objetivo:
# - Obtener los datos de Bogotá del Informe de Medición de Pobreza Monetaria y
#   Desigualdad de 2018 utilizando información del GEIH.
#
# Fuente principal:
# - https://ignaciomsarmiento.github.io/GEIH2018_sample/
# =============================================================================

# Cargar configuración base
source(here::here("scripts", "00_Config.R"))

# -----------------------------------------------------------------------------
# 1. Definir URL base
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 2. Extraer enlaces de los chunks
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 3. Iterar sobre cada chunk para extraer tablas
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 4. Guardar base consolidada
# -----------------------------------------------------------------------------

write.csv(base_final, file.path(stores_path, "GEIH2018_consolidada.csv"),row.names = FALSE)
saveRDS(base_final, file.path(stores_path, "GEIH2018_consolidada.rds"))

