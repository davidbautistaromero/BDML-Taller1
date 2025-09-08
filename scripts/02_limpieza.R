# =============================================================================
# Script_02: Carga y Limpieza de Datos
# =============================================================================
# Objetivos:
# - Cargar la base consolidada desde el scraping
# - Explorar y limpiar datos (nombres, tipos, valores faltantes)
# - Generar dataset listo para análisis y modelación
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Importamos los datos procesados en la etapa 01 (scraping)
data_raw <- import(here::here("stores", "GEIH2018_consolidada.csv"))

# Inspección inicial
names(data_raw)        # Nombres de variables
str(data_raw)          # Tipos de variables

# -----------------------------------------------------------------------------
# 1. Filtrado 
# -----------------------------------------------------------------------------

# Validamos que la data sea solo de Bogotá
table(data_raw$dominio)


# Restringir datos a personas mayores de 18 años empleadas
data_clean <- data_raw %>%
  filter(age >=  18,  # Mayores de 18
         ocu == 1)    # La variable ocu toma 1 cuando esta ocupado

dim(data_clean)
skim(data_clean) # Resumen general

# -----------------------------------------------------------------------------
# 2. Selección de variables
# -----------------------------------------------------------------------------

data_clean <- data_clean %>%
  dplyr::select(
    # Identificadores
    directorio, secuencia_p, orden,
    # Demografía
    age, sex, estrato1, p6050,
    # Educación
    maxEducLevel,
    # Laboral
    oficio, relab, cuentaPropia, totalHoursWorked, hoursWorkUsual, 
    sizeFirm, p6426, formal, informal,
    # Ingresos
    y_total_m, y_total_m_ha, y_otros_m, y_salary_m, 
    ingtot, ingtotes, ingtotob,y_salary_m_hu,y_ingLab_m_ha,
    # Salud y pensión
    regSalud, cotPension, fex_c
  ) %>%
  rename(
    id_vivienda               = directorio,
    id_hogar                  = secuencia_p,
    id_persona                = orden,
    edad                      = age,
    sexo                      = sex,
    estrato                   = estrato1,
    parentesco_jefe           = p6050,
    nivel_educ_max            = maxEducLevel,
    ocupacion                 = oficio,
    tipo_ocupacion            = relab,
    trabajador_independiente  = cuentaPropia,
    horas_trab                = totalHoursWorked,
    horas_trab_usual          = hoursWorkUsual,
    tamanio_empresa           = sizeFirm,
    experiencia               = p6426,
    trabajo_formal            = formal,
    trabajo_informal          = informal,
    ingreso_total             = y_total_m,
    ingreso_hora              = y_total_m_ha,
    otros_ingresos            = y_otros_m,
    salario_mensual           = y_salary_m,
    ingreso_total_2           = ingtot,
    ingreso_total_imputado    = ingtotes,
    ingreso_total_observado   = ingtotob,
    regimen_salud             = regSalud,
    cotiza_pension            = cotPension,
    factor_expansion          = fex_c,
    salario_real_hora         = y_salary_m_hu, # Ocupación principal
    ingreso_laboral_hora      = y_ingLab_m_ha  # Ingreso laboral por todas las ocupaciones
  )

# Creamos variables que necesitamos para el análisis, saber si es jefe de
# hogar y pasar la experiencia a años para facilitar el análisis

head(data_clean)
table(data_clean$parentesco_jefe)
table(data_clean$experiencia)

# Crear la variable dummy de jefe de hogar
data_clean$dummy_jefe <- ifelse(data_clean$parentesco_jefe == 1, 1, 0)

# Crear la variable de experiencia en años
data_clean <- data_clean %>% 
  mutate(experiencia_anualizada = experiencia / 12)

# Convertimos en factor
data_clean <- data_clean %>%
  mutate(
    ocupacion_factor = as.factor(ocupacion),
    edu_factor      = as.factor(nivel_educ_max),
    estrato_factor  = as.factor(estrato),
    tfirma_factor   = as.factor(tamanio_empresa)
  )

head(data_clean)


# -----------------------------------------------------------------------------
# 3. Revisar  valores faltantes
# -----------------------------------------------------------------------------

# Validamos que variables traen valores faltantes y graficamos
sapply(data_clean, function(x) sum(is.na(x)))

# Gráfica de valores faltantes para toda la base
png(filename = file.path("views", "grafica_valores_faltantes.png"),
    width = 1200, height = 1000)

vis_miss(data_clean) +
  labs(
    title = "Valores faltantes en la base de datos",
    x = "Variables",
    y = "Observaciones"
  ) +
  theme(
    axis.text.y = element_text(angle = 90),
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

dev.off()


# Generamos una nueva gráfica con menos variables
data_missing <- data_clean %>% 
  dplyr::select(
    edad, sexo, estrato,
    nivel_educ_max,
    ocupacion, trabajo_formal, trabajador_independiente,
    horas_trab, horas_trab_usual, tamanio_empresa, trabajo_informal,
    ingreso_total, ingreso_hora, otros_ingresos, salario_mensual,
    experiencia_anualizada, dummy_jefe, salario_real_hora, ingreso_laboral_hora
  )

png(filename = file.path("views", "grafica_valores_faltantes_clave.png"), 
    width = 1000, height = 800)

vis_miss(data_missing) +
  labs(
    title = "Valores faltantes en variables clave",
    x = "Variables",
    y = "Observaciones"
  ) +
  theme(
    axis.text.y = element_text(angle = 90),
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)
  )

dev.off()

# Tabla de missing para variables clave
db_miss <- skim(data_missing) %>% 
  dplyr::select(skim_variable, n_missing)

Nobs <- nrow(data_clean)
db_miss <- db_miss %>% 
  filter(n_missing != 0) %>%
  mutate(p_missing = n_missing / Nobs) %>%
  arrange(-n_missing)

db_miss  # Tabla de missing con porcentaje

# Gráfica de las variables con missing
png(file.path("views", "graf_faltantes_var_clave.png"), width = 800, height = 600)

ggplot(db_miss, aes(x = reorder(skim_variable, +p_missing), y = p_missing)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Variables clave con valores faltantes",
    x = "Variables",
    y = "Proporción de Missing"
  ) +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  )

dev.off()

# Eliminamos la variable otros_ingresos ya que como vemos en la gráfica la mayoria
# de sus datos son missing

data_clean <- data_clean %>%
  dplyr::select(-otros_ingresos)


# Ahora vamos a analizar que haremos con el resto de variables con gran proporción
# de missings
# Analisis de sensibilidad, vemos la media y la mediana antes y después de la transformación 
# Distribucion del ingreso por hora
png(filename = file.path("views", "distribucion_ingreso_hora.png"), width = 1000, height = 800)
ggplot(data_clean, aes(ingreso_hora)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(data_clean$ingreso_hora, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(data_clean$ingreso_hora, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso Hora") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
dev.off()


mean(data_clean$ingreso_hora, na.rm=TRUE)
median(data_clean$ingreso_hora, na.rm=TRUE)

## Imputación por regresión
ingreso_hora_lm <- lm(ingreso_hora ~  sexo + edad + nivel_educ_max + estrato , data = data_clean)
summary(ingreso_hora_lm)

data_clean$ingreso_hora_pred <- predict(ingreso_hora_lm, newdata = data_clean)

data_clean<-  data_clean %>%  
  mutate(ingreso_hora_implm = ifelse(is.na(ingreso_hora) == TRUE, ingreso_hora_pred , ingreso_hora))

mean(data_clean$ingreso_hora_implm, na.rm=TRUE)
median(data_clean$ingreso_hora_implm, na.rm=TRUE)

# imputación por media grupal 
data_clean <- data_clean  %>%
  mutate(ingreso_hora_median = ifelse(is.na(ingreso_hora) == TRUE, median(data_clean$ingreso_hora, na.rm = TRUE) , ingreso_hora))

mean(data_clean$ingreso_hora_median, na.rm=TRUE)
median(data_clean$ingreso_hora_median, na.rm=TRUE)

# La mediana cambia un poco más la distribución que la regresión, así que imputamos por regresión
data_clean <- data_clean %>% 
  mutate(ingreso_hora=ingreso_hora_implm)

## Imputacion por regresión para ingreso por hora
ingreso_total_lm <- lm(ingreso_total ~  sexo + edad + nivel_educ_max + estrato , data = data_clean)
summary(ingreso_total_lm)

data_clean$ingreso_total_pred <- predict(ingreso_total_lm, newdata = data_clean)

data_clean<-  data_clean %>%  
  mutate(ingreso_total = ifelse(is.na(ingreso_total) == TRUE, ingreso_total_pred , ingreso_total))

# Eliminar filas donde no se pudo imputar
data_clean <- data_clean %>% filter(!is.na(ingreso_hora))

# Calcular moda de la educación
mode_edu <- as.numeric(names(sort(table(data_clean$nivel_educ_max), decreasing = TRUE)[1]))

# Imputar missing
data_clean <- data_clean %>%
  mutate(nivel_educ_max = ifelse(is.na(nivel_educ_max), mode_edu, nivel_educ_max))

data_clean <- data_clean %>%
  mutate(edu_factor = ifelse(is.na(nivel_educ_max), mode_edu, nivel_educ_max))
 
# Valores atipicos 

winsorize <- function(x, q = 0.99){
  up <- quantile(x, q, na.rm = TRUE)
  pmin(x, up)
}

data_clean <- data_clean %>%
  mutate(
    ingreso_hora_w = winsorize(ingreso_hora),
    edad_w             = winsorize(edad),
    experiencia_w      = winsorize(experiencia_anualizada),
    horas_trab_w       = winsorize(horas_trab),
    salario_real_hora_w     = winsorize(salario_real_hora),
    ingreso_laboral_hora_w  = winsorize(ingreso_laboral_hora)
    
  )

# Probar winsor arriba y abajo
# Función para winsorizar ambos extremos de la distribución
winsorizer <- function(x, probs = c(0.01, 0.99)) {
  quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

# Crear versiones winsorizadas 
data_clean <- data_clean %>%
  mutate(
    ing_h_winsor = winsorizer(ingreso_hora),
    ingreso_laboral_hora_winsor = winsorizer(ingreso_laboral_hora),
    salario_real_hora_winsor = winsorizer(salario_real_hora)
  )

# Creación de los logaritmos de los ingresos
data_clean <- data_clean %>%
  mutate(
    log_salario_real_hora_winsor = log(salario_real_hora_winsor),
    log_ingreso_laboral_hora_winsor = log(ingreso_laboral_hora_winsor),
    log_ing_h_winsor = log(ing_h_winsor)
  )

# Variables nuevas para el análisis
data_clean <- data_clean %>%
  mutate(Mujer = ifelse(sexo == 0, 1, 0)) 

data_clean <- data_clean %>%
  mutate(Edad2 = edad_w^2)

data_clean <- data_clean %>%
  mutate(ingresos_no_laborales = ingreso_total_observado-salario_mensual)

# Creación de variables de ocupación
# Definir el umbral de frecuencia y el código para la variable de ocupación y otros
table(data_clean$ocupacion)

data_clean <- data_clean %>%
  add_count(ocupacion_factor, name = "n_ocupacion") %>%
  mutate(ocupacion_o = ifelse(n_ocupacion >= 100, ocupacion_factor, 100))

table(data_clean$ocupacion_o)

table(data_clean$ocupacion_factor[data_clean$ocupacion_factor == 45])
table(data_clean$ocupacion_o)
typeof(data_clean$ocupacion_o)

data_clean <- data_clean %>%
  mutate(ocupacion_o = factor(ocupacion_o))

typeof(data_clean$ocupacion_o)

# Mostramos las frecuencias de los códigos en la nueva columna.
cat("--- Frecuencias de la nueva variable 'ocupacion_control' ---\n")
print(table(data_clean$ocupacion_o))

# Definir la variable de ocupaciones directivas y otras
# Definir la lista de códigos de oficio que consideramos directivos
cod_ocup_altos <- c(18, 30, 40, 42, 50, 51, 60, 70)

# Crear la variable dummy 'ocupacion_directiva'
# Si el 'oficio' está en nuestra lista de directivos, asigna 1. De lo contrario, 0.
data_clean <- data_clean %>%
  mutate(
    ocupacion_directiva = ifelse(ocupacion_factor %in% cod_ocup_altos, 1, 0)
  )

# Eliminamos variables innecesarias
data_clean <- data_clean %>%
  dplyr::select(-c(ingreso_hora_pred,ingreso_hora_implm,ingreso_hora_median, ingreso_total_pred))

# Exportar la base final
export(data_clean, store_file("base_final.rds"))

