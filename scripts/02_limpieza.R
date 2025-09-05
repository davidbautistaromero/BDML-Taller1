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
    age, sex, estrato1, p6050, depto,
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
    departamento              = depto,
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
    salario_real_hora         = y_salary_m_hu,#esta es solo la actividad principal
    ingreso_laboral_hora      = y_ingLab_m_ha #ingreso laboral por todas las ocupaciones
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

head(data_clean)

# -----------------------------------------------------------------------------
# 3. Revisar  valores faltantes
# -----------------------------------------------------------------------------

# Validamos que variables traen valores faltantes y graficamos
sapply(data_clean, function(x) sum(is.na(x)))

# Gráfica de missing para toda la base
png(filename = file.path("views", "grafica_missing_todas.png"), width = 1200, height = 1000)
vis_miss(data_clean) +
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
    ingreso_total, ingreso_hora, otros_ingresos,salario_mensual,
    experiencia_anualizada, dummy_jefe, salario_real_hora, ingreso_laboral_hora
  )

png(filename = file.path("views", "grafica_missing_clave.png"), width = 1000, height = 800)
vis_miss(data_missing) +
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

# Gráfica de las 5 variables con más missing
png(file.path("views", "graf_missing_var_prin.png"), width = 800, height = 600)
ggplot(head(db_miss, 5), aes(x = reorder(skim_variable, +p_missing), y = p_missing)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 5 Variables con más Missing", x = "Variables", y = "Proporción de Missing") +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"))
dev.off()



# Eliminamos la variable otros_ingresos ya que como vemos en la gráfica la mayoria
# de sus datos son missing

data_clean <- data_clean %>%
  dplyr::select(-otros_ingresos)


# Ahora vamos a analizar que haremos con el resto de variables con gran proporción
# de missings

# Analisis de sensibilidad, vemos la media antes y después de la transformación 

mean(data_clean$ingreso_hora, na.rm=TRUE)

##Cambiar a regresion
data_clean <- data_clean %>%
  group_by(ocupacion, trabajador_independiente) %>%
  mutate(ingreso_hora = ifelse(is.na(ingreso_hora), mean(ingreso_hora, na.rm = TRUE), ingreso_hora)) %>%
  ungroup()

mean(data_clean$ingreso_hora, na.rm=TRUE)


mean(data_clean$ingreso_total, na.rm=TRUE)

##Cambiar a regresion
data_clean <- data_clean %>%
  group_by(ocupacion, trabajador_independiente) %>%
  mutate(ingreso_total = ifelse(is.na(ingreso_total), mean(ingreso_total, na.rm = TRUE), ingreso_hora)) %>%
  ungroup()

mean(data_clean$ingreso_total, na.rm=TRUE)

#### La media cambia mucho con estos arreglos. Parece mejor la regresión
### Validar método de imputación para el salario


# Eliminar filas donde no se pudo imputar
data_clean <- data_clean %>% filter(!is.na(ingreso_hora))

# Calcular moda de la educación
mode_edu <- as.numeric(names(sort(table(data_clean$nivel_educ_max), decreasing = TRUE)[1]))

# Imputar missing
data_clean <- data_clean %>%
  mutate(nivel_educ_max = ifelse(is.na(nivel_educ_max), mode_edu, nivel_educ_max))


summary(data_clean$ingreso_hora_imp) 

 
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

#probar winsor arriba y abajo
# Función para winsorizar ambaos extremos de la distribuión
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

##Creación de los logaritmos de los ingresos##
data_clean <- data_clean %>%
  mutate(
    log_salario_real_hora_winsor = log(salario_real_hora_winsor),
    log_ingreso_laboral_hora_winsor = log(ingreso_laboral_hora_winsor),
    log_ing_h_winsor = log(ing_h_winsor)
  )


# Variables nuevas para el análisis

data_clean <- data_clean %>%
  mutate(
    log_ing_h = log(ingreso_hora),
    log_ing_h_win = log(ingreso_hora_w),
    log_salario_real_hora_w = log(salario_real_hora_w),
    log_ingreso_laboral_hora_w = log(ingreso_laboral_hora_w)
  )

data_clean <- data_clean %>%
  mutate(
    ocupacion_factor = as.factor(ocupacion),
    edu_factor      = as.factor(nivel_educ_max),
    estrato_factor  = as.factor(estrato),
    tfirma_factor   = as.factor(tamanio_empresa)
  )

data_clean <- data_clean %>%
  mutate(Mujer = ifelse(sexo == 0, 1, 0)) #No necesario, creo #Nico: yo si lo veo necesario, para la interpretación, es decir, que el beta nos de el efecto porcentual de ser mujer para el punto 4

data_clean <- data_clean %>%
  mutate(Edad2 = edad_w^2)

data_clean <- data_clean %>%
  mutate(ingresos_no_laborales = ingreso_total_observado-salario_mensual)


# Exportar la base final
export(data_clean, store_file("base_final.rds"))

