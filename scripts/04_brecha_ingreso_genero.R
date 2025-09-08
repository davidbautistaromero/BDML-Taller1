# =============================================================================
# Script_04: Mismo pago para el mismo trabajo 04_brecha_ingreso_genero
# =============================================================================
# Objetivos:
# - Encontrar la correlación entre el ingreso total frente a ser mujer en Bogotá para el año 2018.
# - Desarrollar una discución teorica sobre cuales son los controles que son capaces de abordar esta idea de manera economicamente correcta.
# - Comparar metodos de estimación con metodos de predicción
# - Graficar 
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Importamos los datos procesados en la etapa 02 (limpieza)
base_final <- import(here::here("stores", "base_final.rds"))
attach(base_final)

#Comparamos las diferentes variables objetivo que son los ingresos

#summary(ingreso_hora) #sin winsor
#summary(ingreso_hora_w) #winsor a la derecha
#summary(ing_h_winsor) #winsor en las dos colas

#summary(ingreso_laboral_hora) #sin winsor
#summary(ingreso_laboral_hora_w) #winsor a la derecha
#summary(ingreso_laboral_hora_winsor) #winsor en las dos colas

#summary(salario_real_hora) #sin winsor
#summary(salario_real_hora_w) #winsor a la derecha
#summary(salario_real_hora_winsor) #winsor en las dos colas

#observamos que la variable salario real  e ingreso laboral difieren en su media.

#para solucionar esto, usaremos la variable que representa mejor la idea de pago igual por trabajos iguales
#esta variable es en nuestro analisis "salario_real_hora" en sus transformación.
#en el documento se desarrolla teoricamente el porque de esta elección.

# Inspección inicial
names(base_final)        # Nombres de variables
str(base_final)          # Tipos de variables


# 4a. Brecha Incondicional

#lm(log_ing_h_win ~ Mujer, data = base_final)
#lm(log_ingreso_laboral_hora_w ~ Mujer, data = base_final)
#lm(log_salario_real_hora_w ~ Mujer, data = base_final)

#lm(log(ingreso_hora) ~ Mujer, data = base_final)
#lm(log(ingreso_laboral_hora) ~ Mujer, data = base_final)
#lm(log(salario_real_hora) ~ Mujer, data = base_final)

#lm(log(ing_h_winsor) ~ Mujer, data = base_final)
#lm(log(ingreso_laboral_hora_winsor) ~ Mujer, data = base_final)
#lm(log(salario_real_hora_winsor) ~ Mujer, data = base_final)

modelo_unicaocup <- lm(log(salario_real_hora_winsor) ~ Mujer, data = base_final)
#modelo_multiocup <- lm(log(ingreso_laboral_hora_winsor) ~ Mujer, data = base_final)
#modelo_ingresotot <- lm(log(ing_h_winsor) ~ Mujer, data = base_final)

# 4b. Brecha Condicional con FWL

#Definimos los controles que consideramos buenos omitiendo los malos
#¿hablamos de los malos?


###############################################################################
# Estimar los cuatro modelos de regresión
# Modelo 1: Sin control por ocupación
#m1 <- lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, data = base_final)

# Modelo 2: Control por 'ocupacion_o' todas las ocupaciones
m_controles <- lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), data = base_final)

# Modelo 3: Control por 'ocupacion_directiva' Variable creada mediante selección 
#m3 <- lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + ocupacion_directiva, data = base_final)

# Modelo 4: Control por 'tipo_ocupacion' una variable que habla del tipo de la ocupación ->descartado
#m4 <- lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(tipo_ocupacion), data = base_final)



#######


# --- Comparación con el modelo completo para verificar ---

# Estimamos el modelo completo usando lm()
model_completo <- lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), data = base_final)
m_full <- resid(model_completo)


########################################################################################
#### salario_real_hora tiene menos observaciones y para que el FWL se pueda multiplicar matricialmente, se debe acortar la base por las dimenciones
base_final <- base_final %>% filter(!is.na(log_salario_real_hora_winsor))

# === Matriz de controles X (sin Mujer) ===
form_X <- ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o)
X_mat <- model.matrix(form_X, data = base_final)

# === Regresión de Mujer en X ===
m_Mujer_X <- lm(Mujer ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), data = base_final)
res_Mujer <- resid(m_Mujer_X)

# === 5. Regresión de log_salario en X ===
m_Y_X <- lm(log_salario_real_hora_winsor ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), data = base_final)
res_Y <- resid(m_Y_X)

# === 6. Regresión FWL (resid_Y ~ resid_Mujer) ===
m_FWL <- lm(res_Y ~ res_Mujer -1 )  # +0 evita intercepto redundante
summary(m_FWL)

########################################################################################

# Recrear la base de datos para asegurar la replicabilidad del ejemplo
# En tu script, simplemente asegúrate de que 'base_final' esté cargada y limpia.
if (!exists("base_final")) {
  set.seed(10101)
  n <- 1000
  base_final <- tibble(
    Mujer = sample(0:1, n, replace = TRUE),
    edad = sample(18:65, n, replace = TRUE),
    nivel_educ_max = factor(sample(1:7, n, replace = TRUE)),
    tamanio_empresa = factor(sample(1:5, n, replace = TRUE)),
    trabajo_formal = sample(0:1, n, replace = TRUE),
    log_salario_real_hora_w = 2 + (-0.15 * Mujer) + (0.05 * edad) + (-0.0005 * edad^2) +
      as.numeric(nivel_educ_max) * 0.1 + as.numeric(tamanio_empresa) * 0.08 +
      (0.2 * trabajo_formal) + rnorm(n, 0, 0.5)
  ) %>% mutate(Edad2 = edad^2) %>%
    filter(!is.na(log_salario_real_hora_winsor)) # Importante: filtrar NAs
}

# Definir la función para el estimador FWL (para usar con boot)
# Esta función encapsula todo tu código anterior.
# Recibe los datos y los 'indices' de la muestra bootstrap actual.
fwl_bootstrap_function <- function(data, indices) {
  # Crear la muestra bootstrap del ciclo actual
  sample_data <- data[indices, ]
  
  # Definir la fórmula de los controles
  controls_formula <- log_salario_real_hora_winsor ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal
  
  # Paso 1: Residualizar Y (log_salario_real_hora_winsor)
  lm_y <- lm(update(controls_formula, log_salario_real_hora_winsor ~ .), data = sample_data)
  y_tilde <- residuals(lm_y)
  
  # Paso 2: Residualizar X (Mujer)
  lm_mujer <- lm(update(controls_formula, Mujer ~ .), data = sample_data)
  mujer_tilde <- residuals(lm_mujer)
  
  # Paso 3: Regresión final y extracción del coeficiente
  coef_fwl <- coef(lm(y_tilde ~ mujer_tilde - 1))[1]
  
  return(coef_fwl)
}

# 4. Ejecutar el bootstrap
set.seed(10101) # Semilla del taller para replicabilidad
bootstrap_fwl_results <- boot(
  data = base_final, 
  statistic = fwl_bootstrap_function, 
  R = 1000 # 1000 replicaciones es un buen estándar
)

# Asumo que los objetos 'model_completo', 'm_FWL', y 'bootstrap_fwl_results'
# ya existen en tu entorno de R.

# --- Inicio del Código para la Tabla Final del WFL ---

# 1. Extraer los resultados del Modelo Completo (OLS)
beta_full <- coef(model_completo)["Mujer"]
ci_full <- confint(model_completo, "Mujer", level = 0.95)

# 2. Extraer los resultados del Modelo FWL manual
beta_FWL <- coef(m_FWL)["res_Mujer"]
ci_FWL <- confint(m_FWL, "res_Mujer", level = 0.95)

# 3. Extraer los resultados del Bootstrap
beta_boot <- bootstrap_fwl_results$t0 # El coeficiente original
ci_boot_obj <- boot.ci(bootstrap_fwl_results, type = "perc", index = 1)
summary(bootstrap_fwl_results)
# Extraer los límites inferior y superior del intervalo de percentiles
ci_boot <- ci_boot_obj$percent[4:5] 

# 4. Crear una tabla comparativa de las tres estimaciones
tabla_comparativa_final <- data.frame(
  Estimador = c("1. Modelo Completo (OLS)", 
                "2. Modelo FWL (Manual)", 
                "3. FWL con Bootstrap (Robusto)"),
  Beta_Mujer = c(beta_full, beta_FWL, beta_boot),
  Límite_Inferior_95 = c(ci_full[1], ci_FWL[1], ci_boot[1]),
  Límite_Superior_95 = c(ci_full[2], ci_FWL[2], ci_boot[2])
)

# 5. Imprimir la tabla final bien formateada
cat("--- Tabla Comparativa Final de Estimadores de la Brecha de Género ---\n")
print(tabla_comparativa_final, row.names = FALSE)

# 3. Crear el objeto xtable y generar el código LaTeX
xtable_obj <- xtable(
  tabla_comparativa_final,
  caption = "Comparación Final de Estimadores para la Brecha de Género",
  label = "tab:comparacion_final",
  digits = 4 # Controlar el número de decimales
)

# 4. Imprimir el código LaTeX en la consola
# 'comment = FALSE' elimina la marca de tiempo que xtable añade por defecto
print(xtable_obj, type = "latex", comment = FALSE, include.rownames = FALSE)

############################################################################
########################## tabla regresiones ################################
# 4. Crear la fila personalizada para los controles de ocupación
# Esta es una lista donde cada elemento es un vector que representa una fila en la tabla.
# El primer elemento del vector es el nombre de la fila, y los siguientes son los valores para cada columna.
especificacion <- list(c("Especificación", "Sin Controles", "Con los controles", "FWL Manual", "FWL Bootstrap"))

# 5. Generar la tabla stargazer para LaTeX
stargazer(modelo_unicaocup, m_controles, m_FWL, modelo_unicaocup,
          type = "latex",
          title = "Análisis de la Brecha Salarial de Género con Diferentes especificaciones",
          
          # --- Argumentos clave para la personalización ---
          keep = c("Mujer"), # Solo muestra el coeficiente de la variable 'Mujer'
          covariate.labels = c("Mujer"), # Etiqueta para la variable 'Mujer'
          add.lines = especificacion, # Añade la fila personalizada
          
          # --- Formato y etiquetas ---
          dep.var.labels.include = FALSE, # Oculta los nombres de la variable dependiente
          dep.var.caption = "Variable Dependiente: Log(Salario Real por Hora)",
          column.labels = c("Modelo unconditional", "Con controles", "FWL", "Bootstrap"),
          notes = "Errores estándar en paréntesis.",
          notes.align = "l",
          align = TRUE,
          
          # --- Opciones para una tabla LaTeX más limpia ---
          header = FALSE, # Evita la cabecera por defecto de stargazer
          no.space = TRUE, # Elimina espacios extra entre filas
          float = TRUE, # Asegura que sea un entorno 'table' flotante
          font.size = "small", # Ajusta el tamaño de la fuente si es necesario
          out = file.path("views", "rmse_validatiion_set.tex")
)


################## GRAFICO ####################################
# Estimar el modelo con interacciones
# Usamos Mujer * (edad + Edad2) para que R incluya los términos principales y sus interacciones.
model_interaction <- lm(log_salario_real_hora_winsor ~ Mujer * (edad + Edad2) + 
                          factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), 
                        data = base_final)

# Presentar los resultados del modelo
stargazer(model_interaction, type = "text", title = "Modelo de Perfil Edad-Ingreso por Género")

# Función para calcular la "Edad Pico" para el Bootstrap
peak_age_function <- function(data, indices) {
  # Crear la muestra bootstrap
  sample_data <- data[indices, ]
  
  # Estimar el modelo en la muestra
  model <- lm(log_salario_real_hora_winsor ~ Mujer * (edad + Edad2) + 
                factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal + factor(ocupacion_o), 
              data = sample_data)
  
  coefs <- coef(model)
  
  # Edad pico para Hombres (Mujer = 0)
  # El perfil es: b_edad * edad + b_edad2 * edad^2
  peak_age_male <- -coefs["edad"] / (2 * coefs["Edad2"])
  
  # Edad pico para Mujeres (Mujer = 1)
  # El perfil es: (b_edad + b_interaccion_edad) * edad + (b_edad2 + b_interaccion_edad2) * edad^2
  b_age_female <- coefs["edad"] + coefs["Mujer:edad"]
  b_age2_female <- coefs["Edad2"] + coefs["Mujer:Edad2"]
  peak_age_female <- -b_age_female / (2 * b_age2_female)
  
  return(c(peak_age_male = peak_age_male, peak_age_female = peak_age_female))
}

# 5. Ejecutar el bootstrap para la edad pico
set.seed(10101) # Para replicabilidad
peak_age_bootstrap <- boot(
  data = base_final, 
  statistic = peak_age_function, 
  R = 1000 # 1000 replicaciones
)

# 6. Presentar los resultados de la edad pico y sus intervalos de confianza
cat("--- Edad Pico Estimada y sus Intervalos de Confianza (Bootstrap) ---\n")
print(peak_age_bootstrap)

cat("\n--- Intervalo de Confianza al 95% para la Edad Pico (Hombres) ---\n")
boot.ci(peak_age_bootstrap, type = "perc", index = 1)

cat("\n--- Intervalo de Confianza al 95% para la Edad Pico (Mujeres) ---\n")
boot.ci(peak_age_bootstrap, type = "perc", index = 2)

# 7. Graficar los perfiles de ingreso predichos
# Crear un grid de datos para predecir
new_data <- expand.grid(
  edad = seq(18, 65, by = 1),
  Mujer = c(0, 1),
  # Mantenemos los otros controles en sus valores promedio/modales
  nivel_educ_max = 7,
  tamanio_empresa = 5,
  trabajo_formal = 1,
  ocupacion_o = 36
)
new_data$Edad2 <- new_data$edad^2
# Predecir los salarios
new_data$predictions_with_ci <- predict(model_interaction, newdata = new_data, interval = "confidence", level = 0.95)
predictions_with_ci <- predict(model_interaction, newdata = new_data, interval = "confidence", level = 0.95)
df.predict.peak.age<-as.data.frame(predictions_with_ci)
names(df.predict.peak.age)

new_data$fit <- df.predict.peak.age$fit
new_data$lwr <- df.predict.peak.age$lwr
new_data$upr <- df.predict.peak.age$upr


##################### Exportar la base final #################################
export(new_data, store_file("edad_pico.rds"))
##########################

# 1. Definir las edades pico a partir de tus resultados del bootstrap
peak_age_hombres <- 61.18792
peak_age_mujeres <- 57.15148

# 2. Código para la gráfica
png(file.path("views", "graf_edad_pico_intervalos.png"), width = 800, height = 600)

ggplot(new_data, aes(x = edad, y = fit, color = factor(Mujer), fill = factor(Mujer))) +
  
  # Capa para el intervalo de confianza (la cinta sombreada)
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  
  # Capa para la línea de predicción puntual
  geom_line(linewidth = 1.2) +
  
  # --- NUEVAS LÍNEAS VERTICALES ---
  # Línea para la edad pico de los hombres
  geom_vline(xintercept = peak_age_hombres, linetype = "dashed", color = "#0072B2", linewidth = 1) +
  
  # Línea para la edad pico de las mujeres
  geom_vline(xintercept = peak_age_mujeres, linetype = "dashed", color = "#D55E00", linewidth = 1) +
  
  # --- ETIQUETAS PARA LAS LÍNEAS VERTICALES ---
  annotate("text", 
           x = peak_age_hombres, 
           y = min(new_data$lwr), # Posición en el eje Y (abajo)
           label = paste("Pico Hombres\n", round(peak_age_hombres, 1), "años"), 
           vjust = -0.5, # Ajuste vertical para que no se pegue a la línea
           color = "#0072B2",
           fontface = "bold") +
  
  annotate("text", 
           x = peak_age_mujeres, 
           y = min(new_data$lwr), # Posición en el eje Y (abajo)
           label = paste("Pico Mujeres\n", round(peak_age_mujeres, 1), "años"), 
           vjust = -1.5, # Ajuste vertical diferente para evitar solapamiento
           color = "#D55E00",
           fontface = "bold") +
  
  # Etiquetas y formato general
  labs(
    title = "Perfil Edad-Ingreso Predicho por Género con Edades Pico",
    x = "Edad",
    y = "Logaritmo del Salario por Hora (Predicho)",
    color = "Género",
    fill = "Género" # Asegúrate de incluir 'fill' en labs si lo usas en aes()
  ) +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Hombres", "Mujeres")) +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Hombres", "Mujeres")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

dev.off()
