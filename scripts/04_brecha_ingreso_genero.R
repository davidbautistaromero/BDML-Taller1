# =============================================================================
# Script_04: Mismo pago apra el mismo trabajo 04_brecha_ingreso_genero
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


#probar winsor arriba y abajo
# Función para winsorizar ambaos extremos de la distribuión
winsorizer <- function(x, probs = c(0.01, 0.99)) {
  quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

# Crear versiones winsorizadas 
base_final <- base_final %>%
  mutate(
    ing_h_winsor = winsorizer(ingreso_hora),
    ingreso_laboral_hora_winsor = winsorizer(ingreso_laboral_hora),
    salario_real_hora_winsor = winsorizer(salario_real_hora)
  )

##Creación de los logaritmos de los ingresos##
base_final <- base_final %>%
  mutate(
    log_salario_real_hora_winsor = log(salario_real_hora_winsor),
    log_ingreso_laboral_hora_winsor = log(ingreso_laboral_hora_winsor),
    log_ing_h_winsor = log(ing_h_winsor)
  )

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
#en el documento se desarrolla teoricamente el por que de esta elección.

# Inspección inicial
names(base_final)        # Nombres de variables
str(base_final)          # Tipos de variables
attach(base_final)

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
modelo_multiocup <- lm(log(ingreso_laboral_hora_winsor) ~ Mujer, data = base_final)
modelo_ingresotot <- lm(log(ing_h_winsor) ~ Mujer, data = base_final)

# 4. Generar la tabla con stargazer
stargazer(modelo_unicaocup,modelo_multiocup, modelo_ingresotot, 
          type = "text",
          title = "Comparación de la Brecha Salarial Incondicional con Diferentes Mediciones de Ingreso por Hora",
          dep.var.labels.include = FALSE, # No incluir los nombres largos de las variables dependientes
          column.labels = c("Log(Ing. ocupación principal)", "Log(Ing. todas las ocupaciones)", "Log(total ingresos)"),
          covariate.labels = c("Mujer", "Constante"),
          notes = "Errores estándar en paréntesis.",
          align = TRUE)

# 4b. Brecha Condicional con FWL

#Definimos los controles que consideramos buenos omitiendo los malos
#¿hablamos de los malos?

lm(log_salario_real_hora_winsor ~ Mujer + edad + Edad2 +factor(nivel_educ_max) +factor(tamanio_empresa) + trabajo_formal)
# --- Comparación con el modelo completo para verificar ---

# Estimamos el modelo completo usando lm()
model_completo <- lm(log_salario_real_hora_w ~ Mujer + edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, data = base_final)
m_full <- resid(model_completo)


########################################################################################
base_final <- base_final %>% filter(!is.na(log_salario_real_hora_w))

# === 3. Matriz de controles X (sin Mujer) ===
form_X <- ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal
X_mat <- model.matrix(form_X, data = base_final)

# === 4. Regresión de Mujer en X ===
m_Mujer_X <- lm(Mujer ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, data = base_final)
res_Mujer <- resid(m_Mujer_X)

# === 5. Regresión de log_salario en X ===
m_Y_X <- lm(log_salario_real_hora_w ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, data = base_final)
res_Y <- resid(m_Y_X)

# === 6. Regresión FWL (resid_Y ~ resid_Mujer) ===
m_FWL <- lm(res_Y ~ res_Mujer -1 )  # +0 evita intercepto redundante
summary(m_FWL)

# === 7. Comparar coeficientes ===
cbind(
  beta_full = coef(m_full)["Mujer"],
  beta_FWL  = coef(m_FWL)["res_Mujer"]
)


########################################################################################
# 1. Cargar librerías necesarias
# Nos aseguramos de tener 'boot' para el procedimiento de bootstrap.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, boot, stargazer)

# 2. Recrear la base de datos para asegurar la replicabilidad del ejemplo
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
    filter(!is.na(log_salario_real_hora_w)) # Importante: filtrar NAs
}

# 3. Definir la función para el estimador FWL (para usar con boot)
# Esta función encapsula todo tu código anterior.
# Recibe los datos y los 'indices' de la muestra bootstrap actual.
fwl_bootstrap_function <- function(data, indices) {
  # Crear la muestra bootstrap del ciclo actual
  sample_data <- data[indices, ]
  
  # Definir la fórmula de los controles
  controls_formula <- log_salario_real_hora_w ~ edad + Edad2 + factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal
  
  # Paso 1: Residualizar Y (log_salario_real_hora_w)
  lm_y <- lm(update(controls_formula, log_salario_real_hora_w ~ .), data = sample_data)
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

# 5. Presentar los resultados del bootstrap
cat("--- Resultados del Bootstrap para el Coeficiente de 'Mujer' ---\n")
print(bootstrap_fwl_results)

# 6. Calcular y presentar los intervalos de confianza
cat("\n--- Intervalos de Confianza al 95% (Método de Percentiles) ---\n")
boot.ci(bootstrap_fwl_results, type = "perc")

################## GRAFICO ####################################
# Estimar el modelo con interacciones
# Usamos Mujer * (edad + Edad2) para que R incluya los términos principales y sus interacciones.
model_interaction <- lm(log_salario_real_hora_w ~ Mujer * (edad + Edad2) + 
                          factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, 
                        data = base_final)

# Presentar los resultados del modelo
stargazer(model_interaction, type = "text", title = "Modelo de Perfil Edad-Ingreso por Género")

# Función para calcular la "Edad Pico" para el Bootstrap
peak_age_function <- function(data, indices) {
  # Crear la muestra bootstrap
  sample_data <- data[indices, ]
  
  # Estimar el modelo en la muestra
  model <- lm(log_salario_real_hora_w ~ Mujer * (edad + Edad2) + 
                factor(nivel_educ_max) + factor(tamanio_empresa) + trabajo_formal, 
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
  nivel_educ_max = factor(floor(mean(as.numeric(base_final$nivel_educ_max)))),
  tamanio_empresa = factor(floor(mean(as.numeric(base_final$tamanio_empresa)))),
  trabajo_formal = 1
)
new_data$Edad2 <- new_data$edad^2

# Predecir los salarios
new_data$predicted_log_wage <- predict(model_interaction, newdata = new_data)

# Graficar con ggplot2

png(file.path("views", "graf_edad_pico.png"), width = 800, height = 600)
ggplot(new_data, aes(x = edad, y = predicted_log_wage, color = factor(Mujer))) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Perfil Edad-Ingreso Predicho por Género",
    x = "Edad",
    y = "Logaritmo del Salario por Hora (Predicho)",
    color = "Género"
  ) +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Hombres", "Mujeres")) +
  theme_minimal(base_size = 14)
dev.off()

#para observar la edad pico con el ingreso, se selecciona el mayor 
summary(new_data)

##################### Exportar la base final #################################
export(base_final, store_file("base_final.rds"))


