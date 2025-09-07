# =============================================================================
# Script_03: Perfil de edad que maximiza el ingreso 
# =============================================================================
# Objetivos:
# - Estimar una regresión que permita capturar la relacion que existe entre la edad de los 
# trabajadores y su remuneración 
# - Discutir el ajuste del modelo
# - Graficar el perfil de ganancias estimadas y calcular la edad donde se maximiza el ingreso.
# - Emplear Bootsrap para calcular los intervalos de confianza de la edad que maximiza 
# el ingreso
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Importamos los datos procesados en la etapa 02 (limpieza)
base_final <- import(here::here("stores", "base_final.rds"))
attach(base_final)
names(base_final) 
# 1. estimar el modelo que relaciona el ingreso potencial del indivicuo en horas en funcion 
# a su edad. La regresion 1 incluye los ingresos relacionados con la ocupacion pri
# cipal del individuo. La regresion 2 incluye los ingresos obtenidos de varias ocupaciones y 
# la regresion 3 involucra el ingreso total de los individuos.


reg_1 = lm(log_salario_real_hora_w ~ edad + Edad2 , data = base_final)
reg_2 = lm(log_ingreso_laboral_hora_winsor ~ edad + Edad2, data = base_final)
reg_3= lm(log_ing_h ~ edad + Edad2, data = base_final)

# 2. Calcular la edad que maximiza el ingreso en cada modelo 

max_reg1 = -coef(reg_1)["edad"]/(2*coef(reg_1)["Edad2"])
max_reg2 = -coef(reg_2)["edad"]/(2*coef(reg_2)["Edad2"])
max_reg3 = -coef(reg_3)["edad"]/(2*coef(reg_3)["Edad2"])

# 3. Visualizar los resultados con stargazer
stargazer(reg_1, reg_2, reg_3, 
          type = "text",
          title = "Relacion entre el ingreso y la edad",
          dep.var.labels.include = FALSE, # No incluir los nombres largos de las variables dependientes
          column.labels = c("Log(Ing. ocupación principal)", "Log(Ing. todas las ocupaciones)", "Log(total ingresos)"),
          notes = "Errores estándar en paréntesis.",
          add.lines = list(c("Edad que maximiza el ingreso", round(max_reg1), round(max_reg2), round(max_reg3))),
          align = TRUE)

#4. Graficar los resultados. 

rango_edad = seq(min(base_final$edad), max(base_final$edad), by =1 ) # Eje x del grafico
grafico = data.frame(edad = rango_edad, Edad2 = rango_edad^2) # df para almacenar datos del grafico

# agregar los valores predichos a la base del grafico 
pred_1 = predict(reg_1, newdata = grafico, interval = "confidence")
pred_2 = predict(reg_2, newdata = grafico, interval = "confidence")
pred_3 = predict(reg_3, newdata = grafico, interval = "confidence")

# emplear ggplot2 para graficar 
grafico_long <- bind_rows(
  data.frame(edad = rango_edad,
             log_ingreso = pred_1[, "fit"],
             lower = pred_1[, "lwr"],
             upper = pred_1[, "upr"],
             modelo = "Ingresos de la ocupación principal"),
  data.frame(edad = rango_edad,
             log_ingreso = pred_2[, "fit"],
             lower = pred_2[, "lwr"],
             upper = pred_2[, "upr"],
             modelo = "Ingresos por todas las ocupaciones"),
  data.frame(edad = rango_edad,
             log_ingreso = pred_3[, "fit"],
             lower = pred_3[, "lwr"],
             upper = pred_3[, "upr"],
             modelo = "Total ingresos")
)
ggplot(grafico_long, aes(x = edad, y = log_ingreso, color = modelo)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = modelo), alpha = 0.2, color = NA) +
  labs(title = "Relación entre edad e ingresos laborales en Colombia",
       x = "Edad",
       y = "Logaritmo de los ingresos laborales",
       color = "Variable dependiente",
       fill = "Variable dependiente",
       caption = "Datos tomados de la GEIH (2018). Cálculos propios.") +
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#5. Calcular mediante bootstrap los intervalos de confianza de la edad que 
# maximiza el ingreso. 

# funcion que hace la estimacion. Luego el maximo de la funcion de ingreso, indexa 
# y devuleve el estimador de interes
eta1 = function(data, index){
  reg = lm(log_salario_real_hora_w ~ edad + Edad2 , data, subset = index)
  max_ingreso = -coef(reg)["edad"]/(2*coef(reg)["Edad2"])
  return(max_ingreso)
}

eta2 = function(data, index){
  reg = lm(log_ingreso_laboral_hora_winsor ~ edad + Edad2, data, subset = index)
  max_ingreso = -coef(reg)["edad"]/(2*coef(reg)["Edad2"])
  return(max_ingreso)
}

eta3 = function(data, index){
  reg = lm(log_ing_h ~ edad + Edad2, data, subset = index)
  max_ingreso = -coef(reg)["edad"]/(2*coef(reg)["Edad2"])
  return(max_ingreso)
}

# Probemos que la funcion funcione bien 
eta1(base_final, 1:nrow(base_final))==max_reg1
eta2(base_final, 1:nrow(base_final))==max_reg2
eta3(base_final, 1:nrow(base_final))==max_reg3

# Calcular los errores esstandar usando bootstrap y la funcion boot
boot(base_final, eta1, R = 1000)
boot(base_final, eta2, R = 1000)
boot(base_final, eta3, R = 1000)
