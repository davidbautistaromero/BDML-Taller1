# =============================================================================
# Script_05: Predicciones sobre el salario
# =============================================================================
# Objetivos:
# - Encontrar el mejor modelo (entre un subset específico) de la predicción del 
# - Desarrollar una discución teorica sobre cuales son los controles que son capaces de abordar esta idea de manera economicamente correcta.
# - Comparar metodos de estimación con metodos de predicción
# - Graficar 
# =============================================================================

# Cargar la configuración base

source(here::here("scripts", "00_Config.R"))

# Importamos los datos procesados en la etapa 02 (limpieza)
base_final <- import(here::here("stores", "base_final.rds"))
attach(base_final)

# la base de datos final tiene algunos nulos en la variable dependiente
pred_df <- base_final %>%
  filter(!is.na(log_salario_real_hora_winsor))


# ==============================================================================

# 1. Validation Set Approach

# ==============================================================================

# Dividimos el dataset resultante en un set de prueba (30%) y otro de entrenamiento (70%)

set.seed(10101)   

inTrain <- createDataPartition(
  y = pred_df$log_salario_real_hora_winsor,  
  p = .70, 
  list = FALSE
)

train <- pred_df %>% filter(row_number() %in% inTrain)
test  <- pred_df %>% filter(!(row_number() %in% inTrain))


# ==============================================================================

# 2. Entrenamiento de modelos

# ==============================================================================

# Entrenamos los modelos de los dos puntos previos

model1 <- lm(log_salario_real_hora_winsor ~ edad + Edad2 , data = train)

formula2 <- log_salario_real_hora_winsor ~ Mujer + edad + Edad2 +edu_factor +tfirma_factor + trabajo_formal + ocupacion_o
model2 <- lm(formula2, data = train)


# Entrenamos 5 modelos diferentes 

## Agregamos algunas otras variables 

formula3 <- log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + edu_factor + tfirma_factor + trabajo_formal + 
  ocupacion_o + log_ing_h_winsor + estrato_factor

model3 <- lm(formula3, data=train)


## Polinomios de 4 orden para los regresores originales

formula4 <- log_salario_real_hora_winsor ~ Mujer + 
  poly(edad,4, raw = TRUE) + poly(log_ing_h_winsor,4, raw = TRUE) +
  edu_factor + tfirma_factor + trabajo_formal + ocupacion_o + estrato_factor
model4 <- lm(formula4, data=train)

## Polinomios de 4 orden mas interaccion con genero

formula5 <- log_salario_real_hora_winsor ~ Mujer + 
  poly(edad,4, raw = TRUE) + poly(edad,4, raw = TRUE):Mujer +
  poly(log_ing_h_winsor,4, raw = TRUE) + poly(log_ing_h_winsor,4, raw = TRUE):Mujer +
  edu_factor + tfirma_factor + trabajo_formal + ocupacion_o + estrato_factor
model5 <- lm(formula5, data=train)

## Polinomios de 4 orden con interacciones entre ellos

formula6 <- log_salario_real_hora_winsor ~ Mujer + 
  poly(edad,4, raw = TRUE) + poly(edad,4, raw = TRUE):Mujer +
  poly(log_ing_h_winsor,4, raw = TRUE) + poly(log_ing_h_winsor,4, raw = TRUE):Mujer +
  poly(edad,4, raw = TRUE):poly(log_ing_h_winsor,4, raw = TRUE) +
  edu_factor + tfirma_factor + trabajo_formal + ocupacion_o +  estrato_factor
model6 <- lm(formula6, data=train)

## Agregamos más variables al modelo 3

formula7 <- log_salario_real_hora_winsor ~ Mujer + edad + Edad2 + edu_factor + tfirma_factor + trabajo_formal + 
  ocupacion_o + log_ing_h_winsor + estrato_factor + poly(experiencia_anualizada,2,raw=TRUE) + 
  horas_trab + factor(cotiza_pension) + dummy_jefe 
model7 <- lm(formula7,data=train)


# ==============================================================================

# 3. Validación del performance de los modelos

# ==============================================================================

## Calculamos los rmse de cada modelo 

### Modelo 1
pred1 <- predict(model1, newdata = test)
rmse1 <- RMSE(pred1, test$log_salario_real_hora_winsor)


### Modelo 2
pred2 <- predict(model2, newdata = test)
rmse2 <- RMSE(pred2, test$log_salario_real_hora_winsor)


### Modelo 3
pred3 <- predict(model3, newdata = test)
rmse3 <- RMSE(pred3, test$log_salario_real_hora_winsor)


### Modelo 4
pred4 <- predict(model4, newdata = test)
rmse4 <- RMSE(pred4, test$log_salario_real_hora_winsor)


### Modelo 5
pred5 <- predict(model5, newdata = test)
rmse5 <- RMSE(pred5, test$log_salario_real_hora_winsor)


### Modelo 6
pred6 <- predict(model6, newdata = test)
rmse6 <- RMSE(pred6, test$log_salario_real_hora_winsor)


### Modelo 7
pred7 <- predict(model7, newdata = test)
rmse7 <- RMSE(pred7, test$log_salario_real_hora_winsor)

rmse_tbl <- data.frame(
  Modelo = c("modelo 1",
             "modelo 2",
             "modelo 3",
             "modelo 4",
             "modelo 5",
             "modelo 6",
             "modelo 7"),
  RMSE_Test = c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7)
)

stargazer(rmse_tbl, type = "text", summary = FALSE, rownames = FALSE,
          digits=5, title = "RMSE en Set de Prueba")


## El modelo con menor RMSE es el modelo 4 

# ==============================================================================

# 4.Analisis de predicciones

# ==============================================================================

test <- test %>%
  mutate(predictions = pred4)

test <- test %>% 
  mutate(pred_errors = log_salario_real_hora_winsor - predictions)


# Distribucion del ingreso por hora
png(filename = file.path("views", "distribucion_error_pred.png"), width = 1000, height = 800)
ggplot(test, aes(pred_errors)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = quantile(test$pred_errors, probs = 0.025, na.rm = TRUE, names = FALSE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = quantile(test$pred_errors, probs = 0.975, na.rm = TRUE, names = FALSE), linetype = "dashed", color = "red") +  
  ggtitle("Distribucion de los errores de predicción") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
dev.off()

# ==============================================================================

# 5. LOOCV

# ==============================================================================

## El modelo 4 y el modelo 5 presentaron los menores RMSE

loocv <- trainControl(
  method = "LOOCV")

## LOOCV para modelo 4

modelo4cv <- train(formula4,
                  data = pred_df,
                  method = 'lm', 
                  trControl= loocv)

rmse_m4loocv <- modelo4cv$results$RMSE[1]

## LOOCV para modelo 5

modelo5cv <- train(formula5,
                   data = pred_df,
                   method = 'lm', 
                   trControl= loocv)

rmse_m5loocv <- modelo5cv$results$RMSE[1]


## Calculamos el LOOCV mediante una iteración usando el leverage

## Para el modelo 4

model4_full <- lm(formula4, data=pred_df)
predictions_4 <- predict(model4_full, new_data=pred_df)

pred_df <- pred_df %>%
  mutate(predictions_m4=predictions_4)

pred_df <- pred_df %>%
  mutate(residuals_m4=model4_full$residuals)

pred_df <- pred_df %>%
  mutate(leverage4 = hatvalues(model4_full))

residuos_loo4 <- pred_df$residuals_m4/(1-pred_df$leverage4)

rmse_m4loocv_it1 <- sqrt(mean(residuos_loo^2))


## Para el modelo 5

model5_full <- lm(formula5, data=pred_df)
predictions_5 <- predict(model5_full, new_data=pred_df)

pred_df <- pred_df %>%
  mutate(predictions_m5=predictions_5)

pred_df <- pred_df %>%
  mutate(residuals_m5=model5_full$residuals)

pred_df <- pred_df %>%
  mutate(leverage5 = hatvalues(model5_full))

residuos_loo5 <- pred_df$residuals_m5/(1-pred_df$leverage5)

rmse_m5loocv_it1 <- sqrt(mean(residuos_loo5^2))

##objeto con los resultados

final_results <- data.frame(
  Modelo = c("Modelo 4", "Modelo 5"),
  VS_approach = c(rmse4,rmse5),
  LOOCV = c(rmse_m4loocv, rmse_m5loocv),
  LOOCV_unica_iteracion = c(rmse_m4loocv_it1,rmse_m5loocv_it1)
)

stargazer(final_results, type = "text", summary = FALSE, rownames = FALSE,
          digits=5, title = "Comparación RMSE: Validation Set vs LOOCV")

# ==============================================================================

# 6. Exportar Bases de Datos y Resultados

# ==============================================================================

## Tabla RMSE del validation set

stargazer(rmse_tbl, type = "latex", summary = FALSE, rownames = FALSE,
          digits=5, title = "RMSE en Set de Prueba" ,
          out = file.path("views", "rmse_validation_set.tex"))

## Table RMSE Validation Set y LOOCV
stargazer(final_results, type = "latex", summary = FALSE, rownames = FALSE,
          digits=5, title = "Comparación RMSE: Validation Set vs LOOCV",
          out = file.path("views", "RMSE_LOOCV.tex"))

# Exportar la base completa, train y test
export(pred_df, store_file("base_predictions.rds"))
export(train, store_file("train_set.rds"))
export(test, store_file("test_set.rds"))








