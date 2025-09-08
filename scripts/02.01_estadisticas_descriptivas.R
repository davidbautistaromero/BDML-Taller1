# =============================================================================
# Script_02.01: Estadísticas Descriptivas
# =============================================================================
# Objetivos:
# - Explorar la base final limpia (Script_02)
# - Generar estadísticas descriptivas de variables clave
# - Producir tablas y gráficas autocontenidas
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Importar la base procesada
data_clean <- import(here::here("stores", "base_final.rds"))

# -----------------------------------------------------------------------------
# 1. Estadísticas generales
# -----------------------------------------------------------------------------

# Resumen global
skim(data_clean)

# Tabla de descriptivas ampliada
des_vars <- c("salario_real_hora_winsor",
              "edad_w", "horas_trab_w", "experiencia_w",
              "sexo", "estrato", "trabajador_independiente", 
              "trabajo_formal", "dummy_jefe")

nombres <- c("Salario por hora (winsorizado)", 
             "Edad (winsorizada)", 
             "Horas trabajadas (winsorizadas)", 
             "Experiencia (winsorizada)",
             "Sexo (Mujer)", 
             "Estrato", 
             "Trabajador independiente", 
             "Trabajo formal", 
             "Jefe de hogar")

stargazer(data_clean[des_vars],
          type = "text",
          title = "Tabla 1. Estadísticas Descriptivas",
          covariate.labels = nombres,
          digits = 1,
          out = here::here("views", "tabla_estadisticas.txt"))

# -----------------------------------------------------------------------------
# 2. Matriz de correlación
# -----------------------------------------------------------------------------

subset_corr <- data_clean[, c("salario_real_hora_winsor", 
                              "edad_w", "horas_trab_w", "experiencia_w")]

cor_matrix <- cor(subset_corr, use = "complete.obs")

png(filename = here::here("views", "matriz_correlaciones.png"), width = 800, height = 600)
corrplot(cor_matrix, method = "color", 
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8, tl.col = "black")
dev.off()

# -----------------------------------------------------------------------------
# 3. Gráficas descriptivas
# -----------------------------------------------------------------------------

g1 <- ggplot(data_clean, aes(x = salario_real_hora_winsor)) +
  geom_density(fill = "#619CFF", alpha = 0.4, color = "black") +
  geom_vline(aes(xintercept = mean(salario_real_hora_winsor, na.rm = TRUE)), 
             linetype = "dashed", color = "red") +
  labs(title = "Distribución del salario por hora", 
       x = "Salario por hora (winsorizado)", y = "Densidad") +
  theme_minimal()

g2 <- ggplot(data_clean, aes(x = edad_w)) +
  geom_density(fill = "#F8766D", alpha = 0.4, color = "black") +
  geom_vline(aes(xintercept = mean(edad_w, na.rm = TRUE)), 
             linetype = "dashed", color = "red") +
  labs(title = "Distribución de la edad", 
       x = "Edad (winsorizada)", y = "Densidad") +
  theme_minimal()

g3 <- ggplot(data_clean, aes(x = horas_trab_w)) +
  geom_density(fill = "#00BA38", alpha = 0.4, color = "black") +
  geom_vline(aes(xintercept = mean(horas_trab_w, na.rm = TRUE)), 
             linetype = "dashed", color = "red") +
  labs(title = "Distribución de las horas trabajadas", 
       x = "Horas trabajadas (winsorizadas)", y = "Densidad") +
  theme_minimal()

# Combinar en una sola figura
library(patchwork)
densidades <- (g1 | g2 | g3)

# Guardar
ggsave(here::here("views", "densidades.png"), densidades, width = 12, height = 4)

# --- 3.2 Distribución del salario por sexo y formalidad ---
densidad_sexo_formal <- ggplot(data_clean, 
                               aes(x = salario_real_hora_winsor, 
                                   color = interaction(sexo, trabajo_formal),
                                   fill = interaction(sexo, trabajo_formal))) +
  geom_density(alpha = 0.3) +
  scale_color_manual(values = c("#619CFF", "#00BA38", "#F8766D", "#C77CFF"),
                     labels = c("Hombre Informal", "Hombre Formal", 
                                "Mujer Informal", "Mujer Formal")) +
  scale_fill_manual(values = c("#619CFF", "#00BA38", "#F8766D", "#C77CFF"),
                    labels = c("Hombre Informal", "Hombre Formal", 
                               "Mujer Informal", "Mujer Formal")) +
  labs(title = "Distribución del salario por hora por sexo y formalidad",
       x = "Salario por hora (winsorizado)", y = "Densidad", color = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here::here("views", "densidad_sexo_formal.png"), densidad_sexo_formal,
       width = 10, height = 6, dpi = 300, bg = "white")




