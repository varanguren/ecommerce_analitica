# =============================================================================
# PROYECTO FINAL - ANALITICA DE LOS NEGOCIOS
# E-commerce: tiempo de entrega y evaluacion de los clientes
#
# SCRIPT 1 de 2: ANALISIS EXPLORATORIO DE DATOS (EDA)
#
# Que hace este script:
#   1. Carga la base ecommerce_sales_customer_analytics_150k.csv
#   2. Construye las variables clave del estudio
#   3. Calcula estadisticas descriptivas de las 8 variables del estudio
#   4. Genera los graficos para el informe  -> salidas/figuras/
#   5. Exporta la tabla de descriptivas en LaTeX -> salidas/tablas/
#   6. Guarda toda la salida de consola en    -> salidas/logs/eda_log.txt
#
# Paquetes: solo tidyverse (dplyr, ggplot2, readr).
#
# Como correrlo:
#   - Colocar el CSV en la misma carpeta de este script.
#   - En RStudio: Session > Set Working Directory > To Source File Location.
#   - Correr el archivo completo (Ctrl+Shift+Enter).
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)

# ---- 0. Parametros y carpetas de salida -------------------------------------
ruta_datos <- "ecommerce_sales_customer_analytics_150k.csv"  # ajustar si el CSV esta en otra carpeta

dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/tablas",  recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/logs",    recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 12))  # tema sencillo para todos los graficos

# ---- 1. Carga de datos ------------------------------------------------------
# na = c("", "NA"): en esta base los faltantes vienen como celdas vacias
# (por ejemplo, los pedidos cancelados no tienen rating ni tiempos de entrega).
datos <- read_csv(ruta_datos, na = c("", "NA"), show_col_types = FALSE)

# ---- 2. Variables del estudio -----------------------------------------------
# diferencia_entrega = dias reales de entrega - dias estimados de entrega
#   > 0 : el pedido llego tarde  |  = 0 : puntual  |  < 0 : llego antes
# sentimiento_positivo = 1 si la resena es "Positive", 0 en otro caso.
#   DECISION DOCUMENTADA: las resenas "Neutral" se agrupan con "Negative"
#   en la categoria 0, para trabajar con una variable binaria
#   (positivo vs. no positivo), como se acordo en el diseno del estudio.
datos <- datos %>%
  mutate(
    diferencia_entrega   = delivery_days - estimated_delivery_days,
    sentimiento_positivo = ifelse(review_sentiment == "Positive", 1, 0)
  )

# ---- 3. Registro (log) de toda la salida ------------------------------------
sink("salidas/logs/eda_log.txt", split = TRUE)

cat("=============================================================\n")
cat("EDA - PROYECTO ECOMMERCE (tiempo de entrega y satisfaccion)\n")
cat("Corrida:", as.character(Sys.time()), "\n")
cat("=============================================================\n\n")

cat("Observaciones totales de la base:", nrow(datos), "\n")
cat("Columnas totales:", ncol(datos), "\n\n")

# ---- 4. Valores faltantes en las variables del estudio ----------------------
vars_estudio <- c("customer_rating", "review_sentiment", "delivery_days",
                  "estimated_delivery_days", "diferencia_entrega",
                  "customer_age", "gender", "customer_type", "shipping_method")

cat("--- Valores faltantes por variable ---\n")
faltantes <- sapply(vars_estudio, function(v) sum(is.na(datos[[v]])))
print(faltantes)
cat("\nNota: los faltantes de rating/sentimiento/tiempos corresponden a\n")
cat("pedidos cancelados o pendientes, que nunca se entregaron.\n\n")

# ---- 5. Estadisticas descriptivas: variables numericas ----------------------
vars_num <- c("customer_rating", "delivery_days", "estimated_delivery_days",
              "diferencia_entrega", "customer_age")

resumen_num <- bind_rows(lapply(vars_num, function(v) {
  x <- datos[[v]]
  tibble(
    variable  = v,
    n         = sum(!is.na(x)),
    faltantes = sum(is.na(x)),
    media     = mean(x, na.rm = TRUE),
    desv_est  = sd(x, na.rm = TRUE),
    minimo    = min(x, na.rm = TRUE),
    mediana   = median(x, na.rm = TRUE),
    maximo    = max(x, na.rm = TRUE)
  )
}))

cat("--- Estadisticas descriptivas (variables numericas) ---\n")
print(tibble::as_tibble(resumen_num), width = Inf)
cat("\n")

# ---- 6. Tablas de frecuencia: variables categoricas -------------------------
tabla_frecuencia <- function(df, var) {
  df %>%
    count(.data[[var]]) %>%
    mutate(porcentaje = round(100 * n / sum(n), 2)) %>%
    arrange(desc(n))
}

for (v in c("review_sentiment", "gender", "customer_type", "shipping_method")) {
  cat("--- Tabla de frecuencia:", v, "---\n")
  print(tibble::as_tibble(tabla_frecuencia(datos, v)), width = Inf)
  cat("\n")
}

cat("--- Variable dependiente del logit: sentimiento_positivo ---\n")
cat("(1 = resena positiva; 0 = neutral o negativa, segun la decision documentada)\n")
print(tibble::as_tibble(tabla_frecuencia(datos, "sentimiento_positivo")), width = Inf)
cat("\n")

cat("--- Distribucion de la diferencia de entrega (dias) ---\n")
print(tibble::as_tibble(tabla_frecuencia(datos, "diferencia_entrega")), width = Inf)
cat("\n")

# ---- 7. Tabla LaTeX de descriptivas -----------------------------------------
tex_tabla_descriptivas <- function(df, archivo, titulo, etiqueta) {
  fmt <- function(z) formatC(as.numeric(z), format = "f", digits = 2)
  filas <- vapply(seq_len(nrow(df)), function(i) {
    paste0(
      gsub("_", "\\_", df$variable[i], fixed = TRUE), " & ",
      df$n[i], " & ", df$faltantes[i], " & ",
      fmt(df$media[i]), " & ", fmt(df$desv_est[i]), " & ",
      fmt(df$minimo[i]), " & ", fmt(df$mediana[i]), " & ", fmt(df$maximo[i]),
      " \\\\"
    )
  }, character(1))
  lineas <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", titulo, "}"),
    paste0("\\label{", etiqueta, "}"),
    "\\begin{tabular}{lrrrrrrr}",
    "\\toprule",
    "Variable & n & Faltantes & Media & Desv. est. & Min. & Mediana & Max. \\\\",
    "\\midrule",
    filas,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lineas, archivo, useBytes = TRUE)
}

tex_tabla_descriptivas(
  resumen_num,
  "salidas/tablas/tabla_descriptivas.tex",
  "Estadisticas descriptivas de las variables numericas del estudio",
  "tab:descriptivas"
)
cat("Tabla LaTeX de descriptivas guardada en salidas/tablas/tabla_descriptivas.tex\n\n")

# ---- 8. Graficos para el informe --------------------------------------------
# Solo se grafican pedidos con entrega registrada (sin faltantes).
entregados <- datos %>% filter(!is.na(diferencia_entrega))

# 8.1 Distribucion de la calificacion (variable dependiente del modelo lineal)
g1 <- datos %>%
  filter(!is.na(customer_rating)) %>%
  ggplot(aes(customer_rating)) +
  geom_histogram(binwidth = 0.1, fill = "#2C7FB8", color = "white") +
  labs(title = "Distribucion de la calificacion de los clientes",
       x = "Calificacion (customer_rating, escala de 1 a 5)",
       y = "Numero de pedidos")
ggsave("salidas/figuras/fig_01_hist_rating.png", g1, width = 7, height = 4.5, dpi = 300)

# 8.2 Frecuencia de los sentimientos de las resenas
g2 <- datos %>%
  filter(!is.na(review_sentiment)) %>%
  ggplot(aes(x = reorder(review_sentiment, review_sentiment, length))) +
  geom_bar(fill = "#2C7FB8") +
  labs(title = "Sentimiento de las resenas",
       x = "Sentimiento (review_sentiment)", y = "Numero de pedidos")
ggsave("salidas/figuras/fig_02_barras_sentimiento.png", g2, width = 7, height = 4.5, dpi = 300)

# 8.3 Distribucion de la diferencia entre entrega real y estimada
g3 <- entregados %>%
  ggplot(aes(factor(diferencia_entrega))) +
  geom_bar(fill = "#2C7FB8") +
  labs(title = "Diferencia entre tiempo de entrega real y estimado",
       x = "Diferencia de entrega (dias; negativo = llego antes)",
       y = "Numero de pedidos")
ggsave("salidas/figuras/fig_03_barras_diferencia.png", g3, width = 7, height = 4.5, dpi = 300)

# 8.4 Calificacion promedio segun la diferencia de entrega (grafico clave)
rating_por_dif <- entregados %>%
  filter(!is.na(customer_rating)) %>%
  group_by(diferencia_entrega) %>%
  summarise(rating_medio = mean(customer_rating), n = n(), .groups = "drop")

g4 <- ggplot(rating_por_dif, aes(factor(diferencia_entrega), rating_medio)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(aes(label = round(rating_medio, 2)), vjust = -0.4, size = 3.5) +
  labs(title = "Calificacion promedio segun la diferencia de entrega",
       x = "Diferencia de entrega (dias; negativo = llego antes)",
       y = "Calificacion promedio")
ggsave("salidas/figuras/fig_04_rating_por_diferencia.png", g4, width = 7, height = 4.5, dpi = 300)

# 8.5 Tiempos de entrega reales vs. estimados
g5 <- entregados %>%
  ggplot() +
  geom_density(aes(delivery_days, color = "Real"), linewidth = 1) +
  geom_density(aes(estimated_delivery_days, color = "Estimado"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "#2C7FB8", "Estimado" = "#D95F02")) +
  labs(title = "Tiempo de entrega real vs. estimado",
       x = "Dias de entrega", y = "Densidad", color = "Tiempo")
ggsave("salidas/figuras/fig_05_densidad_tiempos.png", g5, width = 7, height = 4.5, dpi = 300)

# 8.6 Calificacion por metodo de envio (control)
g6 <- entregados %>%
  filter(!is.na(customer_rating)) %>%
  ggplot(aes(shipping_method, customer_rating)) +
  geom_boxplot(fill = "#A6CEE3") +
  labs(title = "Calificacion segun el metodo de envio",
       x = "Metodo de envio (shipping_method)", y = "Calificacion")
ggsave("salidas/figuras/fig_06_boxplot_rating_envio.png", g6, width = 7, height = 4.5, dpi = 300)

# 8.7 Genero de los clientes (control)
g7 <- datos %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(gender)) +
  geom_bar(fill = "#2C7FB8") +
  labs(title = "Distribucion por genero", x = "Genero", y = "Numero de pedidos")
ggsave("salidas/figuras/fig_07_barras_genero.png", g7, width = 7, height = 4.5, dpi = 300)

# 8.8 Tipo de cliente (control)
g8 <- datos %>%
  filter(!is.na(customer_type)) %>%
  ggplot(aes(customer_type)) +
  geom_bar(fill = "#2C7FB8") +
  labs(title = "Distribucion por tipo de cliente",
       x = "Tipo de cliente (customer_type)", y = "Numero de pedidos")
ggsave("salidas/figuras/fig_08_barras_tipo_cliente.png", g8, width = 7, height = 4.5, dpi = 300)

# 8.9 Edad de los clientes (control)
g9 <- datos %>%
  filter(!is.na(customer_age)) %>%
  ggplot(aes(customer_age)) +
  geom_histogram(binwidth = 2, fill = "#2C7FB8", color = "white") +
  labs(title = "Distribucion de la edad de los clientes",
       x = "Edad (customer_age)", y = "Numero de pedidos")
ggsave("salidas/figuras/fig_09_hist_edad.png", g9, width = 7, height = 4.5, dpi = 300)

cat("Graficos guardados en salidas/figuras/\n\n")
cat("Fin del EDA.\n")

sink()  # cierra el log
