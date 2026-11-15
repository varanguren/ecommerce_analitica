# =============================================================================
# PROYECTO FINAL - ANALITICA DE LOS NEGOCIOS
# E-commerce: tiempo de entrega y evaluacion de los clientes
#
# SCRIPT 2 de 2: MODELOS
#   Modelo 1 (central): regresion lineal multiple (OLS)
#     customer_rating ~ diferencia_entrega + estimated_delivery_days + controles
#   Modelo 2 (robustez): logit
#     sentimiento_positivo ~ diferencia_entrega + estimated_delivery_days + controles
#   Variante de robustez adicional: el atraso como variable binaria
#   (atrasado = 1 si la entrega llego tarde), para OLS y para logit.
#
# Interpretacion: el objetivo es medir ASOCIACION (correlacion) entre el
# tiempo de entrega y la evaluacion del cliente. NO se busca probar
# causalidad; los coeficientes se leen como asociaciones.
#
# Salidas:
#   - Tablas de coeficientes en LaTeX  -> salidas/tablas/*.tex
#   - Graficos de diagnostico          -> salidas/figuras/
#   - Log de consola                   -> salidas/logs/modelos_log.txt
#
# Paquetes: solo tidyverse (dplyr, ggplot2, readr). Los modelos usan
# funciones base de R (lm, glm), que no requieren paquetes adicionales.
#
# Como correrlo: igual que el script 1 (CSV en la misma carpeta,
# fijar el directorio de trabajo al archivo y correr todo).
# Se recomienda correr primero 01_eda.R.
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)

# ---- 0. Parametros y carpetas de salida -------------------------------------
ruta_datos <- "ecommerce_sales_customer_analytics_150k.csv"  # ajustar si el CSV esta en otra carpeta

dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/tablas",  recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/logs",    recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 12))

# ---- 1. Carga y preparacion (misma logica del script 1) ---------------------
datos <- read_csv(ruta_datos, na = c("", "NA"), show_col_types = FALSE)

datos <- datos %>%
  mutate(
    diferencia_entrega   = delivery_days - estimated_delivery_days,
    # DECISION DOCUMENTADA: "Neutral" se agrupa con "Negative" en 0,
    # para tener una variable binaria (positivo vs. no positivo).
    sentimiento_positivo = ifelse(review_sentiment == "Positive", 1, 0),
    # Variable de robustez: 1 si el pedido llego tarde, 0 si llego a tiempo o antes
    atrasado = ifelse(diferencia_entrega > 0, 1, 0)
  )

# ---- 2. Muestra del modelo ---------------------------------------------------
# Se usan los pedidos con entrega registrada e informacion completa en las
# variables del modelo. En esta base, los pedidos cancelados/pendientes no
# tienen rating, sentimiento ni tiempos, y los devueltos no registran
# tiempos de entrega; por eso quedan fuera del analisis.
datos_modelo <- datos %>%
  filter(
    !is.na(customer_rating),
    !is.na(diferencia_entrega),
    !is.na(estimated_delivery_days),
    !is.na(customer_age),
    !is.na(gender),
    !is.na(customer_type),
    !is.na(shipping_method),
    !is.na(sentimiento_positivo)
  )

# ---- 3. Variables categoricas como factores (dummies k-1) -------------------
# R convierte cada factor en k-1 variables dummy dentro del modelo y deja
# una categoria por fuera: la CATEGORIA DE REFERENCIA, contra la cual se
# comparan las demas. Se fijan con relevel() para que sean claras:
#   - gender:          referencia = "Female" (si existe en la base)
#   - customer_type:   referencia = la categoria mas frecuente
#   - shipping_method: referencia = "Standard" (el metodo mas comun)
# Para cambiar una referencia basta editar el argumento ref = "...".
datos_modelo <- datos_modelo %>%
  mutate(
    gender          = factor(gender),
    customer_type   = factor(customer_type),
    shipping_method = factor(shipping_method)
  )

if ("Female" %in% levels(datos_modelo$gender)) {
  datos_modelo$gender <- relevel(datos_modelo$gender, ref = "Female")
}
ref_tipo <- names(sort(table(datos_modelo$customer_type), decreasing = TRUE))[1]
datos_modelo$customer_type <- relevel(datos_modelo$customer_type, ref = ref_tipo)
if ("Standard" %in% levels(datos_modelo$shipping_method)) {
  datos_modelo$shipping_method <- relevel(datos_modelo$shipping_method, ref = "Standard")
}

# ---- 4. Log de salida ---------------------------------------------------------
sink("salidas/logs/modelos_log.txt", split = TRUE)

cat("=============================================================\n")
cat("MODELOS - PROYECTO ECOMMERCE (tiempo de entrega y satisfaccion)\n")
cat("Corrida:", as.character(Sys.time()), "\n")
cat("=============================================================\n\n")

cat("Observaciones en la muestra del modelo:", nrow(datos_modelo), "\n\n")
cat("Categorias de referencia de las variables categoricas:\n")
cat("  gender:          ", levels(datos_modelo$gender)[1], "\n")
cat("  customer_type:   ", levels(datos_modelo$customer_type)[1], "\n")
cat("  shipping_method: ", levels(datos_modelo$shipping_method)[1], "\n\n")

cat("NOTA DE INTERPRETACION: los resultados miden ASOCIACION entre el\n")
cat("tiempo de entrega y la evaluacion del cliente; no prueban causalidad.\n\n")

# ---- 5. MODELO 1: regresion lineal multiple (OLS) ---------------------------
# Variable dependiente: customer_rating.
# Nota metodologica (limitacion que se menciona en el informe): el rating es
# una variable acotada entre 1 y 5; se trata como continua como primera
# aproximacion. Esta limitacion se documenta en la seccion de resultados.
modelo_ols <- lm(
  customer_rating ~ diferencia_entrega + estimated_delivery_days +
    customer_age + gender + customer_type + shipping_method,
  data = datos_modelo
)
resumen_ols <- summary(modelo_ols)

cat("=============================================================\n")
cat("MODELO 1: REGRESION LINEAL MULTIPLE (OLS)\n")
cat("Dependiente: customer_rating\n")
cat("=============================================================\n")
print(resumen_ols)

r2      <- resumen_ols$r.squared
r2_adj  <- resumen_ols$adj.r.squared
rmse    <- sqrt(mean(residuals(modelo_ols)^2))

cat("\n--- Medidas de ajuste (OLS) ---\n")
cat("R2:            ", round(r2, 4), "\n")
cat("R2 ajustado:   ", round(r2_adj, 4), "\n")
cat("RMSE:          ", round(rmse, 4), "\n")
cat("AIC:           ", round(AIC(modelo_ols), 1), "\n")
cat("BIC:           ", round(BIC(modelo_ols), 1), "\n")
cat("Observaciones: ", nobs(modelo_ols), "\n\n")

# ---- 6. Robustez del OLS: atraso como variable binaria ----------------------
# Motivacion: el EDA sugiere que la caida de la calificacion se concentra en
# que el pedido llegue tarde (si/no), mas que en el numero de dias de atraso.
# Si el signo y la significancia se mantienen, el resultado es robusto.
modelo_ols_rob <- lm(
  customer_rating ~ atrasado + estimated_delivery_days +
    customer_age + gender + customer_type + shipping_method,
  data = datos_modelo
)

cat("=============================================================\n")
cat("ROBUSTEZ OLS: atraso binario (atrasado = 1 si llego tarde)\n")
cat("=============================================================\n")
print(summary(modelo_ols_rob))
cat("R2 ajustado:", round(summary(modelo_ols_rob)$adj.r.squared, 4), "\n\n")

# ---- 7. Diagnostico grafico del OLS -----------------------------------------
# Residuos vs. ajustados, normalidad (QQ), escala-localizacion y apalancamiento.
png("salidas/figuras/diag_residuos_ols.png", width = 900, height = 800, res = 110)
par(mfrow = c(2, 2))
plot(modelo_ols)
dev.off()
cat("Graficos de diagnostico guardados en salidas/figuras/diag_residuos_ols.png\n\n")

# ---- 8. MODELO 2: logit (sentimiento positivo) ------------------------------
# Variable dependiente: sentimiento_positivo (1 = resena positiva,
# 0 = neutral o negativa, segun la decision documentada).
modelo_logit <- glm(
  sentimiento_positivo ~ diferencia_entrega + estimated_delivery_days +
    customer_age + gender + customer_type + shipping_method,
  data = datos_modelo,
  family = binomial(link = "logit")
)
resumen_logit <- summary(modelo_logit)

cat("=============================================================\n")
cat("MODELO 2: LOGIT\n")
cat("Dependiente: sentimiento_positivo (1 = Positive, 0 = Neutral/Negative)\n")
cat("=============================================================\n")
print(resumen_logit)

# Pseudo R2 de McFadden = 1 - logL(modelo) / logL(modelo nulo)
logit_nulo  <- glm(sentimiento_positivo ~ 1, data = datos_modelo, family = binomial)
r2_mcfadden <- 1 - as.numeric(logLik(modelo_logit)) / as.numeric(logLik(logit_nulo))

# Bondad de ajuste adicional: porcentaje de aciertos con corte en 0.5
prob_pred  <- predict(modelo_logit, type = "response")
pred_clase <- as.integer(prob_pred >= 0.5)
accuracy   <- mean(pred_clase == datos_modelo$sentimiento_positivo)

cat("\n--- Medidas de ajuste (logit) ---\n")
cat("Pseudo R2 de McFadden: ", round(r2_mcfadden, 4), "\n")
cat("AIC:                   ", round(AIC(modelo_logit), 1), "\n")
cat("Porcentaje de aciertos (corte 0.5): ", round(100 * accuracy, 2), "%\n")
cat("Observaciones:         ", nobs(modelo_logit), "\n\n")

# Odds ratios con intervalos de confianza al 95%
# OR > 1: la variable se asocia con MAYOR probabilidad de resena positiva.
# OR < 1: se asocia con MENOR probabilidad.
ic_logit <- confint.default(modelo_logit, level = 0.95)
tabla_or <- data.frame(
  variable = names(coef(modelo_logit)),
  odds_ratio = exp(coef(modelo_logit)),
  ic_inf = exp(ic_logit[, 1]),
  ic_sup = exp(ic_logit[, 2])
)
cat("--- Odds ratios (logit) ---\n")
print(tibble::as_tibble(tabla_or), width = Inf)
cat("\n")

# ---- 9. Robustez del logit: atraso como variable binaria --------------------
modelo_logit_rob <- glm(
  sentimiento_positivo ~ atrasado + estimated_delivery_days +
    customer_age + gender + customer_type + shipping_method,
  data = datos_modelo,
  family = binomial(link = "logit")
)

cat("=============================================================\n")
cat("ROBUSTEZ LOGIT: atraso binario (atrasado = 1 si llego tarde)\n")
cat("=============================================================\n")
print(summary(modelo_logit_rob))

ic_rob <- confint.default(modelo_logit_rob, level = 0.95)
tabla_or_rob <- data.frame(
  variable = names(coef(modelo_logit_rob)),
  odds_ratio = exp(coef(modelo_logit_rob)),
  ic_inf = exp(ic_rob[, 1]),
  ic_sup = exp(ic_rob[, 2])
)
cat("\n--- Odds ratios (logit, robustez) ---\n")
print(tibble::as_tibble(tabla_or_rob), width = Inf)
cat("\n")

# ---- 10. Exportar tablas en LaTeX --------------------------------------------
fmt3 <- function(z) formatC(as.numeric(z), format = "f", digits = 3)
estrellas <- function(p) {
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}
nota_estrellas <- paste0(
  "\\begin{minipage}{0.9\\textwidth}\\footnotesize ",
  "Errores estandar entre parentesis. ",
  "*** p$<$0.01, ** p$<$0.05, * p$<$0.1. ",
  "\\end{minipage}"
)

# 10.1 Tabla de coeficientes (OLS o logit)
tex_tabla_coef <- function(modelo, archivo, titulo, etiqueta) {
  s <- summary(modelo)$coefficients
  filas <- vapply(seq_len(nrow(s)), function(i) {
    paste0(
      gsub("_", "\\_", rownames(s)[i], fixed = TRUE), " & ",
      fmt3(s[i, 1]), estrellas(s[i, 4]), " & (",
      fmt3(s[i, 2]), ") \\\\"
    )
  }, character(1))
  lineas <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", titulo, "}"),
    paste0("\\label{", etiqueta, "}"),
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Variable & Coeficiente & (Err. est.) \\\\",
    "\\midrule",
    filas,
    "\\bottomrule",
    "\\end{tabular}",
    nota_estrellas,
    "\\end{table}"
  )
  writeLines(lineas, archivo, useBytes = TRUE)
}

# 10.2 Tabla de odds ratios (logit)
tex_tabla_or <- function(modelo, archivo, titulo, etiqueta) {
  ic <- confint.default(modelo, level = 0.95)
  filas <- vapply(seq_along(coef(modelo)), function(i) {
    paste0(
      gsub("_", "\\_", names(coef(modelo))[i], fixed = TRUE), " & ",
      fmt3(exp(coef(modelo)[i])), " & ",
      fmt3(exp(ic[i, 1])), " & ",
      fmt3(exp(ic[i, 2])), " \\\\"
    )
  }, character(1))
  lineas <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", titulo, "}"),
    paste0("\\label{", etiqueta, "}"),
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Variable & Odds ratio & IC 95\\% inf. & IC 95\\% sup. \\\\",
    "\\midrule",
    filas,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lineas, archivo, useBytes = TRUE)
}

tex_tabla_coef(modelo_ols,
               "salidas/tablas/tabla_ols.tex",
               "Regresion lineal multiple: calificacion del cliente",
               "tab:ols")
tex_tabla_coef(modelo_ols_rob,
               "salidas/tablas/tabla_ols_robustez.tex",
               "Regresion lineal multiple: especificacion con atraso binario",
               "tab:olsrob")
tex_tabla_coef(modelo_logit,
               "salidas/tablas/tabla_logit.tex",
               "Modelo logit: probabilidad de resena positiva",
               "tab:logit")
tex_tabla_or(modelo_logit,
             "salidas/tablas/tabla_logit_odds.tex",
             "Odds ratios del modelo logit",
             "tab:logitor")
tex_tabla_or(modelo_logit_rob,
             "salidas/tablas/tabla_logit_odds_robustez.tex",
             "Odds ratios del modelo logit (especificacion con atraso binario)",
             "tab:logitorrob")

cat("Tablas LaTeX guardadas en salidas/tablas/\n\n")

# ---- 11. Reproducibilidad ----------------------------------------------------
cat("--- Informacion de la sesion (para reproducibilidad) ---\n")
print(sessionInfo())

cat("\nFin del script de modelos.\n")

sink()  # cierra el log
