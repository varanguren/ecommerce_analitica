PROYECTO ECOMMERCE - Scripts de R
=================================

Contenido
---------
01_eda.R      Script 1: analisis exploratorio (descriptivas, frecuencias y
              graficos para el informe).
02_modelos.R  Script 2: regresion lineal multiple (OLS), logit y
              especificaciones de robustez, con tablas en LaTeX.

Como correrlos
--------------
1. Guardar el CSV ecommerce_sales_customer_analytics_150k.csv en la misma
   carpeta de los scripts (o ajustar la variable ruta_datos al inicio de
   cada script).
2. En RStudio: Session > Set Working Directory > To Source File Location.
3. Correr primero 01_eda.R y despues 02_modelos.R.
4. Al final, enviar los archivos de salidas/logs/ (eda_log.txt y
   modelos_log.txt) para armar la seccion de resultados del informe.

Salidas
-------
salidas/figuras/   Graficos en PNG (300 dpi) listos para el informe.
salidas/tablas/    Tablas en formato LaTeX (.tex) para incluir con \input{}.
salidas/logs/      Registros de consola con todas las estadisticas.

Notas acordadas del diseno
--------------------------
- Variables del estudio (nombres reales en el CSV):
    customer_rating, review_sentiment, delivery_days,
    estimated_delivery_days, customer_age, gender, customer_type,
    shipping_method
  (delivery_time del planteamiento = delivery_days;
   expected_time = estimated_delivery_days)
- Independiente central: diferencia_entrega = delivery_days -
  estimated_delivery_days, y estimated_delivery_days por separado.
- Dependientes: customer_rating (OLS) y sentimiento_positivo (logit).
- sentimiento_positivo = 1 si review_sentiment es "Positive"; "Neutral"
  se agrupa con "Negative" en 0 (decision documentada en los scripts).
- Controles categoricos como dummies k-1 con categorias de referencia
  explicitas (gender: Female; customer_type: la mas frecuente;
  shipping_method: Standard).
- Limitacion documentada: customer_rating esta acotado entre 1 y 5 y se
  trata como continua como primera aproximacion.
- Interpretacion: asociacion/correlacion, NO causalidad.
