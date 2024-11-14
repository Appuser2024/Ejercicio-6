# Datos observados
obs <- c(32, 15, 9, 4)
n <- 60

# Calcular lambda (media muestral)
lambda <- sum((0:3) * obs) / n
cat("Lambda estimado:", lambda, "\n")

# Calcular probabilidades teóricas
prob_teoricas <- dpois(0:3, lambda)
# Calcular frecuencias esperadas
esp <- n * prob_teoricas

# Mostrar tabla comparativa
tabla <- data.frame(
  x = 0:3,
  Observado = obs,
  Esperado = esp,
  prob_teorica = prob_teoricas
)
print(tabla)

# Calcular estadístico chi-cuadrado
chi_sq <- sum((obs - esp)^2 / esp)
gl <- length(obs) - 1 - 1  # grados de libertad (k-1-1), restamos 1 por estimar lambda
p_valor <- 1 - pchisq(chi_sq, gl)

cat("\n Estadístico Chi-cuadrado:", chi_sq)
cat("\n Grados de libertad:", gl)
cat("\n p-valor:", p_valor)

# Graficar comparación
barplot(rbind(obs, esp), 
        beside = TRUE,
        names.arg = 0:3,
        col = c("lightblue", "pink"),
        main = "Frecuencias Observadas vs Esperadas",
        xlab = "Número de defectos",
        ylab = "Frecuencia")
legend("topright", 
       c("Observado", "Esperado"), 
       fill = c("lightblue", "pink"))