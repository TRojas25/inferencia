datos <- read.csv("C:/Users/tomas/OneDrive/Escritorio/Rstudio codes/inferencia/ames-housing.csv")
#datos = read.csv("C:/Users/Felipe/Desktop/Proyecto Inferencia/ames-housing.csv")
#1.a
library(e1071)
library(ggplot2)
library(nortest)
precio <- datos$SalePrice
hist(precio)


# Calcular medidas de tendencia del precio.
cat("=== MEDIDAS COMPARATIVAS ===\n")
cat("Media:", round(mean(precio), 2), "\n")
cat("Desviación:", round(sd(precio), 2), "\n")
cat("Coef. Variación:", round(sd(precio)/mean(precio), 3), "\n")
cat("Mediana", median(precio))
cat("Maximo y minimo", max(precio) , min(precio))
asimetria <- skewness(precio, na.rm = TRUE)
print(paste("Asimetría:", round(asimetria, 4)))
cat("Kurtosis", round(kurtosis(precio), 2))
quantile(precio)



############## 2.c #########
# 1. BOXPLOT
bp <- boxplot(precio,
              names = c("Precio de venta"),
              #main = "Comparación de Dispersión y Outliers",
              ylab = "Precio de venta",
              col = c("lightblue"),
              pch = 19)
# Añadir líneas de media
abline(h = mean(precio), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = c("Promedio"),
       col = c("blue"), lwd = 2)

# Contar outliers
num_outliers <- length(bp$out)
print(paste("Número de outliers:", num_outliers))

#1.b
#Distribucion gamma escala
alpha_est <-(mean(precio))^2/(sd(precio))^2
beta_est <-mean(precio)^2/(sd(precio))^2



#1.c

log_precio <- log(precio)

# Estimadores MLE
mu_mle <- mean(log_precio)
sigma2_mle <- mean((log_precio - mu_mle)^2)  # MLE (sesgado)
sigma2_unbiased <- var(log_precio)           # Insesgado (n-1 en denominador)

cat("=== ESTIMADORES MLE LOG-NORMAL ===\n")
cat("μ_MLE =", round(mu_mle, 6), "\n")
cat("σ²_MLE =", round(sigma2_mle, 6), "\n")
cat("σ_MLE =", round(sqrt(sigma2_mle), 6), "\n")
cat("σ²_insesgado =", round(sigma2_unbiased, 6), "\n")
cat("σ_insesgado =", round(sqrt(sigma2_unbiased), 6), "\n")



# Parámetros determinados por metodo momentos
mu <- 12.01
sigma2 <- 0.17
sigma <- sqrt(sigma2)  # σ = 0.4123

# Crear histograma con curva log-normal ajustada
hist(precio, freq = FALSE, breaks = 50, 
     col = "lightblue", border = "white",
     main = "Ajuste Distribución Log-Normal a SalePrice",
     xlab = "Precio de Venta (USD)", 
     ylab = "Densidad",
     ylim = c(0, 1.2e-5))  # Ajustar límite Y para mejor visualización

# Curva de densidad log-normal teórica
x_seq <- seq(min(precio), max(precio), length = 1000)
y_lnorm <- dlnorm(x_seq, meanlog = mu, sdlog = sigma)
lines(x_seq, y_lnorm, col = "red", lwd = 2.5, lty = 1)

# Añadir línea vertical en la media teórica
media_teorica <- exp(mu + sigma2/2)
abline(v = media_teorica, col = "darkblue", lwd = 2, lty = 2)

# Añadir línea vertical en la mediana teórica
mediana_teorica <- exp(mu)
abline(v = mediana_teorica, col = "darkgreen", lwd = 2, lty = 2)

# Leyenda mejorada
legend("topright", 
       legend = c("Datos", 
                  paste("Log-Normal(μ=", mu, ", σ²=", sigma2, ")", sep = ""),
                  paste("Media teórica =", round(media_teorica, 0)),
                  paste("Mediana teórica =", round(mediana_teorica, 0))),
       col = c("lightblue", "red", "darkblue", "darkgreen"), 
       lwd = c(NA, 2.5, 2, 2), 
       lty = c(NA, 1, 2, 2),
       fill = c("lightblue", NA, NA, NA),
       border = c("black", NA, NA, NA),
       bty = "n",
       cex = 0.8)


#1.d
# Aplicar test KS
ks_test <- ks.test(precio, "plnorm", meanlog = mu, sdlog = sigma)

cat("Test KS con parámetros conocidos:\n")
cat("Estadístico D =", round(ks_test$statistic, 4), "\n")
cat("p-value =", round(ks_test$p.value, 4), "\n")


##otra forma
library(fitdistrplus)

# Ajuste log-normal
fit <- fitdist(precio, "lnorm")

# Test de bondad de ajuste
gofstat(fit)
plot(fit)
summary(fit)

fit2 <- fitdist(precio, "weibull")
gofstat(fit2)
plot(fit2)
summary(fit2)

# SOLUCIÓN PARA AJUSTAR GAMMA
cat("=== AJUSTE DISTRIBUCIÓN GAMMA - SOLUCIÓN ===\n\n")

# Método 1: Escalar los datos para mejorar la estabilidad numérica
cat("1. ESCALANDO DATOS (recomendado):\n")
precio_escalado <- precio / 1000  # Convertir a miles de USD

fit_gamma <- fitdist(precio_escalado, "gamma")
gof_gamma <- gofstat(fit_gamma)

cat("Gamma - KS:", round(gof_gamma$ks, 4), "\n")
cat("Gamma - AIC:", round(gof_gamma$aic, 2), "\n\n")

# Recuperar parámetros en escala original
shape_gamma <- fit_gamma$estimate["shape"]
rate_gamma <- fit_gamma$estimate["rate"] / 1000  # Ajustar rate a escala original
scale_gamma <- 1 / rate_gamma

cat("Parámetros Gamma (escala original):\n")
cat("Shape (α) =", round(shape_gamma, 4), "\n")
cat("Rate (β) =", round(rate_gamma, 7), "\n")
cat("Scale (θ = 1/β) =", round(scale_gamma, 2), "USD\n\n")

kstest <- ks.test(precio, "plnorm" , meanlog = 12.02097 , sdlog = 0.407517)
print(kstest$p.value)

# HISTOGRAMA CON DISTRIBUCIÓN GAMMA AJUSTADA
cat("=== HISTOGRAMA: PRECIOS ESCALADOS + GAMMA ===\n\n")

# Parámetros Gamma en escala escalada (miles de USD)
shape_esc <- fit_gamma$estimate["shape"]
rate_esc <- fit_gamma$estimate["rate"]
scale_esc <- 1 / rate_esc

cat("Parámetros Gamma (miles de USD):\n")
cat("Shape (α) =", round(shape_esc, 4), "\n")
cat("Rate (β) =", round(rate_esc, 4), "\n")
cat("Scale (θ) =", round(scale_esc, 4), "miles de USD\n\n")

# Crear histograma
hist(precio_escalado, freq = FALSE, breaks = 50, 
     col = "lightblue", border = "white",
     main = "Ajuste Distribución Gamma a SalePrice",
     xlab = "Precio de Venta (miles de USD)", 
     ylab = "Densidad",
     ylim = c(0, 0.012))

# Curva de densidad Gamma teórica
x_seq_esc <- seq(min(precio_escalado), max(precio_escalado), length = 1000)
y_gamma <- dgamma(x_seq_esc, shape = shape_esc, rate = rate_esc)
lines(x_seq_esc, y_gamma, col = "red", lwd = 2.5)

# Añadir líneas de media y moda
media_gamma_esc <- shape_esc / rate_esc
moda_gamma_esc <- (shape_esc - 1) / rate_esc

abline(v = media_gamma_esc, col = "darkblue", lwd = 2, lty = 2)
abline(v = moda_gamma_esc, col = "darkgreen", lwd = 2, lty = 2)
abline(v = mean(precio_escalado), col = "orange", lwd = 2, lty = 2)

# Leyenda
legend("topright", 
       legend = c("Datos", 
                  paste("Gamma(α=", round(shape_esc, 2), ", β=", round(rate_esc, 3), ")"),
                  paste("Media teórica =", round(media_gamma_esc, 1), "mil USD"),
                  paste("Moda teórica =", round(moda_gamma_esc, 1), "mil USD"),
                  paste("Media real =", round(mean(precio_escalado), 1), "mil USD")),
       col = c("lightblue", "red", "darkblue", "darkgreen", "orange"), 
       lwd = c(NA, 2.5, 2, 2, 2), 
       lty = c(NA, 1, 2, 2, 2),
       fill = c("lightblue", NA, NA, NA, NA),
       border = c("black", NA, NA, NA, NA),
       bty = "n",
       cex = 0.8)



###aqui gana la lognormal.... No se como interpretar que de bajo el valor. al ojo es weno

ad_result <- ad.test(log_precio)
print(ad_result)



library(MASS)

#############Felipoto 06/11/2025##########
#Boxcox
library(forecast)
lambda_forecast <- BoxCox.lambda(precio)
print(lambda_forecast)
precio_box <- BoxCox(precio, lambda = lambda_forecast)
hist(precio_box)


#Test ks normal con boxcox (mal)
meanbox = mean(precio_box)
sdbox = sd(precio_box)
ks_test_norm <- ks.test(precio_box, "pnorm", mean = meanbox, sd = sdbox)
print(ks_test_norm)
#Test ks normal con mle(mal)

precio_box_num <- as.vector(precio_box)
ajuste_normal <- fitdist(precio_box_num, "norm", method="mle")
print(ajuste_normal)

meanbox = ajuste_normal$estimate["mean"]
sdbox = ajuste_normal$estimate["sd"]
ks_test_norm <- ks.test(precio_box, "pnorm", mean = meanbox, sd = sdbox)
print(ks_test_norm)

#test shapiro para normalidad (mal)
shapiro_test <- shapiro.test(precio_box)
print(shapiro_test)

#test ks t-student con mle (mal)
precio_box_num <- as.vector(precio_box)
ajuste_tstudent <- fitdistr(precio_box, "t",
                            start = list(m = mean(precio_box), s = sd(precio_box), df = 10))
print(ajuste_tstudent)

m_est <- ajuste_tstudent$estimate["m"]
s_est <- ajuste_tstudent$estimate["s"]
df_est <- ajuste_tstudent$estimate["df"]


#Estandarizar los datos usando los parámetros estimados(malo)
precio_box_std <- (precio_box_num- m_est) / s_est

ks_test_tstudent <- ks.test(precio_box_std, "pt", df = df_est)

print(ks_test_tstudent) 





# Mejor que KS para datos continuos
library(nortest)
ad_test <- ad.test(precio_box)  # Anderson-Darling
lillie_test <- lillie.test(precio_box)  # Lilliefors

# Para muchos datos, usar:
library(ggplot2)
qqnorm(precio_box)
qqline(precio_box)


#ajustar laplace

# Definir funciones de la distribución Laplace (primero esto)
dlaplace <- function(x, location = 0, scale = 1) {
  if (scale <= 0) stop("scale must be positive")
  (1/(2*scale)) * exp(-abs(x - location)/scale)
}

plaplace <- function(q, location = 0, scale = 1) {
  if (scale <= 0) stop("scale must be positive")
  z <- (q - location)/scale
  0.5 * (1 + sign(z) * (1 - exp(-abs(z))))
}

qlaplace <- function(p, location = 0, scale = 1) {
  if (scale <= 0) stop("scale must be positive")
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")
  location - scale * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

rlaplace <- function(n, location = 0, scale = 1) {
  if (scale <= 0) stop("scale must be positive")
  location + scale * rexp(n) * sample(c(-1, 1), n, replace = TRUE)
}

# Ahora ajustar con los nombres correctos de parámetros
fit_laplace <- fitdist(precio, "laplace", 
                       start = list(location = median(precio), scale = mad(precio)/sqrt(2)))

# Resumen del ajuste
summary(fit_laplace)

# Gráfico del histograma con Laplace ajustada
hist(precio, freq = FALSE, breaks = 50, 
     col = "lightblue", border = "white",
     main = "Ajuste Distribución Laplace a Precios de Vivienda",
     xlab = "Precio de Venta (USD)", 
     ylab = "Densidad",
     ylim = c(0, 1.2e-5))

# Curva de densidad Laplace teórica
x_seq <- seq(min(precio), max(precio), length = 1000)
y_laplace <- dlaplace(x_seq, location = 160006.41, scale = 56028.12)
lines(x_seq, y_laplace, col = "red", lwd = 2.5)

# Añadir líneas de referencia
abline(v = 160006.41, col = "darkblue", lwd = 2, lty = 2) # Mediana Laplace
abline(v = mean(precio), col = "orange", lwd = 2, lty = 2) # Media real
abline(v = median(precio), col = "darkgreen", lwd = 2, lty = 2) # Mediana real

# Leyenda
legend("topright", 
       legend = c("Datos Histograma", 
                  "Laplace(location=160,006, scale=56,028)",
                  paste("Mediana Laplace =", format(160006.41, big.mark = ",", digits = 0)),
                  paste("Media real =", format(round(mean(precio)), big.mark = ",", digits = 0)),
                  paste("Mediana real =", format(round(median(precio)), big.mark = ",", digits = 0))),
       col = c("lightblue", "red", "darkblue", "orange", "darkgreen"), 
       lwd = c(NA, 2.5, 2, 2, 2), 
       lty = c(NA, 1, 2, 2, 2),
       fill = c("lightblue", NA, NA, NA, NA),
       border = c("black", NA, NA, NA, NA),
       bty = "n",
       cex = 0.8)


# Test de Kolmogorov-Smirnov para distribución Laplace
ks_test_laplace <- ks.test(precio, "plaplace", 
                           location = 160006.41, scale = 56028.12)

cat("=== TEST KOLMOGOROV-SMIRNOV - LAPLACE ===\n")
cat("Estadístico D =", round(ks_test_laplace$statistic, 4), "\n")
cat("p-value =", round(ks_test_laplace$p.value, 6), "\n")
cat("Muestra size =", length(precio), "\n\n")




# Encontrar lambda óptimo para Box-Cox
bc <- boxcox(precio ~ 1, lambda = seq(-2, 2, 0.1))
lambda_optimo <- bc$x[which.max(bc$y)]
cat("Lambda óptimo Box-Cox:", round(lambda_optimo, 4), "\n")

# Aplicar transformación Box-Cox
if (abs(lambda_optimo) < 1e-6) {
  precio_bc <- log(precio)
} else {
  precio_bc <- (precio^lambda_optimo - 1) / lambda_optimo
}


# Definir funciones Laplace CORREGIDAS (más robustas)
dlaplace <- function(x, location = 0, scale = 1) {
  # Manejar parámetros inconsistentes devolviendo NaN
  if (scale <= 0 || is.na(scale) || is.infinite(scale)) {
    return(rep(NaN, length(x)))
  }
  (1/(2*scale)) * exp(-abs(x - location)/scale)
}

plaplace <- function(q, location = 0, scale = 1) {
  # Manejar parámetros inconsistentes
  if (scale <= 0 || is.na(scale) || is.infinite(scale)) {
    return(rep(NaN, length(q)))
  }
  z <- (q - location)/scale
  0.5 * (1 + sign(z) * (1 - exp(-abs(z))))
}

qlaplace <- function(p, location = 0, scale = 1) {
  if (scale <= 0 || is.na(scale) || is.infinite(scale)) {
    return(rep(NaN, length(p)))
  }
  if (any(p < 0 | p > 1)) return(rep(NaN, length(p)))
  location - scale * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

rlaplace <- function(n, location = 0, scale = 1) {
  if (scale <= 0 || is.na(scale) || is.infinite(scale)) {
    return(rep(NaN, n))
  }
  location + scale * rexp(n) * sample(c(-1, 1), n, replace = TRUE)
}

# Ajustar distribución Laplace a datos transformados
fit_laplace_bc <- fitdist(precio_bc, "laplace", 
                          start = list(location = median(precio_bc), 
                                       scale = mad(precio_bc)/sqrt(2)))

cat("=== AJUSTE LAPLACE A DATOS BOX-COX ===\n")
summary(fit_laplace_bc)

# Parámetros estimados
location_bc <- fit_laplace_bc$estimate["location"]
scale_bc <- fit_laplace_bc$estimate["scale"]

# Test KS para Laplace en datos transformados
ks_test_laplace_bc <- ks.test(precio_bc, "plaplace", 
                              location = location_bc, scale = scale_bc)

cat("=== TEST KS - LAPLACE (DATOS BOX-COX) ===\n")
cat("Estadístico D =", round(ks_test_laplace_bc$statistic, 4), "\n")
cat("p-value =", round(ks_test_laplace_bc$p.value, 6), "\n")

# Histograma con ajuste Laplace
hist(precio_bc, freq = FALSE, breaks = 50, 
     col = "lightgreen", border = "white",
     main = "Laplace Ajustada a Datos Box-Cox",
     xlab = "Precio Transformado", 
     ylab = "Densidad")

x_seq_bc <- seq(min(precio_bc), max(precio_bc), length = 1000)
y_laplace_bc <- dlaplace(x_seq_bc, location = location_bc, scale = scale_bc)
lines(x_seq_bc, y_laplace_bc, col = "red", lwd = 2.5)













library(caret)
yj_transform <- preProcess(data.frame(precio), 
                           method = c("YeoJohnson"))
precio_yj <- predict(yj_transform, data.frame(precio))$precio

# Ver parámetros de la transformación
print("Parámetros Yeo-Johnson:")
print(yj_transform$yj)

# Histograma de datos transformados
hist(precio_yj, main = "Distribución después de Yeo-Johnson", 
     xlab = "Precio Transformado", col = "lightblue")

# Q-Q Plot de datos transformados
qqnorm(precio_yj, main = "Q-Q Plot: Yeo-Johnson")
qqline(precio_yj, col = "red")

# Calcular parámetros para la distribución normal
mean_yj <- mean(precio_yj)
sd_yj <- sd(precio_yj)

cat("\nParámetros de la distribución normal ajustada:")
cat("\nMedia:", mean_yj)
cat("\nDesviación estándar:", sd_yj)

# Test de Kolmogorov-Smirnov
cat("\n\n--- Test de Kolmogorov-Smirnov ---\n")
ks_test_yj <- ks.test(precio_yj, "pnorm", mean = mean_yj, sd = sd_yj)
print(ks_test_yj)

# Test de Anderson-Darling
cat("\n--- Test de Anderson-Darling ---\n")
ad_test_yj <- ad.test(precio_yj)
print(ad_test_yj)

# Test de Shapiro-Wilk para comparación
cat("\n--- Test de Shapiro-Wilk ---\n")
shapiro_test_yj <- shapiro.test(precio_yj)
print(shapiro_test_yj)

# Evaluación cualitativa
cat("\n--- EVALUACIÓN DE NORMALIDAD ---\n")
cat("Kolmogorov-Smirnov p-value:", ks_test_yj$p.value, "\n")
cat("Anderson-Darling p-value:", ad_test_yj$p.value, "\n")
cat("Shapiro-Wilk p-value:", shapiro_test_yj$p.value, "\n")

if(shapiro_test_yj$p.value > 0.05) {
  cat("CONCLUSIÓN: No se rechaza normalidad (p > 0.05)\n")
} else if(shapiro_test_yj$p.value > 0.01) {
  cat("CONCLUSIÓN: Evidencia moderada contra normalidad (0.01 < p < 0.05)\n")
} else {
  cat("CONCLUSIÓN: Fuente evidencia contra normalidad (p < 0.01)\n")
}
