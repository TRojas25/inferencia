#datos <- read.csv("C:/Users/tomas/OneDrive/Escritorio/Rstudio codes/inferencia/ames-housing.csv")
datos = read.csv("C:/Users/Felipe/Desktop/Proyecto Inferencia/ames-housing.csv")
library(e1071)
library(ggplot2)
library(nortest)
library(MASS)
library(survival)
library(nortest)
library(fitdistrplus)
library(forecast)
library(VGAM)


##### 1.a #####

precio <- datos$SalePrice
length(precio)
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




#BOXPLOT
bp <- boxplot(precio,
              names = c("Precio de venta"),
              main = "Comparación de Dispersión y Outliers",
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

#### 1.b - 1.c - 1.d ######
#Datos originales ajustes
#Precio ajustado
precio_esc <- datos$SalePrice/1000


# Ajuste Lognormal (parámetros 'meanlog' y 'sdlog')
fit_lnorm <- fitdist(precio_esc, "lnorm")
print("--- Resumen Lognormal ---")
print(fit_lnorm)

# Ajuste Gamma (parámetros 'shape' y 'rate')
fit_gamma <- fitdist(precio_esc, "gamma")
print("--- Resumen Gamma ---")
print(fit_gamma)

# Ajuste Weibull (parámetros 'shape' y 'scale')
fit_weibull <- fitdist(precio_esc, "weibull")
print("--- Resumen Weibull ---")
print(fit_weibull)


# Tests de bondad de ajuste
gof_comparison<- gofstat(
  list(fit_lnorm, fit_gamma, fit_weibull),
  fitnames = c("Log-Normal", "Gamma", "Weibull")
  )
cat("\n=== COMPARACIÓN DE BONDAD DE AJUSTE ===\n")
print(gof_comparison)



# Compara el ajuste de las tres distribuciones en un solo gráfico (QQ-Plot, etc.)

par(mfrow = c(2, 2)) # Divide la ventana gráfica en 4 paneles
plot.legend <- c("Lognormal", "Gamma", "Weibull")

# Genera los gráficos de diagnóstico comparados
denscomp(list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend,  xlab = "Precio Escalado")
qqcomp(list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)  
cdfcomp(list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend,  xlab = "Precio Escalado")
ppcomp(list(fit_lnorm, fit_gamma, fit_weibull), legendtext = plot.legend)

# Vuelve al layout original (opcional)
par(mfrow = c(1, 1))

##Kolmogorov smirnov para lognormal

meanlog_est <- fit_lnorm$estimate["meanlog"]
sdlog_est <- fit_lnorm$estimate["sdlog"]
ks_lnorm_test <- ks.test(precio, "plnorm", 
                         meanlog = meanlog_est, 
                         sdlog = sdlog_est)

print(ks_lnorm_test)
#Datos transformados con BoxCox


lambda_forecast <- BoxCox.lambda(precio)
print(paste("Lambda óptima de Box-Cox:", round(lambda_forecast, 4)))
precio_box <- BoxCox(precio, lambda = lambda_forecast)
precio_box <- as.numeric(precio_box)
asimetria <- skewness(precio_box, na.rm = TRUE)
print(paste("Asimetría:", round(asimetria, 4)))

# Genera el histograma de los datos transformados
hist(precio_box, main = "Histograma de precio_box (Transformado Box-Cox)")


# --- Ajuste Normal ---
fit_norm <- fitdist(precio_box, "norm")
print("--- Resumen Normal ---")
print(fit_norm)

# --- Definimos distribución t-Student generalizada ---
dt_t <- function(x, m, s, df) dt((x - m)/s, df) / s
pt_t <- function(q, m, s, df) pt((q - m)/s, df)
qt_t <- function(p, m, s, df) m + s * qt(p, df)
rt_t <- function(n, m, s, df) m + s * rt(n, df)

# --- Ajuste T de Student usando fitdist (ya que ahora tenemos dt_t, pt_t, etc.) ---
fit_tstudent <- fitdist(precio_box, "t_t",
                        start = list(m = mean(precio_box), s = sd(precio_box), df = 10))
print("--- Resumen T-Student ---")
print(fit_tstudent)

# --- Definimos distribución Laplace (doble exponencial) ---
dlaplace2 <- function(x, location, scale) { VGAM::dlaplace(x, location, scale) }
plaplace2 <- function(q, location, scale) { VGAM::plaplace(q, location, scale) }
qlaplace2 <- function(p, location, scale) { VGAM::qlaplace(p, location, scale) }
rlaplace2 <- function(n, location, scale) { VGAM::rlaplace(n, location, scale) }

# --- Ajuste Laplace ---
fit_laplace <- fitdist(precio_box, "laplace2",
                       start = list(location = mean(precio_box), scale = sd(precio_box)/sqrt(2)))
print("--- Resumen Laplace ---")
print(fit_laplace)

# --- Tests de bondad de ajuste ---
gof_comparison_boxcox <- gofstat(
  list(fit_norm, fit_tstudent, fit_laplace),
  fitnames = c("Normal", "T de Student", "Laplace")
)

cat("\n=== COMPARACIÓN DE BONDAD DE AJUSTE ===\n")
print(gof_comparison_boxcox)

# --- Gráficos comparativos ---
par(mfrow = c(2, 2))
plot.legend <- c("Normal", "T-Student", "Laplace")

denscomp(list(fit_norm, fit_tstudent, fit_laplace),
         legendtext = plot.legend,
         main = "Histogram and theorical densities (Box-Cox)",
         xlab = "Precio Transformado (Box-Cox)")

qqcomp(list(fit_norm, fit_tstudent, fit_laplace),
       legendtext = plot.legend,
       main = "Q-Q Comparado (Box-Cox)")

cdfcomp(list(fit_norm, fit_tstudent, fit_laplace),
        legendtext = plot.legend,
        main = "CDF Comparado (Box-Cox)")

ppcomp(list(fit_norm, fit_tstudent, fit_laplace),
       legendtext = plot.legend,
       main = "P-P Comparado (Box-Cox)")

par(mfrow = c(1, 1))

### 1.e ###

valor = 200000
valor_box = BoxCox(valor , lambda = lambda_forecast)
pt_t(valor_box , fit_tstudent$estimate[1] ,fit_tstudent$estimate[2], fit_tstudent$estimate[3])

### 1.f ###
mu = fit_tstudent$estimate[1]
print(mu)
valor_esperado = InvBoxCox(mu, lambda = lambda_forecast)
print(valor_esperado)

### 1.g ###
alpha = 0.05
valor_t = qt_t(1-alpha/2,fit_tstudent$estimate[1] ,fit_tstudent$estimate[2], fit_tstudent$estimate[3] )
print(upper)


Lower = fit_tstudent$estimate[1] -fit_tstudent$estimate[2]/sqrt(length(precio))*valor_t

Upper = fit_tstudent$estimate[1] +fit_tstudent$estimate[2]/sqrt(length(precio))*valor_t
print("El intervalo de confianza para mu con boxcox es")
cat(Lower, "," , Upper)


Lower_trans = InvBoxCox(Lower, lambda = lambda_forecast)
Upper_trans = InvBoxCox(Upper, lambda = lambda_forecast)
print("El intervalo de confianza para mu es")
cat(Lower_trans, "," , Upper_trans)

