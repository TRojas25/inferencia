#3.a
lsat <- c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
gpa <- c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)

datos <- data.frame(
  LSAT = c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594),
  GPA = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)
)

p <- cor(datos$LSAT, datos$GPA)
print(p)

#3.b
n <- length(gpa)
r_jack <- numeric(n)

# Calcular correlaciones eliminando cada observación
for(i in 1:n) {
  r_jack[i] <- cor(lsat[-i], gpa[-i])
}

# Calcular pseudovalores
pseudo <- n * p - (n-1) * r_jack

se_jack <- sqrt((1/(n*(n-1))) * sum((pseudo - mean(pseudo))^2))
print(se_jack)


#3.c
B <- 5000
set.seed(123) 
r_boot <- numeric(B)

for(b in 1:B) {
  indices <- sample(1:n, n, replace = TRUE)
  lsat_boot <- lsat[indices]
  gpa_boot <- gpa[indices]
  
  # Calcular correlación en la muestra bootstrap
  r_boot[b] <- cor(lsat_boot, gpa_boot)
}
se_boot <- sd(r_boot)
print(se_boot)


#3.d

se_asin <- (1 - p^2)/sqrt(n)
print(se_asin)




#extra
library(mvtnorm)
library(ggplot2)



mu <- c(mean(lsat), mean(gpa))
sigma <- cov(cbind(lsat, gpa))

set.seed(123)
normal_bivariada <- rmvnorm(1000, mean = mu, sigma = sigma)
colnames(normal_bivariada) <- c("LSAT", "GPA")
normal_bivariada <- as.data.frame(normal_bivariada)


p1 <- ggplot() +
  geom_point(aes(x = lsat, y = gpa), color = "red", size = 3, alpha = 0.7) +
  geom_point(aes(x = LSAT, y = GPA), data = normal_bivariada, 
             color = "blue", alpha = 0.3, size = 1) +
  labs(title = "Comparación: Datos Reales vs Normal Bivariada",
       subtitle = "Rojo: Datos reales | Azul: Normal bivariada simulada",
       x = "LSAT", y = "GPA") +
  theme_minimal()

# Gráfico 2: Contornos comparativos
p2 <- ggplot() +
  stat_density_2d(aes(x = LSAT, y = GPA), data = normal_bivariada, 
                  color = "blue", linetype = "dashed") +
  stat_density_2d(aes(x = lsat, y = gpa), color = "red") +
  geom_point(aes(x = lsat, y = gpa), color = "red", size = 2) +
  labs(title = "Contornos de Densidad",
       subtitle = "Rojo: Datos reales | Azul: Normal bivariada",
       x = "LSAT", y = "GPA") +
  theme_minimal()

# Gráfico 3: QQ-plot bivariado (para ver normalidad)
library(MASS)
p_lsat <- ggplot() +
  geom_qq(aes(sample = lsat), color = "red", size = 2) + 
  geom_qq_line(aes(sample = lsat), color = "red", linetype = "dashed", size = 1) +
  labs(title = "QQ-Plot para Normalidad - LSAT",
       x = "Cuantiles teóricos N(0,1)", y = "Cuantiles muestrales LSAT") +
  theme_minimal()

p_gpa <- ggplot() +
  geom_qq(aes(sample = gpa), color = "blue", size = 2) + 
  geom_qq_line(aes(sample = gpa), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "QQ-Plot para Normalidad - GPA",
       x = "Cuantiles teóricos N(0,1)", y = "Cuantiles muestrales GPA") +
  theme_minimal()

print(p1)
print(p2)
print(p_lsat)
print(p_gpa)



library(MVN)

# Test de Mardia para normalidad bivariada
resultado <- mvn(datos)
print(resultado)


