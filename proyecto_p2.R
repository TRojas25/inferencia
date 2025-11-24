

datos <- read.table("C:/Users/tomas/OneDrive/Escritorio/Rstudio codes/inferencia/fijiquakesdat.txt", 
                  header = TRUE)
#datos <- read.table("C:/Users/Felipe/Desktop/Proyecto Inferencia/P2/fijiquakes.dat.txt", 
 #                   header = TRUE)
mag <- datos$mag

Fn <- ecdf(mag)
#plot de distribucion empirica
library(ggplot2)

ggplot(data.frame(mag = datos$mag), aes(x = mag)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  labs(title = "Distribución Acumulada Empírica de Magnitudes",
       x = "Magnitud", 
       y = "Probabilidad Acumulada") +
  theme_minimal()

summary(Fn)

#2.b
alpha <- 0.05
n <- length(mag)
epsilon <- sqrt(1/(2*n) * log(2/alpha) )


x_vals <- seq(min(mag), max(mag), length.out = 1000)
Fn_vals <- Fn(x_vals)

Lx = pmax(Fn_vals - epsilon, 0)
Ux = pmin(Fn_vals + epsilon, 1)

# Graficar
# Crear dataframe con las bandas de confianza
df_bands <- data.frame(
  x = x_vals,
  Fn = Fn_vals,
  Lx = Lx,
  Ux = Ux
)

# Gráfico con ggplot2
ggplot(df_bands, aes(x = x)) +
  geom_ribbon(aes(ymin = Lx, ymax = Ux), fill = "red", alpha = 0.1) +
  geom_step(aes(y = Fn, color = "F empírica"), size = 1.2) +
  geom_line(aes(y = Lx, color = "Bandas 95%"), linetype = "dashed", size = 0.8) +
  geom_line(aes(y = Ux, color = "Bandas 95%"), linetype = "dashed", size = 0.8) +
  scale_color_manual(
    name = "",
    values = c("F empírica" = "blue", "Bandas 95%" = "red")
  ) +
  labs(
    title = "Función de Distribución Empírica con Bandas de Confianza 95%",
    x = "Magnitud",
    y = "Probabilidad Acumulada"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray")
  ) +
  scale_y_continuous(limits = c(0, 1))

#2.c
alphat <- 0.025
epsilont <- sqrt(1/(2*n) * log(2/alphat) )

x1 <- 4.9
x2 <- 4.3

Fn_x1 <- Fn(x1)
Fn_x2 <- Fn(x2)

L1 <- pmax(Fn_x1 - epsilont, 0)
U1 <- pmin(Fn_x1 + epsilont, 1)

L2 <- pmax(Fn_x2 - epsilont, 0)
U2 <- pmin(Fn_x2 + epsilont, 1)

inf <- L1 - U2
sup <- U1 - L2
cat("el intervalo es")
cat(inf, ",", sup)
Fn_x1 - Fn_x2

