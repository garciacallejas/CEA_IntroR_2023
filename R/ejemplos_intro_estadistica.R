
library(tidyverse)


# -------------------------------------------------------------------------

eq <- read.csv2("datos/terremotos_simplificado.csv")
sw <- read.csv2("datos/starwars_info_personajes_simplificado.csv")

# -------------------------------------------------------------------------
# poblaciones y muestras

edades <- sample(25:40,size = 100,replace = TRUE)
hist(edades)

muestra_edades <- sample(edades,
                         size = 5,
                         replace = FALSE)
hist(muestra_edades)

# las propiedades de una población o muestra se describen con medidas
# de centralidad: media, mediana, moda
# de dispersión: varianza, desv. típica, asimetría, curtosis

# media poblacional
pop.mean <- mean(eq$M)
pop.mean

# media muestral
sample.eq <- sample(eq$M,
                    size = 10,
                    replace = FALSE)
sample.mean <- mean(sample.eq)
sample.mean

# mediana
median(eq$M)

# moda - no hay función por defecto en R, podemos crear una
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(eq$M)

ggplot(eq, aes(x=M)) +
  geom_histogram(binwidth=.25, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(M, na.rm=T)),
             color="black", size=1) +
  geom_vline(aes(xintercept=median(M, na.rm=T)),
             color="darkred", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=Mode(M)),
             color="darkgreen", linetype="dotted", size=1)


# datos con igual media pero diferente dispersión
n1 <- rnorm(500,0,0.5)
n2 <- rnorm(500,0,5)
compara <- data.frame(n1 = n1, n2 = n2)
compara.long <- pivot_longer(compara,cols = n1:n2)
compara.plot <- ggplot(compara.long,aes(x = value, color = name)) +
  geom_density() +
  theme_bw()
compara.plot

# -------------------------------------------------------------------------
muestra.sw <- sample(sw$height,size = 10, replace = FALSE)

# intervalos de confianza de una media muestral
mean(muestra.sw)
t.test(muestra.sw,conf.level = 0.95)[["conf.int"]]

# -------------------------------------------------------------------------
# figuras

# histograma
ggplot(sw,aes(x = height)) +
  geom_histogram(binwidth = 10, colour="black", fill="white") +
  theme_bw() +
  labs(x = "altura (cm)", y = "frecuencia") +
  ggtitle("histograma")

# densidad
ggplot(sw,aes(x = height)) +
  geom_density(colour="black") +
  theme_bw() +
  labs(x = "altura (cm)", y = "densidad") +
  ggtitle("Distribución de densidad")

# boxplot
ggplot(sw,aes(y = height)) +
  geom_boxplot() +
  theme_bw() +
  scale_x_continuous(breaks=NULL) +
  labs(y = "altura", x = "") +
  ggtitle("Boxplot")

# boxplot y puntos
ggplot(sw,aes(y = height)) +
  geom_jitter(aes(x = 0)) +
  geom_boxplot(alpha = .8) +
  theme_bw() +
  scale_x_continuous(breaks=NULL) +
  labs(y = "altura", x = "") +
  ggtitle("Boxplot y distribución de puntos")

# -------------------------------------------------------------------------
# tests de hipótesis


# 1 - muestras aproximadamente normales
m1 <- rnorm(500,0.5,1)
m2 <- rnorm(500,1.5,1)

t.test(m1,m2)

# -------------------------------------------------------------------------
# muestras no normales

m3 <- na.omit(eq$M)
m4 <- na.omit(sw$mass)

wilcox.test(m3,m4)

# -------------------------------------------------------------------------
# correlaciones
sw.2 <- subset(sw,!is.na(height) & !is.na(mass))

cor(x = sw.2$height,
    y = sw.2$mass,
    method = "pearson")

dat <- mtcars %>%
  select(-vs, -am)
head(dat, 5)
round(cor(dat),digits = 2)

library(corrplot)

corrplot(cor(dat),
         method = "number",
         type = "upper")


# -------------------------------------------------------------------------
# importante siempre visualizar los datos

aq <- datasets::anscombe
head(aq)

p1 <- ggplot(aq,aes(x1, y1)) +
  geom_point(color = "darkorange", size = 2.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  # expand_limits(x = 0, y = 0) +
  labs(x = "x1", y = "y1",
       title = "Dataset 1" ) +
  geom_smooth(method = "lm") +
  theme_bw()
p2 <- ggplot(aq,aes(x2, y2)) +
  geom_point(color = "darkorange", size = 2.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  # expand_limits(x = 0, y = 0) +
  labs(x = "x2", y = "y2",
       title = "Dataset 2" ) +
  geom_smooth(method = "lm") +
  theme_bw()
p3 <- ggplot(aq,aes(x3, y3)) +
  geom_point(color = "darkorange", size = 2.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  # expand_limits(x = 0, y = 0) +
  labs(x = "x3", y = "y3",
       title = "Dataset 3" ) +
  geom_smooth(method = "lm") +
  theme_bw()
p4 <- ggplot(aq,aes(x4, y4)) +
  geom_point(color = "darkorange", size = 2.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  # expand_limits(x = 0, y = 0) +
  labs(x = "x4", y = "y4",
       title = "Dataset 4" ) +
  geom_smooth(method = "lm") +
  theme_bw()
library(patchwork)
wrap_plots(p1,p2,p3,p4)

# -------------------------------------------------------------------------
# modelos de regresión lineal... primero, siempre, comprobar los datos

ggplot(sw, aes(x = mass, y = height)) + 
         geom_point()
hist(sw$mass)
hist(sw$height)

# ajustar el modelo
m1 <- lm(height ~ mass, data = sw)
m1
summary(m1)

# comprobar que los residuos se ajustan a una distribución normal
res <- resid(m1)
hist(res)

plot(m1)

# tabla resumen más limpia
library(broom)
tidy(m1)

# intervalos de confianza de los coeficientes
confint(m1)

# coeficiente de determinación
summary(m1)$adj.r.squared

# visualización
ggplot(sw, aes(x = mass, y = height)) +
  geom_smooth(method = "lm") +
  geom_point()
