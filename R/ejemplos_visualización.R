
library(tidyverse)
library(maps)

# -------------------------------------------------------------------------
# sección de lectura de datos
data(iris)
sw <- read.csv2("datos/starwars_info_personajes_simplificado.csv")
co2 <- read.csv2("datos/mauna_loa_co2.csv")
eq <- read.csv2("datos/terremotos_simplificado.csv")

# -------------------------------------------------------------------------
# sección de tratamiento de datos

co2$date <- as.Date(co2$date)

# -------------------------------------------------------------------------
# sección de gráficas/figuras

# lienzo
f1 <- ggplot(iris)

# lienzo y ejes
f1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width))

# lienzo, ejes, puntos
f1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point()

# especies diferenciadas por color
f2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(color = Species))

# ¿en qué se diferencia de esto?
f2.2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(color = "darkgreen")

f3 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(color = "darkgreen", size = 3, aes(shape = Species))
f3

# -------------------------------------------------------------------------
# gráfico de barras
f4 <- ggplot(iris, aes(x = Species)) + 
  geom_bar(stat = "count")
sw.especie <- ggplot(sw, aes(x = species_group)) + 
  geom_bar(stat = "count")

# -------------------------------------------------------------------------
# gráfico de cajas
f6 <- ggplot(sw, aes(x = species_group, y = height)) +
  geom_boxplot(aes(fill = species_group))

# -------------------------------------------------------------------------
# división por grupos
f7 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(color = Species)) + 
  facet_grid(Species~.)


# -------------------------------------------------------------------------
# gráfico de líneas

l1 <- ggplot(co2, aes(x = date, y = co2ppm, group = 1)) + 
  geom_line() +
  scale_x_date("Date", date_breaks = "5 years",date_labels = "%Y") 

# -------------------------------------------------------------------------
# mapas

world_map <- map_data("world")
head(world_map)

mapamundi <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="gray") +
  theme_bw()

t1 <- ggplot() +
  geom_polygon(data = world_map,aes(x = long,
                                    y = lat,
                                    group = group),
               fill = "gray") +
  geom_point(data = eq, aes(x = Lon, # cuidado con el nombre de columnas...
                            y = Lat),
             color = "darkred", size = 1.2) +
  theme_bw()

# -----------------------------------------------------------------------
# mapa mejorado
t2 <- ggplot() +
  # mapa
  geom_polygon(data = world_map,aes(x = long, y = lat, group = group),
               fill = "gray") +
  # puntos
  geom_point(data = eq, aes(x = Lon, y = Lat, fill = M),
             shape = 21, size = 2) +
  # escala
  scale_fill_continuous(name = "magnitud", type = "viridis") +
  # título
  ggtitle("Magnitud y localización de terremotos históricos")+
  # tema
  theme_bw()


# -------------------------------------------------------------------------
# sección de escritura de resultados

ggsave(filename = "resultados/concentracion_CO2.png",plot = l1,width = 5,height = 5)


