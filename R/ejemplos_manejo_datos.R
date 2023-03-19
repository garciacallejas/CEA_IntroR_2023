
library(tidyverse)

# -------------------------------------------------------------------------
# lectura de datos

nat <- read.csv2("datos/nations.csv")
nat.co2 <- read.csv2("datos/nations_co2.csv",dec = ".")

# -------------------------------------------------------------------------

# selección de columnas - dos métodos equivalentes
nat.2 <- nat[,c("iso3c","year","region","gdp_percap")]
nat.3 <- select(nat,iso3c,region,year,gdp_percap)

# ordenar dataframe
nat.4 <- arrange(nat.3, year, iso3c,region)

# comprobar columnas
str(nat.4)
nat.4$gdp_percap <- as.numeric(nat.4$gdp_percap)

# eliminar posibles duplicados
duplicados <- which(duplicated(nat.4))
# nat.4[duplicados,]
# nat.unique <- unique(nat.4)

# transformar a "wide"
nat.wide <- pivot_wider(nat.4,names_from = year,values_from = gdp_percap)
# transformar de vuelta a "long"
nat.long <- pivot_longer(nat.wide,3:ncol(nat.wide),names_to = "year",values_to = "gdp_percap")

# comprobamos que tras la transformación, es el mismo conjunto de datos
nat.long <- as.data.frame(arrange(nat.long, year, iso3c,region))
nat.long$year <- as.integer(nat.long$year)
identical(nat.long,nat.4)

# eliminar filas con NA
nat.clean <- na.omit(nat.wide)

# seleccionar valores determinados
nat.paises <- subset(nat.clean, iso3c %in% c("ESP","MEX"))

#inciso, podemos dibujar la evolución del gdp de diferentes paises
nat.plot.data <- subset(nat.long,iso3c %in% c("ESP","MEX"))

ggplot(nat.plot.data, aes(x = year, y = gdp_percap, group = iso3c)) + 
  geom_line(aes(color = iso3c))

# unir diferentes tablas
head(nat.long)
head(nat.co2)

nat.completa <- left_join(nat.long,nat.co2)

# -------------------------------------------------------------------------
# ¿y si queremos dibujar las emisiones de co2 por región y año?
# debemos agregar datos por región y por año

nat.regiones.co2 <- nat.completa %>%
  group_by(region,year) %>%
  summarise(co2 = sum(co2_percap,na.rm = T))

ggplot(nat.regiones.co2,aes(x = year, y = co2, group = region)) + 
  geom_line(aes(color = region))

#¿qué sucede al final de los datos?
tail(nat.regiones.co2)
datos.cero <- subset(nat.regiones.co2,co2 == 0)
datos.cero

nat.regiones.co2.limpio <- subset(nat.regiones.co2, co2 != 0)
ggplot(nat.regiones.co2.limpio,aes(x = year, y = co2, group = region)) + 
  geom_line(aes(color = region))


# -------------------------------------------------------------------------
# figura: tasa de natalidad media por regiones, 
# con los datos del último año disponible

natalidad <- nat %>% 
  filter(year == max(year)) %>%
  select(country,birth_rate,region) %>%
  group_by(region) %>%
  summarise(mean_birth_rate = mean(birth_rate,na.rm = T),
            sd_birth_rate = sd(birth_rate, na.rm = T))

# warnings...
nat$birth_rate <- as.numeric(nat$birth_rate)

natalidad <- nat %>% 
  filter(year == max(year)) %>%
  select(country,birth_rate,region) %>%
  group_by(region) %>%
  summarise(mean_birth_rate = mean(birth_rate,na.rm = T),
            sd_birth_rate = sd(birth_rate, na.rm = T))

ggplot(natalidad) + 
  geom_errorbar(aes(x = region, y = mean_birth_rate,
                    ymin = mean_birth_rate - sd_birth_rate,
                    ymax = mean_birth_rate + sd_birth_rate,
                    color = region)) + 
  geom_point(aes(x = region, y = mean_birth_rate, fill = region), 
             shape = 21) + 
  theme_bw()
