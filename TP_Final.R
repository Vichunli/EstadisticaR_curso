library(tidyverse)
library(lubridate)
library(skimr)
datos <- read_csv('datos_tpR.csv')

datos <- datos %>% #pasar fechas a tipo de dato Date
  mutate(Fecha = dmy(Fecha))

str(datos)
summary(datos)
glimpse(datos)
skim(datos)
#no hay NAs ni datos extraños
#las fechas están en formato Date

datos %>% #control vs DO
  ggplot(aes(x = Control, 
             y = DO,
             color = Control)) +
  geom_boxplot(outlier.shape = NA) + #no muestro los outliers por que con la linea que sigue pongo todos los puntos
  geom_jitter(width = 0.2,
              alpha = 0.5) +
  theme_minimal()
