library(tidyverse)
library(lubridate)
library(skimr)

rm(list = ls())

#Acá lo cambiaría por: datos<- read_csv(file = here('datos' , 'datos_tpR.csv'))

datos <- read_csv('datos_tpR.csv')

datos <- datos %>% #pasar fechas a tipo de dato Date
  mutate(Fecha = dmy(Fecha))

str(datos)
summary(datos)
glimpse(datos)
view(datos)
skim(datos)
#no hay NAs ni datos extraños
#las fechas están en formato Date


datos %>%
  select(-Placa) %>%
  filter(Buffer == "Coatting") %>%
  mutate(Fecha = as.numeric(abs(Fecha - max(Fecha)))) %>%
  group_by(Control, Temperatura, Fecha) %>%
  summarise(DOprom = mean(DO), SD = sd(DO)) %>%
  ggplot(aes(x = Fecha,
             y = DOprom,
             color = Temperatura)) +
  geom_point(size = 3) +
  geom_line(aes(linetype = Control)) +
  geom_errorbar(aes(ymin = DOprom - SD,
                    ymax = DOprom + SD), 
                width=.2) +
  theme_minimal()
         

