library(tidyverse)
library(lubridate)
library(skimr)
datos <- read_csv('datos_tpR.csv')

datos <- datos %>% #pasar fechas a tipo de dato Date
  mutate(Fecha = dmy(Fecha))

str(datos)
summary(datos)
glimpse(datos)
#view(datos)
skim(datos)
#no hay NAs ni datos extraños
#las fechas están en formato Date

datos <- datos %>%
  select(-Placa) %>%
  filter(Buffer == "Coatting") %>%
  mutate(Fecha = as.numeric(abs(Fecha - max(Fecha))))
  

datos %>%
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

cmasfreezer <- datos %>%
  filter(Control == "+", Temperatura == "Freezer") %>%
  lm(DO ~ Fecha, .)

g1 <- datos %>%
  filter(Control == "+", Temperatura == "Freezer") %>%
  ggplot(aes(x = Fecha,
       y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Freezer | Control: +")

g2 <- datos %>%
  filter(Control == "-", Temperatura == "Freezer") %>%
  ggplot(aes(x = Fecha,
             y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Freezer | Control: -")

g3 <- datos %>%
  filter(Control == "+", Temperatura == "Heladera") %>%
  ggplot(aes(x = Fecha,
             y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Heladera | Control: +")

g4 <- datos %>%
  filter(Control == "-", Temperatura == "Heladera") %>%
  ggplot(aes(x = Fecha,
             y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Heladera | Control: -")

g5 <- datos %>%
  filter(Control == "+", Temperatura == "Tamb") %>%
  ggplot(aes(x = Fecha,
             y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Tamb | Control: +")

g6 <- datos %>%
  filter(Control == "-", Temperatura == "Tamb") %>%
  ggplot(aes(x = Fecha,
             y = DO)) +
  geom_point() +
  geom_smooth(method="lm", se = FALSE) +
  labs(title = "Temperatura: Tamb | Control: -")

library(patchwork)

(g1 | g2) / (g3 | g4 ) / (g5 | g6)

#calculo outliers

median(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "+"])

limits_median_Tamb_mas <- c(median(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "+"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "+"]),
                   median(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "+"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "+"]))

limits_median_Tamb_menos <- c(median(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "-"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "-"]),
                            median(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "-"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Tamb" | datos$Control == "-"]))

limits_median_freezer_menos <- c(median(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "-"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "-"]),
                              median(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "-"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "-"]))

limits_median_freezer_mas <- c(median(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "+"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "+"]),
                                 median(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "+"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Freezer" | datos$Control == "+"]))

limits_median_heladera_mas <- c(median(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "+"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "+"]),
                               median(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "+"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "+"]))

limits_median_heladera_menos <- c(median(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "-"]) - 2.225 * IQR(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "-"]),
                                median(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "-"]) + 2.225 * IQR(datos$DO[datos$Temperatura == "Heladera" | datos$Control == "-"]))

#segun los limites anteriores, no hay outliers 