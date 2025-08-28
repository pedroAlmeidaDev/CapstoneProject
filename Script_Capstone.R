install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

library(readr)

library(dplyr)

divvy_2019_q1 <- read.csv("/kaggle/input/divvy-trips-2019-q1-divvy-trips-2019-q1/Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")

head(divvy_2019_q1)

divvy_2020_q1 <- read.csv("/kaggle/input/divvy-trips-2020-q1-divvy-trips-2020-q1/Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

head(divvy_2020_q1)

#contagem de membros por tipo em 2020
Qtd.MembersByTypeIn2020 <- divvy_2020_q1 %>%
  group_by(member_casual) %>%
  summarize(count_members = n(), .groups = "drop")

head(Qtd.MembersByTypeIn2020)

#Top 5 estações onde mais partem passeios
Qtd.RidesPerStationIn2020 <- divvy_2020_q1 %>%
  group_by(start_station_name) %>%
  summarize(count_rides = n(), .groups = "drop") %>%
  arrange(desc(count_rides)) %>%
  head(5)

head(Qtd.RidesPerStationIn2020)

#Quantidade de passeios na estação Canal St & Adams S
NumberOfToursInCanalStAdamsSt <- divvy_2020_q1 %>%
  filter(start_station_name == "Canal St & Adams St")

#Representação em barras da quantidade de usuários que partem da estação de Canal St & Adams St por tipo de usuário
ggplot(data = NumberOfToursInCanalStAdamsSt) + 
  geom_bar(mapping = aes(x = member_casual, fill = member_casual)) +
  geom_text(
    mapping = aes(x = member_casual, label = after_stat(count)),
    stat = "count",
    vjust = -0.0
  ) + 
  labs(
    title = "Qtd. Usuários do Canal St & Adams St",
    subtitle = "Por Tipo de Usuário"
  )