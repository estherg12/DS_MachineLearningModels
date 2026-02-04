library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# ---- Q1: Año con mayor nº de ratings (media) ----
result <- movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

head(result) # Máximo

# ---- Q2: rate y avg nº ratings de películas ----
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), # número total de calificaciones
            years = 2018 - first(year), # años desde el estreno hasta 2018
            title = title[1],
            rating = mean(rating)) %>% # calificacion promedio
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) %>% 
  filter(str_detect(title, "Shawshank Redemption|Forrest Gump"))

# ---- Q4: avg rating VS ratins x year ----
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), # número total de calificaciones
            years = 2018 - first(year), # año de estrueno
            title = title[1],
            rating = mean(rating)) %>% # calificación promedio
  mutate(rate = n/years) %>% # cálculo de calificaciones x año
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# ---- Q6: avg rating x semana VS date ----
movielens %>% mutate(date = as_datetime(timestamp)) %>% 
  mutate(date = round_date(date, unit = "week")) %>% # redondea al inicio de la semana
  group_by(date) %>% # agrupar x la nueva fecha semanal
  summarize(rating = mean(rating)) %>% # cálculo del promedio calificación x semana
  ggplot(aes(date, rating)) +
  geom_point() + # puntos de promedio de cada semana
  geom_smooth() # curva de tendencia (suavizado)

# ---- Q8: Género con menor avg rating ----
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
