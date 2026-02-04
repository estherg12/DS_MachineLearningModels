# 1000 schools dataset
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% 
  top_n(10, quality) %>% 
  arrange(desc(quality))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# ---- Q1: top escuales ID, avg ----
# Encontrar las mejores escuelas basadas en el puntaje (score)
top_10_schools <- schools %>% 
  arrange(desc(score)) %>% 
  select(id, size, score) %>% 
  head(10)

top_10_schools$id[1] # ID de la mejor escuela
top_10_schools$score[10] # puntuaje de la 10a escuela

# ---- Q2: comparación mediana de tamaño de escuela con la mediana de las top 10 mejores escuelas ----
median(schools$size) # mediana general del tamaño
schools %>%  # mediana de tamaño de top10
  arrange(desc(score)) %>% # de mayor a menor
  head(10) %>% # las primeras 10
  summarize(median_size = median(size)) %>% 
  pull(median_size)

# ---- Q3: tamaño mediano de las 10 peores escuelas ----
schools %>% 
  arrange(score) %>%           # Ordenamos de menor a mayor score
  head(10) %>%                 # Tomamos las 10 peores
  summarize(median_size = median(size)) %>% 
  pull(median_size)

# ---- Q4: peores y mejores escuelas son pequeñas ----
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) + # Todos los puntos en gris/transparente
  geom_point(data = filter(schools, rank<=10), col = 2) # Resaltar en rojo las 10 escuelas con mayor CALIDAD REAL

# ---- Q5: ID de la top1, score regularizada de la top10 ----
alpha <- 25
overall <- mean(sapply(scores, mean))
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# ---- Q6: alfa que minimiza RMSE ----
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# ---- Q7: con alfa regularizada, repite Q5 ----
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# ---- Q8: alfa con mínimo RMSE ----
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
