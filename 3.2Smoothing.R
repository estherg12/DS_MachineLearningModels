# Mortalidad para Puerto Rico para 2015-2018
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(dslabs)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                  package="dslabs")

dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n == 1),
           which(n >= 28), tail_index:length(s))
  s[-out] %>% str_remove_all("[^\\d\\s]") %>% str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>% .[,1:5] %>% as_tibble() %>%
    setNames(c("day", header)) %>%
    mutate(month = month, day = as.numeric(day)) %>%
    pivot_longer(-c(day, month), names_to = "year", values_to = "deaths") %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month,
                        "JAN" = 1, "FEB" = 2, "MAR" = 3,
                        "APR" = 4, "MAY" = 5, "JUN" = 6,
                        "JUL" = 7, "AGO" = 8, "SEP" = 9,
                        "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")


# Ex 2: Grafique los estimadores suaves como función del día del año, todas en el mismo gráfico pero con diferentes colores
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

# Ex 3: Predecir 2s y 7s con solo la segunda covariable
library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>%
  glm(y ~ x_2, family = "binomial", data = .) %>%
  tidy()
qplot(x_2, y, data = mnist_27$train) # y es binario
# Ajuste una línea loess a los datos anteriores y grafique los resultados
# Observe que hay poder predictivo, excepto que la probabilidad condicional no es lineal
library(tidyverse)
library(dslabs)
data("mnist_27")

fit <- loess(as.numeric(y == "7") ~ x_2, data = mnist_27$train, degree = 1)
p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- ifelse(p_hat > 0.5, "7", "2") %>%
  factor(levels=levels(mnist_27$test$y))
accuracy <- mean(y_hat == mnist_27$test$y, na.rm = TRUE)

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

mnist_27$train %>% 
  mutate(y = ifelse(y == "7", 1, 0)) %>%
  ggplot(aes(x = x_2, y = y)) +
  geom_smooth(method = "loess", method.args = list(degree = 1)) +
  labs(
    title = "Probabilidad Condicional de ser '7' dado x_2",
    y = "Probabilidad (y=1 si es 7)",
    x = "x_2"
  )

