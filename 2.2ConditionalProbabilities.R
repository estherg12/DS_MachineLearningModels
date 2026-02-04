# 1. Test Disease Dataset
set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Ex 1.1: What is the probability that a test is positive?
mean(test)

# Ex 1.2: What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

# Ex 1.3: What is the probability that you have the disease if the test is positive?
mean(disease[test==1]==1)

# Ex 1.4: Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
#If a patient's test is positive, by how many times does that increase their risk of having the disease?
mean(disease[test==1]==1)/mean(disease==1)

# 2. Heights Dataset
library(dslabs)
data("heights")

# Ex 2.1: P(x) = Pr(Male | height = x) for each x
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Ex 2.2: Same but with cut() and quantiles
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# Ex 2.3: generate data from a bivariate normal distrubution using the MASS
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
# Estimate the conditional expectations and make a plot
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
