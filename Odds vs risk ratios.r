library(tidyverse)
library(broom)

set.seed(765)
n <- 100000

data <- tibble(animal = sample(x = c("lion","tiger","bear"), size = n, replace = TRUE, prob = c(0.2,0.3,0.5))) %>% 
mutate(diseased = case_when(animal == "lion" ~ sample(x = 0:1, size = n, replace = TRUE, prob = c(0.5,0.5)),
                            animal == "tiger" ~ sample(x = 0:1, size = n, replace = TRUE, prob = c(0.75,0.25)),
                            animal == "bear" ~ sample(x = 0:1, size = n, replace = TRUE, prob = c(0.9,0.1))))

head(data)

props <- data %>% 
group_by(animal) %>% 
summarise(diseased = round(mean(diseased),2)) %>% 
mutate(odds = c("1:9","1:1","1:3"),
       lab = paste0("Prob:",diseased,"\nOdds:",odds))

head(props)

data %>% 
ggplot(aes(x = animal, fill = as.logical(diseased)))+
geom_bar(position = "fill")+
geom_text(data = props, aes(label = lab, y = diseased))+
coord_flip()+
labs(title = "Simulated data demonstrating odds and risk ratios",
     x = "",
     y = "Proportion",
     fill = "Diseased?")

risk_model <- glm(formula = diseased ~ animal, family = quasipoisson, data = data) %>% 
tidy(conf.int = TRUE)

risk_model %>% 
filter(term != "(Intercept)") %>% 
mutate(exp.low = exp(conf.low),
       exp.high = exp(conf.high))

data$animal <- factor(data$animal)

levels(data$animal)

data$animal <- fct_relevel(`.f` = data$animal, "tiger")

levels(data$animal)

glm(formula = diseased ~ animal, family = quasipoisson, data = data) %>% 
tidy()

data$animal <- fct_relevel(`.f` = data$animal, "bear")

odds_model <- glm(formula = diseased ~ animal, family = quasibinomial, data = data) %>% 
tidy(conf.int = TRUE)

odds_model %>% 
filter(term != "(Intercept)") %>% 
mutate(exp.low = exp(conf.low),
       exp.high = exp(conf.high))

log_model <- glm(formula = diseased ~ animal, family = binomial, data = data) %>% 
tidy(conf.int = TRUE)

log_model %>% 
filter(term != "(Intercept)") %>% 
mutate(exp.low = exp(conf.low),
       exp.high = exp(conf.high))


