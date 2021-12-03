library(tidyverse)
library(psych)
library(car)
library(lmtest)
library(ggplot2)
library(lm.beta)


data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
str(data_sample_1)
summary(data_sample_1)

Assignment_2 <- data_sample_1 %>%
slice(-c(88, 34))

summary(Assignment_2)

 

Assignment_2 %>%
  ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = IQ, y = pain) + geom_point() +
  geom_smooth(method = "lm")

Assignment_2 %>%
  ggplot() + aes(x = household_income, y = pain) + geom_point() +
  geom_smooth(method = "lm")

model_backward <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                       cortisol_serum + mindfulness + 
                       weight + IQ + household_income, data = Assignment_2) 
model_backward %>% 	      
  plot(which = 5)	

model_backward %>% 	     
  plot(which = 4)

Assignment_2 %>% 	
  slice(c(46, 84, 85))	

model_backward %>%  
  plot(which = 2)

model_backward_residual <- enframe(residuals(model_backward)) 

model_backward_residual %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(model_backward_residual)

model_backward %>%      
  residualPlots()


model_backward %>%      
  plot(which = 3)

model_backward %>%      
  ncvTest()

model_backward %>%       
  bptest()

model_backward %>%       
  vif()

backward_code = step(model_backward, direction = "backward")


backward_code_last <- lm(pain ~ age + mindfulness +  
                             cortisol_serum + pain_cat, data = Assignment_2)

comparison_model <- lm(pain ~ age + sex + pain_cat + cortisol_serum + cortisol_saliva
                + mindfulness + STAI_trait, data = Assignment_2)


anova(backward_code_last, comparison_model)

summary(backward_code_last)
summary(comparison_model)

summary(model_backward)

AIC(model_backward)
AIC(backward_code_last)
AIC(comparison_model)


Theory_predict <- predict(comparison_model, Assignment_2)
Backward_predict <- predict(model_backward, Assignment_2)


Residual_1 = sum((Assignment_2[, "pain"] - Theory_predict)^2)
Residual_2 = sum((Assignment_2[, "pain"] - Backward_predict)^2)

Residual_1
Residual_2


Home_assignment_2 = read.csv("https://tinyurl.com/87v6emky")
str(Home_assignment_2)
colSums(is.na(Home_assignment_2))
summary(Home_assignment_2)
View(Home_assignment_2)

Home_assignment_2 <- Home_assignment_2 %>%
  slice(-c(88, 34))

Prediction_1 <- predict(comparison_model, Home_assignment_2)
View(Prediction_1)

Structure_1 = cbind(Home_assignment_2, Prediction_1)
View(Structure_1)

Residual = sum((Home_assignment_2$pain - predict(comparison_model))^2)
Residual


Structure_2 <- predict(backward_code_last, Home_assignment_2)

Prediction_2 = cbind(Home_assignment_2, Structure_2)
View(Prediction_2)

RSS = sum((Home_assignment_2$pain - predict(backward_code_last))^2)
RSS
