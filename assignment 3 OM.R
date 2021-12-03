library(tidyverse)
library(psych)
library(cAIC4) 
library(r2glmm) 
library(lme4) 
library(lmerTest)
library(MuMIn)


# code below from https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
My_file_3 = read.csv("https://tinyurl.com/b385chpu")
my_file_4 = read.csv("https://tinyurl.com/4f8thztv")

View(My_file_3)
View(my_file_4)
str(My_file_3)
str(my_file_4)
colSums(is.na(My_file_3))
colSums(is.na(my_file_4))
summary(My_file_3)
summary(my_file_4)

My_file_3 = My_file_3 %>% 	
  mutate(hospital = factor(hospital))


My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = age) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)		

My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = sex) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_saliva) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)


intercept = My_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)	


intercept     


model_1 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                cortisol_saliva + mindfulness, data = My_file_3)

model_1


random_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                     cortisol_saliva + mindfulness + 
                       (1|hospital), data = My_file_3)

random_model

summary(random_model)
plot(random_model)
qqnorm(resid(random_model))
qqline(resid(random_model))

confidence_level<-confint(random_model)
confidence_level
confidence_level <-stdCoef.merMod(random_model)
confidence_level


mixing_models = lmer(pain ~ cortisol_serum +
                       (cortisol_serum|hospital), data = My_file_3)

summary(mixing_models)


sum(residuals(model_1)^2) 

sum(residuals(random_model)^2)  

sum(residuals(mixing_models)^2)   

cAIC(random_model)$caic 

cAIC(mixing_models)$caic

anova(random_model, mixing_models)


r.squaredGLMM(random_model)

r2beta(random_model, method = "nsj", data = my_file_4)

Prediction_random <- predict(random_model, my_file_4, allow.new.levels=T)

predicted_final = cbind(my_file_4, Prediction_random)
View(predicted_final)

RSS_1 = sum((my_file_4$pain - Prediction_random)^2)
RSS_1

model_mean_value <- lmer(pain ~ 1 + (1|hospital), data = my_file_4)
TSS_1 = sum((my_file_4$pain - predict(model_mean_value))^2)
TSS_1

explanation_variance <- 1-(RSS_1/TSS_1)
explanation_variance

summary(Prediction_random)


My_file_3 <- My_file_3 %>%
  mutate(pred_int = predict(random_model), pred_slope = predict(mixing_models))


My_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                     aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)


My_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
  aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)


