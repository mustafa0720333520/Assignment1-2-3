library(tidyverse)
library(psych)
library(car)
library(lmtest)
library(ggplot2)
library(lm.beta)





data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
str(data_sample_1)
summary(data_sample_1$sex)
summary(data_sample_1)
data_sample_1 <- data_sample_1 %>% 	
  slice(-c(88, 34))
dataReady<-data_sample_1
summary(dataReady)
View(dataReady)



dataReady<-data_sample_1
summary(dataReady)
View(dataReady)

dataReady %>%
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()

dataReady %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

model_one <- lm(pain ~ age + sex, data = dataReady)

model_two <- lm(pain ~ age + sex + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + STAI_trait, data = dataReady) 
summary(model_two)
str(model_two)

dataReady %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() +
  geom_smooth(method = "lm")

dataReady %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() +
  geom_smooth(method = "lm")

model_two %>% 	      
  plot(which = 5)	

model_two %>% 	     
  plot(which = 4)

dataReady %>% 	
  slice(c(46, 73, 85))	

model_two %>%  
  plot(which = 2)

dataReady %>% 	
  slice(c(73, 84, 102))

plot(x = model_two, which = 1) #linearity residual

linier <- fitted.values( object = model_two ) #fitted and observe linearity
plot( x = linier,
        y = dataReady$pain,
        xlab = "Fitted Values",
        ylab = "Observed Values"
)

model_two_residual <- enframe(residuals(model_two)) #normality histogram
model_two_residual %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(model_two_residual)

final_data <- dataReady %>% 	
  slice(-c(73))

 

model_one <- lm(pain ~ age + sex, data = final_data)

model_two <- lm(pain ~ age + sex + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + STAI_trait, data = final_data)

model_two %>% 	     
  plot(which = 5)	

model_two %>% 	     
  plot(which = 4)

model_two %>%      
  plot(which = 2)

model_two_residual <- enframe(residuals(model_two))

model_two_residual %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(model_two_residual)                  

model_two %>%       
  residualPlots()     #linearity 


model_two %>%       #homoscedasticity test
  plot(which = 3)

model_two %>%       #homoscedasticity test
  ncvTest()

model_two %>%       #homoscedasticity test
  bptest()

model_two %>%       #no multicollinearity test
  vif()

model_two
model_one

anova(model_one, model_two)

summary(model_one)
summary(model_two)

AIC(model_one)
AIC(model_two)

confint(model_one)
confint(model_two)

lm.beta(model_one)
lm.beta(model_two)

#credit to zlotan for the code 
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
table1 <- coef_table(model_one)
table2 <- coef_table(model_two)
table1
table2

summary(model_one)$adj.r.squared

summary(model_two)$adj.r.squared




