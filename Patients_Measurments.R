# this document had been created on 2/5/25
# setwd("~/Desktop")

# import libraries 
library(lme4)
library(dplyr)
library(ggplot2)

# this is fake dataset, which you can create yourself utilizing this script: https://github.com/ZulfiiaDitto/Mixed-effect-linear-models/blob/main/Generating_dataset_for_mixed_effect_linear_model.ipynb
# or directly loaded dataset from this link:
# https://github.com/ZulfiiaDitto/Mixed-effect-linear-models/blob/main/patients_measurments.csv

df = read.csv('patients_measurments.csv', header = TRUE)
summary(df)

ggplot(df, aes(x = Measure.Type, y = Value, fill = Measure.Type)) + geom_boxplot() + ggtitle("Values Distribution")

# box plot shows the distribution within the place where the measurement had been collected,
# by measurement type and by Gender 

df %>% 
  group_by( Place, Measure.Type, Gender) %>% 
  ggplot(aes(x = interaction( Place, Measure.Type, Gender), y = Value,  fill = Gender)) + 
  geom_boxplot() + coord_flip() +
  labs(title='Values Distribution',
       subtitle='by measure type, gender and collection place')


# this box plot shows you distribution of the measurments based of the places 
df %>% 
  group_by(Place, Measure.Type) %>% 
  ggplot(aes(x = interaction(Place, Measure.Type), y = Value,  fill = Measure.Type)) + 
  geom_boxplot() + coord_flip() 

# by gender and Collection place
df %>% 
  group_by(Gender, Measure.Type) %>% 
  ggplot(aes(x = interaction(Gender, Measure.Type), y = Value,  fill = Gender)) + 
  geom_boxplot() + coord_flip() 

table(df$Measure.Type)

# lets calculate the how many time different measures had been taken
table = df %>%
  group_by(Patient.ID,Measure.Type,Place) %>%
  summarise(count= n()) 

#lets average this values per measurement type and place
table %>% group_by(Measure.Type,Place) %>% summarise(Mean = mean(count))

# since the each measurement is different in their ranges,
# we will have to filter small subsets based on the measurements type. 

# Blood Pressure df
BP = df %>% filter(df$Measure.Type == 'Blood Pressure')
table(BP$Place)

# Pulse df
pulse = df %>% filter(df$Measure.Type == 'Pulse')
table(pulse$Place)
  
# Respiratory rate df
rr = df %>% filter(df$Measure.Type == 'Respiratory Rate')
table(rr$Place)  


####################### predicting the Values of measurment BP, based on the Place and Gender and Age. 

model = lmer(Value ~ Place  + (1|Patient.ID)  , data=BP)
summary(model)


model.2 = lmer(Value ~ Place + Gender + Age + (1|Patient.ID)  , data=BP)
summary(model.2)

coef(model.2)

#### significance of the model utilizing the anova 

model.null = lmer(Value ~ Place  + (1|Patient.ID)  , data=BP, REML = FALSE)

model.atr = lmer(Value ~ Place + Gender + Age + (1|Patient.ID)  , data=BP, REML = FALSE)

anova(model.null, model.atr)

##### random slope 

model3 = lmer(Value ~ Place + (1+ Value|Patient.ID), data=BP)

summary(model3)

# you can see we over-complex the model -> isSingular() function in both cases return True 

isSingular(model3)

### You can play with the rest of subsets fro Pulse and Respiratory Rate. 


