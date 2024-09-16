library(tidyverse)
escenario <- read.csv("Titanicv2.csv")
View (escenario)
str(escenario)
summary (escenario)

escenario <- drop_na(escenario)

escenario %>% 
  select(Sex, Pclass, Age) %>% 
  filter(Sex == "female") %>% 
  filter(Pclass == "Lower Clas") %>% 
  filter(Age <= 20) %>% 
  count(Age)

  
  
escenario %>% 
  select(Pclass, Sex) %>% 
  group_by(Sex)
  


  sum(escenario$Sex=="female")
sum(escenario$Sex=="male",escenario$Pclass=="Lower Clas")

escenario %>% 
  select(Pclass,Sex) %>% 
  group_by(Pclass)

escenario %>% 
  filter(Pclass == "Middle Class") %>% 
  filter(Survived == "No") %>% 
  filter(Sex == "male") %>% 
  group_by(Embarked) %>% 
  select(Pclass, Survived, Sex,Embarked) %>% 
  count(Embarked)

escenario %>% 
  filter(Sex =="Female") %>%
  filter(Survived =="Yes") %>% 
  select(Pclass, Survived, Sex, Embarked) %>%
  count()


escenario %>% 
  select(Age,Name, Sex) %>% 
  summarise(max(Age))%>% 

  
escenario %>% 
  select(Name, Sex, Age) %>% 
  filter(Survived=="Yes") %>% 
  head()

escenario %>% 
  filter(Sex == "Female") %>% 
  select(Name, Age) %>% 
  head()

escenario %>% 
  filter (Pclass=="Lower Clas") %>%
  select(Name) %>%
  head()  

escenario %>% 
  select(Name, Age, Pclass) %>% 
  filter(Pclass == "Lower Clas", Age > 60) %>% 
  head()
  
escenario %>% 
  select(Name, Survived, Age) %>% 
  filter(Survived == "Yes") %>% 
  count(Survived) %>% 
  head()

escenario %>% 
  select(Survived) %>% 
  filter(Survived == "No") %>% 
  count(Survived) %>% 
  head()

escenario %>% 
  ggplot(aes(x=Survived)) +
  geom_bar()


escenario %>% 
  select(Age) %>% 
  summarise(mean(Age))

escenario %>% 
  select(Age) %>% 
  summarise(median(Age))

escenario %>% 
  select(Fare) %>% 
  summarise(max(Fare), min(Fare), mean(Fare))

escenario %>% 
  ggplot(aes(x=Sex, y= Age)) +
  geom_violin(scale= 'count')
  
escenario %>% 
  select(Name, Age, Pclass, Survived) %>% 
  filter(Age == 76)
