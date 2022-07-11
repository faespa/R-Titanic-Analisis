#install.packages("titanic")

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived), Pclass = factor(Pclass), 
         Sex = factor(Sex))

#?titanic_train
head(titanic_train)
head(titanic)

p <- titanic %>% 
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, fill = Sex)) + 
  geom_density(alpha = 0.9) +
  facet_grid(Sex ~ .)


params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>% 
  ggplot(aes(sample= Age)) +
  geom_qq(dparams = params) +
  geom_abline()


#bar plot survided
p <- titanic %>% 
  ggplot(aes(x = Survived)) +
  geom_bar()


#bar plot survided ans sex same Bar
p <- titanic %>% 
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar()


#bar plot survided ans sex diferent bar
p <- titanic %>% 
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge()) 


#density plot age filled by survival status change Y for count
p <- titanic %>% 
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, fill = factor(Survived, labels=c("no","yes")))) + 
  geom_density(alpha = 0.2) +
  ylab("Count") +
  labs(fill="Survived")



#filter no paid fare and make boxplot fair group and survived
p <- titanic %>%
  filter(!is.na(Fare) & !Fare <= 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot()+
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) + 
  geom_jitter(alpha = 0.2)


#bar plots for Passenger Classt
p <- titanic %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()

p <- titanic %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

p <- titanic %>% 
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())


#density plots for age, filled by survival status yaxis =count, with count on the y-axis,
p <- titanic %>%
  ggplot(aes(x = Age, y = ..count..,fill = Survived)) + 
  geom_density(alpha = 0.2) +
  facet_grid(Pclass ~ Sex)
p



