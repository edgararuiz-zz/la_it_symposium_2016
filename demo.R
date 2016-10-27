# Load libraries
library(tidyverse)
library(babynames)

# Load baby names into a data frame
baby_table <- babynames

# Filter on a name
my_name <- babynames %>%
  filter(name=="John")

# Filter on a name and gender
my_name <- babynames %>%
  filter(name=="John", sex=="M")

#Simple plot
plot(n~year,data=my_name)

#Make it a trend line
plot(n~year,data=my_name, type="l")
#Add a title
plot(n~year,data=my_name, type="l", main="By Number of People")
#Proportion provides a different perspective
plot(prop~year,data=my_name, type="l")
#Add title
plot(prop~year,data=my_name, type="l",main="By Proportion of Population")
#Find out popular names by year
popular <- babynames %>%
  group_by(year) %>%
  arrange(desc(year,n)) 
#Get the top baby name by year
popular <- babynames %>%
  group_by(year) %>%
  arrange(desc(year,n)) %>%
  mutate(rank=row_number()) %>%
  filter(rank==1)
#Popular male names
popular_male <- babynames %>%
  filter(sex=="M") %>%
  group_by(year) %>%
  arrange(desc(year,n)) %>%
  mutate(rank=row_number()) %>%
  filter(rank==1)
#Popular female names
popular_female <- babynames %>%
  filter(sex=="F") %>%
  group_by(year) %>%
  arrange(desc(year,n)) %>%
  mutate(rank=row_number()) %>%
  filter(rank==1)
#Total population
population <- babynames %>%
  group_by(year) %>%
  summarise(births=sum(n)) 

#Plot population
plot(births~year, data=population,type="l")

#Load unenployment data
unemployment <- read_csv("unemployment.csv") 

#Join to population data
population <- population %>%
  inner_join(unemployment,by=c("year"="Year"))

#Correlation 
cor(population)

#Correlation table
population_cor <- cor(population)

#Feb to birth plot
plot(births~Feb,data=population)

#Test model
b_model<- lm(births~Feb, data=population)
summary(b_model)

#Filter for this century
population <- filter(population, year>=2000)

#Correlation table
population_cor <- cor(population)

#Feb to birth plot
plot(births~Jan,data=population)

#Test model
b_model<- lm(births~Jan, data=population)
summary(b_model)

#Create prediction data set
b_pred <- predict(b_model, population, interval="prediction")

#Convert to data frame
b_pred <- as.data.frame(b_pred) %>%
  mutate(year=c(2000:2014))

#Add to population data frame
population <- inner_join(population, b_pred, by="year")

#Introducing ggplot
ggplot(data=population) +
  geom_line(aes(x=year,y=births)) 

#Add fitted line
ggplot(data=population) +
  geom_line(aes(x=year,y=births)) + 
  geom_line(aes(x=year,y=fit))

#Change color to fitter line
ggplot(data=population) +
  geom_line(aes(x=year,y=births)) + 
  geom_line(aes(x=year,y=fit), color="green")

#Adding prediction intervals
ggplot(data=population) +
  geom_line(aes(x=year,y=births)) + 
  geom_line(aes(x=year,y=fit), color="green") + 
  geom_line(aes(x=year,y=upr), color="blue")+ 
  geom_line(aes(x=year,y=lwr), color="red")

#Adding title
ggplot(data=population) +
  geom_line(aes(x=year,y=births)) + 
  geom_line(aes(x=year,y=fit), color="green") + 
  geom_line(aes(x=year,y=upr), color="blue")+ 
  geom_line(aes(x=year,y=lwr), color="red") +
  labs(title="Births Model")






