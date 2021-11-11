#====== announce library ======#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)

#====== load original data from csv ======#
bike_buyers <- read.csv("bike_buyers.csv")
view(bike_buyers)

#====== clean data ======#
#change column name
names(bike_buyers)[names(bike_buyers) == "X.»¿ID"] <- "ID" 
ls(bike_buyers)

#drop NA for all row
bike_buyers_dropNA <- bike_buyers[complete.cases(bike_buyers), ] 

#drop null for all row
bike_buyers_dropNull <- bike_buyers_dropNA[!(bike_buyers_dropNA$Marital.Status=="" |
                        bike_buyers_dropNA$Gender=="" |
                        bike_buyers_dropNA$Home.Owner==""),] 

#see cleaned data
bike_buyers_dropNull %>%
  mutate_if(is.character,as.factor) -> bike_data
view(bike_data)


bike_data$Commute.Distance <- factor(bike_data$Commute.Distance,
                                     level = c("0-1 Miles","1-2 Miles","2-5 Miles",
                                               "5-10 Miles","10+ Miles"))
summary(bike_data)


#====================================================================================#

#say yes for purchased bike
bike_data_yes <- bike_data %>% filter(Purchased.Bike %in% 'Yes')
view(bike_data_yes)
summary(bike_data_yes)

#income purchased bike
bike_data_yes %>% 
  group_by(Income)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_income
  view(percent_income)
ggplot(percent_income,aes(x=Income,y=percent,fill=Income)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFA07A" , high = "#800000")+
  geom_text(aes(label = scales::percent(percent)))

#have children purchased bike
bike_data_yes %>% 
  group_by(Children)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike))-> percent_child
  view(percent_child)
ggplot(percent_child,aes(x=Children,y=percent,fill=Children)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#ADFF2F" , high = "#556B2F")+
  geom_text(aes(label = scales::percent(percent)))

#have car purchased bike
bike_data_yes %>% 
  group_by(Cars)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike))-> percent_car
  view(percent_car)
ggplot(percent_car,aes(x=Cars,y=percent,fill=Cars)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFB6C1" , high = "#FF1493")+
  geom_text(aes(label = scales::percent(percent)))

#age purchased bike
bike_data_agegroup <- bike_data_yes
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 80 & (bike_data_agegroup$Age  <= 89) ] <- "80-89"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 70 & (bike_data_agegroup$Age  <= 79) ] <- "70-79"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 60 & (bike_data_agegroup$Age  <= 69) ] <- "60-69"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 50 & (bike_data_agegroup$Age  <= 59) ] <- "50-59"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 40 & (bike_data_agegroup$Age  <= 49) ] <- "40-49"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 30 & (bike_data_agegroup$Age  <= 39) ] <- "30-39"
bike_data_agegroup$Age[(bike_data_agegroup$Age) >= 20 & (bike_data_agegroup$Age  <= 29) ] <- "20-29"

bike_data_agegroup %>% 
  group_by(Age)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_age
  view(percent_age)
ggplot(percent_age,aes(x=Age,y=percent,fill=Age)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))


#Marital purchased bike
bike_data_yes %>% 
  group_by(Marital.Status)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Marital
  view(percent_Marital)
ggplot(percent_Marital,aes(x=Marital.Status,y=percent,fill=Marital.Status)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#Gender purchased bike
bike_data_yes %>% 
  group_by(Gender)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Gender
view(percent_Gender)
ggplot(percent_Gender,aes(x=Gender,y=percent,fill=Gender)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#Education purchased bike
bike_data_yes %>% 
  group_by(Education)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Education
  view(percent_Education)
ggplot(percent_Education,aes(x=Education,y=percent,fill=Education)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#Occupation purchased bike
bike_data_yes %>% 
  group_by(Occupation)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Occupation
view(percent_Occupation)
ggplot(percent_Occupation,aes(x=Occupation,y=percent,fill=Occupation)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#ome.Owner purchased bike
bike_data_yes %>% 
  group_by( Home.Owner)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Home.Owner
view(percent_Home.Owner)
ggplot(percent_Home.Owner,aes(x= Home.Owner,y=percent,fill= Home.Owner)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#Commute.Distance purchased bike
bike_data_yes %>% 
  group_by(Commute.Distance)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Distance
view(percent_Distance)
ggplot(percent_Distance,aes(x=Commute.Distance,y=percent,fill=Commute.Distance)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#Region purchased bike
bike_data_yes %>% 
  group_by(Region)%>% 
  summarise(say_yes_for_bike=n()) %>% 
  mutate(percent = say_yes_for_bike/sum(say_yes_for_bike)) -> percent_Region
view(percent_Region)
ggplot(percent_Region,aes(x=Region,y=percent,fill=Region)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(percent)))

#====================================================================================#

#correlation
# say_yes <- bike_data %>% filter(Purchased.Bike %in% 'Yes')
# say_no <- bike_data %>% filter(Purchased.Bike %in% 'No')
# 
# checknumeric_all <- sapply(bike_data, is.numeric)######
# selectonlynumeric_all <- cor(bike_data[, checknumeric_all],bike_data$Purchased.Bike)
# corrplot(selectonlynumeric_all,method = "number")
# 
# checknumeric_yes <- sapply(say_yes, is.numeric)
# selectonlynumeric_yes <- cor(say_yes[, checknumeric_yes])
# corrplot(selectonlynumeric_yes,method = "number")
# 
# checknumeric_no <- sapply(say_no, is.numeric)
# selectonlynumeric_no <- cor(say_no[, checknumeric_no])
# corrplot(selectonlynumeric_no,method = "number")


#chi-square test
# bike_data_agegroup_all <- bike_data
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 80 & (bike_data_agegroup_all$Age  <= 89) ] <- "80-89"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 70 & (bike_data_agegroup_all$Age  <= 79) ] <- "70-79"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 60 & (bike_data_agegroup_all$Age  <= 69) ] <- "60-69"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 50 & (bike_data_agegroup_all$Age  <= 59) ] <- "50-59"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 40 & (bike_data_agegroup_all$Age  <= 49) ] <- "40-49"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 30 & (bike_data_agegroup_all$Age  <= 39) ] <- "30-39"
# bike_data_agegroup_all$Age[(bike_data_agegroup_all$Age) >= 20 & (bike_data_agegroup_all$Age  <= 29) ] <- "20-29"
# 
# table(bike_data$Purchased.Bike, bike_data$Children)
# chisq <- chisq.test(bike_data$Purchased.Bike, bike_data$Children, correct=FALSE)
# corrplot(chisq$residuals, is.cor = FALSE,method = "number")
# 
# table(bike_data$Purchased.Bike, bike_data_agegroup_all$Age)
# chisq <- chisq.test(bike_data$Purchased.Bike, bike_data_agegroup_all$Age, correct=FALSE)
# corrplot(chisq$residuals, is.cor = FALSE,method = "number")
# 



#============== test correlation ==================#

checknumeric_all <- sapply(bike_data, is.numeric)
selectonlynumeric_all <- cor(bike_data[, checknumeric_all])
corrplot(selectonlynumeric_all,method = "number")

say_yes <- bike_data %>% filter(Purchased.Bike %in% 'Yes')
checknumeric_yes <- sapply(say_yes, is.numeric)
selectonlynumeric_yes <- cor(say_yes[, checknumeric_yes])
corrplot(selectonlynumeric_yes,method = "number")

say_no <- bike_data %>% filter(Purchased.Bike %in% 'No')
checknumeric_no <- sapply(say_no, is.numeric)
selectonlynumeric_no <- cor(say_no[, checknumeric_no])
corrplot(selectonlynumeric_no,method = "number")


