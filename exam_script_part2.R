#### FINAL EXAM - PART 2

## Making analytic data

library(tidyverse)

#load data
my.data <- read_csv(file="exam_data_f16.csv")

#View(my.data)

raw_data <- read_csv(file="exam_data_f16.csv" ,na=c("","NA","-999", "-888"))

#tell R about categorical variables gender and education
categorical_variables <- select(my.data, gender, education)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)

categorical_variables$education <- as.factor(categorical_variables$education)
levels(categorical_variables$education) <- list("HS"=1,"finished HS"=2,"Some college"=3,"college grad"=4,"graduate degree"=5)


#create scale items
agreeableness_items <- select (my.data, A1, A2, A3, A4, A5)
conscientiousness_items <- select (my.data, C1, C2, C3, C4, C5)
performance_items <- select (my.data, JP1, JP2, JP3, JP4, JP5 )

age <- select(my.data, age)


#check for out of range values
psych::describe(agreeableness_items) # yes, out of range value
psych::describe(conscientiousness_items) # no out range values
psych::describe(performance_items) # no out range values


is_bad_value <- agreeableness_items <1 | agreeableness_items >6
#View(agreeableness_items)
agreeableness_items[is_bad_value] <- NA
#View(agreeableness_items)

#flip reverse key items
agreeableness_items <- mutate(agreeableness_items,A1=7-A1)

conscientiousness_items <- mutate(conscientiousness_items,C4=7-C4)
conscientiousness_items <- mutate(conscientiousness_items,C5=7-C5)

performance_items <- mutate(performance_items,JP1=7-JP1)
performance_items <- mutate(performance_items,JP2=7-JP2)

#obtain scaled scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items) ,check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(conscientiousness_items) ,check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(performance_items), check.keys=FALSE)$scores

#combine into analytic data
analytic.data <- cbind(categorical_variables,age,agreeableness,conscientiousness,performance)

analytic.data

#save data
write_csv(analytic.data,path="final_exam_analytic.data.csv")


### make apa correlation table
library(apaTables)

apa.cor.table(analytic.data, filename="Table1.doc", table.number=1) # correlationation table

## COME BACK AND DO THIS 

#check for curvinlear 
psych::pairs.panels(as.data.frame(analytic.data),lm=TRUE)

### RUN MULTIPLE REGRESSION
## overall


###load data
my.data <- read_csv("final_exam_analytic.data.csv")
glimpse(my.data)

### get initial descriptives
apa.cor.table(my.data)

## EVERYONE

single_reg <- lm(performance~conscientiousness, data=my.data)
summary(single_reg)

apa.reg.table(single_reg)

overall_reg <- lm(performance~conscientiousness + agreeableness, data=my.data)
summary(overall_reg)

apa.reg.table(overall_reg)
summary(overall_reg)

apa.reg.table(overall_reg, filename="Table2.doc", table.number=2)

### MALE 

my.data.male <- filter(my.data, gender=="Male")
View(my.data.male)


single_regmale <- lm(performance~conscientiousness, data=my.data.male)
summary(single_regmale)

apa.reg.table(single_regmale)



male_reg <- lm(performance~conscientiousness + agreeableness, data=my.data.male)
summary(male_reg)

apa.reg.table(male_reg)
summary(male_reg)

apa.reg.table(male_reg, filename="Table3.doc", table.number=3)

### FEMALE
my.data.female <- filter(my.data, gender=="Female")
View(my.data.female)


single_regfemale <- lm(performance~conscientiousness, data=my.data.female)
summary(single_regfemale)

apa.reg.table(single_regfemale)


female_reg <- lm(performance~conscientiousness + agreeableness, data=my.data.female)
summary(female_reg)

apa.reg.table(female_reg)
summary(female_reg)

apa.reg.table(female_reg, filename="Table4.doc", table.number=4)


