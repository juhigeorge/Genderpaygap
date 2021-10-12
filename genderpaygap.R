setwd("C:/Users/juhig/OneDrive/Desktop/BU/course content s1/stati/group_project")
library(readxl)
library(ggplot2)
library(tidyverse)
library(stargazer)
library(fastDummies)
library(foreign)
##main theme setting##
theme_main <- theme(panel.background = element_rect(fill = "white", color = "#455D5A"), 
                    panel.grid = element_line(color = "black", linetype = "dotted", size = 0.2),
                    axis.text = element_text(color = "black", face = "bold"),
                    axis.title = element_text(face = "bold"),
                    legend.title = element_text(face = "bold"),
                    axis.ticks = element_line(color = "#455D5A"))
colorset <- c("#E57577", "#64864A", "#6C5B7B", "#355C7D")
getwd()
data <- read_excel("C:/Users/juhig/OneDrive/Desktop/BU/course content 
s1/stati/group_project/Glassdoor_Gender_Pay_Gap.xlsx")
##Data preparation##
data$Gender[data$Gender == "Female"] <- "female"
data$Gender[data$Gender == "Male"] <- "male"
data <- data %>% mutate_if(is.character, as.factor)
data<-data%>%mutate(TotalPay=BasePay+Bonus)
data<-data%>%mutate(logTotalPay=log(TotalPay,base = exp(1)))
summary(data)
# Creating a dummy variable for gender (male = 1, female = 0).
data$Genderdummy <- ifelse(data$Gender == "male", 1, 0) # male = 1, female = 0
##Creating a summary table for the data;
##creating dummy variable for dept, Education,jobtitle,seniority and perfeval
z<-dummy_cols(data,select_columns =c('Dept','Education','PerfEval','JobTitle'))
##To save file to stata for analysis in stata##
write.dta(z, "z.dta") 
## On average, men are paid 8.10% more than women. 
## Which means for every Â$1 a man earns, a woman will earn 91.9cents.

data %>% group_by(Gender) %>% summarise(number_of_staff = n(), av_salary = 
                                          mean(TotalPay))
salary_gap <- data %>% group_by(Gender) %>% summarise(mean(TotalPay)) %>% pull()
diff(salary_gap)/salary_gap[2]*100
## On median, men are paid 8.11% more than women. 
salary_gap_med <- data %>% group_by(Gender) %>% summarise(median(TotalPay)) %>% 
  pull()
diff(salary_gap_med)/salary_gap_med[2]*100
##checking for outliers
##The 4th quartile of male TotalPay could fit female outlier values too.
##2nd,3rd quartiles is between 80.8k and 112.6 (female),between 87.7 and 121.6k(female)
##payGap exists
boxplot(TotalPay~Gender,data=data, main="TotalPay",
        xlab="Gender", ylab="Totalpay")
# With no control variables. ("unadjusted" pay gap.)
model1 <- lm(logTotalPay ~ Gender, data = data)
summary(model1)
##Adding Age as control variable
model2<- lm(logTotalPay ~ Gender + Age, data = data)
summary(model2)
##Adding Age,PerfEval as control variable
model3<- lm(logTotalPay ~ Gender + Age+ PerfEval, data = data)
summary(model3)
##Adding Age,PerfEval,Education as control variable
model4<- lm(logTotalPay ~ Gender + Age+ PerfEval+Education, data = data)
summary(model4)
##Adding Age,PerfEval,Education,Seniority as control variable
model5<- lm(logTotalPay ~ Gender + Age+ PerfEval+Education+Seniority, data = data)

summary(model5)
##Adding Age,PerfEval,Education,Seniority,Dept as control variable
model6<- lm(logTotalPay ~ Gender + Age+ PerfEval+Education+Seniority+Dept, data = data)
summary(model6)
##Adding Age,PerfEval,Education,Seniority,Dept,JobTitle as control variable
model7<- lm(logTotalPay ~ Gender + Age+ PerfEval+Education+Seniority+Dept+JobTitle, 
            data = data)
summary(model7)
##Table of these regression analysis
stargazer(model1,model2,model3,model4,model5,model6,model7, type = "html", report = 
            'vc*p', out = "C:/Users/juhig/OneDrive/Desktop/BU/course content 
s1/stati/group_project/reg.htm", star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE)
##heterogenous analysis
#Department
Dmodel1<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education, data 
             =subset(z,Dept_Administration==1))
summary(Dmodel1)
Dmodel2<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education, data 
             =subset(z,Dept_Sales==1))
summary(Dmodel2)
Dmodel3<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education, data 
             =subset(z,Dept_Operations==1))
summary(Dmodel3)
Dmodel4<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education, data 
             =subset(z,Dept_Engineering==1))
summary(Dmodel4)
Dmodel5<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education, data 
             =subset(z,Dept_Management==1))
summary(Dmodel5)
##Table of these regressions
stargazer(Dmodel1,Dmodel2,Dmodel3,Dmodel4,Dmodel5, type = "html", report = 'vc*p', out 
          = "C:/Users/juhig/OneDrive/Desktop/BU/course content 
s1/stati/group_project/Department.htm", star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE)
##Job title
Jmodel1<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Data Scientist`==1))
summary(Jmodel1)
Jmodel2<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,JobTitle_Driver==1))
summary(Jmodel2)
Jmodel3<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Graphic Designer`==1))
summary(Jmodel3)
Jmodel4<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,JobTitle_IT==1))
summary(Jmodel4)
Jmodel5<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Marketing Associate`=1))
summary(Jmodel5)
Jmodel6<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Sales Associate`==1))
summary(Jmodel6)
Jmodel7<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Software Engineer`==1))
summary(Jmodel7)
Jmodel8<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Warehouse Associate`==1))
summary(Jmodel8)
Jmodel9<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
             =subset(z,`JobTitle_Data Scientist`==1))

summary(Jmodel9)
Jmodel10<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Education+Dept, data 
              =subset(z,`JobTitle_Data Scientist`==1))
summary(Jmodel10)
##Table of these regressions
stargazer(Jmodel1,Jmodel2,Jmodel3,Jmodel4,Jmodel5,Jmodel6,Jmodel7,Jmodel8,Jmodel9,J
          model10, type = "html", report = 'vc*p', out = "C:/Users/juhig/OneDrive/Desktop/BU/course 
content s1/stati/group_project/jobtitle.htm", star.cutoffs = c(0.05, 0.01, 0.001), align = 
            TRUE)
##Education
Emodel1<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Dept, data 
             =subset(z,Education_Masters==1))
summary(Emodel1)
Emodel2<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Dept, data 
             =subset(z,Education_PhD==1))
summary(Emodel2)
Emodel3<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Dept, data 
             =subset(z,`Education_High School`==1))
summary(Emodel3)
Emodel4<- lm(logTotalPay ~ Gender + Age+ PerfEval+Seniority+Dept, data 
             =subset(z,Education_College==1))
summary(Emodel4)
##Table of these regressions
stargazer(Emodel1,Emodel2,Emodel3,Emodel4, type = "html", report = 'vc*p', out = 
            "C:/Users/juhig/OneDrive/Desktop/BU/course content 
s1/stati/group_project/education.htm", star.cutoffs = c(0.05, 0.01, 0.001), align = TRUE)