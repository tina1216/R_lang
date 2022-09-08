# APU2F2202
# Takako Sekiya
# ------------------------------------------------------------------------------------
# PROGRAMMING FOR DATA ANALYSIS - ASSIGNMENT
# Module Code : CT127-3-2-PFDA
# Module Name : PROGRAMMING FOR DATA ANALYSIS
# Title       : College study and grades
# Intake      : APU2F2202
# Student Name: Takako Sekiya
# Student ID  : TP058659
# Lecturer    : MINNU HELEN JOSEPH
# ------------------------------------------------------------------------------------

install.package("validate")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("ggridges")
install.packages('GGally')
install.packages("pheatmap")

# ================================== Load libraries ==================================

library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(validate)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(GGally)
library(pheatmap)

# =================================== Import Data ====================================
# Get current working directory
getwd()

# clean data set
pure_data <- read.csv("C:\\Users\\takak\\Documents\\Develop\\APU\\PFDA\\student.csv")

# =============================== Data Pre-processing ================================

# view data
view(pure_data)

# check variables in the data set
names(pure_data)

# understanding the structure - data type etc.
str(pure_data)

head(pure_data, 5)
tail(pure_data, 5)

# check to see if there are missing data
sum(is.na(pure_data))

# Validate the data
validate_rules = validator("Index Unique" = is_unique(pure_data$index),
                           "School" = ! is.na(pure_data$school),
                           "Sex" = pure_data$sex %in% c("F", "M"),
                           "Age" = pure_data$age >= 0 & pure_data$age <= 120,
                           "Family Size" = pure_data$famsize %in% c("LE3", "GT3"),
                           "Parent Cohabitation" = pure_data$Pstatus %in% c("T", "A"),
                           "Mom Education" = in_range(pure_data$Medu, min = 0, max =  4),
                           "Dad Education" = in_range(pure_data$Fedu, min = 0 , max =  4),
                           "Mom Job" = pure_data$Mjob %in% c("teacher", "health", "services", "at_home", "other"),
                           "Dad Job" = pure_data$Fjob %in% c("teacher", "health", "services", "at_home", "other"),
                           "School Reason" = pure_data$reason %in% c("home", "reputation", "course", "other"),
                           "Guardian" = pure_data$guardian %in% c("mother", "father", "other"),
                           "Traveltime" = in_range(pure_data$traveltime, min = 1, max =  5),
                           "Weekly Studytime" = in_range(pure_data$studytime, min = 0, max =  10),
                           "Class Failure" = in_range(pure_data$failures, min = 0, max =  5),
                           "Extra edu support" = pure_data$schoolsup %in% c("yes", "no"),
                           "Family edu support" = pure_data$famsup %in% c("yes", "no"),
                           "Extra paid classes" = pure_data$paid %in% c("yes", "no"),
                           "Activities" = pure_data$activities %in% c("yes", "no"),
                           "Nursery Attended" = pure_data$nursery %in% c("yes", "no"),
                           "Want higher edu" = pure_data$higher %in% c("yes", "no"),
                           "Internet at Home" =  pure_data$internet %in% c("yes", "no"),
                           "with romantic relation" = pure_data$romantic %in% c("yes", "no"),
                           "Quality of fam relation" = in_range(pure_data$famrel, min = 1, max =  5),
                           "Free after school" = in_range(pure_data$freetime, min = 0, max =  5),
                           "Going-out" = in_range(pure_data$goout, min = 1, max =  5),
                           "Workday Alcohol" = in_range(pure_data$Dalc, min = 1, max =  5),
                           "Weekend Alcohol" = in_range(pure_data$Walc, min = 1, max =  5),
                           "Health Status" = in_range(pure_data$health, min = 1, max =  5),
                           "School Absences" = in_range(pure_data$absences, min = 0, max =  93),
                           "1st period grade" =  in_range(pure_data$G1, min = 0, max =  20),
                           "2nd period grade" =  in_range(pure_data$G2, min = 0, max =  20),
                           "3rd period grade" =  in_range(pure_data$G3, min = 0, max =  20)
                           )

output = confront(pure_data, validate_rules, key = "index")
plot(output)

# extract only entities I use in this assignment
data = pure_data %>% 
        select(paid, G1, G2, G3, studytime, internet, Mjob, Fjob, freetime,
               absences, address, traveltime, Dalc, Walc, school, health, failures, goout,
               Medu, Fedu, famsup, famrel, famsize, Pstatus, romantic)

summary(data)

# =============================== Data Exploration ================================

# Q1. what factors affects students' grade? (G1-3) -- money
# Q2. What are the reasons for the higher number of failures ? -- time management
# Q3. Does family background affect students educational achievements? -- family

ggplot(data, aes(x = freetime, y = G3, col = romantic, shape = romantic)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "G3 vs freetime by romantic ") +
  facet_grid( ~ romantic)

ggplot(data, aes(x = famrel, y = G3, col = famsize, shape = famsize)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Family relationship vs family size ") +
  facet_grid( ~ famsize)


# =================================== Question 1 =====================================
# Q1. what factors affect students' grade ?
# ====================================================================================

# ----------------------------------- Analysis 1 -------------------------------------
# relationship between internet access & study time

#data frame
internet_df = data %>% select(internet, studytime, G1, G2, G3 )

# internet access vs study time
have_internet = internet_df[internet_df$internet == "yes", ]

no_internet = internet_df[internet_df$internet == "no", ]

boxplot(studytime ~ internet, data = internet_df,
        xlab = "Internet", ylab = "Study Time",
        main = "Internet Availability at Home VS Study Time",
        notch = TRUE, varwidth = TRUE, col = c("#00C897", "#FFD365"))

# internet access vs G3
factor_g3 = as.factor(data$G3)
factor_g3 = ordered(factor_g3)

ggplot(data, mapping = aes(x = factor_g3, fill = internet)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Internet Access vs G3", fill = "internet") +
  xlab("G3") +
  ylab("Percentage(%)")



# ----------------------------------- Analysis 2 -------------------------------------
# relationship between parents' job and internet availability

ggplot(data, mapping = aes(x = fct_rev(fct_infreq(Mjob)), fill = internet)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Mother's job vs Internet Availability at Home", fill = "internet") +
  xlab("Job Type") +
  ylab("Percentage(%)")

ggplot(data, mapping = aes(x = fct_rev(fct_infreq(Fjob)), fill = internet)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Father's job vs Internet Availability at Home", fill = "internet") +
  xlab("Job Type") +
  ylab("Percentage(%)")



# ----------------------------------- Analysis 3 -------------------------------------
# relationship between study time and grade
time_g = data %>% select(studytime, G1, G2, G3)

pairs(time_g, lower.panel = NULL, pch = 19)

# ----------------------------------- Analysis 4 -------------------------------------
# relationship between extra paid class & grade

data %>% select(paid,G1) %>% group_by(paid) %>% 
  ggplot(aes(G1, fill = paid)) +
  geom_bar() +
  facet_grid(~paid) +
  labs(title = "Extra paid class vs G1", fill = "Extra paid class")

data %>% select(paid,G2) %>% group_by(paid) %>% 
  ggplot(aes(G2, fill = paid)) +
  geom_bar() +
  facet_grid(~paid) +
  labs(title = "Extra paid class vs G2", fill = "Extra paid class")

data %>% select(paid, G3) %>% group_by(paid) %>% 
  ggplot(aes(G3, fill = paid)) +
  geom_bar() +
  facet_grid(~paid) +
  labs(title = "Extra paid class vs G3", fill = "Extra paid class")


# ----------------------------------- Analysis 5 -------------------------------------
# parents' job & extra paid class

# Graph - Stacked Barplot
ggplot(data, aes(fill = Mjob, y = ncol(data), x = paid)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Mother's job vs Extra paid class", fill = "Job type")

ggplot(data, aes(fill = Fjob, y = ncol(data), x = paid)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Father's job vs Extra paid class", fill = "Job type")



# =================================== Question 2 =====================================
# Q2. What are the reasons for the higher number of failures ?
# ====================================================================================

# ----------------------------------- Analysis 1 -------------------------------------
# relationship between absences vs grades and failures

# absences vs G1
ggplot(data, aes(x = absences, y = G1)) + 
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(title = "Absences vs G1")

# absences vs G3
ggplot(data, aes(x = absences, y = G2)) + 
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(title = "Absences vs G2")

# absences vs G3
ggplot(data, aes(x = absences, y = G3)) + 
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)  +
  labs(title = "Absences vs G3")

# absences vs failures
ggplot(data, aes(x=failures, y=G3)) + 
  geom_point()+
  labs(title = "Absences vs Failures")


# ----------------------------------- Analysis 2 -------------------------------------
# Do workday and weekend alcohol intake lead students to fail class?
# (workday & weekend alcohol intake and failures)

# Workday Alcohol intake vs Grades
par(mfrow = c(1,3))    
plot(data$Dalc, data$G1, col="#4D96FF", pch = 19)
plot(data$Dalc, data$G2, col="#6BCB77", pch = 19)
plot(data$Dalc, data$G3, col="#FF6B6B", pch = 19)

# Weekend Alcohol intake vs Grades
par(mfrow = c(1,3))    
plot(data$Walc, data$G1, col="#4D96FF", pch = 19)
plot(data$Walc, data$G2, col="#6BCB77", pch = 19)
plot(data$Walc, data$G3, col="#FF6B6B", pch = 19)

# Dalc vs failures
factor_dalc = as.factor(data$Dalc)

ggplot(data, aes(failures, fill = factor_dalc)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Workday alcohol consumption vs Failures", fill = "Alcohol Intake (1=low)") +
  xlab("Number of Past Class Failures") +
  ylab("Percentage(%)")

# Walc vs failures
factor_walc = as.factor(data$Walc)

ggplot(data, aes(failures, fill = factor_walc)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Weekend alcohol consumption vs failures", fill = "Alcohol Intake (1=low)") +
  xlab("Number of Past Class Failures") +
  ylab("Percentage(%)")


# ----------------------------------- Analysis 3 -------------------------------------
#  is there difference of number of failures among the two schools?

# school vs absences
num_absences = as.factor(data$absences)

ggplot(data, mapping = aes(x = (num_absences), fill = school)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "School vs Absences", fill = "School") +
  xlab("Number of Absences") +
  ylab("Percentage(%)") 


# ----------------------------------- Analysis 4 -------------------------------------
# relationship between the number of failures and travel time

# address and travel time
data %>%
  ggplot( aes(x = traveltime, fill = address)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "Address vs Travel time", fill = "Address") 

# G3 and address
data %>%
  ggplot( aes(x = G3, fill = address)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(title = "G3 vs Address", fill = "Address")

# G3 vs travel time
traveltime_fct = as.factor(data$traveltime)

ggplot(data, aes(x = G3, y=traveltime_fct, fill = traveltime_fct)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "bottom") +
  labs(title = "G3 vs Travel time", fill = "Travel time (h)")


# ----------------------------------- Analysis 5 -------------------------------------
# relationship between failures and going out

fail_goout = data %>% select(failures, goout) %>% arrange(failures)

ggplot(data, aes(x = goout, group = failures, fill = failures)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  labs(title = "Failures vs Going out", fill = "Num of failures")


# =================================== Question 3 =====================================
# Q3. Does family background affect student educational achievements?
# ====================================================================================

# ----------------------------------- Analysis 1 -------------------------------------
# relationship between parents educational level and G3

# how many student in each Mjob
ggplot(data, aes(x = Mjob)) + 
  geom_bar(fill = "#FF6B6B") +
  labs(title = "Number of students based on Mother's job type", fill = "Job type")

# how many student in each Fjob
ggplot(data, aes(x = Fjob)) + 
  geom_bar(fill = "#4D96FF") +
  labs(title = "Number of students based on father's job type", fill = "Job type")


ggplot(data, aes(x = Mjob, y = G3)) +
  geom_point()+
  geom_smooth(method = "lm",se = F) +
  labs(title = "G3 vs mother's job type")

ggplot(data, aes(x = Fjob, y = G3)) +
  geom_point()+
  geom_smooth(method = "lm",se = F) +
  labs(title = "G3 vs father's job type")


# Father's educational level
father_edu = ggplot(data, aes(x = Fedu)) + 
  geom_bar(fill='#4D96FF', color="#282828") +
  labs(title = "Father's educational level", x = "Educational Level")
father_edu

# Mother's educational level
mother_edu = ggplot(data, aes(x = Medu)) + 
  geom_bar(fill='#FF6B6B', color="#282828") +
  labs(title = "Mother's educational level", x = "Educational Level")
mother_edu

ggplot(data, aes(x = Fedu, y = Medu)) +
  geom_point(aes(color = G3)) +
  stat_smooth(method = "lm",
              col = "#6BCB77",
              se = FALSE,
              size = 1) +
  labs(title = "Parents' educational level vs G1",
       x = "Father's educational level",
       y = "Mother's educational level")


# ----------------------------------- Analysis 2 -------------------------------------
# relationship between family edu and G3

#scatterplot
par(mfrow = c(1,2))
plot(data$Medu, data$G3, col='#FF6B6B', pch = 19)
plot(data$Fedu, data$G3, col='#4D96FF', pch = 19)



# ----------------------------------- Analysis 3 -------------------------------------
# relationship between family educational support (famsup) & family relationship (famrel)

# how many student with/ without family educational suuport
table(data$famsup)

ggplot(data, aes(x = famrel, y = G3, group = famrel)) + 
  geom_boxplot(color="#282828", fill="#6BCB77") +
  facet_wrap(~famsup) +
  labs(title = "Family relationship vs G3", subtitle = "With/ without Family support",
       x = "Family relationship (1=bad)")



# ----------------------------------- Analysis 4-------------------------------------
# relationship between Pstatus and Grade (G1-G3)

# table of parents status
table(data$Pstatus)

ggplot(data, aes(x = Pstatus, y = G1, fill = school)) + 
  geom_boxplot() +
  labs(title = "G1 vs Parents status", x = "Parents Status")

ggplot(data, aes(x = Pstatus, y = G2, fill = school)) + 
  geom_boxplot() +
  labs(title = "G2 vs Parents status", x = "Parents Status")

ggplot(data, aes(x = Pstatus, y = G3, fill = school)) + 
  geom_boxplot() +
  labs(title = "G3 vs Parents status", x = "Parents Status")



# ====================================================================================
#  CONCLUSION
# ====================================================================================

data_features = Filter(is.numeric, data)

pheatmap(cor(data_features))
