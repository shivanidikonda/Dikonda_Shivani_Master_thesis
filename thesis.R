#load packages
library(tidyverse)
setwd("C:/Users/shiva/OneDrive/Documents/2024 Thesis/R reference/Thesis data for R studio")
thesisdf <- read.csv("Thesis Results.csv")
View(thesisdf)

### Q2 with Q7 ###

#Short variable name listing for coding
EB=(thesisdf)
Age=EB$Age
HIB=EB$Health.insurance.and.medical.benefits_Importance
WLB=EB$Work.life.balance_Importance
FWA=EB$Flexible.work.arrangements_Importance
PTI=EB$Paid.time.off_Importance
PD=EB$Professional.development_Importance
PBI=EB$Performance.bonuses.and.incentives_Importance
RS=EB$Retirement.savings.plans_Importance

install.packages("dplyr") 
library(dplyr)   

###  AGE  ###

# Assign numeric values to each Age category
EB |>
  mutate(Age_numeric = case_when(
    Age == "20 or younger" ~ 1,
    Age == "21-25" ~ 2,
    Age == "26-35" ~ 3,
    Age == "36-45" ~ 4
  )) ->EB1
print(EB1)

#Q7
#Assign numeric values to each HIB category
EB1 |>
  mutate(HIB_numeric = case_when(
    HIB == "5. Very Important" ~ 5,
    HIB == "4. Important" ~ 4,
    HIB == "3. Neutral" ~ 3,
    HIB == "2. Not important" ~ 2,
    HIB == "1. Not important at all" ~ 1
  )) ->EB2
print(EB2)

#Correlation spearman Age VS HIB
result = cor.test(EB2$Age_numeric,EB2$HIB_numeric, method = "spearman")

# Print the result
print(result)

#Load ggplot2 for plotting
library(ggplot2)

#Scatter plot of Age_numeric vs HIB_numeric
ggplot(EB2, aes(x = Age_numeric, y = HIB_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs HIB Importance",
       x = "Age Group (Numeric)",
       y = "HIB Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each FWA category
EB2 |>
  mutate(FWA_numeric = case_when(
    FWA == "5. Very Important" ~ 5,
    FWA == "4. Important" ~ 4,
    FWA == "3. Neutral" ~ 3,
    FWA == "2. Not important" ~ 2,
    FWA == "1. Not important at all" ~ 1
  )) ->EB3
print(EB3)

#Correlation spearman Age VS FWA
result = cor.test(EB3$Age_numeric,EB3$FWA_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs FWA_numeric
ggplot(EB3, aes(x = Age_numeric, y = FWA_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs FWA Importance",
       x = "Age Group (Numeric)",
       y = "FWA Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each PTI category
EB3 |>
  mutate(PTI_numeric = case_when(
    PTI == "5. Very Important" ~ 5,
    PTI == "4. Important" ~ 4,
    PTI == "3. Neutral" ~ 3,
    PTI == "2. Not important" ~ 2,
    PTI == "1. Not important at all" ~ 1
  )) ->EB4
print(EB4)

#Correlation spearman Age VS PTI
result = cor.test(EB4$Age_numeric,EB4$PTI_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs PTI_numeric
ggplot(EB4, aes(x = Age_numeric, y = PTI_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs PTI Importance",
       x = "Age Group (Numeric)",
       y = "PTI Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each PD category
EB4 |>
  mutate(PD_numeric = case_when(
    PD == "5. Very Important" ~ 5,
    PD == "4. Important" ~ 4,
    PD == "3. Neutral" ~ 3,
    PD == "2. Not important" ~ 2,
    PD == "1. Not important at all" ~ 1
  )) ->EB5
print(EB5)

#Correlation spearman Age VS PD
result = cor.test(EB5$Age_numeric,EB5$PD_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs PD_numeric
ggplot(EB5, aes(x = Age_numeric, y = PD_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs PD Importance",
       x = "Age Group (Numeric)",
       y = "PD Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each PBI category
EB5 |>
  mutate(PBI_numeric = case_when(
    PBI == "5. Very Important" ~ 5,
    PBI == "4. Important" ~ 4,
    PBI == "3. Neutral" ~ 3,
    PBI == "2. Not important" ~ 2,
    PBI == "1. Not important at all" ~ 1
  )) ->EB6
print(EB6)

#Correlation spearman Age VS PBI
result = cor.test(EB6$Age_numeric,EB6$PBI_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs PBI_numeric
ggplot(EB6, aes(x = Age_numeric, y = PBI_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs PBI Importance",
       x = "Age Group (Numeric)",
       y = "PBI Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each RS category
EB6 |>
  mutate(RS_numeric = case_when(
    RS == "5. Very Important" ~ 5,
    RS == "4. Important" ~ 4,
    RS == "3. Neutral" ~ 3,
    RS == "2. Not important" ~ 2,
    RS == "1. Not important at all" ~ 1
  )) ->EB7
print(EB7)

#Correlation spearman Age VS RS
result = cor.test(EB7$Age_numeric,EB7$RS_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs RS_numeric
ggplot(EB7, aes(x = Age_numeric, y = RS_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs RS Importance",
       x = "Age Group (Numeric)",
       y = "RS Importance (Numeric)") +
  theme_minimal()

#Assign numeric values to each WLB category
EB7 |>
  mutate(WLB_numeric = case_when(
    WLB == "5. Very Important" ~ 5,
    WLB == "4. Important" ~ 4,
    WLB == "3. Neutral" ~ 3,
    WLB == "2. Not important" ~ 2,
    WLB == "1. Not important at all" ~ 1
  )) ->EB8
print(EB8)

#Correlation spearman Age VS WLB
result = cor.test(EB8$Age_numeric,EB8$WLB_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs WLB_numeric
ggplot(EB8, aes(x = Age_numeric, y = WLB_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs WLB Importance",
       x = "Age Group (Numeric)",
       y = "WLB Importance (Numeric)") +
  theme_minimal()

### Q2 with Q11 ###

EB9=(EB8)
EB10=(EB9)
SB=EB10$Job.Satisfaction_Motivated.by.the.salary.and.benefits
WLBI=EB10$Job.Satisfaction_Motivated.by.Work.life.balance.initiatives
CDO=EB10$Job.Satisfaction_Motivated.by.Career.advancement.opportunities
RA=EB10$Job.Satisfaction_Motivated.by.Recognition.and.appreciation
JS=EB10$Job.Satisfaction_Motivated.by.Job.security
PWEC=EB10$Job.Satisfaction_Motivated.by.positive.work.environment.and.culture
LD=EB10$Job.Satisfaction_Motivated.by.Opportunities.for.learning.and.development

install.packages("dplyr") 
library(dplyr)   

# Assign numeric values to each SB category
EB10 |>
  mutate(SB_numeric = case_when(
    SB == "Strongly Agree" ~ 5,
    SB == "Agree" ~ 4,
    SB == "Neutral" ~ 3,
    SB == "Disagree" ~ 2,
    SB == "Strongly Disagree" ~ 1
  )) ->EB11
print(EB11)

#Correlation spearman Age VS SB
result = cor.test(EB11$Age_numeric,EB11$SB_numeric, method = "spearman")

# Print the result
print(result)

#Load ggplot2 for plotting
library(ggplot2)

#Scatter plot of Age_numeric vs SB_numeric
ggplot(EB11, aes(x = Age_numeric, y = SB_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs SB motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "SB motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each WLBI category
EB11 |>
  mutate(WLBI_numeric = case_when(
    WLBI == "Strongly Agree" ~ 5,
    WLBI == "Agree" ~ 4,
    WLBI == "Neutral" ~ 3,
    WLBI == "Disagree" ~ 2,
    WLBI == "Strongly Disagree" ~ 1
  )) ->EB12
print(EB12)

#Correlation spearman Age VS WLBI
result = cor.test(EB12$Age_numeric,EB12$WLBI_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs WLBI_numeric
ggplot(EB12, aes(x = Age_numeric, y = WLBI_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs WLBI motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "WLBI motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each CDO category
EB12 |>
  mutate(CDO_numeric = case_when(
    CDO == "Strongly Agree" ~ 5,
    CDO == "Agree" ~ 4,
    CDO == "Neutral" ~ 3,
    CDO == "Disagree" ~ 2,
    CDO == "Strongly Disagree" ~ 1
  )) ->EB13
print(EB13)

#Correlation spearman Age VS CDO
result = cor.test(EB13$Age_numeric,EB13$CDO_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs CDO_numeric
ggplot(EB13, aes(x = Age_numeric, y = CDO_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs CDO motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "CDO motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each RA category
EB13 |>
  mutate(RA_numeric = case_when(
    RA == "Strongly Agree" ~ 5,
    RA == "Agree" ~ 4,
    RA == "Neutral" ~ 3,
    RA == "Disagree" ~ 2,
    RA == "Strongly Disagree" ~ 1
  )) ->EB14
print(EB14)

#Correlation spearman Age VS RA
result = cor.test(EB14$Age_numeric,EB14$RA_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs RA_numeric
ggplot(EB14, aes(x = Age_numeric, y = RA_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs RA motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "RA motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each JS category
EB14 |>
  mutate(JS_numeric = case_when(
    JS == "Strongly Agree" ~ 5,
    JS == "Agree" ~ 4,
    JS == "Neutral" ~ 3,
    JS == "Disagree" ~ 2,
    JS == "Strongly Disagree" ~ 1
  )) ->EB15
print(EB15)

#Correlation spearman Age VS JS
result = cor.test(EB15$Age_numeric,EB15$JS_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs JS_numeric
ggplot(EB15, aes(x = Age_numeric, y = JS_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs JS motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "JS motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each PWEC category
EB15 |>
  mutate(PWEC_numeric = case_when(
    PWEC == "Strongly Agree" ~ 5,
    PWEC == "Agree" ~ 4,
    PWEC == "Neutral" ~ 3,
    PWEC == "Disagree" ~ 2,
    PWEC == "Strongly Disagree" ~ 1
  )) ->EB16
print(EB16)

#Correlation spearman Age VS PWEC
result = cor.test(EB16$Age_numeric,EB16$PWEC_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs PWEC_numeric
ggplot(EB16, aes(x = Age_numeric, y = PWEC_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs PWEC motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "PWEC motivation (Numeric)") +
  theme_minimal()

# Assign numeric values to each LD category
EB16 |>
  mutate(LD_numeric = case_when(
    LD == "Strongly Agree" ~ 5,
    LD == "Agree" ~ 4,
    LD == "Neutral" ~ 3,
    LD == "Disagree" ~ 2,
    LD == "Strongly Disagree" ~ 1
  )) ->EB17
print(EB17)

#Correlation spearman Age VS LD
result = cor.test(EB17$Age_numeric,EB17$LD_numeric, method = "spearman")

# Print the result
print(result)

#Scatter plot of Age_numeric vs LD_numeric
ggplot(EB17, aes(x = Age_numeric, y = LD_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of Age vs LD motivation in Job Satisfaction",
       x = "Age Group (Numeric)",
       y = "LD motivation (Numeric)") +
  theme_minimal()

#Importance of Specific Benefits vs. Satisfaction with Current Benefits (Q7 vs. Q9):

#Q9
# No Mutate required for question 9 as it is already numerical 

#Correlation spearman HIB VS Benefits satisfaction current
result = cor.test(EB17$HIB_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

# Print the result
print(result)

#Scatter plot of HIB_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = HIB_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of HIB vs Benefits satisfaction current",
       x = "HIB (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

install.packages("ggplot2")
library(ggplot2)

#Correlation spearman WLB VS Benefits satisfaction current
result = cor.test(EB17$WLB_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

# Print the result
print(result)

#Scatter plot of WLB_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = WLB_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of WLB vs Benefits satisfaction current",
       x = "WLB (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Correlation spearman FWA VS Benefits satisfaction current
result = cor.test(EB17$FWA_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

# Print the result
print(result)

#Scatter plot of FWA_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = FWA_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of FWA vs Benefits satisfaction current",
       x = "FWA (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Correlation spearman PTI VS Benefits satisfaction current
result = cor.test(EB17$PTI_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

# Print the result
print(result)

#Scatter plot of PTI_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = PTI_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of PTI vs Benefits satisfaction current",
       x = "PTI (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Correlation spearman PD VS Benefits satisfaction current
result = cor.test(EB17$PD_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

#Print the result
print(result)

#Scatter plot of PD_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = PD_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of PD vs Benefits satisfaction current",
       x = "PD (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Correlation spearman PBI VS Benefits satisfaction current
result = cor.test(EB17$PBI_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

#Print the result
print(result)

#Scatter plot of PBI_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = PBI_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of PBI vs Benefits satisfaction current",
       x = "PBI (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Correlation spearman RS VS Benefits satisfaction current
result = cor.test(EB17$RS_numeric,EB17$Benefits.Satisfaction_Current, method = "spearman")

#Print the result
print(result)

#Scatter plot of RS_numeric vs Benefits satisfaction current
ggplot(EB17, aes(x = RS_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of RS vs Benefits satisfaction current",
       x = "RS (Numeric)",
       y = "Benefits.Satisfaction_Current (Numeric)") +
  theme_minimal()

#Q7 vs Q11
#WLB=Work.life.balance_Importance  Vs WLBI=Job.Satisfaction_Motivated.by.Work.life.balance.initiatives

#Correlation spearman WLB VS WLBI
result = cor.test(EB17$WLB_numeric,EB17$WLBI_numeric, method = "spearman")

#Print the result
print(result)

#Scatter plot of WLB_numeric importance vs WLBI_numeric job satisfaction 
ggplot(EB17, aes(x = WLB_numeric, y = WLBI_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of WLB_numeric vs WLBI_numeric",
       x = "WLB_numeric",
       y = "WLBI_numeric") +
  theme_minimal()

#Q7 vs Q11
#PD=Professional.development_Importance Vs LD=Job.Satisfaction_Motivated.by.Opportunities.for.learning.and.development

#Correlation spearman PD VS LD
result = cor.test(EB17$PD_numeric,EB17$LD_numeric, method = "spearman")

#Print the result
print(result)

#Scatter plot of PD_numeric importance vs LD_numeric job satisfaction 
ggplot(EB17, aes(x = PD_numeric, y = LD_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of WLB_numeric vs WLBI_numeric",
       x = "PD_numeric",
       y = "LD_numeric") +
  theme_minimal()

#Q7 vs Q11
#PBI=Performance.bonuses.and.incentives_Importance Vs SB=Job.Satisfaction_Motivated.by.the.salary.and.benefits

#Correlation spearman PBI VS SB
result = cor.test(EB17$PBI_numeric,EB17$SB_numeric, method = "spearman")

#Print the result
print(result)

#Scatter plot of PBI_numeric importance vs SB_numeric job satisfaction 
ggplot(EB17, aes(x = PBI_numeric, y = SB_numeric)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of PBI_numeric vs SB_numeric",
       x = "PBI_numeric",
       y = "SB_numeric") +
  theme_minimal()

#Q12 & Q9
EBR=EB17$Importance.of.employee.benefits_Realization

#Importance.of.employee.benefits_Realization Vs Benefits Satisfaction_Current

install.packages("dplyr")
library(dplyr)

#Assign numeric values to each EBR category
EB17 |>
  mutate(EBR_numeric = case_when(
    EBR == "Before starting my career" ~ 1,
    EBR == "Early in my career" ~ 2,
    EBR == "Mid-career" ~ 3,
    EBR == "Recently" ~ 4
      )) ->EB18
print(EB18)

#Correlation spearman EBR VS Benefits Satisfaction_Current
result = cor.test(EB18$EBR_numeric,EB18$Benefits.Satisfaction_Current, method = "spearman")

#Print the result
print(result)

library(ggplot2)

#Scatter plot of EBR_numeric importance vs Benefits Satisfaction_Current_numeric
ggplot(EB18, aes(x = EBR_numeric, y = Benefits.Satisfaction_Current)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter Plot of EBR_numeric vs Benefits Satisfaction_Current_numeric",
       x = "EBR_numeric",
       y = "Benefits Satisfaction_Current_numeric") +
  theme_minimal()

#Descriptive Statistics#

###AGE###
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode of Age
mode_age <- get_mode(EB18$Age)
print(paste("Mode of Age:", mode_age))

# Calculate the median of Age_numeric
median_age <- median(EB18$Age, na.rm = TRUE)
print(paste("Median of Age:", median_age))

# Age Graph #
age_counts <- table(EB18$Age)

barplot_result <- barplot(age_counts,
                          main = "Bar Plot of Age",
                          xlab = "Age",
                          ylab = "Frequency",
                          col = "gray",
                          ylim = c(0, 100),  # Adjust y-axis limit as needed
                          xaxt = "n")  # Suppress default x-axis

axis(1, at = 1:length(age_counts), labels = names(age_counts))

text(barplot_result, age_counts, labels = age_counts, pos = 3, cex = 0.8, col = "black")

median_age <- median(EB18$Age, na.rm = TRUE)

# Add a vertical line at the median
median_pos <- which(names(age_counts) == as.character(median_age))
abline(v = median_pos, col = "blue", lwd = 2, lty = 2)

# HIB Mean, Median, Mode, SD #
mean_HIB <- mean(EB18$HIB_numeric)
print(paste("Mean", mean_HIB))

# Calculate the median of HIB_numeric
median_HIB <- median(EB18$HIB_numeric, na.rm = TRUE)
print(paste("Median of HIB:", median_HIB))

# Calculate the mode of HIB_numeric
mode_HIB <- get_mode(EB18$HIB_numeric)
print(paste("Mode of HIB:", mode_HIB))

# Calculate standard deviation of HIB_numeric
sd_HIB <- sd(EB18$HIB_numeric)
print(sd_HIB)

# FWA Mean, Median, Mode, SD #

mean_FWA <- mean(EB18$FWA_numeric)
print(paste("Mean", mean_FWA))

# Calculate the median of FWA_numeric
median_FWA <- median(EB18$FWA_numeric, na.rm = TRUE)
print(paste("Median of FWA:", median_FWA))

# Calculate the mode of FWA_numeric
mode_FWA <- get_mode(EB18$FWA_numeric)
print(paste("Mode of FWA:", mode_FWA))

# Calculate standard deviation of FWA_numeric
sd_FWA <- sd(EB18$FWA_numeric)
print(sd_FWA)

# PTI Mean, Median, Mode, SD #

mean_PTI <- mean(EB18$PTI_numeric)
print(paste("Mean", mean_PTI))

# Calculate the median of PTI_numeric
median_PTI <- median(EB18$PTI_numeric, na.rm = TRUE)
print(paste("Median of PTI:", median_PTI))

# Calculate the mode of PTI_numeric
mode_PTI <- get_mode(EB18$PTI_numeric)
print(paste("Mode of PTI:", mode_PTI))

# Calculate standard deviation of PTI_numeric
sd_PTI <- sd(EB18$PTI_numeric)
print(sd_PTI)

# PD Mean, Median, Mode, SD #

mean_PD <- mean(EB18$PD_numeric)
print(paste("Mean", mean_PD))

# Calculate the median of PD_numeric
median_PD <- median(EB18$PD_numeric, na.rm = TRUE)
print(paste("Median of PD:", median_PD))

# Calculate the mode of PD_numeric
mode_PD <- get_mode(EB18$PD_numeric)
print(paste("Mode of PD:", mode_PD))

# Calculate standard deviation of PD_numeric
sd_PD <- sd(EB18$PD_numeric)
print(sd_PD)

# PBI Mean, Median, Mode, SD #

mean_PBI <- mean(EB18$PBI_numeric)
print(paste("Mean", mean_PBI))

# Calculate the median of PBI_numeric
median_PBI <- median(EB18$PBI_numeric, na.rm = TRUE)
print(paste("Median of PBI:", median_PBI))

# Calculate the mode of PBI_numeric
mode_PBI <- get_mode(EB18$PBI_numeric)
print(paste("Mode of PBI:", mode_PBI))

# Calculate standard deviation of PBI_numeric
sd_PBI <- sd(EB18$PBI_numeric)
print(sd_PBI)

# RS Mean, Median, Mode, SD #

mean_RS <- mean(EB18$RS_numeric)
print(paste("Mean", mean_RS))

# Calculate the median of RS_numeric
median_RS <- median(EB18$RS_numeric, na.rm = TRUE)
print(paste("Median of RS:", median_RS))

# Calculate the mode of RS_numeric
mode_RS <- get_mode(EB18$RS_numeric)
print(paste("Mode of RS:", mode_RS))

# Calculate standard deviation of RS_numeric
sd_RS <- sd(EB18$RS_numeric)
print(sd_RS)

# WLB Mean, Median, Mode, SD #

mean_WLB <- mean(EB18$WLB_numeric)
print(paste("Mean", mean_WLB))

# Calculate the median of WLB_numeric
median_WLB <- median(EB18$WLB_numeric, na.rm = TRUE)
print(paste("Median of WLB:", median_WLB))

# Calculate the mode of WLB_numeric
mode_WLB <- get_mode(EB18$WLB_numeric)
print(paste("Mode of WLB:", mode_WLB))

# Calculate standard deviation of WLB_numeric
sd_WLB <- sd(EB18$WLB_numeric)
print(sd_WLB)

#Q 9 Mean, Median, Mode, SD #

mean_BSC <- mean(EB18$Benefits.Satisfaction_Current)
print(paste("Mean", mean_BSC))

# Calculate the median of Benefits.Satisfaction_Current
median_BSC <- median(EB18$Benefits.Satisfaction_Current, na.rm = TRUE)
print(paste("Median of BSC:", median_BSC))

# Calculate the mode of Benefits.Satisfaction_Current
mode_BSC <- get_mode(EB18$Benefits.Satisfaction_Current)
print(paste("Mode of BSC:", mode_BSC))

# Calculate standard deviation of Benefits.Satisfaction_Current
sd_BSC <- sd(EB18$Benefits.Satisfaction_Current)
print(sd_BSC)

# Box plot Benefits.Satisfaction_Current
boxplot(EB18$Benefits.Satisfaction_Current,
        main = "Box Plot of Benefits Satisfaction Current",
        ylab = "BSC values",
        col = "gray",
        border = "black"
)  

# Add horizontal lines for mean, mode, and standard deviation (optional)

# Mean line
abline(h = mean_BSC, col = "blue", lwd = 2, lty = 2)

# Median line (already shown in the boxplot by default, but adding explicitly for emphasis)
abline(h = median_BSC, col = "purple", lwd = 2, lty = 2)

# Add legend to indicate the lines
legend("bottomright", legend = c("Mean", "Median"),
       col = c("blue", "purple"), lwd = 2, lty = 2)

#Q 10 Benefits packages updates importance Mean, Median, Mode, SD #

mean_BPUI <- mean(EB18$Benefit.Package.Updates_Importance)
print(paste("Mean", mean_BPUI))

# Calculate the median of Benefits packages updates importance
median_BPUI <- median(EB18$Benefit.Package.Updates_Importance, na.rm = TRUE)
print(paste("Median of BPUI:", median_BPUI))

# Calculate the mode of Benefits packages updates importance
mode_BPUI <- get_mode(EB18$Benefit.Package.Updates_Importance)
print(paste("Mode of BPUI:", mode_BPUI))

# Calculate standard deviation of Benefits packages updates importance
sd_BPUI <- sd(EB18$Benefit.Package.Updates_Importance)
print(sd_BPUI)

# Box plot Benefits packages updates importance
boxplot(EB18$Benefit.Package.Updates_Importance,
        main = "Box Plot of Benefits Packages Updates Importance",
        ylab = "BPUI values",
        col = "gray",
        border = "black"
)  

# Add horizontal lines for mean, mode, and standard deviation (optional)

# Mean line
abline(h = mean_BPUI, col = "blue", lwd = 2, lty = 2)

# Median line (already shown in the boxplot by default, but adding explicitly for emphasis)
abline(h = median_BPUI, col = "purple", lwd = 2, lty = 2)

# Add legend to indicate the lines
legend("bottomright", legend = c("Mean", "Median"),
       col = c("blue", "purple"), lwd = 2, lty = 2)

#Q11
# SB Mean, Median, Mode, SD #

mean_SB <- mean(EB18$SB_numeric)
print(paste("Mean", mean_SB))

# Calculate the median of SB_numeric
median_SB <- median(EB18$SB_numeric, na.rm = TRUE)
print(paste("Median of SB:", median_SB))

# Calculate the mode of SB_numeric
mode_SB <- get_mode(EB18$SB_numeric)
print(paste("Mode of SB:", mode_SB))

# Calculate standard deviation of SB_numeric
sd_SB <- sd(EB18$SB_numeric)
print(sd_SB)

# WLBI Mean, Median, Mode, SD #

mean_WLBI <- mean(EB18$WLBI_numeric)
print(paste("Mean", mean_WLBI))

# Calculate the median of WLBI_numeric
median_WLBI <- median(EB18$WLBI_numeric, na.rm = TRUE)
print(paste("Median of WLBI:", median_WLBI))

# Calculate the mode of WLBI_numeric
mode_WLBI <- get_mode(EB18$WLBI_numeric)
print(paste("Mode of WLBI:", mode_WLBI))

# Calculate standard deviation of WLBI_numeric
sd_WLBI <- sd(EB18$WLBI_numeric)
print(sd_WLBI)

# CDO Mean, Median, Mode, SD #

mean_CDO <- mean(EB18$CDO_numeric)
print(paste("Mean", mean_CDO))

# Calculate the median of CDO_numeric
median_CDO <- median(EB18$CDO_numeric, na.rm = TRUE)
print(paste("Median of CDO:", median_CDO))

# Calculate the mode of CDO_numeric
mode_CDO <- get_mode(EB18$CDO_numeric)
print(paste("Mode of CDO:", mode_CDO))

# Calculate standard deviation of CDO_numeric
sd_CDO <- sd(EB18$CDO_numeric)
print(sd_CDO)

# RA Mean, Median, Mode, SD #

mean_RA <- mean(EB18$RA_numeric)
print(paste("Mean", mean_RA))

# Calculate the median of RA_numeric
median_RA <- median(EB18$RA_numeric, na.rm = TRUE)
print(paste("Median of RA:", median_RA))

# Calculate the mode of RA_numeric
mode_RA <- get_mode(EB18$RA_numeric)
print(paste("Mode of RA:", mode_WLBI))

# Calculate standard deviation of RA_numeric
sd_RA <- sd(EB18$RA_numeric)
print(sd_RA)

# JS Mean, Median, Mode, SD #

mean_JS <- mean(EB18$JS_numeric)
print(paste("Mean", mean_JS))

# Calculate the median of JS_numeric
median_JS <- median(EB18$JS_numeric, na.rm = TRUE)
print(paste("Median of JS:", median_JS))

# Calculate the mode of JS_numeric
mode_JS <- get_mode(EB18$JS_numeric)
print(paste("Mode of JS:", mode_JS))

# Calculate standard deviation of JS_numeric
sd_JS <- sd(EB18$JS_numeric)
print(sd_JS)

# PWEC Mean, Median, Mode, SD #

mean_PWEC <- mean(EB18$PWEC_numeric)
print(paste("Mean", mean_PWEC))

# Calculate the median of PWEC_numeric
median_PWEC <- median(EB18$PWEC_numeric, na.rm = TRUE)
print(paste("Median of PWEC:", median_PWEC))

# Calculate the mode of PWEC_numeric
mode_PWEC <- get_mode(EB18$PWEC_numeric)
print(paste("Mode of PWEC:", mode_PWEC))

# Calculate standard deviation of PWEC_numeric
sd_PWEC <- sd(EB18$PWEC_numeric)
print(sd_PWEC)

# LD Mean, Median, Mode, SD #

mean_LD <- mean(EB18$LD_numeric)
print(paste("Mean", mean_LD))

# Calculate the median of LD_numeric
median_LD <- median(EB18$LD_numeric, na.rm = TRUE)
print(paste("Median of LD:", median_LD))

# Calculate the mode of PWEC_numeric
mode_LD <- get_mode(EB18$LD_numeric)
print(paste("Mode of LD:", mode_LD))

# Calculate standard deviation of LD_numeric
sd_LD <- sd(EB18$LD_numeric)
print(sd_LD)

#Q7 mix box plot
boxplot(list(HIB = EB18$HIB_numeric, WLB = EB18$WLB_numeric, FWA = EB18$FWA_numeric, PTI = EB18$PTI_numeric, PD = EB18$PD_numeric, PBI = EB18$PBI_numeric, RS = EB18$RS_numeric),
        main = "Box Plot of Benefits Importance Measures", 
        col = c("gray", "gray", "gray", "gray", "gray", "gray"),
        ylab = "Values", xlab = "Benefits")

#Q7 mix line graph
means <- c(mean(EB18$HIB_numeric), 
           mean(EB18$WLB_numeric), 
           mean(EB18$FWA_numeric), 
           mean(EB18$PTI_numeric), 
           mean(EB18$PD_numeric), 
           mean(EB18$PBI_numeric), 
           mean(EB18$RS_numeric))

benefits <- c("HIB", "WLB", "FWA", "PTI", "PD", "PBI", "RS")

data <- data.frame(Benefit = benefits, Mean = means)

library(ggplot2)

ggplot(data, aes(x = Benefit, y = Mean, group = 1)) +
  geom_line(color = "gray", size = 1) +
  geom_point(size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Mean Importance of Benefits", y = "Mean Value", x = "Benefits")+
  theme(plot.title = element_text(hjust = 0.5))

#Q11 mix box plot
boxplot(list(SB = EB18$SB_numeric, WLBI = EB18$WLBI_numeric, CDO = EB18$CDO_numeric, RA = EB18$RA_numeric, JS = EB18$JS_numeric, PWEC = EB18$PWEC_numeric, LD = EB18$LD_numeric),
        main = "Box Plot of Job Satisfaction Level based on Motivational Factors", 
        col = c("gray", "gray", "gray", "gray", "gray", "gray"),
        ylab = "Values", xlab = "Benefits")

#Q11 mix line graph
means <- c(mean(EB18$SB_numeric), 
           mean(EB18$WLBI_numeric), 
           mean(EB18$CDO_numeric), 
           mean(EB18$RA_numeric), 
           mean(EB18$JS_numeric), 
           mean(EB18$PWEC_numeric), 
           mean(EB18$LD_numeric))

motivational_factors <- c("SB", "WLBI", "CDO", "RA", "JS", "PWEC", "LD")

data1 <- data.frame(Motivators = motivational_factors, Mean = means)

library(ggplot2)

ggplot(data1, aes(x = Motivators, y = Mean, group = 1)) +
  geom_line(color = "gray", size = 1) +
  geom_point(size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Mean Job Satisfaction Level based on Motivational Factors", y = "Mean Value", x = "Motivational Factors")+
  theme(plot.title = element_text(hjust = 0.5))
