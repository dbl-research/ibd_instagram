
##################################################################

# preamble ####

## load packages ####

library(ggplot2) # data visualization

library(dplyr) # data management
library(tidyr) # data management

library(car) # VIF
library(pwr) # power analysis


## load data ####
insta <- read.csv("Y:/our/Path/To/data.csv")


# Power calculation ####


# Power analysis based on Park et al. (2020) doi:10.1371/journal.pone.0233654
d <- (14.7 - 11.7) / sqrt((4.7^2 + 4.7^2)/2) # formula of effect size: d = (mean_a - mean_b) / (sqrt((sd_a^2 + sd_b^2)/2)
pwr.t.test(d = d, sig.level = 0.05, power = 0.7)




# analysis ####

## Descriptive statistics ####

### Participants characteristics ####
 

#### total number of participants included ####
insta$ID_patient %>% unique() %>% length()

# group size pre- and post-interventional // control and intervention 
table(insta$group, insta$time)


t.test(filter(insta, group == "control" & completed_study == 1 & time_dummy == 0)$age,
       filter(insta, group == "intervention"  & completed_study == 1 & time_dummy == 0)$age)

t.test(filter(insta, group == "control" & completed_study == 1 & time_dummy == 0)$age_diagnosis,
       filter(insta, group == "intervention" & completed_study == 1 & time_dummy == 0)$age_diagnosis)

# Sex
chisq.test(matrix(c(23,24,2,0), nrow = 2))


# Diagnosis
insta %>% filter(time_dummy == 0 & completed_study == 1) %>%
  group_by(group_dummy, diagnosis) %>%
  summarise(n = n())

chisq.test(matrix(c(14,16,11,8), nrow = 2))


### Primary outcome ####

# calculate and save t-test
t0_ci <- t.test(filter(insta, group == "control" & completed_study == 1 & time_dummy == 0)$know,
       filter(insta, group == "intervention" & completed_study == 1 & time_dummy == 0)$know)

t1_ci <- t.test(filter(insta, group == "control" & completed_study == 1 & time_dummy == 1)$know,
       filter(insta, group == "intervention" & completed_study == 1 & time_dummy == 1)$know)

# print t-test
t0_ci
t1_ci

# mean values
mean(filter(insta, completed_study == 1, time_dummy == 0)$know)
mean(filter(insta, completed_study == 1, time_dummy == 1)$know)


# Power
d <- (21.67 - 18.08) / sqrt((1.55^2 + 3.60^2)/2)
pwr.t.test(d = d, sig.level = 0.05, n = 24)



### Primary outcome, ITT ####

# calculate t-test WITH participants that did not complete the study
t.test(filter(insta, group == "control" & time_dummy == 0)$know,
       filter(insta, group == "intervention" & time_dummy == 0)$know)

# calculate mean and sd WITH participants that did not complete the study
mean(filter(insta, time_dummy == 0 & group == "control")$know)
sd(filter(insta, time_dummy == 0 & group == "control")$know)

mean(filter(insta, time_dummy == 0 & group == "intervention")$know)
sd(filter(insta, time_dummy == 0 & group == "intervention")$know)




### Boxplot of knowledge ####


# save t-test of Intervention group pre-intervention VS post-intervention
t01_i <- t.test(filter(insta, group == "intervention" & completed_study == 1 & time_dummy == 0)$know,
                filter(insta, group == "intervention" & completed_study == 1 & time_dummy == 1)$know)

# create data frames for brackets for p-values
df1 <- data.frame(a = c(1,1,2,2), b = c(25,26,26,25))
df2 <- data.frame(a = c(3,3,4,4), b = c(25,26,26,25))
df3 <- data.frame(a = c(2,2,4,4), b = c(27,28,28,27))


#pdf("boxplot.pdf", width = 8, height = 5.0)
#png("boxplot.png", width = 8, height = 5.0, units = "in", res = 100)

ggplot() +
  geom_boxplot(data = insta, aes(x = time_group, y = know, fill = group)) +
  geom_jitter(data = insta, aes(x = time_group, y = know, fill = group), width = 0.1, size = 1) +
  geom_line(data = df1, aes(x = a, y = b)) +
  annotate("text", x = 1.5, y = 26.5, label = paste0("italic(P)==",round(t0_ci$p.value,4)), parse = T) +
  geom_line(data = df2, aes(x = a, y = b)) +
  annotate("text", x = 3.5, y = 26.5, label = paste0("italic(P)==",round(t1_ci$p.value,4)), parse = T) +
  geom_line(data = df3, aes(x = a, y = b)) +
  annotate("text", x = 3.0, y = 28.5, label = paste0("italic(P)==",round(t01_i$p.value,4)), parse = T) +
  labs(x = "Time and Group", y = "IBD-KNOW score") +
  scale_fill_manual(name = "Group", labels = c("Control", "Treatment"),
                    values = c("light grey", "gray52")) +
  scale_x_discrete(limits = c("Preinterventioncontrol", "Preinterventionintervention", "Postinterventioncontrol", "Postinterventionintervention"),
                   labels = c("Preintervention\nControl", "Preintervention\nTreatment", "Postintervention\nControl", "Postintervention\nTreatment")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box.background = element_rect(color = "black"), 
        legend.background = element_blank())

#dev.off()




## Difference in difference regression ####


### Primary models ####

# estimate modedls
model0 <- lm(know ~ female + diagnosis + age + duration, data = insta)
model1 <- lm(know ~ female + diagnosis + age + duration + time_dummy, data = insta)
model2 <- lm(know ~ female + diagnosis + age + duration + group_dummy, data = insta)
model3 <- lm(know ~ female + diagnosis + age + duration + time_dummy*group_dummy, data = insta)

# for p-vals and F
summary(model0)
summary(model3)

# check for multicollinearity
vif(model0)
vif(model1)
vif(model2)
vif(model3)





### Visualization DID treatment effect ####

# extract coefficients from models
c0 <-  model3$coefficients[[1]]
c1 <-  model3$coefficients[[1]] + model3$coefficients[[6]]
t0 <-  model3$coefficients[[1]] + model3$coefficients[[7]]
t1 <-  model3$coefficients[[1]] + model3$coefficients[[6]] + model3$coefficients[[7]] + model3$coefficients[[8]]
cf0 <- model3$coefficients[[1]] + model3$coefficients[[7]]
cf1 <- model3$coefficients[[1]] + model3$coefficients[[7]] + model3$coefficients[[6]]

# create data frame
time <- as.factor(c(0, 1, 0, 1, 0, 1))
grp <- c("control", "control", "treatment", "treatment", "counterfactual", "counterfactual")
know <- c(c0, c1, t0, t1, cf0, cf1)

data <- data.frame(time, grp, know)


# create plot

#pdf("did.pdf", width = 8, height = 5.0)
#png("did.png", width = 8, height = 5.0, units = "in", res = 100)

ggplot(data, aes(x = time, y = know)) +
  geom_point(size = 2.0) +
  geom_line(aes(group = grp, color = grp, linetype = grp), size = 1.0) +
  scale_linetype_manual(values = c("solid", "solid", "dotted"), 
                        name = "Group", 
                        breaks = c("control", "treatment", "counterfactual"), 
                        labels = c("Control", "Treatment", "Counterfactual")) +
  scale_color_manual(values = c("dark grey", "black", "black"), 
                     name = "Group", 
                     breaks = c("control", "treatment", "counterfactual"), 
                     labels = c("Control", "Treatment", "Counterfactual")) +
  labs(#title = "Results of difference-in-differences regression",
       x = "Time", y = "IBD-KNOW score") +
  geom_vline(xintercept = 1) +
  geom_vline(xintercept = 2) +
  ylim(c(18,25)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box.background = element_rect(color = "black"), 
        legend.background = element_blank())

#dev.off()





### robustness checks ####

# save original data frame
insta_save <- insta

# exlucde drop outs
insta <- filter(insta, completed_study == 1)


# estimate models
model0 <- lm(know ~ female + diagnosis + age + duration, data = insta)
model1 <- lm(know ~ female + diagnosis + age + duration + time_dummy, data = insta)
model2 <- lm(know ~ female + diagnosis + age + duration + group_dummy, data = insta)
model3 <- lm(know ~ female + diagnosis + age + duration + time_dummy*group_dummy, data = insta)


# for p-vals and F
summary(model0)
summary(model3)


# check for multicollinearity
vif(model0)
vif(model1)
vif(model2)
vif(model3)

# load saved original data frame
insta <- insta_save

