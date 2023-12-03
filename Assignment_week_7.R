library('tidyverse')
library('broom')
library(lme4,quietly=TRUE)
library(ggplot2)

### DESCRIPTION OF THE DATASET

data(sleepstudy)
?sleepstudy
summary(sleepstudy)

## VISUALIZE EFFECT OF SUBJECTS ON REACTION TIME

ggplot(sleepstudy, aes(x = factor(Subject), y = Reaction, fill = factor(Subject))) +
  geom_boxplot() +
  labs(title = "Boxplot of Reaction Time by Subject",
       x = "Subject",
       y = "Reaction Time") +
  theme_minimal()

sleepstudy %>% select(c(Subject,Reaction)) %>%
  group_by(Subject) %>%
  summarise(Mean_reaction = mean(Reaction))

## SUBSETTING THE DATASET
## VISUALIZE EFFECT OF DAYS ON REACTION TIME

data_1 <- filter(sleepstudy,Days < 2)
summary(data_1)

boxplot(Reaction ~ Days, col=c("white","lightgray"),data_1)

data_2 <- filter(sleepstudy,Days > 2)
summary(data_2)

boxplot(Reaction ~ Days, col=c("white","lightgray"),data_2)

data_3 <- filter(sleepstudy,Days == 2)
summary(data_3)

boxplot(Reaction ~ Subject, col=c("white","lightgray"),sleepstudy)

## PLOT OF HISTOGRAM

hist(sleepstudy$Reaction)

##PLOTTING THE DENSITY DISTRIBUTION OF THE RESPONSE VARIABLE REACTION TIME

ggplot(sleepstudy, aes(x = Reaction)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "PuOr")
  labs(title = "Densityplot of Reaction Time",
       y = "Reaction Time") +
  theme_minimal()

### COMPARING MEANS BETWEEN THE SUBSETTED GROUPS
  
## Extracting the Reaction column
data_1_rt <- data_1$Reaction
data_2_rt <- data_2$Reaction
data_3_rt <- data_3$Reaction
  
##  Two-sample t-test for Adaption stage and Baseline
result_1 <- t.test(data_1_rt, data_3_rt)
result_1
  
## VISUALIZING RESULT FROM T TEST
combined_data_1 <- rbind(
  data.frame(Group = "Adaptation", ReactionTime = data_1_rt),
  data.frame(Group = "Baseline", ReactionTime = data_3_rt)
)
  
ggplot(combined_data_1, aes(x = Group, y = ReactionTime, fill = Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Reaction Time Between Adaption stage and Baseline",
        x = "Group",
        y = "Reaction Time") +
  theme_minimal()
  
## Two-sample t-test for Baseline and Sleep deprived
result_2 <- t.test(data_3_rt, data_2_rt)
result_2
  
## VISUALIZING RESULT FROM T TEST
combined_data_2 <- rbind(
  data.frame(Group = "baseline", ReactionTime = data_3_rt),
  data.frame(Group = "sleep deprived", ReactionTime = data_2_rt)
)
  
ggplot(combined_data_2, aes(x = Group, y = ReactionTime, fill = Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Reaction Time Between Baseline and Sleep deprived",
        x = "Group",
        y = "Reaction Time") +
  theme_minimal()
  
  
### CHECK FOR NORMALITY OF THE RESPONSE VARIABLE

shapiro_test_result <- shapiro.test(sleepstudy$Reaction)
print(shapiro_test_result)
p_value <- shapiro_test_result$p.value
p_value

## DATA TRANSFORMATION
sleepstudy$LogReaction <- log(sleepstudy$Reaction)


## MODEL IMPLEMENTATION

model_1 <- lmer(Reaction~ Days + (1|Subject), sleepstudy)
summary(model_1)

Intercept <- exp(5.530065)
Intercept

model_2 <- lmer(LogReaction~ Days*(1|Subject), sleepstudy)
summary(model_2)

model_3 <- lmer(LogReaction~ Days*(1|Subject), data_2)
summary(model_3)
View(data_2)
## RESIDUAL ANALYSIS FOR MODEL 1

res <- residuals(model_1)

par(mfrow = c(1,3))

hist(res)

qqnorm(res)

qqline(res)

plot(fitted(model_1),res)

## RESIDUAL ANALYSIS FOR MODEL 2

res <- residuals(model_2)

par(mfrow = c(1,3))

hist(res)

qqnorm(res)

qqline(res)

plot(fitted(model_2),res)
