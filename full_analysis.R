############################################################
# PUBLIC SATISFACTION WITH AI
# Full Analysis Code
# nasa_3.may.2026
############################################################

# The dependent variable is AI satisfaction measured on an ordinal scale from 1 to 5.
# Therefore, the main model should be ordinal logistic regression.
# The graphs first describe satisfaction patterns, then show model-based predicted probabilities.

############################################################
# 1. LOAD PACKAGES
############################################################

install.packages(c("tidyverse", "MASS", "ordinal", "reshape2", "knitr"))

library(tidyverse)
library(MASS)
library(ordinal)
library(reshape2)
library(knitr)

############################################################
# 2. LOAD DATA
############################################################

data <- read.csv("prepared_data.csv")

# Check column names
names(data)

############################################################
# 3. PREPARE VARIABLES
############################################################

# Convert rating variables to ordered factors
data$Healthcare_rating <- ordered(data$Healthcare_rating, levels = c(1, 2, 3, 4, 5))
data$Finance_rating    <- ordered(data$Finance_rating, levels = c(1, 2, 3, 4, 5))
data$Hiring_rating     <- ordered(data$Hiring_rating, levels = c(1, 2, 3, 4, 5))
data$Education_rating  <- ordered(data$Education_rating, levels = c(1, 2, 3, 4, 5))
data$Service_rating    <- ordered(data$Service_rating, levels = c(1, 2, 3, 4, 5))

# Education level
data$Education_Level <- factor(
  data$Education_Level,
  levels = c("SSC", "HSC", "Bachelor", "Master", "PhD"),
  ordered = TRUE
)

# AI usage frequency
data$AI_Usage <- factor(
  data$AI_Usage,
  levels = c("Monthly- A few times", "Weekly- A few times", "Daily"),
  ordered = TRUE
)

############################################################
# 4. RESHAPE DATA INTO LONG FORMAT
############################################################

# Why:
# Each respondent answered satisfaction questions for multiple domains.
# Long format allows comparison across domains using one satisfaction variable.

long_data <- data %>%
  pivot_longer(
    cols = c(
      Healthcare_rating,
      Finance_rating,
      Hiring_rating,
      Education_rating,
      Service_rating
    ),
    names_to = "Domain",
    values_to = "Satisfaction"
  )

# Clean domain names
long_data$Domain <- recode(
  long_data$Domain,
  "Healthcare_rating" = "Healthcare",
  "Finance_rating"    = "Finance",
  "Hiring_rating"     = "Hiring",
  "Education_rating"  = "Education",
  "Service_rating"    = "Service"
)

long_data$Domain <- factor(
  long_data$Domain,
  levels = c("Healthcare", "Finance", "Hiring", "Education", "Service")
)

# Numeric version for descriptive graphs
long_data$Satisfaction_num <- as.numeric(long_data$Satisfaction)

# Risk context variable
# Healthcare, Finance, and Hiring are treated as high-risk contexts.
# Education and Service are treated as low-risk contexts.

long_data$Risk <- ifelse(
  long_data$Domain %in% c("Healthcare", "Finance", "Hiring"),
  "High-risk",
  "Low-risk"
)

long_data$Risk <- factor(long_data$Risk, levels = c("High-risk", "Low-risk"))

############################################################
# RQ1 / H1a / H1b
# How does satisfaction vary across decision contexts?
############################################################

############################################################
# GRAPH 1: Mean satisfaction by domain
############################################################

# Why:
# This graph directly answers whether satisfaction differs across AI decision domains.
# It is useful for H1b because it shows whether healthcare is lowest,
# finance follows, and education is highest.

domain_summary <- long_data %>%
  group_by(Domain) %>%
  summarise(
    mean_satisfaction = mean(Satisfaction_num, na.rm = TRUE),
    se = sd(Satisfaction_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

kable(domain_summary, digits = 3)

ggplot(domain_summary,
       aes(x = reorder(Domain, mean_satisfaction),
           y = mean_satisfaction)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(
      ymin = mean_satisfaction - se,
      ymax = mean_satisfaction + se
    ),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    title = "Average Public Satisfaction with AI by Decision Context",
    x = "Decision Context",
    y = "Mean Satisfaction Score"
  ) +
  theme_minimal()

############################################################
# GRAPH 2: Satisfaction distribution by domain
############################################################

# Why:
# Because satisfaction is ordinal, the distribution is important.
# This graph shows how much of each domain falls into low, medium, or high satisfaction.

ggplot(long_data, aes(x = Domain, fill = Satisfaction)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribution of AI Satisfaction Ratings by Domain",
    x = "Decision Context",
    y = "Proportion",
    fill = "Satisfaction Rating"
  ) +
  theme_minimal()

############################################################
# NEW IDEAL GRAPH 1: Heatmap of satisfaction by domain
############################################################

# Why:
# This is a very clear graph for ordinal survey data.
# It shows which satisfaction ratings are concentrated in each domain.
# Darker cells mean more respondents selected that rating.

heat_data <- long_data %>%
  group_by(Domain, Satisfaction) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Domain) %>%
  mutate(percent = n / sum(n) * 100)

ggplot(heat_data, aes(x = Satisfaction, y = Domain, fill = percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(percent, 1)), size = 3) +
  labs(
    title = "Heatmap of Satisfaction Ratings Across AI Decision Contexts",
    x = "Satisfaction Rating",
    y = "Decision Context",
    fill = "Percent"
  ) +
  theme_minimal()

############################################################
# GRAPH 3: High-risk vs low-risk satisfaction
############################################################

# Why:
# This graph directly addresses H1a:
# satisfaction should be lower in high-risk contexts than in low-risk contexts.

risk_summary <- long_data %>%
  group_by(Risk) %>%
  summarise(
    mean_satisfaction = mean(Satisfaction_num, na.rm = TRUE),
    se = sd(Satisfaction_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

kable(risk_summary, digits = 3)

ggplot(risk_summary,
       aes(x = Risk, y = mean_satisfaction, fill = Risk)) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = mean_satisfaction - se,
      ymax = mean_satisfaction + se
    ),
    width = 0.2
  ) +
  labs(
    title = "AI Satisfaction in High-risk and Low-risk Contexts",
    x = "Risk Context",
    y = "Mean Satisfaction Score"
  ) +
  theme_minimal() +
  guides(fill = "none")

############################################################
# ORDINAL MODEL FOR RQ1 / H1
############################################################

# Why:
# Since satisfaction is ordinal, ordinal logistic regression is more appropriate
# than ordinary linear regression.

model_rq1_domain <- polr(
  Satisfaction ~ Domain,
  data = long_data,
  Hess = TRUE
)

summary(model_rq1_domain)

# p-values
ctable_rq1 <- coef(summary(model_rq1_domain))
p_values_rq1 <- pnorm(abs(ctable_rq1[, "t value"]), lower.tail = FALSE) * 2
ctable_rq1 <- cbind(ctable_rq1, "p value" = p_values_rq1)
ctable_rq1

model_rq1_risk <- polr(
  Satisfaction ~ Risk,
  data = long_data,
  Hess = TRUE
)

summary(model_rq1_risk)

############################################################
# MODEL-BASED GRAPH FOR RQ1
############################################################

# Why:
# This graph shows predicted probabilities of each satisfaction level by domain.
# It is stronger than a simple mean graph because it respects the ordinal outcome.

new_domain <- data.frame(
  Domain = levels(long_data$Domain)
)

pred_domain <- predict(
  model_rq1_domain,
  newdata = new_domain,
  type = "probs"
)

pred_domain_df <- cbind(new_domain, pred_domain)

pred_domain_long <- melt(
  pred_domain_df,
  id.vars = "Domain",
  variable.name = "Satisfaction",
  value.name = "Predicted_Probability"
)

ggplot(pred_domain_long,
       aes(x = Domain,
           y = Predicted_Probability,
           fill = Satisfaction)) +
  geom_col(position = "stack") +
  labs(
    title = "Predicted Probability of Satisfaction Ratings by Domain",
    x = "Decision Context",
    y = "Predicted Probability",
    fill = "Satisfaction Rating"
  ) +
  theme_minimal()

############################################################
# RQ2 / H2a / H2b
# Age and education effects
############################################################

############################################################
# GRAPH 4: Age and satisfaction
############################################################

# Why:
# This graph examines H2a:
# older respondents are expected to report lower satisfaction with AI.

age_summary <- long_data %>%
  mutate(
    Age_Group = cut(
      Age,
      breaks = c(17, 25, 35, 45, 60, 100),
      labels = c("18-25", "26-35", "36-45", "46-60", "60+")
    )
  ) %>%
  group_by(Age_Group) %>%
  summarise(
    mean_satisfaction = mean(Satisfaction_num, na.rm = TRUE),
    se = sd(Satisfaction_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(age_summary,
       aes(x = Age_Group,
           y = mean_satisfaction,
           group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_satisfaction - se,
      ymax = mean_satisfaction + se
    ),
    width = 0.1
  ) +
  labs(
    title = "AI Satisfaction Across Age Groups",
    x = "Age Group",
    y = "Mean Satisfaction Score"
  ) +
  theme_minimal()

############################################################
# GRAPH 5: Education and satisfaction
############################################################

# Why:
# This graph examines H2b:
# respondents with higher educational attainment are expected to show higher satisfaction.

education_summary <- long_data %>%
  group_by(Education_Level) %>%
  summarise(
    mean_satisfaction = mean(Satisfaction_num, na.rm = TRUE),
    se = sd(Satisfaction_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(education_summary,
       aes(x = Education_Level,
           y = mean_satisfaction)) +
  geom_col(fill = "darkgreen") +
  geom_errorbar(
    aes(
      ymin = mean_satisfaction - se,
      ymax = mean_satisfaction + se
    ),
    width = 0.2
  ) +
  labs(
    title = "AI Satisfaction by Educational Attainment",
    x = "Education Level",
    y = "Mean Satisfaction Score"
  ) +
  theme_minimal()

############################################################
# ORDINAL MODEL FOR RQ2
############################################################

# Why:
# This model tests whether age and education significantly predict satisfaction.

model_rq2 <- polr(
  Satisfaction ~ Age + Education_Level,
  data = long_data,
  Hess = TRUE
)

summary(model_rq2)

ctable_rq2 <- coef(summary(model_rq2))
p_values_rq2 <- pnorm(abs(ctable_rq2[, "t value"]), lower.tail = FALSE) * 2
ctable_rq2 <- cbind(ctable_rq2, "p value" = p_values_rq2)
ctable_rq2

############################################################
# MODEL-BASED GRAPH FOR AGE AND EDUCATION
############################################################

# Why:
# This graph shows how the probability of the highest satisfaction rating changes
# across age and education levels.

new_age_edu <- expand.grid(
  Age = seq(min(long_data$Age, na.rm = TRUE),
            max(long_data$Age, na.rm = TRUE),
            by = 1),
  Education_Level = levels(long_data$Education_Level)
)

pred_age_edu <- predict(
  model_rq2,
  newdata = new_age_edu,
  type = "probs"
)

pred_age_edu_df <- cbind(new_age_edu, pred_age_edu)

ggplot(pred_age_edu_df,
       aes(x = Age,
           y = `5`,
           color = Education_Level)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted Probability of Highest AI Satisfaction by Age and Education",
    x = "Age",
    y = "Predicted Probability of Satisfaction = 5",
    color = "Education Level"
  ) +
  theme_minimal()

############################################################
# RQ3 / H3a / H3b
# Interaction effects
############################################################

############################################################
# GRAPH 6: Age effect by risk context
############################################################

# Why:
# This graph examines H3a:
# the negative effect of age may be stronger in high-risk contexts.

ggplot(long_data,
       aes(x = Age,
           y = Satisfaction_num,
           color = Risk)) +
  geom_jitter(alpha = 0.25, width = 0.3, height = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Age and AI Satisfaction by Risk Context",
    x = "Age",
    y = "Satisfaction Score",
    color = "Risk Context"
  ) +
  theme_minimal()

############################################################
# GRAPH 7: Education effect by risk context
############################################################

# Why:
# This graph examines H3b:
# the education effect may differ between high-risk and low-risk contexts.

edu_risk_summary <- long_data %>%
  group_by(Education_Level, Risk) %>%
  summarise(
    mean_satisfaction = mean(Satisfaction_num, na.rm = TRUE),
    se = sd(Satisfaction_num, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(edu_risk_summary,
       aes(x = Education_Level,
           y = mean_satisfaction,
           fill = Risk)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(
      ymin = mean_satisfaction - se,
      ymax = mean_satisfaction + se
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Education Effect on AI Satisfaction by Risk Context",
    x = "Education Level",
    y = "Mean Satisfaction Score",
    fill = "Risk Context"
  ) +
  theme_minimal()

############################################################
# ORDINAL MODEL FOR RQ3
############################################################

# Why:
# This model tests whether age and education effects change across risk contexts.
# The interaction terms are the main test for H3a and H3b.

model_rq3 <- polr(
  Satisfaction ~ Risk * Age + Risk * Education_Level,
  data = long_data,
  Hess = TRUE
)

summary(model_rq3)

ctable_rq3 <- coef(summary(model_rq3))
p_values_rq3 <- pnorm(abs(ctable_rq3[, "t value"]), lower.tail = FALSE) * 2
ctable_rq3 <- cbind(ctable_rq3, "p value" = p_values_rq3)
ctable_rq3

############################################################
# NEW IDEAL GRAPH 2: Predicted age interaction by risk
############################################################

# Why:
# This is one of the best graphs for RQ3.
# It shows whether the age slope differs in high-risk and low-risk contexts.

new_age_risk <- expand.grid(
  Age = seq(min(long_data$Age, na.rm = TRUE),
            max(long_data$Age, na.rm = TRUE),
            by = 1),
  Risk = levels(long_data$Risk),
  Education_Level = "Bachelor"
)

pred_age_risk <- predict(
  model_rq3,
  newdata = new_age_risk,
  type = "probs"
)

pred_age_risk_df <- cbind(new_age_risk, pred_age_risk)

ggplot(pred_age_risk_df,
       aes(x = Age,
           y = `5`,
           color = Risk)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Interaction Effect of Age and Risk Context on High AI Satisfaction",
    x = "Age",
    y = "Predicted Probability of Satisfaction = 5",
    color = "Risk Context"
  ) +
  theme_minimal()

############################################################
# NEW IDEAL GRAPH 3: Predicted education interaction by risk
############################################################

# Why:
# This graph tests whether education has a stronger or weaker effect
# depending on whether the AI decision context is high-risk or low-risk.

new_edu_risk <- expand.grid(
  Age = mean(long_data$Age, na.rm = TRUE),
  Risk = levels(long_data$Risk),
  Education_Level = levels(long_data$Education_Level)
)

pred_edu_risk <- predict(
  model_rq3,
  newdata = new_edu_risk,
  type = "probs"
)

pred_edu_risk_df <- cbind(new_edu_risk, pred_edu_risk)

ggplot(pred_edu_risk_df,
       aes(x = Education_Level,
           y = `5`,
           group = Risk,
           color = Risk)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Effect of Education and Risk Context on High AI Satisfaction",
    x = "Education Level",
    y = "Predicted Probability of Satisfaction = 5",
    color = "Risk Context"
  ) +
  theme_minimal()

############################################################
# OPTIONAL: AI USAGE AS CONTROL VARIABLE
############################################################

# Why:
# AI usage may influence satisfaction because frequent users may be more familiar with AI.
# This can be added as a control variable, not as the main hypothesis variable.

model_control <- polr(
  Satisfaction ~ Domain + Age + Education_Level + AI_Usage,
  data = long_data,
  Hess = TRUE
)

summary(model_control)

############################################################
# TABLES FOR PAPER
############################################################

# Table 1: Domain satisfaction summary
kable(domain_summary, digits = 3)

# Table 2: Risk context satisfaction summary
kable(risk_summary, digits = 3)

# Table 3: Education satisfaction summary
kable(education_summary, digits = 3)

# Table 4: Ordinal regression outputs
ctable_rq1
ctable_rq2
ctable_rq3