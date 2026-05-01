##-------------------------------------
# Does satisfaction differ by domain AND usage level?
# Do frequent users trust AI more across domains?
#---------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data
df <- read.csv("prepared_data.csv")

# Convert wide → long using base R
df_long <- data.frame(
  Domain = rep(c("Healthcare","Finance","Education","Hiring","Service"),
               each = nrow(df)),
  Response = c(df$Healthcare,
               df$Finance,
               df$Education,
               df$Hiring,
               df$Service)
)


# Compute proportion of "Yes" for each domain
yes_rate <- c(
  Healthcare = mean(df$Healthcare == "Yes", na.rm = TRUE),
  Finance    = mean(df$Finance == "Yes", na.rm = TRUE),
  Education  = mean(df$Education == "Yes", na.rm = TRUE),
  Hiring     = mean(df$Hiring == "Yes", na.rm = TRUE),
  Service    = mean(df$Service == "Yes", na.rm = TRUE)
)

# Convert to data frame
df_plot <- data.frame(
  Domain = names(yes_rate),
  Satisfaction = yes_rate
)

# Plot (ordered)
library(ggplot2)

ggplot(df_plot, aes(x = reorder(Domain, Satisfaction), y = Satisfaction)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Public Satisfaction with AI by Domain",
       x = "Domain",
       y = "Proportion Yes") +
  theme_minimal()


# Compute proportion of "Yes" for each domain
yes_rate <- c(
  Healthcare = mean(df$Healthcare == "Yes", na.rm = TRUE),
  Finance    = mean(df$Finance == "Yes", na.rm = TRUE),
  Education  = mean(df$Education == "Yes", na.rm = TRUE),
  Hiring     = mean(df$Hiring == "Yes", na.rm = TRUE),
  Service    = mean(df$Service == "Yes", na.rm = TRUE)
)

# Convert to data frame
df_plot <- data.frame(
  Domain = names(yes_rate),
  Satisfaction = yes_rate
)

# Plot (ordered)
library(ggplot2)

ggplot(df_plot, aes(x = reorder(Domain, Satisfaction), y = Satisfaction)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Public Satisfaction with AI by Domain",
       x = "Domain",
       y = "Proportion Yes") +
  theme_minimal()


# --------------------------------------------------------------------------

# Reshape
df_long <- df %>%
  pivot_longer(cols = c(Healthcare, Finance, Education, Hiring, Service),
               names_to = "Domain",
               values_to = "Response")

# Satisfaction variable
df_long$Satisfied <- ifelse(df_long$Response == "Yes", 1, 0)

# Aggregate by Domain + Usage
df_usage <- df_long %>%
  group_by(AI_Usage, Domain) %>%
  summarise(Satisfaction = mean(Satisfied, na.rm = TRUE))

# Plot
# The personal who trusted this Domain and according to their uses duration.
ggplot(df_usage, aes(x = Domain, y = Satisfaction, fill = AI_Usage)) +
  geom_col(position = "dodge") +
  labs(title = "AI Satisfaction by Domain and Usage Frequency",
       y = "Proportion Yes") +
  theme_minimal()

ggplot(df_usage, aes(x = reorder(Domain, Satisfaction), y = Satisfaction)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~AI_Usage) +
  coord_flip() +
  labs(title = "High Trusted level by Domain across Usage Levels",
       y = "Proportion Yes") +
  theme_minimal()

####--------