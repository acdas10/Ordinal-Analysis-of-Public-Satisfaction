# ============================================================
# Title: Public Satisfaction with AI in High-Risk and Low-Risk
#        Decision Contexts: An Ordinal Analysis
# ============================================================
# Packages: base R, MASS (polr), ggplot2, reshape2, lmtest,
#           brant (Brant test), car (Anova), effects
# ============================================================

# --- 0. Install / load packages ---------------------------------------------------
pkgs <- c("MASS", "ggplot2", "reshape2", "lmtest", "car", "effects",
          "gridExtra", "scales")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org",
                                        quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 1. Load & prepare data -------------------------------------------------------
df <- read.csv("prepared_data.csv", stringsAsFactors = FALSE)
df$X <- NULL   # drop row-index column if present
names(df)[names(df) == "Unnamed..0"] <- NULL

# Ordered factor: Education
edu_order <- c("SSC", "HSC", "Bachelor", "Master", "PhD")
df$Education_Level <- factor(df$Education_Level, levels = edu_order, ordered = TRUE)

# Numeric education score for regression (1-5)
df$Edu_Score <- as.numeric(df$Education_Level)

# Ordinal ratings as ordered factors (1–5)
rating_vars <- c("Healthcare_rating", "Finance_rating", "Hiring_rating",
                 "Education_rating", "Service_rating")
for (v in rating_vars) {
  df[[v]] <- factor(df[[v]], levels = 1:5, ordered = TRUE)
}

# Long format: one row per domain rating
df_long <- reshape(df,
  varying   = rating_vars,
  v.names   = "Rating",
  timevar   = "Domain",
  times     = c("Healthcare", "Finance", "Hiring", "Education", "Service"),
  direction = "long"
)
df_long$Domain <- factor(df_long$Domain,
  levels = c("Healthcare", "Finance", "Hiring", "Education", "Service"))

# Risk classification: Healthcare & Finance = High; Education & Service = Low
# Hiring = medium (treated as high for H1a/H3 comparisons)
df_long$Risk <- ifelse(df_long$Domain %in% c("Healthcare", "Finance", "Hiring"),
                       "High-Risk", "Low-Risk")
df_long$Risk <- factor(df_long$Risk, levels = c("Low-Risk", "High-Risk"))

# Numeric rating for descriptive/plotting convenience
df_long$Rating_num <- as.numeric(as.character(df_long$Rating))

cat("=== Data dimensions ===\n")
cat("Observations:", nrow(df), "\n")
cat("Domains in long format:", nrow(df_long), "\n\n")

# --- 2. Descriptive Statistics ---------------------------------------------------
cat("=== Descriptive Statistics: Mean Satisfaction by Domain ===\n")
desc <- aggregate(Rating_num ~ Domain, data = df_long, FUN = function(x)
  c(Mean = round(mean(x), 3), SD = round(sd(x), 3), Median = median(x)))
print(do.call(data.frame, desc))

cat("\n=== Mean Satisfaction by Risk Level ===\n")
desc_risk <- aggregate(Rating_num ~ Risk, data = df_long, FUN = function(x)
  c(Mean = round(mean(x), 3), SD = round(sd(x), 3)))
print(do.call(data.frame, desc_risk))


# ============================================================
# FIGURE 1: Stacked bar chart – satisfaction distribution by domain
# ============================================================
df_prop <- as.data.frame(table(df_long$Domain, df_long$Rating_num))
colnames(df_prop) <- c("Domain", "Rating", "Count")
df_prop$Prop <- ave(df_prop$Count, df_prop$Domain, FUN = function(x) x / sum(x))

fig1 <- ggplot(df_prop, aes(x = Domain, y = Prop * 100, fill = factor(Rating))) +
  geom_bar(stat = "identity", color = "white", size = 0.3) +
  scale_fill_manual(
    values = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
    name   = "Satisfaction\nLevel"
  ) +
  labs(
    title    = "Figure 1. Distribution of Satisfaction Ratings by AI Decision Domain",
    subtitle = "Stacked proportions (%) across five domains",
    x        = "Decision Domain",
    y        = "Percentage (%)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "right",
    axis.text.x   = element_text(angle = 15, hjust = 1)
  )

ggsave("Figure1_Stacked_Bar.png", fig1, width = 9, height = 5.5, dpi = 300)
cat("Figure 1 saved.\n")


# ============================================================
# FIGURE 2: Box plot – mean satisfaction by domain + risk label
# ============================================================
domain_means <- aggregate(Rating_num ~ Domain + Risk, data = df_long, FUN = mean)

fig2 <- ggplot(df_long, aes(x = Domain, y = Rating_num, fill = Risk)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                    name = "Risk Context") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "yellow", color = "black") +
  labs(
    title    = "Figure 2. Satisfaction Distributions Across AI Decision Domains",
    subtitle = "Box: IQR; diamond: mean; colour: risk classification",
    x        = "Decision Domain",
    y        = "Satisfaction Rating (1–5)"
  ) +
  scale_y_continuous(breaks = 1:5) +
  theme_bw(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 11, color = "grey40"),
    legend.position = "top",
    axis.text.x     = element_text(angle = 15, hjust = 1)
  )

ggsave("Figure2_Boxplot_Domain.png", fig2, width = 9, height = 5.5, dpi = 300)
cat("Figure 2 saved.\n")


# ============================================================
# FIGURE 3: Mean satisfaction by Risk × Education
# ============================================================
edu_risk <- aggregate(Rating_num ~ Risk + Education_Level, data = df_long, FUN = mean)

fig3 <- ggplot(edu_risk, aes(x = Education_Level, y = Rating_num,
                              group = Risk, color = Risk, shape = Risk)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                     name = "Risk Context") +
  scale_shape_manual(values = c(16, 17), name = "Risk Context") +
  labs(
    title    = "Figure 3. Mean Satisfaction by Education Level and Risk Context",
    subtitle = "Interaction pattern relevant to H2b and H3b",
    x        = "Education Level",
    y        = "Mean Satisfaction Rating (1–5)"
  ) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "top"
  )

ggsave("Figure3_Edu_Risk_Interaction.png", fig3, width = 9, height = 5.5, dpi = 300)
cat("Figure 3 saved.\n")


# ============================================================
# FIGURE 4: Mean satisfaction by Age group and Risk
# ============================================================
df_long$Age_Group <- cut(df_long$Age,
  breaks = c(17, 22, 27, 32, 60),
  labels = c("18–22", "23–27", "28–32", "33+"))

age_risk <- aggregate(Rating_num ~ Risk + Age_Group, data = df_long, FUN = mean)
age_risk <- age_risk[!is.na(age_risk$Age_Group), ]

fig4 <- ggplot(age_risk, aes(x = Age_Group, y = Rating_num,
                               group = Risk, color = Risk, shape = Risk)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                     name = "Risk Context") +
  scale_shape_manual(values = c(16, 17), name = "Risk Context") +
  labs(
    title    = "Figure 4. Mean Satisfaction by Age Group and Risk Context",
    subtitle = "Interaction pattern relevant to H2a and H3a",
    x        = "Age Group",
    y        = "Mean Satisfaction Rating (1–5)"
  ) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "top"
  )

ggsave("Figure4_Age_Risk_Interaction.png", fig4, width = 9, height = 5.5, dpi = 300)
cat("Figure 4 saved.\n")


# ============================================================
# H1a / H1b: Kruskal-Wallis + pairwise Wilcoxon across domains
# ============================================================
cat("\n\n=== H1a/H1b: Kruskal-Wallis test across domains ===\n")
kw_domain <- kruskal.test(Rating_num ~ Domain, data = df_long)
print(kw_domain)

cat("\n=== Pairwise Wilcoxon (Bonferroni) for domain differences ===\n")
pairwise_domain <- pairwise.wilcox.test(df_long$Rating_num, df_long$Domain,
                                         p.adjust.method = "bonferroni")
print(pairwise_domain)

cat("\n=== H1a: High-Risk vs Low-Risk (Mann-Whitney U) ===\n")
mwu_risk <- wilcox.test(Rating_num ~ Risk, data = df_long, exact = FALSE)
print(mwu_risk)


# ============================================================
# ORDINAL LOGISTIC REGRESSION – domain-by-domain models
# (MASS::polr; method = "logistic" = proportional-odds)
# ============================================================
# Helper to print model summary cleanly
print_polr <- function(model, label) {
  cat("\n\n===", label, "===\n")
  s <- summary(model)
  coef_table <- coef(s)
  # Compute p-values via normal approximation (column name is "t value")
  t_col <- grep("t value", colnames(coef_table))
  p_vals <- 2 * pnorm(abs(coef_table[, t_col]), lower.tail = FALSE)
  coef_table <- cbind(coef_table, "p-value" = p_vals)
  # Compute OR and 95% CI for predictor rows only (not intercepts)
  pred_rows <- !grepl("\\|", rownames(coef_table))
  ci   <- confint(model)
  OR   <- exp(coef(model))
  OR_lo <- exp(ci[, 1])
  OR_hi <- exp(ci[, 2])
  cat("Coefficients with p-values:\n")
  print(round(coef_table, 4))
  cat("\nOdds Ratios (95% CI) for predictors:\n")
  for (nm in names(OR)) {
    cat(sprintf("  %-20s OR = %.3f  [%.3f, %.3f]\n",
                nm, OR[nm], OR_lo[nm], OR_hi[nm]))
  }
  cat("AIC:", round(AIC(model), 2), "\n")
}

# ---- Healthcare (High-Risk) ----
m_hc <- polr(Healthcare_rating ~ Age + Edu_Score, data = df,
             method = "logistic", Hess = TRUE)
print_polr(m_hc, "Model 1: Healthcare (High-Risk)")

# ---- Finance / Banking (High-Risk) ----
m_fin <- polr(Finance_rating ~ Age + Edu_Score, data = df,
              method = "logistic", Hess = TRUE)
print_polr(m_fin, "Model 2: Finance/Banking (High-Risk)")

# ---- Hiring (High-Risk) ----
m_hir <- polr(Hiring_rating ~ Age + Edu_Score, data = df,
              method = "logistic", Hess = TRUE)
print_polr(m_hir, "Model 3: Hiring (High-Risk)")

# ---- Education (Low-Risk) ----
m_edu <- polr(Education_rating ~ Age + Edu_Score, data = df,
              method = "logistic", Hess = TRUE)
print_polr(m_edu, "Model 4: Education (Low-Risk)")

# ---- Customer Service (Low-Risk) ----
m_svc <- polr(Service_rating ~ Age + Edu_Score, data = df,
              method = "logistic", Hess = TRUE)
print_polr(m_svc, "Model 5: Customer Service (Low-Risk)")


# ============================================================
# POOLED MODELS with Risk interaction (H3a / H3b)
# ============================================================
df_long$Rating_ord <- factor(df_long$Rating_num, levels = 1:5, ordered = TRUE)
df_long$Edu_Score  <- as.numeric(df_long$Education_Level)

add_pval <- function(ct) {
  t_col <- grep("t value", colnames(ct))
  cbind(ct, "p-value" = 2 * pnorm(abs(ct[, t_col]), lower.tail = FALSE))
}

cat("\n\n=== Model 6: Pooled – Main effects (RQ2 / H2a / H2b) ===\n")
m_main <- polr(Rating_ord ~ Age + Edu_Score + Risk, data = df_long,
               method = "logistic", Hess = TRUE)
print(round(add_pval(coef(summary(m_main))), 4))
cat("AIC:", round(AIC(m_main), 2), "\n")

cat("\n=== Model 7: Pooled – Age × Risk interaction (H3a) ===\n")
m_age_int <- polr(Rating_ord ~ Age * Risk + Edu_Score, data = df_long,
                  method = "logistic", Hess = TRUE)
print(round(add_pval(coef(summary(m_age_int))), 4))
cat("AIC:", round(AIC(m_age_int), 2), "\n")

cat("\n=== LRT: Main effects vs Age × Risk interaction ===\n")
print(lrtest(m_main, m_age_int))

cat("\n=== Model 8: Pooled – Education × Risk interaction (H3b) ===\n")
m_edu_int <- polr(Rating_ord ~ Age + Edu_Score * Risk, data = df_long,
                  method = "logistic", Hess = TRUE)
print(round(add_pval(coef(summary(m_edu_int))), 4))
cat("AIC:", round(AIC(m_edu_int), 2), "\n")

cat("\n=== LRT: Main effects vs Edu × Risk interaction ===\n")
print(lrtest(m_main, m_edu_int))

# Full interaction model
cat("\n=== Model 9: Full interaction model ===\n")
m_full <- polr(Rating_ord ~ Age * Risk + Edu_Score * Risk, data = df_long,
               method = "logistic", Hess = TRUE)
print(round(add_pval(coef(summary(m_full))), 4))
cat("AIC:", round(AIC(m_full), 2), "\n")

cat("\n=== LRT: Main vs Full interaction model ===\n")
print(lrtest(m_main, m_full))


# ============================================================
# PROPORTIONAL-ODDS ASSUMPTION CHECK
# Manual approach: fit binary logistic models at each cut-point
# and compare coefficients. Significant deviation = violation.
# ============================================================
cat("\n\n=== Proportional-Odds Assumption: Coefficient Stability Across Cut-Points ===\n")
cat("(Brant-style check: coefficients from binary logits at each threshold)\n")

check_po <- function(rating_col, pred_cols, data, label) {
  cat("\n--", label, "--\n")
  r <- as.numeric(as.character(data[[rating_col]]))
  cuts <- 1:4
  coef_mat <- matrix(NA, nrow = length(cuts), ncol = length(pred_cols),
                     dimnames = list(paste0("Y>", cuts), pred_cols))
  for (k in cuts) {
    bin_y <- as.integer(r > k)
    tmp_df <- data.frame(Y = bin_y, data[, pred_cols, drop = FALSE])
    fit <- glm(Y ~ ., data = tmp_df, family = binomial)
    coef_mat[k, ] <- coef(fit)[pred_cols]
  }
  print(round(coef_mat, 4))
  cat("Note: Similar coefficients across rows support proportional-odds assumption.\n")
}

check_po("Healthcare_rating", c("Age", "Edu_Score"), df, "Healthcare")
check_po("Finance_rating",    c("Age", "Edu_Score"), df, "Finance")
check_po("Hiring_rating",     c("Age", "Edu_Score"), df, "Hiring")
check_po("Education_rating",  c("Age", "Edu_Score"), df, "Education")
check_po("Service_rating",    c("Age", "Edu_Score"), df, "Service")


# ============================================================
# FIGURE 5: Predicted probabilities – Age effect by Risk
# ============================================================
# Generate prediction grid
age_seq  <- seq(min(df$Age), max(df$Age), length.out = 50)
edu_mean <- mean(df_long$Edu_Score)

pred_grid <- data.frame(
  Age       = rep(age_seq, 2),
  Risk      = rep(c("Low-Risk", "High-Risk"), each = 50),
  Edu_Score = edu_mean
)
pred_grid$Risk <- factor(pred_grid$Risk, levels = c("Low-Risk", "High-Risk"))

pp_age <- predict(m_age_int, newdata = pred_grid, type = "probs")
pred_age_df <- cbind(pred_grid, as.data.frame(pp_age))
colnames(pred_age_df)[4:8] <- paste0("P(Y=", 1:5, ")")

# Melt for plotting P(Y≥4) = high satisfaction
pred_age_df$P_high <- pred_age_df$`P(Y=4)` + pred_age_df$`P(Y=5)`

fig5 <- ggplot(pred_age_df, aes(x = Age, y = P_high, color = Risk, linetype = Risk)) +
  geom_line(size = 1.4) +
  scale_color_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                     name = "Risk Context") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Risk Context") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = "Figure 5. Predicted Probability of High Satisfaction (≥4) by Age and Risk",
    subtitle = "Based on pooled ordinal model with Age × Risk interaction (H3a)",
    x        = "Age (years)",
    y        = "P(Satisfaction ≥ 4)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "top"
  )

ggsave("Figure5_Predicted_Age_Risk.png", fig5, width = 9, height = 5.5, dpi = 300)
cat("Figure 5 saved.\n")


# ============================================================
# FIGURE 6: Predicted probabilities – Education effect by Risk
# ============================================================
edu_seq <- 1:5
age_mean <- mean(df$Age)

pred_edu_grid <- data.frame(
  Edu_Score = rep(edu_seq, 2),
  Risk       = rep(c("Low-Risk", "High-Risk"), each = 5),
  Age        = age_mean
)
pred_edu_grid$Risk <- factor(pred_edu_grid$Risk, levels = c("Low-Risk", "High-Risk"))

pp_edu <- predict(m_edu_int, newdata = pred_edu_grid, type = "probs")
pred_edu_df <- cbind(pred_edu_grid, as.data.frame(pp_edu))
colnames(pred_edu_df)[4:8] <- paste0("P(Y=", 1:5, ")")
pred_edu_df$P_high <- pred_edu_df$`P(Y=4)` + pred_edu_df$`P(Y=5)`
pred_edu_df$Education_Level <- factor(edu_order[pred_edu_df$Edu_Score],
                                       levels = edu_order)

fig6 <- ggplot(pred_edu_df, aes(x = Education_Level, y = P_high,
                                  group = Risk, color = Risk, shape = Risk)) +
  geom_line(size = 1.4) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                     name = "Risk Context") +
  scale_shape_manual(values = c(16, 17), name = "Risk Context") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = "Figure 6. Predicted Probability of High Satisfaction by Education and Risk",
    subtitle = "Based on pooled ordinal model with Education × Risk interaction (H3b)",
    x        = "Education Level",
    y        = "P(Satisfaction ≥ 4)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "top"
  )

ggsave("Figure6_Predicted_Edu_Risk.png", fig6, width = 9, height = 5.5, dpi = 300)
cat("Figure 6 saved.\n")


# ============================================================
# FIGURE 7: OR forest plot for domain-specific models
# ============================================================
extract_or <- function(model, domain, risk) {
  ci    <- tryCatch(confint(model), error = function(e) NULL)
  if (is.null(ci)) return(NULL)
  vars  <- c("Age", "Edu_Score")
  idx   <- match(vars, rownames(ci))
  if (anyNA(idx)) return(NULL)
  data.frame(
    Domain    = domain,
    Risk      = risk,
    Variable  = vars,
    OR        = exp(coef(model)[vars]),
    OR_lo     = exp(ci[vars, 1]),
    OR_hi     = exp(ci[vars, 2])
  )
}

or_list <- list(
  extract_or(m_hc,  "Healthcare",  "High-Risk"),
  extract_or(m_fin, "Finance",     "High-Risk"),
  extract_or(m_hir, "Hiring",      "High-Risk"),
  extract_or(m_edu, "Education",   "Low-Risk"),
  extract_or(m_svc, "Service",     "Low-Risk")
)
or_df <- do.call(rbind, or_list)
or_df$Domain   <- factor(or_df$Domain,
  levels = c("Healthcare", "Finance", "Hiring", "Education", "Service"))
or_df$Variable <- ifelse(or_df$Variable == "Age", "Age", "Education")
or_df$Risk     <- factor(or_df$Risk, levels = c("Low-Risk", "High-Risk"))

fig7 <- ggplot(or_df, aes(x = OR, y = Domain, color = Risk, shape = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = OR_lo, xmax = OR_hi), height = 0.25, size = 0.8) +
  geom_point(size = 3.5) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_color_manual(values = c("Low-Risk" = "#4575b4", "High-Risk" = "#d73027"),
                     name = "Risk Context") +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(
    title    = "Figure 7. Odds Ratios (95% CI) from Domain-Specific Ordinal Models",
    subtitle = "Vertical dashed line = OR of 1 (null effect)",
    x        = "Odds Ratio",
    y        = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "top"
  )

ggsave("Figure7_Forest_Plot.png", fig7, width = 10, height = 5.5, dpi = 300)
cat("Figure 7 saved.\n")


# ============================================================
# FIGURE 8: Heatmap – mean satisfaction by Education × Domain
# ============================================================
heat_data <- aggregate(Rating_num ~ Education_Level + Domain, data = df_long,
                        FUN = mean)

fig8 <- ggplot(heat_data, aes(x = Domain, y = Education_Level, fill = Rating_num)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Rating_num, 2)), size = 4) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                        midpoint = 3, name = "Mean\nRating") +
  labs(
    title    = "Figure 8. Heatmap of Mean Satisfaction by Education Level and Domain",
    subtitle = "Cell values = mean satisfaction rating (1–5)",
    x        = "Decision Domain",
    y        = "Education Level"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text.x   = element_text(angle = 15, hjust = 1)
  )

ggsave("Figure8_Heatmap.png", fig8, width = 9, height = 5.5, dpi = 300)
cat("Figure 8 saved.\n")


# ============================================================
# FIGURE 9: Model fit comparison (AIC bar chart)
# ============================================================
model_aic <- data.frame(
  Model = c("M6: Main Effects", "M7: Age×Risk", "M8: Edu×Risk", "M9: Full Interaction"),
  AIC   = c(AIC(m_main), AIC(m_age_int), AIC(m_edu_int), AIC(m_full))
)
model_aic$Model <- factor(model_aic$Model, levels = model_aic$Model)

fig9 <- ggplot(model_aic, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "grey30") +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "Figure 9. AIC Comparison Across Pooled Ordinal Models",
    subtitle = "Lower AIC = better model fit",
    x        = NULL,
    y        = "AIC"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text.x   = element_text(angle = 10, hjust = 1)
  )

ggsave("Figure9_AIC_Comparison.png", fig9, width = 8, height = 5, dpi = 300)
cat("Figure 9 saved.\n")


# ============================================================
# HYPOTHESIS SUMMARY TABLE
# ============================================================
cat("\n\n=== HYPOTHESIS SUMMARY ===\n")
hyp_summary <- data.frame(
  Hypothesis = c("H1a", "H1b", "H2a", "H2b", "H3a", "H3b"),
  Statement  = c(
    "Satisfaction lower in high-criticality contexts",
    "Healthcare < Banking < Education",
    "Age negatively associated with satisfaction",
    "Education positively associated with satisfaction",
    "Age effect stronger in high-criticality contexts",
    "Education effect stronger in high-criticality contexts"
  ),
  Test = c(
    "Mann-Whitney U (Risk groups)",
    "Kruskal-Wallis + pairwise Wilcoxon",
    "polr coefficient (Age, pooled)",
    "polr coefficient (Edu_Score, pooled)",
    "LRT: M6 vs M7 (Age×Risk)",
    "LRT: M6 vs M8 (Edu×Risk)"
  )
)
print(hyp_summary, row.names = FALSE)

cat("\n\nAll analyses complete. Figures saved as PNG in working directory.\n")
