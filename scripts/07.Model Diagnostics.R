library(tidyverse)
library(car)


# 1. Load Data ------------------------------------------------------------
students <- readRDS("data/processed/student-mat_cleaned.rds")

# 2. Build Model ----------------------------------------------------------
# Full model
model_full <- lm(
  G3 ~ sex + age + studytime + failures + absences,
  data = students
)

# Reduced model: studytime removed as a whole variable
model_reduced <- lm(
  G3 ~ sex + age + failures + absences,
  data = students
)


# 3. Compare Model --------------------------------------------------------
print(summary(model_full))
print(summary(model_reduced))

# Compare model fit
model_comparison <- AIC(model_full, model_reduced)
print(model_comparison)

# Test whether studytime improves the full model
print(anova(model_reduced, model_full))

# 4. Select Model for Diagnostics -----------------------------------------
final_model <- model_full

# 5. Diagnostics Plots ----------------------------------------------------
png(
  "outputs/figures/model_diagnostics.png",
  width = 1200,
  height = 1000,
  res = 150
)

par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

dev.off()



# 6. Multicollinearity ----------------------------------------------------
vif_results <- car::vif(final_model)
print(vif_results)


# 7. Residual Normality ---------------------------------------------------
normality_test <- shapiro.test(residuals(final_model))
print(normality_test)


# 8. Save results ---------------------------------------------------------
capture.output(
  summary(final_model),
  vif_results,
  normality_test,
  file = "outputs/tables/diagnostic_results.txt"
)

write_csv(
  tibble(
    model = c("Full model", "Reduced model"),
    AIC = c(AIC(model_full), AIC(model_reduced)),
    adjusted_r_squared = c(
      summary(model_full)$adj.r.squared,
      summary(model_reduced)$adj.r.squared
    )
  ),
  "outputs/tables/model_comparison.csv"
)



