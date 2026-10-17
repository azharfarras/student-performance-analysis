library(tidyverse)

# 1. Load Prepared Data ---------------------------------------------------
students <- readRDS("data/processed/student-mat_cleaned.rds")


# 2. Correlation Analysis -------------------------------------------------

correlation <- cor.test(
  students$absences,
  students$G3,
  method = "spearman",
  exact = FALSE
)

print(correlation)


# 3. Scatterplot ----------------------------------------------------------

plot_correlation <- ggplot(students, aes(x = absences, y = G3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Absences and Final Mathematics Grade",
    x = "Number of Absences",
    y = "Final Grade (G3)"
  ) +
  theme_minimal()

print(plot_correlation)

ggsave(
  "outputs/figures/absences_correlation.png",
  plot_correlation,
  width = 8,
  height = 5
)


# 3. Multiple linear Regression -------------------------------------------
model <- lm(G3 ~ sex + age + studytime + failures + absences,
            data = students)

summary(model)


# 4. Save Regression Model ------------------------------------------------
regression_results <- as.data.frame(
  summary(model)$coefficients
)

regression_results$variable <- rownames(regression_results)
rownames(regression_results) <- NULL

regression_results <- regression_results |>
  select(
    variable,
    Estimate,
    'Std. Error',
    't value',
    'Pr(>|t|)'
  )

write_csv(
  regression_results,
  "outputs/tables/regression_results.csv"
)

capture.output(
  summary(model),
  file = "outputs/tables/regression_summary.txt"
)
