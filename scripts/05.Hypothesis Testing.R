library(tidyverse)

# 1. Load Prepared Data ---------------------------------------------------
students <- readRDS("data/processed/student-mat_cleaned.rds")

# 2. Test 1: Finsl Grade by Sex -------------------------------------------
test_sex <- t.test(G3 ~ sex, data = students)

print(test_sex)

# 3. Test 2: Final grade by study time ------------------------------------
test_studytime <- kruskal.test(
  G3 ~ studytime,
  data = students
)

print(test_studytime)


# 4. Test 3: Absences and Final Grade -------------------------------------
test_absences <- cor.test(
  students$absences,
  students$G3,
  method = "spearman",
  exact = FALSE
)

print(test_absences)


# 5. Save results ---------------------------------------------------------
results <- tibble(
  analysis = c(
    "Final Grade by Sex",
    "Final Grade by Study Time",
    "Absences vs Final Grade"
  ),
  method = c(
    "Welh's t-test",
    "Kruskal-Wallis test",
    "Spearman correlation"
  ),
  statistic = c(
    unname(test_sex$statistic),
    unname(test_studytime$statistic),
    unname(test_absences$statistic)
  ),
  p_value = c(
    test_sex$p.value,
    test_studytime$p.value,
    test_absences$p.value
  ),
  conclusion = if_else(
    p_value < 0.05,
    "Statistically significant",
    "Not statistically significant"
  )
)

write_csv(
  results,
  "outputs/tables/hyphothesis_testing_results.csv"
)

print(results)
