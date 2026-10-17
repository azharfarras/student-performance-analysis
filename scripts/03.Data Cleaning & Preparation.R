library(tidyverse)



# 1. Configuration --------------------------------------------------------

data_path <- "data/student-mat.csv"
output_data_dir <- "data/processed"
output_table_dir <- "outputs/tables"

dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)


# 2. Load Data ------------------------------------------------------------
students <- read_delim(data_path, 
                       delim = ";", 
                       show_col_types = FALSE)

# Record original dimensions
original_rows <- nrow(students)
original_columns <- ncol(students)

# 3. Standardize Text Value --------------------------------------------------
students <- students |>
  mutate(
    across(
      where(is_character), ~ str_trim(.x)
    )
  )

# 4. Check Missing Value --------------------------------------------------
missing_before <- tibble(variable = names(students),
                         missing_count = map_int(students, ~ sum(is.na(.x))),
                         missing_percent = map_dbl(students, ~mean(is.na(.x))*100
                         ))

write_csv(missing_before,file.path(output_table_dir, "missing value before cleaning.csv"))

total_missing <- sum(is.na(students))

cat("\n ========== MISSING VALUES ================ \n")
cat("Total missing values:", total_missing, "\n")


# 5. Check Duplicate Rows -------------------------------------------------
duplicate_index <- which(duplicated(students))

if (length(duplicate_index) > 0) {
  duplicate_rows <- students |>
    slice(duplicate_index) |>
    mutate(duplicate_row_number = duplicate_index, .before = 1)
} else {
  duplicate_rows <- tibble(
    duplicate_row_number = integer()
  )
}

write_csv(duplicate_rows,
          file.path(output_table_dir, "duplicate_rows_review.csv"))

cat("\n========== DUPLICATE CHECK =========== \n")
cat("Exact duplicate rows:", length(duplicate_index), "\n")


# 6. Validate Numeric Ranges ----------------------------------------------
range_checks <- tibble(
  variable   = c("age", "absences", "failures", "G1", "G2", "G3", "studytime"),
  valid_min  = c(15, 0, 0, 0, 0, 0, 1),
  valid_max  = c(22, Inf, 3, 20, 20, 20, 4)
) |>
  rowwise() |>
  mutate(
    values_checked = sum(!is.na(students[[variable]])),
    invalid_count  = sum(!is.na(students[[variable]]) &
                           (
                             students[[variable]] < valid_min |
                              students[[variable]] > valid_max)
    )
  ) |>
  ungroup()

write_csv(range_checks, file.path(output_table_dir, "numeric_range_validation.csv"))

print(range_checks)

if (any(range_checks$invalid_count > 0)) {
  stop(
    "Invalid values detected. Review numeric_range_validation.csv ",
    "before continuing."
  )
}

# 7. Check Categorical Values ------------------------------------------------
categorical_variables <- c(
  "school", "sex", "address", "famsize", "Pstatus",
  "schoolsup", "famsup", "paid", "activities",
  "nursery", "higher", "internet", "romantic"
)

missing_columns <- setdiff(
  categorical_variables, names(students)
)

if (length(missing_columns) > 0) {
  stop(
    "Expected categorical columns are missing: ",
    paste(missing_columns, collapse = ",")
  )
}

categorical_summary <- map_dfr(
  categorical_variables,
  function(variable_name) {
    students %>%
      count(category = .data[[variable_name]], name = "frequency") %>%
      mutate(variable = variable_name, .before = 1)
  }
)

write_csv(
  categorical_summary,
  file.path(output_table_dir, "categorical_values_after_cleaning.csv")
)


# 8. Check Variables For Analysis -----------------------------------------
students <- students %>%
  mutate(
    school = factor(school),
    sex = factor(sex, levels = c("F", "M"),
                 labels = c("Female", "Male")),
    address = factor(address, levels = c("U", "R"),
                     labels = c("Urban", "Rural")),
    famsize = factor(famsize),
    Pstatus = factor(Pstatus),
    schoolsup = factor(schoolsup),
    famsup = factor(famsup),
    paid = factor(paid),
    activities = factor(activities),
    nursery = factor(nursery),
    higher = factor(higher),
    internet = factor(internet),
    romantic = factor(romantic),
    studytime = factor(
      studytime,
      levels = 1:4,
      labels = c(
        "Less than 2 hours",
        "2 to 5 hours",
        "5 to 10 hours",
        "More than 10 hours"
      )
    )
  )


students <- students %>%
  mutate(
    school_support_label = recode(
      as.character(schoolsup),
      "yes" = "Yes",
      "no" = "No"
    ),
    internet_label = recode(
      as.character(internet),
      "yes" = "Yes",
      "no" = "No"
    )
  )

# 9. Final Data Validation ------------------------------------------------
final_missing <- sum(is.na(students))
final_rows <- nrow(students)
final_columns <- ncol(students)

preparation_summary <- tibble(
  metric = c(
    "Original rows",
    "Original columns",
    "Prepared rows",
    "Prepared columns",
    "Missing values before cleaning",
    "Missing values after preparation",
    "Exact duplicate rows reviewed",
    "Rows removed automatically"
  ),
  value = c(
    original_rows,
    original_columns,
    final_rows,
    final_columns,
    total_missing,
    final_missing,
    length(duplicate_index),
    0
  )
)

write_csv(
  preparation_summary,
  file.path(output_table_dir, "data_preparation_summary.csv")
)


# 10. Save Prepared Data --------------------------------------------------
write_csv(
  students,
  file.path(output_data_dir, "student-mat_cleaned.csv")
)

# Save an RDS version to preserve R data types and factor levels.
saveRDS(
  students,
  file.path(output_data_dir, "student-mat_cleaned.rds")
)


# 11. Final Report --------------------------------------------------------
cat("\n========== DATA PREPARATION COMPLETE ==========\n")
cat("Prepared rows:", final_rows, "\n")
cat("Prepared columns:", final_columns, "\n")
cat("Missing values:", final_missing, "\n")
cat("Duplicate rows reviewed:", length(duplicate_index), "\n")
cat("Automatically removed rows: 0\n")

cat("\nSaved files:\n")
cat("- data/processed/student-mat_cleaned.csv\n")
cat("- data/processed/student-mat_cleaned.rds\n")
cat("- outputs/tables/missing_values_before_cleaning.csv\n")
cat("- outputs/tables/duplicate_rows_review.csv\n")
cat("- outputs/tables/numeric_range_validation.csv\n")
cat("- outputs/tables/categorical_values_after_cleaning.csv\n")
cat("- outputs/tables/data_preparation_summary.csv\n")


