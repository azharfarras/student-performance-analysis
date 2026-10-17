library(tidyverse)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)


# 01. Load dataset --------------------------------------------------------
data_path <- "data/student-mat.csv"
output_dir <- "outputs/tables"

students <- read_delim(data_path, 
                       delim = ";", 
                       show_col_types = FALSE)


# 02. General data quality overview ---------------------------------------
quality_overview <- tibble(
  metric = c(
    "Number of rows",
    "Number of columns",
    "Total missing values",
    "Total exact duplicate rows"
  ),
  value = c(
    nrow(students),
    ncol(students),
    sum(is.na(students)),
    sum(duplicated(students))
  )
)

print(quality_overview)

write_csv(
  quality_overview,
  file.path(output_dir, "quality_overview.csv")
)


# 03. Missing value assesment ---------------------------------------------
missing_summary <- tibble(
  variable = names(students),
  missing_count = map_int(
    students, ~sum(is.na(.x))
  ),
  missing_percentage = map_dbl(
    students, ~mean(is.na(.x))*100
  )
) |> arrange(desc(missing_percentage))

print(missing_summary, n = Inf)

write_csv(missing_summary, file.path(output_dir, "missing_value_summary.csv"))

# 04. Duplicate row assesment ---------------------------------------------
duplicate_rows <- students |>
  mutate(
    .row_id = row_number(),
    .is_duplicate = duplicated(
      select(students, everything())
    )
  ) |> filter(.is_duplicate)

cat(
  "\nExact duplicate rows after the first occurence:",
  nrow(duplicate_rows),"\n"
)

write_csv(duplicate_rows, file.path(output_dir, "duplicate_rows.csv"))

# 05. Data type assesment -------------------------------------------------
data_type_summary <- tibble(
  variable = names(students),
  data_type = map_chr(
    students, ~paste(class(.x), collapse = ",")
  ),
  unique_values = map_int(
    students, ~ n_distinct(.x, na.rm = TRUE)
  )
)

print(data_type_summary, n = Inf)

write_csv(data_type_summary, file.path(output_dir, "data_type_summary.csv"))

# 06. Validation key variable ranges --------------------------------------
range_checks <- tibble(
  variable = c(
    "G1", "G2", "G3", "studytime", "absences", "failures"
  ),
  expected_range = c(
    "0 to 20", "0 to 20", "0 to 20", "1 to 4",
    "Non-negative", "Non-negative"
  ),
  violations = c(
    sum(students$G1 < 0 | students$G1 > 20, na.rm = TRUE),
    sum(students$G2 < 0 | students$G2 > 20, na.rm = TRUE),
    sum(students$G3 < 0 | students$G3 > 20, na.rm = TRUE),
    sum(students$studytime < 1 | students$studytime > 4, na.rm = TRUE),
    sum(students$absences < 0, na.rm = TRUE),
    sum(students$failures < 0, na.rm = TRUE)
  )
)

print(range_checks)

write_csv(range_checks, file.path(output_dir, "range_validation.csv"))

# 07. Categorical value assesment -----------------------------------------
categorical_variables <- c("school", "sex", "address", "famsize",
                           "Pstatus", "schoolsup", "famsup", "paid",
                           "activities", "nursery", "higher", "internet",
                           "romantic")

categorical_summary <- map_dfr(
  categorical_variables,
  function(variable_name) {
    students |> count(category = .data[[variable_name]],
                      name = "frequency") |>
      mutate(variable = variable_name, .before = 1)
  }
)

print(categorical_summary, n = Inf)

write_csv(categorical_summary, file.path(output_dir = "categorical_value_summary.csv"))

# 08. Logical consistency checks ------------------------------------------
logical_checks <- tibble(
  check = c(
    "G1 greater than G2",
    "G2 greater than G3",
    "G3 equal to zero"
  ),
  count = c(
    sum(students$G1 > students$G2, na.rm = TRUE),
    sum(students$G2 > students$G3, na.rm = TRUE),
    sum(students$G3 == 0, na.rm = TRUE)
  ),
  interpretation = c(
    "Review only; grades can decrease",
    "Review only; grades can decrease",
    "Valid grade value; investigate context"
  )
)

print(logical_checks)

# 09. Data quality assessment status --------------------------------------
quality_status <- tibble(
  check = c(
    "Missing values",
    "Exact duplicate rows",
    "Numeric range violations"
  ),
  issue_count = c(
    sum(is.na(students)),
    sum(duplicated(students)),
    sum(range_checks$violations)
  )
) |> mutate(
  status = if_else(
    issue_count == 0,
    "No issue detected by this check",
    "Requires review"
  )
)

print(quality_status)

write_csv(quality_status,
          file.path(output_dir, "quality_status.csv"))

