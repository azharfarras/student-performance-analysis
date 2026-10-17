library(tidyverse)

# 1. Load Dataset ---------------------------------------------------------

data_path <- "data/student-mat.csv"

students <- read_delim(data_path, 
                       delim = ";", 
                       show_col_types = FALSE)



# 2. Dataset overview -----------------------------------------------------

cat(" ======= DATASET OVERVIEW ============\n")
cat("Datset: Student Performance - Mathematics\n")
cat("Source: UCI Machine Learning Repository \n")
cat("Observations:", nrow(students), "\n")
cat("Variables:", ncol(students), "\n\n")


# 3. Insppect te dataset --------------------------------------------------

cat("======= DATA STRUCTURE =========\n")
glimpse(students)

cat("======= HEAD ROWS =========\n")
print(head(students))

cat("======= VARIABLES NAMES =========\n")
print(names(students))


# 4. Identify variable types ----------------------------------------------

variable_overview <- tibble(
  variable = names(students),
  data_type = map_chr(students, ~paste(class(.x), collapse = ", "))
)

print(variable_overview, n = Inf)


# 5. Review key variables -------------------------------------------------

cat("\n ========= KEY VARIABLES ========\n")
students |> 
  select(G3, sex, studytime, absences, failures, G1, G2) |>
  summary() |>
  print()


# 6. Explore outcome distribution -----------------------------------------
cat("\n =========== FINAL GRADE DISTRIBUTION ============= \n")

print(students |>
        count(G3, name = "frequency") |>
        arrange(G3))

# 7. Explore categorical variables ----------------------------------------
cat("\n =========== SEX DISTRIBUTION ============= \n")
print(count(students, sex))

cat("\n =========== STUDY TIME DISTRIBUTION ============= \n")
print(count(students, studytime))

cat("\n =========== PREVIOUS DISTRIBUTION ============= \n")
print(count(students, failures))

# 8. Initial descriptive statistics ---------------------------------------
cat("\n =========== DESCRIPITVE STATISTICS ============= \n")
students |>
  summarise(
    mean_G3 = mean(G3, na.rm = T),
    median_G3 = median(G3, na.rm = T),
    sd_G3 = sd(G3, na.rm = T),
    min_G3 = min(G3, na.rm = T),
    max_G3 = max(G3, na.rm = T),
    mean_absences = mean(absences, na.rm = T),
    median_absences = median(absences, na.rm = T)
  ) |>
  print()

# 9. Initial visualization ------------------------------------------------
grade_plot <- ggplot(students, aes(X=G3)) +
  geom_histogram(
    binwidth = 1,
    #boundary - -0.5,
    color = "white"
  ) +
  scale_x_continuous(breaks = 0:20) +
  labs(
    title = "Distribution of Final Mathematics Grades",
    x = "Ginal Grade (G3)",
    y = "Number of Students"
  ) +
  theme_minimal()

print(grade_plot)

# 10. Save initial vizualization ------------------------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

ggsave(
  "outputs/figures/final_grade_distribution.png",
  plot = grade_plot,
  width = 8,
  height = 5,
  dpi = 300
)

