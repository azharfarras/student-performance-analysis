# 1. Configuration --------------------------------------------------------

data_path <- "data/processed/student-mat_cleaned.rds"

figure_dir <- "outputs/figures"
table_dir <- "outputs/tables"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

students <- readRDS(data_path)

cat("\n========== EDA: DATA OVERVIEW ==========\n")
cat("Observations:", nrow(students), "\n")
cat("Variables:", ncol(students), "\n")


# 2. Theme Portofolio -----------------------------------------------------

theme_portofolio <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      color = "#243447"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = #596579
    ),
    axis.title = element_text(
      face = "bold",
      color = "#243447"
    ),
    axis.text = element_text(color = "#465366"),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "bottom"
  )


# 3. Descriptive Statistics -----------------------------------------------
# Ringkasan numerik cepat
numeric_summary <- skimr::skim(students)

# Simpan ke CSV
write_csv(
  as.data.frame(numeric_summary),
  file.path(table_dir, "eda_numeric_summary.csv"))


# 4. Distribution of Final Grades (G3) ------------------------------------
grade_summary <- students %>%
  summarise(
    n = sum(!is.na(G3)),
    mean = mean(G3, na.rm = TRUE),
    median = median(G3, na.rm = TRUE),
    sd = sd(G3, na.rm = TRUE),
    min = min(G3, na.rm = TRUE),
    max = max(G3, na.rm = TRUE)
  )

write_csv(
  grade_summary,
  file.path(table_dir, "eda_final_grade_summary.csv")
)

p_grade_distribution <- ggplot(
  students,
  aes(x = G3)
) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    color = "white",
    fill = "#4776A8",
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = mean(students$G3, na.rm = TRUE),
    linetype = "dashed",
    linewidth = 0.8,
    color = "#D95F59"
  ) +
  labs(
    title = "Distribution of Final Mathematics Grades",
    subtitle = "Dashed line indicates the sample mean",
    x = "Final Grade (G3)",
    y = "Number of Students",
    caption = "Source: UCI Student Performance Dataset"
  ) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme_portofolio


# 5. Final Grades by Sex --------------------------------------------------
sex_summary <- students |>
  group_by(sex) |>
  summarise(
    n = sum(!is.na(G3)),
    mena_grade = mean(G3, na.rm = TRUE),
    median_grade = median(G3, na.rm = TRUE),
    sd_grade = sd(G3, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(sex_summary, file.path(table_dir, "eda_grade_by_sex.csv"))

p_grade_by_sex <- ggplot(students, aes(x = sex, y = G3, fill = sex)) +
  geom_boxplot(
    alpha = 0.8,
    width = 0.55,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.5,
    na.rm = TRUE
  ) +
  labs(
    title = "Final Mathematics Grades by Sex",
    subtitle = "Points represent individual students",
    x = "Sex",
    y = "Final Grade (G3)",
    fill = "sex",
    caption = "Source: UCI Student Performance Dataset"
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    limits = c(0, 20)
  ) +
  theme_portofolio +
  theme(legend.position = "none")

ggsave(file.path(figure_dir, "02_grade_by_sex.png"),
       p_grade_by_sex,
       width = 9, height = 6, dpi = 300)


# 6. Study Time and Final Grades ---------------------------------------------
studytime_summary <- students |>
  group_by(studytime, .drop = FALSE) |>
  summarise(
    n = sum(!is.na(G3)),
    mean_grade = mean(G3, na.rm = TRUE),
    median_grade = median(G3, na.rm = TRUE),
    sd_grade = sd(G3, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  studytime_summary,
  file.path(table_dir, "eda_grade_by_studytime.csv")
)

p_grade_by_studytime <- ggplot(
  students, aes(x = studytime, y = G3)
) +
  geom_boxplot(
    fill = "65A69A",
    alpha = 0.8,
    width = 0.6,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.3,
    size = 1.3,
    na.rm = TRUE
  ) +
  labs(
    title = "Final Grades by Weekly Study Time",
    subtitle = "Study time is grouped into four reported categoris",
    x = "Weekly study time",
    y = "Final Grade (G3)",
    caption = "Source: UCI Student Performance Dataset"
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    limits = c(0,20)
  ) +
  theme_portofolio +
  theme(axis.text.x = element_text(
    angle = 15,
    hjust = 1
  ))

ggsave(
  file.path(figure_dir, "03_grade_by_studytime.png"),
  p_grade_by_studytime,
  width = 10,
  height = 6,
  dpi = 300
)


# 7. Absences and Final Grades --------------------------------------------
absence_summary <- students |> 
  summarise (
    n = sum(!is.na(absences) & !is.na(G3)),
    mean_absences = mean(absences, na.rm = TRUE),
    median_absences = median(absences, na.rm = TRUE),
    mean_grade = mean(G3, na.rm = TRUE),
    median_grade = median(G3, na.rm = TRUE)
  )

write_csv(
  absence_summary,
  file.path(table_dir, "eda_absence_summary.csv")
)

correlation_absences <- tibble(
  method = c("Pearson", "Spearman"),
  correlation = c(
    cor(students$absences,
        students$G3,
        use = "complete.obs",
        method = "pearson"
    ),
    cor(
      students$absences,
      students$G3,
      use = "complete.obs",
      method = "spearman"
    )
  )
)

write_csv(
  correlation_absences,
  file.path(table_dir, "eda_absences_grade_correlation.csv")
)

p_absences_grade <- ggplot(
  students, aes(x = absences, y = G3)
) +
  geom_jitter(
    width = 0.15,
    height = 0.12,
    alpha = 0.35,
    color = "#4776A8",
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#D95F59",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  labs(
    title = "Absences and Final Mathematics Grades",
    subtitle = "The line summarizes the linear trend; it does not imply causation",
    x = "Number of Absences",
    y = "Final Grade (G3)",
    caption = "Source: UCI Student Performance Dataset"
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    limits = c(0,20)
  ) +
  theme_portofolio

ggsave(
  file.path(figure_dir, "04_absences_vs_final_grade.png"),
  p_absences_grade,
  width = 9,
  height = 6,
  dpi = 300
)


# 8. Correlation Among Academic Grades ------------------------------------
grade_variables <- students |>
  select(G1, G2, G3)

grade_correlation <- cor(grade_variables,
                         use = "pairwise.complete.obs",
                         method = "pearson")

correlation_table <- as.data.frame(
  as.table(grade_correlation)
) |>
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    correlation = Freq
  )

write_csv(
  correlation_table,
  file.path(table_dir, "eda_grade_correlation_matrix.csv")
)

correlation_long <- as.data.frame(
  as.table(grade_correlation)
) |> 
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    correlation = Freq
  )

p_grade_correlation <- ggplot(
  correlation_long, 
  aes(x = variable_1, y = variable_2, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    color = "#242447",
    size = 5
  ) +
  scale_fill_gradient2(
    low = "#4776A8",
    mid = "white",
    high = "#D95F59",
    midpoint = 0,
    limits = c(-1,1)
  ) +
  coord_equal() +
  labs (
    title = "Correlation Among Academic Grades",
    subtitle = "Pearson correlation coeeficients",
    x = NULL,
    y = NULL,
    fill = "Correlation",
    caption = "source: UCI Student Performance Dataset"
  ) +
  theme_portofolio

ggsave(
  file.path(figure_dir, "05_academic_grade_correlation.png"),
  p_grade_correlation,
  width = 8,
  height = 6,
  dpi = 300
)

# 9. Final EDA Summary ----------------------------------------------------
eda_summary <- tibble(
  metric = c(
    "Number of students",
    "Number of variables",
    "Mean final grade",
    "Median final grade",
    "Standard deviation of final grade",
    "Mean absences",
    "Median absences",
    "Pearson correlation: absences vs G3",
    "Spearman correlation: absences vs G3"
  ),
  value = c(
    nrow(students),
    ncol(students),
    mean(students$G3, na.rm = TRUE),
    median(students$G3, na.rm = TRUE),
    sd(students$G3, na.rm = TRUE),
    mean(students$absences, na.rm = TRUE),
    median(students$absences, na.rm = TRUE),
    correlation_absences$correlation[
      correlation_absences$method == "Pearson"
    ],
    correlation_absences$correlation[
      correlation_absences$method == "Spearman"
    ]
  )
)

write_csv(
  eda_summary,
  file.path(table_dir, "eda_overall_summary.csv")
)

