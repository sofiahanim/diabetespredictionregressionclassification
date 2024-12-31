library(dplyr)
library(readr)
library(gridExtra)
library(grid)

# Set locale to handle UTF-8 encoding properly
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


main_dir <- "./"
script_dir <- file.path(main_dir, "scripts/cleaning")
processed_data_dir <- file.path(main_dir, "data/processed")
cleaned_data_path <- file.path(processed_data_dir, "cleaned_data.csv")
pdf_output_path <- file.path(script_dir, "cleaning_summary.pdf")
txt_output_path <- file.path(script_dir, "cleaning_output.txt")

if (!dir.exists(processed_data_dir)) {
  dir.create(processed_data_dir, recursive = TRUE)
}

raw_data_path <- file.path(main_dir, "data/raw/diabetes.csv")
data <- read_csv(raw_data_path)

# Start logging the cleaning process to a text file
sink(txt_output_path)
cat("Data Cleaning Process\n")
cat("----------------------\n\n")

# #1: Display the initial structure of the dataset
cat("#1: Initial Dataset Glimpse:\n")
print(glimpse(data)) # Prints the data structure and a preview
cat("\n")

# #2: Replace zeros with NA for columns where zeros are biologically implausible
cat("#2: Replacing implausible zero values with NA for specific columns.\n")
zero_replace_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data <- data %>%
  mutate(across(all_of(zero_replace_cols), ~ifelse(. == 0, NA, .)))
cat("Columns with zeros replaced: ", paste(zero_replace_cols, collapse = ", "), "\n\n")

# #3: Impute missing values with the median for numeric columns
cat("#3: Applying median imputation for missing values.\n")
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
cat("Imputation completed for numeric columns.\n\n")

# #4: Convert incorrect data types for specific columns
cat("#4: Converting incorrect data types.\n")
data <- data %>%
  mutate(
    DiabetesPedigreeFunction = as.numeric(DiabetesPedigreeFunction), # Ensure numeric type
    Outcome = as.factor(Outcome) # Convert Outcome to a factor for classification
  )
cat("Columns converted: DiabetesPedigreeFunction (to numeric), Outcome (to factor).\n\n")

# #5: Categorize Age into meaningful groups
cat("#5: Categorizing 'Age' into age groups.\n")
data <- data %>%
  mutate(AgeGroup = case_when(
    Age < 30 ~ "Under 30",
    Age >= 30 & Age < 50 ~ "30-49",
    Age >= 50 ~ "50 and above"
  ))

cat("Age categorized into groups: 'Under 30', '30-49', '50 and above'.\n\n")

# #6: Relabel Outcome for better understanding
cat("#6: Labeling Outcome as Non-Diabetic (0) and Diabetic (1).\n")
data <- data %>%
  mutate(Outcome = recode(Outcome, `0` = "Non-Diabetic", `1` = "Diabetic"))
cat("Outcome labels updated: 0 -> Non-Diabetic, 1 -> Diabetic.\n\n")

# #7: Save the cleaned dataset to the processed folder
write_csv(data, cleaned_data_path)
cat("#7: Cleaned dataset saved to ", cleaned_data_path, "\n")

# Stop logging to the text file
sink()

# Generate PDF documentation for the cleaning steps
pdf(pdf_output_path, width = 8.5, height = 11)

# Clear and divide text and table sections for proper layout
# Dynamic splitting of text and table into separate pages if needed

# Add cleaning process summary text
grid.newpage()
grid.text(
  "Data Cleaning Process Summary",
  gp = gpar(fontsize = 12, fontface = "bold"),
  y = 0.90
)
grid.text(
  paste(
    "Summary of the cleaning steps performed on the dataset:\n",
    "#1: Initial Dataset Glimpse\n",
    "#2: Replacing implausible zero values with NA for specific columns.\n",
    "#3: Applying median imputation for missing values.\n",
    "#4: Converting incorrect data types.\n",
    "#5: Categorizing 'Age' into age groups.\n",
    "#6: Labeling Outcome as Non-Diabetic (0) and Diabetic (1).\n",
    "#7: Cleaned dataset saved to /data/processed/cleaned_data.csv\n"
  ),
  gp = gpar(fontsize = 10),
  just = "left",
  x = 0.05,
  y = 0.75
)

# Render the table cleanly over multiple pages if it exceeds one
rows_per_page <- 20
total_rows <- nrow(summary_data)
pages <- ceiling(total_rows / rows_per_page)

for (page in 1:pages) {
  grid.newpage()
  start_row <- (page - 1) * rows_per_page + 1
  end_row <- min(page * rows_per_page, total_rows)
  table_chunk <- summary_data[start_row:end_row, , drop = FALSE]
  table_grob <- tableGrob(
    table_chunk,
    rows = NULL,
    theme = ttheme_default(
      core = list(fg_params = list(cex = 0.8)),  # Consistent font size
      colhead = list(fg_params = list(fontface = "bold"))
    )
  )
  grid.text(
    paste("Summary Table of Cleaned Data - Part", page),
    gp = gpar(fontsize = 12, fontface = "bold"),
    y = 0.95
  )
  grid.draw(table_grob)
}


# Close the PDF
dev.off()

# Confirmation of process completion
cat("Cleaning process completed.\nOutputs saved to:\n")
cat("- Cleaned Dataset:", cleaned_data_path, "\n")
cat("- Cleaning Summary PDF:", pdf_output_path, "\n")
cat("- Cleaning Output TXT:", txt_output_path, "\n")
