## ---- config_template.R ----
PI10_ROOT <- "PATH/TO/PI10"

DATA_SORTED_DIR <- file.path(PI10_ROOT, "data", "sorted")
VALIDATION_DIR  <- file.path(PI10_ROOT, "Validations")
DEPENDENCIES_DIR <- file.path(PI10_ROOT, "_dependancies")

TAXONOMY_FILE <- file.path(DEPENDENCIES_DIR, "taxonomy", "taxonomy.csv")
EXAMPLE_LABEL_DIR <- file.path(DEPENDENCIES_DIR, "example_labeling")

dataset_id <- "YYYYMMDD-HHMM"

PROCESSED_OUTPUT_DIR <- "PATH/TO/PROCESSED"
