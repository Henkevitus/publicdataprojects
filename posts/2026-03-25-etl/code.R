# chunk-generate-data ----------------------------------------------------

packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "lubridate",
  "janitor",
  "arrow",
  "digest",
  "logger",
  "jsonlite",
  "purrr",
  "tibble",
  "here"
)

invisible(lapply(packages, \(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))

set.seed(1)

base_dir <- here::here("posts", "2026-03-25-etl")
raw_dir <- file.path(base_dir, "data", "raw")
staging_dir <- file.path(base_dir, "data", "staging")
warehouse_dir <- file.path(base_dir, "data", "warehouse")

walk(
  c(raw_dir, staging_dir, warehouse_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

patients <- tibble(
  patient_id = sample(c(paste0("P", 1:500), "P10", "P10 "), 800, TRUE),
  sex = sample(c("M", "F", "male", "female", "Unknown", ""), 800, TRUE),
  dob = sample(c("1980-01-01", "01/02/1975", "1970-13-01", "", NA), 800, TRUE),
  region = sample(
    c(
      "Nordjylland",
      "Midtjylland",
      "Syddanmark",
      "Sjælland",
      "Hovedstaden",
      "??"
    ),
    800,
    TRUE
  )
)

visits <- tibble(
  patient_id = sample(c(paste0("P", 1:500), "P999"), 2000, TRUE),
  visit_date = sample(
    c("2025-01-01", "01/02/2025", "2025-02-30", "", NA),
    2000,
    TRUE
  ),
  clinic_code = sample(c("AAH", "AUH", "OUH", "RH", "UNK", ""), 2000, TRUE),
  visit_type = sample(
    c("outpatient", "inpatient", "ER", "OPD", "IPD"),
    2000,
    TRUE
  )
)

labs <- tibble(
  patient_id = sample(c(paste0("P", 1:500), "P888"), 4000, TRUE),
  sample_date = sample(c("2025-01-05", "05/01/2025", "", NA), 4000, TRUE),
  test = sample(c("hba1c", "HbA1c", "creatinine", "eGFR"), 4000, TRUE),
  value = rnorm(4000, 50, 20),
  unit = sample(c("mmol/mol", "%", "umol/L", "mL/min/1.73m2", ""), 4000, TRUE)
)

write_csv(patients, file.path(raw_dir, "patients.csv"))
write_csv(visits, file.path(raw_dir, "visits.csv"))
write_csv(labs, file.path(raw_dir, "labs.csv"))

# chunk-helpers ----------------------------------------------------------

valid_regions <- c(
  "Nordjylland",
  "Midtjylland",
  "Syddanmark",
  "Sjælland",
  "Hovedstaden"
)
valid_clinics <- c("AAH", "AUH", "OUH", "RH")

missing_id <- function(x) is.na(x) | x == ""

valid_lab_unit <- function(test, unit) {
  case_when(
    test == "hba1c" ~ unit %in% c("%", "mmol/mol"),
    test == "egfr" ~ unit == "mL/min/1.73m2",
    test == "creatinine" ~ unit == "umol/L",
    TRUE ~ FALSE
  )
}

init_logger <- function(run_id, log_dir = "logs") {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_appender(appender_tee(file.path(log_dir, paste0("etl_", run_id, ".log"))))
  log_layout(layout_simple)
  log_threshold(INFO)
}

hash_id <- function(x) {
  vapply(as.character(x), digest, character(1), algo = "xxhash64")
}

parse_date_safe <- function(x) {
  suppressWarnings(as.Date(parse_date_time(
    x,
    orders = c("Y-m-d", "d/m/Y", "d-m-Y")
  )))
}

assert <- function(condition, msg) {
  if (!isTRUE(condition)) stop(msg, call. = FALSE)
}

write_quality_report <- function(path, report_list) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(toJSON(report_list, pretty = TRUE, auto_unbox = TRUE), path)
}

make_quarantine <- function(df, reason, stage, run_id) {
  df |>
    mutate(
      quarantine_reason = reason,
      quarantine_stage = stage,
      run_id = run_id,
      quarantine_time_utc = format(Sys.time(), tz = "UTC")
    )
}

bind_quarantine <- function(...) {
  qs <- list(...)
  qs <- qs[lengths(qs) > 0]
  if (length(qs) == 0) tibble() else bind_rows(qs)
}

# chunk-extract-raw ------------------------------------------------------

extract_raw <- function() {
  log_info("Extract: reading raw files from {raw_dir}")

  patients <- read_csv(
    file.path(raw_dir, "patients.csv"),
    show_col_types = FALSE
  ) |>
    clean_names()
  visits <- read_csv(
    file.path(raw_dir, "visits.csv"),
    show_col_types = FALSE
  ) |>
    clean_names()
  labs <- read_csv(file.path(raw_dir, "labs.csv"), show_col_types = FALSE) |>
    clean_names()

  log_info("Extract: writing staging Parquet")
  write_parquet(patients, file.path(staging_dir, "patients.parquet"))
  write_parquet(visits, file.path(staging_dir, "visits.parquet"))
  write_parquet(labs, file.path(staging_dir, "labs.parquet"))

  list(
    patients_n = nrow(patients),
    visits_n = nrow(visits),
    labs_n = nrow(labs)
  )
}

# chunk-validate ---------------------------------------------------------

validate_raw <- function() {
  patients <- read_parquet(file.path(staging_dir, "patients.parquet"))
  visits <- read_parquet(file.path(staging_dir, "visits.parquet"))
  labs <- read_parquet(file.path(staging_dir, "labs.parquet"))

  assert("patient_id" %in% names(patients), "patients missing patient_id")
  assert(
    all(c("patient_id", "visit_date") %in% names(visits)),
    "visits missing required columns"
  )
  assert(
    all(c("patient_id", "sample_date", "test", "value") %in% names(labs)),
    "labs missing required columns"
  )

  report <- list(
    patients = list(
      n = nrow(patients),
      missing_patient_id = sum(missing_id(str_trim(patients$patient_id))),
      duplicate_patient_id_rows = sum(duplicated(str_trim(patients$patient_id)))
    ),
    visits = list(
      n = nrow(visits),
      missing_patient_id = sum(missing_id(str_trim(visits$patient_id))),
      missing_visit_date = sum(missing_id(str_trim(visits$visit_date))),
      unknown_clinic_code = sum(!(visits$clinic_code %in% valid_clinics))
    ),
    labs = list(
      n = nrow(labs),
      missing_patient_id = sum(missing_id(str_trim(labs$patient_id))),
      missing_sample_date = sum(missing_id(str_trim(labs$sample_date))),
      weird_units = sum(
        !(labs$unit %in% c("mmol/mol", "%", "umol/L", "mL/min/1.73m2", ""))
      )
    )
  )

  log_info("Validate raw: {toJSON(report, auto_unbox = TRUE)}")
  report
}

# chunk-transform_to_warehouse -------------------------------------------

transform_to_warehouse <- function(run_id) {
  patients_raw <- read_parquet(file.path(staging_dir, "patients.parquet"))
  visits_raw <- read_parquet(file.path(staging_dir, "visits.parquet"))
  labs_raw <- read_parquet(file.path(staging_dir, "labs.parquet"))

  # PATIENTS
  patients_std <- patients_raw |>
    mutate(
      across(c(patient_id, sex, region), str_trim),
      patient_id_raw = patient_id,
      sex_raw = sex,
      dob_raw = dob,
      region_raw = region,
      patient_id = str_to_upper(patient_id),
      sex = case_when(
        str_to_lower(sex) %in% c("m", "male") ~ "M",
        str_to_lower(sex) %in% c("f", "female") ~ "F",
        TRUE ~ NA_character_
      ),
      dob = parse_date_safe(dob),
      region = if_else(region %in% valid_regions, region, NA_character_)
    )

  q_pat_missing_id <- patients_std |>
    filter(missing_id(patient_id)) |>
    make_quarantine("Missing or blank patient_id", "transform_patients", run_id)

  patients_ranked <- patients_std |>
    filter(!missing_id(patient_id)) |>
    mutate(
      original_row = row_number(),
      completeness = rowSums(!is.na(across(c(sex, dob, region)))),
      has_dob = !is.na(dob),
      has_sex = !is.na(sex),
      has_region = !is.na(region)
    ) |>
    group_by(patient_id) |>
    arrange(
      desc(completeness),
      desc(has_dob),
      desc(has_sex),
      desc(has_region),
      original_row,
      .by_group = TRUE
    ) |>
    mutate(row_rank_within_patient = row_number()) |>
    ungroup()

  q_pat_duplicate <- patients_ranked |>
    filter(row_rank_within_patient > 1) |>
    make_quarantine(
      "Duplicate patient row not chosen as canonical record",
      "transform_patients",
      run_id
    )

  dim_patient <- patients_ranked |>
    filter(row_rank_within_patient == 1) |>
    select(patient_id, sex, dob, region) |>
    mutate(patient_sk = hash_id(patient_id), .before = patient_id)

  quarantine_patient <- bind_quarantine(q_pat_missing_id, q_pat_duplicate)

  # VISITS
  visits_std <- visits_raw |>
    mutate(
      across(c(patient_id, visit_date, clinic_code, visit_type), str_trim),
      patient_id_raw = patient_id,
      visit_date_raw = visit_date,
      clinic_code_raw = clinic_code,
      visit_type_raw = visit_type,
      patient_id = str_to_upper(patient_id),
      visit_date = parse_date_safe(visit_date),
      clinic_code = if_else(
        str_to_upper(clinic_code) %in% valid_clinics,
        str_to_upper(clinic_code),
        NA_character_
      ),
      visit_type = case_when(
        str_to_lower(visit_type) %in% c("outpatient", "opd") ~ "outpatient",
        str_to_lower(visit_type) %in% c("inpatient", "ipd") ~ "inpatient",
        str_to_lower(visit_type) == "er" ~ "er",
        TRUE ~ NA_character_
      )
    )

  q_visit_missing_id <- visits_std |>
    filter(missing_id(patient_id)) |>
    make_quarantine("Missing or blank patient_id", "transform_visits", run_id)

  q_visit_missing_date <- visits_std |>
    filter(!missing_id(patient_id), is.na(visit_date)) |>
    make_quarantine("Missing or invalid visit_date", "transform_visits", run_id)

  visits_ok <- visits_std |>
    filter(!missing_id(patient_id), !is.na(visit_date)) |>
    transmute(
      patient_sk = hash_id(patient_id),
      patient_id,
      visit_date,
      clinic_code,
      visit_type
    )

  quarantine_visit <- bind_quarantine(q_visit_missing_id, q_visit_missing_date)

  # LABS
  labs_std <- labs_raw |>
    mutate(
      across(c(patient_id, sample_date, test, unit), str_trim),
      patient_id_raw = patient_id,
      sample_date_raw = sample_date,
      test_raw = test,
      unit_raw = unit,
      patient_id = str_to_upper(patient_id),
      sample_date = parse_date_safe(sample_date),
      test = case_when(
        str_to_lower(test) %in% c("hba1c", "hb a1c", "hb_a1c") ~ "hba1c",
        str_to_lower(test) == "creatinine" ~ "creatinine",
        str_to_lower(test) == "egfr" ~ "egfr",
        TRUE ~ NA_character_
      )
    )

  q_lab_missing_id <- labs_std |>
    filter(missing_id(patient_id)) |>
    make_quarantine("Missing or blank patient_id", "transform_labs", run_id)

  q_lab_missing_date <- labs_std |>
    filter(!missing_id(patient_id), is.na(sample_date)) |>
    make_quarantine("Missing or invalid sample_date", "transform_labs", run_id)

  q_lab_unknown_test <- labs_std |>
    filter(!missing_id(patient_id), !is.na(sample_date), is.na(test)) |>
    make_quarantine("Unknown or unmapped lab test", "transform_labs", run_id)

  q_lab_wrong_unit <- labs_std |>
    filter(
      !missing_id(patient_id),
      !is.na(sample_date),
      !is.na(test),
      !valid_lab_unit(test, unit)
    ) |>
    make_quarantine("Wrong unit for the test", "transform_labs", run_id)

  labs_ok <- labs_std |>
    filter(
      !missing_id(patient_id),
      !is.na(sample_date),
      !is.na(test),
      valid_lab_unit(test, unit)
    ) |>
    transmute(
      patient_sk = hash_id(patient_id),
      patient_id,
      sample_date,
      test,
      value,
      unit
    )

  quarantine_lab <- bind_quarantine(
    q_lab_missing_id,
    q_lab_missing_date,
    q_lab_unknown_test,
    q_lab_wrong_unit
  )

  list(
    dim_patient = dim_patient,
    fact_visit = visits_ok,
    fact_lab = labs_ok,
    quarantine_patient = quarantine_patient,
    quarantine_visit = quarantine_visit,
    quarantine_lab = quarantine_lab
  )
}

# chunk-validate-curated -------------------------------------------------

validate_curated <- function(dim_patient, fact_visit, fact_lab, run_id) {
  report <- list(
    dim_patient = list(
      n = nrow(dim_patient),
      patient_id_unique = n_distinct(dim_patient$patient_id),
      patient_sk_unique = n_distinct(dim_patient$patient_sk),
      dup_patient_id = sum(duplicated(dim_patient$patient_id)),
      dup_patient_sk = sum(duplicated(dim_patient$patient_sk))
    )
  )

  assert(
    report$dim_patient$dup_patient_id == 0,
    "dim_patient has duplicate patient_id"
  )
  assert(
    report$dim_patient$dup_patient_sk == 0,
    "dim_patient has duplicate patient_sk"
  )

  bad_visits_fk <- anti_join(fact_visit, dim_patient, by = "patient_sk") |>
    make_quarantine(
      "Visit row has patient_sk not found in dim_patient",
      "validate_curated",
      run_id
    )

  bad_labs_fk <- anti_join(fact_lab, dim_patient, by = "patient_sk") |>
    make_quarantine(
      "Lab row has patient_sk not found in dim_patient",
      "validate_curated",
      run_id
    )

  fact_visit_ok <- semi_join(fact_visit, dim_patient, by = "patient_sk")
  fact_lab_ok <- semi_join(fact_lab, dim_patient, by = "patient_sk") |>
    mutate(lab_row_id = row_number(), unit = str_trim(unit))

  hba1c_bad <- fact_lab_ok |>
    filter(
      test == "hba1c",
      (unit == "mmol/mol" & (value < 10 | value > 200)) |
        (unit == "%" & (value < 2 | value > 16)) |
        is.na(unit)
    ) |>
    make_quarantine(
      "HbA1c outside plausible range for stated unit",
      "validate_curated",
      run_id
    )

  fact_lab_ok <- fact_lab_ok |>
    anti_join(select(hba1c_bad, lab_row_id), by = "lab_row_id")

  report$referential_integrity <- list(
    visits_missing_dim = nrow(bad_visits_fk),
    labs_missing_dim = nrow(bad_labs_fk)
  )

  report$plausibility <- list(
    hba1c_out_of_range = nrow(hba1c_bad)
  )

  list(
    report = report,
    fact_visit = fact_visit_ok,
    fact_lab = fact_lab_ok,
    quarantine_visit_curated = bad_visits_fk,
    quarantine_lab_curated = bind_quarantine(bad_labs_fk, hba1c_bad)
  )
}

# chunk-load -------------------------------------------------------------

load_warehouse <- function(
  dim_patient,
  fact_visit,
  fact_lab,
  quarantine_patient,
  quarantine_visit,
  quarantine_lab,
  run_id
) {
  walk2(
    list(
      dim_patient,
      fact_visit,
      fact_lab,
      quarantine_patient,
      quarantine_visit,
      quarantine_lab
    ),
    file.path(
      warehouse_dir,
      c(
        "dim_patient.parquet",
        "fact_visit.parquet",
        "fact_lab.parquet",
        "quarantine_patient.parquet",
        "quarantine_visit.parquet",
        "quarantine_lab.parquet"
      )
    ),
    write_parquet
  )

  run_log_path <- file.path(warehouse_dir, "etl_run_log.csv")

  run_row <- tibble(
    run_id = run_id,
    run_time_utc = format(Sys.time(), tz = "UTC"),
    dim_patient_n = nrow(dim_patient),
    fact_visit_n = nrow(fact_visit),
    fact_lab_n = nrow(fact_lab),
    quarantine_patient_n = nrow(quarantine_patient),
    quarantine_visit_n = nrow(quarantine_visit),
    quarantine_lab_n = nrow(quarantine_lab)
  )

  if (!file.exists(run_log_path)) {
    write_csv(run_row, run_log_path)
  } else {
    old_log <- read_csv(
      run_log_path,
      show_col_types = FALSE,
      col_types = cols(
        run_id = col_character(),
        run_time_utc = col_character(),
        dim_patient_n = col_double(),
        fact_visit_n = col_double(),
        fact_lab_n = col_double(),
        quarantine_patient_n = col_double(),
        quarantine_visit_n = col_double(),
        quarantine_lab_n = col_double()
      )
    )
    write_csv(bind_rows(old_log, run_row), run_log_path)
  }

  log_info("Load complete: wrote warehouse and quarantine tables")
}

# chunk-run --------------------------------------------------------------

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
init_logger(run_id)

log_info("ETL start, run_id={run_id}")

extract_stats <- extract_raw()
raw_report <- validate_raw()
curated <- transform_to_warehouse(run_id)

curated_checked <- validate_curated(
  dim_patient = curated$dim_patient,
  fact_visit = curated$fact_visit,
  fact_lab = curated$fact_lab,
  run_id = run_id
)

quarantine_visit_all <- bind_quarantine(
  curated$quarantine_visit,
  curated_checked$quarantine_visit_curated
)
quarantine_lab_all <- bind_quarantine(
  curated$quarantine_lab,
  curated_checked$quarantine_lab_curated
)

write_quality_report(
  path = file.path(warehouse_dir, paste0("quality_report_", run_id, ".json")),
  report_list = list(
    extract = extract_stats,
    raw = raw_report,
    curated = curated_checked$report,
    quarantine_counts = list(
      patient = nrow(curated$quarantine_patient),
      visit = nrow(quarantine_visit_all),
      lab = nrow(quarantine_lab_all)
    )
  )
)

load_warehouse(
  dim_patient = curated$dim_patient,
  fact_visit = curated_checked$fact_visit,
  fact_lab = curated_checked$fact_lab,
  quarantine_patient = curated$quarantine_patient,
  quarantine_visit = quarantine_visit_all,
  quarantine_lab = quarantine_lab_all,
  run_id = run_id
)

log_info("ETL done, run_id={run_id}")
