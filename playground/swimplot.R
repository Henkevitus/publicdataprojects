# ============================================================
# 0. Setup
# ============================================================
set.seed(123)

library(dplyr)
library(purrr)
library(ggplot2)
library(forcats)

# ============================================================
# 1. Create dummy data
#    A: treatment states, years 2015–2025
#    B: sarcopenia states, years 2009–2025 (some pre-start)
# ============================================================

n_patients <- 12
ids <- 1:n_patients

# For each patient, pick a start year between 2015 and 2018
baseline_years <- sample(2015:2018, n_patients, replace = TRUE)
max_year <- 2025

# --- Dataset A: id, year, state --------------------------------
# One state per year from start to end of follow-up (≤ 2025)
A <- map_dfr(seq_along(ids), function(i) {
  id    <- ids[i]
  start <- baseline_years[i]
  
  # number of years of follow-up after start (at least 1 year)
  possible_max_fu <- max_year - start
  fu_years        <- sample(1:8, 1)
  fu_years        <- min(fu_years, possible_max_fu)
  
  years <- seq(from = start, to = start + fu_years)
  rel   <- seq_along(years) - 1  # 0,1,2,...
  
  state <- case_when(
    rel == 0            ~ "start",
    rel == fu_years     ~ sample(c("lost to followup", "dead"), 1),
    TRUE                ~ "still on treatment"
  )
  
  tibble(
    id    = id,
    year  = years,
    state = state
  )
})

# --- Dataset B: id, year, sarcopenia ----------------------------
# Several sarcopenia measurements per patient, some before start
sarc_levels <- c("none", "probable", "confirmed", "severe")

B <- map_dfr(seq_along(ids), function(i) {
  id   <- ids[i]
  base <- baseline_years[i]
  
  # Choose between 2 and 5 measurement years from (base-6) to (base+5)
  cand_years <- (base - 6):(base + 5)
  years      <- sort(sample(cand_years, size = sample(2:5, 1)))
  
  tibble(
    id         = id,
    year       = years,
    sarcopenia = sample(sarc_levels, length(years), replace = TRUE)
  )
})

# Peek at data
A
print(B,n=nrow(B))

# ============================================================
# 2. Define baseline (start year) and relative time (start = 0)
# ============================================================

baseline <- A %>%
  filter(state == "start") %>%
  group_by(id) %>%
  summarise(baseline_year = min(year), .groups = "drop")

# Attach baseline and create rel_year (year - baseline_year)
A_rel <- A %>%
  inner_join(baseline, by = "id") %>%
  mutate(rel_year = year - baseline_year)

B_rel <- B %>%
  inner_join(baseline, by = "id") %>%
  mutate(rel_year = year - baseline_year)

# ============================================================
# 3. Build state segments from A (only positive follow-up)
#    and order patients by positive follow-up length
# ============================================================

# Keep only years on/after start (rel_year >= 0)
swim_states <- A_rel %>%
  filter(rel_year >= 0) %>%
  mutate(
    x_start = rel_year,
    x_end   = rel_year + 1
  )

# Follow-up length = max rel_year (only positive side counts)
fu_lengths <- swim_states %>%
  group_by(id) %>%
  summarise(fu_max = max(rel_year, na.rm = TRUE), .groups = "drop")

# Order patients by positive follow-up (longest at top)
id_levels <- fu_lengths %>%
  arrange(desc(fu_max)) %>%
  pull(id)

swim_states <- swim_states %>%
  mutate(id_factor = factor(id, levels = id_levels))

B_rel <- B_rel %>%
  mutate(id_factor = factor(id, levels = id_levels))

# ============================================================
# 4. Prepare sarcopenia points (including negative years)
# ============================================================

sarc_points <- B_rel %>%
  filter(!is.na(sarcopenia)) %>%
  mutate(
    # plot in the middle of the year interval
    x_point = rel_year + 0.5
  )

# ============================================================
# 5. Swimmer plot:
#    - segments: treatment states (from 0 onwards)
#    - points: sarcopenia states (negative + positive years)
# ============================================================

swim_states <- swim_states |> mutate(state = factor(state, levels = c("start","still on treatment", "lost to followup", "dead"), ordered = TRUE))

ggplot() +
  # Treatment state segments
  geom_segment(
    data = swim_states,
    aes(
      x    = x_start,
      xend = x_end,
      y    = id_factor,
      yend = id_factor,
      colour = state
    ),
    linewidth = 4,
    lineend = "butt"
  ) +
  # Sarcopenia points
  geom_point(
    data = sarc_points,
    aes(
      x     = x_point,
      y     = id_factor,
      shape = sarcopenia
    ),
    size = 2.5
  ) +
  # Vertical line at time 0 (start of treatment)
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    name   = "Years since treatment start (negative = pre-start)",
    breaks = seq(-6, 10, by = 1)
  ) +
  labs(
    y      = "Patient ID (ordered by follow-up length)",
    colour = "Treatment state",
    shape  = "Sarcopenia state"
  ) +
  theme_minimal()
