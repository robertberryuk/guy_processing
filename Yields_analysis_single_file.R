# Libraries
library(tidyverse)
library(here)



# Load data
yields <- read_csv(here("In", "Test_sample", "yields_maize.csv"))


# Select the 5 most recent years from each country
yields_recent <- yields %>%
  group_by(Entity) %>%
  arrange(desc(Year)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  group_by(Entity) %>%
  arrange(Year) %>%  # re-order to go from oldest to newest
  mutate(YearLabel = paste0("Y", row_number())) %>%
  ungroup()


# Pivot to wide format using dynamic Y1–Y5
yields_wide <- yields_recent %>%
  select(Entity, YearLabel, Yield) %>%
  pivot_wider(names_from = YearLabel, values_from = Yield) %>%
  drop_na() %>%
  arrange(Entity)


# Computed column: Aggregate production over 5 most recent years
yields_wide <- yields_wide %>%
  mutate(agg_prod = Y1 + Y2 + Y3 + Y4 + Y5)


# Computed column: Average yearly production over 5 most recent years
yields_wide <- yields_wide %>%
  mutate(avg_prod = agg_prod / 5)


# Computed column: Rank based on "agg_prod"
yields_wide <- yields_wide %>%
  mutate(rank = rank(-agg_prod, ties.method = "first")) 


# Computed columns: Year-on-year change
yields_wide <- yields_wide %>%
  mutate(
    diff_y1_y2 = Y2 - Y1,
    diff_y2_y3 = Y3 - Y2,
    diff_y3_y4 = Y4 - Y3,
    diff_y4_y5 = Y5 - Y4
  )


# Computed column: Total difference in production for the Y1-Y5 period
yields_wide <- yields_wide %>%
  mutate(total_diff = Y5 - Y1)



# Computed column: Year-on-year change
yields_wide <- yields_wide %>%
  mutate(pct_var = 100 * (Y5 - Y1) / Y1)


# Computed column: Variance based on total diff betweens Y5 and Y1
yields_wide <- yields_wide %>%
  mutate(pc_variance = (Y5 - Y1) / Y1)

# Computed column: Normalised scores (0-1) based on pc_variance column
yields_wide <- yields_wide %>%
  mutate(norm_score_var = (pc_variance - min(pc_variance)) / (max(pc_variance) - min(pc_variance)))

# Computed column: Normalised scores (0-1) based on pc_variance column - reverse of norm_score
yields_wide <- yields_wide %>%
  mutate(norm_score_var_reverse = (max(pc_variance) - pc_variance) / (max(pc_variance) - min(pc_variance)))


# Computed column: Normalised scores (0-1) based on avg_prod column
yields_wide <- yields_wide %>%
  mutate(norm_score_avg = (avg_prod - min(avg_prod)) / (max(avg_prod) - min(avg_prod)))



# Export the final wide data frame to a CSV file
write_csv(yields_wide, here("Out", "yields_computed_test.csv"))









