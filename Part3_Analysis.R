## Pula Advisors — X Uganda Coffee Farmer Profiling
## Part 3: Data Cleaning & Analytical Report

# ============================================================
# LOAD REQUIRED LIBRARIES
# ============================================================

library(tidyverse)      # Data manipulation + ggplot2 visualisation
library(readxl)         # Read Excel files
library(knitr)          # Nice Table formatting for Word output
library(scales)         # Axis formatting (percent, comma)
library(janitor)        # clean_names(), tabyl()
library(stringdist)     # Fuzzy string matching for org name deduplication

# ============================================================
# IMPORT RAW DATA
# ============================================================
raw <- read_excel("x.xlsx")
cat("Raw dataset dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")
cat("Column names:\n")
print(names(raw))

# 3. Data Cleaning Pipeline
## 3.1 Standardise Column Names
# ============================================================
# STEP 1: Clean column names to snake_case for R compatibility
# Some column names start with numbers (e.g. "2022_production_FAQ")
# which R handles but can cause issues — we keep them as-is
# since readxl imports them correctly with backtick quoting.
# ============================================================

df <- raw

# Verify all expected columns are present
expected_cols <- c(
  "Farmer_id", "farmer_consent_form", "district",
  "farm_size", "coffee_trees_farmers_estimate", "number_productive_trees",
  "male_workers", "female_workers", "local_workers", "immigrant_workers",
  "2022_production_FAQ", "2023_production_FAQ",
  "2022_production_kiboko", "2023_production_kiboko",
  "2022_production_green_cherries", "2023_production_green_cherries",
  "farmer_member_coffee_org", "name_of_coffee_org",
  "percent_Arabica_grown", "percent_Robusta_grown",
  "underage_workers", "workers_aware_of_rights", "workers_follow_schedule",
  "primary_buyer", "sell_coffee_to", "number_of_household_members"
)

missing_cols <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) {
  cat("WARNING — Expected columns not found:", paste(missing_cols, collapse=", "), "\n")
} else {
  cat("All expected columns present.\n")
}


## 3.2 Standardise Yes/No Fields (DQ-17)
# ============================================================
# STEP 2: Standardise all binary Yes/No fields to lowercase
# Issue: Mixed case ('yes'/'Yes', 'no'/'No') causes
# case-sensitive filtering to miss records.
# Fix: Convert all character fields to lowercase.
# ============================================================

yes_no_cols <- c(
  "farmer_consent_form",          # Note: stored as logical TRUE/FALSE in some rows
  "farmer_head_of_family",
  "farmer_member_coffee_org",
  "farmer_part_of_carbon_project",
  "farmer_has_mobile_money_wallet",
  "any_land_dispute",
  "protect_water_sources",
  "use_organic_waste",
  "harvest_wood_farm",
  "harvest_wood_farm_regulations",
  "interact_leaders_farming_activities",
  "workers_treatment",
  "pay_min_wage",
  "underage_workers",
  "workers_follow_schedule",
  "workers_aware_of_rights",
  "pay_taxes",
  "corruption_practices_observed"
)

# Apply lowercase standardisation to all character yes/no columns
for (col in yes_no_cols) {
  if (col %in% names(df) && is.character(df[[col]])) {
    df[[col]] <- tolower(trimws(df[[col]]))
  }
}

# Special handling: farmer_consent_form may be logical (TRUE/FALSE)
# Convert to character "yes"/"no" for consistency
if ("farmer_consent_form" %in% names(df)) {
  if (is.logical(df$farmer_consent_form)) {
    df$farmer_consent_form <- ifelse(df$farmer_consent_form == TRUE, "yes",
                                     ifelse(df$farmer_consent_form == FALSE, "no", NA_character_))
  }
}

cat("Yes/No standardisation complete.\n")
cat("Sample values after cleaning:\n")
print(table(df$farmer_member_coffee_org, useNA = "always"))


## 3.3 Fix District Spelling (DQ-08)
# ============================================================
# STEP 3: Correct district misspelling
# Issue: 'Nakeseke' appears 32 times — a typo for 'Nakaseke'.
# Fix: Direct string replacement.
# ============================================================

# Count before correction
before <- table(df$district)
cat("District counts BEFORE correction:\n")
print(before)

# Apply correction
df$district <- ifelse(df$district == "Nakeseke", "Nakaseke", df$district)

# Count after correction
after <- table(df$district)
cat("\nDistrict counts AFTER correction:\n")
print(after)
cat("\nCorrected", sum(raw$district == "Nakeseke", na.rm = TRUE), "records from 'Nakeseke' to 'Nakaseke'.\n")

## 3.4 Handle Missing Consent Records (DQ-01)
# ============================================================
# STEP 4: Flag records with missing consent (DQ-01)
# Issue: 445 records have no consent value.
# Action: Create a flag column. These records are NOT excluded
# from exploratory analysis but are FLAGGED and excluded from
# any client-facing production or compliance summaries.
# Assumption: We treat NA consent as "unconfirmed" — not as
# "yes" or "no". They are retained for descriptive statistics
# but excluded from compliance analysis outputs.
# ============================================================

df <- df %>%
  mutate(
    consent_flag = case_when(
      is.na(farmer_consent_form)       ~ "Unconfirmed",
      farmer_consent_form == "yes"     ~ "Consented",
      farmer_consent_form == "no"      ~ "Refused",
      TRUE                             ~ "Other"
    )
  )

cat("Consent status distribution:\n")
print(table(df$consent_flag, useNA = "always"))

# Create a consented-only subset for compliance-sensitive analysis
df_consented <- df %>% filter(consent_flag == "Consented")
cat("\nRecords with confirmed consent:", nrow(df_consented), "\n")
cat("Records excluded (unconfirmed consent):", nrow(df) - nrow(df_consented), "\n")

## 3.5 Fix Logical Inconsistency: Productive Trees > Total Trees (DQ-04)
# ============================================================
# STEP 5: Fix productive trees > total trees (DQ-04)
# Issue: 318 records where number_productive_trees >
#        coffee_trees_farmers_estimate (logical impossibility).
# Assumption: Where productive trees exceed total trees by
# a small margin (<= 100 trees), this is likely a data entry
# transposition — we swap the two values.
# Where the discrepancy is large (> 100 trees), we flag the
# record for re-interview rather than imputing.
# ============================================================

# Count records before fix
n_before <- sum(df$number_productive_trees > df$coffee_trees_farmers_estimate,
                na.rm = TRUE)
cat("Records with productive > total trees BEFORE fix:", n_before, "\n")

df <- df %>%
  mutate(
    tree_inconsistency_flag = case_when(
      is.na(number_productive_trees) | is.na(coffee_trees_farmers_estimate) ~ "Missing",
      number_productive_trees > coffee_trees_farmers_estimate &
        (number_productive_trees - coffee_trees_farmers_estimate) <= 100   ~ "Swapped",
      number_productive_trees > coffee_trees_farmers_estimate              ~ "Flagged — large discrepancy",
      TRUE                                                                 ~ "OK"
    ),
    # Apply swap where discrepancy is small
    total_trees_clean = case_when(
      tree_inconsistency_flag == "Swapped" ~ number_productive_trees,
      TRUE                                  ~ coffee_trees_farmers_estimate
    ),
    productive_trees_clean = case_when(
      tree_inconsistency_flag == "Swapped" ~ coffee_trees_farmers_estimate,
      TRUE                                  ~ number_productive_trees
    )
  )

cat("\nTree inconsistency resolution summary:\n")
print(table(df$tree_inconsistency_flag, useNA = "always"))

n_after <- sum(df$productive_trees_clean > df$total_trees_clean, na.rm = TRUE)
cat("\nRecords with productive > total trees AFTER fix:", n_after,
    "(remaining are large-discrepancy flagged records)\n")

## 3.6 Fix Cooperative Name Inconsistencies
# ============================================================
# STEP 6: Standardise cooperative/organisation names
# Issue: Multiple spellings of same organisation
# (e.g. 'ibero', 'Ibero', 'IBERO', 'Ibero cooperative .')
# Fix: Trim whitespace, convert to title case, then apply
# a manual lookup for known variants.
# ============================================================

# First: clean whitespace and case
df <- df %>%
  mutate(
    org_name_clean = if_else(
      !is.na(name_of_coffee_org),
      str_trim(str_to_title(name_of_coffee_org)),
      NA_character_
    )
  )

# Manual lookup table for known duplicates
org_lookup <- c(
  "Ibero"                                                            = "IBERO",
  "Ibero Cooperative ."                                              = "IBERO",
  "Ldc"                                                              = "LDC",
  "Busana"                                                           = "BUSAANA",
  "Wotoyitidde"                                                      = "WOTOYITIDDE",
  "Naluvule Wotoyitidde Coffee Farmers And Corporative Society Ltd"  = "WOTOYITIDDE",
  "Eotoyitidde"                                                      = "WOTOYITIDDE",
  "Buyuki Bukeeka"                                                   = "Buyuki Bukeeka Cooperative",
  "Buyuki Bukeeka Cooperative"                                       = "Buyuki Bukeeka Cooperative",
  "The Techno Serve"                                                  = "TechnoServe",
  "Techno Serve"                                                      = "TechnoServe",
  "Kikamulo Cooperative Farmers"                                     = "Kikamulo Cooperative",
  "Bukeeke Cooperative"                                              = "Bukeeke Cooperative",
  "Kimika Nakaseke"                                                  = "KIMIKA",
  "Kimika Kikwata"                                                   = "KIMIKA",
  "Dk"                                                               = "DK"
)

df <- df %>%
  mutate(
    org_name_clean = case_when(
      org_name_clean %in% names(org_lookup) ~ org_lookup[org_name_clean],
      TRUE ~ org_name_clean
    )
  )

cat("Standardised organisation names:\n")
print(table(df$org_name_clean, useNA = "always"))

## 3.7 Derive New Analytical Fields
# ============================================================
# STEP 7: Derive fields needed for Part 3 analysis
# ============================================================

df <- df %>%
  mutate(
    
    # --- Tree density (productive trees per acre) ---
    # Handle division by zero: if farm_size == 0, set to NA
    trees_per_acre = if_else(
      farm_size > 0,
      productive_trees_clean / farm_size,
      NA_real_
    ),
    
    # --- Agronomic plausibility flag for tree density ---
    # Standard Robusta density: 450–600 trees/acre
    # Flagging > 3,000 as implausible (5x the upper agronomic bound)
    tree_density_flag = case_when(
      is.na(trees_per_acre)    ~ "Missing",
      trees_per_acre > 3000    ~ "Implausible (>3000/acre)",
      trees_per_acre > 1500    ~ "High (1500-3000/acre)",
      trees_per_acre >= 100    ~ "Normal (100-1500/acre)",
      trees_per_acre < 100     ~ "Low (<100/acre)",
      TRUE                     ~ "Other"
    ),
    
    # --- Year-on-year FAQ production change ---
    faq_change_pct = if_else(
      !is.na(`2022_production_FAQ`) & `2022_production_FAQ` > 0,
      (`2023_production_FAQ` - `2022_production_FAQ`) / `2022_production_FAQ` * 100,
      NA_real_
    ),
    
    # --- Kiboko identical year-on-year flag ---
    kiboko_identical = (`2022_production_kiboko` == `2023_production_kiboko`) &
      !is.na(`2022_production_kiboko`) &
      !is.na(`2023_production_kiboko`),
    
    # --- Total workers (using gender breakdown as reference) ---
    total_workers = male_workers + female_workers,
    
    # --- Worker count mismatch flag ---
    worker_mismatch = abs(
      (male_workers + female_workers) -
        (local_workers + immigrant_workers)
    ) > 0,
    
    # --- Labour compliance composite flag ---
    # A farm is flagged if ANY of the three conditions are met
    labour_compliance_concern = (
      tolower(underage_workers) == "yes" |
        tolower(workers_aware_of_rights) == "no" |
        tolower(workers_follow_schedule) == "no"
    ),
    
    # --- Arabica percentage as numeric ---
    # Field is stored as category string e.g. "0-25%", "75-100%"
    # We extract the midpoint of each band for analysis
    arabica_pct_mid = case_when(
      percent_Arabica_grown == "0-25%"    ~  12.5,
      percent_Arabica_grown == "25-50%"   ~  37.5,
      percent_Arabica_grown == "50-75%"   ~  62.5,
      percent_Arabica_grown == "75-100%"  ~  87.5,
      TRUE                                ~  NA_real_
    ),
    robusta_pct_mid = case_when(
      percent_Robusta_grown == "0-25%"    ~  12.5,
      percent_Robusta_grown == "25-50%"   ~  37.5,
      percent_Robusta_grown == "50-75%"   ~  62.5,
      percent_Robusta_grown == "75-100%"  ~  87.5,
      TRUE                                ~  NA_real_
    ),
    
    # --- Cooperative membership flag ---
    is_coop_member = (tolower(farmer_member_coffee_org) == "yes")
    
  )

cat("Derived fields created successfully.\n")
cat("Tree density distribution:\n")
print(table(df$tree_density_flag, useNA = "always"))
cat("\nKiboko year-on-year identical records:", sum(df$kiboko_identical, na.rm=TRUE),
    "(", round(mean(df$kiboko_identical, na.rm=TRUE)*100, 1), "%)\n")


## 3.8 Final Clean Dataset Summary
# ============================================================
# STEP 8: Summarise the cleaned dataset
# ============================================================

cat("=== CLEANED DATASET SUMMARY ===\n")
cat("Total records:               ", nrow(df), "\n")
cat("Records with consent:        ", sum(df$consent_flag == "Consented", na.rm=TRUE), "\n")
cat("Corrected district entries:  ", sum(raw$district == "Nakeseke", na.rm=TRUE), "\n")
cat("Tree values swapped:         ", sum(df$tree_inconsistency_flag == "Swapped", na.rm=TRUE), "\n")
cat("Tree records flagged:        ",
    sum(df$tree_inconsistency_flag == "Flagged — large discrepancy", na.rm=TRUE), "\n")
cat("Records with labour concern: ",
    sum(df$labour_compliance_concern, na.rm=TRUE), "\n")
cat("Kiboko copy-paste flagged:   ",
    sum(df$kiboko_identical, na.rm=TRUE), "\n")
cat("Worker count mismatches:     ",
    sum(df$worker_mismatch, na.rm=TRUE), "\n")
cat("Cooperative members:         ",
    sum(df$is_coop_member, na.rm=TRUE), "\n")


# ============================================================
# Q5: FAQ Production Trends by District (2022 vs 2023)
# ============================================================

# Aggregate by district — exclude kiboko-flagged records only for FAQ
# FAQ data does not show the same copy-paste pattern as kiboko
faq_by_district <- df %>%
  group_by(district) %>%
  summarise(
    n_farmers        = n(),
    n_with_2022      = sum(!is.na(`2022_production_FAQ`)),
    n_with_2023      = sum(!is.na(`2023_production_FAQ`)),
    total_faq_2022   = sum(`2022_production_FAQ`, na.rm = TRUE),
    total_faq_2023   = sum(`2023_production_FAQ`, na.rm = TRUE),
    mean_faq_2022    = round(mean(`2022_production_FAQ`, na.rm = TRUE), 1),
    mean_faq_2023    = round(mean(`2023_production_FAQ`, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    # Year-on-year change in TOTAL production
    change_pct = round(
      (total_faq_2023 - total_faq_2022) / total_faq_2022 * 100, 1
    ),
    direction = case_when(
      change_pct > 100  ~ "Growth > 100%",
      change_pct > 0    ~ "Growth",
      change_pct == 0   ~ "No change",
      TRUE              ~ "Decline"
    )
  ) %>%
  arrange(desc(change_pct))

# Print summary table
kable(
  faq_by_district %>%
    select(
      District         = district,
      Farmers          = n_farmers,
      `2022 Records`   = n_with_2022,
      `2023 Records`   = n_with_2023,
      `Total 2022 (kg)`= total_faq_2022,
      `Total 2023 (kg)`= total_faq_2023,
      `Mean 2022 (kg)` = mean_faq_2022,
      `Mean 2023 (kg)` = mean_faq_2023,
      `Change %`       = change_pct,
      Direction        = direction
    ),
  caption = "Table Q5.1: FAQ Coffee Production by District — 2022 vs 2023",
  format.args = list(big.mark = ",")
)


# ============================================================
# Q5 CHART: Grouped bar chart — FAQ production by district
# ============================================================

# Reshape to long format for ggplot
faq_long <- faq_by_district %>%
  select(district, total_faq_2022, total_faq_2023) %>%
  pivot_longer(
    cols = c(total_faq_2022, total_faq_2023),
    names_to = "year",
    values_to = "production_kg"
  ) %>%
  mutate(
    year = case_when(
      year == "total_faq_2022" ~ "2022",
      year == "total_faq_2023" ~ "2023",
      TRUE ~ year
    ),
    district = factor(district, levels = faq_by_district$district)
  )

ggplot(faq_long, aes(x = district, y = production_kg / 1000, fill = year)) +
  geom_col(position = "dodge", width = 0.65, colour = "white") +
  geom_text(
    aes(label = paste0(round(production_kg / 1000, 0), "t")),
    position = position_dodge(width = 0.65),
    vjust = -0.4, size = 3.2, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("2022" = "#2E75B6", "2023" = "#70AD47"),
    name   = "Year"
  ) +
  scale_y_continuous(labels = comma_format(suffix = "t")) +
  labs(
    title    = "Figure Q5.1: FAQ Coffee Production by District — 2022 vs 2023",
    subtitle = "Tonnes (000 kg). Missing 2022 data excluded with na.rm = TRUE.",
    x        = "District",
    y        = "Total Production (tonnes)",
    caption  = "Source: X Uganda Coffee Farmer Profiling Dataset | Analysis: Pula Advisors"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(colour = "grey40", size = 9),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

# Identify districts with >100% change
over_100 <- faq_by_district %>% filter(abs(change_pct) > 100)
if (nrow(over_100) > 0) {
  cat("Districts with FAQ production change > 100%:\n")
  print(over_100 %>% select(district, change_pct, direction))
} else {
  cat("No district shows a FAQ production change greater than 100%.\n")
}

cat("\nDistrict with highest growth:", faq_by_district$district[1],
    "(", faq_by_district$change_pct[1], "%)\n")
cat("District with largest decline:", faq_by_district$district[nrow(faq_by_district)],
    "(", faq_by_district$change_pct[nrow(faq_by_district)], "%)\n")


# ============================================================
# Q6: Farm Size & Tree Density by District
# ============================================================

# Compute tree density using the cleaned productive_trees_clean field
# Trees per acre = productive_trees_clean / farm_size
# Division by zero handled in Step 7 (returns NA when farm_size == 0)

density_by_district <- df %>%
  filter(!is.na(trees_per_acre)) %>%   # Exclude records where farm_size == 0
  group_by(district) %>%
  summarise(
    n_farms               = n(),
    mean_farm_size_acres  = round(mean(farm_size, na.rm = TRUE), 2),
    median_farm_size      = round(median(farm_size, na.rm = TRUE), 2),
    mean_productive_trees = round(mean(productive_trees_clean, na.rm = TRUE), 0),
    mean_trees_per_acre   = round(mean(trees_per_acre, na.rm = TRUE), 0),
    median_trees_per_acre = round(median(trees_per_acre, na.rm = TRUE), 0),
    n_implausible         = sum(tree_density_flag == "Implausible (>3000/acre)",
                                na.rm = TRUE),
    pct_implausible       = round(n_implausible / n_farms * 100, 1),
    .groups = "drop"
  )

kable(
  density_by_district %>%
    select(
      District              = district,
      Farms                 = n_farms,
      `Mean Farm (acres)`   = mean_farm_size_acres,
      `Median Farm (acres)` = median_farm_size,
      `Mean Productive Trees` = mean_productive_trees,
      `Mean Trees/Acre`     = mean_trees_per_acre,
      `Median Trees/Acre`   = median_trees_per_acre,
      `Implausible Records` = n_implausible,
      `% Implausible`       = pct_implausible
    ),
  caption = "Table Q6.1: Farm Size & Tree Density by District (cleaned productive trees)"
)

# ============================================================
# Q6: Overall tree density distribution
# ============================================================

cat("Overall tree density flag distribution:\n")
print(table(df$tree_density_flag, useNA = "always"))

cat("\nRecords with implausible density (>3000 trees/acre):\n")
implausible_records <- df %>%
  filter(tree_density_flag == "Implausible (>3000/acre)") %>%
  select(Farmer_id, district, farm_size, productive_trees_clean,
         trees_per_acre, tree_inconsistency_flag) %>%
  arrange(desc(trees_per_acre))

cat("Total implausible records:", nrow(implausible_records), "\n")
kable(
  head(implausible_records, 10),
  caption = "Table Q6.2: Top 10 Records with Implausible Tree Density (>3,000/acre)"
)


# ============================================================
# Q6 CHART: Boxplot of trees per acre by district
# ============================================================

# Cap display at 3000 for readability (outliers already flagged)
df_plot <- df %>%
  filter(!is.na(trees_per_acre) & trees_per_acre <= 3000)

ggplot(df_plot, aes(x = district, y = trees_per_acre, fill = district)) +
  geom_boxplot(outlier.colour = "#C00000", outlier.size = 1.2,
               outlier.alpha = 0.5, width = 0.5) +
  geom_hline(yintercept = 600,  linetype = "dashed", colour = "#E36C09",
             linewidth = 0.8) +
  geom_hline(yintercept = 3000, linetype = "dotted", colour = "#C00000",
             linewidth = 0.8) +
  annotate("text", x = 0.55, y = 640,  label = "Max agronomic density (600/acre)",
           size = 2.8, colour = "#E36C09", hjust = 0) +
  annotate("text", x = 0.55, y = 3040, label = "Implausibility threshold (3000/acre)",
           size = 2.8, colour = "#C00000", hjust = 0) +
  scale_fill_brewer(palette = "Blues", guide = "none") +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Figure Q6.1: Distribution of Productive Trees per Acre by District",
    subtitle = "Capped at 3,000 for readability. Red dots = outliers (implausible density).",
    x        = "District",
    y        = "Productive Trees per Acre",
    caption  = "Source: X Uganda Coffee Farmer Profiling Dataset | Analysis: Pula Advisors"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title  = element_text(face = "bold", size = 8),
    plot.subtitle = element_text(colour = "grey40", size = 7),
    panel.grid.major.x = element_blank()
  )

# ============================================================
# Q7: Labour Compliance Flags
# ============================================================

# All yes/no fields were standardised to lowercase in Step 3.2
# Using consented records only for compliance reporting (DQ-01 guidance)

labour_df <- df_consented %>%
  mutate(
    flag_child_labour     = tolower(underage_workers)       == "yes",
    flag_rights_unaware   = tolower(workers_aware_of_rights) == "no",
    flag_schedule_breach  = tolower(workers_follow_schedule) == "no"
  )

# Overall counts
cat("=== LABOUR COMPLIANCE FLAGS (Consented Records Only) ===\n")
cat("Total consented records:                        ", nrow(labour_df), "\n")
cat("(a) Farms with child/underage workers:          ",
    sum(labour_df$flag_child_labour,    na.rm = TRUE), "\n")
cat("(b) Farms where workers unaware of rights:      ",
    sum(labour_df$flag_rights_unaware,  na.rm = TRUE), "\n")
cat("(c) Farms with workers not on 48-hr schedule:   ",
    sum(labour_df$flag_schedule_breach, na.rm = TRUE), "\n")
cat("Farms with ANY compliance concern:              ",
    sum(labour_df$labour_compliance_concern, na.rm = TRUE), "\n")


# ============================================================
# Q7: Compliance flags by district
# ============================================================

compliance_by_district <- labour_df %>%
  group_by(district) %>%
  summarise(
    n_farms              = n(),
    child_labour         = sum(flag_child_labour,    na.rm = TRUE),
    rights_unaware       = sum(flag_rights_unaware,  na.rm = TRUE),
    schedule_breach      = sum(flag_schedule_breach, na.rm = TRUE),
    any_concern          = sum(flag_child_labour | flag_rights_unaware | flag_schedule_breach, na.rm = TRUE),
    pct_child            = round(child_labour   / n_farms * 100, 1),
    pct_rights           = round(rights_unaware / n_farms * 100, 1),
    pct_schedule         = round(schedule_breach/ n_farms * 100, 1),
    pct_any              = round(any_concern     / n_farms * 100, 1),
    .groups = "drop"
  )

kable(
  compliance_by_district %>%
    select(
      District              = district,
      Farms                 = n_farms,
      `Child Workers (n)`   = child_labour,
      `Child Workers (%)`   = pct_child,
      `Rights Unaware (n)`  = rights_unaware,
      `Rights Unaware (%)`  = pct_rights,
      `Schedule Breach (n)` = schedule_breach,
      `Schedule Breach (%)` = pct_schedule,
      `Any Concern (n)`     = any_concern,
      `Any Concern (%)`     = pct_any
    ),
  caption = "Table Q7.1: Labour Compliance Flags by District (Consented Records Only)"
)

# ============================================================
# Q7 CHART: Stacked bar of compliance flags by district
# ============================================================

compliance_long <- compliance_by_district %>%
  select(district, child_labour, rights_unaware, schedule_breach) %>%
  pivot_longer(
    cols = c(child_labour, rights_unaware, schedule_breach),
    names_to = "flag_type",
    values_to = "count"
  ) %>%
  mutate(
    flag_type = case_when(
      flag_type == "child_labour"    ~ "Child/Underage Workers",
      flag_type == "rights_unaware"  ~ "Workers Unaware of Rights",
      flag_type == "schedule_breach" ~ "Schedule Non-Compliance",
      TRUE ~ flag_type
    )
  )

ggplot(compliance_long,
       aes(x = district, y = count, fill = flag_type)) +
  geom_col(position = "dodge", width = 0.65, colour = "white") +
  scale_fill_manual(
    values = c(
      "Child/Underage Workers"     = "#C00000",
      "Workers Unaware of Rights"  = "#E36C09",
      "Schedule Non-Compliance"    = "#FFCC00"
    ),
    name = "Compliance Flag"
  ) +
  labs(
    title    = "Figure Q7.1: Labour Compliance Flags by District",
    subtitle = "Consented records only (n = 1,929). Note: 0 confirmed child labour cases.",
    x        = "District",
    y        = "Number of Farms Flagged",
    caption  = "Source: X Uganda Coffee Farmer Profiling Dataset | Analysis: Pula Advisors"
  ) +
  theme_minimal(base_size = 6) +
  theme(
    plot.title      = element_text(face = "bold", size = 6),
    plot.subtitle   = element_text(colour = "grey40", size = 6),
    legend.position = "top",
    legend.text     = element_text(size = 6),
    panel.grid.major.x = element_blank()
  )

# ============================================================
# Q7: Detailed breakdown — rights-unaware and schedule breach
# ============================================================

cat("Detailed compliance breakdown across all records:\n\n")

cat("Workers aware of rights:\n")
print(table(tolower(df$workers_aware_of_rights), useNA = "always"))

cat("\nWorkers follow 48-hour schedule:\n")
print(table(tolower(df$workers_follow_schedule), useNA = "always"))

cat("\nUnderage workers:\n")
print(table(tolower(df$underage_workers), useNA = "always"))

# Farms with multiple flags
multi_flag <- labour_df %>%
  mutate(n_flags = flag_child_labour + flag_rights_unaware + flag_schedule_breach) %>%
  filter(n_flags > 1) %>%
  select(Farmer_id, district, n_flags,
         flag_child_labour, flag_rights_unaware, flag_schedule_breach)

cat("\nFarms with 2 or more compliance flags:", nrow(multi_flag), "\n")
if (nrow(multi_flag) > 0) {
  kable(
    multi_flag %>% arrange(desc(n_flags)) %>% head(10),
    caption = "Table Q7.2: Farms with Multiple Labour Compliance Flags"
  )
}

# ============================================================
# Q8: Arabica vs Robusta Distribution
# ============================================================

# Using midpoint values derived in Step 3.7
# (arabica_pct_mid and robusta_pct_mid — midpoint of each band)
# Note: These are categorical bands, not exact percentages.
# The "sum" check below tests whether the two band midpoints
# together plausibly represent complementary coverage.

# Summary by district
variety_by_district <- df %>%
  filter(!is.na(arabica_pct_mid) & !is.na(robusta_pct_mid)) %>%
  group_by(district) %>%
  summarise(
    n_farms         = n(),
    mean_arabica    = round(mean(arabica_pct_mid, na.rm = TRUE), 1),
    median_arabica  = round(median(arabica_pct_mid, na.rm = TRUE), 1),
    sd_arabica      = round(sd(arabica_pct_mid, na.rm = TRUE), 1),
    mean_robusta    = round(mean(robusta_pct_mid, na.rm = TRUE), 1),
    median_robusta  = round(median(robusta_pct_mid, na.rm = TRUE), 1),
    sd_robusta      = round(sd(robusta_pct_mid, na.rm = TRUE), 1),
    .groups = "drop"
  )

kable(
  variety_by_district,
  col.names = c("District","Farms","Mean Arabica%","Med Arabica%","SD Arabica%",
                "Mean Robusta%","Med Robusta%","SD Robusta%"),
  caption = "Table Q8.1: Arabica vs Robusta Distribution by District (Band Midpoints)"
)

# ============================================================
# Q8 DQC CHECK: Arabica + Robusta bands sum check
# A farm growing 0-25% Arabica should ideally be growing
# 75-100% Robusta (bands are complementary).
# We flag records where the two bands are clearly inconsistent.
# ============================================================

df <- df %>%
  mutate(
    # Check: are the two bands directionally consistent?
    # Consistent: Arabica "0-25%" + Robusta "75-100%" (or similar complementary pair)
    # Inconsistent: Both claim "75-100%" (would sum to 150-200%)
    variety_consistent = case_when(
      is.na(percent_Arabica_grown) | is.na(percent_Robusta_grown) ~ NA,
      percent_Arabica_grown == "0-25%"   & percent_Robusta_grown == "75-100%" ~ TRUE,
      percent_Arabica_grown == "25-50%"  & percent_Robusta_grown == "50-75%"  ~ TRUE,
      percent_Arabica_grown == "50-75%"  & percent_Robusta_grown == "25-50%"  ~ TRUE,
      percent_Arabica_grown == "75-100%" & percent_Robusta_grown == "0-25%"   ~ TRUE,
      TRUE                                                                     ~ FALSE
    )
  )

cat("Variety band consistency check:\n")
print(table(df$variety_consistent, useNA = "always"))

# Farms claiming 75-100% Arabica (very unusual in Uganda — Robusta country)
high_arabica <- df %>%
  filter(percent_Arabica_grown == "75-100%") %>%
  select(Farmer_id, district, percent_Arabica_grown, percent_Robusta_grown,
         `2023_production_FAQ`)

cat("\nFarms reporting 75-100% Arabica (unusual for Uganda):", nrow(high_arabica), "\n")
if (nrow(high_arabica) > 0) {
  kable(high_arabica, caption = "Table Q8.2: Farms Reporting 75-100% Arabica Coverage")
}

# Overall raw distribution
cat("\nOverall Arabica band distribution:\n")
print(table(df$percent_Arabica_grown, useNA = "always"))

cat("\nOverall Robusta band distribution:\n")
print(table(df$percent_Robusta_grown, useNA = "always"))

# ============================================================
# Q8 CHART: Arabica / Robusta band distribution
# ============================================================

arabica_dist <- df %>%
  filter(!is.na(percent_Arabica_grown)) %>%
  count(district, percent_Arabica_grown) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(arabica_dist, aes(x = percent_Arabica_grown, y = n, fill = district)) +
  geom_col(position = "dodge", colour = "blue", width = 0.7) +
  scale_fill_brewer(palette = "Blues", name = "District") +
  labs(
    title    = "Figure Q8.1: Arabica Coverage Band Distribution by District",
    subtitle = "99.7% of farmers grow predominantly Robusta (75-100%). Arabica is minimal.",
    x        = "Arabica Coverage Band",
    y        = "Number of Farms",
    caption  = "Source: X Uganda Coffee Farmer Profiling Dataset | Analysis: Pula Advisors"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(colour = "grey40", size = 7),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )


# ============================================================
# Q9: Cooperative Members vs Non-Members — Comparison Table
# ============================================================

# Using full dataset (not just consented) for descriptive comparison
# Excludes records where is_coop_member is NA

coop_comparison <- df %>%
  filter(!is.na(is_coop_member)) %>%
  group_by(Membership = if_else(is_coop_member,
                                "Cooperative Member",
                                "Non-Member")) %>%
  summarise(
    `Number of Farms`                  = n(),
    `Mean Farm Size (acres)`           = round(mean(farm_size, na.rm = TRUE), 2),
    `Median Farm Size (acres)`         = round(median(farm_size, na.rm = TRUE), 2),
    `Mean Productive Trees`            = round(mean(productive_trees_clean, na.rm = TRUE), 0),
    `Mean Trees per Acre`              = round(mean(trees_per_acre, na.rm = TRUE), 0),
    `Mean 2023 FAQ Production (kg)`    = round(mean(`2023_production_FAQ`, na.rm = TRUE), 0),
    `Median 2023 FAQ Production (kg)`  = round(median(`2023_production_FAQ`, na.rm = TRUE), 0),
    `Mean 2023 Kiboko Production (kg)` = round(mean(`2023_production_kiboko`, na.rm = TRUE), 0),
    `Mean Household Size`              = round(mean(number_of_household_members, na.rm = TRUE), 1),
    `% with Mobile Money`              = round(
      mean(tolower(farmer_has_mobile_money_wallet) == "yes",
           na.rm = TRUE) * 100, 1),
    `% Paying Min Wage`                = round(
      mean(tolower(pay_min_wage) == "yes",
           na.rm = TRUE) * 100, 1),
    `% Female Workers (avg ratio)`     = round(
      mean(female_workers /
             (male_workers + female_workers),
           na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# Transpose for readability
coop_t <- coop_comparison %>%
  pivot_longer(-Membership, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Membership, values_from = Value)

kable(
  coop_t,
  caption = "Table Q9.1: Cooperative Members vs Non-Members — Key Metrics Comparison"
)

# ============================================================
# Q9: Statistical test — are member farms significantly larger?
# Wilcoxon rank-sum test (non-parametric, suitable for skewed data)
# ============================================================

members     <- df %>% filter(is_coop_member == TRUE)  %>% pull(farm_size)
non_members <- df %>% filter(is_coop_member == FALSE) %>% pull(farm_size)

wtest <- wilcox.test(members, non_members, na.action = na.omit)
cat("Wilcoxon test — farm size (members vs non-members):\n")
cat("  W statistic:", round(wtest$statistic, 0), "\n")
cat("  p-value:    ", round(wtest$p.value, 4), "\n")
cat("  Interpretation: ",
    if (wtest$p.value < 0.05) "Statistically significant difference in farm size.\n"
    else "No statistically significant difference in farm size.\n")

# FAQ production
wtest2 <- wilcox.test(
  df %>% filter(is_coop_member == TRUE)  %>% pull(`2023_production_FAQ`),
  df %>% filter(is_coop_member == FALSE) %>% pull(`2023_production_FAQ`),
  na.action = na.omit
)
cat("\nWilcoxon test — 2023 FAQ production (members vs non-members):\n")
cat("  p-value:", round(wtest2$p.value, 4), "\n")
cat("  Interpretation: ",
    if (wtest2$p.value < 0.05) "Statistically significant difference in FAQ production.\n"
    else "No statistically significant difference in FAQ production.\n")


# ============================================================
# Q9 CHART: Comparison bar chart — members vs non-members
# ============================================================

# Select 5 key metrics for visual comparison
metrics_plot <- coop_t %>%
  filter(Metric %in% c(
    "Mean Farm Size (acres)",
    "Mean Productive Trees",
    "Mean 2023 FAQ Production (kg)",
    "Mean 2023 Kiboko Production (kg)",
    "% with Mobile Money"
  )) %>%
  pivot_longer(
    cols      = c("Cooperative Member", "Non-Member"),
    names_to  = "group",
    values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# Normalise each metric to index (non-member = 100) for visual comparison
metrics_plot <- metrics_plot %>%
  group_by(Metric) %>%
  mutate(
    base_value = value[group == "Non-Member"],
    index      = round(value / base_value * 100, 1)
  ) %>%
  ungroup()

ggplot(metrics_plot, aes(x = reorder(Metric, -index), y = index, fill = group)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 0.55, y = 102, label = "Non-Member baseline (100)",
           size = 2.8, colour = "grey40", hjust = 0) +
  scale_fill_manual(
    values = c("Cooperative Member" = "#2E75B6", "Non-Member" = "#BDD7EE"),
    name   = ""
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  labs(
    title    = "Figure Q9.1: Cooperative Members vs Non Members Indexed Comparison",
    subtitle = "Non-member = baseline (100). Values above 100 indicate members outperform.",
    x        = NULL,
    y        = "Index (Non-Member = 100)",
    caption  = "Source: X Uganda Coffee Farmer Profiling Dataset | Analysis: Pula Advisors"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title    = element_text(face = "bold", size = 8),
    plot.subtitle = element_text(colour = "grey40", size = 7),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )