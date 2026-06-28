#dependencies
install.packages("tidyverse")
install.packages("tidycencus")
install.packages("Hmisc")


#load data if one data set, if two use list list() before readr and keep multiple readr csv location
songkla_data <- readr::read_csv("flood_pdx_sdx_dataset_ht.csv")

#in case of multiple dataset use bind_rows. eg: songkla_data <- songkla_data |>dplyr::dind_rows()

View(songkla_data)

colnames(songkla_data)
#pdx summary 
library(dplyr)

pdx_summary <- songkla_data %>%
  mutate(
    pdx = case_when(
      is.na(pdx) ~ "<NA>",
      trimws(pdx) == "" ~ "<Blank>",
      TRUE ~ pdx
    )
  ) %>%
  count(pdx, sort = TRUE, name = "Frequency") %>%
  rename(ICD10_Code = pdx)

View(pdx_summary)


# Create summary table for maternal ICD-10 groups based on PDX
library(dplyr)
library(stringr)

maternal_summary <- tibble(
  Group = c(
    "Abortion",
    "Pregnancy complications",
    "Maternal care (fetal & placental conditions)",
    "Labour & delivery complications",
    "Normal vaginal delivery",
    "Caesarean delivery",
    "Other delivery",
    "Postpartum (puerperium) complications",
    "Normal pregnancy supervision (ANC)",
    "High-risk pregnancy supervision & antenatal screening"
  ),
  
  `ICD-10 Range` = c(
    "O00–O08",
    "O10–O29",
    "O30–O48",
    "O60–O75",
    "O80",
    "O82",
    "O81, O83–O84",
    "O85–O99",
    "Z34",
    "Z35–Z36"
  ),
  
  Frequency = c(
    sum(str_detect(songkla_data$pdx, "^O0[0-8]")),
    sum(str_detect(songkla_data$pdx, "^O(1[0-9]|2[0-9])")),
    sum(str_detect(songkla_data$pdx, "^O(3[0-9]|4[0-8])")),
    sum(str_detect(songkla_data$pdx, "^O(6[0-9]|7[0-5])")),
    sum(str_detect(songkla_data$pdx, "^O80")),
    sum(str_detect(songkla_data$pdx, "^O82")),
    sum(str_detect(songkla_data$pdx, "^O81|^O83|^O84")),
    sum(str_detect(songkla_data$pdx, "^O(8[5-9]|9[0-9])")),
    sum(str_detect(songkla_data$pdx, "^Z34")),
    sum(str_detect(songkla_data$pdx, "^Z35|^Z36"))
  ),
  
  Includes = c(
    "Ectopic pregnancy, molar pregnancy, spontaneous/induced abortion and complications",
    "Hypertension, diabetes, infections, hyperemesis and other maternal disorders during pregnancy",
    "Multiple pregnancy, fetal abnormalities, placental disorders, malpresentation, post-term pregnancy",
    "Preterm labour, haemorrhage, obstructed labour, obstetric trauma, anaesthesia complications",
    "Spontaneous vaginal delivery",
    "Caesarean section",
    "Instrumental and multiple deliveries",
    "Postpartum infection, venous complications, maternal diseases complicating puerperium",
    "Routine antenatal care (ANC)",
    "High-risk pregnancy supervision and antenatal screening"
  )
)

maternal_summary

# Combine all SDX columns into one vector
library(dplyr)
library(stringr)
library(tidyr)

sdx_long <- songkla_data %>%
  select(starts_with("sdx")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "SDX",
    values_to = "ICD10"
  )

# Create summary table
maternal_summary_sdx <- tibble(
  Group = c(
    "Abortion",
    "Pregnancy complications",
    "Maternal care (fetal & placental conditions)",
    "Labour & delivery complications",
    "Normal vaginal delivery",
    "Caesarean delivery",
    "Other delivery",
    "Postpartum (puerperium) complications",
    "Normal pregnancy supervision (ANC)",
    "High-risk pregnancy supervision & antenatal screening"
  ),
  
  `ICD-10 Range` = c(
    "O00–O08",
    "O10–O29",
    "O30–O48",
    "O60–O75",
    "O80",
    "O82",
    "O81, O83–O84",
    "O85–O99",
    "Z34",
    "Z35–Z36"
  ),
  
  Frequency = c(
    sum(str_detect(sdx_long$ICD10, "^O0[0-8]"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O(1[0-9]|2[0-9])"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O(3[0-9]|4[0-8])"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O(6[0-9]|7[0-5])"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O80"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O82"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O81|^O83|^O84"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^O(8[5-9]|9[0-9])"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^Z34"), na.rm = TRUE),
    sum(str_detect(sdx_long$ICD10, "^Z35|^Z36"), na.rm = TRUE)
  ),
  
  Includes = c(
    "Ectopic pregnancy, molar pregnancy, spontaneous/induced abortion and complications",
    "Hypertension, diabetes, infections, hyperemesis and other maternal disorders during pregnancy",
    "Multiple pregnancy, fetal abnormalities, placental disorders, malpresentation, post-term pregnancy",
    "Preterm labour, haemorrhage, obstructed labour, obstetric trauma, anaesthesia complications",
    "Spontaneous vaginal delivery",
    "Caesarean section",
    "Instrumental and multiple deliveries",
    "Postpartum infection, venous complications, maternal diseases complicating puerperium",
    "Routine antenatal care (ANC)",
    "High-risk pregnancy supervision and antenatal screening"
  )
)

maternal_summary_sdx

# View as spreadsheet
View(maternal_summary_sdx)

#combined table for both pdx and sdx
library(dplyr)
library(tidyr)
library(stringr)
library(gt)
install.packages("gt")

#----------------------------------------------------------
# 1. Define maternal ICD-10 groups
#----------------------------------------------------------
maternal_groups <- tibble(
  Group = c(
    "Normal pregnancy supervision (ANC)",
    "High-risk pregnancy supervision & antenatal screening",
    "Abortion",
    "Pregnancy complications",
    "Maternal care (fetal & placental conditions)",
    "Labour & delivery complications",
    "Normal vaginal delivery",
    "Caesarean delivery",
    "Other delivery",
    "Postpartum (puerperium) complications"
  ),
  
  ICD10 = c(
    "^Z34",
    "^Z35|^Z36",
    "^O0[0-8]",
    "^O(1[0-9]|2[0-9])",
    "^O(3[0-9]|4[0-8])",
    "^O(6[0-9]|7[0-5])",
    "^O80",
    "^O82",
    "^O81|^O83|^O84",
    "^O(8[5-9]|9[0-9])"
  ),
  
  `ICD-10 code(s)` = c(
    "Z34",
    "Z35–Z36",
    "O00–O08",
    "O10–O29",
    "O30–O48",
    "O60–O75",
    "O80",
    "O82",
    "O81, O83–O84",
    "O85–O99"
  ),
  
  `Clinical conditions included` = c(
    "Routine antenatal care",
    "High-risk ANC and antenatal screening",
    "Ectopic pregnancy, miscarriage, induced abortion and related complications",
    "Hypertension, diabetes, infections and other maternal disorders during pregnancy",
    "Multiple pregnancy, fetal abnormalities, placental disorders and post-term pregnancy",
    "Preterm labour, obstetric haemorrhage, obstructed labour, obstetric trauma and anaesthetic complications",
    "Spontaneous vaginal delivery",
    "Caesarean section",
    "Instrumental, assisted and multiple deliveries",
    "Postpartum infection, venous complications and maternal diseases complicating the puerperium"
  )
)

#----------------------------------------------------------
# 2. Create one long SDX column
#----------------------------------------------------------
sdx_long <- songkla_data %>%
  select(starts_with("sdx")) %>%
  pivot_longer(
    everything(),
    values_to = "ICD10"
  ) %>%
  filter(!is.na(ICD10), ICD10 != "")

#----------------------------------------------------------
# 3. Calculate frequencies
#----------------------------------------------------------
publication_table <- maternal_groups %>%
  rowwise() %>%
  mutate(
    `Primary diagnosis (PDX)` =
      sum(str_detect(songkla_data$pdx, ICD10), na.rm = TRUE),
    
    `Secondary diagnosis (SDX1–21)` =
      sum(str_detect(sdx_long$ICD10, ICD10), na.rm = TRUE),
    
    `Any diagnosis` =
      `Primary diagnosis (PDX)` +
      `Secondary diagnosis (SDX1–21)`
  ) %>%
  ungroup() %>%
  select(
    Group,
    `ICD-10 code(s)`,
    `Primary diagnosis (PDX)`,
    `Secondary diagnosis (SDX1–21)`,
    `Any diagnosis`,
    `Clinical conditions included`
  )

publication_table
publication_table %>%
  gt() %>%
  tab_header(
    title = md("**Maternal health-related diagnoses according to WHO ICD-10 (2016)**"),
    subtitle = "Primary and secondary diagnoses recorded in the NHSO e-Claim database 2023 Jan -2025 Dec"
  ) %>%
  fmt_number(
    columns = c(
      `Primary diagnosis (PDX)`,
      `Secondary diagnosis (SDX1–21)`,
      `Any diagnosis`
    ),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      `ICD-10 code(s)`,
      `Primary diagnosis (PDX)`,
      `Secondary diagnosis (SDX1–21)`,
      `Any diagnosis`
    )
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: PDX = primary diagnosis; SDX = secondary diagnosis; ANC = antenatal care."
  )

#others

dx_cols <- c("pdx", paste0("sdx", 1:21))

# --- 1. Per-column summary: total rows, non-missing, missing/empty, distinct codes ---
dx_col_summary <- map_dfr(dx_cols, function(col) {
  x <- songkla_data[[col]]
  tibble(
    column = col,
    n_total = length(x),
    n_missing_or_empty = sum(is.na(x) | x == ""),
    n_non_missing = sum(!is.na(x) & x != ""),
    n_distinct_codes = n_distinct(x[!is.na(x) & x != ""])
  )
})

print(dx_col_summary, n = 22)
#aggregate to monthly data
library(dplyr)
library(lubridate)

songkla_data <- songkla_data |>
  mutate(
    dateadm_parsed = ymd(dateadm),
    year_month = floor_date(dateadm_parsed, "month")
  )

monthly_pdx_summary <- songkla_data |>
  group_by(year_month, pdx) |>
  summarise(
    n_admissions = n(),
    .groups = "drop"
  ) |>
  arrange(year_month, desc(n_admissions))

monthly_pdx_summary
write.csv(
  publication_table,
  "Table1_Maternal_Diagnosis_Groups.csv",
  row.names = FALSE
)

#Create maternal diagnosis groups
library(dplyr)
library(stringr)
library(lubridate)

songkla_data <- songkla_data %>%
  mutate(
    maternal_group = case_when(
      str_detect(pdx, "^Z34") ~ "Normal pregnancy supervision (ANC)",
      str_detect(pdx, "^Z35|^Z36") ~ "High-risk pregnancy supervision & antenatal screening",
      str_detect(pdx, "^O0[0-8]") ~ "Abortion",
      str_detect(pdx, "^O(1[0-9]|2[0-9])") ~ "Pregnancy complications",
      str_detect(pdx, "^O(3[0-9]|4[0-8])") ~ "Maternal care (fetal & placental conditions)",
      str_detect(pdx, "^O(6[0-9]|7[0-5])") ~ "Labour & delivery complications",
      str_detect(pdx, "^O80") ~ "Normal vaginal delivery",
      str_detect(pdx, "^O82") ~ "Caesarean delivery",
      str_detect(pdx, "^O81|^O83|^O84") ~ "Other delivery",
      str_detect(pdx, "^O(8[5-9]|9[0-9])") ~ "Postpartum (puerperium) complications",
      TRUE ~ NA_character_
    )
  )

#Create care pathway
songkla_data <- songkla_data %>%
  mutate(
    care_pathway = case_when(
      maternal_group %in% c(
        "Normal pregnancy supervision (ANC)",
        "High-risk pregnancy supervision & antenatal screening"
      ) ~ "1. Antenatal Care",
      
      maternal_group %in% c(
        "Abortion",
        "Pregnancy complications",
        "Maternal care (fetal & placental conditions)"
      ) ~ "2. Pregnancy",
      
      maternal_group %in% c(
        "Labour & delivery complications",
        "Normal vaginal delivery",
        "Caesarean delivery",
        "Other delivery"
      ) ~ "3. Delivery",
      
      maternal_group == "Postpartum (puerperium) complications" ~
        "4. Postpartum",
      
      TRUE ~ NA_character_
    )
  )
#Aggregate monthly counts
monthly_counts <- songkla_data %>%
  filter(!is.na(maternal_group)) %>%
  mutate(
    year_month = floor_date(dateadm_parsed, "month")
  ) %>%
  group_by(
    year_month,
    care_pathway,
    maternal_group
  ) %>%
  summarise(
    Cases = n(),
    .groups = "drop"
  )
#Order the panels
monthly_counts$maternal_group <- factor(
  monthly_counts$maternal_group,
  levels = c(
    "Normal pregnancy supervision (ANC)",
    "High-risk pregnancy supervision & antenatal screening",
    
    "Abortion",
    "Pregnancy complications",
    "Maternal care (fetal & placental conditions)",
    
    "Labour & delivery complications",
    "Normal vaginal delivery",
    "Caesarean delivery",
    "Other delivery",
    
    "Postpartum (puerperium) complications"
  )
)

monthly_counts$care_pathway <- factor(
  monthly_counts$care_pathway,
  levels = c(
    "1. Antenatal Care",
    "2. Pregnancy",
    "3. Delivery",
    "4. Postpartum"
  )
)
#plot
library(ggplot2)

ggplot(
  monthly_counts,
  aes(
    x = year_month,
    y = Cases
  )
) +
  geom_line(
    linewidth = 0.8,
    colour = "#0072B2"
  ) +
  
  geom_vline(
    xintercept = as.Date("2024-11-01"),
    colour = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  facet_grid(
    care_pathway ~ maternal_group,
    scales = "free_y",
    space = "free_x"
  ) +
  
  labs(
    title = "Monthly maternal healthcare utilization by diagnosis group",
    subtitle = "Primary diagnosis (PDX)",
    x = "Month",
    y = "Monthly number of encounters"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    strip.text.x = element_text(
      face = "bold",
      size = 10
    ),
    strip.text.y = element_text(
      face = "bold",
      angle = 0
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )

table(songkla_data$maternal_group, useNA = "ifany")

class(songkla_data$dateadm_parsed)

songkla_data <- songkla_data %>%
  mutate(
    year_month = floor_date(dateadm_parsed, "month")
  )
table(songkla_data$year_month)

monthly_counts <- songkla_data %>%
  filter(!is.na(maternal_group)) %>%
  group_by(year_month, care_pathway, maternal_group) %>%
  summarise(
    Cases = n(),
    .groups = "drop"
  )

nrow(monthly_counts)

head(monthly_counts)

tail(monthly_counts)

class(songkla_data$dateadm_parsed)

head(songkla_data[, c("dateadm", "dateadm_parsed", "year_month")])

nrow(monthly_counts)
