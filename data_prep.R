
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(here)
library(readxl)
library(kableExtra)

# Prepare data -----------------------------------------------------------

# Load data
km <- read_excel(here("data", "placeringar KM 2021.xlsx"))

# Make tidy data
km_clean <- km %>%
  # make long dataframe
  pivot_longer(!Name, names_to = "comp_no", values_to = "place") %>% 
  # get competition number
  separate(comp_no, into = c(NA, "comp_no"), sep = "KM ") %>%
  janitor::clean_names()

## Calculate points per competition

# Add new variable "points"
tot <- km_clean %>% mutate(points = case_when(
  place == 1 ~ 25,
  place == 2 ~ 22,
  place == 3 ~ 19,
  place == 4 ~ 17,
  place == 5 ~ 15,
  place == 6 ~ 13,
  place == 7 ~ 12,
  place == 8 ~ 11,
  place == 9 ~ 10,
  place == 10 ~ 9,
  place == 11 ~ 8,
  place == 12 ~ 7,
  place == 13 ~ 6,
  place == 14 ~ 5,
  place == 15 ~ 4,
  place == 16 ~ 3,
  place == 17 ~ 2,
  place >= 18 ~ 1
))

# Prepare data for the result table
df <- tot %>% 
  # filter for current competition
  filter(comp_no == 2) %>%
  select(-comp_no) %>% 
  arrange(desc(points)) %>% 
  drop_na(points) %>% 
  rename(Namn = name, Placering = place, Poäng = points)
  

# Create result table for current competition -----------------------------


# Create table output using kableExtra package
kable(df) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


# Create overall standings table ------------------------------------------

## Calculate grand total points for all competitions (1-2)
grand_tot <- tot %>% group_by(name) %>%
  summarise(sum_points = sum(points, na.rm = TRUE)) %>% 
  arrange(desc(sum_points))
View(grand_tot)

##  Change format for output - create "pivot table" with points per competition
piv_comp <- tot %>% 
  select(name, comp_no, points) %>% 
  pivot_wider(names_from = comp_no, values_from = points, names_prefix = "comp_") %>% 
  select(!(comp_3:comp_5))
# View(piv_comp)

# Create new piv df with totals column
res <- piv_comp %>%
  full_join(grand_tot, by = "name") %>% 
  arrange(desc(sum_points))

# Add ranking
res <- res %>% mutate(rank = min_rank(desc(sum_points)))

# Relocate variables
res <- res %>% relocate(rank, .before = name)

# Rename columns for final table Note: change for each competition!!
standings <- res %>% rename("#" = "rank", "Namn" = "name", 
                            "1" = "comp_1", "2" = "comp_2",
                            "Summa Poäng" = "sum_points")

# Create tabel output using kableExtra package
kable(standings) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
