library(tidyverse)
library(palmerpenguins)

penguins_raw <- penguins_raw
glimpse(penguins_raw)

# Select columns 
penguins_selected_cols <- select(penguins_raw, Species, Island, `Individual ID`)

penguins_selected_cols <- penguins_raw %>% 
  select(Species, Island, `Flipper Length (mm)`, `Individual ID`) 

penguins_raw %>%
  select(Species:`Individual ID`)

penguins_raw |>
  select(-Species)

penguins_raw |>
  select(Species, matches("Length"))

penguins_raw %>% 
  select(Species, everything())

penguins_raw_reduced <- penguins_raw %>%
  select(Species, matches("Length"))

# Renaming columns 
penguins_raw |>
  rename(id = `Individual ID`,
         flipper_length = `Flipper Length (mm)`)

penguins_raw %>%
  select(Species, `Individual ID`, matches("Length")) %>%
  rename(id = `Individual ID`,
    flipper_length = `Flipper Length (mm)`)

# Creating columns
penguins_raw %>%
  select(Species, matches("Length")) %>%
  rename(flipper_length = `Flipper Length (mm)`) %>%
  # create a new column
  mutate(flipper_length_cm = flipper_length * 0.1,
         culmen_length_cm = `Culmen Length (mm)` * 0.1)

penguins_raw %>%
  select(Species, matches("Length")) %>%
  rename(flipper_length = `Flipper Length (mm)`,
         culmen_length = `Culmen Length (mm)`) %>%
  mutate(flipper_culmen_ratio = flipper_length/culmen_length) 

# Filtering rows
penguins_raw |>
  filter(Species == "Gentoo penguin (Pygoscelis papua)")

penguins_raw %>%
  filter(Island %in% c("Torgersen", "Biscoe"))

penguins_raw %>%
  filter(Island == "Torgersen" & Sex == "MALE")

penguins_raw |> 
  filter(`Flipper Length (mm)` > 200)

penguins_raw %>% 
  filter(`Clutch Completion` == "Yes" & Island == "Torgersen")

## Ordering rows
mass_ordered <- penguins_raw %>%
  arrange(`Body Mass (g)`)

mass_ordered <- penguins_raw %>%
  filter(Island == "Biscoe") |>
  arrange(`Body Mass (g)`)

mass_ordered <- penguins_raw %>%
  filter(Island == "Biscoe") |>
  arrange(-`Body Mass (g)`)

mass_ordered <- penguins_raw %>%
  filter(Island == "Biscoe") |>
  arrange(Sex, `Body Mass (g)`)

# Summarizing data
penguins_raw |> 
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`),
            mean_body_mass = mean(`Body Mass (g)`))

penguins_raw |> 
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`, na.rm = T),
            mean_body_mass = mean(`Body Mass (g)`, na.rm = T))

penguins_raw |> 
  filter(`Clutch Completion` =="Yes") %>%
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm)`, na.rm = T))


penguins_raw |> 
  filter(`Clutch Completion` =="Yes") %>%
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm)`, na.rm = T),
            .by = Sex) %>%
  filter(is.na(Sex)==F)

# Recoding variables 
penguins_raw |>
  count(Species)

penguins_raw |> 
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", 1, 0)) %>%
  count(chinstrap)

penguins_raw |> 
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", "Chinstrap", "Other")) %>%
  count(chinstrap)

penguins_raw |> 
  mutate(species = case_when(
    ## syntax for case_when is condition ~ value
    Species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
    Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    .default = "Other" # what to do for all other cases
  )) %>%
  count(species)

species_table <- penguins_raw |> 
  mutate(species = case_when(
    ## syntax for case_when is condition ~ value
    Species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
    Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    .default = "Other" # what to do for all other cases
  )) %>%
  count(species)

## export this table
library(knitr)
library(kableExtra)

knitr::kable(species_table, format = "html", caption = "N Penguins by Species") |> 
  save_kable("species_table.html")





