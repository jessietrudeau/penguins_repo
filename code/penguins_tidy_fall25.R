library(tidyverse)
library(palmerpenguins)

penguins_raw <- penguins_raw
glimpse(penguins_raw)

?penguins_raw

select(penguins_raw, Species, Island, `Individual ID`)

penguins_raw %>% 
  select(Species:`Individual ID`)

penguins_raw %>%
  select(-Species)

penguins_raw %>% 
  select(Species, everything())

penguins_raw_reduced <- penguins_raw %>%
  select(Species, matches("Length"))

penguins_raw <- penguins_raw |>
  rename(id = `Individual ID`,
         flipper_length = `Flipper Length (mm)`)


penguins_raw %>% 
  select(Species, `Individual ID`, matches("Length")) %>% 
  rename(id = `Individual ID`,
         flipper_length = `Flipper Length (mm)`) %>%
  ## mutate length variables 
  mutate(flipper_length_cm = flipper_length * 0.1,
         culmen_length_cm = `Culmen Length (mm)` * 0.1)

penguins_raw %>% 
  select(Species, matches("Length")) %>%
  rename(flipper_length = `Flipper Length (mm)`) %>%
  mutate(flipper_culmen_ratio = flipper_length / `Culmen Length (mm)`)

penguins_raw |> 
  filter(Species == "Gentoo penguin (Pygoscelis papua)")

penguins_raw %>%
  filter(Island %in% c("Biscoe", "Togersen"))

vector_ex <- c(1, 2, 30:100)
vector_char_ex <- c("a", "jessie", "syracuse")


penguins_raw %>% 
  filter(Island =="Torgersen" & Sex == "MALE" ) %>%
  filter(`Flipper Length (mm)` > 200)


penguins_raw %>% 
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`, na.rm = T),
            max_body_mass = max(`Body Mass (g)`, na.rm = T),
            .by = Sex)


penguins_raw %>%
  count(Species)

chinstrap_count <- penguins_raw |>
## creating new variable if chinstrap (binary)
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", 1, 0)) %>%
  count(chinstrap)


library(knitr)

knitr::kable(chinstrap_count, format ="html", 
             caption ="Count of Penguins, if Chinstrap Species") %>%
  kableExtra::kable_styling('striped') %>%
  kableExtra::save_kable("chinstrap_table.html")







