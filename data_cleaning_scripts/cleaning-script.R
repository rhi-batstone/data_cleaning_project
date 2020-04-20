# Librarys
library(tidyverse)
library(janitor)
library(readxl)

#Reading in the data
candy_2015 <- read_excel("candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("candy_ranking_data/boing-boing-candy-2017.xlsx")
                               

# 2015
candy_2015_clean <- candy_2015 %>% 
  #Pivoting to combine candy into one column
  pivot_longer(
    cols = starts_with("["), 
    names_to = "candy",
    values_to = "rating"
    ) %>% 
  #cleaning column names to snake case
  clean_names() %>% 
  #separating time stamp to separate into date and time cols
  separate(
    timestamp, 
    c("date", "time"), 
    sep = " "
    ) %>%
  # Removing [ ] from candy column
  mutate(candy = str_sub(candy, start = 2, end = -2)) %>%
  #selecting to drop unecessary columns
  select(date, 
         time, 
         age = how_old_are_you, 
         candy, 
         rating, 
         going_out = are_you_going_actually_going_trick_or_treating_yourself)


# 2016
candy_2016_clean <- candy_2016 %>% 
  #Pivoting to combine candy into one column
  pivot_longer(
    cols = starts_with("["), 
    names_to = "candy",
    values_to = "rating"
    ) %>% 
  clean_names() %>% 
  #separating time stamp to separate into date and time cols
  separate(
    timestamp, 
    c("date", "time"), 
    sep = " "
    ) %>%
  # Removing [ ] from candy column
  mutate(candy = str_sub(candy, start = 2, end = -2)) %>%
  #selecting to drop unecessary columns
  select(date, 
         time,
         age = how_old_are_you, 
         gender = your_gender, 
         country_unclean = which_country_do_you_live_in,
         candy, 
         rating,
         going_t_or_t = are_you_going_actually_going_trick_or_treating_yourself)

# 2017

candy_2017_clean <- candy_2017 %>%
  #Pivoting to combine candy into one column
  pivot_longer(
    cols = starts_with("Q6"),
    names_to = "candy",
    values_to = "rating"
    ) %>% 
  #cleaning column names to snake case
  clean_names() %>%
  # Removing "Q6 | " from candy column
  mutate(candy = str_sub(candy, start = 6,)) %>%
  #selecting to drop unecessary columns
  select(age = q3_age,
         gender = q2_gender,
         country_unclean = q4_country, 
         candy, 
         rating, 
         going_t_or_t = q1_going_out)


# Binding table rows 
clean_candy <- bind_rows(candy_2015_clean, candy_2016_clean, candy_2017_clean)


##---------------------------- AGE -----------------------------------

# List of ages that will be included
age_list <- c(16:99)

#Cleaning age data which contains a lot of noisy data
clean_candy <- clean_candy %>% 
  mutate(age = str_sub(age, end = 2)) %>% 
  filter(age %in% age_list)

# Cinverting age to numeric
clean_candy <- clean_candy %>%
  mutate(age = as.numeric(age),
         country_unclean = str_to_lower(country_unclean),
         rating = str_to_lower(rating)
         )




##---------------------------- COUNTRY --------------------------------
# Cleaning country column -------------------------------------------------

#from review possible mispellings
us_mispellings <- c(
  "unites states",
  "murica",
  "united state",
  "united stated",
  "united ststes",
  "trumpistan",
  "united sates",
  "merica",
  "'merica",
  "ahem....amerca",
  "alaska",
  "murrika",
  "california",
  "new jersey",
  "new york",
  "north carolina",
  "pittsburgh",
  "u s",
  "unhinged states",
  "unied states",
  "unite states",
  "united staes",
  "united statea",
  "united statss",
  "the yoo ess of aaayyyyyy",
  "united stetes",
  "units states",
  "cascadia",
  "the republic of cascadia"
)


uk_mispellings <- c(
  "uk",
  "scotland",
  "england",
  "ireland",
  "u.k.",
  "united kindom"
)

unknown_country <- c(
  "a",
  "atlantis",
  "canae",
  "earth",
  "endland",
  "europe",
  "fear and loathing",
  "i don't know anymore",
  "insanity lately",
  "narnia",
  "soviet canuckistan",
  "ud",
  "a tropical island south of the equator",
  "denial",
  "eua",
  "god's country",
  "neverland",
  "one of the best ones",
  "see above",
  "somewhere",
  "there isn't one for old men",
  "this one"
)


clean_candy <- clean_candy %>%
  rename(country_unclean, country = country_unclean) %>% 
  mutate(
    # Find all US
    country = if_else(str_detect(country, "usa"), "united states", country),
    country = if_else(str_detect(country, "us"), "united states", country),
    country = if_else(str_detect(country, "united states"), "united states", country),
    country = if_else(str_detect(country, "u.s."), "united states", country),
    country = if_else(str_detect(country, "america"), "united states", country),
    country = if_else(country %in% us_mispellings, "united states", country),
    # Tidy other countries
    country = if_else(country %in% uk_mispellings, "united kingdom", country),
    country = if_else(country %in% c("canada`", "can"), "canada", country),
    country = if_else(country == "españa", "spain", country),
    country = if_else(country == "korea", "south korea", country),
    country = if_else(country == "the netherlands", "netherlands", country),
    # Change remaining into missing values
    country = if_else(str_detect(country, "[0-9]"), NA_character_, country), 
    country = if_else(country %in% unknown_country, NA_character_, country)
  )



# Writing to CSV
write_csv(clean_candy, "clean_data/candy_clean.csv")

