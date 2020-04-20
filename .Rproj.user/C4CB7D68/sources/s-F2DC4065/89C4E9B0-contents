#load in libraries
library(tidyverse)
library(readxl)
library(janitor)

#load in data
candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")


# Reformat data -----------------------------------------------------------

#clean column names 
candy_2015 <- clean_names(candy_2015)
candy_2016 <- clean_names(candy_2016)
candy_2017 <- clean_names(candy_2017)

#remove question number prefix to column names in 2017 dataset
names(candy_2017) <- str_remove(names(candy_2017), "q[0-9]+_")

# Add an id number to keep track entries when make long format
# convert to desired long format and adding columns 'gender' and 'country' so can bind to 2017
# also add a year column so that know which year the data comes from when bind all sets together
candy_2015_clean <- 
candy_2015 %>%
  mutate(person_id = row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2015,
    gender = NA,
    country = NA
  ) %>%
  select(
    year,
    person_id,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    age = how_old_are_you,
    gender,
    country,
    candy,
    rating
  ) 

# Add an id number to keep track entries when make long format
# convert to desired long format and adding columns 'gender' and 'country' so can bind to 2017
# also add a year column so that know which year the data comes from when bind all sets together
candy_2016_clean <-
candy_2016 %>%
# id number needs to start from the end of 2015 max id as assuming distinct groups of people
  mutate(person_id = max(candy_2015_clean$person_id) + row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2016
  ) %>%
  select(
    year,
    person_id,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    age = how_old_are_you,
    gender = your_gender,
    country = which_country_do_you_live_in,
    candy,
    rating
  ) 

# Add an id number to keep track entries when make long format
# convert to desired long format 
# also add a year column so that know which year the data comes from when bind all sets together
candy_2017_clean <- 
candy_2017 %>%
  rename(x100_grand_bar = `100_grand_bar`) %>%
# id number needs to start from the end of 2016 max id as assuming distinct groups of people
  mutate(person_id = max(candy_2016_clean$person_id) + row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2017
  )  %>%
  select(
    year,
    person_id,
    going_out,
    age,
    gender,
    country,
    candy,
    rating
  )

# bind all the data from each year together
candy_combined <- rbind(candy_2015_clean, candy_2016_clean, candy_2017_clean)

# reformat some of the columns to tidy
candy_combined_tidy <- 
  candy_combined %>% 
  mutate(
    age = as.numeric(age),
    rating = str_to_lower(rating),
    country = str_to_lower(country)
    )


# Cleaning country column -------------------------------------------------

#can see from country column many different ways can say the same country (e.g. us vs united states) but there are also many different mispellings/incorrect names
#to have accurate counts by country need to clean this up - and most will be manual hard coding these mispellings from manually looking at the column entries

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


candy_combined_clean <- 
  candy_combined_tidy %>%
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


# Remove outliers ---------------------------------------------------------

# seems un likely any ages over 99 so have assumed errors (to avoid skewing the average ages) 
candy_combined_clean <- 
  candy_combined_clean %>%
  mutate(age = if_else(age > 99, NA_real_, age))


# Write clean data to csv -------------------------------------------------

write_csv(candy_combined_clean , "clean_data/candy_clean.csv")
