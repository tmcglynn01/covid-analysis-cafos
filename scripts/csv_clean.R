library(tidyverse)
library(forcats)
library(readxl)
library(tabulizer)
library(rJava)

df <- read_csv('data/RAW/cafo_the_data_for_a_cause_challenge.csv')

df %>% count(permit, sort = TRUE)
df %>% count(permit_status, sort = TRUE)
df %>% count(site_name, sort = TRUE)
df %>% count(animal_type, sort = TRUE)
df %>% count(site_state, sort = TRUE)

df$animal_type <- 'chicken'

drop_cols <- c('permit_number', 'source', 'geo_provider', 'location', 'permit',
               'permit_status', 'permit_issue_date', 'uid', 'site_county')
cafo <- df %>% select(-!!drop_cols)

factor_cols <- c('site_name', 'site_state', 'site_city', 'site_zip_code')

summary(cafo$total_no_of_animals)
cafo %>% count(is.na(total_no_of_animal_units), sort = TRUE)
cafo %>% count(is.na(total_no_of_animal_units), sort = TRUE)
# Tons of NA values in both of these columns, lets get more data

# All FDA inspections 
df <- read_excel('data/RAW/fda-inspections.xlsx')
fda_facilities <- df %>% 
  select(1:6, 13) %>% 
  rename(fei_num = 1, name = 2, city = 3, state = 4, zip = 5, country = 6,
         prod_type = 7) %>% 
  distinct(fei_num, .keep_all = TRUE)

# Handle factors for country and product type
fda_facilities %>% count(country, sort = TRUE)
fda_facilities <- filter(fda_facilities, country == 'United States')
fda_facilities %>% count(prod_type)
fda_facilities <- filter(fda_facilities, prod_type == 'Food/Cosmetics')
drop_cols <- c('prod_type', 'country')
fda_facilities <- select(fda_facilities, -!!drop_cols)

factor_cols <- c('city', 'state')
fda_facilities <- fda_facilities %>% 
  mutate(across(where(is.character), str_to_lower)) %>% 
  mutate(name = str_remove_all(name, '[[:punct:]]')) %>% 
  mutate(name = mapply(str_remove, name, 
                           'inc$|co$|llc$|corp$|ltd$|incorporated$|dba$')) %>% 
  mutate(name = mapply(str_trim, name))

df <- read_csv('data/RAW/facilities.csv')

meatpackers <- df %>% 
  mutate(across(where(is.character), str_to_lower)) %>% 
  # Facilities
  mutate(facility = mapply(str_remove_all, facility, '[[:punct:]]')) %>% 
  mutate(facility = mapply(str_remove, facility, 'inc$|co$|llc$|')) %>% 
  mutate(facility = mapply(str_trim, facility)) %>% 
  # Localities
  extract(2, into = c('city', 'state'), regex = '(^.+), (\\w{2}$)') %>% 
  mutate(cattle = str_detect(animals, 'cattle')) %>% 
  mutate(swine = str_detect(animals, 'swine')) %>% 
  select(-animals)

intersect(fda_facilities$name, meatpackers$facility)
# Only 3 values?

cafo <- cafo %>% 
  mutate(name = str_remove_all(site_name, '[[:punct:]]')) %>% 
  mutate(across(where(is.character), str_to_lower))

# Try to unite dfs
# identifier, name, city, state, zip (optional), lat (opt), long (opt)

df <- cafo %>% 
  mutate(across(where(is.character), str_to_lower)) %>% 
  select(name = site_name, city = site_city, state = site_state,
         zip = site_zip_code, lat = latitude, lon = longitude) %>% 
  mutate(id = 'no id', .before = name) %>% 
  mutate(type = 'cafo')
glimpse(df)

df <- fda_facilities %>%
  select(id = fei_num, name = name, city = city, state = state, zip = zip) %>% 
  mutate(lat = NA, lon = NA, type = 'fda facility') %>% 
  union(df)

df <- meatpackers %>% 
  select(id = 4, name = 1, city = 2, state = 3) %>% 
  mutate(zip = NA, lat = NA, lon = NA, type = 'meat packer') %>% 
  union(df)

df <- df %>% distinct(name)

write_csv(df, 'data/CLEAN/combined_df.csv')
write_csv(fda_facilities, 'data/CLEAN/fda_facilities.csv')
write_csv(cafo, 'data/CLEAN/cafo_clean.csv')
write_csv(meatpackers, 'data/CLEAN/meatpackers.csv')