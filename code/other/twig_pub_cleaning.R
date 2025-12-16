# TWIG publication data cleaning (publication: https://www.nature.com/articles/s41597-025-05859-z)

## Author: Jay M Schoen, Data Scentist, WFFRC

# Setup ------------------------------------------------------
library(tidyverse)
library(terra)
library(mapview)

setwd('~/wffrc_data/twig/publication_20250827/')
twig <- vect("treatment_index.gdb/", proxy = T)
names(twig)
nrow(twig)

west_states <- c('CA', 'OR', 'WA', 'ID', 'MT', 'WY', 'CO', 'NM', 'AZ', 'UT', 'NV') 
usa <- vect('../../admin/gadm/gadm41_USA_1_pk.rds')
states <- usa %>% subset(.$ISO_1 %in% str_glue('US-{west_states}')) %>%
  project(twig)

# plot(states)

data <- twig %>% query(filter = states %>% buffer(5e4)) # buffer to include anything <50km from border (will filter states next)
names(data)
nrow(data)
as_tibble(data)%>%pull(state)%>%unique() 

# filter for west states
data_w <- data %>% subset(.$state %in% west_states)
nrow(data_w)
# 1,025,400 records in 11 western states

# # Exports
# writeVector(data_w, 'twig_pub_western_US.gpkg', overwrite = T)
# writeVector(data_w, 'twig_pub_western_US.shp', overwrite = T)

v <- vect('twig_pub_western_US.gpkg')
names(v)
nrow(v)

# Visualizing overlapping records
#   - u_ids to test: 6534884010602, 3573999010602
test <- v %>% subset(.$unique_id == 3573999010602)
t1 <- v %>% crop(test)
t1
mapview(t1, alpha.regions = 0.1)
View(as_tibble(t1))


# Non-spatial work ---------------------------------------------
# d <- as.data.frame(v, geom = 'WKT') %>%
#   as_tibble()
# save(list = 'd', file = 'twig_western_US_df.rda') # saving with geom for filtering later
load('twig_western_US_df.rda', verbose = T)
summary(d)
colSums(is.na(d))

table(d$twig_category)
table(d$type)
table(d$identifier_database)

# summarizing records based on category and type
summary_df <- d %>% group_by(twig_category, type) %>%
  summarise(n = n())

summary_df %>%
  drop_na(twig_category) %>%
  ggplot() +
  geom_col(aes(x = n, y = twig_category, fill = type))


# Cleaning/Filtering --------------------------------------------------

## Processing steps
# - change blanks, 'N/A', etc. to NA
# - filter for date source == 'date_completed'or 'act_comp_dt' (only completed treatments included)
# - drop unique_id dupes
# - drop error-flagged dupes
# - drop dupes if following attributes are the same: name, twig_category, treatment_date, acres
# - convert dates back to UTC
# - correct one record with wrong year entered

d %>% group_by(unique_id) %>% filter(n()>1) %>% View() # confirming 7 unique_id repeats (2-sec differences in 'date_current')

dup_cols <- c('name', 'treatment_date', 'twig_category', 'acres')

# Filtering
d1 <- d %>% 
  mutate(across(everything(), ~ ifelse(. %in% c('', ' ', 'N/A', 'NA', 'Not Applicable'), NA, .))) %>%
  filter(date_source %in% c('date_completed', 'act_comp_dt')) %>%   # needs to be either 'date_completed' or 'act_comp_dt' (different source codes for completion)
  distinct(unique_id, .keep_all = T) %>%
  filter(error %in% c(NA, 'DUPLICATE-KEEP')) %>%
  distinct(across(all_of(dup_cols)), .keep_all = T) %>%
  mutate(treatment_date = as.POSIXct(actual_completion_date, tz = 'UTC')) %>% # converted back to epoch time for some reason
  mutate(across(c(date_current, actual_completion_date), function(x) as.POSIXct(x, tz = 'UTC'))) %>%
  filter(treatment_date < as.Date("2026-01-01"))
  
summary(d1)

# Checking one 'completed' record with future date
test <- filter(d, date_source %in% c('date_completed', 'act_comp_dt'),
                  treatment_date > as.Date("2026-01-01"))
test
test_v <- vect(test, geom = 'geometry', crs = crs(twig))
mapview(test) # appears valid

# Record date likely entered incorrectly (2105 instead of 2015)
#   - changing date and adding back to d1

test <- test %>%
  mutate(treatment_date = as_date('2015-09-30'),
         actual_completion_date = as_date('2015-09-30'))
test

d2 <- bind_rows(d1, test)

colSums(is.na(d2))


# Back to vector
v1 <- vect(d2, geom = 'geometry', crs = crs(twig))
table(is.valid(v1)) # all geometries valid

today <- today() %>% str_replace('-','') %>% str_replace('-', '')
# writeVector(v1, paste0('twig_western_US_filtered_jms_', today, '.shp'), overwrite = T)


# Plots --------------------------------------------------------------

d2 %>% drop_na(twig_category) %>%
  mutate(count = 1) %>%
  ggplot() +
  geom_col(aes(x = count, y = twig_category, fill = type)) +
  # geom_col(aes(x = count, y = twig_category, fill = method)) +
  scale_x_continuous(labels = scales::label_comma()) +
  ggsci::scale_fill_igv(na.value = 'grey80') +
  theme_bw()
# ggsave(filename = 'twig_pub_summary_count.png', width = 18, height = 10)

d2 %>% drop_na(twig_category, type) %>%
# d2 %>% drop_na(twig_category, method) %>%
  mutate(count = 1) %>%
  ggplot() +
  geom_col(aes(x = count, y = twig_category, fill = type)) +
  # geom_col(aes(x = count, y = twig_category, fill = method)) +
  scale_x_continuous(labels = scales::label_comma()) +
  ggsci::scale_fill_igv(na.value = 'grey80') +
  theme_bw()
# ggsave(filename = 'twig_pub_summary_count_noNA_type.png', width = 18, height = 10)

d2 %>% drop_na(twig_category) %>%
  ggplot() +
  geom_col(aes(x = acres, y = twig_category, fill = type)) +
  # geom_col(aes(x = acres, y = twig_category, fill = method)) +
  scale_x_continuous(labels = scales::label_comma()) +
  ggsci::scale_fill_igv(na.value = 'grey80') +
  theme_bw()
# ggsave(filename = 'twig_pub_summary_acres.png', width = 18, height = 10)

d2 %>% drop_na(twig_category, type) %>%
# d2 %>% drop_na(twig_category, method) %>%
  mutate(count = 1) %>%
  ggplot() +
  geom_col(aes(x = acres, y = twig_category, fill = type)) +
  # geom_col(aes(x = acres, y = twig_category, fill = method)) +
  scale_x_continuous(labels = scales::label_comma()) +
  ggsci::scale_fill_igv(na.value = 'grey80') +
  theme_bw()
# ggsave(filename = 'twig_pub_summary_acres_noNA_type.png', width = 18, height = 10)


hist_types_st <- d2 %>% drop_na(twig_category) %>%
  ggplot() +
  geom_histogram(aes(x = state, fill = twig_category), stat = 'count') +
  theme_bw() +
  ggsci::scale_fill_aaas()
hist_types_st
# ggsave('hist_twig_category_states.png', hist_types_st, width = 8, height = 5, dpi = 300)

area_types_st <- d2 %>% drop_na(twig_category) %>%
  group_by(twig_category, state) %>%
  ggplot(aes(x = state, y = acres, fill = twig_category)) + 
  geom_col() +
  theme_bw() +
  ggsci::scale_fill_aaas()
area_types_st
# ggsave('acres_twig_category_states.png', area_types_st, width = 8, height = 5, dpi = 300)


## END ##