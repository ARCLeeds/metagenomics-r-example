# download datafile
source("scripts/setup.R")

library(readr)
library(readxl)
library(tidyr)
library(tibble)

# we can load data with some tidyverse functions
# we might even have tabular data in excel format, we can read that using readxl package
# but we also crucially have to specify the sheet to load
data <- read_excel("data/elife-data.xlsx", sheet = "Taxa_rel_abd")

# or we can also read data from a .csv file
data <- read_csv("data/elife-data.csv")

# we can still index like this
data[1,]

data[,3]

data[,c("SAMEA2737770")]

# but we can also do some cool stuff with dplyr

library(dplyr)

data %>%
  select(SAMEA2737770)

data %>%
  slice(2)

# using select to remove a column
data <- data %>%
  select(-1)

# we can use rename to rename columns by previous name
data <- rename(data, species = "...2")


# transpose data
data <- data %>% 
  column_to_rownames(var = "species") %>%
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 


# filter by condition and return specific column
data %>%
  filter(`[Clostridium] scindens ATCC 35704` > 0.001) %>%
  select(name, `[Clostridium] scindens ATCC 35704`)


# select top 10 samples with specific species abundance
data %>%
  arrange(desc(`[Clostridium] scindens ATCC 35704`)) %>%
  select(name, `[Clostridium] scindens ATCC 35704`) %>%
  head(10)

# calculate column means
data.means <- data %>%
  select(!name) %>%
  summarise(across(.fns = mean)) %>%
  pivot_longer(everything(), names_to = 'species', values_to = 'mean-abd') %>%
  arrange(desc(`mean-abd`))

library(ggplot2)

# plot column means
ggplot(data = data.means, mapping = aes(x = species, y = `mean-abd`)) +
  geom_bar(stat = 'identity')

# sort mean abundances per species/col
data %>%
  summarise(across(.fns = mean)) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value))

# create sub dataset of specific columns
spec.cols <- data %>%
  select(name, `Clostridium sp. D5`, `Butyrivibrio proteoclasticus B316`, `Dorea formicigenerans ATCC 27755`)

# calculate means across rows and select top 10
spec.cols <- spec.cols %>%
  mutate(sums = rowSums(across(where(is.numeric)))) %>%
  select(name, sums) %>% 
  arrange(desc(sums)) %>%
  head(10)

# plot top 10
ggplot(spec.cols, mapping = aes(x = reorder(name, -means), y = means)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

