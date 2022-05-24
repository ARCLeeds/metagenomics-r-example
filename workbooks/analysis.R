# taxa package
source("scripts/setup.R")

library(tidyverse)
require(metacoder)
require(vegan)
library(stringr)

# or we can also read data from a .csv file
data <- read_csv("data/elife-data.csv")

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

# richness of community with alpha diversity

specnumber(data)

chao1.dat <- data %>%
  column_to_rownames(var = "name") %>%
  mutate(. *1e15)  %>%
  estimateR() %>%
  as.data.frame %>%
  slice(2) %>%
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>%
  rename(samples = "name", chao1.index = "value")

chao1.groups <- chao1.dat %>%
  filter(str_detect(samples, "stool|oral")) %>%
  mutate(group = case_when(str_detect(samples, 'stool') ~ 'stool',
                           str_detect(samples, 'oral') ~ 'oral'))

ggplot(chao1.groups, aes(x = group, y = chao1.index)) +
  geom_boxplot()


shannon.data <- data %>%
  column_to_rownames(var = "name") %>%
  diversity("shannon") %>%
  as.data.frame %>%
  rename(shannon.index = ".") %>%
  rownames_to_column(var = "samples")

shan.groups <- shannon.data %>%
  filter(str_detect(samples, "stool|oral")) %>%
  mutate(group = case_when(str_detect(samples, 'stool') ~ 'stool',
                           str_detect(samples, 'oral') ~ 'oral'))

library(ggplot2)

ggplot(shan.groups, aes(x = group, y = shannon.index)) +
  geom_boxplot()

plot(shannon.data)

summary(aov(chao1.groups$chao1.index ~ chao1.groups$group))

summary(aov(shan.groups$shannon.index ~ shan.groups$group))

### wilcoxon test


wilcox.test(filter(shan.groups, str_detect(samples, "stool"))[,c('shannon.index')])

wilcox.test(pull(chao1.groups[,c('chao1.index')]) ~ pull(chao1.groups[,c('group')]))

test.o <- data %>%
  column_to_rownames(var = "name") %>%
  vegdist("bray")

test.p <- data %>%
  column_to_rownames(var = "name") %>%
  vegdist("jaccard")

mantel(test.o, test.p)






