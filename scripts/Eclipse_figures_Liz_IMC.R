#### Eclipse bird analysis
### Liz's figures for paper
## statistical analysis
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
library(tidyverse)

# ----- Lux --------------------------------------------------
# load data
load('data/data_raw_Liz/lux graph.RData')

# plot LUX by time  
lux_time <- lux_data %>%
  ggplot(aes(x = EDT,
             y = lux)) +
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%M") +
  geom_rect(aes(xmin = as.POSIXct("1899-12-31 15:04:55", 
                                  tz = 'EDT'), 
                xmax = as.POSIXct("1899-12-31 15:08:55", 
                                  tz = 'EDT'),
                ymin = 0,
                ymax = Inf),
            color = 'grey',
            fill = 'grey') +
  geom_point(size = 1) +
  labs(x = "Time (EDT)") +
  labs(y = "Lux") + 
  theme_classic(base_size = 12) 

lux_time

# save plot
ggsave("figures/paper/liz/Apr8_lux.pdf",
       height = 5,
       width = 6.5,
       units = 'in')

# ----- VOCALIZING ----------------------------------------------------
# load data
totality_data2 = read.csv('data/data_raw_Liz/totality data.csv') %>% 
  select(-X) %>% 
  mutate(submission.category_totality = case_when(submission.category_totality == 'before' ~ 'Before',
                                                  submission.category_totality == 'during' ~ 'Totality',
                                                  submission.category_totality == 'after' ~ 'After',
                                                  TRUE ~ NA))

## create vocal dataframe
# want counts of vocal vs no vocal across time points
# need to rename variables for use with glm
df.vocal = totality_data2 %>% 
  select(submission.category_totality,
         vocal) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = vocal,
              values_from = Freq) %>% 
  rename(yes = '1') %>% 
  rename(no = '0')


## graph results
# use proportion
df.vocal %>% 
  pivot_longer(cols = c('yes',
                        'no'),
               names_to = 'type',
               values_to = 'freq') %>% 
  group_by(submission.category_totality) %>% 
  mutate(total = sum(freq)) %>% 
  ungroup() %>% 
  mutate(Proportion = 100*freq/total) %>% 
  ggplot(aes(x = factor(submission.category_totality,
                        levels = c("Before", 
                                   "Totality",
                                   "After")),
             y = Proportion,
             fill = paste0(type,
                           submission.category_totality))) +
  # ggplot(aes(x = submission.category_totality,
  #            y = Proportion,
  #            fill = type)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = c('#F3F3F3',
                               '#F3F3F3',
                               '#F3F3F3',
                               '#FFC000',
                               '#B21912',
                               '#404040')) +
  # ggtitle("a) Vocalizing") +
  theme_classic(base_size = 12) +
  # xlab('Period of observation') +
  # ylab('Proportion of total observations') +
  # theme(axis.text = element_text(size = 12)) +
  # theme(axis.title = element_text(size = 14)) +
  theme(legend.position = 'none') +
  theme(axis.title = element_blank()) 


ggplot2::ggsave("figures/paper/liz/vocalizing.png",
                height = 3,
                width = 3,
                units = 'in')


# ----- flying ----------------------------------------------------
# load data
totality_data2 = read.csv('data/data_raw_Liz/totality data.csv') %>% 
  select(-X) %>% 
  mutate(submission.category_totality = case_when(submission.category_totality == 'before' ~ 'Before',
                                                  submission.category_totality == 'during' ~ 'Totality',
                                                  submission.category_totality == 'after' ~ 'After',
                                                  TRUE ~ NA))

## create flying dataframe
# want counts of flying vs no flying across time points
# need to rename variables for use with glm
df.flying = totality_data2 %>% 
  select(submission.category_totality,
         flying) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = flying,
              values_from = Freq) %>% 
  rename(yes = '1') %>% 
  rename(no = '0')


## graph results
# use proportion
df.flying %>% 
  pivot_longer(cols = c('yes',
                        'no'),
               names_to = 'type',
               values_to = 'freq') %>% 
  group_by(submission.category_totality) %>% 
  mutate(total = sum(freq)) %>% 
  ungroup() %>% 
  mutate(Proportion = 100*freq/total) %>% 
  ggplot(aes(x = factor(submission.category_totality,
                        levels = c("Before", 
                                   "Totality",
                                   "After")),
             y = Proportion,
             fill = paste0(type,
                           submission.category_totality))) +
  # ggplot(aes(x = submission.category_totality,
  #            y = Proportion,
  #            fill = type)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = c('#F3F3F3',
                               '#F3F3F3',
                               '#F3F3F3',
                               '#FFC000',
                               '#B21912',
                               '#404040')) +
  # ggtitle("b) Flying") +
  theme_classic(base_size = 12) +
  # xlab('Period of observation') +
  # ylab('Proportion of total observations') +
  # theme(axis.text = element_text(size = 12)) +
  # theme(axis.title = element_text(size = 14)) +
  theme(legend.position = 'none') +
  theme(axis.title = element_blank()) 


ggplot2::ggsave("figures/paper/liz/flying.png",
                height = 3,
                width = 3,
                units = 'in')


# ----- stationary ----------------------------------------------------
# load data
totality_data2 = read.csv('data/data_raw_Liz/totality data.csv') %>% 
  select(-X) %>% 
  mutate(submission.category_totality = case_when(submission.category_totality == 'before' ~ 'Before',
                                                  submission.category_totality == 'during' ~ 'Totality',
                                                  submission.category_totality == 'after' ~ 'After',
                                                  TRUE ~ NA))

## create stationary dataframe
# want counts of stationary vs no stationary across time points
# need to rename variables for use with glm
df.stationary = totality_data2 %>% 
  select(submission.category_totality,
         stationary) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = stationary,
              values_from = Freq) %>% 
  rename(yes = '1') %>% 
  rename(no = '0')


## graph results
# use proportion
df.stationary %>% 
  pivot_longer(cols = c('yes',
                        'no'),
               names_to = 'type',
               values_to = 'freq') %>% 
  group_by(submission.category_totality) %>% 
  mutate(total = sum(freq)) %>% 
  ungroup() %>% 
  mutate(Proportion = 100*freq/total) %>% 
  ggplot(aes(x = factor(submission.category_totality,
                        levels = c("Before", 
                                   "Totality",
                                   "After")),
             y = Proportion,
             fill = paste0(type,
                           submission.category_totality))) +
  # ggplot(aes(x = submission.category_totality,
  #            y = Proportion,
  #            fill = type)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = c('#F3F3F3',
                               '#F3F3F3',
                               '#F3F3F3',
                               '#FFC000',
                               '#B21912',
                               '#404040')) +
  # ggtitle("c) Stationary") +
  theme_classic(base_size = 12) +
  # xlab('Period of observation') +
  # ylab('Proportion of total observations') +
  # theme(axis.text = element_text(size = 12)) +
  # theme(axis.title = element_text(size = 14)) +
  theme(legend.position = 'none')+
  theme(axis.title = element_blank()) 


ggplot2::ggsave("figures/paper/liz/stationary.png",
                height = 3,
                width = 3,
                units = 'in')


# ----- other ----------------------------------------------------
# load data
totality_data2 = read.csv('data/data_raw_Liz/totality data.csv') %>% 
  select(-X) %>% 
  mutate(submission.category_totality = case_when(submission.category_totality == 'before' ~ 'Before',
                                                  submission.category_totality == 'during' ~ 'Totality',
                                                  submission.category_totality == 'after' ~ 'After',
                                                  TRUE ~ NA))

## create other dataframe
# want counts of other vs no other across time points
# need to rename variables for use with glm
df.other = totality_data2 %>% 
  select(submission.category_totality,
         other_new) %>% 
  table() %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = other_new,
              values_from = Freq) %>% 
  rename(yes = '1') %>% 
  rename(no = '0')


## graph results
# use proportion
df.other %>% 
  pivot_longer(cols = c('yes',
                        'no'),
               names_to = 'type',
               values_to = 'freq') %>% 
  group_by(submission.category_totality) %>% 
  mutate(total = sum(freq)) %>% 
  ungroup() %>% 
  mutate(Proportion = 100*freq/total) %>% 
  ggplot(aes(x = factor(submission.category_totality,
                        levels = c("Before", 
                                   "Totality",
                                   "After")),
             y = Proportion,
             fill = paste0(type,
                           submission.category_totality))) +
  # ggplot(aes(x = submission.category_totality,
  #            y = Proportion,
  #            fill = type)) +
  geom_bar(position = 'stack',
           stat = 'identity') +
  scale_fill_manual(values = c('#F3F3F3',
                               '#F3F3F3',
                               '#F3F3F3',
                               '#FFC000',
                               '#B21912',
                               '#404040')) +
  # ggtitle("d) Other") +
  theme_classic(base_size = 12) +
  # xlab('Period of observation') +
  # ylab('Proportion of total observations') +
  # theme(axis.text = element_text(size = 12)) +
  # theme(axis.title = element_text(size = 14)) +
  theme(legend.position = 'none') +
  theme(axis.title = element_blank()) 


ggplot2::ggsave("figures/paper/liz/other.png",
                height = 3,
                width = 3,
                units = 'in')

# ----- Evan --------------------------------------------------

evan <- read.csv("data/data_raw_Liz/evan counts.csv") 

# plot counts
ggplot(evan, 
       aes(x = spp_jwatcher,
           y = counts)) + 
  # geom_boxplot(width = .8) + 
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               binwidth = 6,
               stackgroups = TRUE,
               binpositions = "all", 
               dotsize = 1.75,
               fill = "#404040",
               color = "black") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(x = "Species", 
       y = "Counts")

ggsave("figures/paper/liz/evan repeatability.pdf",
       width = 6.5, 
       height = 4)


