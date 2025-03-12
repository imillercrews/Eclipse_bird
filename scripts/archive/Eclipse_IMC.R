#### Eclipse bird analysis 
### Audio recording data 
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
# ## install changepoint.np
# install.packages('changepoint.np')

# load library
# library(changepoint.np)
library(gtools)
# library(rstatix)
# library(boot)
library(tidyverse)

#### load data ####
## load species
data.species = read.delim('data/52 spp.txt') %>% 
  rename(Common.Name = Level)

# make dataframe of species name and species code
data.species.initials = read.csv('data/Combo_4.8_AFTERNOON_Selection_Table_data.csv') %>% 
  select(Common.Name,
         Species.Code) %>% 
  distinct() %>% 
  filter(Common.Name %in% data.species$Common.Name)

## load data
# add presence column
# add date
# 8th
data.8 = read.csv('data/Combo_4.8_AFTERNOON_Selection_Table_data.csv') %>% 
  dplyr::select(Recorder,
                File.name,
                Adjusted.Begin.Time..s.,
                Common.Name,
                Confidence) %>% 
  rename(Time = Adjusted.Begin.Time..s.) %>% 
  mutate(Present = 1,
         Date = 8) 

# 7th
# change: Adjust.Begin.Time..s.
data.7 = read.csv('data/Combo_4.7_AFTERNOON_Results_Selection_Table_data.csv') %>% 
  dplyr::select(Recorder,
                File.name,
                Adjust.Begin.Time..s.,
                Common.Name,
                Confidence) %>% 
  rename(Time = Adjust.Begin.Time..s.) %>% 
  mutate(Present = 1,
         Date = 7) 

# 6th
data.6 = read.csv('data/Combo_4.6_AFTERNOON_Selection_Table_data.csv') %>% 
  dplyr::select(Recorder,
                File.name,
                Adjusted.Begin.Time..s.,
                Common.Name,
                Confidence) %>% 
  rename(Time = Adjusted.Begin.Time..s.) %>% 
  mutate(Present = 1,
         Date = 6) 

## combine all data together
data = data.8 %>% 
  rbind(data.7) %>% 
  rbind(data.6)

# filter to 52 species list
data = data %>% 
  filter(Common.Name %in% data.species$Common.Name) %>% 
  droplevels()

# check overlap 
# no missing species
data.species$Common.Name %>% 
  symdiff(data$Common.Name %>% 
              unique())

# check recorders across days
data %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()
# missing day 6 data for OWU11 and OWU6


## create sum across recorders
data.sum = data %>% 
  select(Common.Name,
         Time,
         Present,
         Date) %>% 
  group_by(Time,
           Common.Name,
           Date) %>% 
  summarize(Present.all = sum(Present)) %>% 
  ungroup() %>% 
  droplevels()

# create sum with recorders removed
data.sum.filter = data %>% 
  filter(!Recorder %in% c('OWU11',
                          'OWU6')) %>% 
  select(Common.Name,
         Time,
         Present,
         Date) %>% 
  group_by(Time,
           Common.Name,
           Date) %>% 
  summarize(Present.all = sum(Present)) %>% 
  ungroup() %>% 
  droplevels()

### dawn data
# 8th
data.8.dawn = read.csv('data/Combo PREDAWN and DAWN Results(2h predawn dawn).csv') %>% 
  dplyr::select(Recorder,
                File.name,
                Begin.Time..s.,
                Common.Name,
                Confidence) %>% 
  rename(Time = Begin.Time..s.) %>% 
  mutate(Present = 1,
         Date = 8)  


# filter to 52 species list
data.dawn = data.8.dawn %>% 
  filter(Common.Name %in% data.species$Common.Name) %>% 
  droplevels()

# check overlap 
# no missing species
data.species$Common.Name %>% 
  symdiff(data.dawn$Common.Name %>% 
            unique())

# check recorders across days
data.dawn %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()

## create sum across recorders
data.sum.dawn = data.dawn %>% 
  select(Common.Name,
         Time,
         Present,
         Date) %>% 
  group_by(Time,
           Common.Name,
           Date) %>% 
  summarize(Present.all = sum(Present)) %>% 
  ungroup() %>% 
  droplevels()

### dusk data
# 8th
data.dusk = read.csv('data/4.6_4.7_DUSK_Complete_Selection_Table.csv') %>% 
  dplyr::select(Confidence,
                Common.Name,
                Begin.Path,
                File.Offset..s.) %>% 
  mutate(Present = 1,
         Time.day = 'dusk')  


# filter to 52 species list
data.dusk = data.dusk %>% 
  filter(Common.Name %in% data.species$Common.Name) %>% 
  droplevels()

# extract time, date, and recorder
data.dusk = data.dusk %>%
    mutate(Real.time.start = str_sub(Begin.Path,
                                     start = -10,
                                     end = -5),
           Date.exact = str_sub(Begin.Path,
                                     start = -19,
                                     end = -12),
           Recorder = str_sub(Begin.Path,
                          end = -21),
           Recorder = str_sub(Recorder,
                                start = 62),
           Date = case_when(Date.exact == '20240406' ~ 6,
                            Date.exact == '20240407' ~ 7,
                            Date.exact == '20240408' ~ 8),
           Real.time.start.hour = str_sub(Real.time.start,
                                          end = 2) %>% 
             as.numeric(),
           Real.time.start.min = str_sub(Real.time.start,
                                          start = 3,
                                         end = 4) %>% 
             as.numeric(),
           Real.time.start.s = str_sub(Real.time.start,
                                          start = 5,
                                       end = 6) %>% 
             as.numeric(),
           Real.time.seconds = Real.time.start.hour*60*60 +
                                   Real.time.start.min*60 +
                                   Real.time.start.s,
           Real.time = Real.time.seconds + File.Offset..s.) %>% 
    select(-c(Real.time.start,
              Real.time.start.s,
              Real.time.start.min,
              Real.time.start.hour,
              Real.time.seconds,
              File.Offset..s.,
              Date.exact))



# check overlap 
# no missing species
data.species$Common.Name %>% 
  symdiff(data.dusk$Common.Name %>% 
            unique())

# check recorders across days
data.dusk %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()

## create sum across recorders
data.sum.dusk = data.dusk %>% 
  select(Common.Name,
         Real.time,
         Present,
         Date,
         Time.day) %>% 
  group_by(Time.day,
           Real.time,
           Common.Name,
           Date) %>% 
  summarize(Present.all = sum(Present)) %>% 
  ungroup() %>% 
  droplevels()


#### calculate moving window species ####
### calculate rolling average
## create null dataframe
data.sum.null = expand.grid(Common.Name = unique(data.sum$Common.Name),
                             Time = unique(data.sum$Time),
                            Date = unique(data.sum$Date))

# moving window sum across 4 minutes with shift of 1 minute
data.sum.win = data.sum  %>% 
  full_join(data.sum.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                          0,
                          Present.all)) %>% 
  arrange(Time) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = running(Present.all,
                                 width = 4*20,
                                 fun = sum,
                                 align = 'left',
                                 allow.fewer = TRUE,
                                 by = 1)) 


## graph running sum
# loop through all species
for (i in unique(data.sum.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = 3600,
             xmax = 3840,
             ymin = 0,
             ymax = data.sum.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1,
             color = 'grey') +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(0,
         max(data.sum.win$Time)) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species/',
                i, 
                ' calls moving sum.png'))
}

# filter to 1 hour window 
# loop through all species
for (i in unique(data.sum.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.win %>%
    filter(Common.Name == i) %>% 
    filter(Time > 1800) %>% 
    filter(Time < 5640) %>% 
    ggplot() +
    annotate("rect",
             xmin = 3600,
             xmax = 3840,
             ymin = 0,
             ymax = data.sum.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1,
             color = 'grey') +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(1800,
         5640) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_1hour/',
                i, 
                ' calls moving sum 1hour.png'))
}

#### use filtered recorders
### calculate rolling average
## create null dataframe
data.sum.filter.null = expand.grid(Common.Name = unique(data.sum.filter$Common.Name),
                             Time = unique(data.sum.filter$Time),
                            Date = unique(data.sum.filter$Date))

# moving window sum across 4 minutes with shift of 1 minute
data.sum.filter.win = data.sum.filter  %>% 
  full_join(data.sum.filter.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                          0,
                          Present.all)) %>% 
  arrange(Time) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = running(Present.all,
                                 width = 4*20,
                                 fun = sum,
                                 align = 'left',
                                 allow.fewer = TRUE,
                                 by = 1)) 


## graph running sum
# loop through all species
for (i in unique(data.sum.filter.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.filter.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = 3600,
             xmax = 3840,
             ymin = 0,
             ymax = data.sum.filter.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1,
             color = 'grey') +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(0,
         max(data.sum.filter.win$Time)) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_filter/',
                i, 
                ' calls moving sum filter.png'))
}

# filter to 1 hour window 
# loop through all species
for (i in unique(data.sum.filter.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.filter.win %>%
    filter(Common.Name == i) %>% 
    filter(Time > 1800) %>% 
    filter(Time < 5640) %>% 
    ggplot() +
    annotate("rect",
             xmin = 3600,
             xmax = 3840,
             ymin = 0,
             ymax = data.sum.filter.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1,
             color = 'grey') +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(1800,
         5640) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_filter_1hour/',
                i, 
                ' calls moving sum filter 1hour.png'))
}

#### use dawn recorders
### calculate rolling average
## create null dataframe
data.sum.dawn.null = expand.grid(Common.Name = unique(data.sum.dawn$Common.Name),
                                   Time = unique(data.sum.dawn$Time),
                                   Date = unique(data.sum.dawn$Date))

# moving window sum across 4 minutes with shift of 1 minute
data.sum.dawn.win = data.sum.dawn  %>% 
  full_join(data.sum.dawn.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                              0,
                              Present.all)) %>% 
  arrange(Time) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = running(Present.all,
                                 width = 4*20,
                                 fun = sum,
                                 align = 'left',
                                 allow.fewer = TRUE,
                                 by = 1)) 


## graph running sum
# loop through all species
for (i in unique(data.sum.dawn.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.dawn.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    geom_vline(xintercept = 2848) +
    geom_vline(xintercept = 4868) +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(0,
         max(data.sum.dawn.win$Time)) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_dawn/',
                i, 
                ' calls moving sum dawn.png'))
}

# dawn to 1 hour window 
# loop through all species
for (i in unique(data.sum.dawn.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.dawn.win %>%
    filter(Common.Name == i) %>% 
    filter(Time > 1800) %>% 
    filter(Time < 5640) %>% 
    ggplot() +
    geom_vline(xintercept = 2848) +
    geom_vline(xintercept = 4868) +
    geom_line(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (s)') +
    ylab('Calls per four minute window') +
    xlim(1800,
         5640) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_dawn_1hour/',
                i, 
                ' calls moving sum dawn 1hour.png'))
}

#### use dusk recorders
### calculate rolling average
## create null dataframe
data.sum.dusk.null = expand.grid(Common.Name = unique(data.sum.dusk$Common.Name),
                                 Real.time = unique(data.sum.dusk$Real.time),
                                 Date = unique(data.sum.dusk$Date),
                                 Time.day = unique(data.sum.dusk$Time.day))


# moving window sum across 4 minutes with shift of 1 minute
data.sum.dusk.win = data.sum.dusk  %>% 
  full_join(data.sum.dusk.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                              0,
                              Present.all)) %>% 
  arrange(Real.time) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = running(Present.all,
                                 width = 4*20,
                                 fun = sum,
                                 align = 'left',
                                 allow.fewer = TRUE,
                                 by = 1)) 


## graph running sum
# loop through all species
for (i in unique(data.sum.dusk.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.sum.dusk.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    geom_vline(xintercept = 72960) +
    geom_vline(xintercept = 73020) +
    geom_vline(xintercept = 74580) +
    geom_vline(xintercept = 74640) +
    geom_line(aes(x = Real.time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Real.time (s)') +
    ylab('Calls per four minute window') +
    xlim(19*60*60,
         21*60*60) +
    ggtitle(i) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') 
  ggsave(paste0('figures/all_sp/moving_sum_species_dusk/',
                i,
                ' calls moving sum dusk.png'))
}
  
  
# test different size moving windows
  for (j in c(20, 80, 200, 300, 600)) {
  
  # moving window sum across 4 minutes with shift of 1 minute
  tmp = data.sum.dusk  %>% 
    full_join(data.sum.dusk.null) %>% 
    mutate(Present.all = ifelse(is.na(Present.all),
                                0,
                                Present.all)) %>% 
    arrange(Real.time) %>% 
    group_by(Common.Name,
             Date) %>% 
    mutate(species.count = running(Present.all,
                                   width = j,
                                   fun = sum,
                                   align = 'left',
                                   allow.fewer = TRUE,
                                   by = 1)) 
  
  
  ## graph running sum
  # loop through all species
  # for (i in unique(data.sum.dusk.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  tmp %>%
    filter(Common.Name == "American Robin") %>% 
    ggplot() +
    geom_vline(xintercept = 72960) +
    geom_vline(xintercept = 73020) +
    geom_vline(xintercept = 74580) +
    geom_vline(xintercept = 74640) +
    geom_line(aes(x = Real.time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Real.time (s)') +
    ylab('Calls per four minute window') +
    xlim(19*60*60,
         21*60*60) +
    ggtitle(paste0("American Robin",
                   ": ",
                   j/20,
                   " minutes")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date')
    ggsave(paste0('figures/all_sp/moving_sum_species_dusk_win_test/',
                  "American Robin ",
                  j/20,
                  ' min calls moving sum dusk.png'))
  
}

#### distance matrix ####
# use dtwDist to calculate similartiy between species
# https://www.rdocumentation.org/packages/dtw/versions/1.23-1/topics/dtwDist
# https://rtavenar.github.io/blog/dtw.html

# install.packages('dtw')
library(dtw)
library(pvclust)

### afternoon
## convert data to matrix with timeseries as rows for each species
# filter down to 1 hour window
data.sum.win.1hour.mat = data.sum.win %>% 
  filter(Time > 1800) %>% 
  filter(Time < 5640) %>% 
  mutate(ID = paste(Common.Name,
                    Date,
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Time,
              values_from = species.count) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

data.sum.win.mat[1:5,1:5]
str(data.sum.win.mat)


### calculate dtw dissimilarity between species
data.sum.win.1hour.mat = dtwDist(data.sum.win.1hour.mat)

str(data.sum.win.1hour.mat)
View(data.sum.win.1hour.mat)

# save matrix results
write.csv(data.sum.win.1hour.mat,
     'data/data.sum.win.1hour.mat.csv')

#### run with scaled data
### scale data
## convert data to matrix with timeseries as rows for each species
# filter down to 1 hour window
# convert NA to 0
data.sum.win.1hour.mat.scale = data.sum.win %>% 
  filter(Time > 1800) %>% 
  filter(Time < 5640) %>% 
  mutate(ID = paste(Common.Name,
                    Date,
                    sep = "_")) %>% 
  group_by(Date,
           Common.Name) %>% 
  mutate(species.count.scale = species.count/max(species.count),
         species.count.scale = ifelse(is.na(species.count.scale),
                                      0,
                                      species.count.scale)) %>% 
  pivot_wider(id_cols = ID,
              names_from = Time,
              values_from = species.count.scale) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

data.sum.win.1hour.mat.scale[1:5,1:5]
str(data.sum.win.1hour.mat.scale)


### calculate dtw dissimilarity between species
data.sum.win.1hour.mat.scale = dtwDist(data.sum.win.1hour.mat.scale)

str(data.sum.win.1hour.mat.scale)
View(data.sum.win.1hour.mat.scale)

# save matrix results
write.csv(data.sum.win.1hour.mat.scale,
          'data/data.sum.win.1hour.mat.scale.csv')



### dawn
## run with scaled data
## 1 hour
## convert data to matrix with timeseries as rows for each species
# filter down to 1 hour window
# convert NA to 0
data.sum.dawn.win.1hour.mat.scale = data.sum.dawn.win %>% 
  filter(Time > 1800) %>% 
  filter(Time < 5640) %>% 
  mutate(ID = paste(Common.Name,
                    Date,
                    sep = "_")) %>% 
  group_by(Date,
           Common.Name) %>% 
  mutate(species.count.scale = species.count/max(species.count),
         species.count.scale = ifelse(is.na(species.count.scale),
                                      0,
                                      species.count.scale)) %>% 
  pivot_wider(id_cols = ID,
              names_from = Time,
              values_from = species.count.scale) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

data.sum.dawn.win.1hour.mat.scale[1:5,1:5]
str(data.sum.dawn.win.1hour.mat.scale)


### calculate dtw dissimilarity between species
data.sum.dawn.win.1hour.mat.scale = dtwDist(data.sum.dawn.win.1hour.mat.scale)

str(data.sum.dawn.win.1hour.mat.scale)
View(data.sum.dawn.win.1hour.mat.scale)

# save matrix results
write.csv(data.sum.dawn.win.1hour.mat.scale,
          'data/data.sum.dawn.win.1hour.mat.scale.csv')

## 2 hour
## convert data to matrix with timeseries as rows for each species
# filter down to 1 hour window
# convert NA to 0
data.sum.dawn.win.2hour.mat.scale = data.sum.dawn.win %>% 
  mutate(ID = paste(Common.Name,
                    Date,
                    sep = "_")) %>% 
  group_by(Date,
           Common.Name) %>% 
  mutate(species.count.scale = species.count/max(species.count),
         species.count.scale = ifelse(is.na(species.count.scale),
                                      0,
                                      species.count.scale)) %>% 
  pivot_wider(id_cols = ID,
              names_from = Time,
              values_from = species.count.scale) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

data.sum.dawn.win.2hour.mat.scale[1:5,1:5]
str(data.sum.dawn.win.2hour.mat.scale)


### calculate dtw dissimilarity between species
data.sum.dawn.win.2hour.mat.scale = dtwDist(data.sum.dawn.win.2hour.mat.scale)

str(data.sum.dawn.win.2hour.mat.scale)
View(data.sum.dawn.win.2hour.mat.scale)

# save matrix results
write.csv(data.sum.dawn.win.2hour.mat.scale,
          'data/data.sum.dawn.win.2hour.mat.scale.csv')



#### load dtwdist data ####
## afternoon
# calls
data.sum.win.1hour.mat = read.csv('data/data.sum.win.1hour.mat.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# scale
data.sum.win.1hour.mat.scale = read.csv('data/data.sum.win.1hour.mat.scale.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# dawn
# scale, 2 hour
data.sum.dawn.win.2hour.mat.scale = read.csv('data/data.sum.dawn.win.2hour.mat.scale.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# scale, 1 hour
data.sum.dawn.win.1hour.mat.scale = read.csv('data/data.sum.dawn.win.1hour.mat.scale.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()


#### graph cluster ####
# ### hclust
# # create distance matrix
# species.dist.clust = hclust(as.dist(data.sum.win.1hour.mat))
# 
# # graph clustering
# plot(species.dist.clust)

### pvclust
species.dist.pvclust = pvclust(data.sum.win.1hour.mat)

# just afternoon of eclipse
species.dist.pvclust.8 = data.sum.win.1hour.mat %>% 
  subset(grepl("_8",
               rownames(data.sum.win.1hour.mat)),
         grepl("_8",
               rownames(data.sum.win.1hour.mat))) %>% 
  pvclust()

# just afternoon of day 6
species.dist.pvclust.6 = data.sum.win.1hour.mat %>% 
  subset(grepl("_6",
               rownames(data.sum.win.1hour.mat)),
         grepl("_6",
               rownames(data.sum.win.1hour.mat))) %>% 
  pvclust()

# just afternoon of day 7
species.dist.pvclust.7 = data.sum.win.1hour.mat %>% 
  subset(grepl("_7",
               rownames(data.sum.win.1hour.mat)),
         grepl("_7",
               rownames(data.sum.win.1hour.mat))) %>% 
  pvclust()

# seplot(species.dist.pvclust)
# species.dist.pvclust.sep <- seplot(species.dist.pvclust,
#             identify=TRUE)
# msplot(species.dist.pvclust)

## graph pvclust
# all
pdf('figures/all_sp/cluster/species moving window dtwdist all afternoon.pdf')
plot(species.dist.pvclust)
pvrect(species.dist.pvclust,
       alpha=0.95)
dev.off()

# afternoon during eclipse 
pdf('figures/all_sp/cluster/species moving window dtwdist afternoon 8.pdf')
plot(species.dist.pvclust.8)
pvrect(species.dist.pvclust.8,
       alpha=0.95)
dev.off()

# afternoon during 6 
pdf('figures/all_sp/cluster/species moving window dtwdist afternoon 6.pdf')
plot(species.dist.pvclust.6)
pvrect(species.dist.pvclust.6,
       alpha=0.95)
dev.off()

# afternoon during 7 
pdf('figures/all_sp/cluster/species moving window dtwdist afternoon 7.pdf')
plot(species.dist.pvclust.7)
pvrect(species.dist.pvclust.7,
       alpha=0.95)
dev.off()

### compare days for each species 
for (i in unique(data.sum.win$Common.Name)) {
  # subset matrix
  # run hclust
  tmp = data.sum.win.1hour.mat %>% 
    subset(grepl(i,
                 rownames(data.sum.win.1hour.mat)),
           grepl(i,
                 rownames(data.sum.win.1hour.mat))) %>% 
    as.dist() %>% 
    hclust()
  
  # graph
  pdf(paste0('figures/all_sp/cluster/species/',
             i,
             ' moving window 1hour dtwdist.pdf'))
  plot(tmp,
       main = i)
  dev.off()
}






#### graph species in categories ####
## cluster down
# each day
data.sum.win %>%
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'House Sparrow',
                            'Northern Cardinal',
                            'Tufted Titmouse')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot() +
  geom_line(aes(x = Time,
                y = species.count,
                group = Common.Name
  ),
  linewidth = 2,
  color = 'grey') +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count),
              color = 'orange') +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.sum.win$Time),
       max(data.sum.win$Time)) +
  ggtitle('Cluster down') +
  facet_grid(Date~.)
ggsave('figures/all_sp/cluster/Cluster down songs over time.png')

# each day
data.sum.win %>%
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'House Sparrow',
                            'Northern Cardinal',
                            'Tufted Titmouse')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot()  +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date))) +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.sum.win$Time),
       max(data.sum.win$Time)) +
  ggtitle('Cluster down') 
ggsave('figures/all_sp/cluster/Cluster down songs over time date lm.png')

# 1 hour
data.sum.win %>% 
  filter(Time > 1800) %>% 
  filter(Time < 5640) %>% 
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'House Sparrow',
                            'Northern Cardinal',
                            'Tufted Titmouse')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot()  +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date))) +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(1800,
       5640) +
  ggtitle('Cluster down') 
ggsave('figures/all_sp/cluster/Cluster down songs over time date lm 1hour.png')





#### graph cluster scale ####
# ### hclust
# # create distance matrix
# species.dist.clust = hclust(as.dist(data.sum.win.1hour.mat.scale))
# 
# # graph clustering
# plot(species.dist.clust)

### pvclust
species.dist.scale.pvclust = pvclust(data.sum.win.1hour.mat.scale)

# just afternoon of eclipse
species.dist.scale.pvclust.8 = data.sum.win.1hour.mat.scale %>% 
  subset(grepl("_8",
               rownames(data.sum.win.1hour.mat.scale)),
         grepl("_8",
               rownames(data.sum.win.1hour.mat.scale))) %>% 
  pvclust()

# just afternoon of day 6
species.dist.scale.pvclust.6 = data.sum.win.1hour.mat.scale %>% 
  subset(grepl("_6",
               rownames(data.sum.win.1hour.mat.scale)),
         grepl("_6",
               rownames(data.sum.win.1hour.mat.scale))) %>% 
  pvclust()

# just afternoon of day 7
species.dist.scale.pvclust.7 = data.sum.win.1hour.mat.scale %>% 
  subset(grepl("_7",
               rownames(data.sum.win.1hour.mat.scale)),
         grepl("_7",
               rownames(data.sum.win.1hour.mat.scale))) %>% 
  pvclust()

# seplot(species.dist.scale.pvclust)
# species.dist.scale.pvclust.sep <- seplot(species.dist.scale.pvclust,
#             identify=TRUE)
# msplot(species.dist.scale.pvclust)

## graph pvclust
# all
pdf('figures/all_sp/cluster_scale/species moving window dtwdist all afternoon scale.pdf')
plot(species.dist.scale.pvclust)
pvrect(species.dist.scale.pvclust,
       alpha=0.95)
dev.off()

# afternoon during eclipse 
pdf('figures/all_sp/cluster_scale/species moving window dtwdist afternoon 8 scale.pdf')
plot(species.dist.scale.pvclust.8)
pvrect(species.dist.scale.pvclust.8,
       alpha=0.95)
dev.off()

# afternoon during 6 
pdf('figures/all_sp/cluster_scale/species moving window dtwdist afternoon 6 scale.pdf')
plot(species.dist.scale.pvclust.6)
pvrect(species.dist.scale.pvclust.6,
       alpha=0.95)
dev.off()

# afternoon during 7 
pdf('figures/all_sp/cluster_scale/species moving window dtwdist afternoon 7 scale.pdf')
plot(species.dist.scale.pvclust.7)
pvrect(species.dist.scale.pvclust.7,
       alpha=0.95)
dev.off()

### compare days for each species 
for (i in unique(data.sum.win$Common.Name)) {
  # subset matrix
  # run hclust
  tmp = data.sum.win.1hour.mat.scale %>% 
    subset(grepl(i,
                 rownames(data.sum.win.1hour.mat.scale)),
           grepl(i,
                 rownames(data.sum.win.1hour.mat.scale))) %>% 
    as.dist() %>% 
    hclust()
  
  # graph
  pdf(paste0('figures/all_sp/cluster_scale/species/',
             i,
             ' moving window 1hour dtwdist scale.pdf'))
  plot(tmp,
       main = i)
  dev.off()
}






#### graph species in categories scale ####
## cluster down
# each day
data.sum.win %>%
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'Mourning Dove',
                            'Northern Cardinal',
                            'Cedar Waxwing')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot() +
  geom_line(aes(x = Time,
                y = species.count,
                group = Common.Name
  ),
  linewidth = 2,
  color = 'grey') +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count),
              color = 'orange') +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.sum.win$Time),
       max(data.sum.win$Time)) +
  ggtitle('Cluster TRES') +
  facet_grid(Date~.)
ggsave('figures/all_sp/cluster_scale/Cluster TRES songs over time scale.png')

# each day
data.sum.win %>%
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'Mourning Dove',
                            'Northern Cardinal',
                            'Cedar Waxwing')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot()  +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date))) +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.sum.win$Time),
       max(data.sum.win$Time)) +
  ggtitle('Cluster down') 
ggsave('figures/all_sp/cluster_scale/Cluster TRES songs over time date lm.png')

# 1 hour
data.sum.win %>% 
  filter(Time > 1800) %>% 
  filter(Time < 5640) %>% 
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'Mourning Dove',
                            'Northern Cardinal',
                            'Cedar Waxwing')) %>% 
  group_by(Common.Name,
           Date) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot()  +
  annotate("rect",
           xmin = 3600,
           xmax = 3840,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = Time,
                  y = species.count,
                  group = Date,
                  color = as.factor(Date))) +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(1800,
       5640) +
  ggtitle('Cluster down') 
ggsave('figures/all_sp/cluster_scale/Cluster TRES songs over time date lm 1hour scale.png')





#### graph cluster scale dawn 1hour ####
### pvclust
# species.dist.dawn.scale.pvclust.1hour = pvclust(data.sum.dawn.win.1hour.mat.scale)

# just dawn of eclipse
species.dist.dawn.scale.pvclust.1hour.8 = data.sum.dawn.win.1hour.mat.scale %>% 
  subset(grepl("_8",
               rownames(data.sum.dawn.win.1hour.mat.scale)),
         grepl("_8",
               rownames(data.sum.dawn.win.1hour.mat.scale))) %>% 
  pvclust()

# # just dawn of day 6
# species.dist.dawn.scale.pvclust.1hour.6 = data.sum.dawn.win.1hour.mat.scale %>% 
#   subset(grepl("_6",
#                rownames(data.sum.dawn.win.1hour.mat.scale)),
#          grepl("_6",
#                rownames(data.sum.dawn.win.1hour.mat.scale))) %>% 
#   pvclust()
# 
# # just dawn of day 7
# species.dist.dawn.scale.pvclust.1hour.7 = data.sum.dawn.win.1hour.mat.scale %>% 
#   subset(grepl("_7",
#                rownames(data.sum.dawn.win.1hour.mat.scale)),
#          grepl("_7",
#                rownames(data.sum.dawn.win.1hour.mat.scale))) %>% 
#   pvclust()

# ## graph pvclust
# # all
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist all dawn 1hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.1hour)
# pvrect(species.dist.dawn.scale.pvclust.1hour,
#        alpha=0.95)
# dev.off()

# dawn during eclipse 
pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 8 1hour scale.pdf')
plot(species.dist.dawn.scale.pvclust.1hour.8)
pvrect(species.dist.dawn.scale.pvclust.1hour.8,
       alpha=0.95)
dev.off()

# # dawn during 6 
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 6 1hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.1hour.6)
# pvrect(species.dist.dawn.scale.pvclust.1hour.6,
#        alpha=0.95)
# dev.off()
# 
# # dawn during 7 
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 7 1hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.1hour.7)
# pvrect(species.dist.dawn.scale.pvclust.1hour.7,
#        alpha=0.95)
# dev.off()

# ### compare days for each species 
# for (i in unique(data.sum.dawn.win$Common.Name)) {
#   # subset matrix
#   # run hclust
#   tmp = data.sum.dawn.win.1hour.mat.scale %>% 
#     subset(grepl(i,
#                  rownames(data.sum.dawn.win.1hour.mat.scale)),
#            grepl(i,
#                  rownames(data.sum.dawn.win.1hour.mat.scale))) %>% 
#     as.dist() %>% 
#     hclust()
#   
#   # graph
#   pdf(paste0('figures/all_sp/cluster_scale_dawn/species_1hour/',
#              i,
#              ' moving window 1hour dtwdist scale.pdf'))
#   plot(tmp,
#        main = i)
#   dev.off()
# }



## 8th
pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 8 1hour scale cluster.pdf')
species.dist.dawn.scale.pvclust.1hour.8.rename.cut <- cutree(species.dist.dawn.scale.pvclust.1hour.8.rename$hclust ,
                                                             k = 12, 
                                                             order_clusters_as_data = FALSE) 

color_branches(species.dist.dawn.scale.pvclust.1hour.8.rename$hclust,
               k = 12,
               col = palette(rainbow(13)))  %>% 
  plot()
colored_bars(colors = species.dist.dawn.scale.pvclust.1hour.8.rename.cut,
             dend = species.dist.dawn.scale.pvclust.1hour.8.rename$hclust  , 
             sort_by_labels_order = FALSE)
dev.off()



#### graph cluster scale dawn 2hour ####
### pvclust
# species.dist.dawn.scale.pvclust.2hour = pvclust(data.sum.dawn.win.2hour.mat.scale)

# just dawn of eclipse
species.dist.dawn.scale.pvclust.2hour.8 = data.sum.dawn.win.2hour.mat.scale %>% 
  subset(grepl("_8",
               rownames(data.sum.dawn.win.2hour.mat.scale)),
         grepl("_8",
               rownames(data.sum.dawn.win.2hour.mat.scale))) %>% 
  pvclust()

# # just dawn of day 6
# species.dist.dawn.scale.pvclust.2hour.6 = data.sum.dawn.win.2hour.mat.scale %>% 
#   subset(grepl("_6",
#                rownames(data.sum.dawn.win.2hour.mat.scale)),
#          grepl("_6",
#                rownames(data.sum.dawn.win.2hour.mat.scale))) %>% 
#   pvclust()
# 
# # just dawn of day 7
# species.dist.dawn.scale.pvclust.2hour.7 = data.sum.dawn.win.2hour.mat.scale %>% 
#   subset(grepl("_7",
#                rownames(data.sum.dawn.win.2hour.mat.scale)),
#          grepl("_7",
#                rownames(data.sum.dawn.win.2hour.mat.scale))) %>% 
#   pvclust()

# ## graph pvclust
# # all
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist all dawn 2hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.2hour)
# pvrect(species.dist.dawn.scale.pvclust.2hour,
#        alpha=0.95)
# dev.off()

# dawn during eclipse 
pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 8 2hour scale.pdf')
plot(species.dist.dawn.scale.pvclust.2hour.8)
pvrect(species.dist.dawn.scale.pvclust.2hour.8,
       alpha=0.95)
dev.off()

# # dawn during 6 
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 6 2hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.2hour.6)
# pvrect(species.dist.dawn.scale.pvclust.2hour.6,
#        alpha=0.95)
# dev.off()
# 
# # dawn during 7 
# pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 7 2hour scale.pdf')
# plot(species.dist.dawn.scale.pvclust.2hour.7)
# pvrect(species.dist.dawn.scale.pvclust.2hour.7,
#        alpha=0.95)
# dev.off()

# ### compare days for each species 
# for (i in unique(data.sum.dawn.win$Common.Name)) {
#   # subset matrix
#   # run hclust
#   tmp = data.sum.dawn.win.2hour.mat.scale %>% 
#     subset(grepl(i,
#                  rownames(data.sum.dawn.win.2hour.mat.scale)),
#            grepl(i,
#                  rownames(data.sum.dawn.win.2hour.mat.scale))) %>% 
#     as.dist() %>% 
#     hclust()
#   
#   # graph
#   pdf(paste0('figures/all_sp/cluster_scale_dawn/species_2hour/',
#              i,
#              ' moving window 2hour dtwdist scale.pdf'))
#   plot(tmp,
#        main = i)
#   dev.off()
# }


#### calculate clusters
## 8th
pdf('figures/all_sp/cluster_scale_dawn/species moving window dtwdist dawn 8 2hour scale cluster.pdf')
species.dist.dawn.scale.pvclust.2hour.8.rename.cut <- cutree(species.dist.dawn.scale.pvclust.2hour.8.rename$hclust ,
              k = 12, 
              order_clusters_as_data = FALSE) 

color_branches(species.dist.dawn.scale.pvclust.2hour.8.rename$hclust,
               k = 12,
               col = palette(rainbow(13)))  %>% 
  plot()
colored_bars(colors = species.dist.dawn.scale.pvclust.2hour.8.rename.cut,
             dend = species.dist.dawn.scale.pvclust.2hour.8.rename$hclust  , 
             sort_by_labels_order = FALSE)
dev.off()




#### compare dawn 1hour and 2hour trees ####
library(dendextend)

### use pvclust trees from above
### for dawn on the 8th
# species.dist.dawn.scale.pvclust.2hour.8
# species.dist.dawn.scale.pvclust.1hour.8
## replace with species initials
# 1hour list
species.dist.dawn.scale.pvclust.1hour.8.order = data.frame(ID = species.dist.dawn.scale.pvclust.1hour.8$hclust$labels) %>% 
  rownames_to_column('order') %>% 
  separate_wider_delim(cols = ID,
                       delim = '_',
                       names = c('Common.Name',
                                 'Date')) %>% 
  left_join(data.species.initials) %>% 
  pull(Species.Code)

# 2hour list
species.dist.dawn.scale.pvclust.2hour.8.order = data.frame(ID = species.dist.dawn.scale.pvclust.2hour.8$hclust$labels) %>% 
  rownames_to_column('order') %>% 
  separate_wider_delim(cols = ID,
                       delim = '_',
                       names = c('Common.Name',
                                 'Date')) %>% 
  left_join(data.species.initials) %>% 
  pull(Species.Code)

# create new cluster objects with initals 
# 2hour
species.dist.dawn.scale.pvclust.2hour.8.rename = species.dist.dawn.scale.pvclust.2hour.8
species.dist.dawn.scale.pvclust.2hour.8.rename$hclust$labels = species.dist.dawn.scale.pvclust.2hour.8.order

# 1hour
species.dist.dawn.scale.pvclust.1hour.8.rename = species.dist.dawn.scale.pvclust.1hour.8
species.dist.dawn.scale.pvclust.1hour.8.rename$hclust$labels = species.dist.dawn.scale.pvclust.1hour.8.order


## combine dendrograms to list
dends_dawn_1vs2hour <- dendlist(species.dist.dawn.scale.pvclust.2hour.8.rename %>% 
                                  as.dendrogram(), 
                                species.dist.dawn.scale.pvclust.1hour.8.rename %>% 
                          as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_dawn_1vs2hour.untangle <- dends_dawn_1vs2hour %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_sp/cluster_scale_dawn/1hour vs 2hour species moving window dtwdist dawn 8 scale.pdf')
dends_dawn_1vs2hour.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '2hour',
             main_right = '1hour',
             k_labels = 12,
             # k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_dawn_1vs2hour.untangle), 2)))
dev.off()




