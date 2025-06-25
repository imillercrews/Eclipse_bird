#### Eclipse bird analysis 
### Audio recording data 
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
# load library
library(gtools)
library(tidyverse)
library(ggbreak)
library(dendextend)
# install.packages('dtw')
library(dtw)
library(pvclust)
# install.packages("CausalImpact")
library(CausalImpact)

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

# collapse species Common.name to add '.' instead of ' '
data.species.initials.collapse = data.species.initials %>% 
  mutate(Common.Name = str_replace(Common.Name,
                                   " ",
                                   ".")) %>% 
  mutate(Common.Name = str_replace(Common.Name,
                                   "-",
                                   "."))


## load data
# filtered and summed data
data.filter.sum = read.csv('data/data_output/data_all_combined_filter_sum.csv') %>% 
  select(-c(X))

# filtered
data.filter = read.csv('data/data_output/data_all_combined_filter.csv') %>% 
  select(-c(X))

#### calculate moving window species and recorders ####
### calculate rolling average
## create null dataframe
data.null = expand.grid(Common.Name = unique(data.filter$Common.Name),
                            Real.time = data.filter %>% 
                              filter(Time.day == 'dawn') %>% 
                              pull(Real.time) %>% 
                              unique(),
                        Recorder = data.filter %>% 
                          filter(Time.day == 'dawn') %>% 
                          pull(Recorder) %>% 
                          unique(),
                            Date = c(7,
                                     8),
                            Time.day = 'dawn') %>% 
  rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
                    Real.time = data.filter %>% 
                      filter(Time.day == 'dusk') %>% 
                      pull(Real.time) %>% 
                      unique(),
                    Recorder = data.filter %>% 
                      filter(Time.day == 'dusk') %>% 
                      pull(Recorder) %>% 
                      unique(),
                    Date = c(7,
                             6),
                    Time.day = 'dusk')) %>% 
  rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
                    Real.time = data.filter %>% 
                      filter(Time.day == 'afternoon') %>% 
                      pull(Real.time) %>% 
                      unique(),
                    Recorder = data.filter %>% 
                      filter(Time.day == 'afternoon') %>% 
                      pull(Recorder) %>% 
                      unique(),
                    Date = c(6,
                             7,
                             8),
                    Time.day = 'afternoon'))


### 4 minutes
# moving window sum across 4 minutes with shift of 4 minute
data.filter.4.win = data.filter  %>% 
  select(Present,
         Common.Name,
         Date,
         Time.day,
         Recorder,
         Real.time) %>% 
  full_join(data.null) %>% 
  mutate(Present = ifelse(is.na(Present),
                              0,
                          Present)) %>% 
  
  arrange(Real.time) %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Recorder) %>% 
  mutate(species.count = running(Present,
                                 width = 4*20,
                                 fun = sum,
                                 pad = T,
                                 by = 4*20
  ))  %>% 
  ungroup() %>% 
  na.omit

## scale to max value
data.filter.4.win =
  data.filter.4.win %>% 
  group_by(Common.Name,
           Recorder) %>% 
  mutate(max.value = max(species.count),
         species.count.scale = species.count/max.value) %>% 
  ungroup() 

# save data
write.csv(data.filter.4.win,
          'data/data_output/data.filter.4.win.csv')
# # load data
# data.filter.4.win = read.csv('data/data_output/data.filter.4.win.csv')

# check lag in data
data.filter.4.win %>% 
  group_by(Common.Name,
           Date,
           Time.day, 
           Recorder) %>% 
  mutate(test = Real.time - lag(Real.time)) %>% 
  view()

# test
tmp = data.filter  %>% 
  select(Present,
         Common.Name,
         Date,
         Time.day,
         Recorder,
         Real.time) %>% 
  full_join(data.null) %>% 
  mutate(Present = ifelse(is.na(Present),
                          0,
                          Present)) %>% 
  
  arrange(Real.time) %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Recorder) %>% 
  mutate(species.count = running(Present,
                                 width = 4*20,
                                 fun = sum,
                                 pad = T,
                                 by = 4*20
  ))  %>% 
  ungroup()


View(tmp)

# check lag in data
tmp.2 = tmp %>% 
  group_by(Common.Name,
           Date,
           Time.day, 
           Recorder) %>% 
  mutate(test = Real.time - lag(Real.time)) 

View(tmp.2)







#### calculate moving window species ####
### calculate rolling average
## create null dataframe
data.sum.null = expand.grid(Common.Name = unique(data.filter.sum$Common.Name),
                            Real.time = data.filter.sum %>% 
                              filter(Time.day == 'dawn') %>% 
                              pull(Real.time) %>% 
                              unique(),
                            Date = c(7,
                                     8),
                            Time.day = 'dawn') %>% 
  rbind(expand.grid(Common.Name = unique(data.filter.sum$Common.Name),
                    Real.time = data.filter.sum %>% 
                      filter(Time.day == 'dusk') %>% 
                      pull(Real.time) %>% 
                      unique(),
                    Date = c(7,
                             6),
                    Time.day = 'dusk')) %>% 
  rbind(expand.grid(Common.Name = unique(data.filter.sum$Common.Name),
                    Real.time = data.filter.sum %>% 
                      filter(Time.day == 'afternoon') %>% 
                      pull(Real.time) %>% 
                      unique(),
                    Date = c(6,
                             7,
                             8),
                    Time.day = 'afternoon'))
  

### 30 minutes
# moving window sum across 30 minutes with shift of 1 minute
data.filter.sum.30.win = data.filter.sum  %>% 
  full_join(data.sum.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                              0,
                              Present.all)) %>% 
  arrange(Real.time) %>% 
  group_by(Common.Name,
           Date,
           Time.day) %>% 
  mutate(species.count = running(Present.all,
                                 width = 30*20,
                                 fun = sum,
                                 pad = T,
                                 by = 1
                                 ))  %>% 
  ungroup() %>% 
  na.omit

## scale to max value
data.filter.sum.30.win =
  data.filter.sum.30.win %>% 
  group_by(Common.Name) %>% 
  mutate(max.value = max(species.count),
         species.count.scale = species.count/max.value) %>% 
  ungroup() 

## species average across days
data.filter.sum.30.win.avg =
  data.filter.sum.30.win %>% 
  mutate(keep = ifelse(Time.day == 'afternoon' & Date == 8,
                       0,
                       1)) %>% 
  filter(keep == 1) %>% 
  select(-c(keep)) %>% 
  group_by(Common.Name,
           Time.day,
           Real.time) %>% 
  summarize(species.count.scale.avg = mean(species.count.scale)) %>% 
  ungroup() 
  

### 10 minutes
# moving window sum across 10 minutes with shift of 1 minute
data.filter.sum.10.win = data.filter.sum  %>% 
  full_join(data.sum.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                              0,
                              Present.all)) %>% 
  arrange(Real.time) %>% 
  group_by(Common.Name,
           Date,
           Time.day) %>% 
  mutate(species.count = running(Present.all,
                                 width = 10*20,
                                 fun = sum,
                                 pad = T,
                                 by = 1
  ))  %>% 
  ungroup() %>% 
  na.omit

## scale to max value
data.filter.sum.10.win =
  data.filter.sum.10.win %>% 
  group_by(Common.Name) %>% 
  mutate(max.value = max(species.count),
         species.count.scale = species.count/max.value) %>% 
  ungroup() 

#### graph running sum ####
## graph running sum 30 minute
# loop through all species
for (i in unique(data.filter.sum.30.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.filter.sum.30.win %>% 
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    geom_vline(xintercept = 24750/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 26460/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 72960/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 74580/3600,
               linetype = 'dashed') +
    geom_line(aes(x = Real.time/3600,
                  y = species.count,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 30 minute window') +
    xlim(min(data.filter.sum.30.win$Real.time)/3600,
         max(data.filter.sum.30.win$Real.time)/3600) +
    ggtitle(paste0(i,
                   ": 30 min moving sum")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave(paste0('figures/all_cluster/moving_sum_species_30/',
                i, 
                ' calls moving sum 30.png'))
}

## graph running sum 30 minute
## average
# loop through all species
for (i in unique(data.filter.sum.30.win.avg$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.filter.sum.30.win.avg %>% 
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    geom_vline(xintercept = 24750/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 26460/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 72960/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 74580/3600,
               linetype = 'dashed') +
    geom_line(aes(x = Real.time/3600,
                  y = species.count.scale.avg,
                  # group = paste0(Date, Time.day),
                  # color = as.factor(Date)
                  ),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 30 minute window average') +
    xlim(min(data.filter.sum.30.win.avg$Real.time)/3600,
         max(data.filter.sum.30.win.avg$Real.time)/3600) +
    ggtitle(paste0(i,
                   ": 30 min moving sum average")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave(paste0('figures/all_cluster/moving_sum_species_30_avg/',
                i, 
                ' calls moving sum 30 average.png'))
}

## graph running sum 10 minute
# loop through all species
for (i in unique(data.filter.sum.10.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.filter.sum.10.win %>% 
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    geom_vline(xintercept = 24750/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 26460/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 72960/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 74580/3600,
               linetype = 'dashed') +
    geom_line(aes(x = Real.time/3600,
                  y = species.count,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 10 minute window') +
    xlim(min(data.filter.sum.10.win$Real.time)/3600,
         max(data.filter.sum.10.win$Real.time)/3600) +
    ggtitle(paste0(i,
                   ": 10 min moving sum")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave(paste0('figures/all_cluster/moving_sum_species_10/',
                i, 
                ' calls moving sum 10.png'))
}




#### distance matrix ####
# use dtwDist to calculate similartiy between species
# https://www.rdocumentation.org/packages/dtw/versions/1.23-1/topics/dtwDist
# https://rtavenar.github.io/blog/dtw.html


### convert data to matrix with timeseries as rows for each species
## afternoon
data.filter.sum.30.win.avg.aft.mat = 
  data.filter.sum.30.win.avg %>% 
  filter(Time.day == 'afternoon') %>% 
  mutate(ID = paste(Common.Name,
                    'aft',
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Real.time,
              values_from = species.count.scale.avg) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

# eclipse
data.filter.sum.10.win.aft.mat =
  data.filter.sum.10.win %>% 
  filter(Time.day == 'afternoon') %>% 
    filter(Date == 8) %>% 
  mutate(ID = paste(Common.Name,
                    'aft_8',
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Real.time,
              values_from = species.count.scale) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

# eclipse, 1 hour
data.filter.sum.10.win.1hour.aft.mat =
  data.filter.sum.10.win %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date == 8) %>% 
  filter(Real.time > (54295 - 1800)) %>% 
  filter(Real.time < (54535 + 1800)) %>% 
  mutate(ID = paste(Common.Name,
                    'aft_8_1hour',
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Real.time,
              values_from = species.count.scale) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

## dawn
data.filter.sum.30.win.avg.daw.mat = 
  data.filter.sum.30.win.avg %>% 
  filter(Time.day == 'dawn') %>% 
  mutate(ID = paste(Common.Name,
                    'daw',
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Real.time,
              values_from = species.count.scale.avg) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()

## dusk
data.filter.sum.30.win.avg.dus.mat = 
  data.filter.sum.30.win.avg %>% 
  filter(Time.day == 'dusk') %>% 
  mutate(ID = paste(Common.Name,
                    'dus',
                    sep = "_")) %>% 
  pivot_wider(id_cols = ID,
              names_from = Real.time,
              values_from = species.count.scale.avg) %>% 
  column_to_rownames('ID') %>% 
  as.matrix()



### calculate dtw dissimilarity between species
## afternoon
data.filter.sum.30.win.avg.aft.mat.dist = dtwDist(data.filter.sum.30.win.avg.aft.mat,
                                                  window.type = 'sakoechiba',
                                                  window.size = 30*20)

# eclipse
data.filter.sum.10.win.aft.mat.dist = dtwDist(data.filter.sum.10.win.aft.mat,
                                                  window.type = 'sakoechiba',
                                                  window.size = 10*20)


# eclipse, 1 hour
data.filter.sum.10.win.1hour.aft.mat.dist = dtwDist(data.filter.sum.10.win.1hour.aft.mat,
                                              window.type = 'sakoechiba',
                                              window.size = 10*20)


## dawn 
data.filter.sum.30.win.avg.daw.mat.dist = dtwDist(data.filter.sum.30.win.avg.daw.mat,
                                                  window.type = 'sakoechiba',
                                                  window.size = 30*20)

## dusk 
data.filter.sum.30.win.avg.dus.mat.dist = dtwDist(data.filter.sum.30.win.avg.dus.mat,
                                                  window.type = 'sakoechiba',
                                                  window.size = 30*20)


# save matrix results
# afternoon
write.csv(data.filter.sum.30.win.avg.aft.mat.dist,
     'data/data_output/data.filter.sum.30.win.avg.aft.mat.dist.csv')

# eclipse
write.csv(data.filter.sum.10.win.aft.mat.dist,
          'data/data_output/data.filter.sum.10.win.aft.mat.dist.csv')

# eclipse, 1 hour
write.csv(data.filter.sum.10.win.1hour.aft.mat.dist,
          'data/data_output/data.filter.sum.10.win.1hour.aft.mat.dist.csv')


# dawn
write.csv(data.filter.sum.30.win.avg.daw.mat.dist,
          'data/data_output/data.filter.sum.30.win.avg.daw.mat.dist.csv')

# dusk
write.csv(data.filter.sum.30.win.avg.dus.mat.dist,
          'data/data_output/data.filter.sum.30.win.avg.dus.mat.dist.csv')


#### load dtwdist data ####
# afternoon
data.filter.sum.30.win.avg.aft.mat.dist = read.csv('data/data_output/data.filter.sum.30.win.avg.aft.mat.dist.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# eclipse
data.filter.sum.10.win.aft.mat.dist = read.csv('data/data_output/data.filter.sum.10.win.aft.mat.dist.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# eclipse, 1 hour
data.filter.sum.10.win.1hour.aft.mat.dist = read.csv('data/data_output/data.filter.sum.10.win.1hour.aft.mat.dist.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()


# dawn
data.filter.sum.30.win.avg.daw.mat.dist = read.csv('data/data_output/data.filter.sum.30.win.avg.daw.mat.dist.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

# dusk
data.filter.sum.30.win.avg.dus.mat.dist = read.csv('data/data_output/data.filter.sum.30.win.avg.dus.mat.dist.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

#### graph cluster ####
### pvclust
#afternoon
data.filter.sum.30.win.avg.aft.mat.dist.pvclust = pvclust(data.filter.sum.30.win.avg.aft.mat.dist)

# eclipse
data.filter.sum.10.win.aft.mat.dist.pvclust = pvclust(data.filter.sum.10.win.aft.mat.dist)

# eclipse, 1 hour
data.filter.sum.10.win.1hour.aft.mat.dist.pvclust = pvclust(data.filter.sum.10.win.1hour.aft.mat.dist)

#dawn
data.filter.sum.30.win.avg.daw.mat.dist.pvclust = pvclust(data.filter.sum.30.win.avg.daw.mat.dist)

#dusk
data.filter.sum.30.win.avg.dus.mat.dist.pvclust = pvclust(data.filter.sum.30.win.avg.dus.mat.dist)

## graph pvclust
# afternoon
pdf('figures/all_cluster/cluster/species 30 moving window mean dtwdist all afternoon.pdf')
plot(data.filter.sum.30.win.avg.aft.mat.dist.pvclust)
pvrect(data.filter.sum.30.win.avg.aft.mat.dist.pvclust,
       alpha=0.95)
dev.off()

# eclipse
pdf('figures/all_cluster/cluster/species 10 moving window mean dtwdist all eclipse.pdf')
plot(data.filter.sum.10.win.aft.mat.dist.pvclust)
pvrect(data.filter.sum.10.win.aft.mat.dist.pvclust,
       alpha=0.95)
dev.off()

# eclipse, 1 hour
pdf('figures/all_cluster/cluster/species 10 moving window mean dtwdist all eclipse 1 hour.pdf')
plot(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust)
pvrect(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust,
       alpha=0.95)
dev.off()

# dusk
pdf('figures/all_cluster/cluster/species 30 moving window mean dtwdist all dusk.pdf')
plot(data.filter.sum.30.win.avg.dus.mat.dist.pvclust)
pvrect(data.filter.sum.30.win.avg.dus.mat.dist.pvclust,
       alpha=0.95)
dev.off()

# dawn
pdf('figures/all_cluster/cluster/species 30 moving window mean dtwdist all dawn.pdf')
plot(data.filter.sum.30.win.avg.daw.mat.dist.pvclust)
pvrect(data.filter.sum.30.win.avg.daw.mat.dist.pvclust,
       alpha=0.95)
dev.off()

### replace labels for cluster image
## dawn
data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name = data.filter.sum.30.win.avg.daw.mat.dist.pvclust

# get labels
data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name$hclust$labels = 
  data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name$hclust$labels %>% 
  as.data.frame() %>% 
  rownames_to_column('position') %>% 
  separate_wider_delim(cols = ".",
                       delim = '_',
                       names = c('Common.Name',
                                 NA)) %>% 
  left_join(data.species.initials.collapse) %>% 
  pull(Species.Code)

## dusk
data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name = data.filter.sum.30.win.avg.dus.mat.dist.pvclust

# get labels
data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name$hclust$labels = 
  data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name$hclust$labels %>% 
  as.data.frame() %>% 
  rownames_to_column('position') %>% 
  separate_wider_delim(cols = ".",
                       delim = '_',
                       names = c('Common.Name',
                                 NA)) %>% 
  left_join(data.species.initials.collapse) %>% 
  pull(Species.Code)

## afternoon
data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name = data.filter.sum.30.win.avg.aft.mat.dist.pvclust

# get labels
data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name$hclust$labels = 
  data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name$hclust$labels %>% 
  as.data.frame() %>% 
  rownames_to_column('position') %>% 
  separate_wider_delim(cols = ".",
                       delim = '_',
                       names = c('Common.Name',
                                 NA)) %>% 
  left_join(data.species.initials.collapse) %>% 
  pull(Species.Code)

## eclipse
data.filter.sum.10.win.aft.mat.dist.pvclust.name = data.filter.sum.10.win.aft.mat.dist.pvclust

# get labels
data.filter.sum.10.win.aft.mat.dist.pvclust.name$hclust$labels = 
  data.filter.sum.10.win.aft.mat.dist.pvclust.name$hclust$labels %>% 
  as.data.frame() %>% 
  rownames_to_column('position') %>% 
  separate_wider_delim(cols = ".",
                       delim = '_',
                       names = c('Common.Name',
                                 NA,
                                 NA)) %>% 
  left_join(data.species.initials.collapse) %>% 
  pull(Species.Code)

## eclipse, 1 hour
data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name = data.filter.sum.10.win.1hour.aft.mat.dist.pvclust

# get labels
data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name$hclust$labels = 
  data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name$hclust$labels %>% 
  as.data.frame() %>% 
  rownames_to_column('position') %>% 
  separate_wider_delim(cols = ".",
                       delim = '_',
                       names = c('Common.Name',
                                 NA,
                                 NA,
                                 NA)) %>% 
  left_join(data.species.initials.collapse) %>% 
  pull(Species.Code)


## graph clusters
# dawn
pdf('figures/all_cluster/cluster/clusters species 30 moving window mean dtwdist all dawn.pdf',
    width = 10,
    height = 10)
data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name.cut <- cutree(data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name$hclust ,
                                                             k = 12, 
                                                             order_clusters_as_data = FALSE
                                                      ) 

color_branches(data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name$hclust,
               k = 12,
               col = palette(rainbow(12)))  %>% 
  plot(main = "Dawn clusters")
colored_bars(colors = data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name.cut,
             dend = data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name$hclust  ,
             sort_by_labels_order = FALSE)

dev.off()

# dusk
pdf('figures/all_cluster/cluster/clusters species 30 moving window mean dtwdist all dusk.pdf',
    width = 10,
    height = 10)
data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name.cut <- cutree(data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name$hclust ,
                                                                   k = 12, 
                                                                   order_clusters_as_data = FALSE
) 

color_branches(data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name$hclust,
               k = 12,
               col = palette(rainbow(12)))  %>% 
  plot(main = "Dusk clusters")
colored_bars(colors = data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name.cut,
             dend = data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name$hclust  ,
             sort_by_labels_order = FALSE)
dev.off()

# afternoon
pdf('figures/all_cluster/cluster/clusters species 30 moving window mean dtwdist all afternoon.pdf',
    width = 10,
    height = 10)
data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name.cut <- cutree(data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name$hclust ,
                                                                   k = 12, 
                                                                   order_clusters_as_data = FALSE
) 

color_branches(data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name$hclust,
               k = 12,
               col = palette(rainbow(12)))  %>% 
  plot(main = "Afternoon clusters")
colored_bars(colors = data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name.cut,
             dend = data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name$hclust  ,
             sort_by_labels_order = FALSE)
dev.off()

# eclipse
pdf('figures/all_cluster/cluster/clusters species 10 moving window mean dtwdist all eclipse.pdf',
    width = 10,
    height = 10)
data.filter.sum.10.win.aft.mat.dist.pvclust.name.cut <- cutree(data.filter.sum.10.win.aft.mat.dist.pvclust.name$hclust ,
                                                                   k = 5, 
                                                                   order_clusters_as_data = FALSE
) 

color_branches(data.filter.sum.10.win.aft.mat.dist.pvclust.name$hclust,
               k = 5,
               col = palette(rainbow(5)))  %>% 
  plot(main = "Eclipse clusters")
colored_bars(colors = data.filter.sum.10.win.aft.mat.dist.pvclust.name.cut,
             dend = data.filter.sum.10.win.aft.mat.dist.pvclust.name$hclust  ,
             sort_by_labels_order = FALSE)
dev.off()

# eclipse
pdf('figures/all_cluster/cluster/clusters species 10 moving window mean dtwdist all eclipse 1 hour.pdf',
    width = 10,
    height = 10)
data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name.cut <- cutree(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name$hclust ,
                                                               k = 5, 
                                                               order_clusters_as_data = FALSE
) 

color_branches(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name$hclust,
               k = 5,
               col = palette(rainbow(5)))  %>% 
  plot(main = "Eclipse clusters, 1 hour")
colored_bars(colors = data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name.cut,
             dend = data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name$hclust  ,
             sort_by_labels_order = FALSE)
dev.off()


#### compare dendrograms
### compare dawn to dusk
## combine dendrograms to list
dends_dawn_dusk <- dendlist(data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name %>% 
                                  as.dendrogram(), 
                                data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name %>% 
                                  as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_dawn_dusk.untangle <- dends_dawn_dusk %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Dawn vs Dusk clusters species 30 moving window mean dtwdist all.pdf')
dends_dawn_dusk.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = 'Dawn',
             main_right = 'Dusk',
             k_labels = 12,
             # k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_dawn_dusk.untangle), 2)))
dev.off()

### compare 1 hour eclipse to 2 hour eclipse
## combine dendrograms to list
dends_eclipse <- dendlist(data.filter.sum.10.win.aft.mat.dist.pvclust.name %>% 
                              as.dendrogram(), 
                          data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name %>% 
                              as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_eclipse.untangle <- dends_eclipse %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Eclipse 1 vs 2 hour clusters species 10 moving window dtwdist all.pdf')
dends_eclipse.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '2 hour',
             main_right = '1 hour',
             k_labels = 12,
             k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_eclipse.untangle), 2)))
dev.off()

### compare mean afternoon to 2 hour eclipse
## combine dendrograms to list
dends_eclipse_afternoon <- dendlist(data.filter.sum.10.win.aft.mat.dist.pvclust.name %>% 
                            as.dendrogram(), 
                          data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name %>% 
                            as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_eclipse_afternoon.untangle <- dends_eclipse_afternoon %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Eclipse 1 vs avg afternoon clusters species 10 moving window dtwdist all.pdf')
dends_eclipse_afternoon.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '2 hour',
             main_right = 'Avg afternoon',
             k_labels = 12,
             k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_eclipse_afternoon.untangle), 2)))
dev.off()

### compare mean afternoon to 1 hour eclipse
## combine dendrograms to list
dends_eclipse_1hour_afternoon <- dendlist(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name %>% 
                                      as.dendrogram(), 
                                    data.filter.sum.30.win.avg.aft.mat.dist.pvclust.name %>% 
                                      as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_eclipse_1hour_afternoon.untangle <- dends_eclipse_1hour_afternoon %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Eclipse 1 hour vs avg afternoon clusters species 10 moving window dtwdist all.pdf')
dends_eclipse_1hour_afternoon.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '1 hour',
             main_right = 'Avg afternoon',
             k_labels = 12,
             k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_eclipse_1hour_afternoon.untangle), 2)))
dev.off()

### compare mean dawn to 1 hour eclipse
## combine dendrograms to list
dends_eclipse_1hour_dawn <- dendlist(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name %>% 
                                            as.dendrogram(), 
                                          data.filter.sum.30.win.avg.daw.mat.dist.pvclust.name %>% 
                                            as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_eclipse_1hour_dawn.untangle <- dends_eclipse_1hour_dawn %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Eclipse 1 hour vs avg dawn clusters species 10 moving window dtwdist all.pdf')
dends_eclipse_1hour_dawn.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '1 hour',
             main_right = 'Avg dawn',
             k_labels = 12,
             k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_eclipse_1hour_dawn.untangle), 2)))
dev.off()

### compare mean dusk to 1 hour eclipse
## combine dendrograms to list
dends_eclipse_1hour_dusk <- dendlist(data.filter.sum.10.win.1hour.aft.mat.dist.pvclust.name %>% 
                                            as.dendrogram(), 
                                          data.filter.sum.30.win.avg.dus.mat.dist.pvclust.name %>% 
                                            as.dendrogram())

## Graph tanglegram
# untangle stepwise 
dends_eclipse_1hour_dusk.untangle <- dends_eclipse_1hour_dusk %>% 
  untangle(method = "step2side") 
# plotwith entanglment score
pdf('figures/all_cluster/cluster/Eclipse 1 hour vs avg dusk clusters species 10 moving window dtwdist all.pdf')
dends_eclipse_1hour_dusk.untangle %>% 
  tanglegram(common_subtrees_color_branches = F,
             main_left = '1 hour',
             main_right = 'Avg dusk',
             k_labels = 12,
             k_branches = 12,
             lab.cex = 0.75,
             main = paste("entanglement =", 
                          round(entanglement(dends_eclipse_1hour_dusk.untangle), 2)))
dev.off()

#### compare species across day ####
### calculate distance score between species across non-eclipse time of day
### dawn
data.filter.sum.30.win.daw =
  data.filter.sum.30.win %>% 
  filter(Time.day == 'dawn') 

# create empty dataframe
data.filter.sum.30.win.daw.dist = data.frame(Time.day = as.character(),
                                             Common.Name = as.character(),
                                             Dist.value = as.numeric())
  
## loop through each species
## dawn
for (i in unique(data.filter.sum.30.win.daw$Common.Name)) {
  # create tmp matrix
  tmp = 
    data.filter.sum.30.win.daw %>% 
    filter(Common.Name == i) %>% 
    mutate(ID = paste(Common.Name,
                      Date,
                      sep = "_")) %>% 
    pivot_wider(id_cols = ID,
                names_from = Real.time,
                values_from = species.count.scale) %>% 
    column_to_rownames('ID') %>% 
    as.matrix()
  
  # calculate distance
  tmp.dist = dtwDist(tmp,
                     window.type = 'sakoechiba',
                     window.size = 30*20)[1,2]
  
  ## add to matrix
  data.filter.sum.30.win.daw.dist = data.filter.sum.30.win.daw.dist %>% 
    rbind(data.frame(Time.day = 'dawn',
                     Common.Name = i,
                     Dist.value = tmp.dist))
}

### dusk
data.filter.sum.30.win.dus =
  data.filter.sum.30.win %>% 
  filter(Time.day == 'dusk') 

# create empty dataframe
data.filter.sum.30.win.dus.dist = data.frame(Time.day = as.character(),
                                             Common.Name = as.character(),
                                             Dist.value = as.numeric())

## loop through each species
## dusk
for (i in unique(data.filter.sum.30.win.dus$Common.Name)) {
  # create tmp matrix
  tmp = 
    data.filter.sum.30.win.dus %>% 
    filter(Common.Name == i) %>% 
    mutate(ID = paste(Common.Name,
                      Date,
                      sep = "_")) %>% 
    pivot_wider(id_cols = ID,
                names_from = Real.time,
                values_from = species.count.scale) %>% 
    column_to_rownames('ID') %>% 
    as.matrix()
  
  # calculate distance
  tmp.dist = dtwDist(tmp,
                     window.type = 'sakoechiba',
                     window.size = 30*20)[1,2]
  
  ## add to matrix
  data.filter.sum.30.win.dus.dist = data.filter.sum.30.win.dus.dist %>% 
    rbind(data.frame(Time.day = 'dusk',
                     Common.Name = i,
                     Dist.value = tmp.dist))
}

### afternoon
data.filter.sum.30.win.aft =
  data.filter.sum.30.win %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 8)

# create empty dataframe
data.filter.sum.30.win.aft.dist = data.frame(Time.day = as.character(),
                                             Common.Name = as.character(),
                                             Dist.value = as.numeric())

## loop through each species
## afternoon
for (i in unique(data.filter.sum.30.win.aft$Common.Name)) {
  # create tmp matrix
  tmp = 
    data.filter.sum.30.win.aft %>% 
    filter(Common.Name == i) %>% 
    mutate(ID = paste(Common.Name,
                      Date,
                      sep = "_")) %>% 
    pivot_wider(id_cols = ID,
                names_from = Real.time,
                values_from = species.count.scale) %>% 
    column_to_rownames('ID') %>% 
    as.matrix()
  
  # calculate distance
  tmp.dist = dtwDist(tmp,
                     window.type = 'sakoechiba',
                     window.size = 30*20)[1,2]
  
  ## add to matrix
  data.filter.sum.30.win.aft.dist = data.filter.sum.30.win.aft.dist %>% 
    rbind(data.frame(Time.day = 'afternoon',
                     Common.Name = i,
                     Dist.value = tmp.dist))
}

### combine all distance matrices
data.filter.sum.30.win.dist = data.filter.sum.30.win.aft.dist %>% 
  full_join(data.filter.sum.30.win.dus.dist) %>% 
  full_join(data.filter.sum.30.win.daw.dist)

# save data
write.csv(data.filter.sum.30.win.dist,
          'data/data_output/data.filter.sum.30.win.dist.csv')


### graph results
data.filter.sum.30.win.dist %>% 
  mutate(Not.aft = ifelse(Time.day == 'afternoon',
                          0,
                          1)) %>% 
  group_by(Common.Name,
           Not.aft) %>% 
  mutate(avg.value = mean(Dist.value)) %>% 
  ungroup() %>% 
  mutate(avg.value = ifelse(Not.aft == 1,
                            avg.value,
                            0)) %>% 
  group_by(Common.Name) %>% 
  mutate(avg.value = max(avg.value)) %>% 
  ggplot(aes(
    x = Dist.value,
    y = reorder(Common.Name,
                -avg.value),
    color = Time.day,
    group = Common.Name
  )) +
  geom_line(color = 'grey') +
  geom_point() +
  theme_classic() +
  xlab('Dynamic time warping') +
  ylab('Species, order by dawn/dusk mean') +
  ggtitle('Species consistency (before eclipse)')
ggsave('figures/all_cluster/Species consistency across time points.png')

# box plot
data.filter.sum.30.win.dist %>% 
  ggplot(aes(
    y = Dist.value,
    x = Time.day,
    fill = Time.day,
    group = Time.day 
  )) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center",
               binwidth = 30) +
  theme_classic() +
  ylab('Dynamic time warping') +
  xlab('Time of day') +
  ggtitle('Species consistency (before eclipse)')
ggsave('figures/all_cluster/Species consistency across time points boxplot.png')








