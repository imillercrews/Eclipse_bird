#### Eclipse bird analysis 
### Audio recording data 
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
# ## install changepoint.np
# install.packages('changepoint.np')

# load library
library(changepoint.np)
library(gtools)
# library(rstatix)
# library(boot)
library(tidyverse)

#### load data ####
## load test data
# add presence column
data.test = read.csv('data/eclipse afternoon 071924 hour before and after only.txt') %>% 
  mutate(Present = 1)

## create sum across recorders
data.test.sum = data.test %>% 
  select(Common.Name,
         minutes.before.totality.start,
         Present) %>% 
  group_by(minutes.before.totality.start,
           Common.Name) %>% 
  summarize(Present.all = sum(Present)) %>% 
  ungroup() %>% 
  droplevels()



#### calculate moving window and change points ####
#### find change points in audio data
### create wide format audio data
# each row is a species and recorder
data.test.wide = data.test %>%
  select(minutes.before.totality.start,
         Common.Name,
         Present,
         Recorder) %>% 
  arrange(minutes.before.totality.start) %>%
  pivot_wider(id_cols = c(Common.Name,
                          Recorder),
              names_from = minutes.before.totality.start,
              values_from = Present) %>% 
  mutate(Common.Name_Recorder = paste0(Common.Name,
                                       '_',
                                       Recorder)) %>% 
  select(-c(Common.Name,
            Recorder)) %>% 
  column_to_rownames('Common.Name_Recorder') %>% 
  as.matrix()

# convert NA to 0
data.test.wide[is.na(data.test.wide)] <- 0


### run across each species
# data.test.change.point = cpt.np(data.test.wide)
data.test.change.point = cpt.np(data.test.wide)
 
## create dataframe of results
# create empty dataframe
data.change.point.results = NULL

# loop through species
for (i in 1:nrow(data.test.wide)) {
  # create species temporary dataframe
tmp = data.frame(species = rownames(data.test.wide)[i],
                                       change.points = data.test.change.point[[i]]@cpts)
# combine with other species
data.change.point.results = data.change.point.results %>% 
  rbind(tmp)
}


## combine with position and gene name
# drop last level: 64
data.change.point.results = data.change.point.results %>% 
  left_join(data.test.wide %>% 
              colnames() %>% 
              as.data.frame() %>% 
              rename(position = '.') %>% 
              rownames_to_column('change.points') %>% 
              mutate(position = as.integer(position),
                     change.points = as.integer(change.points))) %>% 
  separate_wider_delim(cols = species,
                       delim = '_',
                       names = c('species',
                                 'recorder')) %>% 
  filter(position != 64)

### graph change points
for (i in unique(data.change.point.results$species)) {
data.change.point.results %>% 
    filter(species == i) %>% 
  ggplot(aes(x = position,
             y = recorder)) +
  geom_rect(aes(xmin = -2,
                xmax = 2,
                ymin = 0,
                ymax = data.change.point.results %>% 
                  filter(species == i) %>% 
                  pull(recorder) %>% 
                  unique() %>% 
                  length() + 1)) +
  geom_point() +
  theme_classic() +
  xlab('Time') +
    xlim(min(data.change.point.results$position),
         max(data.change.point.results$position)) +
  ggtitle(paste0(i,
                 ' change points eclipse')) +
  theme(legend.position = "none")
ggsave(paste0('figures/15_sp/change_points/',
              i,
              ' change points eclipse.png'))
}

### calculate rolling average
## create null dataframe
data.test.null = expand.grid(Common.Name = unique(data.test$Common.Name),
                             Recorder = unique(data.test$Recorder),
                             minutes.before.totality.start = unique(data.test$minutes.before.totality.start))

# moving window sum across 4 minutes with shift of 1 minute
data.test.win = data.test %>% 
  select(Common.Name,
         Recorder,
         minutes.before.totality.start,
         Present) %>% 
  full_join(data.test.null) %>% 
  mutate(Present = ifelse(is.na(Present),
                          0,
                          Present)) %>% 
  arrange(minutes.before.totality.start) %>% 
  group_by(Common.Name,
           Recorder) %>% 
  mutate(species.count = running(Present,
                                           width = 4*20,
                                           fun = sum,
                                           align = 'left',
                                           allow.fewer = TRUE,
                                           by = 1)) 


## graph running sum
# loop through all species

for (i in unique(data.test.win$Common.Name)) {

# add change points and eclipse window
  # remove recorders with no recordings
  data.test.win %>% 
  filter(Common.Name == i) %>% 
    group_by(Recorder) %>% 
    mutate(Keep = sum(Present)) %>% 
    ungroup() %>% 
    filter(Keep > 10) %>%
  ggplot() +
  annotate("rect",
          xmin = -2,
                xmax = 2,
                ymin = 0,
                ymax = data.test.win %>% 
            filter(Common.Name == i) %>% 
            pull(species.count) %>% 
            max() + 1) +
  geom_point(aes(x = minutes.before.totality.start,
                 y = species.count,
                 fill = Recorder)) +
  geom_line(aes(x = minutes.before.totality.start,
                y = species.count,
                group = Recorder,
                color = Recorder),
            linewidth = 2) +
  geom_vline(data = data.change.point.results %>% 
              filter(species == i) %>% 
              rename(Recorder = recorder),
            aes(xintercept = position)) +
  theme_classic() +
    xlab('Time') +
    ylab('Calls per four minute window') +
    xlim(min(data.change.point.results$position),
         max(data.change.point.results$position)) +
  ggtitle(i) +
    facet_grid(Recorder ~ .,
               drop = T)
ggsave(paste0('figures/15_sp/moving_sum/',
              i, 
              ' calls moving sum.png'))
}


#### calculate moving window and change points species ####
#### find change points in audio data
### create wide format audio data
# each row is a species and recorder
data.test.sum.wide = data.test.sum %>% 
  arrange(minutes.before.totality.start) %>%
  pivot_wider(id_cols = c(Common.Name),
              names_from = minutes.before.totality.start,
              values_from = Present.all) %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix()

# convert NA to 0
data.test.sum.wide[is.na(data.test.sum.wide)] <- 0


### run across each species
# data.test.sum.change.point = cpt.np(data.test.sum.wide)
data.test.sum.change.point = cpt.np(data.test.sum.wide)

## create dataframe of results
# create empty dataframe
data.sum.change.point.results = NULL

# loop through species
for (i in 1:nrow(data.test.sum.wide)) {
  # create species temporary dataframe
  tmp = data.frame(species = rownames(data.test.sum.wide)[i],
                   change.points = data.test.sum.change.point[[i]]@cpts)
  # combine with other species
  data.sum.change.point.results = data.sum.change.point.results %>% 
    rbind(tmp)
}


## combine with position and gene name
# drop last level: 64
data.sum.change.point.results = data.sum.change.point.results %>% 
  left_join(data.test.sum.wide %>% 
              colnames() %>% 
              as.data.frame() %>% 
              rename(position = '.') %>% 
              rownames_to_column('change.points') %>% 
              mutate(position = as.integer(position),
                     change.points = as.integer(change.points))) %>%
  filter(position != 64)

### graph change points
data.sum.change.point.results %>% 
    ggplot(aes(x = position,
               y = species)) +
    geom_rect(aes(xmin = -2,
                  xmax = 2,
                  ymin = 0,
                  ymax = data.sum.change.point.results %>% 
                    pull(species) %>% 
                    unique() %>% 
                    length() + 1)) +
    geom_point() +
    theme_classic() +
    xlab('Time') +
    xlim(min(data.sum.change.point.results$position),
         max(data.sum.change.point.results$position)) +
    ggtitle(paste0('All change points eclipse')) +
    theme(legend.position = "none")
  ggsave(paste0('figures/15_sp/change_points/All change points eclipse.png'))

### calculate rolling average
## create null dataframe
data.test.sum.null = expand.grid(Common.Name = unique(data.test.sum$Common.Name),
                             minutes.before.totality.start = unique(data.test.sum$minutes.before.totality.start))

# moving window sum across 4 minutes with shift of 1 minute
data.test.sum.win = data.test.sum  %>% 
  full_join(data.test.sum.null) %>% 
  mutate(Present.all = ifelse(is.na(Present.all),
                          0,
                          Present.all)) %>% 
  arrange(minutes.before.totality.start) %>% 
  group_by(Common.Name) %>% 
  mutate(species.count = running(Present.all,
                                 width = 4*20,
                                 fun = sum,
                                 align = 'left',
                                 allow.fewer = TRUE,
                                 by = 1)) 


## graph running sum
# loop through all species

for (i in unique(data.test.sum.win$Common.Name)) {
  
  # add change points and eclipse window
  # remove recorders with no recordings
  data.test.sum.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = -2,
             xmax = 2,
             ymin = 0,
             ymax = data.test.sum.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1) +
    geom_point(aes(x = minutes.before.totality.start,
                   y = species.count)) +
    geom_line(aes(x = minutes.before.totality.start,
                  y = species.count,
                  group = Common.Name),
              linewidth = 2) +
    geom_vline(data = data.sum.change.point.results  %>%
                 filter(species == i),
               aes(xintercept = position)) +
    theme_classic() +
    xlab('Time') +
    ylab('Calls per four minute window') +
    xlim(min(data.sum.change.point.results$position),
         max(data.sum.change.point.results$position)) +
    ggtitle(i)
  ggsave(paste0('figures/15_sp/moving_sum_species/',
                i, 
                ' calls moving sum.png'))
}

#### run change points with 4 minute windows
#### find change points in audio data
### create wide format audio data
# each row is a species and recorder
data.test.sum.window.wide = data.test.sum.win %>% 
  select(-c(Present.all)) %>% 
  arrange(minutes.before.totality.start) %>%
  pivot_wider(id_cols = c(Common.Name),
              names_from = minutes.before.totality.start,
              values_from = species.count) %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix()

# convert NA to 0
data.test.sum.window.wide[is.na(data.test.sum.window.wide)] <- 0


### run across each species
data.test.sum.window.change.point = cpt.np(data.test.sum.window.wide,
                                           minseglen = 20*4)

## create dataframe of results
# create empty dataframe
data.sum.window.change.point.results = NULL

# loop through species
for (i in 1:nrow(data.test.sum.window.wide)) {
  # create species temporary dataframe
  tmp = data.frame(species = rownames(data.test.sum.window.wide)[i],
                   change.points = data.test.sum.window.change.point[[i]]@cpts)
  # combine with other species
  data.sum.window.change.point.results = data.sum.window.change.point.results %>% 
    rbind(tmp)
}


## combine with position and gene name
# drop last level: 64
data.sum.window.change.point.results = data.sum.window.change.point.results %>% 
  left_join(data.test.sum.window.wide %>% 
              colnames() %>% 
              as.data.frame() %>% 
              rename(position = '.') %>% 
              rownames_to_column('change.points') %>% 
              mutate(position = as.integer(position),
                     change.points = as.integer(change.points))) %>%
  filter(position != 64)

## graph running sum
# loop through all species

for (i in unique(data.test.sum.win$Common.Name)) {

  # add change points and eclipse window
  # remove recorders with no recordings
  data.test.sum.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = -2,
             xmax = 2,
             ymin = 0,
             ymax = data.test.sum.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1) +
    geom_point(aes(x = minutes.before.totality.start,
                   y = species.count)) +
    geom_line(aes(x = minutes.before.totality.start,
                  y = species.count,
                  group = Common.Name),
              linewidth = 2) +
    geom_vline(data = data.sum.window.change.point.results  %>%
                 filter(species == i),
               aes(xintercept = position)) +
    theme_classic() +
    xlab('Time') +
    ylab('Calls per four minute window') +
    xlim(min(data.test.sum.win$minutes.before.totality.start),
         max(data.test.sum.win$minutes.before.totality.start)) +
    ggtitle(i)
  ggsave(paste0('figures/15_sp/moving_sum_species_window/',
                i,
                ' calls moving sum.png'))
}


## graph running sum
# no change points
# loop through all species

for (i in unique(data.test.sum.win$Common.Name)) {
  
  # remove recorders with no recordings
  data.test.sum.win %>%
    filter(Common.Name == i) %>% 
    ggplot() +
    annotate("rect",
             xmin = -2,
             xmax = 2,
             ymin = 0,
             ymax = data.test.sum.win %>% 
               filter(Common.Name == i) %>% 
               pull(species.count) %>% 
               max() + 1) +
    geom_point(aes(x = minutes.before.totality.start,
                   y = species.count)) +
    geom_line(aes(x = minutes.before.totality.start,
                  y = species.count,
                  group = Common.Name),
              linewidth = 2) +
    theme_classic() +
    xlab('Time') +
    ylab('Calls per four minute window') +
    xlim(min(data.test.sum.win$minutes.before.totality.start),
         max(data.test.sum.win$minutes.before.totality.start)) +
    ggtitle(i)
  ggsave(paste0('figures/15_sp/moving_sum_species_window_no_cp/',
                i,
                ' calls moving sum.png'))
}




#### distance matrix ####
# use dtwDist to calculate similartiy between species
# https://www.rdocumentation.org/packages/dtw/versions/1.23-1/topics/dtwDist
# https://rtavenar.github.io/blog/dtw.html

# install.packages('dtw')
library(dtw)
library(pvclust)

## convert data to matrix with timeseries as rows for each species
data.test.sum.win.mat = data.test.sum.win %>% 
  pivot_wider(id_cols = Common.Name,
              names_from = minutes.before.totality.start,
              values_from = species.count) %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix()

data.test.sum.win.mat[1:5,1:5]
str(data.test.sum.win.mat)


### calculate dtw dissimilarity between species
# species.dist.mat = dtwDist(data.test.sum.win.mat[])

# str(species.dist.mat)
# View(species.dist.mat)

# save matrix results
# write.csv(species.dist.mat,
#      'data/species.dist.mat.15.sp.csv')

# load data
species.dist.mat = read.csv('data/species.dist.mat.csv') %>% 
  column_to_rownames('X') %>% 
  as.matrix()

### graph cluster
# create distance matrix
species.dist.clust = hclust(as.dist(species.dist.mat))

# graph clustering
plot(species.dist.clust)

## pvclust
species.dist.pvclust = pvclust(species.dist.mat)

# seplot(species.dist.pvclust)
# species.dist.pvclust.sep <- seplot(species.dist.pvclust,
#             identify=TRUE)
# msplot(species.dist.pvclust)

# graph pvclust
pdf('figures/15_sp/cluster/species moving window dtwdist.pdf')
plot(species.dist.pvclust)
pvrect(species.dist.pvclust,
       alpha=0.95)
dev.off()


### graph species in categories 
# cluster 1
data.test.sum.win %>%
  filter(!Common.Name %in% c('House Finch',
                             'Tree Swallow',
                             'House Sparrow',
                             'Northern Cardinal',
                             'Tufted Titmouse')) %>% 
  group_by(Common.Name) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot() +
  geom_line(aes(x = minutes.before.totality.start,
                y = species.count,
                group = Common.Name
  ),
  linewidth = 2,
  color = 'grey') +
  annotate("rect",
           xmin = -2,
           xmax = 2,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = minutes.before.totality.start,
                  y = species.count),
              color = 'orange') +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.test.sum.win$minutes.before.totality.start),
       max(data.test.sum.win$minutes.before.totality.start)) +
  ggtitle('Cluster 1')
ggsave('figures/15_sp/cluster/Cluster 1 songs over time.png')

# cluster 2
data.test.sum.win %>%
  filter(Common.Name %in% c('House Finch',
                            'Tree Swallow',
                            'House Sparrow',
                            'Northern Cardinal',
                            'Tufted Titmouse')) %>% 
  group_by(Common.Name) %>% 
  mutate(species.count = species.count/max(species.count)) %>% 
  ggplot() +
  geom_line(aes(x = minutes.before.totality.start,
                y = species.count,
                group = Common.Name
  ),
  linewidth = 2,
  color = 'grey') +
  annotate("rect",
           xmin = -2,
           xmax = 2,
           ymin = 0,
           ymax = 1) +
  geom_smooth(aes(x = minutes.before.totality.start,
                  y = species.count),
              color = 'orange') +
  theme_classic() +
  xlab('Time') +
  ylab('Calls per four minute window scaled') +
  xlim(min(data.test.sum.win$minutes.before.totality.start),
       max(data.test.sum.win$minutes.before.totality.start)) +
  ggtitle('Cluster 2')
ggsave('figures/15_sp/cluster/Cluster 2 songs over time.png')




