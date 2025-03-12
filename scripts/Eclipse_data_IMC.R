#### Eclipse bird analysis 
### Audio recording data 
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
library(tidyverse)
library(ggbreak)

#### load species data ####
## load species
data.species = read.delim('data/52 spp.txt') %>% 
  rename(Common.Name = Level)

#### load dusk data ####
### dusk data
# All complete
# All
data.dusk = read_delim(list.files(path = 'data/data_raw/4.6_4.7_Dusk/', 
                                       pattern = '*selection.table.txt', 
                                       recursive = T,
                                       full.names = T
)) %>% 
  dplyr::select(Confidence,
                "Common Name",
                "Begin Path",
                "File Offset (s)") %>% 
  mutate(Present = 1,
         Time.day = 'dusk') 

# rename columns
colnames(data.dusk) = c("Confidence",
                             "Common.Name",
                             "Begin.Path",
                             "File.Offset..s.",
                             "Present",
                             "Time.day")

# extract time, date, and recorder
data.dusk =
  data.dusk %>%
  mutate(Real.time.start = str_sub(Begin.Path,
                                   start = -10,
                                   end = -5),
         Date.exact = str_sub(Begin.Path,
                              start = -19,
                              end = -12),
         Recorder = str_sub(Begin.Path,
                            end = -21),
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
            Date.exact)) %>% 
  distinct()

# check file path
data.dusk %>% 
  # filter(Date != 7) %>%
  pull(Recorder) %>% 
  unique() 

# fix recorder 
data.dusk =
  data.dusk %>% 
  separate_wider_delim(Recorder,
                       delim = '\\Recursive\\',
                       names = c(NA,
                                 'Recorder'))

# check overlap 
# no missing species
data.species$Common.Name %>% 
  intersect(data.dusk$Common.Name %>% 
            unique())

# check recorders across days
data.dusk %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()





#### load dawn data ####
### dawn data
# All
data.dawn = read_delim(list.files(path = 'data/data_raw/4.7_4.8_Dawn/', 
                                       pattern = '*selection.table.txt', 
                                       recursive = T,
                                       full.names = T
)) %>% 
  dplyr::select(Confidence,
                "Common Name",
                "Begin Path",
                "File Offset (s)") %>% 
  mutate(Present = 1,
         Time.day = 'dawn') 

# rename columns
colnames(data.dawn) = c("Confidence",
                        "Common.Name",
                        "Begin.Path",
                        "File.Offset..s.",
                        "Present",
                        "Time.day")

# extract time, date, and recorder
data.dawn =
  data.dawn %>%
  mutate(Real.time.start = str_sub(Begin.Path,
                                   start = -10,
                                   end = -5),
         Date.exact = str_sub(Begin.Path,
                              start = -19,
                              end = -12),
         Recorder = str_sub(Begin.Path,
                            end = -21),
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
            Date.exact)) %>% 
  distinct()

## recorder name has wrong number of rows
# check file path
data.dawn = data.dawn %>% 
  separate_wider_delim(Recorder,
                       delim = '\\Desktop\\',
                       names = c(NA,
                                 'Recorder')) %>% 
  separate_wider_delim(Recorder,
                       delim = "DAWN\\",
                       names = c(NA,
                                 'Recorder')) %>% 
  separate_wider_delim(Recorder,
                       delim = "Recordings\\",
                       names = c(NA,
                                 'Recorder'),
                       too_few = 'align_end')

# check overlap 
# no missing species
data.species$Common.Name %>% 
  intersect(data.dawn$Common.Name %>% 
            unique())

# check recorders across days
data.dawn %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()

#### load afternoon data ####
### afternoon data
# All
data.afternoon = read_delim(list.files(path = 'data/data_raw/4.6_4.7_4.8_4.9_AFTERNOON/', 
                                       pattern = '*selection.table.txt', 
                                       recursive = T,
                                       full.names = T
)) %>% 
  dplyr::select(Confidence,
                "Common Name",
                "Begin Path",
                "File Offset (s)") %>% 
  mutate(Present = 1,
         Time.day = 'afternoon') 

# rename columns
colnames(data.afternoon) = c("Confidence",
                             "Common.Name",
                             "Begin.Path",
                             "File.Offset..s.",
                             "Present",
                             "Time.day")

# extract time, date, and recorder
data.afternoon =
  data.afternoon %>%
  mutate(Real.time.start = str_sub(Begin.Path,
                                   start = -10,
                                   end = -5),
         Date.exact = str_sub(Begin.Path,
                              start = -19,
                              end = -12),
         Recorder = str_sub(Begin.Path,
                            end = -21),
         Date = case_when(Date.exact == '20240406' ~ 6,
                          Date.exact == '20240407' ~ 7,
                          Date.exact == '20240408' ~ 8,
                          Date.exact == '20240409' ~ 9),
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
            Date.exact)) %>% 
  distinct()

## recorder name has wrong number of rows
# check file path
data.afternoon %>% 
  # filter(Date != 7) %>%
  pull(Recorder) %>% 
  unique() 
# all end in '\\Recursive\\' before recorder name
# except for the 9th which ends with"\\Recordings\\

# fix recorder 
data.afternoon =
  data.afternoon %>% 
  separate_wider_delim(Recorder,
                       delim = '\\Recursive\\',
                       names = c(NA,
                                 'Recorder'),
                       too_few = 'align_end') %>% 
  separate_wider_delim(Recorder,
                       delim = '\\Recordings\\',
                       names = c(NA,
                                 'Recorder'),
                       too_few = 'align_end')

# check overlap 
# no missing species
data.species$Common.Name %>% 
  intersect(data.afternoon$Common.Name %>% 
            unique())

# check recorders across days
data.afternoon %>% 
  dplyr::select(Recorder, 
                Date) %>% 
  distinct() %>% 
  table()

#### combine and save data ####
## combine and save data
data = data.dawn %>% 
  rbind(data.afternoon) %>% 
  rbind(data.dusk)

# check data
str(data)

# save data
write.csv(data,
          'data/data_output/data_all_combined.csv')

# confidence stats
data.confidence = data %>% 
  group_by(Common.Name) %>% 
  summarise(Confidence.min = min(Confidence),
            Confidence.max = max(Confidence),
            Confidence.mean = mean(Confidence),
            Confidence.sd = sd(Confidence)) %>% 
  ungroup()

# save data
write.csv(data.confidence,
          'data/data_output/data_all_combined_confidence.csv',
          row.names = F)

## create summary file across recorders
data.sum = data %>% 
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
  droplevels() %>% 
  distinct()

# save data
write.csv(data.sum,
          'data/data_output/data_all_combined_sum.csv')

## check that each time point is in data
data %>% 
  group_by(Time.day,
           Recorder,
           Date) %>% 
  summarise(max = max(Real.time),
            min = min(Real.time)) %>% 
  pivot_longer(cols = c('min',
                        'max'),
               names_to = 'type',
               values_to = 'value') %>% 
  mutate(ID = paste0(Recorder,
                     Date,
                     Time.day)) %>% 
  ggplot(aes(x = value,
             y = as.factor(Date),
             group = ID,
             color = Recorder
             )) +
  geom_vline(xintercept = 21900) +
  geom_vline(xintercept = 29100) +
  geom_vline(xintercept = 50695) +
  geom_vline(xintercept = 58135) +
  geom_vline(xintercept = 70200) +
  geom_vline(xintercept = 77400) +
  geom_vline(xintercept = 54295,
             linetype = 'dashed') +
  geom_vline(xintercept = 54535,
             linetype = 'dashed') +
  geom_vline(xintercept = 24750,
             linetype = 'dashed') +
  geom_vline(xintercept = 26460,
             linetype = 'dashed') +
  geom_vline(xintercept = 72960,
             linetype = 'dashed') +
  geom_vline(xintercept = 74580,
             linetype = 'dashed') +
  geom_boxplot() +
  # facet_grid(~Time.day,
  #            scales = 'free') +
  theme_classic() +
  ylab('Date') +
  xlab('Time (s)')
ggsave('figures/Data file sanity check of timing.png')


### create filtered data set
## filter to 52 species list
data.filter = data %>%
  filter(Common.Name %in% data.species$Common.Name) %>%
  droplevels()

## filter to time of interest
data.filter = data.filter %>% 
  mutate(keep = case_when(
    Time.day == 'dawn' & Real.time < 21900 ~ 0,
    Time.day == 'dawn' & Real.time > 29100 ~ 0,
    Time.day == 'afternoon' & Real.time < 50695 ~ 0,
    Time.day == 'afternoon' & Real.time > 58135 ~ 0,
    Time.day == 'dusk' & Real.time < 70200 ~ 0,
    Time.day == 'dusk' & Real.time > 77400 ~ 0,
    TRUE ~ 1
  )) %>% 
  filter(keep == 1) %>% 
  select(-c(keep))

# check that each time point is in data
data.filter %>% 
  group_by(Time.day,
           Recorder,
           Date) %>% 
  summarise(max = max(Real.time),
            min = min(Real.time)) %>% 
  pivot_longer(cols = c('min',
                        'max'),
               names_to = 'type',
               values_to = 'value') %>% 
  mutate(ID = paste0(Recorder,
                     Date,
                     Time.day)) %>% 
  ggplot(aes(x = value,
             y = as.factor(Date),
             group = ID,
             color = Recorder
  )) +
  geom_vline(xintercept = 21900) +
  geom_vline(xintercept = 29100) +
  geom_vline(xintercept = 50695) +
  geom_vline(xintercept = 58135) +
  geom_vline(xintercept = 70200) +
  geom_vline(xintercept = 77400) +
  geom_vline(xintercept = 54295,
             linetype = 'dashed') +
  geom_vline(xintercept = 54535,
             linetype = 'dashed') +
  geom_vline(xintercept = 24750,
             linetype = 'dashed') +
  geom_vline(xintercept = 26460,
             linetype = 'dashed') +
  geom_vline(xintercept = 72960,
             linetype = 'dashed') +
  geom_vline(xintercept = 74580,
             linetype = 'dashed') +
  geom_boxplot() +
  # facet_grid(~Time.day,
  #            scales = 'free') +
  theme_classic() +
  ylab('Date') +
  xlab('Time (s)')
ggsave('figures/Data file sanity check of timing filter.png')

# save data
write.csv(data.filter,
          'data/data_output/data_all_combined_filter.csv')

## create summary file across recorders
data.filter.sum = data.filter %>% 
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
  droplevels() %>% 
  distinct()

# save data
write.csv(data.filter.sum,
          'data/data_output/data_all_combined_filter_sum.csv')

#### create 1 minute sum ####
### create dataframe with every second second
## create null dataframe
# create 0 for every second
data.null.sec = expand.grid(Common.Name = unique(data.filter$Common.Name),
                            Real.time = seq(data.filter %>% 
                                              filter(Time.day == 'dawn') %>% 
                                              pull(Real.time) %>% 
                                              min(),
                                            data.filter %>% 
                                              filter(Time.day == 'dawn') %>% 
                                              pull(Real.time) %>% 
                                              max(),
                                            by = 1),
                            Recorder = data.filter %>% 
                              filter(Time.day == 'dawn') %>% 
                              pull(Recorder) %>% 
                              unique(),
                            Date = c(7,
                                     8),
                            Time.day = 'dawn') %>% 
  rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
                    Real.time = seq(data.filter %>% 
                                      filter(Time.day == 'dusk') %>% 
                                      pull(Real.time) %>% 
                                      min(),
                                    data.filter %>% 
                                      filter(Time.day == 'dusk') %>% 
                                      pull(Real.time) %>% 
                                      max(),
                                    by = 1),
                    Recorder = data.filter %>% 
                      filter(Time.day == 'dusk') %>% 
                      pull(Recorder) %>% 
                      unique(),
                    Date = c(7,
                             6),
                    Time.day = 'dusk')) %>% 
  rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
                    Real.time = seq(data.filter %>% 
                                      filter(Time.day == 'afternoon') %>% 
                                      pull(Real.time) %>% 
                                      min(),
                                    data.filter %>% 
                                      filter(Time.day == 'afternoon') %>% 
                                      pull(Real.time) %>% 
                                      max(),
                                    by = 1),
                    Recorder = data.filter %>% 
                      filter(Time.day == 'afternoon') %>% 
                      pull(Recorder) %>% 
                      unique(),
                    Date = c(6,
                             7,
                             8,
                             9),
                    Time.day = 'afternoon'))


## combine null data with full dataset
# set NA to 0
data.filter.sec = data.filter  %>% 
  select(Present,
         Common.Name,
         Date,
         Time.day,
         Recorder,
         Real.time) %>% 
  full_join(data.null.sec) %>% 
  mutate(Present = ifelse(is.na(Present),
                          0,
                          Present)) %>% 
  
  arrange(Real.time) 

## create minute group sequence to assign time to 60 second intervals
# cut off the last second
data.sec.group = data.frame(Real.time = seq(data.filter %>% 
                                              filter(Time.day == 'dawn') %>% 
                                              pull(Real.time) %>% 
                                              min(),
                                            data.filter %>% 
                                              filter(Time.day == 'dawn') %>% 
                                              pull(Real.time) %>% 
                                              max() - 1, #remove last second
                                            by = 1), #length is 7200
                            Min.group = rep(seq(from = 21900,
                                                to = 29099, 
                                                by = 60),
                                            each = 60),
                            Time.day = 'dawn') %>% 
  rbind(data.frame(Real.time = seq(data.filter %>% 
                                     filter(Time.day == 'dusk') %>% 
                                     pull(Real.time) %>% 
                                     min(),
                                   data.filter %>% 
                                     filter(Time.day == 'dusk') %>% 
                                     pull(Real.time) %>% 
                                     max() - 1, #remove last second
                                   by = 1), #length is 7200
                   Min.group = rep(seq(from = 70200,
                                       to = 77399, 
                                       by = 60),
                                   each = 60),
                   Time.day = 'dusk')) %>% 
  rbind(data.frame(Real.time = seq(data.filter %>% 
                                     filter(Time.day == 'afternoon') %>% 
                                     pull(Real.time) %>% 
                                     min(),
                                   data.filter %>% 
                                     filter(Time.day == 'afternoon') %>% 
                                     pull(Real.time) %>% 
                                     max() - 1, #remove last second
                                   by = 1), #length is 7440
                   Min.group = rep(seq(from = 50695,
                                       to = 58134, 
                                       by = 60),
                                   each = 60),
                   Time.day = 'afternoon')) 

# add minute group
# need to remove NA from last second of each
data.filter.sec.group = data.filter.sec %>% 
  full_join(data.sec.group) %>% 
  na.omit()

### create summary data.frame
## add time to eclipse (set eclipse as 0)
# recorders
data.filter.sec.sum.recorder = data.filter.sec.group %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Recorder,
           Min.group) %>% 
  summarise(Sum = sum(sum(Present))) %>% 
  mutate(Min.eclipse = (Min.group - 54295)/60)

# birds
data.filter.sec.sum = data.filter.sec.group %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Min.group) %>% 
  summarise(Sum = sum(sum(Present))) %>% 
  mutate(Min.eclipse = (Min.group - 54295)/60)

## test graph
data.filter.sec.sum %>%
  filter(Common.Name == "American Robin") %>%
  filter(Time.day == 'afternoon') %>% 
  ggplot(aes(x = Min.group,
             y = Sum,
             group = paste0(Date,
                            Time.day),
             color = as.factor(Date))) +
  geom_vline(xintercept = 54295)+
  geom_vline(xintercept = 54535)+
  geom_line() +
  theme_classic() 



#### save data for minute summary ####
## create summary stats table for paper
data.summary = data.filter.sec.sum.recorder %>% 
  filter(Sum != 0) %>% 
  select(Common.Name,
         Date,
         Time.day,
         Recorder) %>%
  distinct() %>% 
  group_by(Common.Name,
           Date,
           Time.day) %>%
  summarise(Number.sites = n()) %>% 
  ungroup() %>% 
  mutate(Day.time = paste(Date,
                           Time.day,
                          'number.sites',
                          sep='.')) %>% 
  select(-c(Date,
            Time.day)) %>% 
pivot_wider(names_from = 'Day.time',
            values_from = 'Number.sites') %>% 
  full_join(data.filter.sec.sum.recorder %>% 
              group_by(Common.Name,
                       Date,
                       Time.day) %>%
              summarise(Counts = sum(Sum)) %>% 
              ungroup() %>%  
              mutate(Day.time = paste(Date,
                                       Time.day,
                                       'counts',
                                       sep='.')) %>% 
              select(-c(Date,
                        Time.day)) %>% 
              pivot_wider(names_from = 'Day.time',
                          values_from = 'Counts')) %>% 
  full_join(data.filter.sec.sum.recorder %>% 
              group_by(Common.Name) %>%
              summarise(Total = sum(Sum)) %>% 
              ungroup()) %>% 
  full_join(data.filter.sec.sum.recorder %>% 
              group_by(Date,
                       Time.day) %>%
              summarise(Counts = sum(Sum)) %>% 
              ungroup() %>%  
              mutate(Day.time = paste(Date,
                                      Time.day,
                                      'counts',
                                      sep='.')) %>% 
              select(-c(Date,
                        Time.day)) %>% 
              pivot_wider(names_from = 'Day.time',
                          values_from = 'Counts') %>% 
              mutate(Common.Name = 'Total') %>% 
  full_join(data.filter.sec.sum.recorder %>% 
              ungroup() %>% 
              summarise(Total = sum(Sum)) %>% 
              mutate(Common.Name = 'Total')) )
# save data
write.csv(data.summary,
          'data/data_output/data_all_combined_summary.csv',
          row.names = F)

# recorder
# save data
write.csv(data.filter.sec.sum.recorder,
          'data/data_output/one.min.sum/data.filter.sec.sum.recorder.csv')

# species sum
# save data
write.csv(data.filter.sec.sum,
          'data/data_output/one.min.sum/data.filter.sec.sum.csv')
# load data
data.filter.sec.sum = read.csv('data/data_output/one.min.sum/data.filter.sec.sum.csv') 

#### create 4 minute sum ####
### create dataframe with every second second
## create null dataframe
# create 0 for every second
# data.null.sec = expand.grid(Common.Name = unique(data.filter$Common.Name),
#                             Real.time = seq(data.filter %>% 
#                                               filter(Time.day == 'dawn') %>% 
#                                               pull(Real.time) %>% 
#                                               min(),
#                                             data.filter %>% 
#                                               filter(Time.day == 'dawn') %>% 
#                                               pull(Real.time) %>% 
#                                               max(),
#                                             by = 1),
#                             Recorder = data.filter %>% 
#                               filter(Time.day == 'dawn') %>% 
#                               pull(Recorder) %>% 
#                               unique(),
#                             Date = c(7,
#                                      8),
#                             Time.day = 'dawn') %>% 
#   rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
#                     Real.time = seq(data.filter %>% 
#                                       filter(Time.day == 'dusk') %>% 
#                                       pull(Real.time) %>% 
#                                       min(),
#                                     data.filter %>% 
#                                       filter(Time.day == 'dusk') %>% 
#                                       pull(Real.time) %>% 
#                                       max(),
#                                     by = 1),
#                     Recorder = data.filter %>% 
#                       filter(Time.day == 'dusk') %>% 
#                       pull(Recorder) %>% 
#                       unique(),
#                     Date = c(7,
#                              6),
#                     Time.day = 'dusk')) %>% 
#   rbind(expand.grid(Common.Name = unique(data.filter$Common.Name),
#                     Real.time = seq(data.filter %>% 
#                                       filter(Time.day == 'afternoon') %>% 
#                                       pull(Real.time) %>% 
#                                       min(),
#                                     data.filter %>% 
#                                       filter(Time.day == 'afternoon') %>% 
#                                       pull(Real.time) %>% 
#                                       max(),
#                                     by = 1),
#                     Recorder = data.filter %>% 
#                       filter(Time.day == 'afternoon') %>% 
#                       pull(Recorder) %>% 
#                       unique(),
#                     Date = c(6,
#                              7,
#                              8),
#                     Time.day = 'afternoon'))
# 
# 
# ## combine null data with full dataset
# # set NA to 0
# data.filter.sec = data.filter  %>% 
#   select(Present,
#          Common.Name,
#          Date,
#          Time.day,
#          Recorder,
#          Real.time) %>% 
#   full_join(data.null.sec) %>% 
#   mutate(Present = ifelse(is.na(Present),
#                           0,
#                           Present)) %>% 
#   
#   arrange(Real.time) 

## create 4 minute group sequence to assign time to 240 second intervals
# cut off the last second
data.sec.group.4 = data.frame(Real.time = seq(data.filter %>% 
                                                filter(Time.day == 'dawn') %>% 
                                                pull(Real.time) %>% 
                                                min(),
                                              data.filter %>% 
                                                filter(Time.day == 'dawn') %>% 
                                                pull(Real.time) %>% 
                                                max() - 1, #remove last second
                                              by = 1), #length is 7200
                              Min.group = rep(seq(from = 21900,
                                                  to = 29099, 
                                                  by = 240),
                                              each = 240),
                              Time.day = 'dawn') %>% 
  rbind(data.frame(Real.time = seq(data.filter %>% 
                                     filter(Time.day == 'dusk') %>% 
                                     pull(Real.time) %>% 
                                     min(),
                                   data.filter %>% 
                                     filter(Time.day == 'dusk') %>% 
                                     pull(Real.time) %>% 
                                     max() - 1, #remove last second
                                   by = 1), #length is 7200
                   Min.group = rep(seq(from = 70200,
                                       to = 77399, 
                                       by = 240),
                                   each = 240),
                   Time.day = 'dusk')) %>% 
  rbind(data.frame(Real.time = seq(data.filter %>% 
                                     filter(Time.day == 'afternoon') %>% 
                                     pull(Real.time) %>% 
                                     min(),
                                   data.filter %>% 
                                     filter(Time.day == 'afternoon') %>% 
                                     pull(Real.time) %>% 
                                     max() - 1, #remove last second
                                   by = 1), #length is 7440
                   Min.group = rep(seq(from = 50695,
                                       to = 58134, 
                                       by = 240),
                                   each = 240),
                   Time.day = 'afternoon')) 

# add minute group
# need to remove NA from last second of each
data.filter.sec.group.4 = data.filter.sec %>% 
  full_join(data.sec.group.4) %>% 
  na.omit()

### create summary data.frame
## add time to eclipse (set eclipse as 0)
# recorders
data.filter.sec.sum.recorder.4 = data.filter.sec.group.4 %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Recorder,
           Min.group) %>% 
  summarise(Sum = sum(sum(Present))) %>% 
  mutate(Min.eclipse = (Min.group - 54295)/60)

# birds
data.filter.sec.sum.4 = data.filter.sec.group.4 %>% 
  group_by(Common.Name,
           Date,
           Time.day,
           Min.group) %>% 
  summarise(Sum = sum(sum(Present))) %>% 
  mutate(Min.eclipse = (Min.group - 54295)/60)

## test graph
data.filter.sec.sum.4 %>%
  filter(Common.Name == "American Robin") %>%
  filter(Time.day == 'afternoon') %>% 
  ggplot(aes(x = Min.group,
             y = Sum,
             group = paste0(Date,
                            Time.day),
             color = as.factor(Date))) +
  geom_vline(xintercept = 54295)+
  geom_vline(xintercept = 54535)+
  geom_line() +
  theme_classic() 



#### save data 4 minute summary ####
## recorder
# save data
write.csv(data.filter.sec.sum.recorder.4,
          'data/data_output/four.min.sum/data.filter.sec.sum.recorder.4.csv')

## species sum
# save data
write.csv(data.filter.sec.sum.4,
          'data/data_output/four.min.sum/data.filter.sec.sum.4.csv')


#### graph sum each species ####
## load in lux data
# expand time to seconds
data.lux = read.csv('data/data_raw/lux_data.csv') %>% 
  dplyr::select(-c(datetime_original,
                   continuous.numbering,
                   date.time.formatted)) %>% 
  separate_wider_delim(cols = EDT,
                       delim = " ",
                       names = c(NA,
                                 'Time'),
                       cols_remove = F) %>% 
  separate_wider_delim(cols = Time,
                       delim = ":",
                       names = c('h',
                                 'm',
                                 's')) %>% 
  mutate(Time = 3600*as.numeric(h)+60*as.numeric(m)+as.numeric(s)) %>% 
  dplyr::select(-c(h,m,s))

## graph sum all species
# 1 minute
data.filter.sec.sum %>% 
  select(-X) %>% 
  group_by(Date,
           Time.day,
           Min.group,
           Min.eclipse) %>% 
  summarise(Sum = sum(Sum)) %>% 
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
    geom_line(aes(x = Min.group/3600,
                  y = Sum,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 1 minute window') +
    xlim(min(data.filter.sec.sum$Min.group)/3600,
         max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle("All birds: 1 min sum") +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black',
                                  'yellow'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave('figures/moving_sum/moving_sum_species_1/All birds calls moving sum 1 min.png')

  # get average for non-eclipse days  
  data.filter.sec.sum %>% 
    select(-X) %>% 
    filter(Time.day == 'afternoon') %>% 
    mutate(eclipse = ifelse(Date == 8,
                            "Eclipse",
                            "Average Day")) %>% 
    group_by(Date,
             eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Sum.total = sum(Sum)) %>% 
    ungroup() %>% 
    group_by(eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Avg.total = mean(Sum.total)) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    # geom_vline(xintercept = 24750/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 26460/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 72960/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 74580/3600,
    #            linetype = 'dashed') +
    geom_line(aes(x = (Min.group+30)/3600, # add 120 seconds to center it
                  y = Avg.total,
                  group = eclipse,
                  color = as.factor(eclipse)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 1 minute window') +
    # xlim(min(data.filter.sec.sum$Min.group)/3600,
    #      max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle("All birds: 1 min sum") +
    scale_color_manual(values = c("Average Day" = 'grey',
                                  "Eclipse" = '#EE8012'),
                       name = 'Day') 
  ggsave('figures/moving_sum/moving_sum_species_1/All birds calls moving sum 1 min afternoon average.png')
  
  # paper
  # get average for non-eclipse days  
  data.filter.sec.sum %>% 
    select(-X) %>% 
    filter(Time.day == 'afternoon') %>% 
    mutate(eclipse = ifelse(Date == 8,
                            "Eclipse",
                            "Average Day")) %>% 
    group_by(Date,
             eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Sum.total = sum(Sum)) %>% 
    ungroup() %>% 
    group_by(eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Avg.total = mean(Sum.total)) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             fill = '#404040') +
    # geom_vline(xintercept = 24750/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 26460/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 72960/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 74580/3600,
    #            linetype = 'dashed') +
    geom_line(aes(x = (Min.group+30)/3600, # add 120 seconds to center it
                  y = Avg.total,
                  group = eclipse,
                  color = as.factor(eclipse)),
              linewidth = 1) +
    theme_classic(base_size = 12) +
    # theme(legend.position = "inside",
    #       legend.position.inside = c(0.90,0.90)) +
    xlab('Time (hrs)') +
    ylab('Vocalizations per minute') +
    # xlim(min(data.filter.sec.sum$Min.group)/3600,
    #      max(data.filter.sec.sum$Min.group)/3600) +
    # ggtitle("All birds: 1 min sum") +
    scale_color_manual(values = c("Average Day" = 'grey50',
                                  "Eclipse" = '#00C58F'),
                       name = 'Day') +
    annotate("rect",
             xmin = (53575)/3600, #-12
             xmax = (54295)/3600, #-1, add 1 for minute
             ymin = 0,
             ymax = 12,
             fill = '#B21912') +
    annotate("rect",
             xmin = (54535)/3600, #4
             xmax = (55255)/3600, #4+12-1, add 1 for minute
             ymin = 0,
             ymax = 12,
             fill = '#EE8012') +
    annotate("rect",
             xmin = (55735)/3600, #24
             xmax = (56455)/3600, #35, add 1 for minute
             ymin = 0,
             ymax = 12,
             fill = '#FFC000') +
    ylim(0,200)
  ggsave('figures/moving_sum/moving_sum_species_1/All birds calls moving sum 1 min afternoon average paper.pdf',
         height = 3.5,
         width = 6.5,
         units = 'in')
  
  
  
# 4 minute
  data.filter.sec.sum.4 %>% 
    select(-X) %>% 
    group_by(Date,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Sum = sum(Sum)) %>% 
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
    geom_line(aes(x = Min.group/3600,
                  y = Sum,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 4 minute window') +
    xlim(min(data.filter.sec.sum$Min.group)/3600,
         max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle("All birds: 4 min sum") +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black',
                                  'yellow'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave('figures/moving_sum/moving_sum_species_4/All birds calls moving sum 4 min.png')
  
# just afternoon
# 4 minute
  data.filter.sec.sum.4 %>% 
    select(-X) %>% 
    filter(Time.day == 'afternoon') %>% 
    mutate(eclipse = ifelse(Date == 8,
                            "Eclipse",
                            "Average Day")) %>% 
    group_by(Date,
             eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Sum.total = sum(Sum)) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    # geom_vline(xintercept = 24750/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 26460/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 72960/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 74580/3600,
    #            linetype = 'dashed') +
    geom_line(aes(x = (Min.group+120)/3600, # add 120 seconds to center it
                  y = Sum.total,
                  group = Date,
                  color = as.factor(eclipse)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 4 minute window') +
    # xlim(min(data.filter.sec.sum$Min.group)/3600,
    #      max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle("All birds: 4 min sum") +
    scale_color_manual(values = c("Average Day" = 'grey',
                                  "Eclipse" = '#EE8012'),
                       name = 'Day') 
  ggsave('figures/moving_sum/moving_sum_species_4/All birds calls moving sum 4 min afternoon.png')
  
# get average for non-eclipse days  
data.filter.sec.sum.4 %>% 
    select(-X) %>% 
    filter(Time.day == 'afternoon') %>% 
    mutate(eclipse = ifelse(Date == 8,
                            "Eclipse",
                            "Average Day")) %>% 
    group_by(Date,
             eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Sum.total = sum(Sum)) %>% 
    ungroup() %>% 
    group_by(eclipse,
             Time.day,
             Min.group,
             Min.eclipse) %>% 
    summarise(Avg.total = mean(Sum.total)) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = 'grey') +
    # geom_vline(xintercept = 24750/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 26460/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 72960/3600,
    #            linetype = 'dashed') +
    # geom_vline(xintercept = 74580/3600,
    #            linetype = 'dashed') +
    geom_line(aes(x = (Min.group+120)/3600, # add 120 seconds to center it
                  y = Avg.total,
                  group = eclipse,
                  color = as.factor(eclipse)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 4 minute window') +
    # xlim(min(data.filter.sec.sum$Min.group)/3600,
    #      max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle("All birds: 4 min sum") +
    scale_color_manual(values = c("Average Day" = 'grey',
                                  "Eclipse" = '#EE8012'),
                       name = 'Day') 
ggsave('figures/moving_sum/moving_sum_species_4/All birds calls moving sum 4 min afternoon average.png')


# add lux data 
data.filter.sec.sum.4 %>% 
  select(-X) %>% 
  filter(Time.day == 'afternoon') %>% 
  mutate(eclipse = ifelse(Date == 8,
                          "Eclipse",
                          "Average Day")) %>% 
  group_by(Date,
           eclipse,
           Time.day,
           Min.group,
           Min.eclipse) %>% 
  summarise(Sum.total = sum(Sum)) %>% 
  ungroup() %>% 
  group_by(eclipse,
           Time.day,
           Min.group,
           Min.eclipse) %>% 
  summarise(Avg.total = mean(Sum.total)) %>% 
  ggplot() +
  annotate("rect",
           xmin = 54295/3600,
           xmax = 54535/3600,
           ymin = 0,
           ymax = Inf,
           color = 'grey') +
  # geom_vline(xintercept = 24750/3600,
  #            linetype = 'dashed') +
  # geom_vline(xintercept = 26460/3600,
  #            linetype = 'dashed') +
  # geom_vline(xintercept = 72960/3600,
  #            linetype = 'dashed') +
  # geom_vline(xintercept = 74580/3600,
  #            linetype = 'dashed') +
  geom_line(aes(x = (Min.group+120)/3600, # add 120 seconds to center it
                y = Avg.total,
                group = eclipse,
                color = as.factor(eclipse)),
            linewidth = 2) +
  theme_classic() +
  xlab('Time (hrs)') +
  ylab('Calls per 4 minute window') +
  # xlim(min(data.filter.sec.sum$Min.group)/3600,
  #      max(data.filter.sec.sum$Min.group)/3600) +
  ggtitle("All birds: 4 min sum") +
  scale_color_manual(values = c("Average Day" = 'grey',
                                "Eclipse" = '#EE8012'),
                     name = 'Day') + 
  geom_smooth(data= data.lux %>%
              filter(Time >= 50695) %>%
              filter(Time <= 57895),
            aes(x = Time/3600,
                y = lux/125),
            color = 'black',
            se = FALSE,
            span = 0.05,
            linetype = 'dashed') +
  scale_y_continuous(
    sec.axis = sec_axis(~.x*125, name="lux",labels = scales::comma))
ggsave('figures/moving_sum/moving_sum_species_4/All birds calls moving sum 4 min afternoon average lux.png')

  
## graph sum 1 minute window 
# loop through all species
for (i in unique(data.filter.sec.sum$Common.Name)) {
  
  data.filter.sec.sum %>% 
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
    geom_line(aes(x = Min.group/3600,
                  y = Sum,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 1 minute window') +
    xlim(min(data.filter.sec.sum$Min.group)/3600,
         max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle(paste0(i,
                   ": 1 min sum")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black',
                                  'yellow'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave(paste0('figures/moving_sum/moving_sum_species_1/',
                i, 
                ' calls moving sum 1 min.png'))
}

# create one pdf
# paper
# loop through all species
for (i in unique(data.filter.sec.sum$Common.Name)) {
  
  p = data.filter.sec.sum %>% 
    filter(Common.Name == i) %>% 
    mutate(Day = ifelse(Date == 8 & Time.day == 'afternoon',
                               'Eclipse',
                               'Normal Day')) %>% 
    ggplot() +
    annotate("rect",
             xmin = 54295/3600,
             xmax = 54535/3600,
             ymin = 0,
             ymax = Inf,
             color = '#404040') +
    geom_vline(xintercept = 24750/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 26460/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 72960/3600,
               linetype = 'dashed') +
    geom_vline(xintercept = 74580/3600,
               linetype = 'dashed') +
    geom_line(aes(x = Min.group/3600,
                  y = Sum,
                  group = paste0(Date, Time.day),
                  color = factor(Day,
                                 levels = c('Normal Day',
                                            'Eclipse')),
                  alpha = Date),
              linewidth = 1) +
    theme_classic(base_size = 12) +
    xlab('Time (hrs)') +
    ylab('Vocalizations per min') +
    xlim(min(data.filter.sec.sum$Min.group)/3600,
         max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle(paste0(i)) +
    scale_color_manual(values = c('black',
                                  '#00C58F'),
                       name = 'Day') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600)) +
    theme(axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.line.x.top = element_blank())
  ggsave(paste0('figures/moving_sum/moving_sum_species_1/',
                i, 
                ' calls moving sum 1 min.png'))

}

# # create one pdf
# # paper
# # loop through all species
# 
#   p = lapply(unique(data.filter.sec.sum$Common.Name)[1:6], function(i) {
#     data.filter.sec.sum %>% 
#     filter(Common.Name == i) %>% 
#     mutate(Day = ifelse(Date == 8 & Time.day == 'afternoon',
#                         'Eclipse',
#                         'Normal Day')) %>% 
#     ggplot() +
#     annotate("rect",
#              xmin = 54295/3600,
#              xmax = 54535/3600,
#              ymin = 0,
#              ymax = Inf,
#              color = '#404040') +
#     geom_vline(xintercept = 24750/3600,
#                linetype = 'dashed') +
#     geom_vline(xintercept = 26460/3600,
#                linetype = 'dashed') +
#     geom_vline(xintercept = 72960/3600,
#                linetype = 'dashed') +
#     geom_vline(xintercept = 74580/3600,
#                linetype = 'dashed') +
#     geom_line(aes(x = Min.group/3600,
#                   y = Sum,
#                   group = paste0(Date, Time.day),
#                   color = factor(Day,
#                                  levels = c('Normal Day',
#                                             'Eclipse')),
#                   alpha = Date),
#               linewidth = 1) +
#     theme_classic(base_size = 12) +
#     xlab('Time (hrs)') +
#     ylab('Vocalizations per min') +
#     xlim(min(data.filter.sec.sum$Min.group)/3600,
#          max(data.filter.sec.sum$Min.group)/3600) +
#     ggtitle(paste0(i)) +
#     scale_color_manual(values = c('black',
#                                   '#00C58F'),
#                        name = 'Day') +
#     scale_x_break(c(29100/3600,
#                     50695/3600)) +
#     scale_x_break(c(58135/3600,
#                     70200/3600)) +
#     theme(axis.text.x.top = element_blank(),
#           axis.ticks.x.top = element_blank(),
#           axis.line.x.top = element_blank())
#   
# })
#   
#   ggsave(
#     filename = "test.pdf", 
#     plot = gridExtra::marrangeGrob(p,
#                         nrow=6,
#                         ncol=1)
#   )
#   

## graph sum 4 minute window 
# loop through all species
for (i in unique(data.filter.sec.sum.4$Common.Name)) {
  
  data.filter.sec.sum %>% 
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
    geom_line(aes(x = Min.group/3600,
                  y = Sum,
                  group = paste0(Date, Time.day),
                  color = as.factor(Date)),
              linewidth = 2) +
    theme_classic() +
    xlab('Time (hrs)') +
    ylab('Calls per 4 minute window') +
    xlim(min(data.filter.sec.sum$Min.group)/3600,
         max(data.filter.sec.sum$Min.group)/3600) +
    ggtitle(paste0(i,
                   ": 4 min sum")) +
    scale_color_manual(values = c('orange',
                                  'red',
                                  'black',
                                  'yellow'),
                       name = 'Date') +
    scale_x_break(c(29100/3600,
                    50695/3600)) +
    scale_x_break(c(58135/3600,
                    70200/3600))
  ggsave(paste0('figures/moving_sum/moving_sum_species_4/',
                i, 
                ' calls moving sum 4 min.png'))
}