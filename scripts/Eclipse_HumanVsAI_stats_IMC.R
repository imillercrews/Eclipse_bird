#### Eclipse bird analysis 
### Comparing Human vs AI generated data 
## statistical analysis
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
# load library
library(tidyverse)
library(jtools)
library(lme4)
library(multcompView)
library(gridExtra)
library(rstatix)
# library(mgcv)
# library(glmmTMB)

#### load data ####
## load 4 minute summary data
data.filter.sec.sum.4 = read.csv('data/data_output/four.min.sum/data.filter.sec.sum.4.csv')
# recorder
data.filter.sec.sum.recorder.4 = read.csv('data/data_output/four.min.sum/data.filter.sec.sum.recorder.4.csv')


## load minute summary data
# species sum
data.filter.sec.sum = read.csv('data/data_output/one.min.sum/data.filter.sec.sum.csv')

# filter to 4 minutes of totality and 4 minutes afterwards
# make names all lowercase
data.ai.sum = data.filter.sec.sum %>% 
  filter(Date==8) %>% 
  filter(Time.day=="afternoon") %>% 
  filter(Min.eclipse>=0) %>% 
  filter(Min.eclipse<=7) %>% 
  mutate(Common.Name = tolower(Common.Name)) %>% 
  group_by(Common.Name) %>% 
  summarise(AI.sum = sum(Sum))

## load Human data
data.human = read.csv('data/data_raw/evan_counts_recorder.csv')

# rename variables
colnames(data.human) = c(
  "Common.Name",
  "Recorder",
  "Sum"
)

# sum human data
data.human.sum = data.human %>% 
  group_by(Common.Name) %>% 
  summarise(Human.sum = sum(Sum))

### combine human and AI data
data.human.ai.sum = data.human.sum %>% 
  left_join(data.ai.sum)

### old
### Human data for 7th and 8th across time chunks
# # remove space before species name
# data.human.chunk = read.csv('data/filtered manual scores.csv') %>% 
#   mutate(spp = str_sub(spp,
#                        2))
# 
# 
# tmp = read.csv('data/data_raw/manual scoring_4 time points.csv') %>% 
#   dplyr::rename(spp = SPP.NAME) %>% 
#   mutate(spp = str_sub(spp,
#                        2))
# 
# ## make long format
# data.human.chunk.long = data.human.chunk %>% 
#   pivot_longer(cols = c("first..the.7th.3pm.",
#                         "second..the.8th.3pm",
#                         "third..the.4.min.just.after",
#                         "fourth..the.half.hour.after"),
#                names_to = 'time.chunk',
#                values_to = 'counts') %>% 
#   mutate(time.chunk = case_when(time.chunk == "first..the.7th.3pm." ~ "7th.0-4",
#                                 time.chunk == "second..the.8th.3pm" ~ "8th.0-4",
#                                 time.chunk == "third..the.4.min.just.after" ~ "8th.4-8",
#                                 time.chunk == "fourth..the.half.hour.after" ~ "8th.28-32"))
# 
# 
# ## sum species count across time.bin
# data.human.chunk.long.sum = data.human.chunk.long %>% 
#   group_by(recorder,
#            spp,
#            time.chunk) %>% 
#   summarise(sum.counts = sum(counts)) %>% 
#   ungroup()
# # convert to factors
# data.human.chunk.long.sum = data.human.chunk.long.sum %>% 
#   mutate(recorder = as.factor(recorder),
#          spp = as.factor(spp),
#          time.chunk = as.factor(time.chunk))

### new
### Human data for 7th and 8th across time chunks
# remove space before species name
data.human.chunk = read.csv('data/data_raw/manual scoring_4 time points.csv') %>% 
  dplyr::rename(spp = SPP.NAME) %>% 
  mutate(spp = str_sub(spp,
                       2))

## make long format
data.human.chunk.long = data.human.chunk %>% 
  pivot_longer(cols = c("counts_daybefore",
                        "counts_during",
                        "counts_after",
                        "counts_after_after"),
               names_to = 'time.chunk',
               values_to = 'counts') %>% 
  mutate(time.chunk = case_when(time.chunk == "counts_daybefore" ~ "7th.0-4",
                                time.chunk == "counts_during" ~ "8th.0-4",
                                time.chunk == "counts_after" ~ "8th.4-8",
                                time.chunk == "counts_after_after" ~ "8th.28-32")) %>% 
  dplyr::rename(sum.counts = counts)

# convert to factors
data.human.chunk.long.sum = data.human.chunk.long %>% 
  mutate(recorder = as.factor(recorder),
         spp = as.factor(spp),
         time.chunk = as.factor(time.chunk))

#### compare human to ai data ####
### graph comparison
data.human.ai.sum %>% 
  ggplot(aes(
    x = Human.sum,
    y = AI.sum
  )) +
  geom_point() +
  theme_classic()


# #### run GLM
# ### use poisson model 
# Human.ai.model.p <- glm(AI.sum ~ Human.sum,
#                       family = 'poisson', 
#                       data = data.human.ai.sum)
# summary(Human.ai.model.p)
# 
# # diagnostic plots
# par(mfrow = c(2,3))
# plot(Human.ai.model.p,
#      which = 1:6)
# 
# # check for outlier
# round(cooks.distance(Human.ai.model.p))
# # remove robins
# 
# ## poisson model again with outlier removed
# # remove outlier 
# data.human.ai.sum.p.outlier = data.human.ai.sum[- which.max(round(cooks.distance(Human.ai.model.p))),]
# # run model
# Human.ai.model.p.outlier <- glm(AI.sum ~ Human.sum,
#                         family = 'poisson', 
#                         data = data.human.ai.sum.p.outlier)
# summary(Human.ai.model.p.outlier)
# 
# # diagnostic plots
# par(mfrow = c(2,3))
# plot(Human.ai.model.p.outlier,
#      which = 1:6)
# 
# # check for outlier
# round(cooks.distance(Human.ai.model.p.outlier))
# # no outliers by cooks distance
# 
### use quasipoisson model to deal with overdispersion
Human.ai.model.o <- glm(AI.sum ~ Human.sum,
                     family = 'quasipoisson',
                     data = data.human.ai.sum )
# summary(Human.ai.model.o)
# 
# # diagnostic plots
# par(mfrow = c(2,3))
# plot(Human.ai.model.o,
#      which = 1:6)
# 
# # check for outlier
# round(cooks.distance(Human.ai.model.o))
# # remove robins
# # Calculate R-squared
# 1 - (summary(Human.ai.model.o)$deviance / summary(Human.ai.model.o)$null.deviance)
# # pseudo-Rsquared or D-squared
# # explained deviance of model
# 1-(Human.ai.model.o$deviance/Human.ai.model.o$null.deviance)
# # 0.5788553
# 
# # slope
# exp(Human.ai.model.o$coefficients[2])
# # 1.006837
# 
# 100*(exp(Human.ai.model.o$coefficients[2]) - 1)
# # 0.68%

## quasipoisson model again with outlier removed
# remove outlier 
data.human.ai.sum.o.outlier = data.human.ai.sum[- which.max(round(cooks.distance(Human.ai.model.o))),]
# run model
Human.ai.model.o.outlier <- glm(AI.sum ~ Human.sum,
                      family = 'quasipoisson', 
                      data = data.human.ai.sum.o.outlier)
summary(Human.ai.model.o.outlier)

# diagnostic plots
png('figures/all_sp_HumanVsAI/Human counts vs AI counts quasipoisson model diagnostic.png')
par(mfrow = c(2,3))
plot(Human.ai.model.o.outlier,
     which = 1:6)
dev.off()

# check for outlier
round(cooks.distance(Human.ai.model.o.outlier))
# no outliers by cooks distance

# pseudo-Rsquared or D-squared
# explained deviance of model
1-(Human.ai.model.o.outlier$deviance/Human.ai.model.o.outlier$null.deviance)
# 0.4193799

# slope
exp(Human.ai.model.o.outlier$coefficients[2])
# 1.013055

100*(exp(Human.ai.model.o.outlier$coefficients[2]) - 1)
# 1.31%

## graph
# model with outlier removed
effect_plot(Human.ai.model.o.outlier,
            pred = Human.sum,
            interval = T,
            int.type = 'confidence',
            int.width = 0.95,
            rug = T,
            plot.points = TRUE) +
  geom_point(data = data.human.ai.sum %>% 
               filter(Common.Name != 'american robin'),
             aes(x = Human.sum,
                 y = AI.sum))+
  xlim(0,150) +
  ylim(0,150)
ggsave('figures/all_sp_HumanVsAI/Human counts vs AI counts quasipoisson model outlier removed.png',
       height = 5,
       width = 5)

# model with outlier 
effect_plot(Human.ai.model.o.outlier,
            pred = Human.sum,
            interval = T,
            int.type = 'confidence',
            int.width = 0.95,
            rug =TRUE,
            plot.points = TRUE) +
  geom_point(data = data.human.ai.sum,
             aes(x = Human.sum,
                 y = AI.sum)) +
  xlim(0,350) +
  ylim(0,350)
ggsave('figures/all_sp_HumanVsAI/Human counts vs AI counts quasipoisson model outlier.png',
       height = 5,
       width = 5)

# model with outlier removed paper
effect_plot(Human.ai.model.o.outlier,
            pred = Human.sum,
            interval = F,
            # int.type = 'confidence',
            # int.width = 0.95,
            rug = F,
            plot.points = TRUE,
            line.colors = 'grey50') +
  geom_point(data = data.human.ai.sum %>% 
               filter(Common.Name != 'american robin'),
             aes(x = Human.sum,
                 y = AI.sum))+
  xlim(0,150) +
  ylim(0,150) +
  xlab('Counts (Human scored)') +
  ylab('Counts (AI scored)') +
  theme_classic(base_size = 12) 
ggsave('figures/all_sp_HumanVsAI/Human counts vs AI counts quasipoisson model outlier removed paper.pdf',
       height = 3,
       width = 3,
       units = 'in')

# # graph residuals and mean
# # get average of both for groups of 4
# data.frame(Human.sum = Human.ai.model.p$data$Human.sum,
#            residuals.square = (Human.ai.model.p$residuals)^2) %>% 
#   arrange(Human.sum) %>% 
#   cbind(data.frame(group = rep(1:5, each = 4))) %>% 
#   group_by(group) %>% 
#   summarise(Human.sum.avg = mean(Human.sum),
#             residuals.square.avg = mean(residuals.square)) %>% 
#   ggplot(aes(
#     x = Human.sum.avg,
#     y = residuals.square.avg
#   )) +
#   geom_point()
# 
# data.frame(Human.sum = Human.ai.model.p.outlier$data$Human.sum,
#            residuals.square = (Human.ai.model.p.outlier$residuals)^2) %>% 
#   arrange(Human.sum) %>% 
#   cbind(data.frame(group = rep(1:5, each = 4, length.out = 19))) %>% 
#   group_by(group) %>% 
#   summarise(Human.sum.avg = mean(Human.sum),
#             residuals.square.avg = mean(residuals.square)) %>% 
#   ggplot(aes(
#     x = Human.sum.avg,
#     y = residuals.square.avg
#   )) +
#   geom_point()
# 
# data.frame(Human.sum = Human.ai.model.o$data$Human.sum,
#            residuals.square = (Human.ai.model.o$residuals)^2) %>% 
#   arrange(Human.sum) %>% 
#   cbind(data.frame(group = rep(1:5, each = 4))) %>% 
#   group_by(group) %>% 
#   summarise(Human.sum.avg = mean(Human.sum),
#             residuals.square.avg = mean(residuals.square)) %>% 
#   ggplot(aes(
#     x = Human.sum.avg,
#     y = residuals.square.avg
#   )) +
#   geom_point()
# 
# data.frame(Human.sum = Human.ai.model.o.outlier$data$Human.sum,
#            residuals.square = (Human.ai.model.o.outlier$residuals)^2) %>% 
#   arrange(Human.sum) %>% 
#   cbind(data.frame(group = rep(1:5, each = 4, length.out = 19))) %>% 
#   group_by(group) %>% 
#   summarise(Human.sum.avg = mean(Human.sum),
#             residuals.square.avg = mean(residuals.square)) %>% 
#   ggplot(aes(
#     x = Human.sum.avg,
#     y = residuals.square.avg
#   )) +
#   geom_point()





#### Run stats on eclipse day with Human data ####
### compare Human 0-4 minutes on 7th to AI data for each bird
## make histogram graph for each bird in human data
# get list of birds from human data
human.bird.list = data.human.chunk.long.sum %>% 
  pull(spp) %>% 
  unique()

# print all into one pdf
pdf("figures/all_sp_HumanVsAI/Human 7th 0-4 minute vs AI 7th data.pdf",
    onefile = TRUE)
for (i in human.bird.list) {
  
  # get data for species on the 7th
  tmp = data.filter.sec.sum.4 %>% 
    mutate(Common.Name = tolower(Common.Name)) %>% 
    filter(Common.Name == i) %>% 
    filter(Time.day == 'afternoon') %>% 
    filter(Date == 7) 
  
  # get value from human data for 0 to 4 minutes
  value.human.7 = data.human.chunk.long.sum %>% 
    filter(spp == i) %>% 
    filter(time.chunk == "7th.0-4") %>% 
    summarise(total = sum(sum.counts)) %>% 
    pull(total)
  
  # get value from AI data for 0 to 4 minutes
  value.AI.7 = tmp %>% 
    filter(Min.eclipse == 0) %>% 
    pull(Sum)
  
  # make dataframe of totality on 7th time values
  value.7 = data.frame(Data.type = c("Human",
                                     "AI"),
                       value = c(value.human.7,
                                 value.AI.7)) %>% 
    mutate(Data.type = fct_relevel(Data.type,
                               c("Human",
                                 "AI")))
  
  # graph for 2 hours around totality time on 7th
  p1 = tmp %>% 
    ggplot(aes(Sum)) +
    geom_histogram(binwidth = 1) +
    geom_vline(data = value.7,
               aes(xintercept = value,
                   color = Data.type)) + 
    theme_classic() +
    ggtitle(paste0(i,
                   ": 7th afternoon call histogram 2 hour")) +
    xlab('Number of calls per 4 min window') +
    scale_color_manual(values = c('red',
                                  'black'))
  
  # graph for 1 hour around totality time on 7th
  p2 = tmp %>% 
    filter(Min.eclipse > -30) %>% 
    filter(Min.eclipse < 30) %>% 
    ggplot(aes(Sum)) +
    geom_histogram(binwidth = 1) +
    geom_vline(data = value.7,
               aes(xintercept = value,
                   color = Data.type)) + 
    theme_classic() +
    ggtitle(paste0(i,
                   ": 7th afternoon call histogram 1 hour")) +
    xlab('Number of calls per 4 min window') +
    scale_color_manual(values = c('black',
                                  'red'))
  
 grid.arrange(p1,p2, nrow = 2)
  
}
dev.off()

### pairwise models across time chunks
## set up time chunks in AI data
data.filter.sec.sum.recorder.4.chunk = data.filter.sec.sum.recorder.4 %>% 
  mutate(Common.Name = tolower(Common.Name)) %>% 
  filter(Time.day == 'afternoon') %>% 
  mutate(time.chunk = case_when(Min.eclipse == 0 & Date == 7 ~ "7th.0-4",
                                Min.eclipse == 0 & Date == 8 ~ "8th.0-4",
                                Min.eclipse == 4 & Date == 8 ~ "8th.4-8",
                                Min.eclipse == 28 & Date == 8 ~ "8th.28-32",
                                TRUE ~ NA),
         data.type = "AI") %>% 
  na.omit() %>% 
  rename(spp = Common.Name) %>% 
  rename(recorder = Recorder) %>% 
  rename(sum.counts = Sum) %>% 
  select(-c(X,
            Date,
            Time.day,
            Min.group,
            Min.eclipse))

## make summary graph for each bird
# print all into one pdf
pdf("figures/all_sp_HumanVsAI/Human counts summary per bird.pdf",
    onefile = TRUE)
for(i in unique(data.human.chunk.long.sum$spp)){
  p <-
    data.human.chunk.long.sum %>% 
    filter(spp == i) %>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    ggplot(aes(x = time.chunk, 
               y = sum.counts)) +
    geom_vline(xintercept = 1.5) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(height = 0,
                width = 0.1) +
    theme_classic() +
    ylab('4 minute human scoring window') +
    xlab('Bird calls per recorder') +
    theme(legend.position = 'none') +
    ggtitle(paste0(i,
                   ': human scored calls per 4 minutes'))
  print(p)
}
dev.off()

## make summary graph for each bird and AI
# get max range
max_y_range <- max(data.human.chunk.long.sum %>% 
                     mutate(data.type = "Human") %>% 
                     full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                     pull(sum.counts)) - min(data.human.chunk.long.sum %>% 
                                               mutate(data.type = "Human") %>% 
                                               full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                                               pull(sum.counts))

# print all into one pdf
pdf("figures/all_sp_HumanVsAI/Human and AI counts summary per bird.pdf",
    onefile = TRUE)
for(i in unique(data.filter.sec.sum.recorder.4.chunk$spp)){
  p <-
    data.human.chunk.long.sum %>% 
    mutate(data.type = "Human") %>% 
    full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
    filter(spp == i) %>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>%
    ggplot(aes(x = time.chunk, 
               y = sum.counts,
               fill = data.type)) +
    geom_vline(xintercept = 1.5) + 
    geom_boxplot(outlier.shape = NA,
                 position = position_dodge(width = 0.5)) +
    geom_dotplot(binaxis = "y",
                 stackdir = 'center',
                 position = position_dodge(width = 0.5),
                 color = 'black',
                 binwidth = 1,
                 dotsize = (max(data.human.chunk.long.sum %>% 
                                  mutate(data.type = "Human") %>% 
                                  full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                                  filter(spp == i) %>% 
                                  pull(sum.counts)) - min(data.human.chunk.long.sum %>% 
                                                            mutate(data.type = "Human") %>% 
                                                            full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                                                            filter(spp == i) %>% 
                                                            pull(sum.counts)))/max_y_range) +
    theme_classic() +
    ylab('4 minute scoring window') +
    xlab('Bird calls per recorder') +
    ggtitle(paste0(i,
                   ': scored calls per 4 minutes')) +
    scale_fill_manual(values = c("red",
                                 "darkgrey"))
  print(p)
}
dev.off()

#### human data
### run wilcox test on each group pairwise 
## create empty summary dataframe
data.human.chunk.wilcox.results = data.frame(p.val.adj = as.numeric(),
                                             comparison = as.character(),
                                             Species = as.character())

## run for each species seperately
for (i in unique(data.human.chunk.long.sum$spp)) {
  # kruskal.test(sum.counts ~ time.chunk, 
  #              data = data.human.chunk.long.sum %>% 
  #                filter(spp == i))
  # 
  
  ## run pairwise wilcox test
  tmp = pairwise.wilcox.test(data.human.chunk.long.sum %>%
                         filter(spp == i) %>%
                         pull(sum.counts),
                       data.human.chunk.long.sum %>%
                         filter(spp == i) %>%
                         pull(time.chunk),
                       p.adjust.method = "fdr")

  # extract p-value
  # rename with comparison
  tmp.p = tmp$p.value %>%
    as.data.frame %>%
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    na.omit() %>%
    rename(p.val.adj = value) %>%
    mutate(comparison = case_when(name == "7th.0-4" ~ paste0(name,
                                                             "vs",
                                                             rowname),
                                  name == "8th.0-4" ~ paste0(name,
                                                             "vs",
                                                             rowname),
                                  name == "8th.28-32" ~ paste0(rowname,
                                                             "vs",
                                                             name)),
           Species = i) %>%
    arrange(comparison) %>%
    select(-c(rowname,
              name))
  
  
  
  # add to dataframe
  data.human.chunk.wilcox.results = tmp.p %>% 
    rbind(data.human.chunk.wilcox.results)
  
}

# add missing comparisons
# set p-value to NA
data.human.chunk.wilcox.results = data.human.chunk.wilcox.results %>% 
  full_join(expand.grid(comparison = unique(data.human.chunk.wilcox.results$comparison),
                        Species = unique(data.human.chunk.wilcox.results$Species)))

### save results
write.csv(data.human.chunk.wilcox.results, 
          file = 'data/data_output/data.human.chunk.wilcox.results.csv')

# ## add significance to summary graph for each bird
# # print all into one pdf
# pdf("figures/all_sp_HumanVsAI/Human counts summary per bird sig.pdf",
#     onefile = TRUE)
# for(i in unique(data.human.chunk.long.sum$spp)){
#   # get pvalues and set non-significance to 1
#   # rename to letters
#   tmp =
#     data.human.chunk.wilcox.results %>% 
#     filter(Species == i) %>% 
#     mutate(p.val.adj.sig = ifelse(p.val.adj < 0.05,
#                                   p.val.adj,
#                                   1),
#            p.val.adj.sig = ifelse(is.na(p.val.adj),
#                                   1,
#                                   p.val.adj)) %>% 
#     mutate(comp = str_replace_all(comparison,
#                                   "7th.0-4",
#                                   "a"),
#            comp = str_replace_all(comp,
#                                   "8th.0-4",
#                                   "b"),
#            comp = str_replace_all(comp,
#                                   "8th.4-8",
#                                   "c"),
#            comp = str_replace_all(comp,
#                                   "8th.28-32",
#                                   "d"),
#            comp = str_replace_all(comp,
#                                   'vs',
#                                   '-')) 
#   
#   # get letters
#   tmp.p = tmp$p.val.adj.sig
#   names(tmp.p) = tmp$comp
#   tmp.let = multcompLetters(tmp.p)
#   
#   # convert to data frame
#   tmp.let.df = tmp.let$Letters %>% 
#     as.data.frame() %>% 
#     rename(group = '.') %>% 
#     rownames_to_column('time.chunk') %>% 
#     mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
#                                   time.chunk == "b" ~ "8th.0-4",
#                                   time.chunk == "c" ~ "8th.4-8",
#                                   time.chunk == "d" ~ "8th.28-32"))%>% 
#     mutate(time.chunk = fct_relevel(time.chunk,
#                                     c("7th.0-4",
#                                       "8th.0-4",
#                                       "8th.4-8",
#                                       "8th.28-32"))) %>% 
#     mutate(y = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              pull(sum.counts) %>% 
#              max())
# 
#   
#   
#   # create graph
#   p <-
#     data.human.chunk.long.sum %>% 
#     filter(spp == i) %>% 
#     mutate(time.chunk = fct_relevel(time.chunk,
#                                     c("7th.0-4",
#                                       "8th.0-4",
#                                       "8th.4-8",
#                                       "8th.28-32"))) %>% 
#     ggplot(aes(x = time.chunk, 
#                y = sum.counts,
#                group = time.chunk)) +
#     geom_vline(xintercept = 1.5) + 
#     geom_boxplot(outlier.shape = NA) +
#       geom_jitter(height = 0,
#                   width = 0.05) +
#     geom_text(data = tmp.let.df,
#               aes(x = time.chunk,
#                   y = y,
#                   label = group),
#               vjust = "inward",
#               hjust = 2) + 
#     theme_classic() +
#     ylab('4 minute human scoring window') +
#     xlab('Bird calls per recorder') +
#     theme(legend.position = 'none') +
#     ggtitle(paste0(i,
#                    ': human scored calls per 4 minutes'))
#   print(p)
# }
# dev.off()

### create mini plots on one graph for paper
## create list of significant difference letters from Dunn
# for each species 
# create empty dataframe
tmp.let.df = data.frame(time.chunk = as.character(),
                        group = as.character(),
                        y = as.numeric(),
                        Species = as.character())

# loop through each species to get letters per time chunk  
for(i in unique(data.human.chunk.long.sum$spp)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.human.chunk.wilcox.results %>% 
    filter(Species == i) %>%
    mutate(p.val.adj.sig = ifelse(p.val.adj < 0.05,
                              p.val.adj,
                              1),
           p.val.adj.sig = ifelse(is.na(p.val.adj),
                              1,
                              p.val.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.val.adj.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.human.chunk.long.sum %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max(),
           Species = i) %>% 
    rbind(tmp.let.df)
}


# create graph of mini boxplots
data.human.chunk.long.sum %>% 
  mutate(time.chunk = fct_relevel(time.chunk,
                                  c("7th.0-4",
                                    "8th.0-4",
                                    "8th.4-8",
                                    "8th.28-32"))) %>% 
  dplyr::rename(Species = spp) %>% 
  ggplot(aes(x = time.chunk, 
             y = sum.counts,
             group = time.chunk,
             fill = time.chunk)) +
  geom_vline(xintercept = 1.5) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0,
              width = 0.05) +
  geom_text(data = tmp.let.df,
            aes(x = time.chunk,
                y = y,
                label = group),
            vjust = "inward",
            hjust = 1) + 
  theme_classic() +
  ylab('Bird calls per recorder') +
  xlab('4-minute human scoring window') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  facet_wrap(~Species,
             nrow = 5,
             ncol = 4,
             scales = "free_y") +
  scale_fill_manual(values = c("7th.0-4" = "lightgrey",
                               "8th.0-4" = "#404040",
                               "8th.4-8" = "#EE8012",
                               "8th.28-32" = "#FFC000")) +
  ggtitle('Human wilcox')
ggsave("figures/all_sp_HumanVsAI/Human counts summary per bird sig paper.png",
       height = 10,
       width = 8)

### run dunn test on each group pairwise 
## create empty summary dataframe
data.human.chunk.dunn.results = data.frame(p.val.adj = as.numeric(),
                                             comparison = as.character(),
                                             Species = as.character())

## run for each species seperately
for (i in unique(data.human.chunk.long.sum$spp)) {
    # dunn test
  tmp = dunn_test(sum.counts ~ time.chunk,
                  data = data.human.chunk.long.sum %>%
                    filter(spp == i),
                  p.adjust.method = "fdr")
  
  
  
  # extract p-value from Dunn
  # rename with comparison
  tmp.p = tmp %>% 
    na.omit() %>% 
    mutate(comparison = case_when(group1 == "7th.0-4" ~ paste0(group1,
                                                               "vs",
                                                               group2),
                                  group1 == "8th.0-4" ~ paste0(group1,
                                                               "vs",
                                                               group2),
                                  group1 == "8th.28-32" ~ paste0(group2,
                                                                 "vs",
                                                                 group1)),
           Species = i) %>% 
    arrange(comparison) %>% 
    select(c(comparison,
             Species,
             statistic,
             p,
             p.adj))
  
  # add to dataframe
  data.human.chunk.dunn.results = tmp.p %>% 
    rbind(data.human.chunk.dunn.results)
  
}

# add missing comparisons
# set p-value to NA
data.human.chunk.dunn.results = data.human.chunk.dunn.results %>% 
  full_join(expand.grid(comparison = unique(data.human.chunk.dunn.results$comparison),
                        Species = unique(data.human.chunk.dunn.results$Species)))

### save results
write.csv(data.human.chunk.dunn.results, 
          file = 'data/data_output/data.human.chunk.dunn.results.csv')

# ## add significance to summary graph for each bird
# # print all into one pdf
# pdf("figures/all_sp_HumanVsAI/Human counts summary per bird sig dunn.pdf",
#     onefile = TRUE)
# for(i in unique(data.human.chunk.long.sum$spp)){
#   # get pvalues and set non-significance to 1
#   # rename to letters
#   tmp =
#     data.human.chunk.dunn.results %>% 
#     filter(Species == i) %>% 
#     mutate(p.adj.sig = ifelse(p.adj < 0.05,
#                               p.adj,
#                               1),
#            p.adj.sig = ifelse(is.na(p.adj),
#                               1,
#                               p.adj)) %>% 
#     mutate(comp = str_replace_all(comparison,
#                                   "7th.0-4",
#                                   "a"),
#            comp = str_replace_all(comp,
#                                   "8th.0-4",
#                                   "b"),
#            comp = str_replace_all(comp,
#                                   "8th.4-8",
#                                   "c"),
#            comp = str_replace_all(comp,
#                                   "8th.28-32",
#                                   "d"),
#            comp = str_replace_all(comp,
#                                   'vs',
#                                   '-')) 
#   
#   # get letters
#   tmp.p = tmp$p.adj.sig
#   names(tmp.p) = tmp$comp
#   tmp.let = multcompLetters(tmp.p)
#   
#   # convert to data frame
#   tmp.let.df = tmp.let$Letters %>% 
#     as.data.frame() %>% 
#     rename(group = '.') %>% 
#     rownames_to_column('time.chunk') %>% 
#     mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
#                                   time.chunk == "b" ~ "8th.0-4",
#                                   time.chunk == "c" ~ "8th.4-8",
#                                   time.chunk == "d" ~ "8th.28-32"))%>% 
#     mutate(time.chunk = fct_relevel(time.chunk,
#                                     c("7th.0-4",
#                                       "8th.0-4",
#                                       "8th.4-8",
#                                       "8th.28-32"))) %>% 
#     mutate(y = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              pull(sum.counts) %>% 
#              max())
#   
#   
#   
#   # create graph
#   p <-
#     data.human.chunk.long.sum %>% 
#     filter(spp == i) %>% 
#     mutate(time.chunk = fct_relevel(time.chunk,
#                                     c("7th.0-4",
#                                       "8th.0-4",
#                                       "8th.4-8",
#                                       "8th.28-32"))) %>% 
#     ggplot(aes(x = time.chunk, 
#                y = sum.counts,
#                group = time.chunk)) +
#     geom_vline(xintercept = 1.5) + 
#     geom_boxplot(outlier.shape = NA) +
#     geom_jitter(height = 0,
#                 width = 0.05) +
#     geom_text(data = tmp.let.df,
#               aes(x = time.chunk,
#                   y = y,
#                   label = group),
#               vjust = "inward",
#               hjust = 2) + 
#     theme_classic() +
#     ylab('4 minute human scoring window') +
#     xlab('Bird calls per recorder') +
#     theme(legend.position = 'none') +
#     ggtitle(paste0(i,
#                    ': human scored calls per 4 minutes'))
#   print(p)
# }
# dev.off()


### create mini plots on one graph for paper
# load data
data.human.chunk.dunn.results = read.csv('data/data_output/data.human.chunk.dunn.results.csv') %>% 
  dplyr::select(-X)

## create list of significant difference letters from Dunn
# for each species 
# create empty dataframe
tmp.let.df = data.frame(time.chunk = as.character(),
                        group = as.character(),
                        y = as.numeric(),
                        Species = as.character())

# loop through each species to get letters per time chunk  
for(i in unique(data.human.chunk.long.sum$spp)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.human.chunk.dunn.results %>% 
    filter(Species == i) %>%
    mutate(p.adj.sig = ifelse(p.adj < 0.05,
                              p.adj,
                              1),
           p.adj.sig = ifelse(is.na(p.adj),
                              1,
                              p.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.adj.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.human.chunk.long.sum %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max(),
           Species = i) %>% 
    rbind(tmp.let.df)
}


# create graph of mini boxplots
data.human.chunk.long.sum %>% 
  mutate(time.chunk = fct_relevel(time.chunk,
                                  c("7th.0-4",
                                    "8th.0-4",
                                    "8th.4-8",
                                    "8th.28-32"))) %>% 
  dplyr::rename(Species = spp) %>% 
  ggplot(aes(x = time.chunk, 
             y = sum.counts,
             group = time.chunk,
             fill = time.chunk)) +
  geom_vline(xintercept = 1.5) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0,
              width = 0.05) +
  geom_text(data = tmp.let.df,
            aes(x = time.chunk,
                y = y,
                label = group),
            vjust = "inward",
            hjust = 1) + 
  theme_classic() +
  ylab('Bird calls per recorder') +
  xlab('4-minute human scoring window') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  facet_wrap(~Species,
             nrow = 5,
             ncol = 4,
             scales = "free_y") +
  scale_fill_manual(values = c("7th.0-4" = "lightgrey",
                               "8th.0-4" = "#404040",
                               "8th.4-8" = "#EE8012",
                               "8th.28-32" = "#FFC000"))+
  ggtitle('Human dunn')
ggsave("figures/all_sp_HumanVsAI/Human counts summary per bird sig dunn paper.png",
       height = 10,
       width = 8)

### run zero inflated wilcox test on each group pairwise 
library(ZIR)

## create pairwise function for zero inflated wilcox test
pairwise.wilcox.zero.inflated.test= function (x, g, p.adjust.method = p.adjust.methods, perm.n = perm.n, 
                                              ...) 
{
  p.adjust.method <- match.arg(p.adjust.method)
  DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(g)))
  g <- factor(g)
  
  compare.levels <- function(i, j) {
    xi <- x[as.integer(g) == i]
    xj <- x[as.integer(g) == j]
    wt <- ziw(xi, xj, perm.n = perm.n, ...)
    METHOD <<- wt$method
    wt$p.value
  }
  compare.levels.stat <- function(i, j) {
    xi <- x[as.integer(g) == i]
    xj <- x[as.integer(g) == j]
    wt <- ziw(xi, xj, perm.n = perm.n, ...)
    METHOD <<- wt$method
    wt$statistics
  }
  PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method = 'none')
  PVAL.adj <- pairwise.table(compare.levels, levels(g), p.adjust.method)
  STATS <- pairwise.table(compare.levels.stat, levels(g), p.adjust.method = 'none')
  ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
              p.adjust.method = p.adjust.method, p.value.adj = PVAL.adj, statistics = STATS)
  class(ans) <- "pairwise.htest"
  ans
}

## create empty summary dataframe
data.human.chunk.wilcox.zero.results = data.frame(p.val.adj = as.numeric(),
                                                  comparison = as.character(),
                                                  Species = as.character())

## run for each species seperately
for (i in unique(data.human.chunk.long.sum$spp)) {
  # kruskal.test(sum.counts ~ time.chunk, 
  #              data = data.human.chunk.long.sum %>% 
  #                filter(spp == i))
  # 
  
  ## run pairwise wilcox test
  tmp = pairwise.wilcox.zero.inflated.test(data.human.chunk.long.sum %>%
                                             filter(spp == i) %>%
                                             pull(sum.counts),
                                           data.human.chunk.long.sum %>%
                                             filter(spp == i) %>%
                                             pull(time.chunk),
                                           perm.n = 1000,
                                           p.adjust.method = 'fdr')
  
  # extract p-value
  # rename with comparison
  tmp.p = tmp$p.value %>%
    as.data.frame %>%
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    na.omit() %>%
    rename(p.val = value) %>%
    mutate(comparison = case_when(name == "7th.0-4" ~ paste0(name,
                                                             "vs",
                                                             rowname),
                                  name == "8th.0-4" ~ paste0(name,
                                                             "vs",
                                                             rowname),
                                  name == "8th.28-32" ~ paste0(name,
                                                               "vs",
                                                               rowname)),
           Species = i) %>%
    arrange(comparison) %>%
    select(-c(rowname,
              name)) %>% 
    full_join(tmp$p.value.adj %>%
                as.data.frame %>%
                rownames_to_column() %>%
                pivot_longer(-rowname) %>%
                na.omit() %>%
                rename(p.val.adj = value) %>%
                mutate(comparison = case_when(name == "7th.0-4" ~ paste0(name,
                                                                         "vs",
                                                                         rowname),
                                              name == "8th.0-4" ~ paste0(name,
                                                                         "vs",
                                                                         rowname),
                                              name == "8th.28-32" ~ paste0(name,
                                                                           "vs",
                                                                           rowname)),
                       Species = i) %>%
                arrange(comparison) %>%
                select(-c(rowname,
                          name))) %>% 
    full_join(tmp$statistics %>%
                as.data.frame %>%
                rownames_to_column() %>%
                pivot_longer(-rowname) %>%
                na.omit() %>%
                rename(statistic = value) %>%
                mutate(comparison = case_when(name == "7th.0-4" ~ paste0(name,
                                                                         "vs",
                                                                         rowname),
                                              name == "8th.0-4" ~ paste0(name,
                                                                         "vs",
                                                                         rowname),
                                              name == "8th.28-32" ~ paste0(name,
                                                                           "vs",
                                                                           rowname)),
                       Species = i) %>%
                arrange(comparison) %>%
                select(-c(rowname,
                          name))) %>% 
    relocate(comparison) %>% 
    relocate(Species)

  # add to dataframe
  data.human.chunk.wilcox.zero.results = tmp.p %>% 
    rbind(data.human.chunk.wilcox.zero.results)
  
}

# add missing comparisons
# set p-value to NA
data.human.chunk.wilcox.zero.results = data.human.chunk.wilcox.zero.results %>% 
  full_join(expand.grid(comparison = unique(data.human.chunk.wilcox.zero.results$comparison),
                        Species = unique(data.human.chunk.wilcox.zero.results$Species)))

### save results
write.csv(data.human.chunk.wilcox.zero.results, 
          file = 'data/data_output/data.human.chunk.wilcox.zero.results.csv')

### create mini plots on one graph for paper
## create list of significant difference letters
# for each species 
# create empty dataframe
tmp.let.df = data.frame(time.chunk = as.character(),
                        group = as.character(),
                        y = as.numeric(),
                        Species = as.character())

# loop through each species to get letters per time chunk  
for(i in unique(data.human.chunk.long.sum$spp)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.human.chunk.wilcox.zero.results %>% 
    filter(Species == i) %>%
    mutate(p.val.adj.sig = ifelse(p.val.adj < 0.05,
                                  p.val.adj,
                                  1),
           p.val.adj.sig = ifelse(is.na(p.val.adj),
                                  1,
                                  p.val.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.val.adj.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.human.chunk.long.sum %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max(),
           Species = i) %>% 
    rbind(tmp.let.df)
}


# create graph of mini boxplots
data.human.chunk.long.sum %>% 
  mutate(time.chunk = fct_relevel(time.chunk,
                                  c("7th.0-4",
                                    "8th.0-4",
                                    "8th.4-8",
                                    "8th.28-32"))) %>% 
  dplyr::rename(Species = spp) %>% 
  ggplot(aes(x = time.chunk, 
             y = sum.counts,
             group = time.chunk,
             fill = time.chunk)) +
  geom_vline(xintercept = 1.5) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0,
              width = 0.05) +
  geom_text(data = tmp.let.df,
            aes(x = time.chunk,
                y = y,
                label = group),
            vjust = "inward",
            hjust = 1) + 
  theme_classic() +
  ylab('Bird calls per recorder') +
  xlab('4-minute human scoring window') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  facet_wrap(~Species,
             nrow = 5,
             ncol = 4,
             scales = "free_y") +
  scale_fill_manual(values = c("7th.0-4" = "lightgrey",
                               "8th.0-4" = "#404040",
                               "8th.4-8" = "#EE8012",
                               "8th.28-32" = "#FFC000")) +
  ggtitle('Human wilcox zero inflated')
ggsave("figures/all_sp_HumanVsAI/Human counts summary per bird sig zero inflated paper.png",
       height = 10,
       width = 8)

## Just use p-value not corrected pvalue
# create list of significant difference letters
# for each species 
# create empty dataframe
tmp.let.df = data.frame(time.chunk = as.character(),
                        group = as.character(),
                        y = as.numeric(),
                        Species = as.character())

# loop through each species to get letters per time chunk  
for(i in unique(data.human.chunk.long.sum$spp)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.human.chunk.wilcox.zero.results %>% 
    filter(Species == i) %>%
    mutate(p.val.sig = ifelse(p.val < 0.05,
                                  p.val,
                                  1),
           p.val.sig = ifelse(is.na(p.val),
                                  1,
                                  p.val)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.val.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.human.chunk.long.sum %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max(),
           Species = i) %>% 
    rbind(tmp.let.df)
}


# create graph of mini boxplots
data.human.chunk.long.sum %>% 
  mutate(time.chunk = fct_relevel(time.chunk,
                                  c("7th.0-4",
                                    "8th.0-4",
                                    "8th.4-8",
                                    "8th.28-32"))) %>% 
  dplyr::rename(Species = spp) %>% 
  ggplot(aes(x = time.chunk, 
             y = sum.counts,
             group = time.chunk,
             fill = time.chunk)) +
  geom_vline(xintercept = 1.5) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0,
              width = 0.05) +
  geom_text(data = tmp.let.df,
            aes(x = time.chunk,
                y = y,
                label = group),
            vjust = "inward",
            hjust = 1) + 
  theme_classic() +
  ylab('Bird calls per recorder') +
  xlab('4-minute human scoring window') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  facet_wrap(~Species,
             nrow = 5,
             ncol = 4,
             scales = "free_y") +
  scale_fill_manual(values = c("7th.0-4" = "lightgrey",
                               "8th.0-4" = "#404040",
                               "8th.4-8" = "#EE8012",
                               "8th.28-32" = "#FFC000")) +
  ggtitle('Human wilcox zero inflated not corrected')
ggsave("figures/all_sp_HumanVsAI/Human counts summary per bird sig zero inflated paper not corrected.png",
       height = 10,
       width = 8)


#### AI data
### run dunn test/Dunn test on each group pairwise 
## create empty summary dataframe
data.AI.chunk.dunn.results = data.frame(p.val.adj = as.numeric(),
                                             comparison = as.character(),
                                             Species = as.character())

## run for each species separately
# remove species with all zeros ("eastern phoebe","northern rough-winged swallow","pine siskin")
for (i in unique(data.filter.sec.sum.recorder.4.chunk$spp)[-c(18,33,35)]) {
    #  run pairwise dunn test
  tmp = dunn_test(sum.counts ~ time.chunk,
                           data = data.filter.sec.sum.recorder.4.chunk %>%
                             filter(spp == i),
                             p.adjust.method = "fdr")
  
  
  # extract p-value from Dunn
  # rename with comparison
  tmp.p = tmp %>% 
    na.omit() %>% 
    mutate(comparison = case_when(group1 == "7th.0-4" ~ paste0(group1,
                                                             "vs",
                                                             group2),
                                  group1 == "8th.0-4" ~ paste0(group1,
                                                             "vs",
                                                             group2),
                                  group1 == "8th.28-32" ~ paste0(group2,
                                                               "vs",
                                                               group1)),
           Species = i) %>% 
    arrange(comparison) %>% 
    select(c(comparison,
              Species,
             p.adj))
  
  # add to dataframe
  data.AI.chunk.dunn.results = tmp.p %>% 
    rbind(data.AI.chunk.dunn.results)
  
}

# add missing comparisons
# set p-value to NA
data.AI.chunk.dunn.results = data.AI.chunk.dunn.results %>% 
  full_join(expand.grid(comparison = unique(data.AI.chunk.dunn.results$comparison),
                        Species = unique(data.filter.sec.sum.recorder.4.chunk$spp)))

### save results
write.csv(data.AI.chunk.dunn.results, 
          file = 'data/data_output/data.AI.chunk.dunn.results.csv')

## add significance to summary graph for each bird
# print all into one pdf
pdf("figures/all_sp_HumanVsAI/AI counts summary per bird sig dunn.pdf",
    onefile = TRUE)
for(i in unique(data.AI.chunk.dunn.results$Species)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.AI.chunk.dunn.results %>% 
    filter(Species == i) %>% 
    mutate(p.adj.sig = ifelse(p.adj < 0.05,
                                  p.adj,
                                  1),
           p.adj.sig = ifelse(is.na(p.adj),
                                  1,
                                  p.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.adj.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.filter.sec.sum.recorder.4.chunk %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max())
  
  
  
  # create graph
  p <-
    data.human.chunk.long.sum %>% 
    filter(spp == i) %>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    ggplot(aes(x = time.chunk, 
               y = sum.counts,
               group = time.chunk)) +
    geom_vline(xintercept = 1.5) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(height = 0,
                width = 0.05) +
    geom_text(data = tmp.let.df,
              aes(x = time.chunk,
                  y = y,
                  label = group),
              vjust = "inward",
              hjust = 2) + 
    theme_classic() +
    ylab('4 minute AI scoring window') +
    xlab('Bird calls per recorder') +
    theme(legend.position = 'none') +
    ggtitle(paste0(i,
                   ': AI scored calls per 4 minutes'))
  print(p)
}
dev.off()


### graph results for human and AI dunn test
## add significance to summary graph for each bird
# print all into one pdf
pdf("figures/all_sp_HumanVsAI/Human and AI counts summary per bird sig dunn.pdf",
    onefile = TRUE)
for(i in unique(data.AI.chunk.dunn.results$Species)){
  # get pvalues and set non-significance to 1
  # rename to letters
  tmp =
    data.AI.chunk.dunn.results %>% 
    filter(Species == i) %>% 
    mutate(p.adj.sig = ifelse(p.adj < 0.05,
                              p.adj,
                              1),
           p.adj.sig = ifelse(is.na(p.adj),
                              1,
                              p.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.p = tmp$p.adj.sig
  names(tmp.p) = tmp$comp
  tmp.let = multcompLetters(tmp.p)
  
  # convert to data frame
  tmp.let.df = tmp.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.filter.sec.sum.recorder.4.chunk %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max())
  
  ## human
  tmp.h =
    data.human.chunk.dunn.results %>% 
    filter(Species == i) %>% 
    mutate(p.adj.sig = ifelse(p.adj < 0.05,
                              p.adj,
                              1),
           p.adj.sig = ifelse(is.na(p.adj),
                              1,
                              p.adj)) %>% 
    mutate(comp = str_replace_all(comparison,
                                  "7th.0-4",
                                  "a"),
           comp = str_replace_all(comp,
                                  "8th.0-4",
                                  "b"),
           comp = str_replace_all(comp,
                                  "8th.4-8",
                                  "c"),
           comp = str_replace_all(comp,
                                  "8th.28-32",
                                  "d"),
           comp = str_replace_all(comp,
                                  'vs',
                                  '-')) 
  
  # get letters
  tmp.h.p = tmp.h$p.adj.sig
  names(tmp.h.p) = tmp.h$comp
  tmp.h.let = multcompLetters(tmp.h.p)
  
  # convert to data frame
  tmp.h.let.df = tmp.h.let$Letters %>% 
    as.data.frame() %>% 
    rename(group = '.') %>% 
    rownames_to_column('time.chunk') %>% 
    mutate(time.chunk = case_when(time.chunk == "a" ~ "7th.0-4",
                                  time.chunk == "b" ~ "8th.0-4",
                                  time.chunk == "c" ~ "8th.4-8",
                                  time.chunk == "d" ~ "8th.28-32"))%>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>% 
    mutate(y = data.human.chunk.long.sum %>% 
             filter(spp == i) %>% 
             pull(sum.counts) %>% 
             max())
  
  # combine letter data
  tmp.let.df.all = tmp.h.let.df %>% 
    mutate(data.type = 'Human') %>% 
    rbind(tmp.let.df %>% 
            mutate(data.type = 'AI')) %>% 
    mutate(y.max = max(y))
  
  
  # create graph
  p <-
    data.human.chunk.long.sum %>% 
    mutate(data.type = "Human") %>% 
    full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
    filter(spp == i) %>% 
    mutate(time.chunk = fct_relevel(time.chunk,
                                    c("7th.0-4",
                                      "8th.0-4",
                                      "8th.4-8",
                                      "8th.28-32"))) %>%
    ggplot(aes(x = time.chunk, 
               y = sum.counts,
               fill = data.type)) +
    geom_vline(xintercept = 1.5) + 
    geom_boxplot(outlier.shape = NA,
                 position = position_dodge(width = 0.5)) +
    geom_dotplot(binaxis = "y",
                 stackdir = 'center',
                 position = position_dodge(width = 0.5),
                 color = 'black',
                 binwidth = 1,
                 dotsize = (max(data.human.chunk.long.sum %>% 
                                  mutate(data.type = "Human") %>% 
                                  full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                                  filter(spp == i) %>% 
                                  pull(sum.counts)) - min(data.human.chunk.long.sum %>% 
                                                            mutate(data.type = "Human") %>% 
                                                            full_join(data.filter.sec.sum.recorder.4.chunk) %>% 
                                                            filter(spp == i) %>% 
                                                            pull(sum.counts)))/max_y_range) +
    theme_classic() +
    ylab('4 minute scoring window') +
    xlab('Bird calls per recorder') +
    ggtitle(paste0(i,
                   ': scored calls per 4 minutes')) +
    scale_fill_manual(values = c("red",
                                 "darkgrey"))+
    geom_text(data = tmp.let.df.all,
              aes(x = time.chunk,
                  y = y.max,
                  label = group,
                  color = data.type),
              vjust = "inward",
              hjust = 2,
              position = position_dodge(width = 0.5)) + 
      scale_color_manual(values = c("red",
                                   "darkgrey"))+
    theme_classic() +
    ylab('Countes per 4 minute scoring window') +
    xlab('Bird calls per recorder') +
    # theme(legend.position = 'none') +
    ggtitle(paste0(i,
                   ': AI scored calls per 4 minutes'))
  print(p)
}
dev.off()



#### Trash: Human GAM time chunks ####
# ## compare 0-4 minutes on the 7th and 8th
# # difference in groups
# data_7th.04_8th.04 = data.human.chunk.long.sum %>% 
#   filter(time.chunk != '8th.28-32') %>% 
#   filter(time.chunk != '8th.4-8') %>% 
#   mutate(sum.counts = ifelse(time.chunk == '7th.0-4',
#                              -sum.counts,
#                              sum.counts)) %>% 
#   group_by(recorder,
#            spp) %>% 
#   summarise(sum.counts = sum(sum.counts)) %>% 
#   ungroup()
# # compare groups
# mod_7th.04_8th.04 <- gam(sum.counts ~spp+s(recorder, bs ='re')
#                          ,
#             data = data_7th.04_8th.04, 
#             method = "REML")
# 
# 
# sink("figures/all_sp_HumanVsAI/7th.0-4 vs 8th.0-4.txt")
# print("7th.0-4 vs 8th.0-4")
# summary(mod_7th.04_8th.04)
# sink()
# 
# ## compare 0-4 minutes on the 7th to 4-8 8th
# # difference in groups
# data_7th.04_8th.48 = data.human.chunk.long.sum %>% 
#   filter(time.chunk != '8th.28-32') %>% 
#   filter(time.chunk != '8th.0-4') %>% 
#   mutate(sum.counts = ifelse(time.chunk == '7th.0-4',
#                              -sum.counts,
#                              sum.counts)) %>% 
#   group_by(recorder,
#            spp) %>% 
#   summarise(sum.counts = sum(sum.counts)) %>% 
#   ungroup()
# # compare groups
# mod_7th.04_8th.48 <- gam(sum.counts ~spp+s(recorder, bs ='re')
#                          ,
#                          data = data_7th.04_8th.48, 
#                          method = "REML")
# 
# 
# sink("figures/all_sp_HumanVsAI/7th.0-4 vs 8th.4-8.txt")
# print("7th.0-4 vs 8th.4-8")
# summary(mod_7th.04_8th.48)
# sink()
# 
# 
# ## compare 0-4 minutes on the 7th to 28-32 8th
# # difference in groups
# data_7th.04_8th.2832 = data.human.chunk.long.sum %>% 
#   filter(time.chunk != '8th.4-8') %>% 
#   filter(time.chunk != '8th.0-4') %>% 
#   mutate(sum.counts = ifelse(time.chunk == '7th.0-4',
#                              -sum.counts,
#                              sum.counts)) %>% 
#   group_by(recorder,
#            spp) %>% 
#   summarise(sum.counts = sum(sum.counts)) %>% 
#   ungroup()
# # compare groups
# mod_7th.04_8th.2832 <- gam(sum.counts ~spp+s(recorder, bs ='re')
#                            ,
#                          data = data_7th.04_8th.2832, 
#                          method = "REML")
# 
# 
# sink("figures/all_sp_HumanVsAI/7th.0-4 vs 8th.28-32.txt")
# print("7th.0-4 vs 8th.28-32")
# summary(mod_7th.04_8th.2832)
# sink()
# 
# ### create a summary dataframe
# data.human.chunk.GAM.results = as.data.frame(summary(mod_7th.04_8th.2832)[["p.table"]]) %>% 
#   rownames_to_column('Species') %>% 
#   mutate(comparison = '7th.04_8th.2832') %>% 
#   rbind(as.data.frame(summary(mod_7th.04_8th.48)[["p.table"]]) %>% 
#           rownames_to_column('Species') %>% 
#           mutate(comparison = '7th.04_8th.48')) %>% 
#   rbind(as.data.frame(summary(mod_7th.04_8th.04)[["p.table"]]) %>% 
#           rownames_to_column('Species') %>% 
#           mutate(comparison = '7th.04_8th.04'))
# 
# # ## save results
# # write.csv(data.human.chunk.GAM.results,
# #           'data/data_output/data.human.chunk.GAM.results.csv')
# # load data
# data.human.chunk.GAM.results = read.csv('data/data_output/data.human.chunk.GAM.results.csv') %>% 
#   select(-X)
# 
# ### graph heatmap of results  
# data.human.chunk.GAM.results %>% 
#   filter(Species != "(Intercept)") %>% 
#   full_join(data.human.chunk.GAM.results %>% 
#               filter(Species == "(Intercept)") %>% 
#               select(Estimate,
#                      comparison) %>% 
#               rename(Estimate.int = Estimate)) %>%
#   mutate(Estimate = Estimate + Estimate.int) %>% 
#   separate_wider_delim(cols = Species,
#                        delim = 'spp',
#                        names = c(NA,
#                                  'Species')) %>% 
#   mutate(eclipse.time = case_when(comparison == "7th.04_8th.2832" ~ "28-32 minutes",
#                                   comparison == "7th.04_8th.48" ~ "4-8 minutes",
#                                   comparison == "7th.04_8th.04" ~ "0-4 minutes")) %>% 
#   mutate(eclipse.time = fct_relevel(eclipse.time,
#                                   "0-4 minutes",
#                                   "4-8 minutes",
#                                   "28-32 minutes")) %>% 
#   mutate(p_value_col = ifelse(Pr...t.. < 0.05,
#                               1,
#                               0),
#          Estimate_col = ifelse(p_value_col == 1,
#                                Estimate,
#                                NA)) %>% 
#   ggplot(aes(x = eclipse.time,
#              y = Species,
#              label = prettyNum(Estimate,
#                                digits = 2))) + 
#   theme_classic()+
#   geom_tile(aes(fill = Estimate_col),
#             color = 'black') +
#   geom_text() +
#   xlab('') +
#   ylab('') +
#   # scale_fill_gradientn(colors = c("darkblue", 
#   #                                 "blue",
#   #                                 "white",
#   #                                 "red",
#   #                                 "darkred"),
#   #                      na.value = 'white') +
#   scale_fill_gradient2(low = 'darkblue',
#                        high = 'darkred',
#                        na.value = 'white') +
#   # theme(legend.position = 'none') +
#   ggtitle('Human score GAM results')+
#   theme(axis.ticks=element_blank(),
#         axis.line = element_blank())
# ggsave('figures/all_sp_HumanVsAI/Human counts GAM results.png')




















#### Trash: run zero inflated poisson ####
# 
# ### check number of zeros in data
# data.human.chunk.long.sum %>%
#   mutate(zero = ifelse(sum.counts == 0, 0 ,1)) %>% 
#   select(zero) %>% 
#   table()
# # zero
# # 0   1 
# # 694 346 
# 
# ## compare days
# data.human.chunk.long.sum %>% 
#   ggplot(aes(sum.counts)) +
#   geom_histogram() +
#   facet_grid(time.chunk ~.,
#              scales = 'free_y') +
#   theme_classic() 
# 
# 
# 
# # just compare 8th to 7th
# tmp = data.human.chunk.long.sum %>% 
#   mutate(day = ifelse(grepl('8th',time.chunk),
#                       "8th",
#                       '7th'),
#          day = as.factor(day),
#          time.window = ifelse(grepl('0-4',time.chunk),
#                               "0-4",
#                               'other')) %>% 
#   filter(time.window != 'other') %>% 
#   mutate(time.window = as.factor(time.window)) %>% 
#   select(-c(time.window))
# 
# # take difference
# tmp = tmp %>% 
#   mutate(sum.counts = ifelse(day == '7th',
#                              -sum.counts,
#                              sum.counts)) %>% 
#   group_by(recorder,
#            spp) %>% 
#   summarise(sum.counts = sum(sum.counts)) %>% 
#   ungroup()
# 
# 
# 
# 
# 
# 
# human.time.chunk.zip.model <- glm(sum.counts ~ spp*time.chunk, 
#                                   data = data.human.chunk.long.sum)
# 
# human.time.chunk.zip.model <- glmmTMB(sum.counts ~ spp+time.chunk + (1|recorder), 
#                                       ziformula = ~1,
#                                       family = poisson(),
#                                       data = data.human.chunk.long.sum )
# 
# summary(human.time.chunk.zip.model)
# emmeans::emmeans(human.time.chunk.zip.model,
#                  "time.chunk")
# 
# ### check for overdispersion! 
# # diagnostic plots
# par(mfrow = c(2,3))
# plot(human.time.chunk.zip.model,
#      which = 1:6)
# 
# 
# 
# 
# data.human.chunk.long.sum
# 
# 
# mod1 <- gam(sum.counts ~time.chunk ,
#             data = data.human.chunk.long.sum, 
#             family =poisson(),
#             method = "REML")
# summary(mod1)
# 
# # need to run for large data
# mod1 <- bam(sum.counts ~time.chunk ,
#             data = data.human.chunk.long.sum, 
#             family =poisson(),
#             method = "REML")
# summary(mod1)
# 
# # random effect of recorder
# mod1 <- bam(sum.counts ~time.chunk+spp+ s(recorder, bs ='re') ,
#             data = data.human.chunk.long.sum, 
#             family =poisson(),
#             method = "REML")
# summary(mod1)
# 
# # interaction effect
# mod1 <- bam(sum.counts ~spp*time.chunk+s(recorder, bs ='re'),
#             data = data.human.chunk.long.sum, 
#             # family =poisson(),
#             method = "REML")
# summary(mod1)
# 
# # poisson
# mod1 <- gam(sum.counts ~spp*time.chunk+s(recorder, bs ='re'),
#             data = data.human.chunk.long.sum, 
#             family =poisson(),
#             method = "REML")
# summary(mod1)
# 
# # negative binomial
# mod1 <- gam(sum.counts ~spp*time.chunk+s(recorder, bs ='re'),
#             data = data.human.chunk.long.sum, 
#             family =nb(),
#             method = "REML")
# summary(mod1)
# 
# # ZIP
# mod1 <- gam(list(sum.counts ~time.chunk+spp+s(recorder, bs ='re'),
#                  ~1),
#             data = data.human.chunk.long.sum, 
#             family =ziplss(),
#             method = "REML")
# summary(mod1)
# human.time.chunk.zip.model <- glmmTMB(sum.counts ~ spp+time.chunk + (1|recorder), 
#                                       ziformula = ~1,
#                                       family = poisson(),
#                                       data = data.human.chunk.long.sum)
# 
# # ZIP with interaction
# mod1 <- gam(list(sum.counts ~time.chunk+spp+s(recorder, bs ='re'),
#                  ~1),
#             data = data.human.chunk.long.sum, 
#             family =ziplss(),
#             method = "REML")
# summary(mod1)
# 
# 
# # binomial with 1 and 0
# # failed to converge
# tmp <- glmer(sum.counts.bi ~ time.chunk + (1|recorder),
#              family = binomial,
#              data = data.human.chunk.long.sum %>% 
#                filter(spp == i) %>% 
#                mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                              0,
#                                              1)))
# # binomial with 1 and 0 and no random effect
# tmp <- glm(sum.counts.bi ~ time.chunk,
#            family = binomial,
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                            0,
#                                            1)))
# 
# # negative binomial 
# # bayesian approach binomial with 1 and 0
# library(rstanarm)    
# tmp = stan_glm(data = data.human.chunk.long.sum %>% 
#                  filter(spp == i) %>% 
#                  mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                                0,
#                                                1)),
#                sum.counts.bi ~ time.chunk, 
#                # offset = log_time_offset_if_any,
#                family = neg_binomial_2)
# 
# # poisson
# # bayesian approach
# #Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# library(rstanarm)    
# tmp = stan_glm(data = data.human.chunk.long.sum %>% 
#                  filter(spp == i),
#                sum.counts ~ time.chunk, 
#                # offset = log_time_offset_if_any,
#                family = poisson)
# 
# # negative binomial 
# # bayesian approach binomial with 1 and 0
# # random effect
# #Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# library(rstanarm)    
# tmp = stan_glmer.nb(data = data.human.chunk.long.sum %>% 
#                       filter(spp == i) %>% 
#                       mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                                     0,
#                                                     1)),
#                     sum.counts.bi ~ time.chunk + (1|recorder))
# 
# # negative binomial with 1 and 0
# # boundary singular
# tmp <- glmer.nb(sum.counts.bi ~ time.chunk + (1|recorder),
#                 data = data.human.chunk.long.sum %>% 
#                   filter(spp == i) %>% 
#                   mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                                 0,
#                                                 1)))
# 
# # negative binomial with 1 and 0 no random effect
# # boundary singular
# tmp <- glm.nb(sum.counts.bi ~ time.chunk,
#               data = data.human.chunk.long.sum %>% 
#                 filter(spp == i) %>% 
#                 mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                               0,
#                                               1)))
# 
# # negative binomial
# # failed to converge
# tmp <- glmer.nb(sum.counts ~ time.chunk + (1|recorder),
#                 data = data.human.chunk.long.sum %>% 
#                   filter(spp == i) )
# 
# # binomial
# # failed to converge
# tmp <- glmer(sum.counts.bi ~ time.chunk + (1|recorder),
#              family = binomial,
#              data = data.human.chunk.long.sum %>% 
#                filter(spp == i) %>% 
#                mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                              0,
#                                              1)))
# 
# # binomial
# tmp <- glm(sum.counts.bi ~ time.chunk,
#            family = binomial,
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                            0,
#                                            1)))
# # quasibinomial
# tmp <- glm(sum.counts.bi ~ time.chunk,
#            family = quasibinomial,
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                            0,
#                                            1)))
# 
# 
# # poisson
# # failed to converge
# tmp <- glmer(sum.counts ~ time.chunk + (1|recorder),
#              family = "poisson",
#              data = data.human.chunk.long.sum %>% 
#                filter(spp == i))
# 
# # poisson no random effect
# tmp <- glm(sum.counts ~ time.chunk,
#            family = "poisson",
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i))
# 
# # GAM poisson
# # Fitting terminated with step failure 
# tmp <- gam(sum.counts ~time.chunk+s(recorder, bs ='re'),
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i),
#            family =poisson(),
#            method = "REML")
# 
# # GAM negative binomial
# # Fitting terminated with step failure 
# tmp <- gam(sum.counts ~time.chunk+s(recorder, bs ='re'),
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i),
#            family =nb(),
#            method = "REML")
# 
# # GAM negative binomial with 1 and 0
# # No error?
# tmp <- gam(sum.counts.bi ~time.chunk+s(recorder, bs ='re'),
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i) %>% 
#              mutate(sum.counts.bi = ifelse(sum.counts == 0,
#                                            0,
#                                            1)),
#            family =nb(),
#            method = "REML")
# 
# # ZIP
# # singular matrix in 'backsolve'
# tmp <- gam(list(sum.counts ~time.chunk+s(recorder, bs ='re'),
#                 ~1),
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i),
#            family =ziplss(),
#            method = "REML")
# 
# # ZIP
# # singular matrix in 'backsolve'
# tmp <- gam(list(sum.counts ~time.chunk+s(recorder, bs ='re'),
#                 ~1),
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i),
#            family =ziplss(),
#            method = "REML")
# 
# # ZIP no random effect
# # 
# tmp <- gam(sum.counts ~time.chunk,
#            data = data.human.chunk.long.sum %>% 
#              filter(spp == i),
#            family =ziplss(),
#            method = "REML")
# 
# # # ZIP
# # #   negative log-likelihood is NaN at starting parameter values
# # # problem with matrix
# # tmp <- glmmTMB(sum.counts ~ time.chunk + (1|recorder),
# #                                       ziformula = ~1,
# #                                       family = poisson(),
# #                                       data = data.human.chunk.long.sum %>% 
# #                   filter(spp == i))
# 
# 
# 
# getME(tmp, 
#       "glm.nb.theta")
# 
# summary(tmp)
# 
# 
# contrast(emmeans(tmp,
#                  "time.chunk"),
#          "pairwise", 
#          adjust = "Tukey")
# 
# 
# pairs(emmeans(tmp, ~ time.chunk), 
#       adjust="tukey")
# 
# library(multcomp)
# 
# summary(glht(tmp, 
#              mcp(time.chunk = "Tukey")),
#         test=adjusted("single-step"))
# 
# ### check for overdispersion!
# # diagnostic plots
# par(mfrow = c(2,3))
# plot(tmp,
#      which = 1:6)
