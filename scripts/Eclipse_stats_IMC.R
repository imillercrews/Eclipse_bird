#### Eclipse bird analysis 
### Audio recording data 
## statistical analysis
# R 4.2.1

## setwd
setwd("/geode2/home/u040/imillerc/Quartz/Eclipse")

#### load libraries ####
# load library
library(tidyverse)
# install.packages("CausalImpact")
library(CausalImpact)
library(mgcv)
library(itsadug)
library(dtw)
library(changepoint.np)
library(pvclust)
library(ComplexHeatmap)

#### load data ####
## load minute summary data
# recorder data
data.filter.sec.sum.recorder = read.csv('data/data_output/one.min.sum/data.filter.sec.sum.recorder.csv')

# species sum
data.filter.sec.sum = read.csv('data/data_output/one.min.sum/data.filter.sec.sum.csv')

#### GAM to model counts across days and species ####
### GAM 30 min around eclipse
### compare 6 and 7 to the 8
## smooth across time
m.eclipse.30 <- gam(Sum~Common.Name*Eclipse+s(Min.eclipse),
            data=data.filter.sec.sum %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Min.eclipse >= -15) %>% 
              filter(Min.eclipse <= 18) %>% 
              mutate(Eclipse = ifelse(Date == 8,
                                      1,
                                      0),
                     Eclipse = as.factor(Eclipse)) 
            )

## get summary
m.eclipse.30.summary = summary.gam(m.eclipse.30)
# get pvalues from summary 
m.eclipse.30.summary.table = m.eclipse.30.summary$p.table %>% 
  as.data.frame() %>% 
  rownames_to_column('Predictor')

# save results
write.csv(m.eclipse.30.summary.table,
          file = 'data/data_output/m.eclipse.30.summary.table.csv')

## Plotting the fitted effects of grouping predictors
# interaction
png('figures/all_sp_GAM/Eclipse 30 min plot interaction.png',
    height = 10,
    width = 10,
    units = 'in',
    res = 720)
plot_parametric(m.eclipse.30, 
                pred=list(Common.Name=data.filter.sec.sum %>% 
                            pull(Common.Name) %>% 
                            unique(),
                          Eclipse = 1),
                main = "Interaction Bird by Eclipse: 30 min") 
dev.off()
# birds
png('figures/all_sp_GAM/Eclipse 30 min plot bird.png',
    height = 10,
    width = 10,
    units = 'in',
    res = 720)
plot_parametric(m.eclipse.30, 
                pred=list(Common.Name=data.filter.sec.sum %>% 
                            pull(Common.Name) %>% 
                            unique()),
                main = "Bird: 30 min")
dev.off()


### GAM 60 min around eclipse
### compare 6 and 7 to the 8
## smooth across time
m.eclipse.60 <- gam(Sum~Common.Name*Eclipse+s(Min.eclipse),
                    data=data.filter.sec.sum %>% 
                      filter(Time.day == 'afternoon') %>% 
                      filter(Min.eclipse >= -30) %>% 
                      filter(Min.eclipse <= 33) %>% 
                      mutate(Eclipse = ifelse(Date == 8,
                                              1,
                                              0),
                             Eclipse = as.factor(Eclipse)) 
                    )
## get summary
m.eclipse.60.summary = summary.gam(m.eclipse.60)
# get pvalues from summary 
m.eclipse.60.summary.table = m.eclipse.60.summary$p.table %>% 
  as.data.frame() %>% 
  rownames_to_column('Predictor')

# save results
write.csv(m.eclipse.60.summary.table,
          file = 'data/data_output/m.eclipse.60.summary.table.csv')

## Plotting the fitted effects of grouping predictors
# interaction
png('figures/all_sp_GAM/Eclipse 60 min plot interaction.png',
    height = 10,
    width = 10,
    units = 'in',
    res = 720)
plot_parametric(m.eclipse.60, 
                pred=list(Common.Name=data.filter.sec.sum %>% 
                            pull(Common.Name) %>% 
                            unique(),
                          Eclipse = 1),
                main = "Interaction Bird by Eclipse: 60 min")
dev.off()
# birds
png('figures/all_sp_GAM/Eclipse 60 min plot interaction.png',
    height = 10,
    width = 10,
    units = 'in',
    res = 720)
plot_parametric(m.eclipse.60, 
                pred=list(Common.Name=data.filter.sec.sum %>% 
                            pull(Common.Name) %>% 
                            unique()),
                main = "Bird: 60 min")
  
dev.off()



















#### compare time of day each species with dtw ####
#### 1 minute sum
#### calculate distance score between species across non-eclipse time of day
#### dawn
data.filter.sec.sum.daw =
  data.filter.sec.sum %>% 
  filter(Time.day == 'dawn') 

# create empty dataframe
data.filter.sec.sum.daw.dist = data.frame(Time.day = as.character(),
                                          Common.Name = as.character(),
                                          Dist.value = as.numeric(),
                                          Window.size = as.numeric(),
                                          Data.type = as.character(),
                                          Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.daw = data.frame(Time.day = as.character(),
                          Common.Name = as.character(),
                          Dist.value = as.numeric(),
                          Window.size = as.numeric(),
                          data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.daw$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.daw %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.daw = tmp.dist.daw %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "dawn",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.daw %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'dawn 7_8 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/dawn/dawn 7_8 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.daw$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.daw %>% 
                                    filter(Common.Name==i) %>% 
                                    filter(data.type == 'regular') %>% 
                                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.daw %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': dawn 7_8 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/dawn/changepoints/regular/",
                i,
                " dawn 7_8 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.daw.dist = data.filter.sec.sum.daw.dist %>% 
    rbind(data.frame(
      Time.day = 'dawn',
      Date = "7_8",
      Common.Name = i,
      Dist.value = tmp.dist.daw %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.daw$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.daw %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.daw = tmp.dist.daw %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "dawn",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.daw %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'dawn 7_8 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/dawn/dawn 7_8 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.daw$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.daw %>% 
                                    filter(Common.Name==i) %>% 
                                    filter(data.type == 'scale') %>% 
                                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.daw %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': dawn 7_8 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/dawn/changepoints/scale/",
                i,
                " dawn 7_8 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.daw.dist = data.filter.sec.sum.daw.dist %>% 
    rbind(data.frame(
      Time.day = 'dawn',
      Date = "7_8",
      Common.Name = i,
      Dist.value = tmp.dist.daw %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph dawn results
## compare regular vs scale distance
data.filter.sec.sum.daw.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dawn distance value comparison')
ggsave("figures/all_sp_dist/dawn/dawn 7_8 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.daw.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.daw %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dawn distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/dawn/dawn 7_8 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.daw.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.daw %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dawn distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/dawn/dawn 7_8 distance values scale vs sum calls.png")

#### dusk
data.filter.sec.sum.dus =
  data.filter.sec.sum %>% 
  filter(Time.day == 'dusk') 

## filter to birds present on both days
dusk.missing.names = data.filter.sec.sum.dus %>% 
  group_by(Common.Name,
           Date) %>% 
  summarise(Sum = sum(Sum)) %>% 
  filter(Sum == 0) %>% 
  pull(Common.Name)

# remove low count birds
data.filter.sec.sum.dus = data.filter.sec.sum.dus %>% 
  filter(!(Common.Name %in% c(dusk.missing.names)))


# create empty dataframe
data.filter.sec.sum.dus.dist = data.frame(Time.day = as.character(),
                                          Common.Name = as.character(),
                                          Dist.value = as.numeric(),
                                          Window.size = as.numeric(),
                                          Data.type = as.character(),
                                          Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.dus = data.frame(Time.day = as.character(),
                          Common.Name = as.character(),
                          Dist.value = as.numeric(),
                          Window.size = as.numeric(),
                          data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.dus$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.dus %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.dus = tmp.dist.dus %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "dusk",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.dus %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'dusk 6_7 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/dusk/dusk 6_7 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.dus$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.dus %>% 
                                    filter(Common.Name==i) %>% 
                                    filter(data.type == 'regular') %>% 
                                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.dus %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': dusk 6_7 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/dusk/changepoints/regular/",
                i,
                " dusk 6_7 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.dus.dist = data.filter.sec.sum.dus.dist %>% 
    rbind(data.frame(
      Time.day = 'dusk',
      Date = "6_7",
      Common.Name = i,
      Dist.value = tmp.dist.dus %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.dus$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.dus %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.dus = tmp.dist.dus %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "dusk",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.dus %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'dusk 6_7 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/dusk/dusk 6_7 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.dus$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.dus %>% 
                                    filter(Common.Name==i) %>% 
                                    filter(data.type == 'scale') %>% 
                                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.dus %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': dusk 6_7 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/dusk/changepoints/scale/",
                i,
                " dusk 6_7 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.dus.dist = data.filter.sec.sum.dus.dist %>% 
    rbind(data.frame(
      Time.day = 'dusk',
      Date = "6_7",
      Common.Name = i,
      Dist.value = tmp.dist.dus %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph dusk results
## compare regular vs scale distance
data.filter.sec.sum.dus.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dusk distance value comparison')
ggsave("figures/all_sp_dist/dusk/dusk 6_7 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.dus.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.dus %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dusk distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/dusk/dusk 6_7 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.dus.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.dus %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'dusk distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/dusk/dusk 6_7 distance values scale vs sum calls.png")

#### afternoon 6_7
data.filter.sec.sum.aft.6_7 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 8)%>% 
  filter(Date != 9)
# 
# ## filter to birds present on both days
# aft.6_7.missing.names = data.filter.sec.sum.aft.6_7 %>% 
#   group_by(Common.Name,
#            Date) %>% 
#   summarise(Sum = sum(Sum)) %>% 
#   filter(Sum == 0) %>% 
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.6_7 = data.filter.sec.sum.aft.6_7 %>% 
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.6_7.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.6_7 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_7$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_7 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_7 = tmp.dist.aft.6_7 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_7 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_7 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/6_7/afternoon 6_7 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_7$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_7 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_7 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_7 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_7/changepoints/regular/",
                i,
                " afternoon 6_7 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_7.dist = data.filter.sec.sum.aft.6_7.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_7",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_7 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_7$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_7 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_7 = tmp.dist.aft.6_7 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_7 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_7 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/6_7/afternoon 6_7 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_7$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_7 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_7 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_7 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_7/changepoints/scale/",
                i,
                " afternoon 6_7 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_7.dist = data.filter.sec.sum.aft.6_7.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_7",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_7 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.6_7.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/6_7/afternoon 6_7 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.6_7.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_7 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_7/afternoon 6_7 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.6_7.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_7 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_7/afternoon 6_7 distance values scale vs sum calls.png")




#### afternoon 6_9
data.filter.sec.sum.aft.6_9 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 8)%>% 
  filter(Date != 7)
# 
# ## filter to birds present on both days
# aft.6_9.missing.names = data.filter.sec.sum.aft.6_9 %>% 
#   group_by(Common.Name,
#            Date) %>% 
#   summarise(Sum = sum(Sum)) %>% 
#   filter(Sum == 0) %>% 
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.6_9 = data.filter.sec.sum.aft.6_9 %>% 
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.6_9.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.6_9 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_9 = tmp.dist.aft.6_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_9 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_9 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/6_9/afternoon 6_9 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_9 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_9/changepoints/regular/",
                i,
                " afternoon 6_9 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_9.dist = data.filter.sec.sum.aft.6_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_9 = tmp.dist.aft.6_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_9 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_9 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/6_9/afternoon 6_9 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_9 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_9/changepoints/scale/",
                i,
                " afternoon 6_9 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_9.dist = data.filter.sec.sum.aft.6_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.6_9.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/6_9/afternoon 6_9 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.6_9.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_9/afternoon 6_9 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.6_9.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_9/afternoon 6_9 distance values scale vs sum calls.png")

#### afternoon 7_9
data.filter.sec.sum.aft.7_9 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 8)%>% 
  filter(Date != 6)

# ## filter to birds present on both days
# aft.7_9.missing.names = data.filter.sec.sum.aft.7_9 %>% 
#   group_by(Common.Name,
#            Date) %>% 
#   summarise(Sum = sum(Sum)) %>% 
#   filter(Sum == 0) %>% 
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.7_9 = data.filter.sec.sum.aft.7_9 %>% 
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.7_9.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.7_9 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.7_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.7_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.7_9 = tmp.dist.aft.7_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.7_9 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 7_9 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/7_9/afternoon 7_9 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.7_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.7_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.7_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 7_9 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/7_9/changepoints/regular/",
                i,
                " afternoon 7_9 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.7_9.dist = data.filter.sec.sum.aft.7_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "7_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.7_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.7_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.7_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.7_9 = tmp.dist.aft.7_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.7_9 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 7_9 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/7_9/afternoon 7_9 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.7_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.7_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.7_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 7_9 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/7_9/changepoints/scale/",
                i,
                " afternoon 7_9 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.7_9.dist = data.filter.sec.sum.aft.7_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "7_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.7_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.7_9.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/7_9/afternoon 7_9 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.7_9.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.7_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/7_9/afternoon 7_9 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.7_9.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.7_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/7_9/afternoon 7_9 distance values scale vs sum calls.png")


#### afternoon 6_8
data.filter.sec.sum.aft.6_8 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 7)%>% 
  filter(Date != 9)

# ## filter to birds present on both days
# aft.6_8.missing.names = data.filter.sec.sum.aft.6_8 %>%
#   group_by(Common.Name,
#            Date) %>%
#   summarise(Sum = sum(Sum)) %>%
#   filter(Sum == 0) %>%
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.6_8 = data.filter.sec.sum.aft.6_8 %>%
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.6_8.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.6_8 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_8$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_8 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_8 = tmp.dist.aft.6_8 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_8 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_8 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/6_8/afternoon 6_8 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_8$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_8 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_8 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_8 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_8/changepoints/regular/",
                i,
                " afternoon 6_8 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_8.dist = data.filter.sec.sum.aft.6_8.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_8",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_8 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.6_8$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.6_8 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.6_8 = tmp.dist.aft.6_8 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.6_8 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 6_8 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/6_8/afternoon 6_8 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.6_8$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.6_8 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.6_8 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 6_8 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/6_8/changepoints/scale/",
                i,
                " afternoon 6_8 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.6_8.dist = data.filter.sec.sum.aft.6_8.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "6_8",
      Common.Name = i,
      Dist.value = tmp.dist.aft.6_8 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.6_8.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/6_8/afternoon 6_8 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.6_8.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_8 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_8/afternoon 6_8 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.6_8.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.6_8 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/6_8/afternoon 6_8 distance values scale vs sum calls.png")



#### afternoon 7_8
data.filter.sec.sum.aft.7_8 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 6)%>% 
  filter(Date != 9)

# ## filter to birds present on both days
# aft.7_8.missing.names = data.filter.sec.sum.aft.7_8 %>%
#   group_by(Common.Name,
#            Date) %>%
#   summarise(Sum = sum(Sum)) %>%
#   filter(Sum == 0) %>%
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.7_8 = data.filter.sec.sum.aft.7_8 %>%
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.7_8.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.7_8 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.7_8$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.7_8 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.7_8 = tmp.dist.aft.7_8 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.7_8 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 7_8 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/7_8/afternoon 7_8 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.7_8$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.7_8 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.7_8 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 7_8 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/7_8/changepoints/regular/",
                i,
                " afternoon 7_8 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.7_8.dist = data.filter.sec.sum.aft.7_8.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "7_8",
      Common.Name = i,
      Dist.value = tmp.dist.aft.7_8 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.7_8$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.7_8 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.7_8 = tmp.dist.aft.7_8 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.7_8 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 7_8 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/7_8/afternoon 7_8 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.7_8$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.7_8 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.7_8 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 7_8 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/7_8/changepoints/scale/",
                i,
                " afternoon 7_8 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.7_8.dist = data.filter.sec.sum.aft.7_8.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "7_8",
      Common.Name = i,
      Dist.value = tmp.dist.aft.7_8 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.7_8.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/7_8/afternoon 7_8 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.7_8.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.7_8 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/7_8/afternoon 7_8 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.7_8.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.7_8 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/7_8/afternoon 7_8 distance values scale vs sum calls.png")



#### afternoon 8_9
data.filter.sec.sum.aft.8_9 =
  data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date != 7)%>% 
  filter(Date != 6)

# ## filter to birds present on both days
# aft.8_9.missing.names = data.filter.sec.sum.aft.8_9 %>%
#   group_by(Common.Name,
#            Date) %>%
#   summarise(Sum = sum(Sum)) %>%
#   filter(Sum == 0) %>%
#   pull(Common.Name)
# 
# # remove low count birds
# data.filter.sec.sum.aft.8_9 = data.filter.sec.sum.aft.8_9 %>%
#   filter(!(Common.Name %in% c(aft.missing.names)))

# create empty dataframe
data.filter.sec.sum.aft.8_9.dist = data.frame(Time.day = as.character(),
                                              Common.Name = as.character(),
                                              Dist.value = as.numeric(),
                                              Window.size = as.numeric(),
                                              Data.type = as.character(),
                                              Dates = as.character())
# create empty dataframe to store distance by window size data
tmp.dist.aft.8_9 = data.frame(Time.day = as.character(),
                              Common.Name = as.character(),
                              Dist.value = as.numeric(),
                              Window.size = as.numeric(),
                              data.type = as.character())

### raw calls data
## loop through each species
# calculate all distance values
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.8_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.8_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.8_9 = tmp.dist.aft.8_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="regular")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.8_9 %>% 
  filter(data.type == "regular") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 8_9 distance values per sakoechiba window size')
ggsave("figures/all_sp_dist/afternoon/8_9/afternoon 8_9 distance values per sakoechiba window size.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.8_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.8_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'regular') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.8_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 8_9 distance values per sakoechiba window size'))
  ggsave(paste0("figures/all_sp_dist/afternoon/8_9/changepoints/regular/",
                i,
                " afternoon 8_9 distance values per sakoechiba window size.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.8_9.dist = data.filter.sec.sum.aft.8_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "8_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.8_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "regular"
    ))
}

### scale calls data
## scale data for each day
## loop through each species
# calculate all distance values 
## run for all species and all window sizes up to 1 hour
for (i in unique(data.filter.sec.sum.aft.8_9$Common.Name)) {
  for (j in seq(1:60)) {
    # create tmp matrix for species 
    tmp = 
      data.filter.sec.sum.aft.8_9 %>% 
      filter(Common.Name == i) %>% 
      mutate(ID = paste(Common.Name,
                        Date,
                        sep = "_")) %>% 
      group_by(Common.Name,
               Date) %>% 
      mutate(Sum = Sum/(max(Sum))) %>% 
      pivot_wider(id_cols = ID,
                  names_from = Min.group,
                  values_from = Sum) %>% 
      column_to_rownames('ID') %>% 
      as.matrix()
    
    # calculate distance for each window size
    tmp.dist.aft.8_9 = tmp.dist.aft.8_9 %>% 
      rbind(data.frame(Dist.value = dtwDist(tmp,
                                            window.type = 'sakoechiba',
                                            window.size = j)[1,2],
                       Window.size = j,
                       Common.Name = i,
                       Time.day = "afternoon",
                       data.type ="scale")) %>% 
      distinct()
    
  }
}

# graph values for each window size
tmp.dist.aft.8_9 %>% 
  filter(data.type == "scale") %>% 
  ggplot(aes(y = Dist.value,
             x = Window.size,
             color = as.numeric(as.factor(Common.Name)))) +
  geom_line(aes(group = Common.Name)) +
  theme_classic() +
  guides(color = 'none') +
  labs(title= 'afternoon 8_9 distance values per sakoechiba window size scale')
ggsave("figures/all_sp_dist/afternoon/8_9/afternoon 8_9 distance values per sakoechiba window size scale.png")

## select distance value
# calculate change point value for each species and select distance value
for (i in unique(tmp.dist.aft.8_9$Common.Name)) {
  tmp.cp = cpt.np(tmp.dist.aft.8_9 %>% 
                    filter(Common.Name==i) %>% 
                    filter(data.type == 'scale') %>% 
                    pull(Dist.value))@cpts[1]
  # graph changepoint
  tmp.dist.aft.8_9 %>% 
    filter(Common.Name == i) %>% 
    ggplot(aes(y = Dist.value,
               x = Window.size,
               color = as.numeric(as.factor(Common.Name)))) +
    geom_line(aes(group = Common.Name)) +
    geom_vline(xintercept = tmp.cp) +
    theme_classic() +
    guides(color = 'none') +
    labs(title=paste0(i,
                      ': afternoon 8_9 distance values per sakoechiba window size scale'))
  ggsave(paste0("figures/all_sp_dist/afternoon/8_9/changepoints/scale/",
                i,
                " afternoon 8_9 distance values per sakoechiba window size scale.png"))
  
  # select first changepoint value
  data.filter.sec.sum.aft.8_9.dist = data.filter.sec.sum.aft.8_9.dist %>% 
    rbind(data.frame(
      Time.day = 'afternoon',
      Date = "8_9",
      Common.Name = i,
      Dist.value = tmp.dist.aft.8_9 %>% 
        filter(Common.Name == i) %>% 
        filter(Window.size == tmp.cp) %>% 
        filter(data.type == "scale") %>% 
        pull(Dist.value),
      Window.size = tmp.cp,
      data.type = "scale"
    ))
}

### graph afternoon results
## compare regular vs scale distance
data.filter.sec.sum.aft.8_9.dist %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  ggplot(aes(x = regular,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value comparison')
ggsave("figures/all_sp_dist/afternoon/8_9/afternoon 8_9 distance values regular vs scale.png")

## compare sum number of calls vs regular distance
data.filter.sec.sum.aft.8_9.dist %>% 
  filter(data.type == "regular") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.8_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = regular)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls') +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/8_9/afternoon 8_9 distance values regular vs sum calls.png")

## compare sum number of calls vs scale distance
data.filter.sec.sum.aft.8_9.dist %>% 
  filter(data.type == "scale") %>% 
  pivot_wider(id_cols = 'Common.Name',
              names_from = 'data.type',
              values_from = 'Dist.value') %>%
  left_join(data.filter.sec.sum.aft.8_9 %>% 
              group_by(Common.Name) %>% 
              summarise(Sum.calls = sum(Sum))) %>% 
  ggplot(aes(x = Sum.calls,
             y = scale)) +
  geom_point() +
  theme_classic() +
  labs(title = 'afternoon distance value vs sum calls')  +
  geom_smooth(method = 'lm')
ggsave("figures/all_sp_dist/afternoon/8_9/afternoon 8_9 distance values scale vs sum calls.png")






#### combine all distance matrices and save ####
data.filter.sec.sum.dist = data.filter.sec.sum.daw.dist %>% 
  full_join(data.filter.sec.sum.dus.dist) %>% 
  full_join(data.filter.sec.sum.aft.6_7.dist) %>% 
  full_join(data.filter.sec.sum.aft.6_9.dist) %>% 
  full_join(data.filter.sec.sum.aft.7_9.dist)  %>% 
  full_join(data.filter.sec.sum.aft.6_8.dist)  %>% 
  full_join(data.filter.sec.sum.aft.7_8.dist)  %>% 
  full_join(data.filter.sec.sum.aft.8_9.dist) 

# save data
write.csv(data.filter.sec.sum.dist,
          'data/data_output/data.filter.sec.sum.dist.csv')
# load data
data.filter.sec.sum.dist = read.csv('data/data_output/data.filter.sec.sum.dist.csv') %>% 
  dplyr::select(-c(X))

#### graph combined distance matrices ####
### regular
## for each species compare consistency 
data.filter.sec.sum.dist %>% 
  filter(data.type == 'regular') %>% 
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
  ungroup() %>% 
  mutate(Eclipse = ifelse(
    grepl("8",Date),
    "Eclipse",
    "Other")) %>% 
  mutate(Time.day = ifelse(Eclipse=="Eclipse" & Time.day=="afternoon",
                           "Eclipse.afternoon",
                           Time.day
                           )) %>% 
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
  ggtitle('Species consistency')
ggsave('figures/all_sp_dist/Species consistency across time points.png')

## box plot across times of day
data.filter.sec.sum.dist %>% 
  filter(data.type == 'regular') %>% 
  mutate(Eclipse = ifelse(
    grepl("8",Date),
    "Eclipse",
    "Other")) %>% 
  mutate(Time.day = ifelse(Eclipse=="Eclipse" & Time.day=="afternoon",
                           "Eclipse.afternoon",
                           Time.day
  )) %>% 
  mutate(ID = paste0(Time.day, Date)) %>% 
  ggplot(aes(
    y = Dist.value,
    x = ID,
    fill = Time.day,
    group = ID 
  )) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center",
               binwidth = 10) +
  theme_classic() +
  ylab('Dynamic time warping') +
  xlab('Time of day') +
  ggtitle('Species consistency') +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust=0.5))
ggsave('figures/all_sp_dist/Species consistency across time points boxplot.png')

### scale
## for each species compare consistency 
data.filter.sec.sum.dist %>% 
  filter(data.type == 'scale') %>% 
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
  ungroup() %>% 
  mutate(Eclipse = ifelse(
    grepl("8",Date),
    "Eclipse",
    "Other")) %>% 
  mutate(Time.day = ifelse(Eclipse=="Eclipse" & Time.day=="afternoon",
                           "Eclipse.afternoon",
                           Time.day
  )) %>% 
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
  xlab('Dynamic time warping scale') +
  ylab('Species, order by dawn/dusk mean') +
  ggtitle('Species consistency')
ggsave('figures/all_sp_dist/Species consistency across time points scale.png')

## box plot across times of day
data.filter.sec.sum.dist %>% 
  filter(data.type == 'scale') %>% 
  mutate(Eclipse = ifelse(
    grepl("8",Date),
    "Eclipse",
    "Other")) %>% 
  mutate(Time.day = ifelse(Eclipse=="Eclipse" & Time.day=="afternoon",
                           "Eclipse.afternoon",
                           Time.day
  )) %>% 
  mutate(ID = paste0(Time.day, Date)) %>% 
  ggplot(aes(
    y = Dist.value,
    x = ID,
    fill = Time.day,
    group = ID 
  )) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center",
               binwidth = 0.5) +
  theme_classic() +
  ylab('Dynamic time warping scale') +
  xlab('Time of day') +
  ggtitle('Species consistency') +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust=0.5))
ggsave('figures/all_sp_dist/Species consistency across time points boxplot scale.png')

# paper
data.filter.sec.sum.dist %>% 
  filter(data.type == 'scale') %>% 
  mutate(Eclipse = ifelse(
    grepl("8",Date),
    "Eclipse",
    "Other")) %>% 
  mutate(Time.day = ifelse(Eclipse=="Eclipse" & Time.day=="afternoon",
                           "Eclipse.afternoon",
                           Time.day
  )) %>% 

  filter(Time.day != 'Eclipse.afternoon') %>% 
  group_by(Common.Name,
           Time.day) %>% 
    summarise(Dist.value = mean(Dist.value)) %>% 
  ggplot(aes(
    y = Dist.value,
    x = factor(Time.day,
               levels = c('dawn',
                          'afternoon',
                          'dusk')),
    group = Time.day 
  )) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center",
               binwidth = 0.5) +
  theme_classic(base_size = 12) +
  ylab('Dynamic time warping') +
  xlab('Time of day') +
  ggtitle('Species consistency score') 
ggsave('figures/all_sp_dist/Species consistency across time points boxplot scale paper.pdf',
       width = 6.5,
       height = 5,
       units = 'in')





#### causal inference bayesian ####
### test case with robin on the 8th
# https://cran.r-project.org/web/packages/CausalImpact/vignettes/CausalImpact.html

# there should be no missing values
# the response variable must be in the first column and any covariates in subsequent columns
# There should not be a “year” or index column

#### check 60 to 30 minutes before the eclipse
## use the 6,7,9 as training and compare with 30 min before on 8th 

## create empty dataframe
impact.results.pre = data.frame()

### run for every species across all time windows before eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in c(30)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=-60) %>% 
      filter(Min.eclipse <=-1*j) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-1*j) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-1*j) %>% 
              ungroup())  %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-1*j) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,93), 
                           post.period = c(94,124),
                           model.args = list(prior.level.sd = 0.1))
    
    ## graph
    # create dataframe with time chunks
    tmp.time = tmp %>% 
      rownames_to_column("order") %>% 
      mutate(order = as.numeric(order))
    
    plot(impact) +
      ggtitle(paste0(i,"_pre_eclipse_",j, "minutes_Casual Impact")) +
      geom_point(data = tmp.time,
                 aes(x = order,
                     y = 0,
                     color = as.factor(Date)))
    ggsave(paste0('figures/all_sp_CI/pre_eclipse/',
                  j,
                  "minutes/",
                  i,"_pre_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.pre = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "pre_eclipse",
             p.adj = p.adjust(p,
                              method = 'fdr',
                              52)) %>%
      rbind(impact.results.pre)
  }
}


#### after eclipse
### run for 4 minute windows
## use the 6,7,9, and 30 min before on 8th as training windows

## create empty dataframe
impact.results.after = data.frame()

### run for every species across all time windows after eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=4) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-60+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,4*j), 
                           post.period = c(4*j+1,4*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    ## graph
    # create dataframe with time chunks
    tmp.time = tmp %>% 
      rownames_to_column("order") %>% 
      mutate(order = as.numeric(order))
    
    plot(impact) +
      ggtitle(paste0(i,"_after_eclipse_",j, "minutes_Casual Impact")) +
      geom_point(data = tmp.time,
                 aes(x = order,
                     y = 0,
                     color = as.factor(Date)))
    ggsave(paste0('figures/all_sp_CI/after_eclipse/',
                  j,
                  "minutes/",
                  i,"_after_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.after = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "after_eclipse",
             Training.dates = '6,7,9,8') %>%
      rbind(impact.results.after)
  }
}

### training window of 6,7,9
### run for every species across all time windows after eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=4) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,3*j), 
                           post.period = c(3*j+1,3*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact) +
    #   ggtitle(paste0(i,"_after_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/after_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_after_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.after = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "after_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.results.after)
  }
}

# add adjusted pvalue
impact.results.after = impact.results.after %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()

#### before eclipse
### run for 4 minute windows
## use the 6,7,9, and 30 min before on 8th as training windows

## create empty dataframe
impact.results.before = data.frame()

### run for every species across all time windows before eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-60+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,4*j), 
                           post.period = c(4*j+1,4*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact) +
    #   ggtitle(paste0(i,"_before_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/before_eclipse/',
    #               j,
    #               "minutes/",
    #               i,"_before_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.before = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "before_eclipse",
             Training.dates = '6,7,9,8') %>%
      rbind(impact.results.before)
  }
}


### training window of 6,7,9
### run for every species across all time windows before eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,3*j), 
                           post.period = c(3*j+1,3*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact) +
    #   ggtitle(paste0(i,"_before_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/before_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_before_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.before = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "before_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.results.before)
  }
}

# add adjusted pvalue
impact.results.before = impact.results.before %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                 method = 'fdr',
                 52)) %>% 
  ungroup()

#### during eclipse
### run for 4 minute windows
## use the 6,7,9, and 30 min before on 8th as training windows

## create empty dataframe
impact.results.during = data.frame()

### run for every species across all time windows during eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=2-j/2) %>% 
      filter(Min.eclipse <=1+j/2) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=-60) %>% 
              filter(Min.eclipse <=-60+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,4*j), 
                           post.period = c(4*j+1,4*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact) +
    #   ggtitle(paste0(i,"_during_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/during_eclipse/',
    #               j,
    #               "minutes/",
    #               i,"_during_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.during = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "during_eclipse",
             Training.dates = '6,7,9,8') %>%
      rbind(impact.results.during)
  }
}


### training window of 6,7,9
### run for every species across all time windows during eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subracte one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=2-j/2) %>% 
      filter(Min.eclipse <=1+j/2) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=2-j/2) %>% 
              filter(Min.eclipse <=1+j/2) %>% 
              ungroup())
    
    # run bayesian analysis
    impact <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,3*j), 
                           post.period = c(3*j+1,3*j+j),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact) +
    #   ggtitle(paste0(i,"_during_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/during_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_during_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact)
    # save results to table
    impact.results.during = impact$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "during_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.results.during)
  }
}

# add adjusted pvalue
impact.results.during = impact.results.during %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()

#### combine results and save
impact.results = impact.results.before %>% 
  rbind(impact.results.during) %>% 
  rbind(impact.results.after)

## save results casual impacts results
write.csv(impact.results,
          file = 'data/data_output/impact.results.csv')

#### graph casual impact results ####
#### graph number of significant for each category per bird
impact.results %>% 
  filter(p.adj <0.05) %>% 
  group_by(Common.Name,
           Window.time) %>% 
  summarise(count = n()) %>% 
  full_join(expand.grid(Common.Name = unique(impact.results$Common.Name),
                        Window.time = unique(impact.results$Window.time))) %>% 
  mutate(count = ifelse(is.na(count),
                        0,
                        count)) %>% 
  group_by(Common.Name) %>% 
  mutate(mean = mean(count)) %>% 
  ggplot(aes(x = count,
             y= reorder(Common.Name,
                        mean),
             group = Window.time,
             color = Window.time)) +
    geom_line(aes(group = Common.Name),
              color = 'grey') + 
  geom_point(position = position_dodge(width=0.5)) +
  theme_classic() +
  xlim(0,15) +
  xlab('Number of significant results (p.adj < 0.05)') +
  ylab('Species') +
  labs(title = 'Combined casual impact results')
ggsave('figures/all_sp_CI/Combined casual impact results.png')

#### create forest plots of results from casual impacts
# Use 95% confidence interval and compare each species as rows
# run for 12 minutes after, 12 minutes before, and 4 minutes of totality
# use 6,7,9 afternoon pre-training

### absolute effect
## totality
impact.results %>% 
  filter(Window.time == 'during_eclipse') %>% 
  filter(Window.size == 4) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -AbsEffect),
             group = Common.Name)) +
  geom_point(aes(x = AbsEffect,
                 color = p.adj.color)) +
  geom_linerange(aes(xmin = AbsEffect.lower,
                     xmax = AbsEffect.upper)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('During Totality: 4 min, 6,7,9')
ggsave('figures/all_sp_CI/During totality casual impact results.png')

## after
impact.results %>% 
  filter(Window.time == 'after_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -AbsEffect),
             group = Common.Name)) +
  geom_vline(xintercept = 0,
             color = 'grey') +
  geom_linerange(aes(xmin = AbsEffect.lower,
                     xmax = AbsEffect.upper)) +
  geom_point(aes(x = AbsEffect,
                 color = p.adj.color)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('After Totality: 12 min, 6,7,9')
ggsave('figures/all_sp_CI/After totality casual impact results.png')


## before
impact.results %>% 
  filter(Window.time == 'before_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -AbsEffect),
             group = Common.Name)) +
  geom_vline(xintercept = 0,
             color = 'grey') +
  geom_linerange(aes(xmin = AbsEffect.lower,
                     xmax = AbsEffect.upper)) +
  geom_point(aes(x = AbsEffect,
                 color = p.adj.color)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('Before Totality: 12 min, 6,7,9')
ggsave('figures/all_sp_CI/Before totality casual impact results.png')


### relative effect
## totality
impact.results %>% 
  filter(Window.time == 'during_eclipse') %>% 
  filter(Window.size == 4) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -RelEffect),
             group = Common.Name)) +
  geom_point(aes(x = RelEffect,
                 color = p.adj.color)) +
  geom_linerange(aes(xmin = RelEffect.lower,
                     xmax = RelEffect.upper)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Relative effect') +
  ggtitle('During Totality: 4 min, 6,7,9')
ggsave('figures/all_sp_CI/During totality casual impact results relative.png')

## after
impact.results %>% 
  filter(Window.time == 'after_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -RelEffect),
             group = Common.Name)) +
  geom_vline(xintercept = 0,
             color = 'grey') +
  geom_linerange(aes(xmin = RelEffect.lower,
                     xmax = RelEffect.upper)) +
  geom_point(aes(x = RelEffect,
                 color = p.adj.color)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('After Totality: 12 min, 6,7,9')
ggsave('figures/all_sp_CI/After totality casual impact results relative.png')


## before
impact.results %>% 
  filter(Window.time == 'before_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -RelEffect),
             group = Common.Name)) +
  geom_vline(xintercept = 0,
             color = 'grey') +
  geom_linerange(aes(xmin = RelEffect.lower,
                     xmax = RelEffect.upper)) +
  geom_point(aes(x = RelEffect,
                 color = p.adj.color)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('Before Totality: 12 min, 6,7,9')
ggsave('figures/all_sp_CI/Before totality casual impact results relative.png')













#### standardized causal inference bayesian ####
# https://cran.r-project.org/web/packages/CausalImpact/vignettes/CausalImpact.html

# there should be no missing values
# the response variable must be in the first column and any covariates in subsequent columns
# There should not be a “year” or index column

#### standardize training window so that it is the same for after, before, and during

#### after eclipse
### run for 4 minute windows

## create empty dataframe
impact.stand.results.after = data.frame()

### training window of 6,7,9
### run for every species across all time windows after eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subtract one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=4) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup())
    
    # get length of data
    n.tmp = nrow(tmp)
    
    # run bayesian analysis
    impact.stand <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,n.tmp-j), 
                           post.period = c(n.tmp-j+1,n.tmp),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact.stand) +
    #   ggtitle(paste0(i,"_after_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/after_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_after_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact.stand)
    # save results to table
    impact.stand.results.after = impact.stand$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "after_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.stand.results.after)
  }
}

# add adjusted pvalue
impact.stand.results.after = impact.stand.results.after %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()

#### before eclipse
### run for 4 minute windows

## create empty dataframe
impact.stand.results.before = data.frame()

### training window of 6,7,9
### run for every species across all time windows before eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in seq(4,28,by = 4)) {
    
    ## create tmp data
    # need to subtract one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=-1) %>% 
              ungroup())
  
    # get length of data
    n.tmp = nrow(tmp)
    
    # run bayesian analysis
    impact.stand <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,n.tmp-j), 
                           post.period = c(n.tmp-j+1,n.tmp),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact.stand) +
    #   ggtitle(paste0(i,"_before_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/before_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_before_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact.stand)
    # save results to table
    impact.stand.results.before = impact.stand$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = j,
             Window.time = "before_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.stand.results.before)
  }
}

# add adjusted pvalue
impact.stand.results.before = impact.stand.results.before %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()

#### during eclipse
### run for 4 minute windows

## create empty dataframe
impact.stand.results.during = data.frame()

### training window of 6,7,9
### run for every species across all time windows during eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  for (j in c(12)) {
    
    ## create tmp data
    # need to subtract one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=0) %>% 
              filter(Min.eclipse <=3) %>% 
              ungroup())
    
    # get length of data
    n.tmp = nrow(tmp)
    
    # run bayesian analysis
    impact.stand <- CausalImpact(tmp %>% 
                             select(Sum), 
                           pre.period = c(1,n.tmp-4), 
                           post.period = c(n.tmp-4+1,n.tmp),
                           model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact.stand) +
    #   ggtitle(paste0(i,"_during_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/during_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_during_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact.stand)
    # save results to table
    impact.stand.results.during = impact.stand$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = 4,
             Window.time = "during_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.stand.results.during)
  }
}

# add adjusted pvalue
impact.stand.results.during = impact.stand.results.during %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()


#### after.28 eclipse
### run for 4 minute windows centered on 28-32 minutes
## just do 12 minutes

## create empty dataframe
impact.stand.results.after.28 = data.frame()

### training window of 6,7,9
### run for every species across all time windows after.28 eclipse
for (i in unique(data.filter.sec.sum$Common.Name)) {
  # for (j in seq(4,28,by = 4)) {
  for (j in c(12)) {
    
    ## create tmp data
    # need to subtract one from every window size
    tmp = data.filter.sec.sum %>% 
      filter(Common.Name == i) %>% 
      filter(Time.day == 'afternoon') %>% 
      filter(Date == 6) %>% 
      filter(Min.eclipse >=0-j) %>% 
      filter(Min.eclipse <=4+j-1) %>% 
      ungroup() %>% 
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 7) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 9) %>% 
              filter(Min.eclipse >=0-j) %>% 
              filter(Min.eclipse <=4+j-1) %>% 
              ungroup()) %>%
      rbind(data.filter.sec.sum %>% 
              filter(Common.Name == i) %>% 
              filter(Time.day == 'afternoon') %>% 
              filter(Date == 8) %>% 
              filter(Min.eclipse >=24) %>% 
              filter(Min.eclipse <=35) %>% 
              ungroup())
    
    # get length of data
    n.tmp = nrow(tmp)
    
    # run bayesian analysis
    impact.stand <- CausalImpact(tmp %>% 
                                   select(Sum), 
                                 pre.period = c(1,n.tmp-j), 
                                 post.period = c(n.tmp-j+1,n.tmp),
                                 model.args = list(prior.level.sd = 0.1))
    
    # ## graph
    # # create dataframe with time chunks
    # tmp.time = tmp %>% 
    #   rownames_to_column("order") %>% 
    #   mutate(order = as.numeric(order))
    # 
    # plot(impact.stand) +
    #   ggtitle(paste0(i,"_after.28_eclipse_",j, "minutes_Casual Impact")) +
    #   geom_point(data = tmp.time,
    #              aes(x = order,
    #                  y = 0,
    #                  color = as.factor(Date)))
    # ggsave(paste0('figures/all_sp_CI/after.28_eclipse_679/',
    #               j,
    #               "minutes/",
    #               i,"_after.28_eclipse_",j, "minutes_Casual Impact.png"))
    
    # summary(impact.stand)
    # save results to table
    impact.stand.results.after.28 = impact.stand$summary %>%
      as.data.frame() %>%
      rownames_to_column('Type.results') %>% 
      filter(Type.results != 'Average') %>% 
      mutate(Common.Name = i,
             Window.size = 12,
             Window.time = "after.28_eclipse",
             Training.dates = '6,7,9') %>%
      rbind(impact.stand.results.after.28)
  }
  
}

# add adjusted pvalue
impact.stand.results.after.28 = impact.stand.results.after.28 %>% 
  group_by(Training.dates,
           Window.size) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          52)) %>% 
  ungroup()

#### combine results and save
impact.stand.results = impact.stand.results.before %>% 
  rbind(impact.stand.results.during) %>% 
  rbind(impact.stand.results.after) %>% 
  rbind(impact.stand.results.after.28)

## save results casual impact.stands results
write.csv(impact.stand.results,
          file = 'data/data_output/impact.stand.results.csv')
# # load data
# impact.stand.results = read.csv('data/data_output/impact.stand.results.csv') %>%
#   select(-X)

#### graph standardized casual impact.stand results ####
# load data
impact.stand.results = read.csv('data/data_output/impact.stand.results.csv') %>% 
  select(-X)


impact.stand.results = impact.stand.results %>% 
  group_by(Training.dates,
           Window.size,
           Window.time,
           Common.Name) %>% 
  mutate(p.adj = p.adjust(p,
                          method = 'fdr',
                          4)) %>% 
  ungroup()


#### graph number of significant for each category per bird
impact.stand.results %>% 
  filter(p.adj <0.05) %>% 
  group_by(Common.Name,
           Window.time) %>% 
  summarise(count = n()) %>% 
  full_join(expand.grid(Common.Name = unique(impact.stand.results$Common.Name),
                        Window.time = unique(impact.stand.results$Window.time))) %>% 
  mutate(count = ifelse(is.na(count),
                        0,
                        count)) %>% 
  group_by(Common.Name) %>% 
  mutate(mean = mean(count)) %>% 
  filter(Window.time != 'after.28_eclipse') %>% 
  ggplot(aes(x = count,
             y= reorder(Common.Name,
                        mean),
             group = Window.time,
             color = Window.time)) +
  geom_line(aes(group = Common.Name),
            color = 'grey') + 
  geom_point(position = position_dodge(width=0.5)) +
  theme_classic() +
  xlim(0,15) +
  xlab('Number of significant results (p.adj < 0.05)') +
  ylab('Species') +
  labs(title = 'Combined casual impact.stand results')
ggsave('figures/all_sp_CI/Combined casual impact.stand results.png')

#### create forest plots of results from casual impact.stands
# Use 95% confidence interval and compare each species as rows
# run for 12 minutes after, 12 minutes before, and 4 minutes of totality
# use 6,7,9 afternoon pre-training

### absolute effect
## totality
impact.stand.results %>% 
  filter(Window.time == 'during_eclipse') %>% 
  filter(Window.size == 4) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  ggplot(aes(y = reorder(Common.Name,
                         -AbsEffect),
             group = Common.Name)) +
  geom_point(aes(x = AbsEffect,
                 color = p.adj.color)) +
  geom_linerange(aes(xmin = AbsEffect.lower,
                     xmax = AbsEffect.upper)) +
  theme_classic() +
  scale_color_manual(values = c('black',
                                'red')) +
  ylab('Species') +
  xlab('Effect') +
  ggtitle('During Totality: 4 min; 6,7,9; stand')
ggsave('figures/all_sp_CI/During totality casual impact.stand results.png')


### graph absolute cumulative and average effects
## loop through each window size
# facet window.time
# for (j in unique(impact.stand.results$Window.size)) {
  for (j in c(12)) {
  
  # create temporary dataframe
  tmp = impact.stand.results %>% 
    filter(Window.time != 'during_eclipse') %>% 
    filter(Window.time != 'after.28_eclipse') %>% 
    filter(Window.size == j) %>% 
    filter(Training.dates == "6,7,9") %>% 
    mutate(p.adj.color = ifelse(p.adj<0.05,
                                'sig',
                                'not_sig')) 
  
  # add data for during totality
  tmp = impact.stand.results %>% 
    filter(Window.time == 'during_eclipse') %>% 
    filter(Window.size == 4) %>% 
    filter(Training.dates == "6,7,9") %>% 
    mutate(p.adj.color = ifelse(p.adj<0.05,
                                'sig',
                                'not_sig')) %>% 
      rbind(tmp)
  
  # add data for after 28 
  tmp = impact.stand.results %>% 
    filter(Window.time == 'after.28_eclipse') %>% 
    filter(Window.size == 12) %>% 
    filter(Training.dates == "6,7,9") %>% 
    mutate(p.adj.color = ifelse(p.adj<0.05,
                                'sig',
                                'not_sig')) %>% 
    rbind(tmp)
  
  # create groups
  sig.group.species = tmp %>% 
    select(c(Window.time,
             Common.Name,
             p.adj.color)) %>% 
    pivot_wider(id_cols = Common.Name,
                names_from = Window.time,
                values_from = p.adj.color) %>% 
    mutate(during_eclipse = ifelse(during_eclipse == 'sig',
                                   'during',
                                   '')) %>% 
    mutate(after_eclipse = ifelse(after_eclipse == 'sig',
                                   'later',
                                   '')) %>% 
    mutate(before_eclipse = ifelse(before_eclipse == 'sig',
                                   'before',
                                   '')) %>% 
    mutate(after.28_eclipse = ifelse(after.28_eclipse == 'sig',
                                   'later.28',
                                   '')) %>% 
    mutate(species.group = paste0(before_eclipse,
                                 during_eclipse,
                                 after_eclipse,
                                 after.28_eclipse)) 
  

  
  # add species group to data
  tmp = tmp %>% 
    full_join(sig.group.species) %>% 
    arrange(species.group)
  
  # rename after to later
  tmp = tmp %>% 
    mutate(Window.time.name = case_when(Window.time == 'after.28_eclipse' ~ 'later.28',
                                        Window.time == 'after_eclipse' ~ 'later',
                                        Window.time == 'before_eclipse' ~ 'before',
                                        Window.time == 'during_eclipse' ~ 'during'))
  
  ## create groups by when maximum effect
  # calculate effect size per minute
  tmp = tmp %>%  
    mutate(avg.effect = AbsEffect/Window.size)
  
  # order by group
    sig.group.species.max = tmp %>%  
      filter(species.group!='') %>% 
      group_by(Common.Name) %>% 
    mutate(max.abs.effect.rel.pred.time = max(abs(avg.effect)),
           species.group.fact.rel.name =ifelse(max.abs.effect.rel.pred.time == abs(avg.effect),
                                          Window.time.name,
                                          NA)) %>% 
    na.omit() %>% 
      ungroup() %>% 
      arrange(species.group.fact.rel.name,
              desc(avg.effect)) %>% 
      rownames_to_column('species.group.fact.rel') %>% 
    select(Common.Name,
           species.group.fact.rel,
           species.group.fact.rel.name) %>% 
      mutate(species.group.fact.rel = as.integer(species.group.fact.rel))

    
    # add to species max group to data
    tmp = tmp %>% 
      full_join(sig.group.species.max %>% 
                  select(-c(species.group.fact.rel.name)))
  
  # graph cumulative
  tmp %>% 
    ggplot(aes(y = reorder(Common.Name,
                           -AbsEffect),
               group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = AbsEffect,
                   color = p.adj.color)) +
    geom_linerange(aes(xmin = AbsEffect.lower,
                       xmax = AbsEffect.upper)) +
    theme_classic() +
    scale_color_manual(values = c('black',
                                  'red')) +
    ylab('Species') +
    xlab('Effect') +
    ggtitle(paste0('Casual impact summary: ',
                   j,
                   ' min; 6,7,9; stand; all')) +
    facet_grid(~Window.time.name)
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results all.png'))
  
  # graph cumulative
  # just significant samples
  tmp %>%  
    filter(species.group!='') %>% 
    ggplot(aes(y = reorder(Common.Name,
                           -AbsEffect),
               group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = AbsEffect,
                   color = p.adj.color)) +
    geom_linerange(aes(xmin = AbsEffect.lower,
                       xmax = AbsEffect.upper)) +
    theme_classic() +
    scale_color_manual(values = c('black',
                                  'red')) +
    ylab('Species') +
    xlab('Effect') +
    ggtitle(paste0('Casual impact summary: ',
                   j,
                   ' min; 6,7,9; stand')) +
    facet_grid(species.group~Window.time.name,
               scales = 'free_y')
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results.png'))
  
  # graph average
  tmp %>% 
    ggplot(aes(y = reorder(Common.Name,
                           -AbsEffect),
               group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = AbsEffect/Window.size,
                   color = p.adj.color)) +
    geom_linerange(aes(xmin = AbsEffect.lower/Window.size,
                       xmax = AbsEffect.upper/Window.size)) +
    theme_classic() +
    scale_color_manual(values = c('black',
                                  'red')) +
    ylab('Species') +
    xlab('Average effect') +
    ggtitle(paste0('Casual impact average summary: ',
                   j,
                   ' min; 6,7,9; stand; all')) +
    facet_grid(~Window.time.name)
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results all average.png'))
  
  # graph average
  # just significant samples
  tmp %>%  
    filter(species.group!='') %>% 
    ggplot(aes(y = reorder(Common.Name,
                           -AbsEffect),
               group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = AbsEffect/Window.size,
                   color = p.adj.color)) +
    geom_linerange(aes(xmin = AbsEffect.lower/Window.size,
                       xmax = AbsEffect.upper/Window.size)) +
    theme_classic() +
    scale_color_manual(values = c('black',
                                  'red')) +
    ylab('Species') +
    xlab('Average effect') +
    ggtitle(paste0('Casual impact average summary: ',
                   j,
                   ' min; 6,7,9; stand')) +
    facet_grid(species.group~Window.time.name,
               scales = 'free_y')
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results average.png'))
  
  # graph paper
  # create label colors
  sig.group.species.max.color = sig.group.species.max %>% 
    arrange(-species.group.fact.rel) %>% 
    mutate(color = case_when(species.group.fact.rel.name == "before" ~ '#B21912',
                             species.group.fact.rel.name == "during" ~ '#404040',
                             species.group.fact.rel.name == "later" ~ '#EE8012',
                             species.group.fact.rel.name == "later.28" ~ '#FFC000',
                             TRUE ~ 'grey')) %>% 
    pull(color)
  
  # just significant samples
  # average
  # remove facets
  # recolor
  tmp %>%  
    filter(species.group!='') %>%
    mutate(p.adj.color.name = case_when(p.adj.color == "sig" & Window.time == "before_eclipse" ~ "Before",
                                        p.adj.color == "sig" & Window.time == "during_eclipse" ~ "Totality",
                                        p.adj.color == "sig" & Window.time == "after_eclipse" ~ "After",
                                        p.adj.color == "sig" & Window.time == "after.28_eclipse" ~ "Later",
                                        TRUE ~ p.adj.color),
           Window.time.name = case_when(Window.time == "before_eclipse" ~ "Before",
                                        Window.time == "during_eclipse" ~ "Totality",
                                        Window.time == "after_eclipse" ~ "After",
                                        Window.time == "after.28_eclipse" ~ "Later",
                                        TRUE ~ p.adj.color),
           species.group.fact = as.numeric(as.factor(species.group))) %>%
    mutate(Common.Name = case_when(Common.Name == 'Northern Rough-winged Swallow' ~ "No. Rough-winged Swallow",
                                   TRUE ~ Common.Name)) %>% 
    ggplot(aes(
      y = reorder(Common.Name,
                           -species.group.fact.rel),
               group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = avg.effect,
                   fill = p.adj.color.name),
               size = 3,
               shape = 21,
               color = 'black') +
    geom_linerange(aes(xmin = AbsEffect.lower/Window.size,
                       xmax = AbsEffect.upper/Window.size),
                   linewidth = 1) +
    theme_classic(base_size = 12) +
    scale_fill_manual(values = c("Before" = '#B21912',
                                  "Totality" = '#404040',
                                  "After" = '#EE8012',
                                  "Later" = '#FFC000',
                                  "not_sig" = 'white')) + 
    ylab('') +
    xlab('Average effect') +
    facet_grid(.~factor(Window.time.name,
                        levels = c("Before",
                                   "Totality",
                                   "After" ,
                                   "Later")),
               scales = 'free_y') +
    theme(legend.position = "none")+ 
    theme(axis.text.y = element_text(colour = sig.group.species.max.color),
          axis.text.x = element_text(size = rel(0.75)))
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results average paper.pdf'),
         height = 5,
         width = 6.5,
         units = 'in')
  
  # all birds 
  
  # create label colors
  sig.group.species.max.all.color = sig.group.species.max %>% 
    arrange(-species.group.fact.rel) %>% 
    rbind(tmp %>% 
            filter(species.group=='') %>%
            select(Common.Name) %>% 
            distinct() %>% 
            mutate(species.group.fact.rel.name = 'none',
                   species.group.fact.rel = 100)) %>% 
    mutate(color = case_when(species.group.fact.rel.name == "before" ~ '#B21912',
                             species.group.fact.rel.name == "during" ~ '#404040',
                             species.group.fact.rel.name == "later" ~ '#EE8012',
                             species.group.fact.rel.name == "later.28" ~ '#FFC000',
                             TRUE ~ 'grey50')) %>% 
    arrange(desc(Common.Name)) %>% 
    pull(color)
  
  # alphabetical
  tmp %>%  
    mutate(p.adj.color.name = case_when(p.adj.color == "sig" & Window.time == "before_eclipse" ~ "Before",
                                        p.adj.color == "sig" & Window.time == "during_eclipse" ~ "Totality",
                                        p.adj.color == "sig" & Window.time == "after_eclipse" ~ "After",
                                        p.adj.color == "sig" & Window.time == "after.28_eclipse" ~ "Later",
                                        TRUE ~ p.adj.color),
           Window.time.name = case_when(Window.time == "before_eclipse" ~ "Before",
                                        Window.time == "during_eclipse" ~ "Totality",
                                        Window.time == "after_eclipse" ~ "After",
                                        Window.time == "after.28_eclipse" ~ "Later",
                                        TRUE ~ p.adj.color),
           species.group.fact = as.numeric(as.factor(species.group))) %>%
    mutate(Common.Name = case_when(Common.Name == 'Northern Rough-winged Swallow' ~ "No. Rough-winged Swallow",
                                   TRUE ~ Common.Name)) %>% 
    ggplot(aes(
      y = factor(Common.Name,
                 levels = rev(levels(factor(Common.Name)))),
      group = Common.Name)) +
    geom_vline(xintercept = 0,
               color = 'black') +
    geom_point(aes(x = avg.effect,
                   fill = p.adj.color.name),
               size = 3,
               shape = 21,
               color = 'black') +
    geom_linerange(aes(xmin = AbsEffect.lower/Window.size,
                       xmax = AbsEffect.upper/Window.size),
                   linewidth = 1) +
    theme_classic(base_size = 12) +
    scale_fill_manual(values = c("Before" = '#B21912',
                                 "Totality" = '#404040',
                                 "After" = '#EE8012',
                                 "Later" = '#FFC000',
                                 "not_sig" = 'white')) + 
    ylab('') +
    xlab('Average effect') +
    facet_grid(.~factor(Window.time.name,
                        levels = c("Before",
                                   "Totality",
                                   "After" ,
                                   "Later")),
               scales = 'free_y') +
    theme(legend.position = "none")+ 
    theme(axis.text.y = element_text(colour = sig.group.species.max.all.color),
          axis.text.x = element_text(size = rel(0.75)))
  ggsave(paste0('figures/all_sp_CI/stand/',
                j,
                '/window.size.',
                j,
                ' casual impact.stand results average all paper.pdf'),
         height = 10,
         width = 6.5,
         units = 'in')
  }

### graph summary of all time points
## compare species significance across time windows
impact.stand.results %>% 
  filter(p.adj < 0.05) %>% 
  mutate(Sig = 1) %>% 
  ggplot(aes(x = Window.size,
             y = Common.Name,
             fill = Sig)) +
  geom_tile(width = 3) +
  theme_classic() +
  facet_grid(~Window.time) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 28, by = 4)) +
  ggtitle('Significant species per window size')
ggsave('figures/all_sp_CI/Species summary across time windows casual impact.stand results.png')

## compare summary of significance across time windows
impact.stand.results %>% 
  filter(p.adj < 0.05) %>% 
  mutate(Sig = 1) %>% 
  group_by(Window.time,
           Window.size) %>% 
  summarise(Sig.total = sum (Sig)) %>% 
  ggplot(aes(x = Window.size,
             y = Window.time,
             label = Sig.total)) +
  geom_label() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 28, by = 4)) +
  ggtitle('Number of significant species per window size')
ggsave('figures/all_sp_CI/Summary across time windows casual impact.stand results.png')

### graph data of calls for different time bins
# 12 minute time bins
# make temporary data frame
tmp.data = data.filter.sec.sum %>% 
  filter(Time.day == 'afternoon') %>% 
  filter(Date == 6) %>% 
  filter(Min.eclipse >=0-12) %>% 
  filter(Min.eclipse <=4+12-1) %>% 
  ungroup() %>% 
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 7) %>% 
          filter(Min.eclipse >=0-12) %>% 
          filter(Min.eclipse <=4+12-1) %>% 
          ungroup()) %>%
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 9) %>% 
          filter(Min.eclipse >=0-12) %>% 
          filter(Min.eclipse <=4+12-1) %>% 
          ungroup()) %>%
  mutate(Data.type = "train",
         group = paste0(Data.type,
                        Date)) %>% 
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 8) %>% 
          filter(Min.eclipse >=0-12) %>% 
          filter(Min.eclipse <=-1) %>% 
          ungroup()%>%
          mutate(Data.type = "test",
                 group = "before")) %>% 
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 8) %>% 
          filter(Min.eclipse >=4) %>% 
          filter(Min.eclipse <=4+12-1) %>% 
          ungroup()%>%
          mutate(Data.type = "test",
                 group = "later")) %>% 
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 8) %>% 
          filter(Min.eclipse >=0) %>% 
          filter(Min.eclipse <=3) %>% 
          ungroup()%>%
          mutate(Data.type = "test",
                 group = "during")) %>% 
  rbind(data.filter.sec.sum %>% 
          filter(Time.day == 'afternoon') %>% 
          filter(Date == 8) %>% 
          filter(Min.eclipse >=24) %>% 
          filter(Min.eclipse <=35) %>% 
          ungroup()%>%
          mutate(Data.type = "test",
                 group = "later.28")) 

# create one pdf with all figures
pdf("figures/all_sp_CI/stand/12/Casual impact.stand data.pdf",
    onefile = TRUE)
for(i in unique(data.filter.sec.sum$Common.Name)){
p = tmp.data %>% 
  filter(Common.Name == i) %>%
  arrange(Data.type) %>% 
  ggplot(aes(
    x = Min.eclipse,
    y = Sum,
    group = group,
  )) +
  geom_rect(aes(xmin = -0.5,
                xmax = 3.5,
                ymin = 0,
                ymax = Inf,
                fill = Data.type)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  facet_grid(Common.Name~factor(Data.type, levels = c('train',
                                                      'test')),
             scales = "free") +
  theme(legend.position = "none",
        panel.spacing = unit(0, "lines")) +
  scale_fill_manual(values = c("grey",
                               "white"))
print(p)
}
dev.off()

## just significant birds
# get temporary impact results 
tmp.CI.sig = impact.stand.results %>% 
  filter(Window.time != 'during_eclipse') %>% 
  filter(Window.time != 'after.28_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) 

# add data for during totality
tmp.CI.sig = impact.stand.results %>% 
  filter(Window.time == 'during_eclipse') %>% 
  filter(Window.size == 4) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  rbind(tmp.CI.sig)

# add data for after 28 
tmp.CI.sig = impact.stand.results %>% 
  filter(Window.time == 'after.28_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(p.adj.color = ifelse(p.adj<0.05,
                              'sig',
                              'not_sig')) %>% 
  rbind(tmp.CI.sig)

# create groups
sig.group.species = tmp.CI.sig %>%
  select(c(Window.time,
           Common.Name,
           p.adj.color)) %>%
  pivot_wider(id_cols = Common.Name,
              names_from = Window.time,
              values_from = p.adj.color) %>%
  mutate(during_eclipse = ifelse(during_eclipse == 'sig',
                                 'during',
                                 '')) %>%
  mutate(after_eclipse = ifelse(after_eclipse == 'sig',
                                'later',
                                '')) %>%
  mutate(before_eclipse = ifelse(before_eclipse == 'sig',
                                 'before',
                                 '')) %>%
  mutate(after.28_eclipse = ifelse(after.28_eclipse == 'sig',
                                   'later.28',
                                   '')) %>%
  mutate(species.group = paste0(before_eclipse,
                                during_eclipse,
                                after_eclipse,
                                after.28_eclipse))

# add species group to data
tmp.CI.sig = tmp.CI.sig %>%
  full_join(sig.group.species) %>%
  arrange(species.group)

# rename after to later
tmp.CI.sig = tmp.CI.sig %>%
  mutate(Window.time.name = case_when(Window.time == 'after.28_eclipse' ~ 'later.28',
                                      Window.time == 'after_eclipse' ~ 'later',
                                      Window.time == 'before_eclipse' ~ 'before',
                                      Window.time == 'during_eclipse' ~ 'during'))


# create one pdf with all figures
pdf("figures/all_sp_CI/stand/12/Casual impact.stand data sig.pdf",
    onefile = TRUE)
for(i in unique(data.filter.sec.sum$Common.Name)){
  # make data frame of rectangles to highlight significant regions
  tmp = tmp.data %>% 
    filter(Common.Name == i) %>%
    filter(Data.type == "test") %>% 
    group_by(group,
             Common.Name) %>%
    summarise(min.x = min(Min.eclipse)-0.5,
              max.x = max(Min.eclipse)+0.5)
  # filter down to significant
  tmp = tmp.CI.sig %>% 
    filter(Common.Name == i) %>% 
    filter(p.adj.color == "sig") %>% 
    dplyr::select(Common.Name,
                  Window.time.name) %>% 
    dplyr::rename(group= Window.time.name) %>% 
    mutate(keep = 1) %>% 
    full_join(tmp) %>% 
    filter(keep == 1)
  
  # make graph
  p = tmp.data %>% 
    filter(Common.Name == i) %>%
    full_join(tmp) %>% 
    ggplot(aes(
      x = Min.eclipse,
      y = Sum,
      group = group,
    )) +
    geom_rect(aes(xmin = min.x,
                  xmax = max.x,
                  ymin = 0,
                  ymax = Inf,
                  fill = group)) +
    geom_line() +
    geom_point() +
    theme_classic() +
    theme(legend.position = "none",
          panel.spacing = unit(0, "lines")) +
    scale_fill_manual(values = c("before" = '#B21912',
                                 "during" = '#404040',
                                 "later" = '#EE8012',
                                 "later.28" = '#FFC000',
                                 "not_sig" = 'grey'))+
    facet_grid(Common.Name~factor(Data.type,
                                  levels = c('train',
                                             "test")),
               scales = "free_x")
  print(p)
  
}
dev.off()

## filter to just significant birds
# get list of significant birds
CI.sig.birds = tmp.CI.sig %>% 
  filter(p.adj.color == 'sig') %>% 
  pull(Common.Name) %>% 
  unique()
  
# create one pdf with all figures
pdf("figures/all_sp_CI/stand/12/Casual impact.stand data sig filter.pdf",
    onefile = TRUE)
for(i in CI.sig.birds){
  # make data frame of rectangles to highlight significant regions
  tmp = tmp.data %>% 
    filter(Common.Name == i) %>%
    filter(Data.type == "test") %>% 
    group_by(group,
             Common.Name) %>%
    summarise(min.x = min(Min.eclipse)-0.5,
              max.x = max(Min.eclipse)+0.5)
  # filter down to significant
  tmp = tmp.CI.sig %>% 
    filter(Common.Name == i) %>% 
    filter(p.adj.color == "sig") %>% 
    dplyr::select(Common.Name,
                  Window.time.name) %>% 
    dplyr::rename(group= Window.time.name) %>% 
    mutate(keep = 1) %>% 
    full_join(tmp) %>% 
    filter(keep == 1)
  
  # make graph
  p = tmp.data %>% 
    filter(Common.Name == i) %>%
    full_join(tmp) %>% 
    ggplot(aes(
      x = Min.eclipse,
      y = Sum,
      group = group,
    )) +
    geom_rect(aes(xmin = min.x,
                  xmax = max.x,
                  ymin = 0,
                  ymax = Inf,
                  fill = group)) +
    geom_line() +
    geom_point() +
    theme_classic() +
    theme(legend.position = "none",
          panel.spacing = unit(0, "lines")) +
    scale_fill_manual(values = c("before" = '#B21912',
                                 "during" = '#404040',
                                 "later" = '#EE8012',
                                 "later.28" = '#FFC000',
                                 "not_sig" = 'grey'))+
    facet_grid(Common.Name~factor(Data.type,
                                  levels = c('train',
                                             "test")),
               scales = "free_x")
  print(p)
  
}
dev.off()



#### bin dawn/dusk/afternoon ####
### create average calls across days
# create groups
# sum across time, average across days, scale per minute, scale per max value
data.filter.sec.sum.avg = data.filter.sec.sum %>% 
  select(-X) %>% 
  mutate(time.bin.group = case_when(Time.day == 'dawn' & Min.group < 24750 ~ 'pre.civil.twilight',
                                Time.day == 'dawn' & Min.group >= 24750 & Min.group <= 26460 ~ 'pre.sunrise',
                                Time.day == 'dawn' & Min.group > 26460 ~ 'post.sunrise',
                                Time.day == 'afternoon' & Date != 8 ~ 'afternoon',
                                Time.day == 'afternoon' & Date == 8 ~ 'eclipse',
                                Time.day == 'dusk' & Min.group < 72960 ~ 'pre.sunset',
                                Time.day == 'dusk' & Min.group >= 72960 & Min.group <= 74580 ~ 'pre.civil.twilight.end',
                                Time.day == 'dusk' & Min.group > 74580 ~ 'night'),
         time.bin.group.num = case_when(Time.day == 'dawn' & Min.group < 24750 ~ 1,
                                Time.day == 'dawn' & Min.group >= 24750 & Min.group <= 26460 ~ 2,
                                Time.day == 'dawn' & Min.group > 26460 ~ 3,
                                Time.day == 'afternoon' & Date != 8 ~ 4,
                                Time.day == 'afternoon' & Date == 8 ~ 4,
                                Time.day == 'dusk' & Min.group < 72960 ~ 5,
                                Time.day == 'dusk' & Min.group >= 72960 & Min.group <= 74580 ~ 6,
                                Time.day == 'dusk' & Min.group > 74580 ~ 7)) %>% 
  group_by(Common.Name,
           Time.day,
           time.bin.group,
           time.bin.group.num) %>% 
  summarise(bin.count.sum = sum(Sum)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg = case_when(time.bin.group == "pre.civil.twilight" ~ bin.count.sum/2,
                                       time.bin.group == "pre.sunrise" ~ bin.count.sum/2,
                                       time.bin.group == "post.sunrise" ~ bin.count.sum/2,
                                       time.bin.group == "afternoon" ~ bin.count.sum/3,
                                       time.bin.group == "eclipse" ~ bin.count.sum,
                                       time.bin.group == "pre.sunset" ~ bin.count.sum/2,
                                       time.bin.group == "pre.civil.twilight.end" ~ bin.count.sum/2,
                                       time.bin.group == "night" ~ bin.count.sum/2)) %>% 
  mutate(bin.count.sum.avg.per.min = case_when(time.bin.group == "pre.civil.twilight" ~ bin.count.sum.avg/47.5,
                                       time.bin.group == "pre.sunrise" ~ bin.count.sum.avg/28.5,
                                       time.bin.group == "post.sunrise" ~ bin.count.sum.avg/43,
                                       time.bin.group == "afternoon" ~ bin.count.sum.avg/123,
                                       time.bin.group == "eclipse" ~ bin.count.sum.avg/123,
                                       time.bin.group == "pre.sunset" ~ bin.count.sum.avg/46,
                                       time.bin.group == "pre.civil.twilight.end" ~ bin.count.sum.avg/27,
                                       time.bin.group == "night" ~ bin.count.sum.avg/46)) %>% 
  mutate(eclipse = ifelse(time.bin.group == "eclipse",
                          1,
                          0)) %>% 
  group_by(Common.Name) %>% 
  mutate(bin.count.sum.avg.per.min.scale.eclipse = bin.count.sum.avg.per.min/max(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  group_by(Common.Name,
           eclipse) %>% 
  mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/max(bin.count.sum.avg.per.min)) %>% 
  ungroup()
  
## save data file
write.csv(data.filter.sec.sum.avg,
          'data/data_output/data.filter.sec.sum.avg.csv')
# load data
data.filter.sec.sum.avg = read.csv('data/data_output/data.filter.sec.sum.avg.csv') %>% 
  dplyr::select(-X)

### create graphs of data to assess dawn vs dusk behavior
## average counts per bird
# print all into one pdf
pdf("figures/all_sp_bins/average counts per bird time bins.pdf",
    onefile = TRUE)
for(i in unique(data.filter.sec.sum.avg$Common.Name)){
  p <-
    data.filter.sec.sum.avg %>% 
    filter(Common.Name == i) %>% 
    mutate(eclipse = ifelse(time.bin.group == 'eclipse',
                            'eclipse',
                            'normal')) %>% 
    ggplot(aes(x = time.bin.group.num, 
               y = bin.count.sum.avg.per.min,
               color = eclipse)) +
    geom_vline(xintercept = 3.5,
               linetype = 'dashed') +
    geom_vline(xintercept = 4.5,
               linetype = 'dashed') +
    geom_point() +
    theme_classic() +
    scale_color_manual(values = c('red',
                                  'black')) +
    ylab('Average calls per minute') +
    xlab('Time bin across day') +
    theme(legend.position = 'none') +
    ggtitle(paste0(i,
                   ': calls per minute across day'))
  print(p)
}
dev.off()

# paper
# pdf("figures/all_sp_bins/average counts per bird time bins paper.pdf",
#     onefile = TRUE)
for(i in unique(data.filter.sec.sum.avg$Common.Name)){
  p <-
    data.filter.sec.sum.avg %>% 
    filter(Common.Name == i) %>% 
    filter(time.bin.group != 'eclipse') %>% 
    ggplot(aes(x = as.factor(time.bin.group.num), 
               y = bin.count.sum.avg.per.min)) +
    geom_vline(xintercept = 3.5,
               linetype = 'dashed') +
    geom_vline(xintercept = 4.5,
               linetype = 'dashed') +
    geom_bar(stat= 'identity') +
    theme_classic(base_size = 12) +
    ylab('Average vocalizations per minute') +
    xlab('Time bin across day') +
    theme(legend.position = 'none') +
    ggtitle(paste0(i)) +
    annotate('text',
             label = 'dawn',
                  x = 2,
                  y = Inf,
             vjust = 1)+
    annotate('text',
             label = 'afternoon',
             x = 4,
             y = Inf,
             vjust = 1)+
    annotate('text',
             label = 'dusk',
             x = 6,
             y = Inf,
             vjust = 1)
  # print(p)
  ggsave(paste0('figures/all_sp_bins/time_bin/',
                i, 
                ' average counts per time bin paper.png'),
         plot = p)
}
# dev.off()

## scaled average counts per bird
# print all into one pdf
pdf("figures/all_sp_bins/scaled average counts per bird time bins.pdf",
    onefile = TRUE)
for(i in unique(data.filter.sec.sum.avg$Common.Name)){
  p <-
    data.filter.sec.sum.avg %>% 
    filter(Common.Name == i) %>%
    filter(time.bin.group != 'eclipse') %>% 
    ggplot(aes(x = time.bin.group.num, 
               y = bin.count.sum.avg.per.min.scale)) +
    geom_vline(xintercept = 3.5,
               linetype = 'dashed') +
    geom_vline(xintercept = 4.5,
               linetype = 'dashed') +
    geom_point() +
    theme_classic() +
    scale_color_manual(values = c('black')) +
    ylab('Scaled average calls per minute') +
    xlab('Time bin across day') +
    theme(legend.position = 'none') +
    ggtitle(paste0(i,
                   ': scaled calls per minute across day'))
  print(p)
}
dev.off()

#### create clusters by time of day
### All time points
# remove eclipse
data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat)

# graph clustering 
pdf('figures/all_sp_bins/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 8)

# graph heatmap
pdf('figures/all_sp_bins/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names,
        border = 'black')
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# graph birds per cluster
data.filter.sec.sum.avg %>% 
  full_join(bird.cluster.names.df) %>% 
  filter(time.bin.group != 'eclipse') %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             color = Common.Name.color)) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed') +
  geom_vline(xintercept = 4.5,
             linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4)
ggsave('figures/all_sp_bins/Time bin cluster heatmap line graphs.pdf',
    width = 10,
    height = 8)


### morning and afternoon
# remove eclipse
data.filter.sec.sum.avg.mat.morning = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num != 5) %>% 
  filter(time.bin.group.num != 6) %>% 
  filter(time.bin.group.num != 7) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
waa <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat.morning %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df.morning <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df.morning, aes(x = clusters, 
                   y = wss, 
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/Time bin cluster screeplot morning.png')

# cluster birds
bird.cluster.morning = pvclust(data.filter.sec.sum.avg.mat.morning)

# graph clustering 
pdf('figures/all_sp_bins/Time bin cluster morning.pdf')
plot(bird.cluster.morning)
dev.off()

# graph heatmap
pdf('figures/all_sp_bins/Time bin cluster heatmap morning.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat.morning, 
        cluster_columns = bird.cluster.morning$hclust, 
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = c(7),
        border = 'black')
dev.off()

### evening and afternoon
# remove eclipse
data.filter.sec.sum.avg.mat.evening = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num != 1) %>% 
  filter(time.bin.group.num != 2) %>% 
  filter(time.bin.group.num != 3) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
waa <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat.evening %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df.evening <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df.evening, aes(x = clusters, 
                           y = wss, 
                           group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/Time bin cluster screeplot evening.png')


# cluster birds
bird.cluster.evening = pvclust(data.filter.sec.sum.avg.mat.evening)

# graph clustering 
pdf('figures/all_sp_bins/Time bin cluster evening.pdf')
plot(bird.cluster.evening)
dev.off()

# graph heatmap
pdf('figures/all_sp_bins/Time bin cluster heatmap evening.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat.evening, 
        cluster_columns = bird.cluster.evening$hclust, 
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = c(7),
        border = 'black')
dev.off()

### no.night and afternoon
# remove eclipse
data.filter.sec.sum.avg.mat.no.night = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num != 7) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
waa <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat.no.night %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df.no.night <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df.no.night, aes(x = clusters, 
                           y = wss, 
                           group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/Time bin cluster screeplot no.night.png')


# cluster birds
bird.cluster.no.night = pvclust(data.filter.sec.sum.avg.mat.no.night)

# graph clustering 
pdf('figures/all_sp_bins/Time bin cluster no.night.pdf')
plot(bird.cluster.no.night)
dev.off()

# graph heatmap
pdf('figures/all_sp_bins/Time bin cluster heatmap no.night.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat.no.night, 
        cluster_columns = bird.cluster.no.night$hclust, 
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = c(7),
        border = 'black')
dev.off()








#### clustering ####
#### create clusters by time of day
### Max value as reference
## All time points
# remove eclipse
data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# # Decide how many clusters to look at
# n_clusters <- 20
# 
# # Initialize total within sum of squares error: wss
# wss <- numeric(n_clusters)
# 
# set.seed(123)
# 
# # Look over 1 to n possible clusters
# for (i in 1:20) {
#   # Fit the model: km.out
#   km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
#                      t(), 
#                    centers = i)
#   # Save the within cluster sum of squares
#   wss[i] <- km.out$tot.withinss
# }
# 
# # Produce a scree plot
# wss_df <- tibble(clusters = 1:20, wss = wss)
# 
# ggplot(wss_df, aes(x = clusters, 
#                    y = wss,  
#                    group = 1)) +
#   geom_point(size = 4)+
#   geom_line() +
#   xlab('Number of clusters') +
#   theme_classic()
# ggsave('figures/all_sp_bins/test/all.max/Time bin cluster screeplot.png')
# 
# # cluster birds
# bird.cluster = pvclust(data.filter.sec.sum.avg.mat)
# 
# # graph clustering 
# pdf('figures/all_sp_bins/test/all.max/Time bin cluster.pdf')
# plot(bird.cluster)
# dev.off()
# 
# # assign clusters
# bird.cluster.names = cutree(bird.cluster$hclust,
#                             k = 12)
# 
# # graph heatmap
# pdf('figures/all_sp_bins/test/all.max/Time bin cluster heatmap.pdf',
#     width = 10,
#     height = 8)
# Heatmap(data.filter.sec.sum.avg.mat, 
#         # cluster_columns = bird.cluster$hclust,
#         heatmap_legend_param = list(title = "Calls"),
#         col = c("white",
#                 "black"),
#         column_split  = bird.cluster.names,
#         border = 'black',
#         row_title = 'all.max')
# dev.off()
# 
# # assign clusters
# bird.cluster.names.df = bird.cluster.names %>% 
#   as.data.frame() %>% 
#   rename(Cluster_id = '.') %>% 
#   rownames_to_column('Common.Name')
# 
# # graph birds per cluster
# data.filter.sec.sum.avg %>% 
#   full_join(bird.cluster.names.df) %>% 
#   filter(time.bin.group != 'eclipse') %>% 
#   # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
#   #                                    1,
#   #                                    0)) %>% 
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              # color = Common.Name.color
#              )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   geom_vline(xintercept = 4.5,
#              linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   theme(legend.position = 'none') +
#   facet_wrap(Cluster_id~.,
#              nrow = 3,
#              ncol = 4)+
#   ggtitle('all.max')
# ggsave('figures/all_sp_bins/test/all.max/Time bin cluster heatmap line graphs.pdf',
#        width = 10,
#        height = 8)
# 
# ## dawn time points
# # remove eclipse
# data.filter.sec.sum.avg.filter = data.filter.sec.sum.avg %>% 
#   filter(eclipse == 0) %>% 
#   filter(time.bin.group.num %in% c(1:4)) %>% 
#   select(Common.Name,
#          time.bin.group.num,
#          bin.count.sum.avg.per.min) %>%
#   group_by(Common.Name) %>% 
#   mutate(max = max(bin.count.sum.avg.per.min)) %>% 
#   ungroup() %>% 
#   mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/max) %>% 
#   select(Common.Name,
#          time.bin.group.num,
#          bin.count.sum.avg.per.min.scale) 
# 
# # make matrix
# data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg.filter %>% 
#   pivot_wider(names_from = 'time.bin.group.num',
#               values_from = 'bin.count.sum.avg.per.min.scale') %>% 
#   column_to_rownames('Common.Name') %>% 
#   as.matrix() %>% 
#   t()
# 
# # Decide how many clusters to look at
# n_clusters <- 20
# 
# # Initialize total within sum of squares error: wss
# wss <- numeric(n_clusters)
# 
# set.seed(123)
# 
# # Look over 1 to n possible clusters
# for (i in 1:20) {
#   # Fit the model: km.out
#   km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
#                      t(), 
#                    centers = i)
#   # Save the within cluster sum of squares
#   wss[i] <- km.out$tot.withinss
# }
# 
# # Produce a scree plot
# wss_df <- tibble(clusters = 1:20, wss = wss)
# 
# ggplot(wss_df, aes(x = clusters, 
#                    y = wss,  
#                    group = 1)) +
#   geom_point(size = 4)+
#   geom_line() +
#   xlab('Number of clusters') +
#   theme_classic()
# ggsave('figures/all_sp_bins/test/dawn.max/Time bin cluster screeplot.png')
# 
# # cluster birds
# bird.cluster = pvclust(data.filter.sec.sum.avg.mat)
# 
# # graph clustering 
# pdf('figures/all_sp_bins/test/dawn.max/Time bin cluster.pdf')
# plot(bird.cluster)
# dev.off()
# 
# # assign clusters
# bird.cluster.names = cutree(bird.cluster$hclust,
#                             k = 12)
# 
# # graph heatmap
# pdf('figures/all_sp_bins/test/dawn.max/Time bin cluster heatmap.pdf',
#     width = 10,
#     height = 8)
# Heatmap(data.filter.sec.sum.avg.mat, 
#         # cluster_columns = bird.cluster$hclust,
#         heatmap_legend_param = list(title = "Calls"),
#         col = c("white",
#                 "black"),
#         column_split  = bird.cluster.names,
#         border = 'black',
#         row_title = 'dawn.max')
# dev.off()
# 
# # assign clusters
# bird.cluster.names.df = bird.cluster.names %>% 
#   as.data.frame() %>% 
#   rename(Cluster_id = '.') %>% 
#   rownames_to_column('Common.Name')
# 
# # graph birds per cluster
# data.filter.sec.sum.avg.filter %>% 
#   full_join(bird.cluster.names.df) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(1:4)) %>% 
#   # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
#   #                                    1,
#   #                                    0)) %>% 
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              # color = Common.Name.color
#              )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   # geom_vline(xintercept = 4.5,
#   #            linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   theme(legend.position = 'none') +
#   facet_wrap(Cluster_id~.,
#              nrow = 3,
#              ncol = 4)+
#   ggtitle('dawn.max')
# ggsave('figures/all_sp_bins/test/dawn.max/Time bin cluster heatmap line graphs.pdf',
#        width = 10,
#        height = 8)
# 
# ## dusk time points
# # remove eclipse
# data.filter.sec.sum.avg.filter = data.filter.sec.sum.avg %>% 
#   filter(eclipse == 0) %>% 
#   filter(time.bin.group.num %in% c(4:7)) %>% 
#   select(Common.Name,
#          time.bin.group.num,
#          bin.count.sum.avg.per.min) %>%
#   group_by(Common.Name) %>% 
#   mutate(max = max(bin.count.sum.avg.per.min)) %>% 
#   ungroup() %>% 
#   mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/max) %>% 
#   select(Common.Name,
#          time.bin.group.num,
#          bin.count.sum.avg.per.min.scale) 
# 
# # make matrix
# data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg.filter %>% 
#   pivot_wider(names_from = 'time.bin.group.num',
#               values_from = 'bin.count.sum.avg.per.min.scale') %>% 
#   column_to_rownames('Common.Name') %>% 
#   as.matrix() %>% 
#   t()
# 
# # Decide how many clusters to look at
# n_clusters <- 20
# 
# # Initialize total within sum of squares error: wss
# wss <- numeric(n_clusters)
# 
# set.seed(123)
# 
# # Look over 1 to n possible clusters
# for (i in 1:20) {
#   # Fit the model: km.out
#   km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
#                      t(), 
#                    centers = i)
#   # Save the within cluster sum of squares
#   wss[i] <- km.out$tot.withinss
# }
# 
# # Produce a scree plot
# wss_df <- tibble(clusters = 1:20, wss = wss)
# 
# ggplot(wss_df, aes(x = clusters, 
#                    y = wss,  
#                    group = 1)) +
#   geom_point(size = 4)+
#   geom_line() +
#   xlab('Number of clusters') +
#   theme_classic()
# ggsave('figures/all_sp_bins/test/dusk.max/Time bin cluster screeplot.png')
# 
# # cluster birds
# bird.cluster = pvclust(data.filter.sec.sum.avg.mat)
# 
# # graph clustering 
# pdf('figures/all_sp_bins/test/dusk.max/Time bin cluster.pdf')
# plot(bird.cluster)
# dev.off()
# 
# # assign clusters
# bird.cluster.names = cutree(bird.cluster$hclust,
#                             k = 12)
# 
# # graph heatmap
# pdf('figures/all_sp_bins/test/dusk.max/Time bin cluster heatmap.pdf',
#     width = 10,
#     height = 8)
# Heatmap(data.filter.sec.sum.avg.mat, 
#         # cluster_columns = bird.cluster$hclust,
#         heatmap_legend_param = list(title = "Calls"),
#         col = c("white",
#                 "black"),
#         column_split  = bird.cluster.names,
#         border = 'black',
#         row_title = 'dusk.max') 
# dev.off()
# 
# # assign clusters
# bird.cluster.names.df = bird.cluster.names %>% 
#   as.data.frame() %>% 
#   rename(Cluster_id = '.') %>% 
#   rownames_to_column('Common.Name')
# 
# # graph birds per cluster
# data.filter.sec.sum.avg.filter %>% 
#   full_join(bird.cluster.names.df) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(4:7)) %>% 
#   # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
#   #                                    1,
#   #                                    0)) %>% 
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              # color = Common.Name.color
#   )) +
#   # geom_vline(xintercept = 3.5,
#   #            linetype = 'dashed') +
#   geom_vline(xintercept = 4.5,
#              linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   theme(legend.position = 'none') +
#   facet_wrap(Cluster_id~.,
#              nrow = 3,
#              ncol = 4) +
#   ggtitle('dusk.max')
# ggsave('figures/all_sp_bins/test/dusk.max/Time bin cluster heatmap line graphs.pdf',
#        width = 10,
#        height = 8)

### sum value as reference
## All time points
# remove eclipse
data.filter.sec.sum.avg.filter = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min) %>%
  group_by(Common.Name) %>% 
  mutate(sum = sum(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/sum) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) 

# make matrix
data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg.filter %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()


# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/test/all.sum/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat)

# graph clustering 
pdf('figures/all_sp_bins/test/all.sum/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 12)

# graph heatmap
pdf('figures/all_sp_bins/test/all.sum/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names,
        border = 'black',
        row_title = 'all.sum')
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# graph birds per cluster
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df) %>%
  # filter(time.bin.group != 'eclipse') %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             # color = Common.Name.color
  )) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed') +
  geom_vline(xintercept = 4.5,
             linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4)+
  ggtitle('all.sum')
ggsave('figures/all_sp_bins/test/all.sum/Time bin cluster heatmap line graphs.pdf',
       width = 10,
       height = 8)

## dawn time points
# remove eclipse
data.filter.sec.sum.avg.filter = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num %in% c(1:4)) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min) %>%
  group_by(Common.Name) %>% 
  mutate(sum = sum(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/sum) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) 

# make matrix
data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg.filter %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()


# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat)

# graph clustering 
pdf('figures/all_sp_bins/test/dawn.sum/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 12)

# graph heatmap
pdf('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names,
        border = 'black',
        row_title = 'dawn.sum')
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# save clusters
write.csv(bird.cluster.names.df %>% 
            rename(Cluster_id_dawn = Cluster_id),
          'data/data_output/song_clusters/dawn.sum.cluster.csv',
          row.names = F)

# graph birds per cluster
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             # color = Common.Name.color
  )) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4)+
  ggtitle('dawn.sum')
ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap line graphs.pdf',
       width = 10,
       height = 8)

# assign clusters by time point of chorus
# no: 5,8,12
# early: 3,4,10,
# mid: 6,7,11
# late: 1,2,9
bird.cluster.names.df.assign = bird.cluster.names.df %>% 
  mutate(Culster_id_assign = case_when(
    Cluster_id %in% c(5,8,12) ~ "none",
    Cluster_id %in% c(3,4,10) ~ "early",
    Cluster_id %in% c(6,7,11) ~ "mid",
    Cluster_id %in% c(1,2,9) ~ "late",
    TRUE ~ NA
  ))

# convert dataframe to named vector for use with heatmap
bird.cluster.names.assign = bird.cluster.names.df.assign %>% 
  select(Common.Name,
         Culster_id_assign) %>% 
  deframe()

# heatmap with new graphs
pdf('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap assign.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names.assign,
        border = 'black',
        row_title = 'dawn.sum')
dev.off()

# graph new clusters
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df.assign) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
                                                       'Rock Pigeon',
                                                       'Tree Swallow',
                                                       'Song Sparrow'),
                                    Common.Name,
                                    'Other')) %>%
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             color = factor(Common.Name.color,
                            levels = c('American Robin',
                                       'Song Sparrow',
                                       'Tree Swallow',
                                       'Rock Pigeon' ,
                                       "Other" ))
  )) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(factor(Culster_id_assign,
                    levels = c("early",
                               "mid",
                               "late",
                               "none"))~.,
             nrow = 1,
             ncol = 4)+
  ggtitle('dawn.sum') +
  scale_color_manual("Bird",
                     values = c('American Robin' = 'black',
                                'Rock Pigeon' = "black",
                                'Tree Swallow' = "black",
                                'Song Sparrow' = "black",
                                "Other" = "black"))
ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap line graphs assign.pdf',
       width = 10,
       height = 8)

# color specific species
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df.assign) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
                                                       'Rock Pigeon',
                                                       'Tree Swallow',
                                                       'Song Sparrow'),
                                    Common.Name,
                                     'Other')) %>%
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             color = factor(Common.Name.color,
                            levels = c('American Robin',
                                       'Song Sparrow',
                                       'Tree Swallow',
                                       'Rock Pigeon' ,
                                       "Other" ))
  )) +
  geom_vline(xintercept = 3.5,
             linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(factor(Culster_id_assign,
                    levels = c("early",
                               "mid",
                               "late",
                               "none"))~.,
             nrow = 1,
             ncol = 4)+
  ggtitle('dawn.sum') +
  scale_color_manual("Bird",
                     values = c('American Robin' = 'red',
                                'Rock Pigeon' = "black",
                                'Tree Swallow' = "blue",
                                'Song Sparrow' = "brown",
                                "Other" = "grey"))
ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap line graphs assign color.pdf',
       width = 10,
       height = 8)



## dusk time points
# remove eclipse
data.filter.sec.sum.avg.filter = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num %in% c(4:7)) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min) %>%
  group_by(Common.Name) %>% 
  mutate(sum = sum(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.scale = bin.count.sum.avg.per.min/sum) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale) 

# make matrix
data.filter.sec.sum.avg.mat = data.filter.sec.sum.avg.filter %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()

# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat)

# graph clustering 
pdf('figures/all_sp_bins/test/dusk.sum/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 12)

# graph heatmap
pdf('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names,
        border = 'black',
        row_title = 'dusk.sum') 
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# save clusters
write.csv(bird.cluster.names.df %>% 
            rename(Cluster_id_dusk = Cluster_id),
          'data/data_output/song_clusters/dusk.sum.cluster.csv',
          row.names = F)


# graph birds per cluster
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(4:7)) %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             # color = Common.Name.color
  )) +
  # geom_vline(xintercept = 3.5,
  #            linetype = 'dashed') +
  geom_vline(xintercept = 4.5,
             linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4) +
  ggtitle('dusk.sum')
ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap line graphs.pdf',
       width = 10,
       height = 8)

# assign clusters by time point of chorus
# no: 2,4,9,10,11,7,12
# early: 1,6,8
# mid: 3,5
# late: nonte
bird.cluster.names.df.assign = bird.cluster.names.df %>% 
  mutate(Culster_id_assign = case_when(
    Cluster_id %in% c(2,4,9,10,11,7,12) ~ "none",
    Cluster_id %in% c(1,6,8) ~ "early",
    Cluster_id %in% c(3,5) ~ "mid",
    Cluster_id %in% c() ~ "late",
    TRUE ~ NA
  ))

# convert dataframe to named vector for use with heatmap
bird.cluster.names.assign = bird.cluster.names.df.assign %>% 
  select(Common.Name,
         Culster_id_assign) %>% 
  deframe()

# heatmap with new graphs
pdf('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap assign.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("white",
                "black"),
        column_split  = bird.cluster.names.assign,
        border = 'black',
        row_title = 'dusk.sum')
dev.off()

# graph new clusters
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df.assign) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
                                                       'Rock Pigeon',
                                                       'Tree Swallow',
                                                       'Song Sparrow'),
                                    Common.Name,
                                    'Other')) %>%
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             color = factor(Common.Name.color,
                            levels = c('American Robin',
                                       'Song Sparrow',
                                       'Tree Swallow',
                                       'Rock Pigeon' ,
                                       "Other" ))
  )) +
  geom_vline(xintercept = 4.5,
             linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(factor(Culster_id_assign,
                    levels = c("early",
                               "mid",
                               "late",
                               "none"))~.,
             nrow = 1,
             ncol = 4)+
  ggtitle('dusk.sum') +
  scale_color_manual("Bird",
                     values = c('American Robin' = 'black',
                                'Rock Pigeon' = "black",
                                'Tree Swallow' = "black",
                                'Song Sparrow' = "black",
                                "Other" = "black"))
ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap line graphs assign.pdf',
       width = 10,
       height = 8)

# color specific species
data.filter.sec.sum.avg.filter %>% 
  full_join(bird.cluster.names.df.assign) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
                                                       'Rock Pigeon',
                                                       'Tree Swallow',
                                                       'Song Sparrow'),
                                    Common.Name,
                                    'Other')) %>%
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale,
             group = Common.Name,
             color = factor(Common.Name.color,
                            levels = c('American Robin',
                                       'Song Sparrow',
                                       'Tree Swallow',
                                       'Rock Pigeon' ,
                                       "Other" ))
  )) +
  geom_vline(xintercept = 4.5,
             linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point() +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(factor(Culster_id_assign,
                    levels = c("early",
                               "mid",
                               "late",
                               "none"))~.,
             nrow = 1,
             ncol = 4)+
  ggtitle('dusk.sum') +
  scale_color_manual("Bird",
                     values = c('American Robin' = 'red',
                                'Rock Pigeon' = "black",
                                'Tree Swallow' = "blue",
                                'Song Sparrow' = "brown",
                                "Other" = "grey"))
ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap line graphs assign color.pdf',
       width = 10,
       height = 8)

#### compare relative to afternoon
### dawn time points
# remove eclipse
data.filter.sec.sum.avg.filter.rel = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num %in% c(1:4)) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min) %>%
  left_join(data.filter.sec.sum.avg %>% 
              filter(eclipse == 0) %>% 
              filter(time.bin.group.num %in% c(4)) %>% 
              select(Common.Name,
                     bin.count.sum.avg.per.min) %>% 
              rename(afternoon.bin.count.sum.avg.per.min = bin.count.sum.avg.per.min)) %>% 
  group_by(Common.Name) %>% 
  mutate(sum = sum(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.scale.rel = (bin.count.sum.avg.per.min-afternoon.bin.count.sum.avg.per.min)/sum) %>%
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale.rel) %>% 
  filter(time.bin.group.num != 4)

# make matrix
data.filter.sec.sum.avg.mat.rel = data.filter.sec.sum.avg.filter.rel %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale.rel') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()


# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat.rel %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/test/dawn.sum.rel/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat.rel,
                       r=seq(0.7,1.4,by=.1))

# graph clustering 
pdf('figures/all_sp_bins/test/dawn.sum.rel/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 12)

# graph heatmap
pdf('figures/all_sp_bins/test/dawn.sum.rel/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat.rel, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("red",
                "white",
                "blue"),
        column_split  = bird.cluster.names,
        border = 'black',
        row_title = 'dawn.sum.rel')
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# save clusters
write.csv(bird.cluster.names.df %>% 
            rename(Cluster_id_dawn_rel = Cluster_id),
          'data/data_output/song_clusters/dawn.rel.cluster.csv',
          row.names = F)

# graph birds per cluster
data.filter.sec.sum.avg.filter.rel %>% 
  full_join(bird.cluster.names.df) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale.rel,
             group = Common.Name,
             # color = Common.Name.color
  )) +
  # geom_vline(xintercept = 3.5,
  #            linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point(aes(color = bin.count.sum.avg.per.min.scale.rel > 0)) +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4)+
  ggtitle('dawn.sum.rel')
ggsave('figures/all_sp_bins/test/dawn.sum.rel/Time bin cluster heatmap line graphs.pdf',
       width = 10,
       height = 8)

# # assign clusters by time point of chorus
# # no: 5,8,12
# # early: 3,4,10,
# # mid: 6,7,11
# # late: 1,2,9
# bird.cluster.names.df.assign = bird.cluster.names.df %>% 
#   mutate(Culster_id_assign = case_when(
#     Cluster_id %in% c(5,8,12) ~ "none",
#     Cluster_id %in% c(3,4,10) ~ "early",
#     Cluster_id %in% c(6,7,11) ~ "mid",
#     Cluster_id %in% c(1,2,9) ~ "late",
#     TRUE ~ NA
#   ))
# 
# # convert dataframe to named vector for use with heatmap
# bird.cluster.names.assign = bird.cluster.names.df.assign %>% 
#   select(Common.Name,
#          Culster_id_assign) %>% 
#   deframe()
# 
# # heatmap with new graphs
# pdf('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap assign.pdf',
#     width = 10,
#     height = 8)
# Heatmap(data.filter.sec.sum.avg.mat, 
#         # cluster_columns = bird.cluster$hclust,
#         heatmap_legend_param = list(title = "Calls"),
#         col = c("white",
#                 "black"),
#         column_split  = bird.cluster.names.assign,
#         border = 'black',
#         row_title = 'dawn.sum')
# dev.off()
# 
# # graph new clusters
# data.filter.sec.sum.avg.filter.rel %>% 
#   full_join(bird.cluster.names.df.assign) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(1:4)) %>% 
#   mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
#                                                        'Rock Pigeon',
#                                                        'Tree Swallow',
#                                                        'Song Sparrow'),
#                                     Common.Name,
#                                     'Other')) %>%
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              color = factor(Common.Name.color,
#                             levels = c('American Robin',
#                                        'Song Sparrow',
#                                        'Tree Swallow',
#                                        'Rock Pigeon' ,
#                                        "Other" ))
#   )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   # geom_vline(xintercept = 4.5,
#   #            linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   # theme(legend.position = 'none') +
#   facet_wrap(factor(Culster_id_assign,
#                     levels = c("early",
#                                "mid",
#                                "late",
#                                "none"))~.,
#              nrow = 1,
#              ncol = 4)+
#   ggtitle('dawn.sum') +
#   scale_color_manual("Bird",
#                      values = c('American Robin' = 'black',
#                                 'Rock Pigeon' = "black",
#                                 'Tree Swallow' = "black",
#                                 'Song Sparrow' = "black",
#                                 "Other" = "black"))
# ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap line graphs assign.pdf',
#        width = 10,
#        height = 8)
# 
# # color specific species
# data.filter.sec.sum.avg.filter.rel %>% 
#   full_join(bird.cluster.names.df.assign) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(1:4)) %>% 
#   mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
#                                                        'Rock Pigeon',
#                                                        'Tree Swallow',
#                                                        'Song Sparrow'),
#                                     Common.Name,
#                                     'Other')) %>%
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              color = factor(Common.Name.color,
#                             levels = c('American Robin',
#                                        'Song Sparrow',
#                                        'Tree Swallow',
#                                        'Rock Pigeon' ,
#                                        "Other" ))
#   )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   # geom_vline(xintercept = 4.5,
#   #            linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   # theme(legend.position = 'none') +
#   facet_wrap(factor(Culster_id_assign,
#                     levels = c("early",
#                                "mid",
#                                "late",
#                                "none"))~.,
#              nrow = 1,
#              ncol = 4)+
#   ggtitle('dawn.sum') +
#   scale_color_manual("Bird",
#                      values = c('American Robin' = 'red',
#                                 'Rock Pigeon' = "black",
#                                 'Tree Swallow' = "blue",
#                                 'Song Sparrow' = "brown",
#                                 "Other" = "grey"))
# ggsave('figures/all_sp_bins/test/dawn.sum/Time bin cluster heatmap line graphs assign color.pdf',
#        width = 10,
#        height = 8)


### dusk time points
# remove eclipse
data.filter.sec.sum.avg.filter.rel = data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num %in% c(4:7)) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min) %>%
  left_join(data.filter.sec.sum.avg %>% 
              filter(eclipse == 0) %>% 
              filter(time.bin.group.num %in% c(4)) %>% 
              select(Common.Name,
                     bin.count.sum.avg.per.min) %>% 
              rename(afternoon.bin.count.sum.avg.per.min = bin.count.sum.avg.per.min)) %>% 
  group_by(Common.Name) %>% 
  mutate(sum = sum(bin.count.sum.avg.per.min)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.scale.rel = (bin.count.sum.avg.per.min-afternoon.bin.count.sum.avg.per.min)/sum) %>%
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min.scale.rel) %>% 
  filter(time.bin.group.num != 4)

# make matrix
data.filter.sec.sum.avg.mat.rel = data.filter.sec.sum.avg.filter.rel %>% 
  pivot_wider(names_from = 'time.bin.group.num',
              values_from = 'bin.count.sum.avg.per.min.scale.rel') %>% 
  column_to_rownames('Common.Name') %>% 
  as.matrix() %>% 
  t()


# Decide how many clusters to look at
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:20) {
  # Fit the model: km.out
  km.out <- kmeans(data.filter.sec.sum.avg.mat.rel %>% 
                     t(), 
                   centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:20, wss = wss)

ggplot(wss_df, aes(x = clusters, 
                   y = wss,  
                   group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  xlab('Number of clusters') +
  theme_classic()
ggsave('figures/all_sp_bins/test/dusk.sum.rel/Time bin cluster screeplot.png')

# cluster birds
bird.cluster = pvclust(data.filter.sec.sum.avg.mat.rel,
                       r=seq(0.7,1.4,by=.1))

# graph clustering 
pdf('figures/all_sp_bins/test/dusk.sum.rel/Time bin cluster.pdf')
plot(bird.cluster)
dev.off()

# assign clusters
bird.cluster.names = cutree(bird.cluster$hclust,
                            k = 12)

# graph heatmap
pdf('figures/all_sp_bins/test/dusk.sum.rel/Time bin cluster heatmap.pdf',
    width = 10,
    height = 8)
Heatmap(data.filter.sec.sum.avg.mat.rel, 
        # cluster_columns = bird.cluster$hclust,
        heatmap_legend_param = list(title = "Calls"),
        col = c("red",
                "white",
                "blue"),
        column_split  = bird.cluster.names,
        border = 'black',
        row_title = 'dusk.sum.rel')
dev.off()

# assign clusters
bird.cluster.names.df = bird.cluster.names %>% 
  as.data.frame() %>% 
  rename(Cluster_id = '.') %>% 
  rownames_to_column('Common.Name')

# save clusters
write.csv(bird.cluster.names.df %>% 
            rename(Cluster_id_dusk_rel = Cluster_id),
          'data/data_output/song_clusters/dusk.rel.cluster.csv',
          row.names = F)

# graph birds per cluster
data.filter.sec.sum.avg.filter.rel %>% 
  full_join(bird.cluster.names.df) %>% 
  # filter(time.bin.group != 'eclipse') %>% 
  # filter(time.bin.group.num %in% c(1:4)) %>% 
  # mutate(Common.Name.color = ifelse(Common.Name == "American Robin",
  #                                    1,
  #                                    0)) %>% 
  ggplot(aes(x = time.bin.group.num, 
             y = bin.count.sum.avg.per.min.scale.rel,
             group = Common.Name,
             # color = Common.Name.color
  )) +
  # geom_vline(xintercept = 3.5,
  #            linetype = 'dashed') +
  # geom_vline(xintercept = 4.5,
  #            linetype = 'dashed') +
  geom_point(aes(color = bin.count.sum.avg.per.min.scale.rel > 0)) +
  geom_line() +
  theme_classic() +
  # scale_color_manual(values = c('red')) +
  ylab('Scaled average calls per minute') +
  xlab('Time bin across day') +
  # theme(legend.position = 'none') +
  facet_wrap(Cluster_id~.,
             nrow = 3,
             ncol = 4)+
  ggtitle('dusk.sum.rel') 
ggsave('figures/all_sp_bins/test/dusk.sum.rel/Time bin cluster heatmap line graphs.pdf',
       width = 10,
       height = 8)

# # assign clusters by time point of chorus
# # no: 5,8,12
# # early: 3,4,10,
# # mid: 6,7,11
# # late: 1,2,9
# bird.cluster.names.df.assign = bird.cluster.names.df %>% 
#   mutate(Culster_id_assign = case_when(
#     Cluster_id %in% c(5,8,12) ~ "none",
#     Cluster_id %in% c(3,4,10) ~ "early",
#     Cluster_id %in% c(6,7,11) ~ "mid",
#     Cluster_id %in% c(1,2,9) ~ "late",
#     TRUE ~ NA
#   ))
# 
# # convert dataframe to named vector for use with heatmap
# bird.cluster.names.assign = bird.cluster.names.df.assign %>% 
#   select(Common.Name,
#          Culster_id_assign) %>% 
#   deframe()
# 
# # heatmap with new graphs
# pdf('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap assign.pdf',
#     width = 10,
#     height = 8)
# Heatmap(data.filter.sec.sum.avg.mat, 
#         # cluster_columns = bird.cluster$hclust,
#         heatmap_legend_param = list(title = "Calls"),
#         col = c("white",
#                 "black"),
#         column_split  = bird.cluster.names.assign,
#         border = 'black',
#         row_title = 'dusk.sum')
# dev.off()
# 
# # graph new clusters
# data.filter.sec.sum.avg.filter.rel %>% 
#   full_join(bird.cluster.names.df.assign) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(1:4)) %>% 
#   mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
#                                                        'Rock Pigeon',
#                                                        'Tree Swallow',
#                                                        'Song Sparrow'),
#                                     Common.Name,
#                                     'Other')) %>%
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              color = factor(Common.Name.color,
#                             levels = c('American Robin',
#                                        'Song Sparrow',
#                                        'Tree Swallow',
#                                        'Rock Pigeon' ,
#                                        "Other" ))
#   )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   # geom_vline(xintercept = 4.5,
#   #            linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   # theme(legend.position = 'none') +
#   facet_wrap(factor(Culster_id_assign,
#                     levels = c("early",
#                                "mid",
#                                "late",
#                                "none"))~.,
#              nrow = 1,
#              ncol = 4)+
#   ggtitle('dusk.sum') +
#   scale_color_manual("Bird",
#                      values = c('American Robin' = 'black',
#                                 'Rock Pigeon' = "black",
#                                 'Tree Swallow' = "black",
#                                 'Song Sparrow' = "black",
#                                 "Other" = "black"))
# ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap line graphs assign.pdf',
#        width = 10,
#        height = 8)
# 
# # color specific species
# data.filter.sec.sum.avg.filter.rel %>% 
#   full_join(bird.cluster.names.df.assign) %>% 
#   # filter(time.bin.group != 'eclipse') %>% 
#   # filter(time.bin.group.num %in% c(1:4)) %>% 
#   mutate(Common.Name.color = ifelse(Common.Name %in% c('American Robin',
#                                                        'Rock Pigeon',
#                                                        'Tree Swallow',
#                                                        'Song Sparrow'),
#                                     Common.Name,
#                                     'Other')) %>%
#   ggplot(aes(x = time.bin.group.num, 
#              y = bin.count.sum.avg.per.min.scale,
#              group = Common.Name,
#              color = factor(Common.Name.color,
#                             levels = c('American Robin',
#                                        'Song Sparrow',
#                                        'Tree Swallow',
#                                        'Rock Pigeon' ,
#                                        "Other" ))
#   )) +
#   geom_vline(xintercept = 3.5,
#              linetype = 'dashed') +
#   # geom_vline(xintercept = 4.5,
#   #            linetype = 'dashed') +
#   geom_point() +
#   geom_line() +
#   theme_classic() +
#   # scale_color_manual(values = c('red')) +
#   ylab('Scaled average calls per minute') +
#   xlab('Time bin across day') +
#   # theme(legend.position = 'none') +
#   facet_wrap(factor(Culster_id_assign,
#                     levels = c("early",
#                                "mid",
#                                "late",
#                                "none"))~.,
#              nrow = 1,
#              ncol = 4)+
#   ggtitle('dusk.sum') +
#   scale_color_manual("Bird",
#                      values = c('American Robin' = 'red',
#                                 'Rock Pigeon' = "black",
#                                 'Tree Swallow' = "blue",
#                                 'Song Sparrow' = "brown",
#                                 "Other" = "grey"))
# ggsave('figures/all_sp_bins/test/dusk.sum/Time bin cluster heatmap line graphs assign color.pdf',
#        width = 10,
#        height = 8)



#### time bin chorus threshold ####
### compare ratio of calls to afternoon
data.filter.sec.sum.avg.filter.ratio =
  data.filter.sec.sum.avg %>% 
  filter(eclipse == 0) %>% 
  filter(time.bin.group.num != c(4)) %>% 
  select(Common.Name,
         time.bin.group.num,
         bin.count.sum.avg.per.min,
         Time.day) %>%
  left_join(data.filter.sec.sum.avg %>% 
              filter(eclipse == 0) %>% 
              filter(time.bin.group.num %in% c(4)) %>% 
              select(Common.Name,
                     bin.count.sum.avg.per.min) %>% 
              rename(afternoon.bin.count.sum.avg.per.min = bin.count.sum.avg.per.min)) %>% 
  group_by(Common.Name,
           Time.day) %>%
  mutate(max.time.bin = max(bin.count.sum.avg.per.min),
         max.time.bin = ifelse(max.time.bin == bin.count.sum.avg.per.min,
                               1,
                               0)) %>% 
  ungroup() %>% 
  mutate(bin.count.sum.avg.per.min.ratio.afternoon = log(bin.count.sum.avg.per.min/afternoon.bin.count.sum.avg.per.min),
         percent.change.afternoon = 100*((exp(bin.count.sum.avg.per.min.ratio.afternoon)-1))) %>% 
  filter(max.time.bin == 1 ) %>% 
  select(-c(max.time.bin)) %>% 
  group_by(Common.Name,
           Time.day) %>%
  mutate(Max = max(bin.count.sum.avg.per.min,afternoon.bin.count.sum.avg.per.min)) %>% 
  ungroup()  %>% 
  mutate(Chorus = ifelse(percent.change.afternoon > 100,
                         1,
                         0)) 

# save data frame
write.csv(data.filter.sec.sum.avg.filter.ratio,
          file = 'data/data_output/data.filter.sec.sum.avg.filter.ratio.csv',
          row.names = F)
# load
data.filter.sec.sum.avg.filter.ratio = read.csv('data/data_output/data.filter.sec.sum.avg.filter.ratio.csv')

## graph distribution of max calls
# data.filter.sec.sum.avg.filter.ratio %>% 
#   ggplot(aes(y = log(percent.change.afternoon),
#              x = Time.day)) +
#   geom_violin()+
#   geom_point() +
#   geom_dotplot(binaxis = 'y',
#                binwidth = 0.1,
#                aes(fill = as.factor(Chorus))) +
#   geom_hline(yintercept = 0)+
#   theme_classic() +
#   ylab('Number of birds') +
#   xlab('Percent change from afternoon') +
#   theme(legend.position = 'null') +
#   scale_fill_manual(values = c('gray75',
#                                'black')) +
#   ggtitle('Chorus for dawn and dusk ') 


data.filter.sec.sum.avg.filter.ratio %>% 
  ggplot(aes(x = percent.change.afternoon,
             fill = as.factor(Chorus))) +
  geom_histogram(binwidth = 10)+
  # geom_violin()+
  # geom_point() +
  # geom_dotplot(binaxis = 'y',
  #              binwidth = 0.1,
  #              aes(fill = as.factor(Chorus))) +
  geom_vline(xintercept = 0,
             linetype = 1)+
  theme_classic(base_size = 12) +
  ylab('Number of birds') +
  xlab('Percent change from afternoon') +
  theme(legend.position = 'null') +
  scale_fill_manual(values = c('gray75',
                               'black')) +
  ggtitle('Chorus for dawn and dusk ') +
    facet_grid(Time.day~.)
  ggsave('figures/all_sp_bins/Chorus distribution.pdf',
         height = 5,
         width = 6.5,
         units = 'in')
  

### run logistic regression for eclipse effect vs dawn/dusk chorus
## create data frame chorus type 
chorus.type = data.filter.sec.sum.avg.filter.ratio %>% 
  dplyr::select(Common.Name,
                Chorus,
                Time.day)

## get data frame of casual impacts stats
# load data
impact.stand.results = read.csv('data/data_output/impact.stand.results.csv') %>% 
  select(-X)
  
# create temporary dataframe
impact.stand.results.sig = impact.stand.results %>% 
  filter(Window.time != 'during_eclipse') %>% 
  filter(Window.size == 12) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(sig.direction = case_when(p.adj<0.05 & AbsEffect > 0 ~ 1,
                                 p.adj<0.05 & AbsEffect < 0 ~ -1,
                                 TRUE ~ 0)) %>% 
  dplyr::select(Common.Name,
                Window.time,
                sig.direction)

# add data for during totality
impact.stand.results.sig = impact.stand.results %>% 
  filter(Window.time == 'during_eclipse') %>% 
  filter(Window.size == 4) %>% 
  filter(Training.dates == "6,7,9") %>% 
  mutate(sig.direction = case_when(p.adj<0.05 & AbsEffect > 0 ~ 1,
                                   p.adj<0.05 & AbsEffect < 0 ~ -1,
                                   TRUE ~ 0)) %>% 
  dplyr::select(Common.Name,
                Window.time,
                sig.direction) %>% 
  rbind(impact.stand.results.sig)

## combine data  
impact.stand.results.sig = impact.stand.results.sig %>% 
  full_join(chorus.type)

# create column of 1 or 0 significance 
impact.stand.results.sig = impact.stand.results.sig %>% 
  mutate(sig = ifelse(sig.direction !=0,
                      1,
                      0))

# only look at positive effect
# create column of 1 or 0 significance 
impact.stand.results.sig = impact.stand.results.sig %>% 
  mutate(sig.pos = ifelse(sig.direction >0,
                      1,
                      0))

## calculate multinomial regression
# before
glm.before = VGAM::vglm(as.factor(sig.direction) ~ Chorus,
                 data = impact.stand.results.sig %>% 
                   filter(Window.time == 'before_eclipse') %>% 
                   filter(Time.day == 'dusk'),
                 family = 'multinomial')


summary(glm.before)
sink('figures/all_sp_bins/regression/glm.before.txt')
print(summary(glm.before))
sink()

# during
glm.during = VGAM::vglm(as.factor(sig.direction) ~ Chorus,
                 data = impact.stand.results.sig %>% 
                   filter(Window.time == 'during_eclipse') %>% 
                   filter(Time.day == 'dusk'),
                 family = 'multinomial')

summary(glm.during)
sink('figures/all_sp_bins/regression/glm.during.txt')
print(summary(glm.during))
sink()

# after
glm.after = VGAM::vglm(as.factor(sig.direction) ~ Chorus,
                 data = impact.stand.results.sig %>% 
                   filter(Window.time == 'after_eclipse') %>% 
                   filter(Time.day == 'dawn'),
                 family = 'multinomial')

summary(glm.after)
sink('figures/all_sp_bins/regression/glm.after.txt')
print(summary(glm.after))
sink()

# later
glm.later = VGAM::vglm(as.factor(sig.direction) ~ Chorus,
                 data = impact.stand.results.sig %>% 
                   filter(Window.time == 'after.28_eclipse') %>% 
                   filter(Time.day == 'dawn'),
                 family = 'multinomial')

summary(glm.later)
sink('figures/all_sp_bins/regression/glm.later.txt')
print(summary(glm.later))
sink()  

## calculate binomial regression
# before
glm.before.bi = glm(sig~ Chorus,
                        data = impact.stand.results.sig %>% 
                          filter(Window.time == 'before_eclipse') %>% 
                          filter(Time.day == 'dusk'),
                        family = 'binomial')


summary(glm.before.bi)
sink('figures/all_sp_bins/regression/glm.before.bi.txt')
print(summary(glm.before.bi))
sink()

# during
glm.during.bi = glm(sig~ Chorus,
                    data = impact.stand.results.sig %>% 
                      filter(Window.time == 'during_eclipse') %>% 
                      filter(Time.day == 'dusk'),
                    family = 'binomial')


summary(glm.during.bi)
sink('figures/all_sp_bins/regression/glm.during.bi.txt')
print(summary(glm.during.bi))
sink()

# after
glm.after.bi = glm(sig~ Chorus,
                    data = impact.stand.results.sig %>% 
                      filter(Window.time == 'after_eclipse') %>% 
                      filter(Time.day == 'dawn'),
                    family = 'binomial')
summary(glm.after.bi)

sink('figures/all_sp_bins/regression/glm.after.bi.txt')
print(summary(glm.after.bi))
sink()

# later
glm.later.bi = glm(sig~ Chorus,
                   data = impact.stand.results.sig %>% 
                     filter(Window.time == 'after.28_eclipse') %>% 
                     filter(Time.day == 'dawn'),
                   family = 'binomial')


summary(glm.later.bi)
sink('figures/all_sp_bins/regression/glm.later.bi.txt')
print(summary(glm.later.bi))
sink()
  
  
## calculate binomial regression
## combine -1 and 0
# before
glm.before.bi.pos = glm(sig.pos~ Chorus,
                    data = impact.stand.results.sig %>%
                      filter(Window.time == 'before_eclipse') %>%
                      filter(Time.day == 'dusk'),
                    family = 'binomial')


summary(glm.before.bi.pos)
sink('figures/all_sp_bins/regression/glm.before.bi.pos.txt')
print(summary(glm.before.bi.pos))
sink()

# during
glm.during.bi.pos = glm(sig.pos~ Chorus,
                    data = impact.stand.results.sig %>%
                      filter(Window.time == 'during_eclipse') %>%
                      filter(Time.day == 'dusk'),
                    family = 'binomial')


summary(glm.during.bi.pos)
sink('figures/all_sp_bins/regression/glm.during.bi.pos.txt')
print(summary(glm.during.bi.pos))
sink()

# after
glm.after.bi.pos = glm(sig.pos~ Chorus,
                   data = impact.stand.results.sig %>%
                     filter(Window.time == 'after_eclipse') %>%
                     filter(Time.day == 'dawn'),
                   family = 'binomial')
summary(glm.after.bi.pos)

sink('figures/all_sp_bins/regression/glm.after.bi.pos.txt')
print(summary(glm.after.bi.pos))
sink()

# later
glm.later.bi.pos = glm(sig.pos~ Chorus,
                   data = impact.stand.results.sig %>%
                     filter(Window.time == 'after.28_eclipse') %>%
                     filter(Time.day == 'dawn'),
                   family = 'binomial')


summary(glm.later.bi.pos)
sink('figures/all_sp_bins/regression/glm.later.bi.pos.txt')
print(summary(glm.later.bi.pos))
sink()
  

