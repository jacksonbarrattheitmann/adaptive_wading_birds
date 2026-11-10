# Reading in some of the nest success data to take a peek

library(dplyr)
library(tidyverse)
library(readxl)

# We already have the wader data downloaded so
# no need to download it, but others will need to do it. 

dat <- readRDS("Data/nest_success_94_25.RDS")

## What we want to do to get at the adaptive community dynamics portion
## is calculate the biomass production for each species, and each colony. 

## Let's load AVONET

avonet <- read_excel("Data/AVONET Supplementary dataset 1.xlsx", sheet = "AVONET2_eBird")

## and our crosswalk table for our purposes

cross <- read_csv("Data/crosswalk_table.csv") 

## Quick filter
avonet_filt <- avonet %>%
  filter(Species2 %in% cross$Species2)

## Select the columns that we need
avonet_filt <- avonet_filt %>%
  select(Species2, Beak.Length_Culmen, Mass, `Hand-Wing.Index`)

## I need to create a composite score for the smhe and swhe

cross <- cross %>%
  left_join(avonet_filt, by = "Species2")

cross_smhe <- cross %>%
  filter(species_max == "smhe") %>%
  group_by(species_max) %>%
  summarize(Beak.Length_Culmen = mean(Beak.Length_Culmen),
            Mass = mean(Mass),
            `Hand-Wing.Index` = mean(`Hand-Wing.Index`)) %>%
  rename(species = species_max)


cross_smwh <- cross %>%
  filter(species_sub == "smwh") %>%
  group_by(species_sub) %>%
  summarize(Beak.Length_Culmen = mean(Beak.Length_Culmen),
            Mass = mean(Mass),
            `Hand-Wing.Index` = mean(`Hand-Wing.Index`)) %>%
  
  rename(species = species_sub)

## Now some annoying re-joining of everything

cross_final <- cross %>%
  select(-species_max, -species_sub, -Species2) %>%
  rbind(cross_smwh) %>%
  rbind(cross_smhe)


## and now join this to our actual nest_success data frame

dat_trait <- dat %>%
  left_join(cross_final, by = "species") %>%
  mutate(Biomass_start = (incubation_k*(Mass*2)),
         Biomass_out = overall_p*(incubation_k*Mass),
         Total_Beak = incubation_k*Beak.Length_Culmen,
         Total_HWI = incubation_k*`Hand-Wing.Index`)

dat_trait_filt <- dat_trait %>%
  filter(!is.na(overall_p)) %>%
  filter(year > 2002)

## We do need to filter out some species though, because we know they didn't receive equal 
## effort in data collection. And there may be issues with the calculation logic. 
species_int <- c("greg", "whib", "wost")


dat_trait_filt <- dat_trait_filt %>%
  filter(species %in% species_int)

## Reduces our data points, but the biomass calculation really doesn't change much.

## Finally, to really get at our main question, let's apply the overall_p 
## to the newer communities and see if the biomass output looks similar
## under different relative abundance measures

dat_trait_origin <- dat_trait_filt %>%
  filter(year == 2003) %>%
  select(species, overall_p) %>%
  rename(success_baseline = overall_p)

## let's re-join this
dat_trait_compiled <- dat_trait_filt %>%
  left_join(dat_trait_origin, by = "species") %>%
  mutate(Biomass_baseline = success_baseline*(incubation_k*Mass))

dat_yr_summary_compiled <- dat_trait_compiled %>%
  group_by(year) %>%
  summarize(total_nests = sum(incubation_k, na.rm = TRUE),
            CWM_Beak = sum((Total_Beak)/total_nests, na.rm = TRUE),
            CWM_HWI = sum((Total_HWI)/total_nests, na.rm = TRUE),
            Biomass_output = sum(Biomass_out, na.rm = TRUE),
            Biomass_baseline_output = sum(Biomass_baseline, na.rm = TRUE),
            Biomass_relative_growth = (sum(Biomass_out, na.rm = TRUE))/(sum(Biomass_start, na.rm = TRUE)),
            Biomass_baseline_relative_growth = (sum(Biomass_baseline, na.rm = TRUE))/(sum(Biomass_start, na.rm = TRUE)),
            Baseline_diff = Biomass_relative_growth - Biomass_baseline_relative_growth)

###### Plot this #######
ggplot(data = dat_yr_summary_compiled, aes(x = year, y = Baseline_diff)) +
  geom_point(aes(color = Baseline_diff), size = 3, alpha = 0.99) +
  geom_line() +
  scale_color_gradient2(
    low = "red",      # Color for negative values
    mid = "gray90",       # Color for values close to 0
    high = "blue",     # Color for positive values
    midpoint = 0          # Shifts color explicitly at zero
  ) +
  theme_bw()


### Let's re-do this but group by species, to see if the signal is more explicit 
### at the species versus the community level. 
dat_yr_summary_compiled_spp <- dat_trait_compiled %>%
  group_by(year, species) %>%
  summarize(total_nests = sum(incubation_k, na.rm = TRUE),
            CWM_Beak = sum((Total_Beak)/total_nests, na.rm = TRUE),
            CWM_HWI = sum((Total_HWI)/total_nests, na.rm = TRUE),
            Biomass_output = sum(Biomass_out, na.rm = TRUE),
            Biomass_baseline_output = sum(Biomass_baseline, na.rm = TRUE),
            Biomass_relative_growth = (sum(Biomass_out, na.rm = TRUE))/(sum(Biomass_start, na.rm = TRUE)),
            Biomass_baseline_relative_growth = (sum(Biomass_baseline, na.rm = TRUE))/(sum(Biomass_start, na.rm = TRUE)),
            Baseline_diff = Biomass_relative_growth - Biomass_baseline_relative_growth)

###### Plot this #######
ggplot(data = dat_yr_summary_compiled_spp, aes(x = year, y = Baseline_diff)) +
  geom_point(aes(color = Baseline_diff), size = 3, alpha = 0.99) +
  geom_line() +
  facet_wrap(~species) +
  scale_color_gradient2(
    low = "#e74c3c",      # Color for negative values
    mid = "gray90",       # Color for values close to 0
    high = "lightblue",     # Color for positive values
    midpoint = 0          # Shifts color explicitly at zero
  ) +
  theme_bw()

### Perhaps we also need to do this in reverse, take the number of nests 
### from the baseline year, and multiply by that year's success to see if they would have done better.

### Need to figure out conceptually what the difference is here (?). 