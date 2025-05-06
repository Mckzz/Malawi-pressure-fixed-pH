library(tidyverse)
library(ggplot2)
library(readr)
library(emmeans)

##  pull out max crush depths for each larva of each species and each pH

# fresh water density value
FWD <- 997.0474
# gravity acceleration
g <- 9.80665

crush_depths <- pH_crushes %>%
  ungroup() %>%
  select(species, larva, type, sac, pH, kpa) %>%
  group_by(pH, species, larva, sac) %>%
  mutate(crush_press.kpa = max(kpa)) %>%
  mutate(kpa = NULL) %>%
  mutate(depth.m = (crush_press.kpa*1000)/(FWD*g)) %>%
  # group_by(pH, species) %>%
  # mutate(spp.mean_depth = mean(depth.m)) %>%
  # mutate(spp.sd_depth = sd(depth.m)) %>%
  unique() %>%
  ungroup() %>%
  arrange(pH, species, larva) %>%
  group_by(pH, species, larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs for the grouping
  ungroup() %>%
  print()

# why zeros for anom?

## bring in head caps (for instar) and sac diameters

headcap <- read_csv("~/student_documents/UBC/Research/Malawi/excised_pressure_headcaps.csv")
sac_widths.all_spp <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sac_widths-all_spp.csv") %>%
  print()

measures <- left_join(sac_widths.all_spp, headcap)

crush_depths_measures <- left_join(crush_depths, measures) %>%
  mutate(aspectLW = length_0press / diam_1st) %>%
  print()

# make separate data frames to filter for fourth instars separately
crush_depths_measures.amer <- filter(crush_depths_measures, species == "C. americanus") %>%
  filter(head_cap > 1.35) %>% #all are already 4ths
  print()
crush_depths_measures.anom <- filter(crush_depths_measures, species == "C. anomalus") %>%
  filter(head_cap > 0.8) %>% #all are already 4ths
  print()
crush_depths_measures.pallid <- filter(crush_depths_measures, species == "C. pallidipes") %>%
  filter(head_cap > 1) %>% # removed 2
  print()
crush_depths_measures.ed <- filter(crush_depths_measures, species == "C. edulis") %>%
  filter(head_cap > 0.88) %>% #all are already 4ths
  print()

crush_depths_measures.4th <- rbind(crush_depths_measures.amer, 
                               crush_depths_measures.anom, 
                               crush_depths_measures.pallid, 
                               crush_depths_measures.ed) %>%
  filter(depth.m != 0) %>% #remove zero depths
  group_by(species, pH) %>% # clac means etc. from each species and pH
  mutate(mean.spp_pH_crush.kpa = mean(crush_press.kpa), .after = crush_press.kpa) %>%
  mutate(mean.spp_pH_crush.m = mean(depth.m), 
         sd.spp_pH_crush.m = sd(depth.m), .after = depth.m) %>%
  mutate(mean_diam_1st = mean(diam_1st), 
         sd_diam_1st = sd(diam_1st)) %>%
  mutate(log_depth = log(depth.m)) %>%
  mutate(logdiam = log(diam_1st)) %>%
  # group_by(species, pH) %>%
  # mutate(average_depth = mean(depth.m), .after = pH) %>%
  print()


# get intact crush depth data for each species, make it "pH 5" for putting it on the plot below 
combine_intact_crush <- read_csv("C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\Malawi\\data/intact_crush_all_4th.csv") %>%
  rename(depth_m = depth.m) %>%
  group_by(species) %>%
  mutate(mean_depth_m = mean(depth_m), 
         sd_depth_m = sd(depth_m)) %>%
  mutate(pH = 5) %>%
  print()

######    excised, pH 6, 7, 8, the four species crush depths
ggplot() +
  geom_point(data = crush_depths_measures.4th, aes(x = pH, y = depth.m,
                                               group = interaction(species, pH),
                                               colour = species),
             position = position_nudge(x = -0.05)) +
  geom_point(data = crush_depths_measures.4th, aes(x = pH, y = mean.spp_pH_crush.m,
                                               group = interaction(species, pH),
                                               colour = species), 
             size = 4, 
             shape = 1, 
             position = position_nudge(x = 0.05)) +
  geom_errorbar(data = crush_depths_measures.4th, 
                aes(x= pH, 
                    ymin= mean.spp_pH_crush.m - sd.spp_pH_crush.m, 
                    ymax= mean.spp_pH_crush.m + sd.spp_pH_crush.m, 
                    colour = species), 
                width= 0.1, 
                position = position_nudge(x = 0.05)) +
  geom_point(data = combine_intact_crush, 
             aes(x = pH, y = depth_m, 
                 group = interaction(species, pH), 
                 colour = species)) +
  geom_point(data = combine_intact_crush, 
             aes(x = pH, y = mean_depth_m, 
                 group = interaction(species, pH), 
                 colour = species),
             size = 4, 
             shape = 1, 
             position = position_nudge(x = 0.07)) +
  geom_errorbar(data = combine_intact_crush, 
                aes(x= pH, 
                    ymin= mean_depth_m - sd_depth_m, 
                    ymax= mean_depth_m + sd_depth_m, 
                    colour = species), 
                width= 0.1, 
                position = position_nudge(x = 0.07)) +
  scale_y_continuous(n.breaks = 10) +
  ylab("Maximum depth withstood (m)") +
  theme_classic()




library(lmerTest)
library(emmeans)

z.pH_spp_interact <- lmer(depth.m ~ pH * species + (1|ID), data = crush_depths_measures.4th, na.action = na.exclude)
z.pH_spp <- lmer(depth.m ~ pH + species + (1|ID), data = crush_depths_measures.4th, na.action = na.exclude)

isSingular(z.pH_spp_interact, tol = 1e-4)
isSingular(z.pH_spp, tol = 1e-4)

summary(z.pH_spp_interact)
plot(z.pH_spp_interact)
anova(z.pH_spp_interact)

summary(z.pH_spp)
plot(z.pH_spp)
anova(z.pH_spp)

AIC(z.pH_spp_interact, z.pH_spp) # with interaction has a lower AIC

pH.spp_crush_trends <- emtrends(z.pH_spp_interact, ~ species, var = "pH") %>%
  as.data.frame() %>%
  mutate(t_value = pH.trend / SE, 
         p_value = 2 * pt(-abs(t_value), df)) %>%
  print()
# for the interaction, anova significance is mainly driven by edulis, pallidipes marginal non, rest non-sig

# treat pH as a factor for pairwise
z.pH.fct_spp_interact <- lmer(depth.m ~ as.factor(pH) * species + (1|ID), data = crush_depths_measures.4th, na.action = na.exclude)
emmeans(z.pH.fct_spp_interact, pairwise ~ species * pH)
# edulis 6 - 7 significant, 7 - 8 non-sig

#make df for t test of edulis pH 7 and 8
edulis7_8t.test <- crush_depths_measures.4th %>%
  filter(species == "C. edulis") %>%
  filter(pH > 6) %>%
  ungroup() %>%
  group_by(pH) %>%
  mutate(average_depth = mean(depth.m), .after = pH) %>%
  print()

t.test(depth.m ~ as.character(pH), data = edulis7_8t.test)
# non-significant with much more believable p value
# try  model / anova / tukeu of 6, 7, and 8??

# species specific model to try tukey
edulis6_7_8.separate <- crush_depths_measures.4th %>%
  filter(species == "C. edulis") %>%
  print()

z.pH.fct_spp_interact.ed <- lmer(depth.m ~ as.factor(pH) + (1|ID), data = edulis6_7_8.separate, na.action = na.exclude)
emmeans(z.pH.fct_spp_interact.ed, pairwise ~ pH)
# p = 0.9952 for 7 - 8


t.test(depth.m ~ intact, data = crush_compare)






# paired t-test of 1st and last sac diameters to test if it changed
# sac widths pivoted long from widths only df
widths_long <- sac_widths.all_spp %>%
  pivot_longer(cols=c(`diam_1st`, `diam_last`), 
               names_to = "first_last", values_to = "diameter") %>%
  print()

t.test(diameter ~ first_last, paired = T, data = filter(widths_long, species == "C. edulis"))

# % difference from first to last diameter is 0.7540942 (note that it's positive (diff between species?))
(((mean(crush_depths_measures.4th$diam_last)) - (mean(crush_depths_measures.4th$diam_1st))) / 
  mean(crush_depths_measures.4th$diam_1st)) *100

###
###  t-testing the individual species
###
t.test(diameter ~ first_last, paired = T, data = filter(widths_long, species == "C. americanus"))
amer <- widths_long %>%
  filter(species == "C. americanus") %>%
  pivot_wider(names_from = first_last, values_from = diameter) %>%
  print()

(((mean(amer$diam_last)) - (mean(amer$diam_1st))) / 
    mean(amer$diam_1st)) *100
# 1.047949

t.test(diameter ~ first_last, paired = T, data = filter(widths_long, species == "C. anomalus"))
anom <- widths_long %>%
  filter(species == "C. anomalus") %>%
  pivot_wider(names_from = first_last, values_from = diameter) %>%
  print()

(((mean(anom$diam_last)) - (mean(anom$diam_1st))) / 
    mean(anom$diam_1st)) *100
# 0.2115449

t.test(diameter ~ first_last, paired = T, data = filter(widths_long, species == "C. pallidipes"))
pal <- widths_long %>%
  filter(species == "C. pallidipes") %>%
  pivot_wider(names_from = first_last, values_from = diameter) %>%
  print()

(((mean(pal$diam_last)) - (mean(pal$diam_1st))) / 
    mean(pal$diam_1st)) *100
# 1.990698

t.test(diameter ~ first_last, paired = T, data = filter(widths_long, species == "C. edulis"))
ed <- widths_long %>%
  filter(species == "C. edulis") %>%
  pivot_wider(names_from = first_last, values_from = diameter) %>%
  print()

(((mean(ed$diam_last)) - (mean(ed$diam_1st))) / 
    mean(ed$diam_1st)) *100
# -0.3606541

# plot 1st vs last diameters
ggplot(filter(widths_long, species == "C. edulis"),
       aes(x = first_last, y = diameter,
           colour = species)) +
  geom_jitter()

ggplot(widths_long, 
       aes(x = first_last, y = diameter,
           colour = species)) +
  geom_jitter()

# plot head caps
ggplot(crush_depths_measures.4th, 
       aes(x = species, y = head_cap, 
           colour = species)) + 
  scale_y_continuous(n.breaks = 10) +
  geom_jitter()

# plot mean crush depths
ggplot(crush_depths_measures.4th, 
       aes(x = species, y = mean.spp_crush_press.kpa, 
           colour = species,
           label = mean.spp_crush_press.kpa)) + 
  #scale_y_continuous(n.breaks = 10) +
  geom_point(size = 4) +
  geom_text()



# model species diameters to simply test if they are different
diam_mod <- lmer(diam_1st ~ species + (1|ID), data = crush_depths_measures.4th)
isSingular(diam_mod, tol = 1e-4)

summary(diam_mod)

install.packages("emmeans")
library(emmeans)
emmeans(diam_mod, list(pairwise ~ species), adjust = "tukey")
emmeans(diam_mod, "species")




# get max edulis diameter
max.larv <- crush_depths_measures.4th %>%
  filter(species == "C. edulis" & pH == 6) %>%
  print()
max(max.larv$diam_1st)
#rm(max)                                                                    



############
############  bring in intact larva crush data to compare
############

intact_crush <- read_csv("~/student_documents\\UBC\\Research\\Malawi\\data\\whole larva pressure/intact_crush_depth.csv") %>%
  filter(head_cap_len.mm > 0.88) %>% # filter for 4th instar
  select(larva, depth.m) %>%
  mutate(intact = "yes", .before = larva)

print(intact_crush)

#sacs from one larva avaraged for the excised portion
excised_crush <- crush_depths %>%
  ungroup() %>%
  filter(species == "C. edulis" & pH == 6) %>%
  select(larva, depth.m) %>%
  group_by(larva) %>%
  mutate(depth.m.means = mean(depth.m)) %>%
  mutate(intact = "no", .before = larva) %>%
  mutate(depth.m = NULL) %>%
  rename(depth.m = depth.m.means) %>%
  unique() %>%
  print()


crush_compare <- rbind(intact_crush, excised_crush) %>%
  print()

# 4th instars not significantly different between intact and excised for crush pressure
t.test(depth.m ~ intact, data = crush_compare)


