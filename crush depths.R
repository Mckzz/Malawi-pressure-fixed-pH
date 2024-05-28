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
  group_by(pH, species) %>%
  mutate(mean_depth = mean(depth.m)) %>%
  mutate(sd_depth = sd(depth.m)) %>%
  unique() %>%
  arrange(pH, species, larva) %>%
  group_by(pH, species, larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs for the grouping
  print()

# why zeros for palid?

ggplot(data = crush_depths, aes(x = pH, y = depth.m,
                                group = interaction(species, pH),
                                colour = species)) +
  geom_point() +
  geom_point(aes(y = mean_depth), 
             size = 4, 
             shape = 1, 
             position = position_nudge(x = 0.1)) +
  geom_errorbar(aes(x= pH, ymin= mean_depth - sd_depth, 
                    ymax= mean_depth + sd_depth),
                width= 0.1,
                position = position_nudge(x = 0.1))


library(lmerTest)

z <- lmer(depth.m ~ pH + species + (1|ID), data = crush_depths, na.action = na.exclude)
isSingular(z, tol = 1e-4)

summary(z)
VarCorr(z)
confint(z)

anova(z)
plot(z)

