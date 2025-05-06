library(tidyverse)
library(ggplot2)
library(readr)
library(emmeans)

# stress / strains at pH 6


## Run "combined pressure runs" to goe the dfs for this script
## cycle through the locations

##Y axis -> psi or kpa, x axis -> area_set_0psi or pct.area

#plot within a location
ggplot(data = filter(pH_crushes, pH == "6"),
       #pH_crushes,
       aes(x= depth.m, y = delta_frac.area,
           group = interaction(species, pH, larva, sac),
           #group = as_factor(pH),
           colour = as_factor(species)), na.rm = F) +
  geom_point(aes(x= depth.m, y = delta_frac.area, 
                 colour = species), alpha = 0.2, size = 0.3) +
  geom_line(aes(x= depth.m, y = delta_frac.area, 
                colour = species), alpha = 0.2, size = 0.2) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. anomalus"), 
              method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 200),
              inherit.aes = F,
              aes(x = depth.m,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. pallidipes"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 1200),
              inherit.aes = F,
              aes(x = depth.m,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. edulis"), method = 'lm', 
              formula = y ~ splines::bs(x, df = 3, knots = 400),
              inherit.aes = F,
              aes(x = depth.m,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. americanus"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 30),
              inherit.aes = F,
              aes(x = depth.m,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  # ylim(0.1, 0.4) + # scale for Kenya sacs
  # xlim(0, 750) + # scale for Kenya sacs
  # ylim(0.05, 0.25) + # scale for Malawi sacs
  # xlim(0, 850) + # scale for Malawi sacs
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm")) 


ggsave("pH 6 stress-strains, three places.pdf", 
       units = c("cm"), 
       width = 12, height = 12, 
       path = "../")   

#setwd("../../../../")
#################################################################################


# filter for low pressure elastic modulus analysis

linear_region <- pH_crushes %>%
  filter(delta_frac.area < 0.19 
         & pH == "6") %>%
  group_by(species, larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs down to the lowest the grouping
  arrange(ID)

linear_region.7 <- pH_crushes %>%
  filter(delta_frac.area < 0.19 
         & pH == "7") %>%
  group_by(species, larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs down to the lowest the grouping
  arrange(ID)

linear_region.8 <- pH_crushes %>%
  filter(delta_frac.area < 0.19 
         & pH == "8") %>%
  group_by(species, larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs down to the lowest the grouping
  arrange(ID)
  
print(linear_region)

#bare bones models, no colour or labels
ggplot(data = linear_region, aes(x= kpa, y = delta_frac.area, 
                                 group = species)) + 
  geom_smooth(method = 'lm',
              formula = y ~ x-1,) + # force through zero)
  coord_flip()

# linear regions of squishing chosen, so then use linear models for slopes/ estimates
ggplot(data = linear_region, aes(x= kpa, y = delta_frac.area,
                                   group = interaction(species, larva, sac))) +
  geom_smooth(data = filter(linear_region, species == "C. anomalus"), method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = species)) +
  geom_smooth(data = filter(linear_region, species == "C. pallidipes"), method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = species)) +
  geom_smooth(data = filter(linear_region, species == "C. edulis"), method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = species)) +
  geom_smooth(data = filter(linear_region, species == "C. americanus"), method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = species)) +
  geom_point(aes(x= kpa, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.3) +
  geom_line(aes(x= kpa, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.2) +
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  ylab("fractional length change (ΔL/Lo) at pH 6") +
  xlab("simulated depth (kPa)") +
  xlim(0, 1600) +
  ylim(0, 0.3) +
  # ylim(0.1, 0.4) + # scale for Kenya sacs
  # xlim(0, 750) + # scale for Kenya sacs
  # ylim(0.05, 0.25) + # scale for Malawi sacs
  # xlim(0, 850) + # scale for Malawi sacs
  theme_classic() +
  #theme(legend.position = c(0.2, 0.8)) +
  theme(legend.position = NULL) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm"))

# # linear regions of squishing chosen, so then use linear models for slopes/ estimates
# ggplot(data = linear_region.8, aes(x= kpa, y = delta_frac.area,
#            group = interaction(species, larva, sac))) +
#   geom_smooth(data = filter(linear_region.8, species == "C. anomalus"), method = 'lm',
#               formula = y ~ x-1, # force through zero
#               # inherit.aes = F,
#               aes(x = kpa,
#                   y = delta_frac.area,
#                   #colour = as_factor(pH),
#                   colour = species, # for when displayed separately
#                   group = species)) +
#   geom_smooth(data = filter(linear_region.8, species == "C. pallidipes"), method = 'lm',
#               formula = y ~ x-1, # force through zero
#               # inherit.aes = F,
#               aes(x = kpa,
#                   y = delta_frac.area,
#                   #colour = as_factor(pH),
#                   colour = species, # for when displayed separately
#                   group = species)) +
#   geom_smooth(data = filter(linear_region.8, species == "C. edulis"), method = 'lm',
#               formula = y ~ x-1, # force through zero
#               # inherit.aes = F,
#               aes(x = kpa,
#                   y = delta_frac.area,
#                   #colour = as_factor(pH),
#                   colour = species, # for when displayed separately
#                   group = species)) +
#   geom_smooth(data = filter(linear_region.8, species == "C. americanus"), method = 'lm',
#               formula = y ~ x-1, # force through zero
#               # inherit.aes = F,
#               aes(x = kpa,
#                   y = delta_frac.area,
#                   #colour = as_factor(pH),
#                   colour = species, # for when displayed separately
#                   group = species)) +
#   geom_point(aes(x= kpa, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.3) +
#   geom_line(aes(x= kpa, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.2) +
#   coord_flip() +
#   scale_x_continuous(n.breaks = 10) +
#   ylab("fractional length change (ΔL/Lo) at pH 8") +
#   xlab("simulated depth (kPa)") +
#   xlim(0, 1600) +
#   ylim(0, 0.3) +
#   # ylim(0.1, 0.4) + # scale for Kenya sacs
#   # xlim(0, 750) + # scale for Kenya sacs
#   # ylim(0.05, 0.25) + # scale for Malawi sacs
#   # xlim(0, 850) + # scale for Malawi sacs
#   theme_classic() +
#   #theme(legend.position = c(0.2, 0.8)) +
#   theme(legend.position = NULL) +
#   #theme(legend.position = "none") +
#   theme(axis.ticks.length = unit(-1, "mm"))

ggsave("pH 6 moduli.pdf", 
       units = c("cm"), 
       width = 12, height = 12, 
       path = "../") 

# random term could vary by intercept only (1|ID), or by slope as well (delta_frac.area|ID)

ymod <- lmer(kpa ~ -1 + delta_frac.area * species + (delta_frac.area|ID), data = linear_region) #including slope variation for random factor, forced through origin
#ymod2 <- lmer(kpa ~ delta_frac.area +  delta_frac.area:species + (delta_frac.area|ID), data = linear_region) #including slope variation for random factor
isSingular(ymod2, tol = 1e-4)

plot(ymod)
summary(ymod)
x <- anova(ymod)
x

AIC(ymod2, ymod) #ymod is better

# xmod <- lmer (delta_frac.area ~ kpa * species + (1|ID), data = linear_region) # reverse axes of modulus analysis
# plot(xmod)
# summary(xmod)

emmeans(ymod, ~ species) # just gives means of crush depth
emtrends(ymod, ~ species, var="delta_frac.area") #gives slopes, or kPa gained per mm diameter

modulus_trends <- emtrends(ymod, ~ species, var = "delta_frac.area") %>% 
  as.data.frame() %>%
  mutate(t_value = delta_frac.area.trend / SE, 
         p_value = 2 * pt(-abs(t_value), df)) %>%
  #mutate(instar_num.trend_unlog = 10^instar_num.trend) %>%
  print()

pairs(modulus_trends) # do it as not yet a df


# this one's p values make some sense   ### estimates are differences
emmeans(ymod, list(pairwise ~ species), adjust = "tukey")





# moduli for pH 8
ymod.8 <- lmer(kpa ~ -1 + delta_frac.area * species + (delta_frac.area|ID), data = linear_region.8)
plot(ymod.8)
summary(ymod.8)
q <- anova(ymod.8)
q


library(lmerTest)
lmerTest::anova(ymod)

install.packages(install.packages('multcomp'))
library(multcomp)
summary(glht(ymod, linfct = mcp(species = "Tukey")), test = adjusted("holm"))



ymod.ed <- lmer(kpa ~ delta_frac.area + (1|ID), data = filter(linear_region, species == "C. edulis"))

isSingular(ymod.ed, tol = 1e-4)

summary(ymod.ed)
anova(ymod.ed)
coef(ymod)

# make df of edulis pH 8 and pH 6 linear regions
linear_region.6.ed <- linear_region %>%
  filter(species == "C. edulis") %>%
  print()

linear_region.8.ed <- linear_region.8 %>%
  filter(species == "C. edulis") %>%
  print()

rm(linear_region.8_6.ed)

linear_region.6_8.ed <- rbind(linear_region.8.ed, linear_region.6.ed) 
# linear regions of squishing chosen, so then use linear models for slopes/ estimates
ggplot(data = linear_region.6_8.ed, aes(x= kpa, y = delta_frac.area,
                                        group = interaction(as.factor(pH), larva, sac),
                                        colour = as.factor(pH))) +
  geom_point() +
  geom_line() +
  geom_smooth(data = linear_region.6_8.ed, method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = pH)) +
  # geom_point(aes(x= kpa, y = delta_frac.area, group = as.factor(pH)), alpha = 0.2, size = 0.3) +
  # geom_line(aes(x= kpa, y = delta_frac.area, group = as.factor(pH)), alpha = 0.2, size = 0.2) +
  # geom_vline(xintercept = 516) +
  # geom_vline(xintercept = 432) +
  # geom_vline(xintercept = 1000) +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("fractional length change (ΔL/Lo)") +
  xlab("simulated depth (kPa)") +
  xlim(0, 1600) +
  ylim(0, 0.3) +
  # ylim(0.1, 0.4) + # scale for Kenya sacs
  # xlim(0, 750) + # scale for Kenya sacs
  # ylim(0.05, 0.25) + # scale for Malawi sacs
  # xlim(0, 850) + # scale for Malawi sacs
  theme_classic() +
  #theme(legend.position = c(0.2, 0.8)) +
  theme(legend.position = NULL) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm"))


ed_6_8_mod <- lm(kpa ~ -1 + delta_frac.area * pH, data = linear_region.6_8.ed)
summary(ed_6_8_mod)

ed_6_8_mod.mix <- lmer(kpa ~ -1 + delta_frac.area * 
                         #pH
                         as_factor(pH) 
                       + (1|ID), data = linear_region.6_8.ed)
summary(ed_6_8_mod.mix)
anova(ed_6_8_mod.mix) 
# pH - delta_frac.area interaction is not significant with forced intercept
##### when pH IS A FACTOR. This is more appropriate??
#  I think yes, because, though pH is continuous, the two pHs are 
#  highly distinct states where directionality of modulus isn't clearly linked
#  to directionality of pH...

# only use where there is 
emtrends(ed_6_8_mod.mix, ~ pH, var = "delta_frac.area")
ed_6_8_mixtrends <- emtrends(ed_6_8_mod.mix, ~ pH, var = "delta_frac.area") %>%
  as.data.frame() %>%
  mutate(t_value = delta_frac.area.trend / SE, 
         p_value = 2 * pt(-abs(t_value), df)) %>%
  print()

pairs(ed_6_8_mixtrends) 
# doesn't work once its a data.frame
# confirms non-sig, so does anova

# model to test all species at pH 6 vs. 8
all_spp_6_8.lin <- pH_crushes %>%
  filter(pH != 7 &
           delta_frac.area < 0.19) %>%
  select(larva, sac, pH, species, kpa, delta_frac.area) %>%
  group_by(pH, species, larva) %>%
  mutate(ID = cur_group_id(), .before = larva) %>%
  print()

ggplot(data = all_spp_6_8.lin, aes(x= kpa, y = delta_frac.area,
                                   group = interaction(species, as.factor(pH), larva, sac), 
                                   colour = species, linetype = as.factor(pH))) +
  geom_point() +
  geom_line() +
  geom_smooth(data = all_spp_6_8.lin, method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = interaction(species, pH))) +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("fractional length change (ΔL/Lo)") +
  xlab("simulated depth (kPa)") +
  xlim(0, 1600) +
  ylim(0, 0.3) +
  theme_classic() +
  #theme(legend.position = c(0.2, 0.8)) +
  theme(legend.position = NULL) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm"))


# check individual species
ggplot(data = filter(all_spp_6_8.lin, species == "C. americanus"), aes(x= kpa, y = delta_frac.area,
                                                                       group = interaction(
                                                                         #species, 
                                                                         as.factor(pH), larva, sac), 
                                                                       #colour = species,
                                                                       linetype = as.factor(pH))) +
  geom_point() +
  geom_line() +
  geom_smooth(data = filter(all_spp_6_8.lin, species == "C. americanus"), method = 'lm',
              formula = y ~ x-1, # force through zero
              # inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = pH))+
  #interaction(species, pH))) +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("fractional length change (ΔL/Lo)") +
  xlab("simulated depth (kPa)") +
  xlim(0, 1600) +
  ylim(0, 0.3) +
  theme_classic() +
  #theme(legend.position = c(0.2, 0.8)) +
  theme(legend.position = NULL) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm"))


all_spp_6_8.lin.mod <- lmer(kpa ~ -1 + delta_frac.area + species + as_factor(pH)
                            + (1|ID), data = all_spp_6_8.lin)

all_spp_6_8.lin.mod.inter.1 <- lmer(kpa ~ -1 + delta_frac.area * species * 
                                      #pH
                                      as_factor(pH) 
                            + (1|ID), data = all_spp_6_8.lin)

# all_spp_6_8.lin.mod.inter.2 <- lmer(kpa ~ -1 + delta_frac.area * as_factor(pH) + species  
#                                     + (1|ID), data = all_spp_6_8.lin)
                                    
summary(all_spp_6_8.lin.mod.inter.1)
anova(all_spp_6_8.lin.mod.inter.1) 

# summary(all_spp_6_8.lin.mod.inter.2)
# anova(all_spp_6_8.lin.mod.inter.2) 

AIC(all_spp_6_8.lin.mod.inter.1, all_spp_6_8.lin.mod.inter.2) # 1 is better


pH_trends.spp <- emtrends(all_spp_6_8.lin.mod.inter.1, ~ pH + species, var = "delta_frac.area")
pairs(pH_trends.spp)


######################################     post stifffness transition    #####################################3

##  plots of individual species to select post stiffness transition test if significant slope

ggplot(data = filter(pH_crushes, pH == "6" & species == "C. edulis"), aes(x= kpa, y = delta_frac.area,
           group = interaction(species, pH, larva, sac),
           #group = as_factor(pH),
           colour = as_factor(species)), na.rm = F) +
  geom_point(aes(x= kpa, y = delta_frac.area), alpha = 0.2, size = 0.6) +
  geom_line(aes(x= kpa, y = delta_frac.area), alpha = 0.2, size = 0.6) +
  geom_smooth(data = filter(pH_crushes, pH == "6" 
                            & species == "C. edulis"
                            & delta_frac.area > 0.279), 
              method = 'lm',
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = "goldenrod", # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" 
                            & species == "C. edulis"), 
              method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 2500),
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  #colour = "orangered3", # for when displayed separately
                  group = interaction(species, pH))) +
  coord_flip() +
  #geom_hline(yintercept = 0.279) + # frac change after which stiff region lin mod is made
  scale_x_continuous(n.breaks = 10) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm"))

ed.stiff_lin_region <- pH_crushes %>%
  filter(species == "C. edulis" 
         & delta_frac.area < 0.279
         #& delta_frac.area < 0.28
         & pH == "6") %>%
  group_by(larva) %>%
  mutate(ID = cur_group_id(), .after = larva) %>% #makes unique IDs down to the lowest the grouping
  arrange(ID)

ymod.stiff.ed <- lmer(kpa ~ delta_frac.area + (1|ID), data = ed.stiff_lin_region)
# isSingular(ymod.stiff.am, tol = 1e-4)
summary(ymod.stiff.ed)
# anova(ymod.stiff.ed)

ymod.stiff.ed_quad <- lmer(kpa ~ (delta_frac.area + I(delta_frac.area^2)) + (1|ID), data = ed.stiff_lin_region)
#anova(ymod.stiff.ed_quad)

aic.ed <- AIC(ymod.stiff.ed, ymod.stiff.ed_quad)

(aic.ed$AIC[1] / aic.ed$AIC[2])*100 #fraction of how much better is linear than quad

####################################################
# playing with stuff

############
############  airsac size at a given pressure across pHs
############  look at species full pressure profiles individually for the three pHs
############  choose a pressure in the later linear region, at the shoulder, and the vertical region, if present
############        must be useable at all three pHs
############  at these pressures, what does mean size do across pHs?
############  
############  

amer.press_pH <- pH_crushes %>%
  filter(species == "C. americanus") %>%
  print()

ggplot(data = amer.press_pH,
       aes(x= kpa, y = delta_frac.area,
           group = interaction(pH, larva, sac),
           colour = as_factor(pH)), na.rm = F) +
  geom_point(size = 1) +
  geom_smooth(data = amer.press_pH, method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 250),
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = as_factor(pH),
                  colour = as_factor(pH))) + # for when displayed separately
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(axis.ticks.length = unit(-1, "mm")) 


anom.press_pH <- pH_crushes %>%
  filter(species == "C. anomalus") %>%
  print()
#######           n of 2 in pH 8        ########
ggplot(data = anom.press_pH,
       aes(x= kpa, y = delta_frac.area,
           group = interaction(pH, larva, sac),
           colour = as_factor(pH)), na.rm = F) +
  geom_point(size = 1) +
  geom_line(size = 0.8) +
  geom_smooth(data = anom.press_pH, method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 250),
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = as_factor(pH),
                  colour = as_factor(pH))) + # for when displayed separately
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(axis.ticks.length = unit(-1, "mm")) 

pal.press_pH <- pH_crushes %>%
  filter(species == "C. pallidipes") %>%
  print()

ggplot(data = pal.press_pH,
       aes(x= kpa, y = delta_frac.area,
           group = interaction(pH, larva, sac),
           colour = as_factor(pH)), na.rm = F) +
  geom_point(size = 1) +
  geom_line(size = 0.2, alpha = 0.5) +
  geom_smooth(data = pal.press_pH, method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 250),
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = as_factor(pH),
                  colour = as_factor(pH))) + # for when displayed separately
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(axis.ticks.length = unit(-1, "mm")) 


ed.press_pH <- pH_crushes %>%
  filter(species == "C. edulis") %>%
  print()

ggplot(data = ed.press_pH,
       aes(x= kpa, y = delta_frac.area,
           group = interaction(pH, larva, sac),
           colour = as_factor(pH)), na.rm = F) +
  # geom_point(size = 1) +
  # geom_line(size = 0.2, alpha = 0.5) +
  geom_smooth(data = ed.press_pH, method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 800),
              inherit.aes = F,
              aes(x = kpa,
                  y = delta_frac.area,
                  group = as_factor(pH),
                  colour = as_factor(pH))) + # for when displayed separately
  coord_flip() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 20) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  theme(axis.ticks.length = unit(-1, "mm")) 

### delta_frac.area for vertical: 0.31
### delta_frac.area for shoulder: 0.285
### delta_frac.area for late linear: 0.185

# absolute sizez
ggplot(data = filter(pH_crushes, pH == "6"),
       #pH_crushes,
       aes(x= kpa,
           group = interaction(pH, larva, sac),
           #group = as_factor(pH),
           colour = as_factor(pH)), na.rm = F) +
  geom_smooth(data = filter(pH_crushes, pH == 6), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 120),
              inherit.aes = F,
              aes(x = psi,
                  y = Area,
                  colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  group = pH)) +
  geom_smooth(data = filter(pH_crushes, pH == 7), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 300),
              inherit.aes = F,
              aes(x = psi,
                  y = Area,
                  colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  group = pH)) +
  geom_smooth(data = filter(pH_crushes, pH == 8), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 20),
              inherit.aes = F,
              aes(x = psi,
                  y = Area,
                  colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  group = pH)) +
  # geom_point(aes(y= Area), alpha = 0.2) +
  # geom_line(aes(y= Area)) +
  # ylim(0.1, 0.4) + # scale for Kenya sacs
  # xlim(0, 750) + # scale for Kenya sacs
  # ylim(0.05, 0.25) + # scale for Malawi sacs
  # xlim(0, 850) + # scale for Malawi sacs
  theme_classic() +
  theme(legend.position = c(0.8, 0.8)) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm")) 

ggsave("three_splines_abs.pdf", 
       units = c("cm"), 
       width = 6, height = 6, 
       path = "../")

#####################################################################################




###########   looking at/ comparing individual larvae   ##########

larva_choose <- larvae.df %>%
  filter(larva == "larva 11" | larva == "larva 12")
print(larva_choose)

# absolute sizez
ggplot(data = larva_choose, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= Area), size = 1) +
  geom_line(aes(y= Area))


# difference from 0 psi
ggplot(data = larva_choose, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= area_set_0psi)) +
  geom_line(aes(y= area_set_0psi))



