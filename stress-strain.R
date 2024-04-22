# stress / strains at pH 6


## Run "combined pressure runs" to goe the dfs for this script
## cycle through the locations

##Y axis -> psi or kpa, x axis -> area_set_0psi or pct.area

#plot within a location
ggplot(data = filter(pH_crushes, pH == "6"),
       #pH_crushes,
       aes(x= kpa, y = delta_frac.area,
           group = interaction(species, pH, larva, sac),
           #group = as_factor(pH),
           colour = as_factor(pH)), na.rm = F) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. anomalus"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 95),
              inherit.aes = F,
              aes(x = psi,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. pallidipes"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 170),
              inherit.aes = F,
              aes(x = psi,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. edulis"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 400),
              inherit.aes = F,
              aes(x = psi,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_smooth(data = filter(pH_crushes, pH == "6" & species == "C. americanus"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 30),
              inherit.aes = F,
              aes(x = psi,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  colour = species, # for when displayed separately
                  group = interaction(species, pH))) +
  geom_point(aes(x= psi, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.3) +
  geom_line(aes(x= psi, y = delta_frac.area, colour = species), alpha = 0.2, size = 0.2) +
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
       path = "../../../")   

#################################################################################

# making a common data frame for the pH_crushes from each location

pH_crushes.everywhere <- rbind(pH_crushes.BC, pH_crushes.Kenya, pH_crushes.Malawi) %>%
  mutate(kPa = psi * 6.89476)

ggplot(data = filter(pH_crushes.everywhere, pH == "6"),
       #pH_crushes,
       aes(x= kPa, y = delta_frac.area,
           group = interaction(location, pH, larva, sac),
           #group = as_factor(pH),
           colour = as_factor(location)), na.rm = F) +
  geom_smooth(data = filter(pH_crushes.everywhere, pH == 6, location == "BC"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 20),
              inherit.aes = F,
              aes(x = kPa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  colour = location,
                  group = location)) +
  geom_smooth(data = filter(pH_crushes.everywhere, pH == 6, location == "Kenya"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 10),
              inherit.aes = F,
              aes(x = kPa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  colour = location,
                  group = location)) +
  geom_smooth(data = filter(pH_crushes.everywhere, pH == 6, location == "Malawi"), method = 'lm',
              formula = y ~ splines::bs(x, df = 3, knots = 2000),
              inherit.aes = F,
              aes(x = kPa,
                  y = delta_frac.area,
                  #colour = as_factor(pH),
                  #colour = "black", # for when displayed separately
                  colour = location,
                  group = location)) +
  # geom_point(aes(x= psi, y = delta_frac.area), alpha = 0.2) +
  # geom_line(aes(x= psi, y = delta_frac.area)) +
  coord_flip() +
  # ylim(0.1, 0.4) + # scale for Kenya sacs
  # xlim(0, 750) + # scale for Kenya sacs
  # ylim(0.05, 0.25) + # scale for Malawi sacs
  # xlim(0, 850) + # scale for Malawi sacs
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)) +
  #theme(legend.position = "none") +
  theme(axis.ticks.length = unit(-1, "mm")) 


setwd("~/student_documents/UBC/Research/Malawi/combined locations")
ggsave("pH 6 stress-strain, all three locations.pdf", 
       units = c("cm"), 
       width = 10, height = 10)   

