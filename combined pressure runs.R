library(tidyverse)
library(ggplot2)
library(readr)

# setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")
# setwd("./sacs pH 6 then pressure")
# setwd("./area results, thresh calc")

#setwd("~/student_documents/UBC/Research/Malawi\\KenyaData\\DinoLite\\pH6_sac_pressurization/area results, thresh calc")

#setwd("~/student_documents/UBC/Research/Malawi\\data\\sac pressure, fixed pH\\sacs pH 6 then pressure/area results, thresh calc")
 
setwd("~/student_documents/UBC/Research/Malawi\\americanus comparison\\pH6/area results, thresh calc")


# the list of file names will provide the files for r to read in, as well as 
# the names for the indvd larva column
list_of_results <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(list_of_results)

#rm(list_of_results)

# read in files, modify text in larva column, add psi column for x axis
larvae.df <- read_delim(list_of_results,
                      delim = ",",
                      id = "larva") %>%
  mutate(" " = NULL) %>%
  mutate(location = "Kenya") %>%
  mutate(larva = substr(larva, 1, nchar(larva)-4)) %>% #drop file extension
  group_by(larva, sac) %>%
  mutate(psi = (row_number() * 30) - 30) %>% # x axis starting from zero [[[((( FOR MALAWI DATA )))]]]
  #mutate(psi = (row_number() * 10) - 10) %>% # x axis starting from zero [[[((( FOR KENYA DATA )))]]]
  #slice(-1) %>% # FOR AMERICANUS removes 1st obsv of group (still to figure out showing 0 - 01h change)
  mutate(area_set_0psi = Area - Area[1]) %>% # still abs size, but starting from o psi
  mutate(delta_frac.area = abs((Area - Area[1]) / Area[1])) %>%
  mutate(pct.area = ((Area - Area[1]) / Area[1]) * 100) %>%
  ungroup() %>%
  group_by(psi) %>%
  mutate(mean_pct_area = mean(pct.area)) %>% # mean for all sacs at a psi
  mutate(sd_pct_area = sd(pct.area)) 

print(larvae.df)

#larvae.df <- larvae.df %>% 

# % change
ggplot(data = larvae.df, 
       aes(x = psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= pct.area), size = 2, alpha = 0.4) +
  geom_line(aes(y= pct.area), alpha = 0.4) +
  geom_point(aes(y= mean_pct_area), size = 3, colour = "black") +
  geom_line(aes(y= mean_pct_area), colour = "black")

# absolute sizez
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_smooth(method = 'lm', 
              formula = y ~ splines::bs(x, df = 4, knots = 200),
              inherit.aes = F, 
              aes(x = psi, y= Area), 
              color = '#555555') +
  geom_point(aes(y= Area)) +
  geom_line(aes(y= Area))

# difference from 0 psi
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
#  geom_point(aes(y= area_set_0psi, size = 2)) +
  geom_point(aes(y= area_set_0psi)) +
  geom_line(aes(y= area_set_0psi))


######################   making the file for a given pH   #########################
getwd()


write_csv(larvae.df,
          "../../data frames by pH/larvae_pH6.csv")


###################################################################################

## combine the three pH crushes
setwd("../../data frames by pH")
pH_crush_files <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(pH_crush_files)

pH_crushes.BC <- read_delim(pH_crush_files, ###  rename for location   ###
                         delim = ",",
                         id = NULL)

pH_crushes.BC <- pH_crushes.BC %>%  ###  rename for location   ###
  ungroup() %>%
  group_by(pH, psi) %>%
  mutate(mean_area_by_pH = mean(Area)) %>%
  mutate(mean_set0 = mean(area_set_0psi)) # mean for all sacs at a psi

# absolute sizez
ggplot(data = filter(pH_crushes, pH == "6"),
         #pH_crushes,
       aes(x= psi,
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


#############################     model?     ##################################
install.packages("ggformula")
library(ggformula)

ggplot(larvae.df, aes(y = pct.area, x = psi)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_spline(col = "black", df = 4) +
  theme_minimal()

log_larvae.df <- larvae.df %>%
  mutate(kpa_sealv = (psi * 6.89476) + 101.325) %>%
  mutate(log_kpa_sealv = log(kpa_sealv)) %>%
  mutate(log_area = log(Area)) %>%
  mutate(pct100.area = pct.area + 100) %>%
  mutate(log_pct100.area = log(pct100.area)) %>%
  select(-area_set_0psi) %>%
  select(larva, sac, type, 
         psi, kpa_sealv, log_kpa_sealv, 
         Area, log_area, pct.area, pct100.area, log_pct100.area)

print(log_larvae.df)

ggplot(log_larvae.df, aes(y = pct100.area, x = kpa_sealv)) + 
  geom_point(size = 3, col = "firebrick") + 
  #geom_spline(col = "black", df = 4) +
  geom_smooth(method = "loess", se = T)
  theme_minimal()


#try a Michaelis-Menten curve/ model
mich_men <- nls(Area ~ a*kpa_sealv / ( b+kpa_sealv), 
                  data = log_larvae.df, list(a = 1, b = 1))
#plot
ggplot(log_larvae.df, aes(y = pct100.area, x = kpa_sealv)) + 
  geom_smooth(method = "nls", method.args = 
                list(formula = y ~ a * x / (b + x), start = list(a = 1, b = 1)), 
              data = log_larvae.df, se = FALSE, col = "black") + 
  geom_point(size = 3, col = "firebrick") +
  theme_classic()



# various linear models
mod1 <- lm(Area ~ psi, larvae.df)
mod2 <- lm(Area ~ psi + type, larvae.df)
mod3 <- lm(Area ~ psi * type, larvae.df)
mod4 <- lm(Area ~ (psi + I(psi^2)) * type, larvae.df)
mod5 <- lm(Area ~ (psi + I(psi^2) + I(psi^3)) * type, larvae.df)

plot(mod2)

ggplot(larvae.df, aes(y = pct.area, x = psi)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = "lm", se = FALSE,
              formula = y ~ (x + I((x^-2)))) +
  theme(aspect.ratio = 0.80) +
  theme_classic()


