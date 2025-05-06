library(tidyverse)
library(ggplot2)
library(readr)

# setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")
# setwd("./sacs pH 6 then pressure")
# setwd("./area results, thresh calc")


setwd("~/student_documents/UBC/Research/Malawi\\KenyaData\\DinoLite\\pH6_sac_pressurization/area results, thresh calc")

#setwd("~/student_documents/UBC/Research/Malawi\\data\\sac pressure, fixed pH\\sacs pH 8 then pressure/area results, thresh calc")
 
#setwd("~/student_documents/UBC/Research/Malawi\\americanus comparison\\pH6/area results, thresh calc")

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
  mutate(species = "NA") %>%  ####  rename for location  ####   (NA for Kenya, to be filled in after)
  mutate(larva = substr(larva, 1, nchar(larva)-4)) %>% #drop file extension
  group_by(larva, sac) %>%
  #mutate(psi = (row_number() * 30) - 30) %>% # x axis starting from zero [[[((( FOR MALAWI DATA )))]]]
  mutate(psi = (row_number() * 10) - 10) %>% # x axis starting from zero [[[((( FOR KENYA DATA )))]]]
  #slice(-1) %>% # FOR AMERICANUS removes 1st obsv of group (still to figure out showing 0 - 01h change)
  mutate(kpa = (psi * 6.89476)) %>%
  mutate(area_set_0psi = Area - Area[1]) %>% # still abs size, but starting from o psi
  mutate(delta_frac.area = abs((Area - Area[1]) / Area[1])) %>%
  mutate(pct.area = ((Area - Area[1]) / Area[1]) * 100) %>%
  ungroup() %>%
  group_by(kpa) %>%
  mutate(mean_pct_area = mean(pct.area)) %>% # mean for all sacs at a psi
  mutate(sd_pct_area = sd(pct.area)) 

print(larvae.df)

((6.89476 * 630)*1000)/(FWD*g)
((6.89476 * 720)*1000)/(FWD*g)
((6.89476 * 819)*1000)/(FWD*g)

######################   making the file for a given pH   #########################
getwd()


write_csv(larvae.df,
          "../../data frames by pH/larvae_pH8.csv")

#larvae.df <- larvae.df %>% 

# % change
# ggplot(data = larvae.df, 
#        aes(x = psi,
#            group = interaction(larva, sac),
#            colour = larva), na.rm = F) +
#   geom_point(aes(y= pct.area), size = 2, alpha = 0.4) +
#   geom_line(aes(y= pct.area), alpha = 0.4) +
#   geom_smooth(method = 'lm', 
#               formula = y ~ splines::bs(x, df = 4, knots = 200),
#               inherit.aes = F, 
#               aes(x = psi, y= pct.area), 
#               color = '#555555') 
#   #geom_point(aes(y= mean_pct_area), size = 3, colour = "black") +
#   #geom_line(aes(y= mean_pct_area), colour = "black")
# 
# # absolute sizez
# ggplot(data = larvae.df, 
#        aes(x= psi,
#            group = interaction(larva, sac),
#            colour = larva), na.rm = F) +
#   geom_smooth(method = 'lm', 
#               formula = y ~ splines::bs(x, df = 4, knots = 200),
#               inherit.aes = F, 
#               aes(x = psi, y= Area), 
#               color = '#555555') +
#   geom_point(aes(y= Area)) +
#   geom_line(aes(y= Area))
# 
# # difference from 0 psi
# ggplot(data = larvae.df, 
#        aes(x= psi,
#            group = interaction(larva, sac),
#            colour = larva), na.rm = F) +
# #  geom_point(aes(y= area_set_0psi, size = 2)) +
#   geom_point(aes(y= area_set_0psi)) +
#   geom_line(aes(y= area_set_0psi))





###################################################################################

## combine the three pH crushes
setwd("../../data frames by pH")
pH_crush_files <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(pH_crush_files)

########  dfs NAMED INDIVIDUALLY FOR EACH LOCATION   ######## 
pH_crushes.Kenya <- read_delim(pH_crush_files, ###  rename for location   ###
                         delim = ",",
                         id = NULL)

pH_crushes.Kenya <- pH_crushes.Kenya %>%  ###  rename for location   ###
  ungroup() %>%
  group_by(pH, psi) %>%
  mutate(mean_area_by_pH = mean(Area)) %>%
  mutate(mean_set0 = mean(area_set_0psi)) %>% # mean for all sacs at a psi
  mutate(species = NULL)

head(pH_crushes.Kenya)

# Integrate Kenya species
# bring in manual species column
Kenya_species <- read_csv("~/student_documents/UBC/Research/Malawi\\data/Kenya_to_ID.csv") %>%
  select(species)

head(Kenya_species) # check
tail(Kenya_species)

pH_crushes.Kenya$species <- Kenya_species$species # tac on species column

pH_crushes.Kenya <- relocate(pH_crushes.Kenya, species, .before = kpa) # move species column to same position as other dfs
  

(200*1000)/(FWD*g)


#####################################################################################
##  combine

pH_crushes <- rbind(pH_crushes.BC, pH_crushes.Kenya, pH_crushes.Malawi) %>%
  mutate(depth.m = (kpa*1000)/(FWD*g)) %>%
  ungroup()

head(pH_crushes)
tail(pH_crushes)


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


