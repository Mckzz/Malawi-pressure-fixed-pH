library(tidyverse)
library(ggplot2)
library(readr)

setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")


setwd("./sacs pH 6 then pressure")
setwd("./area results, thresh calc")


# the list of file names will provide the files for r to read in, as well as 
# the names for the indvd larva column
list_of_results <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
head(list_of_results, n = 10)

# read in files, modify text in larva column, add psi column for x axis
larvae.df <- read_delim(list_of_results,
                      delim = ",",
                      id = "larva") %>%
  mutate(" " = NULL) %>%
  mutate(larva = substr(larva, 1, nchar(larva)-4)) %>% #drop file extension
  group_by(larva, sac) %>%
  mutate(psi = (row_number() * 30) - 30) %>% # x axis starting from zero
  mutate(area_set_0psi = Area - Area[1]) %>% # still abs size, but starting from o psi
  mutate(pct.area = ((Area - Area[1]) / Area[1]) * 100)


print(larvae.df)

# absolute sizez
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
#  geom_point(aes(y= Area, size = 2)) +
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

# % change
ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= pct.area), size = 2) 
  #geom_point(aes(y= pct.area)) +
  #geom_line(aes(y= pct.area))

###########   looking at/ comparing individual larvae   ##########

larva_choose <- larvae.df %>%
  filter(larva == "larva 11" | larva == "larva 3")
print(larva_choose)

# absolute sizez
ggplot(data = larva_choose, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= Area, size = 1.5)) +
  geom_line(aes(y= Area))


# difference from 0 psi
ggplot(data = larva_choose, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= area_set_0psi)) +
  geom_line(aes(y= area_set_0psi))


###############################################################
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


