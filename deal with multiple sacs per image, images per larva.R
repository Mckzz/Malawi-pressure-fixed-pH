library(tidyverse)
library(ggplot2)
library(readr)


# setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")
# 
# 
# setwd("./sacs pH 6 then pressure")
# #setwd("./area results, thresh calc")

setwd("~/student_documents/UBC/Research/Malawi\\americanus comparison\\pH8")


#########       for multi sac images: make sac and type columns manually
larva_8 <- read_csv(
  "larva 8.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac) %>% # so the psi x axis can be repeated over it in the other script
  group_by(sac) %>%
  mutate(pH = 8) %>%
  mutate(psi = c(0, 0, 10, 20,
                 seq(25, length(sac) * 5, by = 5))) %>% # FOR AMERICANUS x axis changing sequence
  print() %>%
  write_csv("./area results, thresh calc/larva 8.csv") # send combined csv to folder where main script reads from

# psi_scale <- c(seq(0, ((length(larva_3$sac) + 2) *5), by = 5))
# view(psi_scale)

# send combined csv to folder where main script reads from
# write_csv(larva_1,
#           "./area results, thresh calc/larva 1.csv")


########     when larva is over two image sets     #######

# bring all sacs from one larva into a single df, arranged by sac 
# so that the psi x axis can still be created along with the larger df
#########       after manually assigning sac number and type       #############
larva_10a <- read_csv("larva 10a.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_10a)

larva_10b <- read_csv("larva 10b.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_10b)

# combine to one df for larva
larva_10 <- rbind(larva_10a, larva_10b) %>%
  group_by(sac) %>%
  mutate(pH = 8) %>%
  mutate(psi = c(0, 0, 10, 20,
                 seq(25, length(sac) * 5, by = 5))) %>% # FOR AMERICANUS x axis changing sequence
  print()


#rm(larva_8)


# send combined csv to folder where main script reads from
write_csv(larva_10,
           "./area results, thresh calc/larva 10.csv")
