library(tidyverse)
library(ggplot2)
library(readr)


setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")


setwd("./sacs pH 6 then pressure")
setwd("./area results, thresh calc")

#########       for multi sac images: make sac and type columns manually
larva_5 <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/larva 5 raw.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac) # so the psi x axis can be repeated over it in the other script
print(larva_5)

# send combined csv to folder where main script reads from
write_csv(larva_5,
          "~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/area results, thresh calc/larva 5.csv")


########     when larva is over two image sets     #######

# bring all sacs from one larva into a single df, arranged by sac 
# so that the psi x axis can still be created along with the larger df
#########       after manually assigning sac number and type       #############
larva_3a <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/3/larva 3a (still needs sacs separated, data wrangle).csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_3b)

larva_3b <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/3/larva 3b.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_3b)

# combine to one df for larva
larva_3 <- rbind(larva_3a, larva_3b)
print(larva_3)

# send combined csv to folder where main script reads from
write_csv(larva_3,
           "~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/area results, thresh calc/larva 3.csv")
