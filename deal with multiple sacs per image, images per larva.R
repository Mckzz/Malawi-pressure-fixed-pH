library(tidyverse)
library(ggplot2)
library(readr)


setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH")


setwd("./sacs pH 6 then pressure")
#setwd("./area results, thresh calc")


#########       for multi sac images: make sac and type columns manually
larva_12 <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/larva 12, raw.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac) # so the psi x axis can be repeated over it in the other script
print(larva_12)

# send combined csv to folder where main script reads from
write_csv(larva_12,
          "~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/area results, thresh calc/larva 12.csv")



########     when larva is over two image sets     #######

# bring all sacs from one larva into a single df, arranged by sac 
# so that the psi x axis can still be created along with the larger df
#########       after manually assigning sac number and type       #############
larva_8a <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/larva 8a, v2.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_8a)

larva_8b <- read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/larva 8b, v2.csv") %>%
  rename(" " = ...1) %>% #so that the output can be fed into the larger data frame
  arrange(sac)
print(larva_8b)

# combine to one df for larva
larva_8 <- rbind(larva_8a, larva_8b)
print(larva_8)

#rm(larva_8)


# send combined csv to folder where main script reads from
write_csv(larva_8,
           "~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/sacs pH 6 then pressure/area results, thresh calc/larva 8.csv")
