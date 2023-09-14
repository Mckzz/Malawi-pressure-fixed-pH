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
  mutate(psi = (row_number() *30) -30)

print(larvae.df)


ggplot(data = larvae.df, 
       aes(x= psi,
           group = interaction(larva, sac),
           colour = larva), na.rm = F) +
  geom_point(aes(y= Area)) +
  geom_line(aes(y= Area)) 






