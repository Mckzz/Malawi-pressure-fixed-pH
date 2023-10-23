



setwd("~/student_documents/UBC/Research/Malawi/data/sac pressure, fixed pH/data frames by pH")



list_of_files <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) #leave out folder path
print(list_of_files)
#rm(list_of_results)

# read in files, modify text in larva column, add psi column for x axis
combined <- read_delim(list_of_files,
                        delim = ",",
                        id = "file")
print(combined)

ggplot(data = combined,
       aes(x = psi,
           group = as.factor(pH),
           colour = as.factor(pH))) +
  geom_point(aes(y= mean_pct_area), size = 2) +
  geom_line(aes(y= mean_pct_area)) +
  geom_errorbar(mapping = aes(x = psi, 
                              ymin = mean_pct_area - sd_pct_area, 
                              ymax = mean_pct_area + sd_pct_area), 
                width = 10)



