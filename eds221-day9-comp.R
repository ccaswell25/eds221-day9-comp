library(patchwork)
library(tidyverse)
library(palmerpenguins)
library(kableExtra)

#Warm-Up:
  
#Starting from penguins create a new subset where you
#only keep columns species, island, sex, body_mass_g, and flipper_length_mm
#filter to only include Chinstrap and Gentoos
#rename the species column to penguin_species
#create a summary table that contains the mean body mass, mean flipper length, and total count of obs each by species and sex

penguins2 <- penguins %>%
  select("species", "island", "sex", "body_mass_g", "flipper_length_mm") %>% 
  filter(species %in% c("Chinstrap","Gentoo")) %>% 
  rename("penguin_species" = species) %>% 
  group_by(penguin_species, sex) %>% 
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE), mean_flipper = mean(flipper_length_mm, na.rm = TRUE), count = n())

#A few variations on dplyr::across()
#Create a summary table of max values for all numeric variables in the penguins dataset 
penguins %>% 
  group_by(species) %>% 
  summarize(across(where(is.numeric), max, na.rm=TRUE))
#I am going to create a summary table for information grouped by species where is.numeric is true and return the maximum

#Calculate the mean value of all columns that start with "bill". We can use the starts_with and ends_with functions
penguins %>% 
  group_by(species) %>% 
  summarize(across(starts_with("bill"), mean, na.rm = TRUE))

#Write code that will return the minimum value of any columns that end with the chracter"mm", grouped by island and year
penguins %>% 
  group_by(island, year) %>% 
  summarize(across(ends_with("mm"), min, na.rm = TRUE))

#Find and return both the mean and standard deviation of all columns starting with "bill", grouped by penguin species, and returning a new name that combines the original column with the function 
penguins %>% 
  group_by(species) %>% 
  summarize(across(starts_with("bill"),
                   list(mean = mean, 
                        sd = sd),
                   .names = "{.col}_{.fn}"))
#The .names function allows us to reference columns and functions using the {.} format. We need to feed the summarize function a list since we are using multiple functions

#How do we make a nicer looking table? kable and kableExtra functions! Kable() adds lines and formatting
penguins_table <- penguins %>% 
  group_by(species, sex) %>% 
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE), sd_mass = sd(body_mass_g, na.rm = TRUE)) %>% 
  kable(col.names = c("Species", "Sex", "Mean body mass (g)", "SD body mass (g)")) %>% 
  kable_styling(full_widgth = FALSE)


#Let's use patchwork:
chart1 <- penguins %>% 
  ggplot(aes(x= body_mass_g, y = flipper_length_mm)) + geom_point()

chart2 <- penguins %>% 
  ggplot(aes(x=bill_length_mm)) + geom_histogram()

(chart1 + chart2 + chart1)/chart2
#this returns our charts side by side and above/below each other
#i can right click on the below figure and click inspect to get details on the chart:

#| fig-cap: Penguin body mass observations by species.
ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_jitter(width = .2, alpha = .5, color = "navy")
