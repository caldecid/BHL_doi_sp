
# missing species in FB and present in IPNI and POWO ----------------------

##libraries
devtools::install_github("barnabywalker/kewr")
library(kewr)
library(tidyverse)
library(readr)
library(readxl)
library(plyr)
library(writexl)



# Angiosperms -------------------------------------------------------------


##calling dataset
angio <- read_excel("Data/Raw/datasheet_angiosperms_en.xlsx")

##running function of missing species in Flora de Brasil
df_BHL <- BHL_function(df = angio)

##replacing Leguminosae by Fabaceae
df_BHL <- df_BHL %>% mutate(family = case_when(family == "Leguminosae" ~ "Fabaceae", 
                                       .default = as.character(family)))
##family present
family_x <- sort(unique(df_BHL$family))

##list for each family
list_present <- vector("list", length = length(family_x))

##recovering ID
angio_id <- angio %>% select(ID, Family) %>% drop_na %>%
                    filter(Family %in% family_x)

##for loop for generating the list and the csv file for each family 
for(i in seq_along(family_x)){
  
  list_present[[i]] <- df_BHL %>% filter(family == family_x[i])
  
  write_csv(list_present[[i]],
  file =paste0("Data/Processed/Angiosperms/Family/",
                     family_x[i], "_FB",
                 angio_id$ID[i], ".csv"))
}


# Gymnosperms ---------------------------------------------------------------

gymnosperms <- read_excel("Data/Raw/datasheet_gymnosperms_en.xlsx")

x_gymno <- BHL_function(df = gymnosperms)

##family present
family_gymno <- sort(unique(x_gymno$family))

##list for each family
list_gymno <- vector("list", length = length(family_gymno))

##recovering ID
gymno_id <- gymnosperms %>% select(ID, Family) %>% drop_na %>%
  filter(Family %in% family_gymno)

##for loop for generating the list and the csv file for each family 
for(i in seq_along(family_gymno)){
  
  list_gymno[[i]] <- x_gymno %>% filter(family == family_gymno[i])
  
  write_csv(list_gymno[[i]],
            file =paste0("Data/Processed/Gymnosperms/Family/",
                         family_gymno[i], "_FB",
                         gymno_id$ID[i], ".csv"))
}


# Ferns -------------------------------------------------------------------
ferns <- read_excel("Data/Raw/datasheet_ferns_en.xlsx")

x_ferns <- BHL_function(df = ferns)

##family present
family_ferns <- sort(unique(x_ferns$family))

##list for each family
list_ferns <- vector("list", length = length(family_ferns))

##recovering ID
ferns_id <- ferns %>% select(ID, Family) %>% drop_na %>%
  filter(Family %in% family_ferns)

##for loop for generating the list and the csv file for each family 
for(i in seq_along(family_ferns)){
  
  list_ferns[[i]] <- x_ferns %>% filter(family == family_ferns[i])
  
  write_csv(list_ferns[[i]],
            file =paste0("Data/Processed/Ferns/Family/",
                         family_ferns[i], "_FB",
                         ferns_id$ID[i], ".csv"))
}

