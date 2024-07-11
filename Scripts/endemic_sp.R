
# Looking for endemic sp in FFB -------------------------------------------


##libraries
library(tidyverse)
library(readr)
library(readxl)
library(plyr)
library(writexl)



# Angiosperms -------------------------------------------------------------


##calling dataset
angio <- read_excel("Data/Raw/datasheet_angiosperms_en.xlsx")


#########Endemic species

angio_endemic <- complete_df(df = angio) %>%
                      filter(Endemic == "Is endemic from Brazil")

##saving

write_xlsx(angio_endemic,
            path = "Data/Processed/Endemic/angiosperms_endemic_FFB.xlsx")

# Gymnosperms ---------------------------------------------------------------

gymnosperms <- read_excel("Data/Raw/datasheet_gymnosperms_en.xlsx")


gymno_endemic <- complete_df(df = gymnosperms) %>%
                  filter(Endemic == "Is endemic from Brazil")

write_xlsx(gymno_endemic,
           path = "Data/Processed/Endemic/gymnosperms_endemic_FFB.xlsx")

# Ferns -------------------------------------------------------------------
ferns <- read_excel("Data/Raw/datasheet_ferns_en.xlsx")

ferns_endemic <- complete_df(df = ferns) %>%
               filter(Endemic == "Is endemic from Brazil")

write_xlsx(ferns_endemic,
           path = "Data/Processed/Endemic/fernss_endemic_FFB.xlsx")
