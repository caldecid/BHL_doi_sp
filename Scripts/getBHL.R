#-------------------------------------------------------------------------------
#### Get BHL and DOI links from IPNI ####
#
# Author: Carlos Calderon & Domingos Cardoso
# Modified: Thu Jul 11 17:38:28 2024
# 
#-------------------------------------------------------------------------------

# This function obtains the BHL and DOI links from Royal Botanic Gardens, Kew's 
# International Plant Names Index (IPNI)
# https://www.ipni.org


#-------------------------------------------------------------------------------
#### Load libraries ####
#-------------------------------------------------------------------------------

library(devtools)
# devtools::install_github("barnabywalker/kewr")
library(kewr)
library(tidyverse)
library(readxl)
library(plyr)
library(stringr)
library(dplyr)
library(tibble)


#-------------------------------------------------------------------------------
#### Load main function getBHL and associated auxiliary function ####
#-------------------------------------------------------------------------------

getBHL <- function(df){
  
  # Completing dataframe information (auxiliary function)
  df = complete_df(df = df)
  
  df <- df %>% dplyr::rename(id_FFB = ID)  %>%
    select("id_FFB", "Group", "Family", "Genus", "Species", "taxon_name", "Author") %>%
    tibble::add_column(reference = NA,
                       publication = NA,
                       publicationYear = NA,
                       bhlLink = NA,
                       remarks = NA,
                       doi = NA,
                       id_IPNI = NA,
                       url_IPNI = NA)

# Loop for finding missing species in the Flora de Brasil
  
    for (i in seq_along(df$taxon_name)) {
      
      # Pause for 300 seconds right after every 500th search,
      # because IPNI website may crash when searching uninterruptedly.
      if (i %% 500 == 0) {
        Sys.sleep(300)
      }
      
    # tryCatch for handling the missing families in IPNI
    tryCatch({
      ##calling the species within families in IPNI
      ipni_df = tidy(kewr::search_ipni(df$taxon_name[i],
                                 filters = c("species")))
     
    }, error = function(e){
      message("Absent species in IPNI")
      print(e)
    })
    
      if (nrow(ipni_df) > 1) {
        au <- paste(ipni_df$name, ipni_df$authors)
        tf <- au %in% paste(df$taxon_name[i], df$Author[i])
        ipni_df = ipni_df[tf, ]
      }
      
      if (nrow(ipni_df) == 1) {
        
        if (any(names(ipni_df) %in% "reference")) {
        df$reference[i] = ipni_df$reference
        }
        if (any(names(ipni_df) %in% "publication")) {
        df$publication[i] = ipni_df$publication
        }
        if (any(names(ipni_df) %in% "publicationYear")) {
        df$publicationYear[i] = ipni_df$publicationYear
        }
        if (any(names(ipni_df) %in% "bhlLink")) {
          df$bhlLink[i] = ipni_df$bhlLink
        }
        if (any(names(ipni_df) %in% "remarks")) {
          df$remarks[i] = ipni_df$remarks
        }
        df$id_IPNI[i] = ipni_df$id
        
        message("BHL and Reference data retrieved from IPNI ", i, "/", length(df$taxon_name))
        
      }
      
  }
  
  # Obtaining the IPNI url
  
  df <- df %>%
    dplyr::mutate(url_IPNI = paste0("www.ipni.org/n/", id_IPNI))
  
  ##obtaining the doi
  df$doi <- ifelse(stringr::str_detect(df$remarks,
                                               "doi:[^\\s]+"), 
                           paste0("https://doi.org/",
                                  stringr::str_extract(df$remarks,
                                                       "(?<=doi:)[^\\s]+")), NA)
  df$url_IPNI[grepl("NA$", df$url_IPNI)] = NA                           

  return(df)

}


#-------------------------------------------------------------------------------
# Auxiliary function

complete_df <- function(df){
  ##checking and changing if column names are in Portuguese
  if (any(colnames(df) %in% "Espécie")) {
    colnames(df) <- c("ID", "Rank", "Group", "Class", "Division",
                      "Order", "Family", "Subfamily", "Tribe", "Genus",
                      "Species", "Subspecies", "Variety", "Form", "Author",
                      "Registry", "Status", "Qualifier", "Original work",
                      "Origin", "Endemic", "Occurs in Brazil", "Vouchers", 
                      "Reference", "Life Form", "Substrate", "Plant hosts", 
                      "Animals hosts", "Region Distribution", "State",                    
                      "Hydrographic Distribution", "Environment",              
                      "Phytogeographic Domains", "Vegetation Types",         
                      "has as a synonym", "Is a synonym", "Vernacular name", 
                      "Bibliographic", "Authorship", "Last Update",              
                      "User's Last Updated", "How To Cite")
  }
  
  # Manipulating dataframe
  df <- df %>% fill(Family, Genus) %>% 
    drop_na(Species) %>% 
    unite("taxon_name",
          Genus:Species,
          sep = " ", remove = FALSE)
  
  df = df[!df$Qualifier %in% c("Orthographical variant"), ]
  
  return(df)
}



#-------------------------------------------------------------------------------
#### Load data, run function and save data ####
#-------------------------------------------------------------------------------

# Angiosperms ------------------------------------------------------------------
angios <- readxl::read_excel("Data/Raw/datasheet_angiosperms_en.xlsx")
df_BHL_angios <- getBHL(df = angios)
write.csv(df_BHL_angios, "Data/Processed_/angiosperms_BHL.csv")

# Gymnosperms ------------------------------------------------------------------
gymnos <- readxl::read_excel("Data/Raw/datasheet_gymnosperms_en.xlsx")
df_BHL_gimnos <- getBHL(df = gymnos)
write.csv(df_BHL_gimnos, "Data/Processed_/gymnosperms_BHL.csv")

# Ferns ----------------------------------_____---------------------------------
ferns <- readxl::read_excel("Data/Raw/datasheet_ferns_en.xlsx")
df_BHL_ferns <- getBHL(df = ferns)
write.csv(df_BHL_ferns, "Data/Processed_/ferns_BHL.csv")

