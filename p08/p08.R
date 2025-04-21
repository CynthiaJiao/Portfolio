
# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)


# set url ----------------------------------------------------------------------

first_url <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"

# read page --------------------------------------------------------------

page <- read_html(first_url)

# scrape PAC name ----------------------------------------------------------------

PAC_name <- page %>%
  html_nodes(".color-category") %>% ## this is retrieved using selector Gadget
  html_text()

PAC_name

# scrape country of origin/parent company -----------------------------------------------------------------

country_company <- page %>%
  html_elements("table tbody tr") %>%  # select all rows
  html_elements(xpath = "./td[2]") %>% # select every second column in each row
  html_text(trim = TRUE)

country_company

# scrape total ---------------------------------------------------------------

total <- page %>%
  html_nodes(".number:nth-child(3)") %>%
  html_text()

total

# scrape dems ---------------------------------------------------------------

dems <- page %>%
  html_nodes(".number:nth-child(4)") %>%
  html_text()

dems

# scrape repubs ---------------------------------------------------------------

repubs <- page %>%
  html_nodes(".number~ .number+ .number") %>%
  html_text()

repubs

# put together in a data frame -------------------------------------------------

PAC_2020 <- tibble(
  PAC_names = PAC_name,
  'Country of Origin/Parent Company' = country_company,
  Total = total,
  Dems = dems,
  Repubs = repubs
)

PAC_2020

# add years so that we know which year this data is ----------------------------

PAC_2020 <- PAC_2020 %>%
  mutate(year = 2020)

## writing a function to scrape PAC data in a given year

scrape_pac <- function(year) {
  # Create URL from year
  url <- paste0("https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/", year)
  page <- read_html(url)
  
  PAC_name <- page %>%
    html_nodes(".color-category") %>%
    html_text()
  
  country_company <- page %>%
    html_elements("table tbody tr") %>%
    html_elements(xpath = "./td[2]") %>%
    html_text(trim = TRUE)
  
  total <- page %>%
    html_nodes(".number:nth-child(3)") %>%
    html_text()
  
  dems <- page %>%
    html_nodes(".number:nth-child(4)") %>%
    html_text()
  
  repubs <- page %>%
    html_nodes(".number~ .number+ .number") %>%
    html_text()
  
  pac <- tibble(
    PAC_names = PAC_name,
    'Country of Origin/Parent Company' = country_company,
    Total = total,
    Dems = dems,
    Repubs = repubs,
    year = year
  )
  
  return(pac)
}

## Example usage

PAC_2020 <- scrape_pac(2020)
PAC_2018 <- scrape_pac(2018)
PAC_2000 <- scrape_pac(2000)

## Define the years to scrape
years <- seq(2000, 2024, by = 2)

# Scrape all years and bind into one data frame
pac_all <- map_dfr(years, scrape_pac)
write_csv(pac_all, "PAC-all.csv")
