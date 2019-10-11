`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

#diseases header
diseases <- rep(c("meningitis", "cholera", "vhf1", "measles", "yellow fever",
                  "guinea worm", "human influenza", "any pheic"), each = 2)
#combining with cases and deaths
cases <- rep(c("cases", "death"), length(unique(diseases)))
diseases <- paste(diseases, cases, sep = "_")
#final headers
headers <- c(c("location", "afp_cases"), diseases)

#this will be the headers of all my data-tables

week_expression <- stringr::regex("w\\d+\\s+from:\\s+\\d{4}-\\d{2}-\\d{2}\\s+to:\\s+\\d{4}-\\d{2}-\\d{2}",
                         ignore_case = TRUE)

#population data coming from humdata
nga_pop <- read.csv("./data-raw/nga_pop_adm2_2016.csv")
#borno 2016 data pop
borno_pop <- nga_pop %>%
  dplyr::filter(admin1Name == "Borno")

#correcting the names to fit my desired names
borno_pop  %<>%
  dplyr::select(admin2Name, Pop2016) %>%
  dplyr::rename(location = admin2Name,
         population = Pop2016) %>%
  dplyr::mutate(location = gsub("\\/|\\s", "-", location))

attributes(borno_pop)$label <- "Population Data From OCHA, 2016 estimate"

#data population on borno
use_data(borno_pop, overwrite = TRUE)

#joining_vector is the vector for joining data

joining_vector <- c("location", "week", "year", "from", "to", "events", "population")


#Selecting vector is the joining vector without the events.
selecting_vector <- setdiff(joining_vector, "events")

internal_data <- list(headers = headers,
                      joining_vector = joining_vector,
                      week_expression = week_expression,
                      selecting_vector = selecting_vector)

use_data(internal_data, internal = TRUE, overwrite = TRUE)

