#'detect empty columns
#'@noRd
empty_columns <- function(.data){
  #return true if all is empty
  !(all(is.na(.data))|all(.data == ""))
}

#'Filtering empty columns
#'
#'Filtering on empty columns
#'
#'@noRd
filter_empty_columns <- function(.data){
  #that its
  Filter(empty_columns, .data)
}

#'Split a character to vector

#'@noRd
split_to_vector <- function(.character){
  stringr::str_split(.character, "\\s", simplify = TRUE) %>%
    as.vector()
}

#'Creating the header of the final data-frame
#'
#'@noRd
add_borno_columns <- function(.data, reg_exp = "location|cas|disease|year|week|annex"){
  colnames(.data) <- internal_data$headers

  #filtering on unwanted lines
  .data[!grepl(reg_exp, .data$location, ignore.case = TRUE), ]
}

#'A little function to check if a file is a pdf and return a stop otherwise
#'
#'@param .file_path the path to the pdf file
#'@export
check_pdfs <- function(.file_path){
  if(!all(grepl("\\.pdf$", .file_path))){
    stop("A non pdf file found")
  }
}

#' Extracting the metadata from one pdf file.
#'
#'@param .file_path A character vector of length one, indicating the path to a pdf file
#'@return A tibble
#'@importFrom magrittr %>%
#'@export
#'@family Extracting functions
create_metadata <- function(.file_path){
  #reading the table
  raw_text <- pdftools::pdf_text(.file_path)

  metadata <- stringr::str_extract(raw_text, internal_data$week_expression) %>%
    filter_empty_columns() %>%
    stringr::str_squish() %>%
    stringr::str_split("\\s", simplify = TRUE) %>%
    as.vector()

  #removing the from and to.
  #The first element is the week, the second one is the from and the third
  #one is the path
  metadata <- metadata[!grepl("from|to", metadata, ignore.case = TRUE)]

  #final metadata
  dplyr::tibble(path = .file_path, week = stringr::str_remove(metadata[1], "W"),
                 from = metadata[2], to = metadata[3], year = metadata[3]) %>%
    dplyr::mutate_at("year", function(x) stringr::str_extract(x, "\\d{4}") %>% as.numeric()) %>%
    dplyr::mutate_at("week", as.numeric)
}



#' Converting one PDF text into tibble format
#'
#' @param .file_path A character vector of length one. The path to a pdf file
#' @return A tibble
#' @importFrom magrittr %>%
#' @export
#' @family  Extracting functions
convert_file <- function(.file_path){
  #read with pdftools
  table_test <- pdftools::pdf_text(.file_path)
  #split a each line
  table_test <- stringr::str_split(table_test, pattern = "\\n")
  #At this step, I have two list and only the firs element contains interesting
  #stuffs
  table_test <- table_test[[1L]]
  #removing repetiting splaces
  table_test <- purrr::map(table_test, stringr::str_squish)
  #split each line to a vector
  table_test <- purrr::map(table_test, split_to_vector)
  # creating the final table
  table_test %>%
    filter_empty_columns() %>%
    plyr::ldply(rbind) %>%
    #adding final columns
    add_borno_columns() %>%
    dplyr::mutate(path = .file_path) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::as_tibble()
}


#'Add the population Data to the final data
#'
#'@param .data A tibble
#'@param pop_data A tibble with two columns (`location` and `population`). See the
#'    `borno_pop` format.
#'@return A tibble
#'@importFrom magrittr %>%
#'@export
#'@family add functions
add_population <- function(.data, pop_data = borno_pop){

  #checking the colnames of pupulation data
  if(any(colnames(pop_data) != c("location", "population"))){
    stop('colnames of population data shoud be: c("location", "population")')
  }

  .data  %<>%
    #filtering the location not wanted
    dplyr::filter_at(dplyr::vars("location"),
                     dplyr::any_vars(. != "Uba" & . != "Kusar")
                     )%>%
    #correcting some lga names to fit the population data
    dplyr::mutate_at("location", function(x) gsub("Askira", "Askira-Uba", x, fixed = TRUE)) %>%
    dplyr::mutate_at("location", function(x) gsub("Kwaya", "Kwaya-Kusar", x, fixed = TRUE)) %>%
    dplyr::mutate_at("location", function(x) gsub("/", "-", x)) %>%
    #joining
    dplyr::left_join(pop_data, by = "location")
}


#'Selecting a portion of the data and turn it to long format using my scheme
#'@noRd
#'@importFrom rlang !!!
#'@importFrom magrittr %>%
select_portions <- function(.data, select = internal_data$selecting_vector, .part = "cases"){

  #defining quosures for dealing with names in the function
  to_select <- rlang::enquos(select)
  value_name <- paste("number", .part, sep = "_")

  .data %>%
    dplyr::select(!!!to_select, dplyr::ends_with(.part)) %>%
    tidyr::gather(key = "events", value = !!value_name,
           dplyr::ends_with(.part)) %>%
    dplyr::mutate_at("events",
                     function(x) stringr::str_remove(x, paste("(_", .part, ")$", sep = ""))
                     )
}

#'Converting data to long format
#'
#'@param .data A tibble
#'@return A tibble
#'@importFrom magrittr %>%
#'@export
#'@family Extracting Functions
convert_to_long <- function(.data){

  #number of cases
  nb_cases <- .data %>% select_portions()
  #number of deaths
  nb_deaths <- .data %>% select_portions(.part = "death")

  #joining
  dplyr::left_join(nb_cases, nb_deaths, by = internal_data$joining_vector)

}

#'Adding metadata to raw_data

#'@param .data A tibble
#'@param .metadata A tibble, metadata of the file (s)
#'@importFrom magrittr %>%
#'@export
#'@family add functions
add_metadata <- function(.data, .metadata){
  #adding the metadata
  .data %>%
    #left_join with metadata
    dplyr::left_join(.metadata, by = "path") %>%
    dplyr::select(-"path") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("cases")), as.numeric) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("death")), as.numeric)

}


#'Converting all pdfs withing a folder to a data.frame
#'
#'@param .folder_path A character vector of length one, path to the folder of
#'files.
#'@param keepwide Logical. Should we keep final wide or long format?
#'@return A tibble
#'@importFrom magrittr %>%
#'@importFrom magrittr %<>%
#'@export
#'@family Extracting functions
convert_files <- function(.folder_path, keepwide = FALSE){
  #listing all the pdf files in the folder
  pdf_files <- list.files(.folder_path, full.names = TRUE)

  #check if all file names are pdfs.
  purrr::map(pdf_files, check_pdfs)

  #creating the metadata
  metadata <- purrr::map_dfr(pdf_files, create_metadata)

  #creating the tables from the raw files
  raw_table <- purrr::map_dfr(pdf_files, convert_file)

  #adding the metadata
  raw_table  %<>% add_metadata(metadata)

  #adding the population_data
  raw_table  %<>% add_population()

  #converting to long format and adding some variables
  if(keepwide) return(raw_table)

  #the user can decide whether to keep the wide format or not
  raw_table  %>% convert_to_long()



}

#'Update historic data using one file
#'
#'@param .file_path A character vector of length One, path to one pdf file
#'@param keepwide Logical Should we keep final long or wide format?
#'@param update Logical. Update with historical data or not?
#'@return A tibble with updated data
#'@importFrom magrittr %>%
#'@importFrom magrittr  %<>%
#'@export
#'@family update functions
file_update <- function(.file_path, keepwide = FALSE, update = TRUE){

  #be sure to have a pdf
  check_pdfs(.file_path)

  #creating the raw_table
  raw_table <- convert_file(.file_path)

  #creating the metadata
  raw_table  %<>%  add_metadata(create_metadata(.file_path))

  #adding the population
  raw_table  %<>% add_population()

  #converting to long format and adding some variables such as attack rate and
  #cfr

  #the user can decide whether to keep the wide format or not
  if(!keepwide) raw_table   %<>%  convert_to_long()

  #Now I will check if the user wants raw_tabe without updating or not
  if(!update) return(raw_table)

  #returning the raw historic data with updated data
  if(keepwide) return(historic_wide_data %>% dplyr::bind_rows(raw_table) %>% dplyr::distinct())

  #the default behaviour is to update long data and return it
  historic_long_data %>% dplyr::bind_rows(raw_table) %>% dplyr::distinct()

}

#'Update historic data using a folder
#'
#'@param .folder_path A character vector of length one, path to the foler with files
#'@param keepwide logical. Should we keep final long or wide format?
#'@param update Logical. Update with historical data or not?
#'@return A tibble with updated data
#'@importFrom magrittr %>%
#'@importFrom magrittr  %<>%
#'@family update functions
#'@export
folder_update <- function(.folder_path, keepwide = FALSE, update = TRUE){

  new_data <- convert_files(.folder_path, keepwide = keepwide)
  #make update or not
  if(!update) return(new_data)
  #keep wide format or not
  if(keepwide) return(historic_wide_data %>% dplyr::bind_rows(new_data) %>% dplyr::distinct())
  #default behaviour
  historic_long_data %>% dplyr::bind_rows(new_data) %>% dplyr::distinct()

}