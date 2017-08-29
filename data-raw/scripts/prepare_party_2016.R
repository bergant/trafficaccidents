
library(tidyverse)
source("data-raw/scripts/prepare_codetables.R")

prepare_party_2016 <- function() {


  source_col_names <-
    c(
      "FIOStevilkaZadeve",
      "Povzrocitelj",
      "StarostVLetih",
      "Spol",
      "UEStalnegaPrebivalisca",
      "Drzavljanstvo",
      "PoskodbeUdelezenca",
      "VrstaUdelezenca",
      "UporabaVarnostnegaPasu",
      "VozniskiStazVLetih",
      "VozniskiStazVMesecih",
      "VrednostAlkotesta",
      "VrednostStrokovnegaPregleda"
    )


  dict <-
    list(
      event_id = "integer",
      accident_role = c(
        "1" = "At fault",
        "0" = "Participant"
      ),
      age = "integer",
      gender = c(
        "1" = "Male",
        "2" = "Female"
      ),
      p_district = "NULL",
      country = "integer",
      p_injury = c(
        "B" = "No injury",
        "U" = "No injury",
        "L" = "Minor",
        "H" = "Severe",
        "S" = "Fatal"
      ),
      traffic_role = c(
        "KO" = "Cyclist",
        "OD" = "Person responsible",
        "OS" = "Other",
        "PE" = "Pedestrian",
        "SD" = "Individual",
        "PT" = "Passenger",
        "PO" = "Legal person",
        "SP" = "Entrepreneur",
        "SM" = "Custodian",
        "AV" = "Bus driver",
        "DS" = "Machine driver",
        "KM" = "Motorized bicycle driver",
        "KV" = "Van driver",
        "LK" = "Light quadricycle driver",
        "MO" = "Light motorcycle driver",
        "MK" = "Motorcycle driver",
        "OA" = "Car driver",
        "SV" = "Special vehicle driver",
        "SK" = "Quadricycle driver",
        "TV" = "Truck driver",
        "TR" = "Tractor driver",
        "TK" = "Three-wheeled vehicle driver",
        "KR" = "X - At fault"
      ),
      seat_belt_or_helmet = c(
        "DA" = "Yes",
        "NE" = "No"
      ),
      experience_y = "integer",
      experience_m = "integer",
      alcotest = "numeric",
      alco_exam = "numeric"
    )


  #cbind(source_col_names, names(dict))

  col_names <- names(dict)
  col_spec = sapply(dict, function(x){ ifelse(length(x) > 1, NA, x)})

  dat_dirs <-
    list.dirs("data-raw/source_files", full.names = TRUE, recursive = FALSE)

  # only 2016
  dat_dirs <- tail(dat_dirs, 1)


  party <-
    dat_dirs %>%
    setNames(basename(dat_dirs)) %>%
    map(list.files, pattern = "PNL-OSEBE*.*", full.names = TRUE ) %>%
    compact() %>%
    purrr::keep(~length(.) > 0) %>%
    map(read.delim,
        sep = "$",
        col.names = col_names,
        colClasses = col_spec,
        stringsAsFactors = FALSE) %>%
    bind_rows(.id = "src_file") %>%
    as_tibble

  party$p_injury <- trimws(party$p_injury)
  party$traffic_role <- trimws(party$traffic_role)

  # convert to factors
  code_table_factors <- names(dict[lengths(dict)>1])
  for(col_name in code_table_factors) {
    party[col_name] <- dict[[col_name]][as.character(party[[col_name]])]
    party[col_name] <- factor(party[[col_name]], levels = unique(dict[[col_name]]))
  }
  other_factors <- setdiff(names(col_spec[is.na(col_spec)]), code_table_factors)
  party[-1][other_factors] <- lapply(party[-1][other_factors], as.factor)



  # country code table
  party$country[party$country==""] <- NA
  party$country[party$country=="NI VNEŠENO"] <- NA
  party$country[party$country=="NEZNANO"] <- NA

  trans_country <-
    read.table("data-raw/code_tables/trans_country.csv",
               stringsAsFactors = FALSE,
               header = TRUE, quote = '"', sep = ",",
               fileEncoding = "UTF-8")

  party <-
    party %>%
    #mutate(country = as.character(country)) %>%
    left_join(trans_country %>% filter(!is.na(ID)), by = c("country" = "ID")) %>%
    mutate(
      country = Name,
      country_iso = Alpha_2,
      Name = NULL,
      Alpha_2 = NULL,
      ID = NULL,
      slo_name = NULL
    ) %>%
    mutate(
      country = factor(country, levels = unique(trans_country$Name)),
      country_iso = factor(country_iso, levels = unique(trans_country$Alpha_2))
    )

  summary(party)

  party
}

party_2016 <- prepare_party_2016()
party <- bind_rows(party, party_2016)


# source("data-raw/scripts/prepare_codetables.R")
# districts <- get_code_table("LOOB", "data-raw/code_tables/")
# districts_levels <- head(districts$DESCRIPTION, -3)
# party <-
#   party %>%
#   left_join(districts, c(p_district = "ID")) %>%
#   mutate(
#     p_district = factor(DESCRIPTION, levels = districts_levels),
#     DESCRIPTION = NULL)


rm(party_2016)

