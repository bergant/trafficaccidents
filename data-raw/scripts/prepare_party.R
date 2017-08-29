
library(tidyverse)

#source("data-raw/scripts/prepare_codetables.R")

prepare_party <- function() {



  # source_col_names:

  # Številka zadeve
  # Kot kaj nastopa oseba v PN
  # Starost osebe v času PN
  # Spol osebe
  # Državljanstvo osebe
  # Poskodba osebe
  # Vrsta udeleženca v PN
  # Uporaba varnostnega pasu ali čelade
  # Izkušenost voznika LL-MM
  # Vrednost alkotesta, če je bil opravljen
  # Vrednost stokovnega pregleda, če je bil opravljen

  dict <-
    list(
      event_id = "integer",
      accident_role = c(
        "POVZROČITELJ" = "At fault",
        "UDELEŽENEC" = "Participant"
      ),
      age = "integer",
      gender = c(
        "MOŠKI" = "Male",
        "ŽENSKI" = "Female"
      ),
      country = NA,
      p_injury = c(
        "BREZ POŠKODBE" = "No injury",
        "BREZ POŠKODBE-UZ" = "No injury",
        "LAŽJA TELESNA POŠKODBA" = "Minor",
        "HUDA TELESNA POŠKODBA" = "Severe",
        "SMRT" = "Fatal"
      ),
      traffic_role = c(
        "KOLESAR" = "Cyclist",
        "ODGOVORNA OSEBA" = "Person responsible",
        "OSTALO" = "Other",
        "PEŠEC" = "Pedestrian",
        "POSAMEZNIK, S.P., KI SAMOSTOJNO OPRAVLJA DEJAVNOST IN ZAPOSLUJE DRUGE" = "Individual",
        "POTNIK" = "Passenger",
        "PRAVNA OSEBA" = "Legal person",
        "SAMOSTOJNI PODJETNIK" = "Entrepreneur",
        "SKRBNIK MLADOLETNIKA" = "Custodian",
        "VOZNIK AVTOBUSA" = "Bus driver",
        "VOZNIK DELOVNEGA STROJA" = "Machine driver",
        "VOZNIK KOLESA Z MOTORJEM" = "Motorized bicycle driver",
        "VOZNIK KOMBINIRANEGA VOZILA" = "Van driver",
        "VOZNIK LAHKEGA ŠTIRIKOLESA" = "Light quadricycle driver",
        "VOZNIK MOPEDA" = "Light motorcycle driver",
        "VOZNIK MOTORNEGA KOLESA" = "Motorcycle driver",
        "VOZNIK OSEBNEGA AVTOMOBILA" = "Car driver",
        "VOZNIK SPECIALNEGA VOZILA" = "Special vehicle driver",
        "VOZNIK ŠTIRIKOLESA" = "Quadricycle driver",
        "VOZNIK TOVORNEGA VOZILA" = "Truck driver",
        "VOZNIK TRAKTORJA" = "Tractor driver",
        "VOZNIK TRIKOLESA" = "Three-wheeled vehicle driver",
        "X-KRŠITELJ - JRM" = "X - At fault"
      ),
      seat_belt_or_helmet = c(
        "DA" = "Yes",
        "NE" = "No"
      ),
      experience = "character",
      alcotest = "numeric",
      alco_exam = "numeric"
    )
  col_names <- names(dict)
  col_spec = sapply(dict, function(x){ ifelse(length(x) > 1, NA, x)})

  dat_dirs <-
    list.dirs("data-raw/source_files", full.names = TRUE, recursive = FALSE)

  dat_dirs <- head(dat_dirs, -1)


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

  # convert to factors
  code_table_factors <- names(dict[lengths(dict)>1])
  for(col_name in code_table_factors) {
    party[col_name] <- dict[[col_name]][as.character(party[[col_name]])]
    party[col_name] <- factor(party[[col_name]], levels = unique(dict[[col_name]]))
  }
  other_factors <- setdiff(names(col_spec[is.na(col_spec)]), code_table_factors)
  party[-1][other_factors] <- lapply(party[-1][other_factors], as.factor)

  party$country[party$country==""] <- NA
  party$country[party$country=="NI VNEŠENO"] <- NA
  party$country[party$country=="NEZNANO"] <- NA


  summary(party)


#  View(read.delim("data-raw/source_files/2016/PNL-OSEBE-2016.TXT", sep = "$", nrows = 2))

  # separate experience
  party <-
    party %>%
    separate(experience, into = c("experience_y", "experience_m"), convert = TRUE)

  # country code
  trans_country <-
    read.table("data-raw/code_tables/trans_country.csv",
               stringsAsFactors = FALSE,
               header = TRUE, quote = '"', sep = ",",
               fileEncoding = "UTF-8")

  party <-
    party %>%
    mutate(country = as.character(country)) %>%
    left_join(trans_country, by = c("country" = "slo_name")) %>%
    mutate(
      country = Name,
      country_iso = Alpha_2,
      Name = NULL,
      Alpha_2 = NULL,
      ID = NULL
    ) %>%
    mutate(
      country = factor(country, levels = unique(trans_country$Name)),
      country_iso = factor(country_iso, levels = unique(trans_country$Alpha_2))
    )

  #party$p_district <- NA
  party
}

party <- prepare_party()



