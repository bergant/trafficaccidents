prepare_events <- function(source_data_path = "data-raw/source_files") {

  library(tidyverse)

  source_col_names <-
    c(
      "FIOStevilkaZadeve",
      "KlasifikacijaNesrece",
      "UpravnaEnotaStoritve",
      "DatumPN",
      "UraPN",
      "VNaselju",
      "VrstaCeste",
      "SifraCesteNaselja",
      "TekstCesteNaselja",
      "SifraOdsekaUlice",
      "TekstOdsekaUlice",
      "StacionazaDogodka",
      "Lokacija",
      "VzrokNesrece",
      "TipNesrece",
      "VremenskeOkoliscine",
      "StanjePrometa",
      "StanjeVozisca",
      "VrstaVozisca",
      "GeoKoordinata.X",
      "GeoKoordinata.Y"
    )

  setClass("slo_date")
  setAs("character","slo_date", function(from) as.Date(from, format="%d.%m.%Y") )

  dict <-
    list(
      event_id = "integer",
      injury = c(
        "B" = "No injury",
        "U" = "No injury",
        "L" = "Minor",
        "H" = "Severe",
        "S" = "Fatal"
      ),
      district= "NULL",
      date = "slo_date",
      hour = "integer",
      in_city = c(
        "0" = "Not in city",
        "1" = "In city"
      ),
      road_type = c(
        "0" = "Motorway",
        "H" = "Expressway",
        "1" = "Main road I",
        "2" = "Main road II",
        "3" = "Regional road I",
        "4" = "Regional road II",
        "5" = "Regional road III",
        "T" = "Regional tourist road",
        "L" = "Local road",
        "V" = "Street",
        "N" = "Street"
      ),
      location_id = NA,
      location = "NULL",
      road_section_id = NA,
      road_section = "NULL",
      pos_number = "integer",
      location_type = NA,
      cause = c(
        'HI' = "Unadjusted speed",
        'SV' = "Wrong way",
        'PD' = "Breaking priorty rules",
        'PR' = "Improper overtaking",
        'VR' = "Following distance",
        'PV' = "Vehicle moving",
        'NP' = "Pedestrian fault",
        'VO' = "Vehicle irregularities",
        'TO' = "Cargo irregularities",
        'CE' = "Road irregularity",
        'OS' = "Other"
      ),
      event_type = c(
        'ČT' = "Head-on collision" ,
        'BT' = "Side collision" ,
        'PR' = "Rollover" ,
        'NT' = "Rear-end collision" ,
        'OP' = "Sideswipe" ,
        'TO' = "Single car accident" ,
        'TV' = "Collision with parked vehicle",
        'PP' = "Pedestrian hit" ,
        'PZ' = "Animal overrun" ,
        'OS' = "Other"
      ),
      weather = c(
        'J' = "Clear",
        'O' = "Cloudy",
        'D' = "Rainy",
        'M' = "Foggy",
        'S' = "Snow",
        'T' = "Hail",
        'V' = "Windy",
        'N' = "Unknown"
      ),
      traffic = c(
        'N' = "Normal",
        'R' = "Light",
        'G' = "Heavy",
        'Z' = "Congestions",
        'E' = "Unknown"
      ),
      surface_conditions = c(
        'SU' = "Dry",
        'MO' = "Wet",
        'SP' = "Slippery",
        'SL' = "Snow - cleared",
        'SN' = "Snow - uncleared",
        'PN' = "Ice",
        'PP' = "De-iced",
        'BL' = "Muddy",
        'OS' = "Other"
      ),
      surface = c(
        'AH' = "Rough asphalt",
        'AN' = "Uneven asphalt",
        'AZ' = "Sleepery asphalt",
        'MA' = "Macadam",
        'OS' = "Other"
      ),
      pos_x = "integer",
      pos_y = "integer"
    )

  col_names <- names(dict)
  col_spec = sapply(dict, function(x){
    ifelse(length(x) > 1, NA, x)
  })

  cbind(
    source_col_names,
    names(dict),
    dict
  )


  # read files
  dat_dirs <-
    list.dirs(source_data_path, full.names = TRUE, recursive = FALSE)


  event <-
    dat_dirs %>%
    map(list.files, pattern = "PNL\\.DOGODKI*.*", full.names = TRUE ) %>%
    setNames(basename(dat_dirs)) %>%
    purrr::keep(~length(.) > 0) %>%
    map(read.delim,
        sep = "$",
        col.names = col_names,
        colClasses = col_spec,
        stringsAsFactors = FALSE) %>%
    bind_rows(.id = "src_file") %>%
    arrange(src_file, date) %>%
    as_tibble

  # errors in source data (spaces)
  event <-
    event %>%
    filter(nchar(as.character(injury)) == 1)

  # convert to factors
  code_table_factors <- names(dict[lengths(dict)>1])
  for(col_name in code_table_factors) {
    event[col_name] <- dict[[col_name]][as.character(event[[col_name]])]
    event[col_name] <- factor(event[[col_name]], levels = unique(dict[[col_name]]))
  }
  other_factors <- setdiff(names(col_spec[is.na(col_spec)]), code_table_factors)
  event[-1][other_factors] <- lapply(event[-1][other_factors], as.factor)

  # convert dates
  #event[, "date"] <- as.Date(event$date, format = "%d.%m.%Y")

  # set NA for positions == 0
  event[event$pos_x == 0, c("pos_x", "pos_y")] <- c(NA, NA)

  # #### apply districts code table
  # source("data-raw/scripts/prepare_codetables.R")
  # districts <- get_code_table("LOOB", "data-raw/code_tables/")
  # districts_levels <- head(districts$DESCRIPTION, -3)
  # event <-
  #   event %>%
  #   left_join(districts, c(district = "ID")) %>%
  #   mutate(
  #     district = factor(DESCRIPTION, levels = districts_levels),
  #     DESCRIPTION = NULL)

  event
}


# add WGS84 coordinates (GPS lon, lat)
convert_coordinates <- function() {

  library(rgdal)
  coords <- event[!is.na(event$pos_x), c("pos_y", "pos_x")]
  names(coords) <- c("pos_x", "pos_y")
  coordinates(coords) <- c("pos_x", "pos_y")
  proj_ref_system <-
    "+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9999 +x_0=500000 +y_0=-5000000 +ellps=bessel +towgs84=682,-203,480,0,0,0,0 +units=m +no_defs"
  proj4string(coords) <- CRS(proj_ref_system)
  coords_WGS84 <-
    spTransform(coords, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  event$lon <- NA
  event$lat <- NA
  event[!is.na(event$pos_x), c("lon", "lat")] <-
    as_data_frame(coords_WGS84@coords)

  event
}


event <- prepare_events("data-raw/source_files/")
event <- convert_coordinates()




if(FALSE) {

  summary(event)

}


