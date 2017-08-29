#' Traffic accidents
#'
#' Accident events examined by police from 2005 to 2016
#'
#' @source
#'   http://www.policija.si/index.php/statistika/prometna-varnost
#'
#'   http://www.policija.si/baza/pnYYYY.zip
#'    (where YYYY in 05 to 2016)
#'
#' @format Data frame with columns
#' \describe{
#' \item{src_file}{A source file (year)}
#' \item{event_id}{Event id - unique within the src_file}
#' \item{injury}{Injury category}
#' \item{district}{Country district}
#' \item{date}{Date of the event}
#' \item{hour}{Hour of the event}
#' \item{in_city}{Is in the city}
#' \item{road_type}{Road category}
#' \item{road_mark}{Road id}
#' \item{location}{City or street}
#' \item{road_section_mark}{Section code}
#' \item{road_section}{Section}
#' \item{pos_number}{Address}
#' \item{location_type}{}
#' \item{cause}{Main cause type}
#' \item{event_type}{Type of accident}
#' \item{weather}{Weather}
#' \item{traffic}{Traffic}
#' \item{surface_conditions}{Surface condition}
#' \item{surface}{Type of surface}
#' \item{pos_x}{Position D48}
#' \item{pos_y}{Position D48}
#' \item{lon}{Position WGS84}
#' \item{lat}{Position WGS84}
#' }
#' @examples
#'   head(event)
"event"

#' Parties
#'
#' Parties involved in traffic accidents
#'
#' @source
#'   http://www.policija.si/index.php/statistika/prometna-varnost
#'
#'   http://www.policija.si/baza/pnYYYY.zip, PNL-OSEBE-yyyy.TXT
#'    (where YYYY in 05 to 2016)
#'
#' @format Data frame with columns
#' \describe{
#' \item{src_file}{Source file (year)}
#' \item{event_id}{Event id from event - use with src_file}
#' \item{accident_role}{At fault or just involved}
#' \item{age}{Party age}
#' \item{gender}{Gender}
#' \item{country}{Country name}
#' \item{p_injury}{Party injury severity}
#' \item{traffic_role}{Traffic role - e.g. Car driver, Truck driver ...}
#' \item{seat_belt_or_helmet}{}
#' \item{experience_y}{Experience - years}
#' \item{experience_m}{Experience - months}
#' \item{alcotest}{Result of alcotest}
#' \item{alco_exam}{Result of alco exam}
#' \item{country_iso}{2-letter country id}
#' \item{p_district}{Party district}
#'}
#' @examples
#'   head(party)
"party"

#' Calendar
#'
#' Calendar from 2005 - 2016
#'
#' @format Data frame with columns
#' \describe{
#' \item{date}{Date}
#' \item{date_POSIXct}{Date and time (0h)}
#' \item{year}{Year}
#' \item{month}{Month}
#' \item{day}{Day}
#' \item{wday}{Week day}
#' \item{year_day}{Year day}
#' \item{working_day}{"Working day" or "Weekend or holliday"}
#' }
#' @examples
#'   head(calendar)
"calendar"




