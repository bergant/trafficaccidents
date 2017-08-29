
# script to download the data from policija.si
# See http://www.policija.si/index.php/statistika/prometna-varnost

year_range <- 1995:2016
year_name <- sprintf("%s", year_range)
# until year 2011 the year in the file name is only 2 digits:
year_name[1:16] <- substring(year_name[1:16], 3, 5)

# download and unzip files
from_url <- sprintf("http://www.policija.si/baza/pn%s.zip", year_name)
target <- sprintf("data-raw/source/%d.zip", year_range)
for(i in seq_along(year_range)) {
  download.file(from_url[i], target[i])
  unzip(zipfile = target[i], exdir = sprintf("data-raw/source_files/%s", year_range[i]))
}

# remove ZIP files
unlink(target)
