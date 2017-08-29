get_code_table <- function(code,
                           #path = "http://policija.si/baza",
                           path = "data-raw/source_data",
                           file = file.path(path, paste0(code, ".TXT"))
) {

  s = readLines(file, 1, warn = FALSE)
  pos1 <- regexpr("\\b", s, perl = TRUE)
  pos2 <- regexpr(" \\b", substring(s, pos1), perl = TRUE)
  pos <- as.numeric(pos1 + pos2)

  dat <- read.fwf(file, widths = c(pos-1, 200), strip.white = TRUE, skip = 1, as.is = TRUE)
  dat <- dat[complete.cases(dat), ]
  names(dat) <- c("ID", "DESCRIPTION")
  dat
}

get_all_code_tables <- function(x, path = "tmp") {
  code_tables <-
    lapply( x, function(x) get_code_table(x, path = path))
  names(code_tables) <- x
  if(!is.null(names(x)))
    names(code_tables) <- names(x)
  code_tables
}

# code_tables <- get_all_code_tables(
#   c(poskodba = "PRPO",
#     obcina = "LOOB",
#     kat_ceste = "LOVC",
#     prizorisce = "PRKD",
#     vzrok = "PRVZ",
#     tip_nesrece = "PRTN",
#     vreme = "PRVR",
#     promet = "PRSP",
#     vozisce = "PRPV",
#     povrsina = "PRSV",
#     drzava = "LODZ",
#     udelezenec = "PRVU"),
#   "data-raw/code_tables"
# )




# c(
#   "poskodba",
#   "obcina",
#   "kat_ceste",
#   "prizorisce",
#   "vzrok",
#   "tip_nesrece",
#   "vreme",
#   "promet",
#   "vozisce",
#   "povrsina",
#   "drzava",
#   "udelezenec"
# )

#cat(sprintf("`%s` = '%s'", sifrant$tip_nesrece$DESCRIPTION, sifrant$tip_nesrece$ID), sep = "\n")

