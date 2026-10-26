# Permanent categorical colours: SU branding plus NHS England colours.
# SU: _brand.yml. NHS: https://service-manual.nhs.uk/design-system/styles/colour
# Keys are identities, never their position, volume or presence in a chart.
hybrid_palette <- c(
  su_blue = "#5881C1", orange = "#ED8B00", dark_green = "#006747",
  purple = "#330072", pink = "#AE2573", su_yellow = "#F9BD07",
  aqua_green = "#00A499", dark_red = "#8A1538", light_blue = "#41B6E6",
  light_green = "#78BE20", dark_blue = "#003087", dark_pink = "#7C2855",
  green = "#009639", aqua_blue = "#00A9CE", blue = "#005EB8",
  yellow = "#FAE100", charcoal = "#2C2825", grey = "#959595")

organisation_colour_key <- c(
  # Six main trusts: deliberately different hue families first.
  RNA = "#5881C1", RXK = "#ED8B00", RBK = "#006747",
  RL4 = "#330072", RRK = "#AE2573", RQ3 = "#F9BD07",
  # Other cluster providers retain their own colours in every subset.
  RRJ = "#00A499", RXT = "#8A1538", RYW = "#41B6E6",
  RYA = "#78BE20", RYK = "#003087",
  # ICBs have a separate categorical key; they are a different node layer.
  QHL = "#5881C1", QUA = "#ED8B00", QNC = "#006747",
  QWU = "#330072", QGH = "#AE2573", QOC = "#F9BD07", QJ2 = "#00A499",
  CLUSTER = "#2C2825")

status_colour_key <- c(Other = "#7C2855", Home = "#009639",
  `No booking` = "#00A9CE", `No delivery` = "#2C2825", Unknown = "#959595")

local_authority_colour_key <- local({
  areas <- read.csv("config/local_authorities.csv", stringsAsFactors = FALSE)
  stats::setNames(toupper(areas$colour), toupper(areas$area_code))
})

# National feeder LAs/providers can exceed an 18-colour categorical palette.
# Unlisted codes receive a repeatable shade derived from a hybrid anchor.
# This fallback never reallocates existing colours when another code appears.
# Explicit assignments above take priority; labels/order remain essential.
extended_identity_colour <- function(key) {
  vapply(key, function(value) {
    hash <- 0
    for (byte in utf8ToInt(value)) hash <- (hash * 131 + byte) %% 2147483647
    anchors <- hybrid_palette[1:14]
    anchor <- grDevices::col2rgb(anchors[1 + hash %% length(anchors)]) / 255
    shade <- 0.78 + ((hash %/% length(anchors)) %% 401) / 1000
    rgb <- if (shade <= 1) anchor * shade else anchor + (1 - anchor) * (shade - 1)
    grDevices::rgb(rgb[1], rgb[2], rgb[3])
  }, character(1), USE.NAMES = FALSE)
}

identity_colours <- function(codes) {
  original <- as.character(codes)
  key <- toupper(trimws(sub("^(trust|icb)-", "", original, ignore.case = TRUE)))
  colours <- unname(organisation_colour_key[key])
  # LA colours share the configured ONS key across Sankeys and trend charts.
  area_colours <- unname(local_authority_colour_key[key])
  colours[!is.na(area_colours)] <- area_colours[!is.na(area_colours)]
  status <- rep(NA_character_, length(key))
  status[grepl("^(OTHER($| TRUST| PROVIDER| ICB)|ALL OTHER )", key)] <- "Other"
  status[grepl("^HOME($| BIRTH| DELIVERY)", key)] <- "Home"
  status[grepl("^NO (RECORDED )?BOOKING", key)] <- "No booking"
  status[grepl("^NO (RECORDED )?DELIVERY", key)] <- "No delivery"
  status[is.na(key) | key %in% c("", "NA", "N/A", "NULL", "<NA>") |
    grepl("^(UNKNOWN|MISSING|NOT (STATED|KNOWN|RECORDED)|L999)", key)] <- "Unknown"
  colours[!is.na(status)] <- unname(status_colour_key[status[!is.na(status)]])
  missing <- is.na(colours)
  colours[missing] <- extended_identity_colour(key[missing])
  stats::setNames(colours, original)
}

organisation_line_colours <- function(keys) identity_colours(keys)
