# Clean Sarah's person_id_mother / lsoa_2011 / ethnicity extract before joining.
demographic_ethnicity_groups <- c(W = "White", M = "Mixed", A = "Asian or Asian British",
  B = "Black or Black British", OOC = "Other ethnic groups - Chinese",
  OO = "Other ethnic groups - Any other", Z = "Not stated", TC = "Two conflicting")

demographic_mother_id <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & trimws(x) == ""] <- NA_character_
  x
}

demographic_parts <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  parts <- toupper(trimws(strsplit(x, "|", fixed = TRUE)[[1]]))
  if (grepl("\\|\\s*$", x)) parts <- c(parts, "")
  parts
}

prepare_demographics <- function(demographics,
    imd_lookup = read.csv("data/lookups/imd2019_lsoa2011.csv"), lsoa_seed = 20261021L) {
  if (length(lsoa_seed) != 1L || is.na(lsoa_seed)) stop("Supply one non-missing LSOA seed.")
  required <- c("person_id_mother", "lsoa_2011", "ethnicity")
  missing <- setdiff(required, names(demographics))
  if (length(missing)) stop("Demographics needs the new three-column extract; missing: ",
    paste(missing, collapse = ", "), call. = FALSE)
  lookup <- demographics |>
    dplyr::transmute(person_id_mother = demographic_mother_id(person_id_mother),
      lsoa_2011_raw = as.character(lsoa_2011), ethnicity_raw = as.character(ethnicity)) |>
    dplyr::distinct()
  if (anyDuplicated(lookup$person_id_mother[!is.na(lookup$person_id_mother)]))
    stop("Conflicting demographics rows for the same mother; resolve these before joining.", call. = FALSE)
  if (anyNA(imd_lookup$lsoa_2011) || anyDuplicated(imd_lookup$lsoa_2011) ||
      !all(imd_lookup$imd_decile %in% 1:10)) stop("Invalid IMD lookup keys or deciles.")

  ethnicity_lookup <- read.csv("data/lookups/ethnic_category.csv")
  pair_rules <- as.matrix(read.csv("data/lookups/ethnicity_pair_rules.csv", row.names = 1,
    check.names = FALSE))
  ethnicity_values <- unique(lookup$ethnicity_raw)
  ethnicity_clean <- lapply(ethnicity_values, function(value) {
    parts <- demographic_parts(value)
    codes <- unique(parts)
    status <- if (!length(parts)) "Missing" else if (length(parts) > 2L)
      "More than two supplied values: review" else if (any(!codes %in% ethnicity_lookup$code))
      "Code outside supplied grid: review" else if (length(codes) == 1L) "Single category" else "Pair grid"
    group <- NA_character_
    if (status == "Single category") group <- ethnicity_lookup$group_code[match(codes, ethnicity_lookup$code)]
    if (status == "Pair grid") group <- pair_rules[codes[2], codes[1]]
    broad <- unname(demographic_ethnicity_groups[group])
    description <- if (status == "Single category")
      ethnicity_lookup$description[match(codes, ethnicity_lookup$code)] else broad
    source_description <- ethnicity_lookup$description[match(parts, ethnicity_lookup$code)]
    source_description[parts == "99"] <- "Not known"
    source_description[is.na(source_description)] <- paste0("Unmapped code: ", parts[is.na(source_description)])
    data.frame(ethnicity_raw = value, ethnicity_group_code = group, ethnicity_broad = broad,
      ethnicity_description = description,
      ethnicity_source_description = if (length(parts)) paste(source_description, collapse = " | ") else NA_character_,
      ethnicity_status = if (!is.na(group) && group == "TC") "Two conflicting" else status)
  })
  ethnicity_clean <- if (length(ethnicity_clean)) dplyr::bind_rows(ethnicity_clean) else
    data.frame(ethnicity_raw = character(), ethnicity_group_code = character(), ethnicity_broad = character(),
      ethnicity_description = character(), ethnicity_source_description = character(), ethnicity_status = character())
  lookup <- dplyr::left_join(lookup, ethnicity_clean, by = "ethnicity_raw", relationship = "many-to-one")

  lsoas <- lapply(lookup$lsoa_2011_raw, demographic_parts)
  lookup$lsoa_options <- lengths(lsoas)
  lookup$lsoa_choice <- vapply(seq_along(lsoas), function(i) {
    parts <- lsoas[[i]]
    if (!length(parts) || length(parts) > 2L || any(!nzchar(parts))) return(NA_integer_)
    if (length(parts) == 1L) return(1L)
    # Seeded pseudo-random first/second choice, stable per mother across row order and reruns.
    hash <- digest::digest(paste(lsoa_seed, lookup$person_id_mother[i], sep = ":"),
      algo = "xxhash32", serialize = FALSE)
    1L + strtoi(substr(hash, 1, 7), base = 16L) %% 2L
  }, integer(1))
  lookup$lsoa_2011 <- vapply(seq_along(lsoas), function(i) {
    if (is.na(lookup$lsoa_choice[i])) NA_character_ else lsoas[[i]][lookup$lsoa_choice[i]]
  }, character(1))
  lookup$imd_decile <- as.integer(imd_lookup$imd_decile[match(lookup$lsoa_2011, imd_lookup$lsoa_2011)])
  lookup$lsoa_status <- dplyr::case_when(
    lookup$lsoa_options == 0 ~ "Missing LSOA",
    is.na(lookup$lsoa_choice) ~ "Malformed or more than two LSOAs: review",
    is.na(lookup$imd_decile) ~ "Selected LSOA absent from England IMD 2019",
    lookup$lsoa_options == 2 ~ "Random first/second LSOA selected",
    TRUE ~ "Single LSOA matched")
  lookup$lsoa_seed <- rep(lsoa_seed, nrow(lookup))
  lookup |> dplyr::select(person_id_mother, ethnicity_broad, ethnicity_description, imd_decile,
    dplyr::everything())
}

demographics_qa <- function(cleaned) {
  list(ethnicity = dplyr::count(cleaned, ethnicity_raw, ethnicity_source_description,
      ethnicity_broad, ethnicity_status, name = "Mothers", sort = TRUE),
    imd = dplyr::count(cleaned, lsoa_status, lsoa_choice, imd_decile, name = "Mothers"),
    unmatched_lsoas = cleaned |> dplyr::filter(is.na(imd_decile)) |>
      dplyr::count(lsoa_2011_raw, lsoa_2011, lsoa_status, name = "Mothers", sort = TRUE))
}
