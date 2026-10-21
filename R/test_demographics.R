# Synthetic checks only: no Azure and no report outputs.
library(dplyr)
source("R/demographics.R")
source("R/report_cohort.R")

local({
  codes <- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "P", "R", "S", "Z")
  pairs <- expand.grid(first = codes, second = codes, stringsAsFactors = FALSE)
  input <- data.frame(person_id_mother = as.character(seq_len(nrow(pairs))),
    lsoa_2011 = "E01000001|E01000002", ethnicity = paste(pairs$first, pairs$second, sep = "|"))
  result <- prepare_demographics(input)
  broad <- c(rep("W", 3), rep("M", 4), rep("A", 4), rep("B", 3), "OOC", "OO", "Z")
  names(broad) <- codes
  # Independent interpretation of the supplied grid, including the L/R exception.
  expected <- mapply(function(first, second) {
    a <- broad[[first]]; b <- broad[[second]]
    if (a == b) return(a)
    if (a == "M" || b == "M") return("M")
    if (a == "Z") return(b)
    if (b == "Z") return(a)
    if (a == "OO") return(b)
    if (b == "OO") return(a)
    if (setequal(c(first, second), c("L", "R"))) return("OOC")
    "TC"
  }, pairs$first, pairs$second)
  stopifnot(identical(result$ethnicity_group_code, unname(expected)))
  singles <- input[seq_along(codes), ]; singles$ethnicity <- codes
  single_result <- prepare_demographics(singles)
  stopifnot(identical(single_result$ethnicity_group_code, unname(broad)),
    single_result$ethnicity_description[1] == "White - British")
  reversed <- input; reversed$ethnicity <- paste(pairs$second, pairs$first, sep = "|")
  stopifnot(identical(result$ethnicity_broad, prepare_demographics(reversed)$ethnicity_broad))

  edge <- data.frame(person_id_mother = paste0("edge", 1:10),
    lsoa_2011 = c(NA, "", "W01000001", "E01000001|", "E01000001|E01000002|E01000003",
      "E01000001", " e01000002 ", "E01000001|E01000001", "E01000003", "E01000001"),
    ethnicity = c(NA, "", "99", "A|99", "X", "A|H|R", " s | n ", "A|A", "Z", "A|"))
  edge_result <- prepare_demographics(edge)
  stopifnot(all(is.na(edge_result$ethnicity_broad[c(1:6, 10)])),
    edge_result$ethnicity_broad[7] == "Black or Black British",
    edge_result$ethnicity_description[8] == "White - British",
    edge_result$ethnicity_broad[9] == "Not stated",
    all(is.na(edge_result$imd_decile[1:5])),
    identical(edge_result$imd_decile[6:10], c(9L, 10L, 9L, 5L, 9L)),
    nrow(prepare_demographics(input[0, ])) == 0)
  set.seed(27); rng_before <- .Random.seed
  reordered <- prepare_demographics(input[nrow(input):1, ])
  stopifnot(identical(rng_before, .Random.seed),
    identical(reordered$lsoa_2011[match(result$person_id_mother, reordered$person_id_mother)], result$lsoa_2011),
    setequal(result$lsoa_choice, 1:2),
    identical(prepare_demographics(input)$lsoa_2011, result$lsoa_2011))
  more_input <- bind_rows(input, transform(input[1, ], person_id_mother = "added-mother"))
  stopifnot(identical(prepare_demographics(more_input)$lsoa_2011[seq_len(nrow(input))], result$lsoa_2011),
    any(prepare_demographics(input, lsoa_seed = 123)$lsoa_choice != result$lsoa_choice))
  # The selected option remains selected even when it lacks an IMD match.
  sparse_lookup <- data.frame(lsoa_2011 = "E01000001", imd_decile = 9L)
  sparse <- prepare_demographics(input, imd_lookup = sparse_lookup)
  stopifnot(identical(sparse$lsoa_2011, result$lsoa_2011),
    all(is.na(sparse$imd_decile[sparse$lsoa_choice == 2])))
  stopifnot(nrow(prepare_demographics(bind_rows(input, input[1, ]))) == nrow(input),
    inherits(try(prepare_demographics(bind_rows(input, transform(input[1, ], ethnicity = "Z"))), silent = TRUE), "try-error"))
  imd <- read.csv("data/lookups/imd2019_lsoa2011.csv")
  stopifnot(nrow(imd) == 32844L, !anyDuplicated(imd$lsoa_2011), all(imd$imd_decile %in% 1:10))

  # Load actual organisation/counting functions without executing the report.
  for (statement in parse("R/Maternity analysis.R")) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        as.character(statement[[2]])[1] %in% c("cluster_icbs", "icb_provider_codes", "provider_icb",
          "cluster_trusts", "trust_labels", "flow_trust_labels", "group_flow_code", "classify_pathway_outcome"))
      eval(statement)
  }
  for (statement in parse("R/Demo Sankeys.R")) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  }
  cohort <- data.frame(person_id_mother = c("1", "1", "2", "3", NA, "", "not-in-lookup"),
    uniq_preg_id = as.character(1:7), birth_yr = "2025/26", birth_flag = c(1, 1, 0, 1, 1, 0, 0),
    birth_date = "2025-12-01", la_of_mother = "E08000031", age_at_birth_mother = 30,
    age_at_booking_mother = 29, birth_provider = "RL4", home_birth = c(0, 1, 0, 0, 0, 0, 0),
    resident_icb = "QUA", booking_provider = c("RL4", "RL4", "RL4", NA, NA, NA, "RL4"),
    antenatal_app_date = c("2025-06-01", "2025-06-01", "2025-06-01", NA, NA, NA, "2025-06-01"))
  demo_input <- data.frame(person_id_mother = c("1", "2", "3", NA, ""),
    lsoa_2011 = "E01000001|E01000002", ethnicity = c("S|N", "A|H", "L|R", "A", "A"))
  cohort$birth_yr[1:2] <- c("2023/24", "2024/25")
  cohort <- bind_rows(cohort, transform(cohort[1, ], uniq_preg_id = "8",
    birth_yr = "2025/26", age_at_birth_mother = 35))
  master <- build_demo_sankeys(cohort, prepare_demographics(demo_input))
  stopifnot(sum(master$N) == nrow(cohort), sum(master$N[is.na(master$ethnicity_broad)]) == 3,
    sum(master$N[master$ethnicity_broad %in% "Black or Black British"]) == 3,
    sum(master$N[master$ethnicity_broad %in% "Two conflicting"]) == 1,
    !any(grepl("person|preg|lsoa|raw", names(master))),
    nrow(distinct(filter(master, ethnicity_broad == "Black or Black British"),
      imd_decile, ethnicity_description)) == 1,
    n_distinct(master$Age_band[master$ethnicity_broad %in% "Black or Black British"]) == 2)
  for (field in c("ethnicity_broad", "ethnicity_description", "imd_decile", "Age_band"))
    stopifnot(sum((master |> group_by(.data[[field]]) |> summarise(N = sum(N)))$N) == nrow(cohort))
  cat("PASS: 289 ethnicity pairs, singles/unknowns, repeatable LSOA selection, IMD lookup and pregnancy join.\n")
})
