# Actual public ONS counts plus invented MSDS rows; no Azure access.
local({
  code <- parse("R/Maternity analysis.R")
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        identical(statement[[2]], as.name("required_inputs"))) break
    eval(statement)
  }
  for (statement in code) if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
      is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  near <- function(x, y) stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-10)))
  actual <- read_private_nhs_counts()
  saved_actual <- actual
  factors <- prepare_private_nhs_factors(actual)
  near(factors$pooled$Total_maternities, c(37007,187513,425426,583070,351629,80480,7276,163,1672564))
  near(factors$pooled$Non_NHS_maternities, c(14,318,850,1884,1882,579,78,2,5607))
  near(factors$pooled$NHS_maternities, c(36667,185220,418521,569273,341275,78408,7144,137,1636645))
  near(factors$pooled$Private_NHS_factor, 1 - factors$pooled$Non_NHS_maternities / factors$pooled$Total_maternities)
  stopifnot(!any(factors$modifications$Private_NHS_age_band %in% c("Unknown (derived)", "All ages")))
  endpoints <- factors$modifications[match(c(13,19,20,24,25,29,30,34,35,39,40,44,45,49,50,54), factors$modifications$Age), ]
  stopifnot(identical(endpoints$Private_NHS_age_band,
    rep(c("Under 20","20-24","25-29","30-34","35-39","40-44","45+","45+"), each = 2)))
  # Unequal year sizes: pool counts, rather than averaging annual rates.
  varied <- actual
  for (year in 2023:2025) {
    rows <- varied$Year == year
    varied$Total_maternities[rows] <- c(100,1000,100)[year-2022]
    varied$Non_NHS_maternities[rows] <- c(10,10,30)[year-2022]
    varied$NHS_maternities[rows] <- varied$Total_maternities[rows] - varied$Non_NHS_maternities[rows]
    varied$Home_maternities[rows] <- varied$Elsewhere_maternities[rows] <- varied$Unallocated_place[rows] <- 0
  }
  near(prepare_private_nhs_factors(varied)$pooled$Private_NHS_factor, rep(1-50/1200,9))
  # Source validity checks now belong to manual QA, not every factor calculation.
  for (statement in parse("R/QA.R")) if (is.call(statement) &&
      identical(statement[[1]], as.name("<-")) &&
      identical(statement[[2]], as.name("qa_private_nhs_source_valid"))) eval(statement)
  stopifnot(qa_private_nhs_source_valid(actual))
  bad <- actual[-1, ]
  stopifnot(!qa_private_nhs_source_valid(bad))
  bad <- actual; bad$Non_NHS_maternities[1] <- NA_real_
  stopifnot(!qa_private_nhs_source_valid(bad))
  # Empty descriptive unknown-age rows must not fail QA for valid model factors.
  no_unknown <- actual
  count_columns <- setdiff(names(actual), c("Year", "Private_NHS_age_band"))
  for (year in 2023:2025) {
    unknown <- no_unknown$Year == year & no_unknown$Private_NHS_age_band == "Unknown (derived)"
    total <- no_unknown$Year == year & no_unknown$Private_NHS_age_band == "All ages"
    no_unknown[total, count_columns] <- no_unknown[total, count_columns] - no_unknown[unknown, count_columns]
    no_unknown[unknown, count_columns] <- 0
  }
  empty_unknown <- prepare_private_nhs_factors(no_unknown)
  near(empty_unknown$modifications$Private_NHS_factor, factors$modifications$Private_NHS_factor)
  qa_code <- parse("R/QA.R")
  for (statement in qa_code) if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
      is.symbol(statement[[2]]) && as.character(statement[[2]]) %in% c("numbers_match", "projection_numbers_match")) eval(statement)
  find_private_check <- function(x) {
    if (missing(x)) return(NULL)
    if (!is.call(x) && !is.expression(x)) return(NULL)
    if (is.call(x) && identical(x[[1]], as.name("qa_check")) &&
        grepl("Private-NHS factors use pooled England maternity counts", paste(deparse(x[[2]]), collapse=""), fixed=TRUE)) return(x)
    for (part in as.list(x)[-1L]) {
      found <- find_private_check(part)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  private_check <- find_private_check(qa_code)
  stopifnot(!is.null(private_check))
  qa_check <- function(description, passed, ...) isTRUE(passed)
  label <- "Test"
  private <- empty_unknown$pooled
  stopifnot(eval(private_check))
  private$Private_NHS_factor[1] <- NA_real_
  stopifnot(!eval(private_check))
  # Manual QA flags a missing denominator in a band used by the model.
  bad <- actual; bad[bad$Private_NHS_age_band == "Under 20", count_columns] <- 0
  stopifnot(!qa_private_nhs_source_valid(bad))
  # The old scale/allocation tests can set this factor to one explicitly.
  neutral <- actual
  neutral$NHS_maternities <- neutral$NHS_maternities + neutral$Non_NHS_maternities
  neutral$Non_NHS_maternities <- 0
  cells <- tidyr::expand_grid(Age = 13:54, FY = c("2023/24","2024/25","2025/26"), Row = 1:12)
  cohort <- data.frame(person_id_mother=seq_len(nrow(cells)), uniq_preg_id=seq_len(nrow(cells)),
    birth_yr=cells$FY, birth_flag=as.integer(cells$Row <= 10),
    birth_date=as.Date(paste0(substr(cells$FY,1,4),"-12-01")),
    la_of_mother="E08000025", age_at_birth_mother=cells$Age, age_at_booking_mother=NA_real_, birth_provider="RQ3",
    home_birth=0L, resident_icb="QHL", booking_provider="RQ3",
    antenatal_app_date=as.Date(paste0(substr(cells$FY,1,4),"-06-01")))
  resident <- cohort |> dplyr::filter(birth_flag == 1) |>
    dplyr::count(la_of_mother,age_at_birth_mother,birth_yr,name="count") |>
    dplyr::mutate(age_at_booking_mother = NA_real_, group = "antenatal+delivery", edd_yr = NA_character_)
  resident <- dplyr::bind_rows(resident,
    cohort |> dplyr::filter(birth_flag == 0) |>
      dplyr::count(la_of_mother, age_at_booking_mother = age_at_birth_mother,
        edd_yr = birth_yr, name = "count") |>
      dplyr::mutate(group = "antenatal only", age_at_birth_mother = NA_real_, birth_yr = NA_character_))
  ons <- tidyr::expand_grid(area_code="E08000025",age_group=as.character(15:44),year=2023:2027) |>
    dplyr::mutate(area_name=area_code,projected_births=100)
  counts <- read_national_age_counts()
  before <- build_projection_master(cohort,resident,ons,counts,start_year=2023,private_nhs_counts=neutral)
  after <- build_projection_master(cohort,resident,ons,counts,start_year=2023,private_nhs_counts=actual)
  # A zero multiplier produces genuine zeros, not missing projections.
  all_private <- actual
  all_private$Non_NHS_maternities <- all_private$Total_maternities
  all_private[setdiff(count_columns,c("Total_maternities","Non_NHS_maternities"))] <- 0
  zero <- build_projection_master(cohort,resident,ons,counts,start_year=2023,private_nhs_counts=all_private)
  stopifnot(all(zero$Adjusted_maternities == 0), all(zero$Projection_available))
  # No evidence to split either ONS boundary must not borrow a private rate.
  sparse_cohort <- cohort[cohort$age_at_birth_mother %in% 16:43, ]
  sparse_resident <- resident |>
    dplyr::filter(dplyr::coalesce(age_at_birth_mother, age_at_booking_mother) %in% 16:43)
  sparse <- build_projection_master(sparse_cohort,sparse_resident,ons,counts,start_year=2023,private_nhs_counts=actual)
  unsplit <- sparse[is.na(sparse$Age), ]
  stopifnot(nrow(unsplit)==10L, all(is.na(unsplit$Private_NHS_factor)),
    all(unsplit$Adjusted_maternities == 0), all(unsplit$Exact_age_live_births==100))
  zero_ons <- ons; zero_ons$projected_births[zero_ons$age_group %in% c("15","44")] <- 0
  sparse_zero <- build_projection_master(sparse_cohort,sparse_resident,zero_ons,counts,start_year=2023,private_nhs_counts=actual)
  stopifnot(all(sparse_zero$Adjusted_maternities[is.na(sparse_zero$Age)]==0))
  stopifnot(identical(attr(before,"allocation_tables"),attr(after,"allocation_tables")), identical(actual,saved_actual))
  near(after$Adjusted_maternities,before$Adjusted_maternities * after$Private_NHS_factor)
  near(after$Maternities_before_private,after$Exact_age_live_births*after$Stillbirth_factor*after$Multiplicity_factor)
  near(after$Maternities_after_private,after$Maternities_before_private*after$Private_NHS_factor)
  near(after$Adjusted_maternities,after$Maternities_after_private*after$Bookings_uplift_modifier)
  for (type in c("Booking","Delivery")) {
    near(after[[paste0(type,"_denominator")]],before[[paste0(type,"_denominator")]])
    fields <- grep(paste0("^",type,"_count_"),names(after),value=TRUE)
    for (field in fields) near(after[[field]],before[[field]]*after$Private_NHS_factor)
    near(rowSums(after[fields]),after$Adjusted_maternities)
  }
  final_cohort_2326 <- cohort
  projection_la_lookup <- readr::read_csv("data/lookups/la_names.csv",show_col_types=FALSE)
  projection_trust_lookup <- readr::read_csv("data/lookups/trust_names.csv",show_col_types=FALSE)
  projection_catchment_las <- cluster_las
  old <- build_first_draft_projections(master=before,prepare_outputs=FALSE)
  new <- build_first_draft_projections(master=after,prepare_outputs=FALSE)
  stopifnot(grepl("ONS births (L) \u00d7 local exact-age share \u00d7 [1 + S/L]", new$note, fixed = TRUE),
    grepl("\u00d7 M/(L+S) \u00d7 Private\u2013NHS modifier \u00d7 maternity pathway adjustment", new$note, fixed = TRUE))
  for (type in c("Booking","Delivery")) {
    a <- old$total_comparison[[type]]; b <- new$total_comparison[[type]]
    unaffected <- a$Series %in% c("MSDS recorded","ONS base")
    near(a$Value[unaffected],b$Value[unaffected])
  }
  near(new$maternity_projections$Projected[new$maternity_projections$Key=="trust-rq3"],
    tapply(after$Delivery_count_RQ3,after$Year,sum))
  near(new$booking_total_projections$Projected[new$booking_total_projections$Key=="trust-rq3"],
    tapply(after$Booking_count_RQ3,after$Year,sum))
  table_html <- gt::as_raw_html(private_nhs_modifier_table(factors$pooled))
  stopifnot(grepl("Private\u2013NHS modifier", table_html, fixed = TRUE),
    grepl("not rates assigned to known model ages", table_html, fixed = TRUE))
  message("PASS: published Table 4 totals, weighted pooling, age boundaries, zero unknown-age QA, missing model rates, zero multipliers and unsplit boundaries, one reduction shared by booking/delivery allocations, unchanged raw denominators and historical/ONS comparators.")
})
