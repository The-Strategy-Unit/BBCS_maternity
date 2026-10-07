# Arithmetic and source-contract checks
# Small counted examples verify rates.
local({
  source("R/ons_data.R")
  source("R/report_cohort.R")
  source("R/age_boundary_review.R")
  near <- function(x, y) stopifnot(isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-9)))
  code <- parse("R/Maternity analysis.R")
  # Load definitions and general settings, without executing the MSDS analysis.
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-"))) {
      name <- as.character(statement[[2]])
      if (identical(name, "required_inputs")) break
    }
    eval(statement)
  }
  for (statement in code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  }
  counts <- read_national_age_counts()
  # Section 2.2.2 includes booking trusts outside the six named labels.
  # A trust without a named label keeps its provider code as the row name.
  starts <- data.frame(booking_code = c("RXT", "RQ3"),
    outcome = c("No recorded delivery", "Within cluster"))
  pathway <- pathway_summary(c("RXT", "RQ3"), "Trust")
  stopifnot(identical(pathway$Organisation[1], "RXT"))
  near(pathway$Starts, c(1, 1))
  near(pathway$No_delivery, c(1, 0))
  near(pathway_summary("QHL", "ICB")$Starts, 2)
  factors <- prepare_projection_age_factors(counts)
  stopifnot(nrow(counts) == 117, all(factors$factors$Stillbirth_factor >= 1))
  source25 <- counts[counts$age == "25", ]
  f25 <- factors$factors[factors$factors$age == "25", ]
  near(f25$Stillbirth_ratio, sum(source25$stillbirths) / sum(source25$live_births))
  near(f25$Multiplicity_factor, sum(source25$maternities) / sum(source25$total_births))
  near(factors$factors$Maternity_modifier,
    factors$factors$Pooled_maternities / factors$factors$Pooled_live_births)
  stopifnot(length(unique(factors$modifications$Stillbirth_factor[which(factors$modifications$Age >= 50)])) == 1)

  # Unequal annual volumes distinguish pooling counts from averaging ratios.
  varied <- counts
  ix <- which(varied$age == "25")
  varied$live_births[ix] <- c(100,200,700)
  varied$stillbirths[ix] <- c(10,0,70)
  varied$maternities[ix] <- c(99,100,385)
  varied$total_births <- varied$live_births + varied$stillbirths
  v <- prepare_projection_age_factors(varied)$factors
  v <- v[v$age=="25", ]
  near(v$Pooled_live_births, 1000)
  near(v$Pooled_stillbirths, 80)
  near(v$Pooled_maternities, 584)
  near(v$Stillbirth_factor, 1080/1000)
  near(v$Multiplicity_factor, 584/1080)
  near(v$Maternity_modifier, 584/1000)
  stopifnot(abs(v$Maternity_modifier-((1.1+1+1.1)/3)*((.9+.5+.5)/3))>1e-3,
    abs(v$Maternity_modifier-(.99+.5+.55)/3)>1e-3)

  # Combined-age fallback factors also pool source counts before division.
  for (boundary in c(15L, 44L)) {
    ages <- if (boundary == 15L) c("<14", "14", "15") else c(as.character(44:49), "50+")
    source <- counts[counts$age %in% ages, ]
    fallback <- factors$modifications |>
      dplyr::filter(is.na(Age), ONS_source_age == boundary)
    near(fallback$Stillbirth_factor, 1 + sum(source$stillbirths)/sum(source$live_births))
    near(fallback$Multiplicity_factor, sum(source$maternities)/sum(source$total_births))
  }

  # Table 7 provides counts for pooled descriptive proportions too.
  multiplicity <- read_national_multiplicity()
  pooled <- pooled_multiplicity_rates(multiplicity)
  totals <- pooled |> dplyr::group_by(age_band) |>
    dplyr::summarise(total = sum(pooled_proportion), .groups = "drop")
  near(totals$total, rep(1, nrow(totals)))
  unequal <- data.frame(age_band = "25-29", multiplicity = "2",
    maternities = c(1, 90), all_maternities = c(10, 100))
  near(pooled_multiplicity_rates(unequal)$pooled_proportion, 91/110)
  stopifnot(nchar(gt::as_raw_html(multiplicity_table(pooled))) > 0)

  # LA A, age 25: delivery A 12 / 20; annual-average share would be 2 / 3.
  # Bookings without a recorded delivery affect booking shares, not delivery shares.
  cells <- data.frame(LA = c(rep("E08000025", 3), "E08000027", rep("E08000025", 5)),
    Age = c(25,25,25,25,13,14,15,44,50),
    FY = c("2023/24","2024/25","2025/26","2023/24",rep("2025/26",5)),
    Trust = c("RQ3","RRK","RQ3","RRK",rep("RQ3",5)), N = c(2,8,10,20,1,2,7,4,6))
  cells <- dplyr::bind_rows(cells, data.frame(LA="E08000025", Age=43, FY="2025/26", Trust="RQ3", N=5))
  # Include activity in BOTH ICBs so reconciliation is not tested against an empty ICB.
  cells <- dplyr::bind_rows(cells, data.frame(LA="E08000025", Age=26, FY="2025/26", Trust="RL4", N=9))
  rows <- cells[rep(seq_len(nrow(cells)), cells$N), ]
  cohort <- data.frame(person_id_mother = seq_len(nrow(rows)), uniq_preg_id = seq_len(nrow(rows)),
    birth_yr = rows$FY, birth_flag = 1L, birth_date = as.Date(paste0(substr(rows$FY,1,4),"-12-01")),
    la_of_mother = rows$LA, age_at_birth_mother = rows$Age, birth_provider = rows$Trust,
    home_birth = 0L, resident_icb = "QHL", booking_provider = "RRK",
    antenatal_app_date = as.Date(paste0(substr(rows$FY,1,4),"-06-01")))
  cohort$booking_provider[cohort$birth_provider == "RL4"] <- "RL4"
  residents <- cohort |> dplyr::count(birth_yr, la_of_mother, age_at_birth_mother, name = "count")
  extra <- cohort[rep(1,20), ]
  extra$birth_flag <- 0L
  extra$birth_date <- as.Date(NA)
  extra$birth_yr <- NA_character_
  extra$booking_provider <- "RQ3"
  extra$antenatal_app_date <- as.Date("2025-06-01")
  extra$person_id_mother <- extra$uniq_preg_id <- 1001:1020
  booking_only <- extra[1:5, ]
  booking_only$booking_provider <- "RXT"
  booking_only$person_id_mother <- booking_only$uniq_preg_id <- 2001:2005
  cohort <- dplyr::bind_rows(cohort, extra, booking_only)
  ons <- tidyr::expand_grid(area_code = unique(cells$LA), age_group = as.character(15:44), year = 2025:2026)
  ons$area_name <- ons$area_code
  ons$projected_births <- ifelse(ons$year == 2025, 100, 110)
  m <- build_projection_master(cohort, residents, ons, counts)
  stopifnot(identical(sort(unique(m$Year)), 2025:2026),
    all(m$ONS_reference_year == 2025L))
  stopifnot(identical(names(m)[1:6], c("LA", "Age", "Year", "Exact_age_live_births",
    "Delivery_projection_base", "Booking_projection_base")))
  near(m$Exact_age_live_births, m$ONS_live_births * m$Age_weight)
  a <- m[m$LA == "E08000025" & m$Age == 25 & !is.na(m$Age), ]
  near(a$Delivery_proportion_RQ3, c(.6,.6))
  near(a$Booking_proportion_RQ3, rep(20/45,2))
  near(a$Delivery_projection_base, a$Adjusted_maternities)
  near(a$Booking_projection_base, a$Adjusted_maternities)
  near(a$Booking_count_RQ3, a$Booking_projection_base * 20/45)
  near(a$Adjusted_births_ls, c(100,110) * f25$Stillbirth_factor)
  near(a$Adjusted_maternities, a$Adjusted_births_ls * f25$Multiplicity_factor)
  near(a$Delivery_count_RQ3, a$Adjusted_maternities * .6)
  near(m$Age_weight[m$LA == "E08000025" & m$Year == 2025 & m$Age %in% 13:15], c(.1,.2,.7))
  for (type in c("Delivery", "Booking")) {
    volume <- m[[paste0(type, "_projection_base")]]
    near(rowSums(m[grep(paste0("^",type,"_count_"), names(m), value=TRUE)]),volume)
    long <- projection_destination_rows(m, type)
    value <- if (type == "Delivery") "Projected_maternities" else "Projected_bookings"
    near(sum(long[[value]]),sum(volume))
  }
  # Changing booking providers cannot alter delivery counts.
  altered <- cohort
  altered$booking_provider <- "RRK"
  m2 <- build_projection_master(altered, residents, ons, counts)
  delivery_columns <- grep("^Delivery_",names(m),value=TRUE)
  stopifnot(identical(m[delivery_columns],m2[delivery_columns]))
  # Changing delivery providers cannot alter booking counts.
  altered <- cohort
  altered$birth_provider <- "RRK"
  m2 <- build_projection_master(altered, residents, ons, counts)
  booking_columns <- grep("^Booking_",names(m),value=TRUE)
  stopifnot(identical(m[booking_columns],m2[booking_columns]))
  # Booking-year and delivery-year filters use their respective source fields.
  period_rows <- cohort[rep(1,3), ]
  period_rows$birth_yr <- c("2022/23","2025/26","2026/27")
  period_rows$birth_date <- as.Date(c("2023-02-01","2025-12-01","2026-12-01"))
  period_rows$antenatal_app_date <- as.Date(c("2023-04-01","2023-03-31","2026-03-31"))
  period_rows$birth_provider <- period_rows$booking_provider <- c("RQ3","RRK","RQ3")
  period_counts <- period_rows |> dplyr::count(birth_yr,la_of_mother,age_at_birth_mother,name="count")
  periods <- build_allocation_tables(period_rows,period_counts)
  stopifnot(identical(periods$delivery$Destination,"RRK"),
    identical(periods$booking$Destination,"RQ3"))
  near(periods$delivery$Count,1)
  near(periods$booking$Count,2)
  near(periods$booking$MSDS_booking_total,2)

  # Younger-age weights pool years too; do not use only the latest age distribution.
  early <- cohort[rep(which(cohort$age_at_birth_mother==13)[1],9), ]
  early$birth_yr <- "2023/24"
  early$birth_date <- as.Date("2023-12-01")
  early$antenatal_app_date <- as.Date("2023-06-01")
  age_cohort <- dplyr::bind_rows(cohort,early)
  age_residents <- age_cohort |> dplyr::filter(birth_flag==1) |>
    dplyr::count(birth_yr,la_of_mother,age_at_birth_mother,name="count")
  age_master <- build_projection_master(age_cohort,age_residents,ons,counts)
  age_rows <- age_master |> dplyr::filter(LA=="E08000025",Year==2025,Age %in% 13:15)
  near(age_rows$Age_weight,c(10,2,7)/19)

  # Character/factor fields from parquet give the same numeric results.
  cohort$age_at_birth_mother <- factor(cohort$age_at_birth_mother)
  residents$count <- as.character(residents$count)
  m3 <- build_projection_master(cohort, residents, ons, counts)
  near(m3$Adjusted_maternities,m$Adjusted_maternities)

  # Seven reporting bands retain boundary ages on the correct side of 44/45.
  stopifnot(identical(report_age_band(c(19,20,24,25,29,30,34,35,39,40,44,45,60)),
    c("Under 20","20-24","20-24","25-29","25-29","30-34","30-34",
      "35-39","35-39","40-44","40-44","45+","45+")),
    length(projection_age_colours) == length(projection_age_bands))
  # All resident charts use the completed master; all years remain available.
  ons_full <- tidyr::expand_grid(area_code = unique(cells$LA), age_group = as.character(15:44), year = 2023:2047)
  ons_full$area_name <- read_area_config()$area_name[match(ons_full$area_code, read_area_config()$area_code)]
  ons_full$projected_births <- ifelse(ons_full$year == 2026, 110, 100)
  full <- build_projection_master(cohort,residents,ons_full,counts,start_year=2023)
  resident <- summarise_resident_projections(full)
  stopifnot(identical(sort(unique(resident$projected_trajectory$year)),2023:2047))
  # Resident comparison uses all-age resident totals, including activity beyond
  # the cluster cohort; counts and historical projections are never rebased.
  comparison_residents <- dplyr::bind_rows(residents |> dplyr::mutate(count = as.numeric(count)),
    data.frame(birth_yr = "2025/26", la_of_mother = "E08000025",
      age_at_birth_mother = NA_real_, count = 7))
  registered_example <- data.frame(area_name = "Birmingham", year = 2025L, maternities = 80)
  comparison <- build_resident_comparison(comparison_residents, registered_example,
    resident$projected_trajectory)
  near(comparison$msds$maternities[comparison$msds$area_code == "E08000025" &
      comparison$msds$Period == "2025/26"],
    sum(as.numeric(residents$count[residents$la_of_mother == "E08000025" & residents$birth_yr == "2025/26"])) + 7)
  near(comparison$data$maternities[comparison$data$Source == "ONS published maternities"],80)
  estimate_rows <- comparison$data |> dplyr::filter(Source == "Adjusted maternity projections")
  near(sum(estimate_rows$maternities),sum(resident$projected_trajectory$maternities))
  stopifnot(min(estimate_rows$Year) == 2023L,
    comparison$latest_recorded_year == 2025L,
    all(comparison$msds$Year == as.integer(substr(comparison$msds$Period, 1, 4))))
  built_comparison <- ggplot2::ggplot_build(comparison$chart)
  stopifnot(setequal(unique(built_comparison$data[[2]]$shape), c(15,16)),
    setequal(unique(built_comparison$data[[1]]$linetype), c("solid","dashed")))

  # The report's 2025 check uses the 2025 count, even when adjacent years differ.
  report_lines <- readLines("index.qmd", warn = FALSE)
  check_start <- grep("^#\\| label: method-1-resident-level-check-table$", report_lines) + 1L
  check_end <- check_start + which(report_lines[check_start:length(report_lines)] == "```")[1] - 2L
  check_code <- parse(text = report_lines[check_start:check_end])
  observed_trajectory <- data.frame(area_name = c("Birmingham", "Dudley"),
    year = 2025L, maternities = c(80, 100))
  projected_trajectory <- data.frame(area_name = rep(c("Birmingham", "Dudley"), 3),
    year = rep(2024:2026, each = 2), maternities = c(20, 30, 90, 120, 900, 1200))
  invisible(eval(check_code))
  near(resident_level_check$Projected, c(90, 120))
  near(resident_level_check$Difference, c(10, 20))
  projected_trajectory <- projected_trajectory[projected_trajectory$year == 2025, ]
  invisible(eval(check_code))
  near(resident_level_check$Projected, c(90, 120))
  by_la <- full |> dplyr::group_by(LA,Year) |>
    dplyr::summarise(N=sum(Adjusted_maternities),.groups="drop") |>
    dplyr::left_join(resident$total_maternity_projections,by=c("LA"="area_code","Year"="year"))
  near(by_la$N,by_la$projected_maternities)
  summed_bands <- resident$age_band_projections |> dplyr::group_by(area_code,year) |>
    dplyr::summarise(N=sum(projected_maternities),.groups="drop") |>
    dplyr::left_join(resident$total_maternity_projections,by=c("area_code","year"))
  near(summed_bands$N,summed_bands$projected_maternities)
  exact_a <- full[full$LA=="E08000025" & full$Year==2025 & !is.na(full$Age), ]
  for (band in c("Under 20","40-44","45+")) {
    expected <- sum(exact_a$Adjusted_maternities[report_age_band(exact_a$Age)==band])
    actual <- resident$age_band_projections |> dplyr::filter(area_code=="E08000025",year==2025,age_band==band)
    near(actual$projected_maternities,expected)
  }
  # Filtering forward years retains allocation metadata used by charts.
  forward <- full[full$Year>=2025, ]
  stopifnot(identical(attr(forward,"Delivery_destinations"),attr(full,"Delivery_destinations")))

  # Check the existing chart-data builder consumes the completed master columns.
  final_cohort_2326 <- cohort
  total_births_by_LA_2326 <- residents
  # Section 2.2.3 prepares its own organisation list, independently of
  # temporary variables inside the separate projection builder.
  geography <- build_delivery_geography()
  near(sum(geography$matrix_data$N),
    sum(prepare_report_cohort(cohort)$Birth_recorded &
      prepare_report_cohort(cohort)$Year %in% "2025/26"))
  projection_master_dictionary <- projection_column_dictionary(m)
  ons_modifications <- factors$modifications
  projection_la_lookup <- readr::read_csv("data/lookups/la_names.csv", show_col_types=FALSE)
  projection_trust_lookup <- readr::read_csv("data/lookups/trust_names.csv", show_col_types=FALSE)
  projection_catchment_las <- read_area_config()$area_code
  result <- build_first_draft_projections(master=m,prepare_outputs=TRUE)

  # ONS 2023/2024/2025 counts pair with MSDS 2023/24, 2024/25 and 2025/26.
  # A much larger 2026 count catches accidental use of the following source year.
  history_ons <- ons_full
  history_ons$projected_births <- dplyr::case_when(
    history_ons$year == 2023 ~ 60, history_ons$year == 2024 ~ 80,
    history_ons$year == 2025 ~ 100, TRUE ~ 900)
  projection_master_backtest <- build_projection_master(cohort, residents, history_ons, counts,
    baseline_fy = "2023/24", end_year = 2025L)
  history <- build_projection_backtest()
  checked_history <- history$data |>
    dplyr::filter(Key == "trust-rq3") |> dplyr::arrange(Year)
  stopifnot(identical(checked_history$Year, 2023:2025),
    identical(checked_history$Year_label, c("2023/24", "2024/25", "2025/26")))
  near(checked_history$Observed, c(2, 0, 35))
  reference_total <- result$maternity_projections |>
    dplyr::filter(Key == "trust-rq3", Year == 2025) |> dplyr::pull(Projected)
  near(checked_history$Modelled, reference_total * c(.6, .8, 1))
  age_review <- build_msds_age_review(cohort, result, history$baseline, trust_labels, cluster_trusts)
  stopifnot(identical(sort(unique(age_review$exact_comparison$year)), 2023:2025))
  age43 <- age_review$exact_comparison |>
    dplyr::filter(Key == "trust-rq3", Age == 43) |> dplyr::arrange(year)
  near(age43$Modelled, c(60, 80, 100) * factors$factors$Maternity_modifier[factors$factors$age == "43"])
  near(age43$Observed, c(0, 0, 5))
  # Sankeys reconcile to observed deliveries and the completed final-year master.
  sankey <- result$sankey_comparison
  near(sum(sankey$flows$Observed), sum(result$observed_totals$Observed_baseline[
    result$observed_totals$Key %in% result$organisations$Key[result$organisations$Level=="Trust"]]))
  near(sum(sankey$flows$Projected), sum(result$maternity_projections$Projected[
    result$maternity_projections$Key=="cluster" & result$maternity_projections$Year==2026]))
  # Grouped change is 65/100-1, not the mean of +100% and -50%.
  observed_flows <- data.frame(LA=c(rep("A",10),rep("B",90),rep("X",2),"Unknown LA"),
    Trust=c(rep("RQ3",100),rep("RL4",2),"RRK"))
  projected_flows <- data.frame(LA=c("A","B","C","X"),Trust=c("RQ3","RQ3","RRK","RL4"),
    Year=2047,Projected_maternities=c(20,45,7,4))
  comparison <- build_projection_sankeys(observed_flows,projected_flows,result$organisations,
    "2025/26",2047,main_las=character(),other_las=0)
  near(comparison$flows$Percent_change[comparison$flows$Trust=="RQ3"],-.35)
  near(comparison$flows$Percent_change[comparison$flows$Trust=="RL4"],1)
  stopifnot(is.na(comparison$flows$Percent_change[
    comparison$flows$Trust=="RRK" & comparison$flows$LA=="All other feeder LAs"]))
  near(comparison$flows$Percent_change[comparison$flows$LA=="Unknown LA"],-1)
  for (field in c("observed","projected")) {
    trace <- plotly::plotly_build(comparison$panels[[field]])$x$data[[1]]
    near(sum(trace$link$value),if(field=="observed") 103 else 76)
    stopifnot(all(trace$link$source>=0),all(trace$link$target<length(trace$node$label)))
  }
  future_trace <- plotly::plotly_build(comparison$panels$projected)$x$data[[1]]
  visible <- comparison$flows[comparison$flows$Projected>0, ]
  stopifnot(future_trace$link$color[is.na(visible$Percent_change)]=="#909090")
  colours <- grDevices::col2rgb(future_trace$link$color)
  stopifnot(all(colours[1,visible$Percent_change<0 & !is.na(visible$Percent_change)] >
    colours[3,visible$Percent_change<0 & !is.na(visible$Percent_change)]),
    all(colours[3,visible$Percent_change>0 & !is.na(visible$Percent_change)] >
    colours[1,visible$Percent_change>0 & !is.na(visible$Percent_change)]))

  # Annual comparisons retain the model's delivery / booking denominators.
  variation <- build_input_variation_review(cohort,residents,varied,attr(m,"allocation_tables"))
  cell <- variation$delivery |>
    dplyr::filter(LA=="E08000025",Age==25,Destination=="RQ3") |> dplyr::arrange(FY)
  near(cell$Denominator,c(2,8,10))
  near(cell$Count,c(2,0,10))
  near(cell$Departure_pp,c(40,-60,40))
  near(weighted.mean(cell$Departure_pp,cell$Denominator),0)
  booking_cell <- variation$booking |>
    dplyr::filter(LA=="E08000025",Age==25,Destination=="RQ3") |> dplyr::arrange(FY)
  near(booking_cell$Denominator,c(2,8,35))
  near(booking_cell$Pooled,rep(20/45,3))
  near(variation$ons_pooled$Stillbirth_ratio[variation$ons_pooled$age=="25"],80/1000)
  # The displayed model marker uses pooled counts, alongside annual points.
  for (chart_name in c("stillbirth_chart", "multiplicity_chart")) {
    field <- if (chart_name == "stillbirth_chart") "Stillbirth_ratio" else "Multiplicity_factor"
    traces <- plotly::plotly_build(variation[[chart_name]])$x$data
    marker <- Filter(function(x) identical(x$name, "Pooled ratio used"), traces)
    stopifnot(length(marker) == 1L)
    expected <- variation$ons_pooled[[field]][match(marker[[1]]$x, variation$ons_pooled$age)]
    near(marker[[1]]$y, expected)
  }
  age_variation <- build_input_variation_review(age_cohort,age_residents,counts,
    attr(age_master,"allocation_tables"))
  age_cell <- age_variation$age_mix |>
    dplyr::filter(LA=="E08000025",Age==14) |> dplyr::arrange(FY)
  near(age_cell$Pooled,rep(2/19,3))
  near(age_cell$Annual[c(1,3)],c(0,.2))
  stopifnot(is.na(age_cell$Annual[2]))
  for (field in c("delivery_chart","booking_chart","age_mix_chart","stillbirth_chart","multiplicity_chart")) {
    stopifnot(length(plotly::plotly_build(variation[[field]])$x$data)>0)
  }
  near(result$maternity_projections$Projected[result$maternity_projections$Level=="Cluster"],
    result$delivery_rows |> dplyr::filter(Delivery_destination %in% cluster_trusts) |>
      dplyr::group_by(Year) |> dplyr::summarise(N=sum(Projected_maternities),.groups="drop") |> dplyr::pull(N))
  # Reporting age bands only sum completed individual-age counts.
  exact_totals <- result$single_age_projections |>
    dplyr::group_by(Key, Year) |> dplyr::summarise(N=sum(Projected),.groups="drop")
  band_totals <- result$age_projections |>
    dplyr::group_by(Key, Year) |> dplyr::summarise(N=sum(Projected),.groups="drop")
  near(exact_totals$N,band_totals$N)
  stopifnot(all(result$age_projections$Age_band %in% projection_age_bands))
  for (chart in result$total_charts) plotly::plotly_build(chart)
  # An LA with deliveries only in 2023/24 still has labels and accounting.
  early_feeder <- result$feeder_shares[result$feeder_shares$LA == "E08000027", ]
  stopifnot(nrow(early_feeder) > 0, all(!is.na(early_feeder$LA_name)),
    "E08000027" %in% result$destination_accounting$LA)
  stopifnot(any(result$booking_total_projections$Key == "trust-rxt" &
      result$booking_total_projections$Projected > 0),
    "trust-rxt" %in% result$booking_chart_groups$Trust$Key)
  for (view in result$booking_charts) for (chart in view) plotly::plotly_build(chart)
  stopifnot(all(result$booking_age_projections$Age_band %in% projection_age_bands))
  # Every displayed organisation uses its own provider membership, not another ICB's rows.
  for (i in seq_len(nrow(result$organisations))) {
    org <- result$organisations[i, ]
    providers <- if (org$Level=="Trust") org$Code else if (org$Level=="ICB")
      names(provider_icb)[provider_icb==org$Code] else cluster_trusts
    for (type in c("Delivery","Booking")) {
      columns <- paste0(type,"_count_",make.names(providers))
      columns <- intersect(columns,names(m))
      expected <- aggregate(rowSums(m[columns]),list(Year=m$Year),sum)
      shown <- if (type=="Delivery") result$maternity_projections else result$booking_total_projections
      shown <- shown[shown$Key==org$Key, ]
      near(shown$Projected,expected$x[match(shown$Year,expected$Year)])
      denominator <- if (type=="Delivery") shown$Observed_baseline else
        rep(shown$Projected[shown$Year==2025],nrow(shown))
      valid <- denominator>0
      near(shown$Percent_change[valid],shown$Projected[valid]/denominator[valid]-1)
      stopifnot(all(is.na(shown$Percent_change[!valid])))
    }
  }
  # Age 43 shrinks while 44 grows: report growth is the change in summed counts,
  # not an unweighted mean of age-specific growth percentages.
  varied_ons <- ons
  varied_ons$projected_births[varied_ons$year==2026 & varied_ons$age_group=="43"] <- 80
  varied_ons$projected_births[varied_ons$year==2026 & varied_ons$age_group=="44"] <- 150
  varied_master <- build_projection_master(cohort,residents,varied_ons,counts)
  varied_result <- build_first_draft_projections(master=varied_master,prepare_outputs=FALSE)
  f43 <- factors$factors$Maternity_modifier[factors$factors$age=="43"]
  f44 <- factors$factors$Maternity_modifier[factors$factors$age=="44"]
  expected_growth <- (80*f43+60*f44)/(100*f43+40*f44)-1
  shown_growth <- varied_result$weighted_age_growth |>
    dplyr::filter(Key=="trust-rq3",Age_band=="40-44",Year==2026)
  near(shown_growth$Growth_change,expected_growth)
  stopifnot(abs(expected_growth-mean(c(-.2,.5)))>1e-3)

  # Descriptive shares retain recorded MSDS counts for LAs without ONS coverage.
  restricted <- build_projection_master(cohort,residents,ons[ons$area_code=="E08000025", ],counts)
  restricted_result <- build_first_draft_projections(master=restricted,prepare_outputs=FALSE)
  actual_feeder <- restricted_result$feeder_shares |>
    dplyr::filter(Key=="cluster",LA=="E08000027",Age==25)
  stopifnot(nrow(actual_feeder)==1)
  near(actual_feeder$Numerator,20)
  near(actual_feeder$Denominator,20)
  accounting <- restricted_result$destination_accounting |>
    dplyr::filter(LA=="E08000027",Age==25)
  near(accounting$Cluster_trusts,20)
  near(accounting$Other_or_unrepresented,0)

  # Chart selections include recorded delivery providers outside the six named labels.
  additional <- cohort
  additional$birth_provider[additional$la_of_mother=="E08000027"] <- "RXT"
  final_cohort_2326 <- additional
  additional_master <- build_projection_master(additional,residents,ons,counts)
  additional_result <- build_first_draft_projections(master=additional_master,prepare_outputs=TRUE)
  stopifnot("trust-rxt" %in% additional_result$delivery_chart_groups$Trust$Key)
  expected_names <- additional_result$delivery_chart_groups$Trust$Organisation
  chart_names <- vapply(plotly::plotly_build(additional_result$total_charts$Trust)$x$data,
    function(trace) trace$name,character(1))
  stopifnot(setequal(chart_names,expected_names))
  final_cohort_2326 <- cohort

  # Output factor labels come from the supplied master.
  different_factors <- build_projection_master(cohort,residents,ons,varied)
  different_result <- build_first_draft_projections(master=different_factors,prepare_outputs=FALSE)
  stopifnot(identical(different_result$ons_modifications,attr(different_factors,"ons_modifications")))

  # Check plotted x/y values as well as the source tables behind them.
  same_series <- function(actual,expected) {
    stopifnot(identical(is.na(as.numeric(actual)),is.na(as.numeric(expected))))
    near(actual[!is.na(actual)],expected[!is.na(expected)])
  }
  for (field in c("total_charts","change_charts")) {
    for (chart in result[[field]]) for (trace in plotly::plotly_build(chart)$x$data) {
      # Plotly may retain an empty placeholder when a zero baseline gives no change values.
      if (is.null(trace$name)) {
        stopifnot(!length(trace[["x"]]),!length(trace[["y"]])); next
      }
      rows <- result$maternity_projections[result$maternity_projections$Organisation==trace$name, ]
      rows <- rows[order(rows$Year), ]
      near(trace[["x"]],rows$Year)
      same_series(trace[["y"]],if (field=="total_charts") rows$Projected else rows$Percent_change)
    }
  }
  for (view in names(result$booking_charts)) for (key in names(result$booking_charts[[view]])) {
    traces <- plotly::plotly_build(result$booking_charts[[view]][[key]])$x$data
    for (trace in traces) {
      if (is.null(trace$name)) {
        expected_change <- result$booking_total_projections |>
          dplyr::filter(Key==key) |> dplyr::pull(Percent_change)
        stopifnot(view=="change",all(is.na(expected_change)),!length(trace[["x"]]),!length(trace[["y"]])); next
      }
      if (view=="age") {
        rows <- result$booking_age_projections |>
          dplyr::filter(Key==key,Age_band==trace$name) |> dplyr::arrange(Year)
        if (!nrow(rows)) { near(trace[["y"]],rep(0,length(trace[["y"]]))); next }
      } else rows <- result$booking_total_projections |>
        dplyr::filter(Key==key) |> dplyr::arrange(Year)
      near(trace[["x"]],rows$Year)
      same_series(trace[["y"]],if (view=="change") rows$Percent_change else rows$Projected)
    }
  }

  # Run the production projection QA against these small arithmetic examples.
  qa_text <- readLines("R/QA.R", warn=FALSE)
  qa_code <- parse("R/QA.R")
  for (statement in qa_code) {
    if (is.call(statement) && identical(statement[[1]], as.name("<-")) &&
        is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement)
  }
  qa_results <- data.frame(Check=character(),Result=character(),Details=character())
  qa_details <- list()
  result$backtest <- list(baseline=result)
  readRDS <- function(path, ...) if (path == ".ons_exploration.rds") resident else result
  file.exists <- function(...) TRUE
  start <- grep("^# 6[.] Check",qa_text)
  finish <- grep("^# Reconcile sections",qa_text)-1L
  eval(parse(text=qa_text[start:finish]))
  stopifnot(all(qa_results$Result == "OK"))
  message("PASS: direct source-year comparisons, pooled Table 5 factors and Table 7 proportions, pooled delivery proportions, age split, separate booking/delivery bases, independent allocations, earlier-year feeders, booking-only providers, seven age bands, shared resident results, pooled ONS ratios, period filters, count-weighted changes, unmatched-ONS descriptive counts and chart provider coverage.")
})
