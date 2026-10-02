schema <- function(x) {
  data.frame(
    column = names(x),
    type = vapply(x, function(col) paste(class(col), collapse = "/"),
                  character(1))
  )
}

list(
  final_cohort = schema(final_cohort_2326),
  total_births_by_LA = schema(total_births_by_LA_2326)
)



table(final_cohort_2326$home_birth, useNA = "ifany")
table(final_cohort_2326$birth_flag, useNA = "ifany")

head(sort(unique(total_births_by_LA_2326$la_of_mother)), 10)
sort(unique(total_births_by_LA_2326$birth_yr))




source("R/Run analysis.R")
source("R/QA.R")
subset(qa_results, Result == "Check")