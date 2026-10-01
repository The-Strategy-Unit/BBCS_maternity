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