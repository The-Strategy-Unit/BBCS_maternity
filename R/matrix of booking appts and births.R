
library(janitor)
library(flextable)
library(dplyr)
library(tidyr)
library(scales)

# TRUST MATRIX------------------------------------------------------------------

## Trust names
trust_names <- data.frame(
  codes = c("RBK", "RL4", "RNA", "RQ3", "RRK", "RXK", "Other"),
  names = c("Walsall", "Wolverhampton", "Dudley", "Women's", "UH Birmingham", "Sandwell", "Other")
)

## Make matrix
matrix <-trust_matrix_data |> 
  left_join(trust_names, by=c("birth_provider_grouped"="codes"))|>
  rename(birth=names)|>
  left_join(trust_names, by=c("booking_provider_grouped"="codes"))|>
  rename(booking=names)|>
  mutate(booking=factor(booking, levels=c("Walsall", "Wolverhampton", "Dudley", "Women's", "UH Birmingham", "Sandwell", "Other")))|>
  mutate(birth=factor(birth, levels=c("Walsall", "Wolverhampton", "Dudley", "Women's", "UH Birmingham", "Sandwell", "Other")))|>
  select(-birth_provider_grouped, -booking_provider_grouped)|>
  pivot_wider(names_from = birth, values_from = n, values_fill = 0, names_expand = TRUE) |> 
  arrange(booking)|>
  as_tabyl()

## Add totals
matrix_with_totals <- matrix |> 
  adorn_totals(where = c("row", "col"))|>
  mutate(across(-booking, as.numeric))

## Add perc
matrix_with_total_and_perc <- matrix_with_totals |>
  adorn_percentages(denominator = "row", affect_cols = -1) |> 
  adorn_pct_formatting(digits = 1) |>             
  adorn_ns(position = "front", ns = matrix_with_totals)|>
  mutate(across(where(is.character) & -booking, ~ if_else(booking == "Total", stringr::str_remove(.x, "\\s*\\(.*\\)"), .x))) |>
  #mutate(Total = stringr::str_remove(Total, "\\s*\\(.*\\)"))|>
  mutate(Side_Title = "Trust where booking appt occurred") |> 
  mutate(Side_Title= ifelse(booking=="Total", NA, Side_Title))|>
  select(Side_Title, everything())

## Create a color mapping function
color_fun <- col_numeric(palette = c("#FCEFBA", "#ec6555"), domain = c(0, 6000))

## Get raw matrix with percentage for colour
raw_matrix <- matrix_with_totals |>
  #adorn_percentages(denominator = "row")|>
  select(-booking, -Total)

## Add colours  
color_matrix <- apply(raw_matrix , 2, color_fun)
color_matrix[nrow(color_matrix), ] <- "#FFFFFF"

## Make flextable matrix
flextable(matrix_with_total_and_perc ) |>
  merge_v(j = "Side_Title", part = "body") |> 
  rotate(j = "Side_Title", rotation = "btlr", align = "center", part = "body") |> 
  valign(j = "Side_Title", valig = "center", part = "body") |> 
  width(j=3:9, width = 3, unit = "cm")|>
  width(j = "Side_Title", width = 0.4) |> 
  add_header_row(top = TRUE, values = c("", "Trust where birth spell occurred"), colwidths = c(2, 8)) |>   
  set_header_labels(Side_Title = "", booking= "") |> 
  bg(j = 3:9, bg = color_matrix, part="body")|>
  align(align = "center", part = "header")|>
  vline(j = 9,  part = "all")|>
 hline(i = 7,  part = "body")|>
  bold(i =8) |> 
  bold(j = "Total")

# ICB MATRIX--------------------------------------------------------------------

## Trust names
icb_names <- data.frame(
  codes = c('QHL', 'QUA', 'QWU', 'QJ2', 'QGH', 'QOC', 'QNC', 'Other'),
  names = c("Birmingham & Solihull", "Black Country", "Coventry & Warwickshire", 
            "Derby & Derbyshire", "Herefordshire & Worcestershire ", "Shropshire, Telford & Wrekin", "Staffordshire & Stoke-on-Trent", "Other")
)

## Make matrix
icb_matrix <-icb_matrix_data |> 
  left_join(icb_names, by=c("resident_icb_interest"="codes"))|>
  rename(resident=names)|>
  left_join(icb_names, by=c("birth_icb_interest"="codes"))|>
  rename(birth=names)|>
  mutate(resident=factor(resident, levels=c("Birmingham & Solihull", "Black Country", "Coventry & Warwickshire", 
                                            "Derby & Derbyshire", "Herefordshire & Worcestershire ", "Shropshire, Telford & Wrekin", "Staffordshire & Stoke-on-Trent", "Other")))|>
  mutate(birth=factor(birth, levels=c("Birmingham & Solihull", "Black Country", "Coventry & Warwickshire", 
                                      "Derby & Derbyshire", "Herefordshire & Worcestershire ", "Shropshire, Telford & Wrekin", "Staffordshire & Stoke-on-Trent", "Other")))|>
  select(-resident_icb_interest, -birth_icb_interest)|>
  pivot_wider(names_from = birth, values_from = n, values_fill = 0, names_expand = TRUE) |> 
  arrange(resident)|>
  as_tabyl()

## Add totals
icb_matrix_with_totals <- icb_matrix |> 
  adorn_totals(where = c("row", "col"))|>
  mutate(across(-resident, as.numeric))

## Add perc
icb_matrix_with_total_and_perc <- icb_matrix_with_totals |>
  adorn_percentages(denominator = "row", affect_cols = -1) |> 
  adorn_pct_formatting(digits = 1) |>             
  adorn_ns(position = "front", ns = icb_matrix_with_totals)|>
  mutate(across(where(is.character) & -resident, ~ if_else(resident == "Total", stringr::str_remove(.x, "\\s*\\(.*\\)"), .x))) |>
  #mutate(Total = stringr::str_remove(Total, "\\s*\\(.*\\)"))|>
  mutate(Side_Title = "Resident ICB of mother") |> 
  mutate(Side_Title= ifelse(resident=="Total", NA, Side_Title))|>
  select(Side_Title, everything())

## Create a color mapping function
icb_color_fun <- col_numeric(palette = c("#FCEFBA", "#ec6555"), domain = c(0, 13000))

## Get raw matrix with percentage for colour
icb_raw_matrix <- icb_matrix_with_totals |>
  #adorn_percentages(denominator = "row")|>
  select(-resident, -Total)

## Add colours  
icb_color_matrix <- apply(icb_raw_matrix , 2, icb_color_fun)
icb_color_matrix[nrow(icb_color_matrix), ] <- "#FFFFFF"

## Make flextable matrix
flextable(icb_matrix_with_total_and_perc ) |>
  merge_v(j = "Side_Title", part = "body") |> 
  rotate(j = "Side_Title", rotation = "btlr", align = "center", part = "body") |> 
  valign(j = "Side_Title", valig = "center", part = "body") |> 
  width(j=3:10, width = 3, unit = "cm")|>
  width(j = "Side_Title", width = 0.4) |> 
  add_header_row(top = TRUE, values = c("", "ICB where birth occurred"), colwidths = c(2, 9)) |>   
  set_header_labels(Side_Title = "", resident= "") |> 
  bg(j = 3:10, bg = icb_color_matrix, part="body")|>
  align(align = "center", part = "header")|>
  vline(j = 10,  part = "all")|>
  hline(i = 8,  part = "body")|>
  bold(i =9) |> 
  bold(j = "Total")

## TRUST MATRIX WITH DOUBLE PERCENTAGES------------------------------------------

## Make matrix
matrix <-trust_matrix_data |> 
  left_join(trust_names, by=c("birth_provider_grouped"="codes"))|>
  rename(birth=names)|>
  left_join(trust_names, by=c("booking_provider_grouped"="codes"))|>
  rename(booking=names)|>
  mutate(booking=factor(booking, levels=c("Walsall", "Wolverhampton", "Dudley", "Women's", "UH Birmingham", "Sandwell", "Other")))|>
  mutate(birth=factor(birth, levels=c("Walsall", "Wolverhampton", "Dudley", "Women's", "UH Birmingham", "Sandwell", "Other")))|>
  select(-birth_provider_grouped, -booking_provider_grouped)|>
  pivot_wider(names_from = birth, values_from = n, values_fill = 0, names_expand = TRUE) |> 
  arrange(booking)|>
  as_tabyl()

## Add totals
matrix_with_totals <- matrix |> 
  adorn_totals(where = c("row", "col"))|>
  mutate(across(-booking, as.numeric))


# Extract just the numeric grid (including totals)
grid_n <- as.matrix(matrix_with_totals[, -1]) 

# Row totals are in the last column, Column totals in the last row
row_totals <- grid_n[, ncol(grid_n)]
col_totals <- grid_n[nrow(grid_n), ]

# Calculate absolute row % and col % matrices
pct_row <- matrix(NA, nrow = nrow(grid_n), ncol = ncol(grid_n))
pct_col <- matrix(NA, nrow = nrow(grid_n), ncol = ncol(grid_n))

for(i in 1:nrow(grid_n)) {
  for(j in 1:ncol(grid_n)) {
    # Row % (avoiding 0 division)
    pct_row[i, j] <- if(row_totals[i] > 0) grid_n[i, j] / row_totals[i] else 0
    # Col % (avoiding 0 division)
    pct_col[i, j] <- if(col_totals[j] > 0) grid_n[i, j] / col_totals[j] else 0
  }
}

# Convert back to data frames with matching structures
grid_pct_row <- as.data.frame(pct_row) |> setNames(names(matrix_with_totals)[-1])
grid_pct_col <- as.data.frame(pct_col) |> setNames(names(matrix_with_totals)[-1])


## Prepare structural text data frame for Flextable
matrix_formatted <- matrix_with_totals |> 
  mutate(Side_Title = "Trust where booking appt occurred") |> 
  mutate(Side_Title = ifelse(booking == "Total", NA, Side_Title)) |> 
  select(Side_Title, everything()) 


## Create a color mapping function (apply only to inner counts, excluding totals)
color_fun <- col_numeric(palette = c("#FCEFBA", "#ec6555"), domain = c(0, 6000))
raw_matrix <- matrix_with_totals |> select(-booking, -Total)

## Add colours (Maintain White for Total row and Total column if needed)
color_matrix <- apply(raw_matrix, 2, color_fun)
color_matrix[nrow(color_matrix), ] <- "#FFFFFF" # White out total row


## Make flextable matrix
ft <- flextable(matrix_formatted) |>
  merge_v(j = "Side_Title", part = "body") |> 
  rotate(j = "Side_Title", rotation = "btlr", align = "center", part = "body") |> 
  valign(j = "Side_Title", valig = "center", part = "body") |> 
  width(j = 3:9, width = 3.2, unit = "cm") |> 
  width(j = "Side_Title", width = 0.4, unit = "cm") |> # Added unit = "cm" here to fix the crash!
  width(j = "Total", width = 2.2, unit = "cm") |>
  add_header_row(top = TRUE, values = c("", "Trust where birth spell occurred"), colwidths = c(2, 8)) |>   
  set_header_labels(Side_Title = "", booking= "") |> 
  bg(j = 3:9, bg =  color_matrix, part = "body") |>
  align(align = "center", part = "header")|>
  vline(j = 9,  part = "all")|>
  hline(i = 7,  part = "body")|>
  bold(i =8) |> 
  bold(j = "Total")


# Target columns 3 to 9 (The main trust grids)
target_cols <- names(matrix_with_totals)[c(-1, -ncol(matrix_with_totals))] 

for(col in target_cols) {
  ft <- ft |> mk_par(
    j = col, 
    i = 1:7, # Keeps it from executing on the "Total" row
    value = as_paragraph(
      # Top Line: Raw count and Row Percent
      as_chunk(get(col), formatter = function(x) sprintf("%.0f ", x)),
      as_chunk(grid_pct_row[[col]][1:7], formatter = function(x) sprintf("(%.1f%%)", x * 100)),
      "\n",
      # Bottom Line: Column Percent 
      colorize(
        as_chunk(grid_pct_col[[col]][1:7], formatter = function(x) sprintf("(▼%.1f%%)", x * 100)),
        color = "#686f73" # Change "gray" to "red", "#888888", etc.
      )
      
    )
  )
}



# Drop percentage strings out of the Total column and Total Row 
for(col in names(matrix_with_totals)[-1]) {
  ft <- ft |> 
    mk_par(
      i = 8, 
      j = col, 
      value = as_paragraph(
        colorize(
          as_chunk(matrix_with_totals[[col]][8], formatter = function(x) sprintf("%.0f", x)),
          color = "#686f73"
        )
      )
    )
}
ft <- ft |> 
  mk_par(j = "Total", i = 1:7, value = as_paragraph(as_chunk(matrix_with_totals[["Total"]][1:7], formatter = function(x) sprintf("%.0f", x))))

# Print table
ft