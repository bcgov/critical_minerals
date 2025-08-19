clean_text_column <- function(data, column) {
  data %>%
    mutate(
      {{ column }} := {{ column }} %>%
        str_to_lower() %>%                      # lowercase
        str_replace_all("[^a-z]"," ") |>
        trimws() |>
        gsub("\\s+", " ", x = _)
       )
}

clean_key <- function(x) {
  x %>%
    as.character() %>%         # Convert factors or other types to character
    iconv(to = "UTF-8") %>%    # Normalize encoding
    stringr::str_squish() %>%  # Remove excess whitespace
    tolower()                  # Normalize case
}

mine_type <- function(mine_name){
  read_excel(here("data", "Upcoming Mine Data - Employment.xlsx"), skip=1)|>
    clean_names()|>
    filter(upcoming_mine==mine_name)|>
    clean_text_column(open_pit_or_underground_mine)|>
    clean_text_column(commodities)|>
    select(mine_type=open_pit_or_underground_mine, commodities)
}

write_group_striped_excel <- function(data, group_col, file, sheet_name = "Sheet1") {
  stopifnot(group_col %in% names(data))
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, startRow = 1)
  stripe1 <- createStyle(fgFill = "#D9E1F2")
  stripe2 <- createStyle(fgFill = "#FFFFFF")
  start_row <- 2
  group_vals <- data[[group_col]]
  group_change <- c(TRUE, group_vals[-1] != group_vals[-length(group_vals)])
  group_ids <- cumsum(group_change)

  for (i in seq_len(nrow(data))) {
    style <- if (group_ids[i] %% 2 == 1) stripe1 else stripe2
    excel_row <- start_row + i - 1
    addStyle(wb, sheet = sheet_name, style = style,
             rows = excel_row, cols = 1:ncol(data), gridExpand = TRUE)
  }
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = "auto")
  saveWorkbook(wb, file = file, overwrite = TRUE)
}

agg_and_write <- function(tbbl){
  file_name <- paste(deparse(substitute(tbbl)), "nocs.xlsx", sep="_")
  tbbl|>
    group_by(non_standard_job_title, commodities, location, mine_type, noc)|>
    summarize(staff=sum(staff, na.rm = TRUE), .groups = "drop")|>
    ungroup()|>
    left_join(nocs_to_names)|>
    arrange(noc)|>
    select(commodities, mine_type, location, staff, non_standard_job_title, noc, noc_name)|>
    write_group_striped_excel("noc", here("out", file_name))
}

map_nocs <- function(jobs_df, mapping_df) {
  jobs_df <- jobs_df|>
    mutate(across(c(non_standard_job_title, mine_type, location), clean_key))
  mapping_df <- mapping_df|>
    mutate(across(c(non_standard_job_title, mine_type, location), clean_key))
  fuzzy_left_join(
    jobs_df, mapping_df,
    by = c("non_standard_job_title" = "non_standard_job_title",
           "mine_type" = "mine_type",
           "location"  = "location"),
    match_fun = list(
      `==`, #exact matches on non_standard_job_title
      function(x, y) {
        is.na(y) | x == y #matches nas or mine_type exactly
      },
      function(x, y) {
        is.na(y) | x == y #matches nas or location exactly
      }
    )
  )%>%
    select(non_standard_job_title = non_standard_job_title.x,
           staff = staff,
           mine_type = mine_type.x,
           commodities,
           location  = location.x,
           noc)
}
