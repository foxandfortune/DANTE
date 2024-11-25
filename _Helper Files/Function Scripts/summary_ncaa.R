summary.ncaa_simulation <- function(object, ...){
  rlang::check_installed(c("gt", "scales (>= 1.2.0)", "hoopR"), "to compute a summary table.")
  
  source_note <- paste(
    "summary of",
    scales::number(
      object$sim_params$simulations,
      scale_cut = scales::cut_short_scale()
    ),
    "simulations using ncaaSimulatoR"
  )
  
  team_logos <- hoopR::espn_mbb_teams(year = object$sim_params$ncaa_season) %>% 
    select(team_id, logo)
  
  teams <- object$teams %>%
    select(-c(sim, exit, first_four, winner_to)) %>% 
    distinct() %>% 
    left_join(team_logos, by = "team_id")
  
  data <- object$overall %>%
    left_join(teams %>% 
                select(team_id, team_name, seed, logo), by = "team_id") %>% 
    mutate(
      bracket_side = case_when(
        region %in% object$sim_params$left_bracket ~ "Left",
        region %in% object$sim_params$right_bracket ~ "Right",
        TRUE ~ NA_character_))
  
  # This returns a named vector. Names are column names in `data` and values will
  # be FALSE if any value in the corresponding column is not NA, TRUE otherwise
  column_is_empty <- colSums(!is.na(data)) > 0
  
  # Get character vector of columns that hold only NA and hide them
  hide_me <- names(column_is_empty[column_is_empty == FALSE])
  
  left_bkt <- data %>%
    filter(bracket_side == "Left") %>%
    select(-bracket_side) %>%
    arrange(region, desc(round_2),
            desc(sweet_sixteen), desc(elite_eight),
            desc(final_four), desc(final),
            desc(won_title))
  
  right_bkt <- data %>%
    filter(bracket_side == "Right") %>%
    select(-bracket_side) %>%
    arrange(region, desc(round_2),
            desc(sweet_sixteen), desc(elite_eight),
            desc(final_four), desc(final),
            desc(won_title))
  
  regions <- c(head(left_bkt$region, 1), tail(left_bkt$region, 1),
               head(right_bkt$region, 1), tail(right_bkt$region, 1))
  
  left.length.1 <- sum(left_bkt$region == regions[1])
  left.length.2 <- sum(left_bkt$region == regions[2])
  right.length.1 <- sum(right_bkt$region == regions[3])
  right.length.2 <- sum(right_bkt$region == regions[4])
  
  max.len <- max(left.length.1, left.length.2,
                 right.length.2, right.length.1)
  
  tbl <- data.frame(
    seed_l1 = c(head(left_bkt$seed, left.length.1),
                rep(NA, max.len - left.length.1)),
    logo_l1  = c(head(left_bkt$logo, left.length.1),
                 rep(NA, max.len - left.length.1)),
    team_l1  = c(head(left_bkt$team_name, left.length.1),
                 rep(NA, max.len - left.length.1)),
    rd2_l1 = c(head(left_bkt$round_2, left.length.1),
                 rep(NA, max.len - left.length.1)),
    sweet_l1 = c(head(left_bkt$sweet_sixteen, left.length.1),
                 rep(NA, max.len - left.length.1)),
    eight_l1 = c(head(left_bkt$elite_eight, left.length.1),
                 rep(NA, max.len - left.length.1)),
    final4_l1 = c(head(left_bkt$final_four, left.length.1),
                  rep(NA, max.len - left.length.1)),
    final_l1 = c(head(left_bkt$final, left.length.1),
                 rep(NA, max.len - left.length.1)),
    winner_l1 = c(head(left_bkt$won_title, left.length.1),
                  rep(NA, max.len - left.length.1)),
    seed_l2 = c(tail(left_bkt$seed, left.length.2),
                rep(NA, max.len - left.length.2)),
    logo_l2  = c(tail(left_bkt$logo, left.length.2),
                 rep(NA, max.len - left.length.2)),
    team_l2  = c(tail(left_bkt$team_name, left.length.2),
                 rep(NA, max.len - left.length.2)),
    rd2_l2 = c(tail(left_bkt$round_2, left.length.2),
                 rep(NA, max.len - left.length.2)),
    sweet_l2 = c(tail(left_bkt$sweet_sixteen, left.length.2),
                 rep(NA, max.len - left.length.2)),
    eight_l2 = c(tail(left_bkt$elite_eight, left.length.2),
                 rep(NA, max.len - left.length.2)),
    final4_l2 = c(tail(left_bkt$final_four, left.length.2),
                  rep(NA, max.len - left.length.2)),
    final_l2 = c(tail(left_bkt$final, left.length.2),
                 rep(NA, max.len - left.length.2)),
    winner_l2 = c(tail(left_bkt$won_title, left.length.2),
                  rep(NA, max.len - left.length.2)),
    seed_r1 = c(head(right_bkt$seed, right.length.1),
                rep(NA, max.len - right.length.1)),
    logo_r1  = c(head(right_bkt$logo, right.length.1),
                 rep(NA, max.len - right.length.1)),
    team_r1  = c(head(right_bkt$team_name, right.length.1),
                 rep(NA, max.len - right.length.1)),
    rd2_r1 = c(head(right_bkt$round_2, right.length.1),
                 rep(NA, max.len - right.length.1)),
    sweet_r1 = c(head(right_bkt$sweet_sixteen, right.length.1),
                 rep(NA, max.len - right.length.1)),
    eight_r1 = c(head(right_bkt$elite_eight, right.length.1),
                 rep(NA, max.len - right.length.1)),
    final4_r1 = c(head(right_bkt$final_four, right.length.1),
                  rep(NA, max.len - right.length.1)),
    final_r1 = c(head(right_bkt$final, right.length.1),
                 rep(NA, max.len - right.length.1)),
    winner_r1 = c(head(right_bkt$won_title, right.length.1),
                  rep(NA, max.len - right.length.1)),
    seed_r2 = c(tail(right_bkt$seed, right.length.2),
                rep(NA, max.len - right.length.2)),
    logo_r2  = c(tail(right_bkt$logo, right.length.2),
                 rep(NA, max.len - right.length.2)),
    team_r2  = c(tail(right_bkt$team_name, right.length.2),
                 rep(NA, max.len - right.length.2)),
    rd2_r2 = c(tail(right_bkt$round_2, right.length.2),
                 rep(NA, max.len - right.length.2)),
    sweet_r2 = c(tail(right_bkt$sweet_sixteen, right.length.2),
                 rep(NA, max.len - right.length.2)),
    eight_r2 = c(tail(right_bkt$elite_eight, right.length.2),
                 rep(NA, max.len - right.length.2)),
    final4_r2 = c(tail(right_bkt$final_four, right.length.2),
                  rep(NA, max.len - right.length.2)),
    final_r2 = c(tail(right_bkt$final, right.length.2),
                 rep(NA, max.len - right.length.2)),
    winner_r2 = c(tail(right_bkt$won_title, right.length.2),
                  rep(NA, max.len - right.length.2))
  )
  
  tbl %>%
    gt::gt() %>%
    # see below
    table_theme() %>%
    gt::sub_missing(columns = c(contains("seed"),
                                contains("logo"),
                                contains("team")), missing_text = "") %>% 
    gt::sub_missing(columns = c(contains("rd2"),
                                contains("sweet"),
                                contains("eight"),
                                contains("final"),
                                contains("winner")), missing_text = " ") %>% 
    gt::tab_header(title = gt::html(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/commons/2/28/March_Madness_logo.svg' height = '50'></br>
                                MARCH MADNESS {object$sim_params$ncaa_season} SIMULATION"))) %>% 
    gt::cols_label(
      seed_l1 = gt::md("**SEED**"),
      seed_l2 = gt::md("**SEED**"),
      seed_r1 = gt::md("**SEED**"),
      seed_r2 = gt::md("**SEED**"),
      
      logo_l1 = "",
      logo_l2 = "",
      logo_r1 = "",
      logo_r2 = "",
      
      team_l1 = gt::md("**TEAM**"),
      team_l2 = gt::md("**TEAM**"),
      team_r1 = gt::md("**TEAM**"),
      team_r2 = gt::md("**TEAM**"),
      
      rd2_l1 = gt::html("RD 2"),
      rd2_l2 = gt::html("RD 2"),
      rd2_r1 = gt::html("RD 2"),
      rd2_r2 = gt::html("RD 2"),
      
      sweet_l1 = gt::html("SWEET 16"),
      sweet_l2 = gt::html("SWEET 16"),
      sweet_r1 = gt::html("SWEET 16"),
      sweet_r2 = gt::html("SWEET 16"),
      
      eight_l1 = gt::html("ELITE 8"),
      eight_l2 = gt::html("ELITE 8"),
      eight_r1 = gt::html("ELITE 8"),
      eight_r2 = gt::html("ELITE 8"),
      
      final4_l1 = gt::html("FINAL 4"),
      final4_l2 = gt::html("FINAL 4"),
      final4_r1 = gt::html("FINAL 4"),
      final4_r2 = gt::html("FINAL 4"),
      
      final_l1 = gt::html("MAKE<br>FINAL"),
      final_l2 = gt::html("MAKE<br>FINAL"),
      final_r1 = gt::html("MAKE<br>FINAL"),
      final_r2 = gt::html("MAKE<br>FINAL"),
      
      winner_l1 = gt::html("WIN<br>TITLE"),
      winner_l2 = gt::html("WIN<br>TITLE"),
      winner_r1 = gt::html("WIN<br>TITLE"),
      winner_r2 = gt::html("WIN<br>TITLE")
    ) %>%
    gt::tab_spanner(label = toupper(regions[1]),
                    columns = gt::ends_with("_l1")) %>% 
    gt::tab_spanner(label = toupper(regions[2]),
                    columns = gt::ends_with("_l2")) %>%
    gt::tab_spanner(label = toupper(regions[3]),
                    columns = gt::ends_with("_r1")) %>%
    gt::tab_spanner(label = toupper(regions[4]),
                    columns = gt::ends_with("_r2")) %>%
    gt::fmt_percent(
      decimals = 0,
      columns = c(
        gt::contains("rd2"),
        gt::contains("sweet"),
        gt::contains("eight"),
        gt::contains("final"),
        gt::contains("winner")),
      rows = starts_with(" ")
    ) %>%
    gt::fmt_percent(
      decimals = 0,
      columns = c(
        gt::contains("rd2"),
        gt::contains("sweet"),
        gt::contains("eight"),
        gt::contains("final"),
        gt::contains("winner")),
      rows = everything()
    ) %>%
    #gt_fmt_pct_special(
    #  columns = c(
    #    gt::contains("sweet"),
    #    gt::contains("eight"),
    #    gt::contains("final"),
    #    gt::contains("winner")
    #  ),
    #  rows = !starts_with(" ")
    #) %>% 
    gt::cols_width(
      gt::contains("rd2") ~ gt::px(60),
      gt::contains("sweet") ~ gt::px(60),
      gt::contains("eight") ~ gt::px(60),
      gt::contains("final") ~ gt::px(60),
      gt::contains("winner") ~ gt::px(60)
    ) %>%
    gt::cols_align(
      align = "center",
      columns = c(
        gt::contains("seed"),
        gt::contains("logo"),
        gt::contains("rd2"),
        gt::contains("sweet"),
        gt::contains("eight"),
        gt::contains("final"),
        gt::contains("winner")
      )
    ) %>%
    gt::data_color(
      columns = c(
        gt::contains("rd2"),
        gt::contains("sweet"),
        gt::contains("eight"),
        gt::contains("final"),
        gt::contains("winner")
      ),
      colors = scales::col_numeric(palette = table_colors_positive, domain = c(0, 1),
                                   na.color = "transparent")
    ) %>%
    gtExtras::gt_img_rows(
      columns = logo_l1,
      img_source = "web",
      height = 30) %>%
    gtExtras::gt_img_rows(
      columns = logo_l2,
      img_source = "web",
      height = 30) %>%
    gtExtras::gt_img_rows(
      columns = logo_r1,
      img_source = "web",
      height = 30) %>%
    gtExtras::gt_img_rows(
      columns = logo_r2,
      img_source = "web",
      height = 30) %>%
    gt::tab_source_note(tools::toTitleCase(source_note)) %>%
    gt::tab_style(
      locations = list(gt::cells_body(gt::contains("seed_")),
                       gt::cells_column_labels(gt::contains("seed_"))),
      style = gt::cell_borders(c("left"), weight = gt::px(2))
    )  %>%
    gt::tab_style(
      locations = list(gt::cells_body("winner_r2"),
                       gt::cells_column_labels(gt::contains("winner_r2"))),
      style = gt::cell_borders(c("right"), weight = gt::px(2))
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(columns = c(gt::contains("seed"),
                                             gt::contains("rd2"),
                                             gt::contains("sweet"),
                                             gt::contains("eight"),
                                             gt::contains("final"),
                                             gt::contains("winner"))),
      style = gt::cell_text(weight = "bold")
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(columns = c(gt::contains("seed"))),
      style = gt::cell_text(weight = "bold")
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        align = "right",
        size = "medium",
        font = list(
          gt::google_font("Audiowide"),
          gt::default_fonts()
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(spanners = everything()),
      style = gt::cell_text(
        align = "center",
        size = "large",
        font = list(
          gt::google_font("Audiowide"),
          gt::default_fonts()
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(align = "center", weight = "bold"),
        gt::cell_fill(color = "#F0F0F0")
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = gt::cell_text(align = "center", weight = "bold")
    )
  
}

saveRDS(summary.ncaa_simulation, 'Simulation Backup/Functions/summary.ncaa_simulation.rds')

# Taken from Thomas Mock's package gtExtras to avoid the dependency
# on a non cran package.
# https://github.com/jthomasmock/gtExtras/blob/HEAD/R/gt_theme_538.R
table_theme <- function(gt_object,...) {
  
  gt_object %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Chivo"),
        gt::default_fonts()
      ),
      weight = 300
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top", color = "black", weight = gt::px(0)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(1)
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.background.color = "white",
      heading.border.bottom.style = "none",
      table.border.top.width = gt::px(3),
      table.border.top.style = "none", #transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.border.lr.style = "none",
      source_notes.background.color = "gray30",
      table.font.size = 16,
      heading.align = "center",
      heading.background.color = "gray30",
      ...
    )
}

saveRDS(table_theme, 'Simulation Backup/Functions/table_theme.rds')

# output of ggsci::rgb_material("light-blue") + "white"
table_colors_positive <- c("white",
                           "#E0F4FEFF", "#B2E5FCFF", "#80D3F9FF", "#4EC3F7FF", "#28B6F6FF", "#02A9F3FF",
                           "#029AE5FF", "#0187D1FF", "#0177BDFF", "#00579AFF"
)

saveRDS(table_colors_positive, 'Simulation Backup/Functions/table_colors_positive.rds')
