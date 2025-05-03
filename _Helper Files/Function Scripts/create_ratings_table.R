
# Men's Basketball Theme ---------
rank_theme_mbb <- function(gt_object,...) {
  
  gt_object %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Big Shoulders"),
        gt::default_fonts()
      ),
      weight = 300
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(
        color = 'ivory1'),
      locations = cells_body(
        columns = gt::everything()
      )) %>% 
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
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(2)
      ),
      locations = gt::cells_body(
        rows = c(rank %% 10 == 0)
      )
    ) %>% 
    gt::tab_options(
      column_labels.background.color = "#9C7B59",
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
      row_group.border.bottom.color = "ivory1",
      stub.border.color = "ivory1",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.border.lr.style = "none",
      source_notes.background.color = "black",
      table.font.size = 20,
      heading.align = "center",
      heading.background.color = "black",
      ...
    )
}

# Women's Basketball theme ------------------------
rank_theme_wbb <- function(gt_object,...) {
  
  gt_object %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Big Shoulders"),
        gt::default_fonts()
      ),
      weight = 300
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(
        color = 'ivory1'),
      locations = cells_body(
        columns = gt::everything()
      )) %>% 
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
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(2)
      ),
      locations = gt::cells_body(
        rows = c(rank %% 10 == 0)
      )
    ) %>% 
    gt::tab_options(
      column_labels.background.color = "lightgoldenrod",
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
      row_group.border.bottom.color = "ivory1",
      stub.border.color = "ivory1",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.border.lr.style = "none",
      source_notes.background.color = "black",
      table.font.size = 20,
      heading.align = "center",
      heading.background.color = "cyan3",
      ...
    )
}


# Ratings table --------------------
create_ratings_table <- function(gtobject, ...){
  gtobject %>% 
    gt::gt() %>%
    # see below
    rank_theme() %>%
    gt::tab_header(title = html(gt_title)) %>%
    gt::cols_label(
      rank = 'Rank', logo = '', team = 'Team',
      total = 'W-L', reg_season = '',
      seed = '',
      conf_tourn = '', title = '',
      conference_short_name = 'Conf',
      oreb_rt = 'OFF', dreb_rt = 'DEF',
      to_rt = 'OFF', to_opp_rt = 'DEF',
      tm_efg = 'OFF', opp_efg = 'DEF', ast_rt = 'OFF',
      ortg = 'OFF', drtg = 'DEF', netrtg = 'NET') %>% 
    gt::cols_width(
      gt::contains("team") ~ gt::px(170),
      c(total:netrtg) ~ gt::px(60),
      contains('conference') ~ gt::px(110)
    ) %>% 
    gtExtras::gt_img_rows(
      columns = logo,
      img_source = "web",
      height = 30) %>%
    gt::fmt_icon(
      columns = title,
      rows = c(title != ''),
      stroke_width = '2px',
      stroke_color = 'goldenrod',
      fill_color = 'gold',
      height = '1em') %>% 
    gt::fmt_icon(
      columns = conf_tourn,
      rows = c(conf_tourn != ''),
      stroke_color = 'gray',
      stroke_width = '2px',
      fill_color = 'gray',
      height = '0.75em') %>% 
    gt::fmt_icon(
      columns = reg_season,
      rows = c(reg_season != ''),
      stroke_color = 'goldenrod',
      stroke_width = '2px',
      fill_color = 'goldenrod',
      height = '0.75em') %>% 
    gt::cols_merge(
      columns = c(team:title),
      pattern = "{1} <sub>{2}</sub> {3} {4} {5}"
    ) %>% 
    gt::tab_spanner(
      label = 'REB RATE',
      columns = oreb_rt:dreb_rt) %>%
    gt::tab_spanner(
      label = 'TO RATE',
      columns = to_rt:to_opp_rt) %>% 
    gt::tab_spanner(
      label = 'TS %',
      columns = tm_efg:opp_efg) %>% 
    gt::tab_spanner(
      label = 'AST RT',
      columns = ast_rt) %>% 
    gt::tab_spanner(
      label = 'EFFICIENCY',
      columns = ortg:netrtg) %>%
    gt::fmt_number(
      columns = pace,
      decimals = 0) %>% 
    gt::fmt_number(
      columns = ortg:netrtg,
      decimals = 1) %>% 
    gt::fmt_percent(
      columns = c(oreb_rt:opp_efg),
      decimals = 1) %>% 
    gt::tab_style(
      style = gt::cell_borders(
        sides = "right", color = "black", weight = gt::px(2),
        style = 'dashed'
      ),
      locations = gt::cells_body(
        columns = c(conference_short_name, total,
                    pace, dreb_rt, to_opp_rt, ast_rt,
                    opp_efg))
    ) %>% 
    gt::tab_style(
      style = gt::cell_borders(
        sides = "right", color = "black", weight = gt::px(1),
        style = 'dashed'
      ),
      locations = gt::cells_body(
        columns = c(drtg))
    ) %>% 
    gt::cols_align(
      align = 'center',
      columns = c(rank, logo, total, reg_season:title,
                  conference_short_name,
                  pace:netrtg)) %>% 
    tab_style(
      style = list(
        "border-radius: 15px;"
      ),
      locations = cells_body(columns = logo)
    )
}

saveRDS(create_ratings_table, 'Other/create_ratings_table.rds')
saveRDS(rank_theme_mbb, 'Other/rank_theme_mbb.rds')
saveRDS(rank_theme_wbb, 'Other/rank_theme_wbb.rds')