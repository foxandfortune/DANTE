# Ratings table (Preseason) --------------------
create_ratings_table_preseason <- function(gtobject, ...){
  gtobject %>% 
    gt::gt() %>%
    # see below
    rank_theme() %>%
    gt::tab_header(title = html(gt_title)) %>%
    gt::cols_label(
      rank = 'Rank', logo = '', team = 'Team',
      
      conference_short_name = 'Conf',
      
      oreb_rt = 'OFF', dreb_rt = 'DEF',
      to_rt = 'OFF', to_opp_rt = 'DEF',
      tm_efg = 'OFF', opp_efg = 'DEF', ast_rt = 'OFF',
      ortg = 'OFF', drtg = 'DEF', netrtg = 'NET') %>% 
    gt::cols_width(
      gt::contains("team") ~ gt::px(170),
      c(logo, pace:netrtg) ~ gt::px(60),
      contains('rank') ~ gt::px(30),
      contains('conference') ~ gt::px(110)
    ) %>% 
    gtExtras::gt_img_rows(
      columns = logo,
      img_source = "web",
      height = 30) %>%
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
        columns = c(conference_short_name,
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
      columns = c(rank, logo,
                  conference_short_name,
                  pace:netrtg)) %>% 
    tab_style(
      style = list(
        "border-radius: 15px;"
      ),
      locations = cells_body(columns = logo)
    )
}

saveRDS(create_ratings_table_preseason,
        '_Helper Files/Other/create_ratings_table_preseason.rds')


# Ratings table (Inseason) --------------------
create_ratings_table_inseason <- function(gtobject, ...){
  gtobject %>% 
    gt::gt() %>%
    # see below
    rank_theme() %>%
    gt::tab_header(title = html(gt_title)) %>%
    gt::cols_label(
      rank = 'Rank', logo = '', team = 'Team',
      total = 'W-L',
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
      columns = c(rank, logo, total, 
                  conference_short_name,
                  pace:netrtg)) %>% 
    tab_style(
      style = list(
        "border-radius: 15px;"
      ),
      locations = cells_body(columns = logo)
    )
}

saveRDS(create_ratings_table_inseason,
        '_Helper Files/Other/create_ratings_table_inseason.rds')
