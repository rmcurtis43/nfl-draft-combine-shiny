rating_stars <- function(rating, max_rating = 5) {
  rounded_rating <- floor(rating + 0.5)  
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) fontawesome::fa("star", fill= "orange") else fontawesome::fa("star", fill= "grey")
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div_out <- htmltools::div(title = label, "aria-label" = label, role = "img", stars)
  
  as.character(div_out) %>% 
    gt::html()
}


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


valueBox3 <- function(value, title, sparkobj = NULL, subtitle, icon = NULL,
                      color = "aqua", width = 12, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(2, 2, 2, 2),
      spacingTop = 5,
      spacingRight = 12,
      spacingBottom = 3,
      spacingLeft = 12,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      borderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}



benchmark_table_global <- function(test = '40yd', positions = c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')){
  
  
  # pull combineR data
  data <- combine_clean
  
  percentile_data <- data %>%
    select(position=position2, height=height_in, weight=weight_lbs, vertical=vertical_in, broad_jump=broad_jump_in, bench, `3cone`=x3cone, shuttle, `40yd`=x40yd) %>%
    pivot_longer(!position, names_to = "Test", values_to = "Value", values_drop_na = TRUE) %>%
    left_join(data %>%
                select(position=position2, height=height_in, weight=weight_lbs, vertical=vertical_in, broad_jump=broad_jump_in, bench, `3cone`=x3cone, shuttle, `40yd`=x40yd) %>%
                pivot_longer(!position, names_to = "Test", values_to = "Value", values_drop_na = TRUE) %>%
                group_by(position, Test) %>%
                summarise(
                  mean = mean(Value),
                  sd = sd(Value),
                  count = n()
                ), c('position', 'Test')) %>%
    #filter(!position %in% c('PK', 'LS')) %>%
    mutate(percentile = round((
      pnorm(Value, mean = mean, sd = sd) *
        100
    ), 0))
  
  
  p <- c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  p_names <- map_chr(p, ~paste0(.x*100, "th"))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)
  
  
  p_funs_rev <- map(p, ~partial(quantile, probs = (1-.x), na.rm = TRUE)) %>%
    set_names(nm = p_names)
  
  
  percentile_data_fwd <- data %>%
    select(position=position2, Height=height_in, Weight=weight_lbs, `Vertical Jump`=vertical_in, `Broad Jump`=broad_jump_in, Bench=bench) %>%
    group_by(position) %>%
    summarize_at(vars(Height:Bench), funs(!!!p_funs))
  
  
  percentile_data_rev <- data %>%
    select(position=position2,`3cone`=x3cone, Shuttle=shuttle, `40yd`=x40yd) %>%
    group_by(position) %>%
    summarize_at(vars(`3cone`:`40yd`), funs(!!!p_funs_rev))
  
  percentile_data <- percentile_data_fwd %>%
    cbind(percentile_data_rev) %>%
    pivot_longer(!position, names_to = "test", values_to = "value") %>%
    separate(test, c('Test', 'Percentile'), sep = '_') %>%
    arrange(position, Test, Percentile) %>%
    mutate(rownum = case_when(
      Percentile == '5th' ~ 1,
      Percentile == '10th' ~ 2,
      Percentile == '20th' ~ 3,
      Percentile == '40th' ~ 4,
      Percentile == '60th' ~ 5,
      Percentile == '80th' ~ 6,
      Percentile == '90th' ~ 7,
      Percentile == '100th' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    group_by(position, Test) %>%
    arrange(position, Test, desc(rownum)) %>%
    select(-rownum) %>%
    ungroup()
  
  ord <- c('5th', '10th', '20th', '40th', '60th', '80th', '90th', '100th')
  
  metric <- ifelse(test == 'Height', '[in]',
                   ifelse(test == 'Weight', '[lbs]',
                          ifelse(test %in% c('Vertical Jump', 'Broad Jump'), '[in]',
                                 ifelse(test == 'Bench', '[reps @ 225lbs]',
                                        '[s]'))))
  
  ex_rev <- percentile_data %>%
    #filter(position %in% positions) %>%
    filter(position %in% c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')) %>%
    
    #filter(Test == test) %>%
    filter(Test %in% c('40yd', 'Shuttle', '3cone')) %>%
    bind_rows(
      percentile_data %>%
        filter(position %in% c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')) %>%
        filter(Test %in% c('40yd', 'Shuttle', '3cone')) %>%
        filter(Percentile == '10th') %>%
        mutate(Percentile = '<10')
    ) %>%
    filter(Percentile != '5th') %>%
    mutate(Percentile = case_when(
      Percentile == '<10' ~ '<10',
      Percentile == '10th' ~ '10-19',
      Percentile == '20th' ~ '20-39',
      Percentile == '40th' ~ '40-59',
      Percentile == '60th' ~ '60-79',
      Percentile == '80th' ~ '80-89',
      Percentile == '90th' ~ '90-99',
      Percentile == '100th' ~ '100',
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '<10' ~ 1,
      Percentile == '10-19' ~ 2,
      Percentile == '20-39' ~ 3,
      Percentile == '40-59' ~ 4,
      Percentile == '60-79' ~ 5,
      Percentile == '80-89' ~ 6,
      Percentile == '90-99' ~ 7,
      Percentile == '100' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(Test, position, rownum) %>%
    select(-rownum) %>%
    mutate(value = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), round(value,0), round(value, 2))) %>%
    mutate(value_m1 = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), value-1, value-0.01)) %>%
    mutate(value_p1 = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), value+1, value+0.01)) %>%
    group_by(Test, position) %>%
    mutate(value_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value)-1, format(round(lead(value)-0.01, 2), nsmall = 2))) %>%
    mutate(value_m1_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value_m1), format(round(lead(value_m1), 2), nsmall = 2))) %>%
    mutate(value_p1_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value_p1), format(round(lead(value_p1), 2), nsmall = 2))) %>%
    mutate(value_final = case_when(
      Percentile == '<10' ~ paste0('>', value),
      Percentile %in% c('10-19', '20-39', '40-59', '60-79', '80-89', '90-99') ~ paste0(format(value_p1_lead, nsmall = 2), '-', format(value, nsmall = 2)),
      Percentile == '100' ~ paste0('\u2264', value),
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '<10' ~ 1,
      Percentile == '10-19' ~ 2,
      Percentile == '20-39' ~ 3,
      Percentile == '40-59' ~ 4,
      Percentile == '60-79' ~ 5,
      Percentile == '80-89' ~ 6,
      Percentile == '90-99' ~ 7,
      Percentile == '100' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(Test, position, desc(rownum)) %>%
    select(-rownum) %>%
    mutate(across(everything(), as.character))
  
  ex_fwd <- percentile_data %>%
    #filter(position %in% positions) %>%
    filter(position %in% c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')) %>%
    
    #filter(Test == test) %>%
    filter(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench')) %>%
    bind_rows(
      percentile_data %>%
        filter(position %in% c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')) %>%
        filter(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench')) %>%
        filter(Percentile == '10th') %>%
        mutate(Percentile = '<10')
    ) %>%
    filter(Percentile != '5th') %>%
    mutate(Percentile = case_when(
      Percentile == '<10' ~ '<10',
      Percentile == '10th' ~ '10-19',
      Percentile == '20th' ~ '20-39',
      Percentile == '40th' ~ '40-59',
      Percentile == '60th' ~ '60-79',
      Percentile == '80th' ~ '80-89',
      Percentile == '90th' ~ '90-99',
      Percentile == '100th' ~ '100',
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '<10' ~ 1,
      Percentile == '10-19' ~ 2,
      Percentile == '20-39' ~ 3,
      Percentile == '40-59' ~ 4,
      Percentile == '60-79' ~ 5,
      Percentile == '80-89' ~ 6,
      Percentile == '90-99' ~ 7,
      Percentile == '100' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(position, rownum) %>%
    select(-rownum) %>%
    mutate(value = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), round(value,0), round(value, 2))) %>%
    mutate(value_m1 = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), value-1, value-0.01)) %>%
    mutate(value_p1 = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), value+1, value+0.01)) %>%
    group_by(Test, position) %>%
    mutate(value_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value)-1, format(round(lead(value)-0.01, 2), nsmall = 2))) %>%
    mutate(value_m1_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value_m1), format(round(lead(value_m1), 2), nsmall = 2))) %>%
    mutate(value_p1_lead = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), lead(value_p1), format(round(lead(value_p1), 2), nsmall = 2))) %>%
    mutate(value_final = case_when(
      Percentile == '<10' ~ paste0('<', value),
      Percentile %in% c('10-19', '20-39', '40-59', '60-79', '80-89', '90-99') ~ paste0(format(value, nsmall = 0), '-', format(value_m1_lead, nsmall = 0)),
      Percentile == '100' ~ paste0('\u2265', value),
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '<10' ~ 1,
      Percentile == '10-19' ~ 2,
      Percentile == '20-39' ~ 3,
      Percentile == '40-59' ~ 4,
      Percentile == '60-79' ~ 5,
      Percentile == '80-89' ~ 6,
      Percentile == '90-99' ~ 7,
      Percentile == '100' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(Test, position, desc(rownum)) %>%
    select(-rownum) %>%
    mutate(across(everything(), as.character))
  
  ex <- ex_fwd %>%
    bind_rows(ex_rev) %>%
    select(position, Test, Percentile, value_final) %>%
    filter(position %in% positions) %>%
    filter(Test == test) %>%
    # filter(position %in% c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR')) %>%
    # filter(Test == '40yd') %>%
    pivot_wider(names_from = c(position), values_from = value_final) %>%
    #mutate_if(is.numeric, round, digits=2) %>%
    mutate(Class = case_when(
      Percentile == '<10' ~ 'Bad',
      Percentile == '10-19' ~ 'Very Poor',
      Percentile == '20-39' ~ 'Poor',
      Percentile == '40-59' ~ 'Fair',
      Percentile == '60-79' ~ 'Good',
      Percentile == '80-89' ~ 'Very Good',
      Percentile == '90-99' ~ 'Excellent',
      Percentile == '100' ~ 'Combine-Leading',
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '<10' ~ 1,
      Percentile == '10-19' ~ 2,
      Percentile == '20-39' ~ 3,
      Percentile == '40-59' ~ 4,
      Percentile == '60-79' ~ 5,
      Percentile == '80-89' ~ 6,
      Percentile == '90-99' ~ 7,
      Percentile == '100' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(desc(rownum)) %>%
    select(-rownum) %>%
    ungroup()
  
  table_perc <- ex %>%
    select(-Test) %>%
    select(Class, Percentile, dplyr::everything()) %>%
    gt() %>%
    tab_header(
      title = test,
      subtitle = 'Percentile by Position'
    ) %>%
    cols_align(
      align = "center",
      columns = c(Percentile, Class)
    ) %>%
    tab_source_note("Data Source: {combineR}/Pro Football Reference") %>%
    tab_header(
      title = md(paste0('**', test,' ',metric[1], '**')),
      subtitle = "Percentile by Position (NFL Combine)"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FAE100"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 1)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "darkgreen"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 2)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00CC00"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 3)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#CFFFA9"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 4)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#ffff99"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 5)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FFCE8D"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 6)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FF8D8D"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 7)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#D53B3B"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = c(Percentile, Class, !!!syms(positions)),
        rows = 8)
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "lightgrey",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          columns = c(Percentile)
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "left",
          color = "lightgrey",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = c(Class)
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "lightgrey",
          weight = px(3)
        )
      ),
      locations = list(
        cells_column_labels(
          columns = gt::everything()
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "lightgrey",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = c(!!!syms(positions))
        )
      )
    ) %>%
    cols_align(
      align = 'center',
      columns = gt::everything()
    ) %>%
    tab_options(
      table.font.size = px(20)
    )
  
  print(table_perc)
  
}
