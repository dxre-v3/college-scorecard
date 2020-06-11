topvars <-  c("faminc", "dep_inc_avg", "pctpell", "md_faminc", "pct25_earn_wne_p6",
              "grad_debt_mdn", "ugds_black", "mn_earn_wne_indep0_p6", 
              "dep_inc_pct_lo", "pct10_earn_wne_p6")
colleges %>% 
  crossing(topvars)


makeGraph = function(df, y){
  label <- terms %>% filter(
    variable_name == str_to_upper(y)
  ) %>% pluck(1)
  
  df %>% 
    select("compl_rpy_5yr_rt", pred = y) %>% 
    ggplot(aes(pred, compl_rpy_5yr_rt)) +
    geom_point(color = '#269844', alpha = 0.5) +
    labs(
      x = label,
      y = "Five-year repayment rate for completers"
    ) +
    theme_minimal()
    
}

makeGraph(colleges, "ugds_black")

terms %>% filter(
    variable_name == str_to_upper("compl_rpy_5yr_rt")
  ) %>% pluck(1)

graphs <- tibble(info = colleges %>% list()) %>% 
  crossing(preds = topvars) %>% 
  mutate(
    graph = map2(info, preds, makeGraph)
  )

