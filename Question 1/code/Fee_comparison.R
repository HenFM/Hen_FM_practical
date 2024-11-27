Cum_Fee_Comparison <- function(data, Fee = 50*1e-4, Start = ymd(20100101), 
                               # Added purely for figure adjustment:
                               Gap = 3, Lvlset = 5,
                               mnthfwd = 18){
  
  # Turn annual fee into monthly compounded:
  # Remember, we want to find x so that its compound is 10 bps, or: (1+x)^12 = 10*1e-4
  feeconverter <- function(x, Ann_Level) (1+x)^(1/Ann_Level)-1
  
  df_p <- 
    data %>% filter(date > Start) %>% 
    mutate('Return - 10 bps' = Returns - feeconverter(10*1e-4, Ann_Level = 12)) %>% 
    mutate('Return - 50 bps' = Returns - feeconverter(50*1e-4, Ann_Level = 12)) %>% 
    mutate('Return - 100 bps' = Returns - feeconverter(100*1e-4, Ann_Level = 12)) %>% 
    mutate('Return - 200 bps' = Returns - feeconverter(200*1e-4, Ann_Level = 12)) %>% 
    mutate('Return - 250 bps' = Returns - feeconverter(250*1e-4, Ann_Level = 12)) %>% 
    mutate('Return - 350 bps' = Returns - feeconverter(350*1e-4, Ann_Level = 12)) %>% 
    rename(Gross = Returns) %>% 
    gather(Type, Rets, -date, -Gross) %>% 
    group_by(Type) %>% filter(date > first(date)) %>% 
    mutate(CP = cumprod(1+Rets)) %>% 
    mutate(Gross = cumprod(1+Gross))
  
  Ord <- c('Return - 10 bps', 'Return - 50 bps', 'Return - 100 bps', 'Return - 200 bps', 'Return - 250 bps', 'Return - 350 bps')
  
  Txt <- 
    df_p %>% filter(date == last(date)) %>% 
    mutate(date = date %m+% months(Gap)) %>% 
    mutate(Text = paste0(round(CP/Gross-1, 3)*100, "%")) %>% 
    # Order:
    mutate(Type = as.factor(Type)) %>%
    mutate(Type = forcats::fct_relevel(Type, Ord))
  
  Shock350 <- 
    df_p %>% filter(date == last(date)) %>% 
    mutate(date = date %m+% months(Gap)) %>% 
    filter(Type == 'Return - 350 bps') %>% pull(CP)
  Shock250 <- 
    df_p %>% filter(date == last(date)) %>% 
    mutate(date = date %m+% months(Gap)) %>% 
    filter(Type == 'Return - 250 bps') %>% pull(CP)
  Shock25 <- 
    df_p %>% filter(date == last(date)) %>% 
    mutate(date = date %m+% months(Gap)) %>% 
    filter(Type == 'Return - 10 bps') %>% pull(CP)
  
  Msg <- 
    df_p %>% ungroup() %>% filter(date > first(date) %m+% months(mnthfwd)) %>% slice(1) %>% 
    mutate(Lvl = Lvlset) %>% mutate(Msg = glue::glue("R1m invested in {format(Start, '%B %Y')}\n* 10bps: R{round(Shock25, 2)}m\n* 250bps: R{round(Shock250, 2)}m\n* 350bps: R{round(Shock350, 2)}m"))
  
  g <-   
    df_p %>% 
    mutate(Type = as.factor(Type)) %>%
    mutate(Type = forcats::fct_relevel(Type, Ord)) %>% 
    fmxdat::plot_order_set( Column = "Type", Order = Ord) %>% 
    
    ggplot() + 
    geom_line(aes(date, Gross), color = "darkgreen", size = 1.5, alpha = 0.95) + 
    geom_line(aes(date, CP, color = Type), size = 1.2, alpha = 0.6) + 
    # ggrepel::geom_label_repel(data = Txt, aes(date, CP, label = Text ), hjust = 0, color = "darkred", alpha = 0.25, size = 3) + 
    geom_text(data = Txt, aes(date, CP, label = Text ), hjust = 0, color = "darkred", size = 3.3) +
    fmxdat::theme_fmx() +  
    geom_label(data = Msg, aes(date, Lvl, label = Msg), color = "darkgreen", alpha = 0.8, size = 4) + 
    labs(title = "Fee Impact on Cumulated Wealth", 
         subtitle = "Monthly Median of Actively managed funds",
         x = "", y = "Cumulative Returns") + 
    
    scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year") + 
    theme(axis.text.x=element_text(angle = 90, hjust = 1))
  
  
  g
  
}