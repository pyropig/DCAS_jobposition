library(tidyverse)
library(patchwork)
plot_missing <- function(x, percent = TRUE ) {
  missing_patterns <- data.frame(is.na(x)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  # keep the col stats 
  r_col <- colSums(is.na(x)) %>% 
    sort(decreasing = TRUE)
  col_level <- names(r_col)
  r_col <- tibble(name = names(r_col),value = r_col)
  r_col$name <- fct_relevel(r_col$name, r_col$name)
  if (percent) {
    r_col$value <- r_col$value / nrow(x) * 100
    r_col_max = 100
    r_col_by = 25
    r_col_lab = "% rows \n missing"
  }else {
    r_col_max = max(r_col['value'])
    r_col_by = ceiling(r_col_max / 3)
    r_col_lab = "num rows \n missing"
  }
  # keep the pattern stats
  pattern_stat <- tibble(index = rownames(missing_patterns), val = missing_patterns[["count"]])
  pattern_stat$index<- fct_relevel(pattern_stat$index, as.character(seq(1,nrow(missing_patterns),1)))
  if (percent) {
    pattern_stat$val <- pattern_stat$val / nrow(x) * 100
    pattern_stat_max = 100
    pattern_stat_by = 25
    pattern_stat_lab = "% rows"
  } else {
    pattern_stat_max = max(pattern_stat['val'])
    pattern_stat_by = ceiling(pattern_stat_max/3)
    pattern_stat_lab = "row count"
  }
  # change the missing data 
  missing_part <- missing_patterns[,-ncol(missing_patterns)]*1
  
  # get the complete number 
  complete_num <- which(rowSums(missing_part)==0)
  
  # change the complete pattern value
  if (length(complete_num!=0)){
    missing_part[complete_num,] = 2
  }
  # change the data for main plotting 
  
  tidymiss <- missing_part %>% 
    rownames_to_column("id") %>%
    gather(key,value,-id) %>%
    mutate(missing = ifelse(value == 2, "complete", ifelse(value == 0,"own","miss")))
  tidymiss$id <- fct_relevel(tidymiss$id, as.character(seq(1,nrow(missing_part),1)))
  #relevel the tidymiss and make the value to be discrete 
  tidymiss$key <- fct_relevel(tidymiss$key, col_level)
  # plot the main part 
  if (length(complete_num!=0)) {
    p1 = ggplot(tidymiss, aes (x = key, y = fct_rev(id), fill = missing)) + 
      geom_tile(color = "white", lwd = 0.2) + 
      scale_fill_manual(values = c("own" ="lightgrey","miss" = "#B6A0E4","complete" = "grey")) + 
      theme_classic() + 
      scale_x_discrete(label = function(x) stringr::str_trunc(x, 7)) +
      theme(legend.position = "none") + 
      annotate("text",label = "complete cases", x= ncol(missing_part)/2.0+0.5,y = nrow(missing_part) - complete_num + 1) + 
      ylab("missing pattern") + 
      xlab("variable")
  } else {
    p1 = ggplot(tidymiss, aes (x = key, y = fct_rev(id), fill = missing)) + 
      geom_tile(color = "white", lwd = 0.2) + 
      scale_fill_manual(values = c("own" ="lightgrey","miss" = "#B6A0E4","complete" = "grey")) + 
      theme_classic() + 
      scale_x_discrete(label = function(x) stringr::str_trunc(x, 7)) +
      theme(legend.position = "none") + 
      ylab("missing pattern") + 
      xlab("variable")
  }
  
  # plot the col graph
  p2 = ggplot(r_col, aes(x =name, y = value)) + 
    geom_bar(stat = "identity",fill = "royalblue", alpha = 0.5) +
    scale_y_continuous(limits = c(0,r_col_max),
                       breaks = seq(0, r_col_max, by = r_col_by),
                       expand = c(0,0)) +
    theme_bw() +
    scale_x_discrete(label = function(x) stringr::str_trunc(x, 7)) +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line( size=.1, color="grey" )) + 
    ylab(r_col_lab)+ 
    xlab("")
  
  # plot the patter graph
  pattern_stat$cop <- vector(length = nrow(pattern_stat))
  pattern_stat[which(pattern_stat$index == complete_num),"cop"] = TRUE
  p3 = ggplot(pattern_stat, aes(x = fct_rev(index), y = val,alpha = cop)) + 
    geom_bar(stat = "identity",fill = "royalblue") +
    scale_y_continuous(limits = c(0, pattern_stat_max),
                       breaks = seq(0, pattern_stat_max, by = pattern_stat_by),
                       expand = c(0,0)) +
    theme_bw() +
    coord_flip() + 
    theme(panel.grid.major.y = element_blank() ,
          panel.grid.major.x= element_line( size=.1, color="grey" ),
          legend.position = "none") + 
    scale_alpha_manual(values = c(0.5,0.8))+
    xlab("")+ 
    ylab(pattern_stat_lab) 
  p2 +  plot_spacer() + p1 + p3 + plot_layout(ncol = 2,widths = c(4, 1), heights = c(1,4))
}