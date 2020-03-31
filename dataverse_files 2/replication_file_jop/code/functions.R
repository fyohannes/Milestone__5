
#Write latex tables with notes:
write_latex = function(star_table, note_text, out_path, scale) {
  
  table_note = note_text %>% 
    str_replace_all("\n", " ")
  
  preamble = c( "\\scalebox{", scale, "}{ 
    \\begin{threeparttable}")
  
  out_table = append(star_table, 
                     preamble,
                     str_detect(star_table, "centering") %>% which) %>%
    append(.,
           c("\\begin{tablenotes}", "\\small", 
              paste0("\\item \\textit{Note}: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01.", table_note), 
              "\\end{tablenotes}", "\\end{threeparttable}}"),
              str_detect(., "[\\\\]end\\{tabular\\}") %>% which)
  
  cat(out_table, sep = '\n', file = out_path)
}

#Write latex tables with notes:
write_latex_plain = function(star_table, note_text, out_path) {
  
  cat(star_table, sep = '\n', file = out_path)
}


#Write latex tables with notes:
write_latex_placebo = function(star_table, note_text, out_path, scale) {
  
  table_note = note_text %>% 
    str_replace_all("\n", " ")
  
  preamble = c( "\\scalebox{", scale, "}{ 
                \\begin{threeparttable}")
  
  out_table = append(star_table, 
                     preamble,
                     str_detect(star_table, "centering") %>% which) %>%
    append(.,
           c("\\begin{tablenotes}", "\\small", 
             paste0("\\item \\textit{Note}: $^{*}$p$<$0.05; $^{**}$p$<$0.01.", table_note), 
             "\\end{tablenotes}", "\\end{threeparttable}}"),
           str_detect(., "[\\\\]end\\{tabular\\}") %>% which)
  
  cat(out_table, sep = '\n', file = out_path)
}


loess_plot <- function(x, y, W, ylab){
  
  ##making the loess fit
  tmp <- na.omit(data.frame(x = x, y = y, W = W))
  #tmp <- tmp[tmp$x > -0.30 & tmp$x < 0.30,]
  loess_reg <- loess(y ~ x * W, data = tmp, degree = 1)
  tmp$y_fitted <- fitted.values(loess_reg)
  tmp$se <- predict(loess_reg, tmp, se = TRUE)$se.fit
  
  
  #making plots
  p <- ggplot(data = tmp, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = y_fitted - 1.67 * se, ymax = y_fitted + 1.67 * se, group = W), alpha = .2, linetype = "dashed", color="darkgrey") +
    geom_line(aes(y = y_fitted, group = W), color = 'black') + theme_minimal() + xlim(-0.02, 0.02) + 
    xlab("Vote Margin for Last Seat") + ylab(ylab) + theme(text=element_text(size=10, family="Times")) +
    stat_summary_bin(fun.y='mean', bins=24,
                     color='black', size=2, geom='point', alpha = 0.4) 
  
  return(p)
}



mod_plot <- function(x, y, d, ylab, xlab, title){
  
  
  tmp <- na.omit(data.frame(x = x, y = y, d = d))
  
  b0 <- summary(lm(y ~ x, data = tmp[tmp$d == 0,]))[[4]][2,1]
  se0 <- summary(lm(y ~ x, data = tmp[tmp$d == 0,]))[[4]][2,2]
  
  b1 <- summary(lm(y ~ x, data = tmp[tmp$d == 1,]))[[4]][2,1]
  se1 <- summary(lm(y ~ x, data = tmp[tmp$d == 1,]))[[4]][2,2]
  
  tmp = cbind(c(b0, b1), c(se0, se1), c("No", "Yes")) %>% 
    data.frame()
  
  tmp$X1 = as.numeric(as.character(tmp$X1))
  tmp$X2 = as.numeric(as.character(tmp$X2))
  
  
  ggplot(tmp, aes(x=factor(X3), y = as.numeric(X1), group = 1)) + 
    geom_point(color = "black", fill = "white", size = 2) + 
    geom_errorbar(aes(ymin=X1 - 1.96*as.numeric(X2), ymax = X1 + 1.96*as.numeric(X2)),
                  width = 0.075, color = "red", position=position_dodge(width=0.5)) +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    geom_line(alpha = 0.5) + 
    xlab(xlab) + 
    ylab(ylab) +
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=12, family="Times")) %>%
    return()
  
  
}

