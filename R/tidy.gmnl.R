# add custom functions to extract estimates (tidy) and goodness-of-fit (glance) information

tidy.gmnl <- function(x, conf.int = FALSE, conf.level = 0.95, wrt = NA,  ...) {
  
  #warn_on_subclass(x)
  
  nobs = x$logLik$nobs
  df = x$logLik$nobs - x$logLik$nparam

  if (!is.na(wrt))
  {
    capture.output(
    ret <- as_tibble(as.data.frame(wtp.gmnl(x,wrt=wrt)), rownames = "term" ),
    file='NUL')
    colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
    
  } 
  else
  {
  if (x$correlation == TRUE){
     ret <- bind_rows(as_tibble(summary(x)$CoefTable, rownames = "term")%>% 
                   filter(substr(term,1,3) != "sd."),
                   as_tibble(se.cov.gmnl(x, sd = TRUE), rownames = "term") %>% 
                   mutate(term = paste("sd",term, sep = ".")))
     } else {
     ret <- as_tibble(summary(x)$CoefTable, rownames = "term")
     }
  
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  
  }

  ret <- ret %>% 
    mutate(
      # class.3.term    
      class = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.([0-9]+)\\..*$", "Class \\1", term), ""),
      class = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)([0-9]+)$", "Class \\1", term), class),
      class = if_else(grepl("^.*:class[0-9]+$", term), gsub("^.*:class([0-9]+)$", "Class \\1", term), class),
      # class.3.term    
      term = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.[0-9]+\\.(.*)$", "\\1", term), term),
      term = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)[0-9]+$", "m.Intercept", term), term),
      term = if_else(grepl("^.*:class[0-9]+$", term), gsub("^(.*):class[0-9]+$", "m.\\1", term), term),
      ) %>%
    select(class, term, estimate, std.error, statistic, p.value)
  
  gsub("^group\\.([0-9]+)\\..*$", "\\1", "group.1.tets")
  
  
  # if (conf.int) {
  #   ci <- broom_confint_terms(x, level = conf.level)
  #   ret <- dplyr::left_join(ret, ci, by = "term")
  # }
  if (conf.int) {
    ret <- ret %>% mutate(conf.low = estimate - 1.96 * std.error,
                          conf.high = estimate + 1.96 * std.error)
  }

  
  ret
}


glance.gmnl <- function(x, ...) {
  
  #warn_on_subclass(x)
  
  # check whether the model was fitted with only an intercept, in which
  # case drop the fstatistic related columns
  int_only <- nrow(summary(x)$coefficients) == 1
  
  with(
    summary(x),
    tibble(
#      r.squared = r.squared,
#      adj.r.squared = adj.r.squared,
#      sigma = sigma,
      # statistic = if (!int_only) {fstatistic["value"]} else {NA_real_},
      # p.value = if (!int_only) {
      #   pf(
      #     fstatistic["value"],
      #     fstatistic["numdf"],
      #     fstatistic["dendf"],
      #     lower.tail = FALSE
      #   )
      # } else {NA_real_},
#      df = if (!int_only) {fstatistic["numdf"]} else {NA_real_},
      logLik = as.numeric(stats::logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
#      deviance = stats::deviance(x),
#      df.residual = df.residual(x),
      nobs = x$logLik$nobs
    )
  )
}




