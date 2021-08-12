# add custom functions to extract estimates (tidy) and goodness-of-fit (glance) information
# Maksym Polyakov 2021/08/12

require(tidyverse)

tidy.gmnl <- function(x, conf.int = FALSE, conf.level = 0.95, wrt = NA,  ...) {
  
  if (!inherits(x, "gmnl")) stop("The model was not estimated using gmnl")
  
  nobs = x$logLik$nobs
  df = x$logLik$nobs - x$logLik$nparam

  if (!is.na(wrt) & !(wrt %in% names(x$coefficients))) {print(paste(wrt, "is not a variable")) }

  if (!is.na(wrt) & (wrt %in% names(x$coefficients))) { 
    # WTP
    if (x$model %in% c("lc", "mm")) {
      # latent class models - process by class
      ret <- NULL
      for(q in 1:x$Q){
        capture.output(
          ret1 <- as_tibble(as.data.frame(wtp.gmnl(x,wrt=paste("class", q, wrt, sep = "."))), rownames = "term" ) %>% 
            # only for the coefficients
            filter(substr(term,9,11) != "sd.") %>% 
            # only current class
            filter(substr(term,1,7) == paste("class", q, sep = ".")) %>% 
            mutate(term = paste("wtp", term, sep = ".")), 
          file='NUL')
        colnames(ret1) <- c("term", "estimate", "std.error", "statistic", "p.value")
        ret <- bind_rows(ret, ret1)
      }
    } else {
      # all other models 
      capture.output(
        ret <- as_tibble(as.data.frame(wtp.gmnl(x,wrt=wrt)), rownames = "term" ) %>% 
          # only for the coefficients
          filter(substr(term,1,3) != "sd.") %>% 
          mutate(term = paste("wtp", term, sep = ".")), 
        file='NUL')
      colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
    }
  } else  {
    # coefficients estimates
    if (x$correlation == TRUE){
      # for correlated random models extract coefficients 
      # separately from standard deviations 
      ret <- as_tibble(summary(x)$CoefTable, rownames = "term")%>% 
        filter(!grepl("^sd\\.|^class\\.[0-9]+\\.sd\\.", term))
      if (x$model %in% c("mm")){  
        #latent class - by class
        for(q in 1:x$Q){
          capture.output(
            ret <- bind_rows(
              ret, as_tibble(se.cov.gmnl(x, sd = TRUE, q), rownames = "term") %>% 
                mutate(term = paste("class", q, "sd",term, sep = "."))), 
            file='NUL')
        }
      } else {
        # uncorrelated models simply get coeftable
        capture.output(
          ret <- bind_rows(
            ret, as_tibble(se.cov.gmnl(x, sd = TRUE), rownames = "term") %>%
              mutate(term = paste("sd",term, sep = "."))),
          file='NUL')
      }
    } else {
      ret <- as_tibble(summary(x)$CoefTable, rownames = "term")
    }
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  }

  # for latent class models:
  # - create class and term columns from term column 
  # - calculate class shares 
  if (x$model %in% c("lc", "mm")) {  
    ret <- ret %>% 
      mutate(
        # class.1.term    
        # add class column    
        class = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.([0-9]+)\\..*$", "Class \\1", term), ""),
        class = if_else(grepl("^wtp.class\\.[0-9]+\\..*$", term), gsub("^wtp.class\\.([0-9]+)\\..*$", "Class \\1", term), class),
        class = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)([0-9]+)$", "Class \\1", term), class),
        class = if_else(grepl("^.*:class[0-9]+$", term), gsub("^.*:class([0-9]+)$", "Class \\1", term), class),
        class = if_else(grepl("^class[0-9]+:.*$", term), gsub("^class([0-9]+):.*$", "Class \\1", term), class),
        # class.1.term    
        # remove "class" from term column    
        term = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.[0-9]+\\.(.*)$", "\\1", term), term),
        term = if_else(grepl("^wtp.class\\.[0-9]+\\..*$", term), gsub("^wtp.class\\.[0-9]+\\.(.*)$", "wtp.\\1", term), term),
        term = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)[0-9]+$", "cl.m Intercept", term), term),
        term = if_else(grepl("^.*:class[0-9]+$", term), gsub("^(.*):class[0-9]+$", "cl.m \\1", term), term),
        term = if_else(grepl("^class[0-9]+:.*$", term), gsub("^class[0-9]+:(.*)$", "cl.m \\1", term), term),
      ) %>%
      select(class, term, estimate, std.error, statistic, p.value)
    
    # calculate class shares
    if (x$model %in% c("lc")) {Qir <- x$Qir} else {Qir <- x$Qir$wnq} 
    Q <- x$Q
    shares <- bind_cols(class =  paste("Class", 1:Q, sep = " "), 
                        term = rep("cl.shares", Q), 
                        estimate = colMeans(Qir)
    )
    ret <- bind_rows(ret, shares)

  }
  
  if (conf.int) {
    ret <- ret %>% mutate(conf.low = estimate - 1.96 * std.error,
                          conf.high = estimate + 1.96 * std.error)
  }

  ret
}


glance.gmnl <- function(x, ...) {
  with(
    summary(x),
    tibble(
      # added number of individuals
      nind = length(x$prob.ind),
      nobs = x$logLik$nobs,
      logLik = as.numeric(stats::logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x)
    )
  )
}




