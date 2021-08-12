# add custom functions to extract estimates (tidy) and goodness-of-fit (glance) information
# Maksym Polyakov 2021/08/12

require(tidyverse)

tidy.gmnl <- function(x, conf.int = FALSE, conf.level = 0.95, wrt = NA,  ...) {
  
  if (!inherits(x, "gmnl")) stop("The model was not estimated using gmnl")
  
  nobs = x$logLik$nobs
  df = x$logLik$nobs - x$logLik$nparam

  #WTP
  if (!is.na(wrt)) { 
    if (x$model %in% c("lc", "mm")) {
      # 
      ret <- NULL
      for(q in 1:x$Q){
        capture.output(
          ret1 <- as_tibble(as.data.frame(wtp.gmnl(x,wrt=paste("class", q, wrt, sep = "."))), rownames = "term" ) %>% 
            filter(substr(term,9,11) != "sd.") %>% 
            filter(substr(term,1,7) == paste("class", q, sep = ".")),
          file='NUL')
        colnames(ret1) <- c("term", "estimate", "std.error", "statistic", "p.value")
        ret <- bind_rows(ret, ret1)
      }
    } else {
      capture.output(
        ret <- as_tibble(as.data.frame(wtp.gmnl(x,wrt=wrt)), rownames = "term" ) %>% 
          filter(substr(term,1,3) != "sd.") %>% 
          mutate(term = paste("wtp", term, sep = ".")), 
        file='NUL')
      colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
    }
  } else  {
    if (x$correlation == TRUE){
      ret <- as_tibble(summary(x)$CoefTable, rownames = "term")%>% 
        filter(!grepl("^sd\\.|^class\\.[0-9]+\\.sd\\.", term))
      if (x$model %in% c("mm")){  
        for(q in 1:x$Q){
          capture.output(
            ret <- bind_rows(
              ret, as_tibble(se.cov.gmnl(x, sd = TRUE, q), rownames = "term") %>% 
                mutate(term = paste("class", q, "sd",term, sep = "."))), 
            file='NUL')
        }
      } else {
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

  if (x$model %in% c("lc", "mm")) {  
    ret <- ret %>% 
      mutate(
        # class.3.term    
        class = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.([0-9]+)\\..*$", "Class \\1", term), ""),
        class = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)([0-9]+)$", "Class \\1", term), class),
        class = if_else(grepl("^.*:class[0-9]+$", term), gsub("^.*:class([0-9]+)$", "Class \\1", term), class),
        class = if_else(grepl("^class[0-9]+:.*$", term), gsub("^class([0-9]+):.*$", "Class \\1", term), class),
        # class.3.term    
        term = if_else(grepl("^class\\.[0-9]+\\..*$", term), gsub("^class\\.[0-9]+\\.(.*)$", "\\1", term), term),
        term = if_else(grepl("^\\(class\\)([0-9]+)", term), gsub("^\\(class\\)[0-9]+$", "cl.m Intercept", term), term),
        term = if_else(grepl("^.*:class[0-9]+$", term), gsub("^(.*):class[0-9]+$", "cl.m \\1", term), term),
        term = if_else(grepl("^class[0-9]+:.*$", term), gsub("^class[0-9]+:(.*)$", "cl.m \\1", term), term),
      ) %>%
      select(class, term, estimate, std.error, statistic, p.value)
    
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




