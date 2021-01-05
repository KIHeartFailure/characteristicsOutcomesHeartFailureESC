table1 <- function(data, name, subset, stratvar, vars, dec = 1) {
  tab1data <- data %>%
    filter(!!subset) 

  ns <- c(nrow(tab1data), tab1data %>% count(!!sym(stratvar)) %>% pull(n))
  
  for (k in seq_along(vars)) {
    nmiss <- paste0(dF(sum(is.na(tab1data %>% pull(!!vars[k]))) / nrow(tab1data) * 100, 1), "%")
    tmp_tab1data <- tab1data %>% filter(!is.na(!!sym(vars[k])))
    
    if (class(tab1data %>% pull(!!vars[k])) == "numeric") {
      tmp_deskout <- contfunc(var = vars[k], stratvar = stratvar, data = tmp_tab1data, dec = dec, nmiss = nmiss)
    }
    if (class(tab1data %>% pull(!!vars[k])) %in% c("factor", "character")) {
      tmp_deskout <- catfunc(var = vars[k], stratvar = stratvar, data = tmp_tab1data, dec = dec, nmiss = nmiss)
    }
    
    names(tmp_deskout)[1] <- "var"
    
    if (k == 1) {deskout <- rbind(c("n", ns, rep(NA, 5)), tmp_deskout)} else { 
      deskout <- rbind(deskout, tmp_deskout)}
  }
  colnames(deskout) <- c(
    c("Variables", "Overall", colnames(deskout)[3:5], "p overall", gsub("p |EF", "", colnames(deskout)[7:9]), "Missing")
  )
  
  deskout <- deskout[, c(1, 10, 2:6, 9, 8, 7)]
  #deskout <- deskout %>%
  #  mutate(Variables = sanitizeTextFunc(Variables))
  
  write.xlsx(deskout, paste0("./output/tabs/tab_", name, "_", Sys.Date(), ".xlsx"), rowNames = FALSE)
  
  footnote(
    mykable(deskout,
            fontsize = 4,
            caption = name,
            longtable = TRUE,
            escape = TRUE
    ) %>%
      landscape(),
    general = c(
      "Categorical variables are presented with n (%) and tested with chi-square test and continuous variables with median [q1-q3], mean (sd) and tested with Kruskal-Wallis test."
    )
  )
}

#table1(data = pdata, name = "test", subset = quote(!is.na(num_age)), stratvar = "d_efgroup", vars = c("num_hsNt", "num_hsBnp", "num_hsTrop", "num_hsSod", "num_hsCre"))
#table1(data = pdata, subset = quote(!is.na(num_age)), stratvar = "d_efgroup", vars = c("num_dmgender", "d_num_dmBmi_cat4"))

#table1(data = pdata, name = "test", subset = quote(!is.na(num_age)), stratvar = "d_efgroup", vars = c("d_num_dmBmi_cat4", "d_num_dmBmi_cat3"))

contfunc <- function(var, stratvar, data, dec, nmiss) {
  deskstatoverall <- data %>%
    summarise(
      q1 = dF(quantile(!!sym(var), na.rm = TRUE, probs = .25), dec),
      med = dF(quantile(!!sym(var), na.rm = TRUE, probs = .5), dec),
      q3 = dF(quantile(!!sym(var), na.rm = TRUE, probs = .75), dec),
      mean = dF(mean(!!sym(var), na.rm = TRUE), dec),
      sd = dF(sd(!!sym(var), na.rm = TRUE), dec)
    ) %>%
    mutate(overall = paste0(med, " [", q1, "-", q3, "]", ", ", mean, " (", sd, ")")) %>%
    select(overall) 
  
  deskstat <- data %>%
    group_by(!!sym(stratvar)) %>%
    summarise(
      q1 = dF(quantile(!!sym(var), na.rm = TRUE, probs = .25), dec),
      med = dF(quantile(!!sym(var), na.rm = TRUE, probs = .5), dec),
      q3 = dF(quantile(!!sym(var), na.rm = TRUE, probs = .75), dec),
      mean = dF(mean(!!sym(var), na.rm = TRUE), dec),
      sd = dF(sd(!!sym(var), na.rm = TRUE), dec), 
      .groups = "drop_last"
    ) %>%
    mutate(ds = paste0(med, " [", q1, "-", q3, "]", ", ", mean, " (", sd, ")")) %>%
    select(!!sym(stratvar), ds) %>%
    pivot_wider(names_from = !!sym(stratvar), values_from = ds)
  
  deskstat <- cbind(deskstatoverall, deskstat)

    poverall <- dF(kruskal.test(formula(paste0(var, "~", stratvar)), data = data)$p.value, dig = 3, p = TRUE)
    deskstat <- cbind(deskstat, poverall)
    levs <- levels(data %>% pull(!!sym(stratvar)))
    if (length(levs) > 2) {
      pstrat <- matrix(rep(NA, length(levs)), ncol = 3)
      pnames <- pstrat
      for (i in seq_along(levs)) {
        pstrat[, i] <- dF(kruskal.test(formula(paste0(var, "~", stratvar)), data = data %>% filter(!!sym(stratvar) != levs[i]))$p.value, dig = 3, p = TRUE)
        pnames[i] <- paste0("p ", paste(levs[seq_along(levs) != i], collapse = " vs "))
      }
    }
    colnames(pstrat) <- pnames
    deskstat <- cbind(var, deskstat, pstrat, nmiss)
}

#koll <- contfunc(var = "num_hsTrop", stratvar = "d_efgroup", data = pdata, dec = 1, nmiss = "20%")

catfunc <- function(var, stratvar, data, dec, nmiss) {
  deskstatoverall <- data %>%
    count(!!sym(var)) %>%
    mutate(p = dF(n / sum(n) * 100, 1), 
           overall = paste0(n, " (", p, "%)")) %>%
    select(!!sym(var), overall) 
  
  deskstat <- data %>%
    group_by(!!sym(stratvar)) %>%
    count(!!sym(var)) %>%
    mutate(p = dF(n / sum(n) * 100, 1), 
           np = paste0(n, " (", p, "%)")) %>%
    select(!!sym(stratvar), !!sym(var), np) %>%
    pivot_wider(names_from = !!sym(stratvar), values_from = np)
    
  deskstat <- full_join(deskstatoverall, deskstat, by = var)
  
  levvar <- levels(data %>% pull(!!sym(var)))
  if (length(levvar) == 2) {
    deskstat <- deskstat %>% slice(n())
    deskstat <- deskstat %>%
      mutate(!!sym(var) := paste0(var, "=", !!sym(var)))
  } else{
    tmp <- deskstat %>% slice(1)
    tmp[,] <- NA
    tmp <- tmp %>%
      mutate(!!sym(var) := var)
    deskstat <- deskstat %>%
      mutate(!!sym(var) := paste0(". ", !!sym(var)))
    deskstat <- rbind(tmp, deskstat)
  }
    
    poverall <- dF(chisq.test(table(data %>% pull(!!sym(stratvar)), data %>% pull(!!sym(var))))$p.value, dig = 3, p = TRUE)
    if (length(levvar) > 2) {
      plevs <- rep(NA, length(levvar))
      plevsnames <- plevs
      
      for (j in seq_along(levvar)){
        tmpdata <- data %>%
          mutate(tmpvar = case_when(!!sym(var) == levvar[j] ~ levvar[j], 
                                    TRUE ~ "X"))
        plevs[j] <- dF(chisq.test(table(tmpdata %>% pull(!!sym(stratvar)), 
                                        tmpdata %>% pull(tmpvar)))$p.value, dig = 3, p = TRUE)
        plevsnames[j] <- paste0(". ", levvar[j])
      }
      
      plevs <- tibble(poverall = c(poverall, plevs), !!sym(var) := c(var, plevsnames))

      deskstat <- full_join(deskstat, plevs, by = var)
    } else{
      deskstat <- cbind(deskstat, poverall)
    }
    
    levs <- levels(data %>% pull(!!sym(stratvar)))
    if (length(levs) > 2) {
      if (length(levvar) == 2) {nrows = 1} else {nrows = length(levvar) + 1}
      pstrat <- matrix(NA, ncol = length(levs) , nrow = nrows)
      pnames <- rep(NA, length(levs))
      
      for (i in seq_along(levs)) {
        tmpdata <- data %>% 
          filter(!!sym(stratvar) != levs[i]) %>% 
          mutate(!!sym(stratvar) := droplevels(!!sym(stratvar)))
        
        pstrat[1, i] <- dF(chisq.test(table(tmpdata %>% pull(!!sym(stratvar)), 
                                           tmpdata %>% pull(!!sym(var))))$p.value, dig = 3, p = TRUE)
        pnames[i] <- paste0("p ", paste(levs[seq_along(levs) != i], collapse = " vs "))
        
        if (length(levvar) > 2) {
          for (j in seq_along(levvar)){
            tmpdata <- tmpdata %>%
              mutate(tmpvar = case_when(!!sym(var) == levvar[j] ~ levvar[j], 
                                        TRUE ~ "X")) 
            pstrat[j + 1, i] <- dF(chisq.test(table(tmpdata %>% pull(!!sym(stratvar)), 
                                            tmpdata %>% pull(tmpvar)))$p.value, dig = 3, p = TRUE)
          }
        }
      }
    }
    colnames(pstrat) <- pnames
    if (length(levvar) > 2) nmiss <- c(nmiss, rep(NA, length(levvar)))
    deskstat <- cbind(deskstat, pstrat, nmiss)
}

#koll <- catfunc(var = "d_num_dmBmi_cat3", stratvar = "d_efgroup", data = data, dec = 1, nmiss = "2%")
