

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC ", min(esc$num_dmVisitdt), " - ", max(esc$num_dmVisitdt)), nrow(esc))

pdata <- esc %>%
  filter(num_dmPtype == "Hospital")
flow <- rbind(flow, c("Hospitalized patients", nrow(pdata)))

pdata <- pdata %>%
  filter(!is.na(num_dcEf))
flow <- rbind(flow, c("EF assessment (Echo-Doppler) during hospitalisation", nrow(pdata)))

pdata <- pdata %>%
  filter((num_hsAcs == "No" | is.na(num_hsAcs)) & (num_hsHosPresCli != "ACS/HF" | is.na(num_hsHosPresCli))) %>%
  mutate(num_hsHosPresCli = droplevels(num_hsHosPresCli))
flow <- rbind(flow, c("Excluding patients with reason for hospitalisation as ASC OR Hospital presentaion, clinical profile as ACS/HF", nrow(pdata)))

pdata <- pdata %>%
  filter(num_dcAorSte == "No" | is.na(num_dcAorSte))
flow <- rbind(flow, c("Excluding patients with moderate-to-severe aortic stenosis", nrow(pdata)))

pdata <- pdata %>%
  mutate(pop_outcome = ifelse(num_dcVital == "Alive", 1, 0))
flow <- rbind(flow, c("Alive at disharge", nrow(pdata %>% filter(pop_outcome == 1))))

pdata <- pdata %>%
  mutate(pop_outcome = ifelse(num_f1lost == "No" & pop_outcome == 1, 1, 0))
flow <- rbind(flow, c("Not lost to follow-up (no information on endpoints available)", nrow(pdata %>% filter(pop_outcome == 1))))

pdata <- pdata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),

    outtime_death = as.numeric(enddtm - startdtm),

    pop_outcome = ifelse(outtime_death >= 0 & pop_outcome == 1, 1, 0)
  )
flow <- rbind(flow, c("No negative follow-up times", nrow(pdata %>% filter(pop_outcome == 1))))

colnames(flow) <- c("Criteria", "N")



# Dist EF for excluded pats -----------------------------------------------

distef <- esc %>%
  filter(
    num_dmPtype == "Hospital",
    !is.na(num_dcEf)
  ) %>%
  mutate(d_efgroup = factor(case_when(
    num_dcEf < 40 ~ "rEF",
    num_dcEf <= 49 ~ "mrEF",
    num_dcEf >= 50 ~ "pEF"
  ),
  levels = c("pEF", "mrEF", "rEF")
  ))

distef1 <- distef %>%
  filter(!((num_hsAcs == "No" | is.na(num_hsAcs)) & (num_hsHosPresCli != "ACS/HF" | is.na(num_hsHosPresCli)))) %>%
  count(d_efgroup)

distef2 <- distef %>%
  filter((num_hsAcs == "No" | is.na(num_hsAcs)) & (num_hsHosPresCli != "ACS/HF" | is.na(num_hsHosPresCli))) %>%
  filter(!(num_dcAorSte == "No" | is.na(num_dcAorSte))) %>%
  count(d_efgroup)

outdistef <- full_join(distef1, distef2, by = "d_efgroup")
colnames(outdistef) <- c("EF", "Excl due to ACS", "Excl due to Aortic stenosis")
