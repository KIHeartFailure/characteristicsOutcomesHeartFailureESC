
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 5)


# Cox regression ----------------------------------------------------------

mod <- coxph(formula(paste0(
  "Surv(outtime_death, out_death == 1) ~ d_efgroup +",
  paste(modvars, collapse = " + "))), data = dataass)


# Checking for non-prop hazards -------------------------------------------

print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

plot(testpat[14])
plot(testpat[15])
plot(testpat[26])
plot(testpat[29])
plot(testpat[32])

# Checking for outliers ---------------------------------------------------

survminer::ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


# Checking for linearity ---------------------------------------------------

ggcoxfunctional(Surv(outtime_death, out_death == 1) ~ num_age + num_dcBpm + num_dcBp1 +
                  num_dcSod + d_dcCKDEPI, data = dataass)

