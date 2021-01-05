

# Impute missing values ---------------------------------------------------

noimpvars <- names(survdata)[!names(survdata) %in% modvars]

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(outtime_death, out_death == 1) ~ 1,
  data = survdata, method = "breslow"
))
survdata <- left_join(survdata, na, by = c("outtime_death" = "time"))

ini <- mice(survdata, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[noimpvars] <- ""

## check no cores
cores_2_use <- parallel::detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- parallel::makeCluster(cores_2_use)
parallel::clusterSetRNGStream(cl, 49956)
doParallel::registerDoParallel(cl)

imp <-
  foreach::foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "survdata"),
    .packages = "mice"
  ) %dopar% {
    mice(survdata,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
doParallel::stopImplicitCluster()
